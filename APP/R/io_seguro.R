# Operações centralizadas de entrada e saída.

mensagem_erro_operacao_arquivo <- function(
    caminho,
    operacao = "usar",
    erro = NULL
) {
  nome <- if (
    length(caminho) == 1L &&
      !is.na(caminho) &&
      nzchar(caminho)
  ) {
    basename(caminho)
  } else {
    "selecionado"
  }

  detalhe <- if (inherits(erro, "condition")) {
    conditionMessage(erro)
  } else if (!is.null(erro) && length(erro) > 0L) {
    as.character(erro[[1L]])
  } else {
    ""
  }

  if (
    nzchar(detalhe) &&
      grepl("feche(-o)?|feche o arquivo", detalhe, ignore.case = TRUE)
  ) {
    return(detalhe)
  }

  mensagem <- paste0(
    "Nao foi possivel ", operacao, " o arquivo '", nome, "'. ",
    "Ele pode estar aberto ou bloqueado por outro programa. ",
    "Feche o arquivo e tente novamente."
  )
  if (nzchar(detalhe)) {
    mensagem <- paste0(mensagem, "\n\nDetalhes tecnicos: ", detalhe)
  }
  mensagem
}

interromper_operacao_arquivo <- function(
    caminho,
    operacao = "usar",
    erro = NULL
) {
  stop(
    mensagem_erro_operacao_arquivo(caminho, operacao, erro),
    call. = FALSE
  )
}

mostrar_erro_arquivo_shiny <- function(
    erro,
    caminho,
    operacao = "usar",
    session = shiny::getDefaultReactiveDomain()
) {
  mensagem <- mensagem_erro_operacao_arquivo(caminho, operacao, erro)
  if (!is.null(session) && requireNamespace("shiny", quietly = TRUE)) {
    shiny::showModal(
      shiny::modalDialog(
        title = "Feche o arquivo para continuar",
        shiny::tags$p(mensagem),
        easyClose = TRUE,
        footer = shiny::modalButton("Entendi")
      ),
      session = session
    )
  } else {
    message(mensagem)
  }
  invisible(NULL)
}

tentar_operacao_arquivo_shiny <- function(
    operacao,
    caminho,
    verbo = "usar",
    session = shiny::getDefaultReactiveDomain()
) {
  tryCatch(
    operacao(),
    error = function(erro) {
      mostrar_erro_arquivo_shiny(
        erro,
        caminho,
        operacao = verbo,
        session = session
      )
      NULL
    }
  )
}

validar_arquivo_para_leitura <- function(caminho, descricao = "arquivo") {
  if (length(caminho) != 1L || is.na(caminho) || !nzchar(caminho)) {
    stop(paste("Caminho inválido para", descricao, "."), call. = FALSE)
  }
  if (!file.exists(caminho)) {
    stop(paste("Arquivo não encontrado:", descricao, "-", caminho), call. = FALSE)
  }
  if (dir.exists(caminho)) {
    stop(paste("Era esperado um arquivo, mas foi encontrada uma pasta:", caminho), call. = FALSE)
  }
  if (file.access(caminho, 4L) != 0L) {
    stop(paste("Sem permissão para ler:", descricao, "-", caminho), call. = FALSE)
  }

  conexao <- suppressWarnings(tryCatch(
    file(caminho, open = "rb"),
    error = function(e) NULL
  ))
  if (is.null(conexao)) {
    interromper_operacao_arquivo(caminho, paste("ler", descricao))
  }
  close(conexao)

  invisible(normalizePath(caminho, winslash = "/", mustWork = TRUE))
}

arquivo_disponivel_para_gravacao <- function(caminho) {
  if (length(caminho) != 1L || is.na(caminho) || !nzchar(caminho)) {
    return(FALSE)
  }
  if (!file.exists(caminho)) {
    return(TRUE)
  }

  conexao <- suppressWarnings(tryCatch(
    file(caminho, open = "r+"),
    error = function(e) NULL
  ))
  if (is.null(conexao)) {
    return(FALSE)
  }
  close(conexao)
  TRUE
}

aguardar_arquivo_para_gravacao <- function(caminho, intervalo = 1) {
  if (arquivo_disponivel_para_gravacao(caminho)) {
    return(invisible(TRUE))
  }

  interromper_operacao_arquivo(caminho, "salvar")
}

validar_arquivo_para_gravacao <- function(caminho, descricao = "arquivo") {
  if (length(caminho) != 1L || is.na(caminho) || !nzchar(caminho)) {
    stop(paste("Caminho inválido para", descricao, "."), call. = FALSE)
  }

  pasta <- dirname(caminho)
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(pasta) || file.access(pasta, 2L) != 0L) {
    stop(paste("Sem permissão para gravar na pasta:", pasta), call. = FALSE)
  }
  if (file.exists(caminho) && file.access(caminho, 2L) != 0L) {
    stop(
      paste("Sem permissão para modificar:", descricao, "-", caminho),
      call. = FALSE
    )
  }
  aguardar_arquivo_para_gravacao(caminho)
  invisible(caminho)
}

ler_linhas_seguro <- function(caminho, ..., descricao = "arquivo") {
  validar_arquivo_para_leitura(caminho, descricao)
  tryCatch(
    base::readLines(caminho, ...),
    error = function(e) {
      interromper_operacao_arquivo(
        caminho,
        paste("ler", descricao),
        e
      )
    }
  )
}

gravar_com_seguranca <- function(caminho, operacao, descricao = "arquivo") {
  validar_arquivo_para_gravacao(caminho, descricao)
  tryCatch(
    {
      operacao(caminho)
      TRUE
    },
    error = function(e) {
      interromper_operacao_arquivo(
        caminho,
        paste("salvar", descricao),
        e
      )
    }
  )
}

gravar_linhas_seguro <- function(texto, caminho, ..., descricao = "arquivo") {
  gravar_com_seguranca(
    caminho,
    function(destino) base::writeLines(texto, destino, ...),
    descricao
  )
}

ler_tabela_segura <- function(caminho, leitor, ..., descricao = "tabela") {
  validar_arquivo_para_leitura(caminho, descricao)
  tryCatch(
    leitor(caminho, ...),
    error = function(e) {
      interromper_operacao_arquivo(
        caminho,
        paste("ler", descricao),
        e
      )
    }
  )
}

gravar_tabela_segura <- function(dados, caminho, gravador, ...) {
  tryCatch(
    gravar_com_seguranca(
      caminho,
      function(destino) gravador(dados, destino, ...),
      "tabela"
    ),
    error = function(e) {
      dominio <- if (requireNamespace("shiny", quietly = TRUE)) {
        shiny::getDefaultReactiveDomain()
      } else {
        NULL
      }
      if (!is.null(dominio)) {
        mostrar_erro_arquivo_shiny(
          e,
          caminho,
          operacao = "salvar",
          session = dominio
        )
      } else {
        warning(conditionMessage(e), call. = FALSE)
      }
      FALSE
    }
  )
}

salvar_rds_seguro <- function(objeto, caminho, ...) {
  gravar_com_seguranca(
    caminho,
    function(destino) base::saveRDS(objeto, destino, ...),
    "arquivo RDS"
  )
}

ler_rds_seguro <- function(caminho, ...) {
  validar_arquivo_para_leitura(caminho, "arquivo RDS")
  tryCatch(
    base::readRDS(caminho, ...),
    error = function(e) {
      interromper_operacao_arquivo(caminho, "ler o arquivo RDS", e)
    }
  )
}

copiar_arquivo_seguro <- function(origem, destino, sobrescrever = TRUE) {
  validar_arquivo_para_leitura(origem, "arquivo de origem")
  caminho_destino <- if (dir.exists(destino)) {
    file.path(destino, basename(origem))
  } else {
    destino
  }
  validar_arquivo_para_gravacao(caminho_destino, "arquivo de destino")

  ok <- file.copy(
    origem,
    destino,
    overwrite = sobrescrever,
    copy.mode = TRUE,
    copy.date = TRUE
  )
  if (!isTRUE(ok)) {
    stop(
      paste("Não foi possível copiar", origem, "para", destino),
      call. = FALSE
    )
  }
  TRUE
}

executar_cequal_monitorado <- function(
    executavel,
    diretorio,
    script_monitor = NULL,
    limite_inatividade = 60L,
    tempo_maximo = 21600L,
    verificar_resultados = TRUE,
    em_otimizacao = FALSE
) {
  if (is.null(script_monitor)) {
    diretorio_app <- get0(
      "app_dir",
      ifnotfound = getwd(),
      inherits = TRUE
    )
    script_monitor <- file.path(
      diretorio_app,
      "scripts",
      "executar_cequal_monitorado.ps1"
    )
  }
  validar_arquivo_para_leitura(executavel, "executável do CE-QUAL-W2")
  validar_arquivo_para_leitura(script_monitor, "monitor do CE-QUAL-W2")

  inicio_execucao <- Sys.time()
  saida <- suppressWarnings(
    system2(
      "powershell.exe",
      args = c(
        "-NoProfile",
        "-ExecutionPolicy", "Bypass",
        "-File", shQuote(script_monitor),
        "-Executavel", shQuote(executavel),
        "-Diretorio", shQuote(diretorio),
        "-LimiteInatividadeSegundos", as.integer(limite_inatividade),
        "-TempoMaximoSegundos", as.integer(tempo_maximo)
      ),
      stdout = TRUE,
      stderr = TRUE,
      wait = TRUE
    )
  )
  codigo_monitor <- attr(saida, "status")
  if (is.null(codigo_monitor)) codigo_monitor <- 0L

  arquivos_resultado <- list.files(
    diretorio,
    pattern = "^(FLOWBAL\\.OPT|tsr_.*\\.OPT)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  resultados_atualizados <- length(arquivos_resultado) > 0L &&
    any(
      file.info(arquivos_resultado)$mtime >=
        (inicio_execucao - 2),
      na.rm = TRUE
    )

  codigo <- as.integer(codigo_monitor)
  if (isTRUE(verificar_resultados)) {
    if (codigo == 0L && !resultados_atualizados) {
      saida <- c(saida, "RESULTADOS_NAO_ATUALIZADOS_NESTA_EXECUCAO")
      codigo <- 5L
    }
  }

  arquivo_log <- file.path(diretorio, "execucao_cequal_monitorada.log")
  gravar_linhas_seguro(
    c(
      paste("Data:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste("Código do monitor:", codigo_monitor),
      paste("Código final:", codigo),
      paste("Resultados atualizados:", resultados_atualizados),
      saida
    ),
    arquivo_log,
    descricao = "log da execução monitorada"
  )

  if (codigo != 0L && requireNamespace("shiny", quietly = TRUE)) {
    mensagem_usuario <- if (any(grepl("W2_ERRORDUMP_DETECTADO", saida))) {
      if (isTRUE(em_otimizacao)) {
        paste(
          "O CE-QUAL-W2 apresentou um erro e esta execução foi encerrada",
          "automaticamente. Ela receberá a penalidade e a otimização continuará."
        )
      } else {
        paste(
          "O CE-QUAL-W2 apresentou um erro e a simulação foi encerrada",
          "automaticamente. Os resultados desta execução foram ignorados."
        )
      }
    } else if (any(grepl("SAIDAS_SEM_ATUALIZACAO", saida))) {
      if (isTRUE(em_otimizacao)) {
        paste(
          "O CE-QUAL-W2 parou de atualizar os resultados e foi encerrado",
          "automaticamente. Esta execução receberá a penalidade."
        )
      } else {
        paste(
          "O CE-QUAL-W2 parou de atualizar os resultados e a simulação",
          "foi encerrada automaticamente."
        )
      }
    } else if (any(grepl("TEMPO_MAXIMO_EXCEDIDO", saida))) {
      paste(
        "Esta execução do CE-QUAL-W2 excedeu o tempo máximo permitido",
        "e foi encerrada automaticamente."
      )
    } else if (any(grepl("RESULTADOS_NAO_ATUALIZADOS", saida))) {
      paste(
        "O CE-QUAL-W2 terminou sem produzir resultados novos.",
        "Os arquivos anteriores não serão utilizados."
      )
    } else {
      paste(
        "Não foi possível concluir esta execução do CE-QUAL-W2.",
        "Consulte execucao_cequal_monitorada.log para os detalhes."
      )
    }

    shiny::showNotification(
      mensagem_usuario,
      type = "warning",
      duration = 10
    )
  }

  list(
    codigo = as.integer(codigo),
    codigo_monitor = as.integer(codigo_monitor),
    resultados_atualizados = resultados_atualizados,
    saida = saida,
    log = arquivo_log
  )
}

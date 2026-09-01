# Estruturas de contexto para simulações CE-QUAL-W2.
#
# As chaves legadas (dados_sim, dados_simulacao, diretorio_cequal e
# reserv_sigla) são mantidas temporariamente para que as funções científicas
# existentes continuem produzindo os mesmos resultados durante a refatoração.

.valor_contexto <- function(dados, nome, padrao = NULL) {
  if (is.null(dados) || !nome %in% names(dados)) {
    return(padrao)
  }

  valor <- dados[[nome]]
  if (length(valor) == 0L) padrao else valor[[1L]]
}

validar_contexto_simulacao <- function(contexto) {
  erros <- character()

  if (
    is.null(contexto$caminhos$diretorio) ||
    !dir.exists(contexto$caminhos$diretorio)
  ) {
    erros <- c(erros, "Diretório do CE-QUAL-W2 inválido.")
  }

  if (
    is.null(contexto$caminhos$controle) ||
    !file.exists(contexto$caminhos$controle)
  ) {
    erros <- c(erros, "Arquivo de controle .npt inválido.")
  }

  campos_obrigatorios <- c("inicio", "fim", "ano", "segmento", "tipo")
  campos_ausentes <- campos_obrigatorios[vapply(
    campos_obrigatorios,
    function(nome) {
      valor <- contexto$configuracao[[nome]]
      is.null(valor) || length(valor) == 0L || is.na(valor)
    },
    logical(1)
  )]

  if (length(campos_ausentes) > 0L) {
    erros <- c(
      erros,
      paste(
        "Configurações ausentes:",
        paste(campos_ausentes, collapse = ", ")
      )
    )
  }

  if (length(erros) > 0L) {
    stop(paste(erros, collapse = "\n"), call. = FALSE)
  }

  invisible(contexto)
}

criar_contexto_simulacao <- function(
    diretorio_cequal,
    arquivo_controle,
    reserv_sigla,
    dados_sim,
    dados_simulacao,
    validar = TRUE
) {
  diretorio_cequal <- normalizePath(
    diretorio_cequal,
    winslash = "/",
    mustWork = TRUE
  )
  arquivo_controle <- normalizePath(
    arquivo_controle,
    winslash = "/",
    mustWork = TRUE
  )

  contexto <- list(
    caminhos = list(
      diretorio = diretorio_cequal,
      controle = arquivo_controle
    ),
    configuracao = list(
      reservatorio = reserv_sigla,
      tipo = .valor_contexto(dados_sim, "tipo"),
      inicio = .valor_contexto(dados_sim, "TMSTRT"),
      fim = .valor_contexto(dados_sim, "TMEND"),
      ano = .valor_contexto(dados_sim, "YEAR"),
      segmento = .valor_contexto(dados_sim, "ITSR"),
      preproc = .valor_contexto(dados_sim, "preproc", 1)
    ),
    condicoes_iniciais = list(
      cota = .valor_contexto(dados_sim, "ELWS"),
      temperatura = .valor_contexto(dados_sim, "TEMP"),
      fosfato = .valor_contexto(dados_sim, "PO4"),
      oxigenio = .valor_contexto(dados_sim, "DO"),
      algas = .valor_contexto(dados_sim, "ALG1")
    ),
    observados = list(
      cota = dados_simulacao$cota_obs,
      temperatura = dados_simulacao$temp_obs,
      evaporacao = dados_simulacao$evap_obs,
      fosfato = dados_simulacao$fosfato_obs,
      nitrito = dados_simulacao$nitrito_obs,
      oxigenio = dados_simulacao$do_obs,
      clorofila = dados_simulacao$chla_obs
    ),
    monitoramento = dados_simulacao$dados_janela,

    # Compatibilidade temporária com o código existente.
    diretorio_cequal = diretorio_cequal,
    reserv_sigla = reserv_sigla,
    dados_sim = dados_sim,
    dados_simulacao = dados_simulacao
  )

  class(contexto) <- c("contexto_simulacao", "list")

  if (isTRUE(validar)) {
    validar_contexto_simulacao(contexto)
  }

  contexto
}

criar_contexto_janela <- function(contexto, indice, carregar_dados) {
  if (!inherits(contexto, "contexto_simulacao")) {
    stop("O objeto informado não é um contexto de simulação.", call. = FALSE)
  }
  if (!is.function(carregar_dados)) {
    stop("carregar_dados deve ser uma função.", call. = FALSE)
  }

  monitoramento <- contexto$monitoramento
  if (
    is.null(monitoramento) ||
    nrow(monitoramento) < 2L ||
    indice < 1L ||
    indice >= nrow(monitoramento)
  ) {
    stop("Índice de janela fora do intervalo disponível.", call. = FALSE)
  }

  dados_sim <- contexto$dados_sim
  dados_sim$TMSTRT <- monitoramento$JDAY[indice]
  dados_sim$TMEND <- monitoramento$JDAY[indice + 1L] + 1
  dados_sim$ELWS <- monitoramento$Cota[indice]
  dados_sim$TEMP <- monitoramento$`TempAgua(C)`[indice]

  dados_simulacao <- carregar_dados(
    contexto$caminhos$diretorio,
    dados_sim
  )
  if (is.null(dados_simulacao)) {
    stop("Não foi possível preparar os dados da janela.", call. = FALSE)
  }

  criar_contexto_simulacao(
    diretorio_cequal = contexto$caminhos$diretorio,
    arquivo_controle = contexto$caminhos$controle,
    reserv_sigla = contexto$configuracao$reservatorio,
    dados_sim = dados_sim,
    dados_simulacao = dados_simulacao
  )
}

preparar_entrada_simulacao <- function(
    contexto = NULL,
    diretorio_cequal = NULL,
    dados_simulacao = NULL
) {
  if (!is.null(contexto)) {
    if (!inherits(contexto, "contexto_simulacao")) {
      stop("O objeto informado não é um contexto de simulação.", call. = FALSE)
    }
    validar_contexto_simulacao(contexto)
    diretorio_cequal <- contexto$caminhos$diretorio
    dados_simulacao <- contexto$dados_simulacao

    # Os campos estruturados são a fonte principal. A lista legada continua
    # existindo somente para os campos ainda não migrados.
    dados_simulacao$julian_day_ini <- contexto$configuracao$inicio
    dados_simulacao$julian_day_fim <- contexto$configuracao$fim
    dados_simulacao$ano_simul <- contexto$configuracao$ano
    dados_simulacao$seg <- contexto$configuracao$segmento
    dados_simulacao$preproc <- contexto$configuracao$preproc
    dados_simulacao$tipo <- contexto$configuracao$tipo
    dados_simulacao$ELWS <- contexto$condicoes_iniciais$cota
    dados_simulacao$TEMP <- contexto$condicoes_iniciais$temperatura
    dados_simulacao$PO4 <- contexto$condicoes_iniciais$fosfato
    dados_simulacao$DO <- contexto$condicoes_iniciais$oxigenio
    dados_simulacao$ALG1 <- contexto$condicoes_iniciais$algas
    dados_simulacao$cota_obs <- contexto$observados$cota
    dados_simulacao$temp_obs <- contexto$observados$temperatura
    dados_simulacao$evap_obs <- contexto$observados$evaporacao
    dados_simulacao$fosfato_obs <- contexto$observados$fosfato
    dados_simulacao$nitrito_obs <- contexto$observados$nitrito
    dados_simulacao$do_obs <- contexto$observados$oxigenio
    dados_simulacao$chla_obs <- contexto$observados$clorofila
    dados_simulacao$dados_janela <- contexto$monitoramento
    dados_simulacao$arquivo_controle <- contexto$caminhos$controle
  }

  if (is.null(diretorio_cequal) || !dir.exists(diretorio_cequal)) {
    stop("Diretório do CE-QUAL-W2 inválido.", call. = FALSE)
  }
  if (is.null(dados_simulacao) || !is.list(dados_simulacao)) {
    stop("Dados da simulação ausentes ou inválidos.", call. = FALSE)
  }

  list(
    diretorio = normalizePath(
      diretorio_cequal,
      winslash = "/",
      mustWork = TRUE
    ),
    dados = dados_simulacao
  )
}

carregar_campos_simulacao <- function(dados_simulacao, campos, ambiente) {
  ausentes <- setdiff(campos, names(dados_simulacao))
  if (length(ausentes) > 0L) {
    stop(
      paste("Dados obrigatórios ausentes:", paste(ausentes, collapse = ", ")),
      call. = FALSE
    )
  }

  list2env(dados_simulacao[campos], envir = ambiente)
  invisible(dados_simulacao[campos])
}

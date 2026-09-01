# Conversor dos arquivos mestres da Engenharia Reversa para uma base RDS
# separada por reservatorio.
#
# Uso interativo:
#   source("scripts/engenharia_reversa/preparar_base_engenharia_reversa.R")
#   preparar_base_engenharia_reversa(
#     dir_origem = "C:/caminho/55_Engenharia_Reversa",
#     dir_destino = "C:/caminho/APP/dados_engenharia_reversa"
#   )
#
# Uso por linha de comando:
#   Rscript scripts/engenharia_reversa/preparar_base_engenharia_reversa.R DIR_ORIGEM DIR_DESTINO

.er_conv_data <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))

  texto <- sub(" .*", "", trimws(as.character(x)))
  resultado <- as.Date(texto, format = "%Y-%m-%d")
  pendentes <- is.na(resultado) & nzchar(texto)
  resultado[pendentes] <- as.Date(texto[pendentes], format = "%d/%m/%Y")
  resultado
}

.er_conv_numero <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NULL", "NA")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

.er_conv_coluna <- function(dados, alternativas, obrigatoria = TRUE) {
  nomes_normalizados <- toupper(gsub("[^A-Z0-9]", "", names(dados)))
  alternativas <- toupper(gsub("[^A-Z0-9]", "", alternativas))
  indice <- match(alternativas, nomes_normalizados, nomatch = 0L)
  indice <- indice[indice > 0L]

  if (length(indice) == 0L) {
    if (isTRUE(obrigatoria)) {
      stop(
        "Coluna nao encontrada. Alternativas: ",
        paste(alternativas, collapse = ", "),
        call. = FALSE
      )
    }
    return(NULL)
  }
  dados[[indice[[1L]]]]
}

.er_conv_ler_serie <- function(workbook, aba) {
  dados <- openxlsx::read.xlsx(workbook, sheet = aba)
  if (ncol(dados) < 2L) {
    stop("A aba ", aba, " nao possui Data e Cota.", call. = FALSE)
  }

  resultado <- data.frame(
    Data = .er_conv_data(dados[[1L]]),
    Cota_m = .er_conv_numero(dados[[2L]])
  )
  if (ncol(dados) >= 3L) {
    resultado$Volume_original_hm3 <- .er_conv_numero(dados[[3L]])
  }
  if (ncol(dados) >= 4L) {
    resultado$Volume_original_pct <- .er_conv_numero(dados[[4L]])
  }

  vazias <- is.na(resultado$Data) & is.na(resultado$Cota_m)
  resultado <- resultado[!vazias, , drop = FALSE]
  resultado <- resultado[order(resultado$Data), , drop = FALSE]
  rownames(resultado) <- NULL
  resultado
}

.er_conv_ler_retiradas <- function(
    id,
    wb_alterado,
    wb_original,
    abas_alterado,
    abas_original
) {
  aba <- as.character(id)
  if (aba %in% abas_alterado) {
    dados <- openxlsx::read.xlsx(wb_alterado, sheet = aba)
    if (ncol(dados) < 4L) {
      stop("Consumo alterado sem a quarta coluna para o ID ", id, ".")
    }
    valores_ls <- .er_conv_numero(dados[[4L]])
    fonte <- "consumos_por_acude_alterado.xlsx"
  } else if (aba %in% abas_original) {
    dados <- openxlsx::read.xlsx(wb_original, sheet = aba)
    if (ncol(dados) < 3L) {
      stop("Consumo original sem tres colunas para o ID ", id, ".")
    }
    c2 <- .er_conv_numero(dados[[2L]])
    c3 <- .er_conv_numero(dados[[3L]])
    c2[is.na(c2)] <- 0
    c3[is.na(c3)] <- 0
    valores_ls <- c2 + c3
    fonte <- "consumos_por_acude.xlsx"
  } else {
    return(list(
      dados = data.frame(
        Data = as.Date(character()),
        Retirada_m3s = numeric()
      ),
      fonte = NA_character_
    ))
  }

  resultado <- data.frame(
    Data = .er_conv_data(dados[[1L]]),
    Retirada_m3s = valores_ls / 1000
  )
  vazias <- is.na(resultado$Data) & is.na(resultado$Retirada_m3s)
  resultado <- resultado[!vazias, , drop = FALSE]
  resultado <- resultado[order(resultado$Data), , drop = FALSE]
  rownames(resultado) <- NULL

  list(dados = resultado, fonte = fonte)
}

.er_conv_cav <- function(id, cav_funceme, cav_oficial) {
  id <- as.integer(id)
  cav_funceme_id <- cav_funceme[
    as.integer(cav_funceme$cba_res_cod) == id,
    ,
    drop = FALSE
  ]

  if (nrow(cav_funceme_id) > 0L) {
    cav <- data.frame(
      Cota_m = .er_conv_numero(cav_funceme_id$cah_cota),
      Area_m2 = .er_conv_numero(cav_funceme_id$cah_area) * 1e6,
      Volume_m3 = .er_conv_numero(cav_funceme_id$cah_volume)
    )
    fonte <- "cav_banco_dados_funceme.csv"
  } else {
    cav_id <- cav_oficial[
      as.integer(cav_oficial$COD) == id,
      ,
      drop = FALSE
    ]
    cav <- data.frame(
      Cota_m = .er_conv_numero(cav_id$COTA),
      Area_m2 = .er_conv_numero(cav_id$AREA_KM2) * 1e6,
      Volume_m3 = .er_conv_numero(cav_id$VOLUME_M3)
    )
    fonte <- "DadosOficiaisCOGERH_155monitorados.xlsx:cav"
  }

  cav <- cav[stats::complete.cases(cav), , drop = FALSE]
  cav <- cav[order(cav$Cota_m), , drop = FALSE]
  duplicadas <- sum(duplicated(cav$Cota_m))
  cav <- cav[!duplicated(cav$Cota_m), , drop = FALSE]
  rownames(cav) <- NULL

  list(dados = cav, fonte = fonte, cotas_duplicadas = duplicadas)
}

.er_conv_parametros_vertedor <- function(linha_acude, vertedor) {
  id <- as.integer(linha_acude$COD[[1L]])
  resultado <- c(
    cota_vertedor_m = .er_conv_numero(linha_acude$CSANGR[[1L]]),
    largura_vertedor_m = .er_conv_numero(linha_acude$LSANGR[[1L]]),
    coeficiente_vertedor = .er_conv_numero(linha_acude$COEFSAGR[[1L]])
  )

  linha_vertedor <- vertedor[
    as.integer(vertedor$res_cod) == id,
    ,
    drop = FALSE
  ]
  if (nrow(linha_vertedor) > 0L) {
    atualizados <- c(
      cota_vertedor_m = .er_conv_numero(
        linha_vertedor$res_cota_sangria[[1L]]
      ),
      largura_vertedor_m = .er_conv_numero(
        linha_vertedor$res_largura_sangradouro[[1L]]
      ),
      coeficiente_vertedor = .er_conv_numero(
        linha_vertedor$res_coeficiente_sangradouro[[1L]]
      )
    )
    usar <- is.finite(atualizados)
    resultado[usar] <- atualizados[usar]
  }
  resultado
}

.er_conv_comportas <- function(id, comportas) {
  dados <- comportas[
    as.integer(comportas$RES_COD) == as.integer(id),
    ,
    drop = FALSE
  ]
  if (nrow(dados) == 0L) {
    return(data.frame(Data = as.Date(character()), Vazao_m3s = numeric()))
  }

  coluna_vazao <- .er_conv_coluna(
    dados,
    c("VAZAO.OSNY", "VAZAO OSNY", "VAZAO_OSNY")
  )
  resultado <- data.frame(
    Data = .er_conv_data(dados$DATA_HORA),
    Vazao_m3s = .er_conv_numero(coluna_vazao)
  )
  resultado <- resultado[!is.na(resultado$Data), , drop = FALSE]
  resultado <- resultado[order(resultado$Data), , drop = FALSE]
  rownames(resultado) <- NULL
  resultado
}

.er_conv_transferencias <- function(id, transferencias) {
  vazao <- .er_conv_numero(
    .er_conv_coluna(transferencias, c("VAZAO L/S", "VAZAO_L/S"))
  ) / 1000
  data <- .er_conv_data(transferencias$DATA)
  receptor <- as.integer(transferencias$COD_ACUDE_RECEPTOR)
  emissor <- as.integer(transferencias$COD_ACUDE_EMISSOR)

  indice_recebidas <- which(receptor == id)
  indice_enviadas <- which(emissor == id)
  recebidas <- data.frame(
    Data = data[indice_recebidas],
    Vazao_m3s = vazao[indice_recebidas],
    Outro_reservatorio_ID = emissor[indice_recebidas]
  )
  enviadas <- data.frame(
    Data = data[indice_enviadas],
    Vazao_m3s = vazao[indice_enviadas],
    Outro_reservatorio_ID = receptor[indice_enviadas]
  )
  list(recebidas = recebidas, enviadas = enviadas)
}

.er_conv_salvar_rds <- function(objeto, arquivo) {
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  temporario <- tempfile(
    pattern = "er_",
    tmpdir = dirname(arquivo),
    fileext = ".rds"
  )
  saveRDS(objeto, temporario, compress = "gzip", version = 3)
  if (!file.rename(temporario, arquivo)) {
    if (!file.copy(temporario, arquivo, overwrite = TRUE)) {
      unlink(temporario)
      stop("Nao foi possivel salvar: ", arquivo, call. = FALSE)
    }
    unlink(temporario)
  }
  invisible(arquivo)
}

.er_conv_manifesto_fontes <- function(arquivos) {
  info <- file.info(arquivos)
  data.frame(
    Arquivo = basename(arquivos),
    Tamanho_bytes = info$size,
    Modificado_em = info$mtime,
    MD5 = unname(tools::md5sum(arquivos)),
    stringsAsFactors = FALSE
  )
}

preparar_base_engenharia_reversa <- function(
    dir_origem,
    dir_destino,
    sobrescrever = TRUE,
    progresso = TRUE,
    ids_selecionados = NULL
) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("O pacote openxlsx e necessario para preparar a base.", call. = FALSE)
  }

  dir_origem <- normalizePath(dir_origem, winslash = "/", mustWork = TRUE)
  dir_destino <- normalizePath(
    dir_destino,
    winslash = "/",
    mustWork = FALSE
  )

  dir_input <- file.path(dir_origem, "Input")
  arquivos <- c(
    series = file.path(dir_input, "Series_reservatorios.xlsx"),
    consumos = file.path(dir_input, "consumos_por_acude.xlsx"),
    consumos_alterados = file.path(
      dir_input,
      "consumos_por_acude_alterado.xlsx"
    ),
    dados_oficiais = file.path(
      dir_input,
      "DadosOficiaisCOGERH_155monitorados.xlsx"
    ),
    chuva = file.path(dir_input, "chuva_acudes_diario.csv"),
    cav_funceme = file.path(dir_input, "cav_banco_dados_funceme.csv"),
    vertedor = file.path(dir_input, "vertedor_dados.csv")
  )
  ausentes <- arquivos[!file.exists(arquivos)]
  if (length(ausentes) > 0L) {
    stop(
      "Arquivo(s) de origem ausente(s):\n",
      paste(ausentes, collapse = "\n"),
      call. = FALSE
    )
  }

  if (dir.exists(dir_destino) && !isTRUE(sobrescrever)) {
    stop(
      "A pasta de destino ja existe e sobrescrever=FALSE:\n",
      dir_destino,
      call. = FALSE
    )
  }
  dir.create(
    file.path(dir_destino, "reservatorios"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  if (isTRUE(progresso)) message("Carregando arquivos mestres...")
  wb_series <- openxlsx::loadWorkbook(arquivos[["series"]])
  wb_consumos <- openxlsx::loadWorkbook(arquivos[["consumos"]])
  wb_consumos_alterados <- openxlsx::loadWorkbook(
    arquivos[["consumos_alterados"]]
  )
  wb_oficial <- openxlsx::loadWorkbook(arquivos[["dados_oficiais"]])

  acudes <- openxlsx::read.xlsx(wb_oficial, sheet = "acudes")
  cav_oficial <- openxlsx::read.xlsx(wb_oficial, sheet = "cav")
  evaporacao <- openxlsx::read.xlsx(wb_oficial, sheet = "evaporacao")
  comportas <- openxlsx::read.xlsx(wb_oficial, sheet = "comporta")
  transferencias <- openxlsx::read.xlsx(wb_oficial, sheet = "transf")

  chuva <- utils::read.csv(
    arquivos[["chuva"]],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(chuva)[[1L]] <- "Data"
  chuva$Data <- .er_conv_data(chuva$Data)
  cav_funceme <- utils::read.csv(
    arquivos[["cav_funceme"]],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  vertedor <- utils::read.csv(
    arquivos[["vertedor"]],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  abas_series <- names(wb_series)
  abas_consumos <- names(wb_consumos)
  abas_consumos_alterados <- names(wb_consumos_alterados)
  ids <- sort(unique(as.integer(acudes$COD[!is.na(acudes$COD)])))
  if (!is.null(ids_selecionados)) {
    ids_selecionados <- unique(as.integer(ids_selecionados))
    inexistentes <- setdiff(ids_selecionados, ids)
    if (length(inexistentes) > 0L) {
      stop(
        "ID(s) nao encontrado(s) na base oficial: ",
        paste(inexistentes, collapse = ", "),
        call. = FALSE
      )
    }
    ids <- ids[ids %in% ids_selecionados]
  }
  meses <- c(
    "JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
    "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"
  )
  gerado_em <- Sys.time()
  linhas_catalogo <- vector("list", length(ids))

  for (indice in seq_along(ids)) {
    id <- ids[[indice]]
    linha_acude <- acudes[as.integer(acudes$COD) == id, , drop = FALSE]
    nome <- as.character(linha_acude$CORPO[[1L]])
    if (isTRUE(progresso)) {
      message(
        sprintf(
          "[%d/%d - %.1f%%] %s (ID %d)",
          indice, length(ids), 100 * indice / length(ids), nome, id
        )
      )
    }

    problemas <- character()
    alertas <- character()
    aba <- as.character(id)

    niveis <- if (aba %in% abas_series) {
      .er_conv_ler_serie(wb_series, aba)
    } else {
      problemas <- c(problemas, "serie de niveis ausente")
      data.frame(Data = as.Date(character()), Cota_m = numeric())
    }

    retiradas_info <- .er_conv_ler_retiradas(
      id,
      wb_consumos_alterados,
      wb_consumos,
      abas_consumos_alterados,
      abas_consumos
    )
    retiradas <- retiradas_info$dados
    if (nrow(retiradas) == 0L) {
      problemas <- c(problemas, "serie de retiradas ausente")
    }

    coluna_chuva <- as.character(id)
    if (coluna_chuva %in% names(chuva)) {
      precipitacao <- data.frame(
        Data = chuva$Data,
        Precipitacao_mm = .er_conv_numero(chuva[[coluna_chuva]])
      )
    } else {
      precipitacao <- data.frame(
        Data = chuva$Data,
        Precipitacao_mm = NA_real_
      )
      alertas <- c(alertas, "coluna de precipitacao ausente")
    }

    cav_info <- .er_conv_cav(id, cav_funceme, cav_oficial)
    cav <- cav_info$dados
    if (nrow(cav) < 2L) {
      problemas <- c(problemas, "curva CAV invalida")
    }
    if (cav_info$cotas_duplicadas > 0L) {
      alertas <- c(
        alertas,
        paste(cav_info$cotas_duplicadas, "cota(s) duplicada(s) na CAV")
      )
    }

    estacao_evap <- as.integer(linha_acude$EstEvap[[1L]])
    linha_evap <- evaporacao[
      as.integer(evaporacao$COD) == estacao_evap,
      ,
      drop = FALSE
    ]
    evaporacao_mensal_mm <- if (nrow(linha_evap) > 0L) {
      valores <- .er_conv_numero(
        unlist(linha_evap[1L, 4:15], use.names = FALSE)
      )
      names(valores) <- meses
      valores
    } else {
      stats::setNames(rep(NA_real_, 12L), meses)
    }
    if (any(!is.finite(evaporacao_mensal_mm))) {
      problemas <- c(problemas, "evaporacao mensal incompleta")
    }

    parametros_vertedor <- .er_conv_parametros_vertedor(
      linha_acude,
      vertedor
    )
    if (any(!is.finite(parametros_vertedor))) {
      problemas <- c(problemas, "parametros do vertedor incompletos")
    }

    dados_comportas <- .er_conv_comportas(id, comportas)
    dados_transferencias <- .er_conv_transferencias(id, transferencias)

    duplicadas_niveis <- sum(duplicated(niveis$Data) & !is.na(niveis$Data))
    duplicadas_retiradas <- sum(
      duplicated(retiradas$Data) & !is.na(retiradas$Data)
    )
    if (duplicadas_niveis > 0L) {
      alertas <- c(
        alertas,
        paste(duplicadas_niveis, "data(s) duplicada(s) nos niveis")
      )
    }
    if (duplicadas_retiradas > 0L) {
      alertas <- c(
        alertas,
        paste(duplicadas_retiradas, "data(s) duplicada(s) nas retiradas")
      )
    }

    reservatorio <- list(
      id = id,
      nome = nome,
      municipio = as.character(linha_acude$MUNICIPIO[[1L]]),
      bacia = as.character(linha_acude$BACIA[[1L]]),
      capacidade_m3 = .er_conv_numero(linha_acude$CAPAC_M3[[1L]]),
      estacao_evaporacao_id = estacao_evap,
      cota_vertedor_m = unname(
        parametros_vertedor[["cota_vertedor_m"]]
      ),
      largura_vertedor_m = unname(
        parametros_vertedor[["largura_vertedor_m"]]
      ),
      coeficiente_vertedor = unname(
        parametros_vertedor[["coeficiente_vertedor"]]
      ),
      fator_vazao_bacia_hidraulica = .er_conv_numero(
        linha_acude$fator_vz_bacia_hidraulica[[1L]]
      )
    )

    objeto <- list(
      schema_version = "1.0.0",
      gerado_em = gerado_em,
      reservatorio = reservatorio,
      niveis = niveis,
      retiradas = retiradas,
      precipitacao = precipitacao,
      cav = cav,
      evaporacao_mensal_mm = evaporacao_mensal_mm,
      comportas = dados_comportas,
      transferencias = dados_transferencias,
      diagnostico = list(
        problemas = unique(problemas),
        alertas = unique(alertas),
        datas_duplicadas_niveis = duplicadas_niveis,
        datas_duplicadas_retiradas = duplicadas_retiradas,
        cotas_duplicadas_cav = cav_info$cotas_duplicadas,
        valores_ausentes_cota = sum(is.na(niveis$Cota_m)),
        valores_ausentes_retirada = sum(is.na(retiradas$Retirada_m3s)),
        valores_ausentes_precipitacao = sum(
          is.na(precipitacao$Precipitacao_mm)
        )
      ),
      fontes = list(
        niveis = "Series_reservatorios.xlsx",
        retiradas = retiradas_info$fonte,
        precipitacao = "chuva_acudes_diario.csv",
        cav = cav_info$fonte,
        metadados = "DadosOficiaisCOGERH_155monitorados.xlsx",
        vertedor = "vertedor_dados.csv",
        comportas = "DadosOficiaisCOGERH_155monitorados.xlsx:comporta",
        transferencias = "DadosOficiaisCOGERH_155monitorados.xlsx:transf"
      )
    )

    arquivo_relativo <- file.path(
      "reservatorios",
      sprintf("reservatorio_%04d.rds", id)
    )
    arquivo_destino <- file.path(dir_destino, arquivo_relativo)
    .er_conv_salvar_rds(objeto, arquivo_destino)

    data_inicio <- if (nrow(niveis) > 0L) {
      min(niveis$Data, na.rm = TRUE)
    } else {
      as.Date(NA)
    }
    data_fim <- if (nrow(niveis) > 0L) {
      max(niveis$Data, na.rm = TRUE)
    } else {
      as.Date(NA)
    }
    tem_transferencia <- (
      nrow(dados_transferencias$recebidas) +
        nrow(dados_transferencias$enviadas)
    ) > 0L

    linhas_catalogo[[indice]] <- data.frame(
      ID = id,
      Reservatorio = nome,
      Municipio = reservatorio$municipio,
      Bacia = reservatorio$bacia,
      Status = if (length(problemas) == 0L) "pronto" else "incompleto",
      Problemas = paste(unique(problemas), collapse = "; "),
      Alertas = paste(unique(alertas), collapse = "; "),
      Data_inicio_niveis = data_inicio,
      Data_fim_niveis = data_fim,
      Registros_niveis = nrow(niveis),
      Datas_duplicadas_niveis = duplicadas_niveis,
      Cotas_ausentes = sum(is.na(niveis$Cota_m)),
      Tem_retiradas = nrow(retiradas) > 0L,
      Tem_precipitacao = any(is.finite(precipitacao$Precipitacao_mm)),
      Tem_CAV = nrow(cav) >= 2L,
      Tem_vertedor = all(is.finite(parametros_vertedor)),
      Tem_evaporacao = all(is.finite(evaporacao_mensal_mm)),
      Tem_comporta = nrow(dados_comportas) > 0L,
      Tem_transferencia = tem_transferencia,
      Arquivo = gsub("\\\\", "/", arquivo_relativo),
      Tamanho_bytes = file.info(arquivo_destino)$size,
      stringsAsFactors = FALSE
    )
  }

  catalogo <- do.call(rbind, linhas_catalogo)
  catalogo <- catalogo[order(catalogo$Reservatorio, catalogo$ID), ]
  rownames(catalogo) <- NULL

  manifesto <- list(
    schema_version = "1.0.0",
    gerado_em = gerado_em,
    dir_origem = dir_origem,
    numero_reservatorios = nrow(catalogo),
    numero_prontos = sum(catalogo$Status == "pronto"),
    numero_incompletos = sum(catalogo$Status != "pronto"),
    fontes = .er_conv_manifesto_fontes(unname(arquivos))
  )

  .er_conv_salvar_rds(
    catalogo,
    file.path(dir_destino, "catalogo_reservatorios.rds")
  )
  .er_conv_salvar_rds(
    manifesto,
    file.path(dir_destino, "manifesto.rds")
  )
  utils::write.csv(
    catalogo,
    file.path(dir_destino, "catalogo_reservatorios.csv"),
    row.names = FALSE,
    na = ""
  )

  if (isTRUE(progresso)) {
    message(
      "Base concluida: ", nrow(catalogo), " reservatorios; ",
      manifesto$numero_prontos, " prontos; ",
      manifesto$numero_incompletos, " incompletos."
    )
  }
  invisible(list(catalogo = catalogo, manifesto = manifesto))
}

.er_executar_linha_comando <- function() {
  argumentos <- commandArgs(trailingOnly = TRUE)
  if (length(argumentos) == 0L) return(invisible(FALSE))
  if (length(argumentos) != 2L) {
    stop(
      "Uso: Rscript scripts/engenharia_reversa/preparar_base_engenharia_reversa.R ",
      "DIR_ORIGEM DIR_DESTINO",
      call. = FALSE
    )
  }
  preparar_base_engenharia_reversa(argumentos[[1L]], argumentos[[2L]])
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  .er_executar_linha_comando()
}

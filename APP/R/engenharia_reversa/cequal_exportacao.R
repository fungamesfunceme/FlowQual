# Modulo: exportacao dos resultados para o CE-QUAL-W2.

.er_cequal_slug <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) "Reservatorio" else x
}

.er_cequal_validar_areas <- function(areas_km2) {
  areas_km2 <- suppressWarnings(as.numeric(areas_km2))
  if (length(areas_km2) < 1L || length(areas_km2) > 5L) {
    stop("Informe entre 1 e 5 areas de contribuicao.", call. = FALSE)
  }
  if (any(!is.finite(areas_km2)) || any(areas_km2 <= 0)) {
    stop(
      "Todas as areas de contribuicao devem ser numeros maiores que zero.",
      call. = FALSE
    )
  }
  areas_km2
}

preparar_series_cequal_engenharia_reversa <- function(resultado, areas_km2) {
  colunas <- c(
    "Data", "Afluencia_calculada_m3s", "Retirada_m3s"
  )
  ausentes <- setdiff(colunas, names(resultado))
  if (length(ausentes) > 0L) {
    stop(
      "O resultado nao possui as colunas necessarias para exportar: ",
      paste(ausentes, collapse = ", "),
      call. = FALSE
    )
  }

  areas_km2 <- .er_cequal_validar_areas(areas_km2)
  datas <- as.Date(resultado$Data)
  afluencia_bruta <- as.numeric(resultado$Afluencia_calculada_m3s)
  retirada_original <- as.numeric(resultado$Retirada_m3s)
  if (any(is.na(datas))) {
    stop(
      "Existem linhas sem data valida; nao e possivel calcular o JDAY.",
      call. = FALSE
    )
  }
  afluencia_bruta[!is.finite(afluencia_bruta)] <- NA_real_
  retirada_original[!is.finite(retirada_original)] <- NA_real_
  if (any(retirada_original < 0, na.rm = TRUE)) {
    stop("A serie de retirada possui valores negativos.", call. = FALSE)
  }

  ordem <- order(datas)
  datas <- datas[ordem]
  afluencia_bruta <- afluencia_bruta[ordem]
  retirada_original <- retirada_original[ordem]

  correcao_retirada <- ifelse(
    is.na(afluencia_bruta),
    NA_real_,
    pmax(-afluencia_bruta, 0)
  )
  afluencia_corrigida <- ifelse(
    is.na(afluencia_bruta),
    NA_real_,
    pmax(afluencia_bruta, 0)
  )
  retirada_corrigida <- retirada_original + correcao_retirada
  linhas_com_na <- is.na(afluencia_corrigida) | is.na(retirada_corrigida)

  ano_inicial <- as.integer(format(min(datas), "%Y"))
  origem_jday <- as.Date(sprintf("%04d-01-01", ano_inicial))
  jday <- as.numeric(datas - origem_jday) + 1
  proporcoes <- areas_km2 / sum(areas_km2)

  qin <- lapply(proporcoes, function(p) afluencia_corrigida * p)
  qout <- lapply(seq_along(proporcoes), function(braco) {
    if (braco == 1L) retirada_corrigida else rep(0, length(retirada_corrigida))
  })

  list(
    datas = datas,
    jday = jday,
    qin = qin,
    qout = qout,
    areas_km2 = areas_km2,
    proporcoes = proporcoes,
    afluencia_bruta_m3s = afluencia_bruta,
    afluencia_corrigida_m3s = afluencia_corrigida,
    retirada_original_m3s = retirada_original,
    correcao_retirada_m3s = correcao_retirada,
    retirada_corrigida_m3s = retirada_corrigida,
    linhas_com_na = linhas_com_na,
    dias_com_na = sum(linhas_com_na),
    dias_corrigidos = sum(afluencia_bruta < 0, na.rm = TRUE),
    ano_inicial = ano_inicial,
    ano_final = as.integer(format(max(datas), "%Y"))
  )
}

.er_cequal_linhas_prn <- function(
    serie,
    valores,
    tipo = c("qin", "qout"),
    reservatorio,
    braco,
    proporcao
) {
  tipo <- match.arg(tipo)
  nome <- iconv(reservatorio, from = "", to = "ASCII//TRANSLIT")
  aviso_na <- if (serie$dias_com_na > 0L) {
    paste0(
      " | ATENCAO: ", serie$dias_com_na,
      " DIA(S) COM NA; CORRIGIR ANTES DE EXECUTAR"
    )
  } else {
    ""
  }
  if (tipo == "qin") {
    cabecalho <- paste0(
      serie$ano_inicial, " a ", serie$ano_final, ", ", nome,
      ", Braco ", braco, ", EngRev, proporcao ",
      formatC(proporcao, format = "f", digits = 6, decimal.mark = "."),
      aviso_na
    )
    colunas <- "JDAY    QIN"
    separador <- "----    ----"
    dados <- sprintf("%8.2f%8.3f", serie$jday, valores)
  } else {
    cabecalho <- paste0(
      serie$ano_inicial, " a ", serie$ano_final, ", ", nome,
      ", Braco ", braco, ", EngRev", aviso_na
    )
    colunas <- "JDAY    QOT"
    separador <- "----    ----"
    dados <- sprintf("%8.2f%8.2f", serie$jday, valores)
  }
  c(cabecalho, colunas, separador, dados)
}

escrever_arquivos_cequal_engenharia_reversa <- function(
    serie,
    reservatorio,
    pasta
) {
  if (!dir.exists(pasta) &&
      !dir.create(pasta, recursive = TRUE, showWarnings = FALSE)) {
    stop("Nao foi possivel criar a pasta temporaria de exportacao.", call. = FALSE)
  }

  identificador <- .er_cequal_slug(reservatorio)
  periodo <- paste0(serie$ano_inicial, "_", serie$ano_final)
  arquivos <- character()

  for (braco in seq_along(serie$proporcoes)) {
    nome_qin <- paste0("Qin", braco, "_", identificador, "_", periodo, ".prn")
    nome_qout <- paste0("Qout", braco, "_", identificador, "_", periodo, ".prn")
    caminho_qin <- file.path(pasta, nome_qin)
    caminho_qout <- file.path(pasta, nome_qout)

    writeLines(
      .er_cequal_linhas_prn(
        serie,
        serie$qin[[braco]],
        "qin",
        reservatorio,
        braco,
        serie$proporcoes[[braco]]
      ),
      caminho_qin,
      sep = "\r\n",
      useBytes = TRUE
    )
    writeLines(
      .er_cequal_linhas_prn(
        serie,
        serie$qout[[braco]],
        "qout",
        reservatorio,
        braco,
        serie$proporcoes[[braco]]
      ),
      caminho_qout,
      sep = "\r\n",
      useBytes = TRUE
    )
    arquivos <- c(arquivos, caminho_qin, caminho_qout)
  }

  if (serie$dias_com_na > 0L) {
    problema <- ifelse(
      is.na(serie$afluencia_corrigida_m3s) &
        is.na(serie$retirada_corrigida_m3s),
      "Afluencia e Qout sem valor",
      ifelse(
        is.na(serie$afluencia_corrigida_m3s),
        "Afluencia sem valor",
        "Qout sem valor"
      )
    )
    relatorio <- data.frame(
      Data = format(serie$datas, "%d/%m/%Y"),
      JDAY = serie$jday,
      Afluencia_bruta_m3s = serie$afluencia_bruta_m3s,
      Qin_total_corrigido_m3s = serie$afluencia_corrigida_m3s,
      Retirada_original_m3s = serie$retirada_original_m3s,
      Correcao_por_Qin_negativo_m3s = serie$correcao_retirada_m3s,
      Qout_braco_1_m3s = serie$retirada_corrigida_m3s,
      Problema = problema,
      stringsAsFactors = FALSE
    )
    relatorio <- relatorio[serie$linhas_com_na, , drop = FALSE]
    caminho_relatorio <- file.path(pasta, "relatorio_lacunas_CEQUAL.csv")
    utils::write.csv2(
      relatorio,
      caminho_relatorio,
      row.names = FALSE,
      na = "NA",
      fileEncoding = "UTF-8"
    )
    arquivos <- c(arquivos, caminho_relatorio)
  }
  arquivos
}

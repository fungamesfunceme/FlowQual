# Acesso sob demanda a base padronizada da Engenharia Reversa.
#
# A base e gerada por scripts/engenharia_reversa/preparar_base_engenharia_reversa.R
# e possui um arquivo
# RDS por reservatorio. Este arquivo nao depende do Shiny nem de pacotes
# externos.

.er_cache_reservatorios <- new.env(parent = emptyenv())

interpolar_lacunas_cota_engenharia_reversa <- function(niveis) {
  if (!is.data.frame(niveis) ||
      !all(c("Data", "Cota_m") %in% names(niveis))) {
    stop("Serie de niveis invalida para interpolacao.", call. = FALSE)
  }

  datas <- as.Date(niveis$Data)
  cotas <- suppressWarnings(as.numeric(niveis$Cota_m))
  validas <- !is.na(datas)
  datas <- datas[validas]
  cotas <- cotas[validas]
  if (length(datas) == 0L) {
    return(data.frame(
      Data = as.Date(character()),
      Cota_m = numeric(),
      Cota_interpolada = logical()
    ))
  }

  ordem <- order(datas)
  datas <- datas[ordem]
  cotas <- cotas[ordem]
  manter <- !duplicated(datas, fromLast = TRUE)
  datas <- datas[manter]
  cotas <- cotas[manter]
  observadas <- is.finite(cotas)

  if (sum(observadas) < 2L) {
    return(data.frame(
      Data = datas,
      Cota_m = cotas,
      Cota_interpolada = rep(FALSE, length(datas))
    ))
  }

  calendario <- seq(min(datas[observadas]), max(datas[observadas]), by = "day")
  indice <- match(calendario, datas)
  cota_calendario <- cotas[indice]
  era_observada <- !is.na(indice) & is.finite(cota_calendario)
  cota_preenchida <- stats::approx(
    x = as.numeric(datas[observadas]),
    y = cotas[observadas],
    xout = as.numeric(calendario),
    method = "linear",
    rule = 1,
    ties = "ordered"
  )$y

  data.frame(
    Data = calendario,
    Cota_m = cota_preenchida,
    Cota_interpolada = !era_observada & is.finite(cota_preenchida)
  )
}

localizar_base_engenharia_reversa <- function(app_dir = getwd()) {
  file.path(
    normalizePath(app_dir, winslash = "/", mustWork = TRUE),
    "dados_engenharia_reversa"
  )
}

ler_catalogo_engenharia_reversa <- function(base_dir) {
  arquivo <- file.path(base_dir, "catalogo_reservatorios.rds")
  if (!file.exists(arquivo)) {
    stop(
      "Catalogo da Engenharia Reversa nao encontrado:\n", arquivo,
      "\nExecute scripts/engenharia_reversa/preparar_base_engenharia_reversa.R ",
      "para gerar a base.",
      call. = FALSE
    )
  }

  catalogo <- readRDS(arquivo)
  colunas <- c("ID", "Reservatorio", "Status", "Arquivo")
  ausentes <- setdiff(colunas, names(catalogo))
  if (length(ausentes) > 0L) {
    stop(
      "O catalogo e invalido. Coluna(s) ausente(s): ",
      paste(ausentes, collapse = ", "),
      call. = FALSE
    )
  }

  catalogo
}

listar_reservatorios_engenharia_reversa <- function(
    base_dir,
    somente_prontos = FALSE
) {
  catalogo <- ler_catalogo_engenharia_reversa(base_dir)
  if (isTRUE(somente_prontos)) {
    catalogo <- catalogo[catalogo$Status == "pronto", , drop = FALSE]
  }
  catalogo[order(catalogo$Reservatorio, catalogo$ID), , drop = FALSE]
}

validar_dados_reservatorio_engenharia_reversa <- function(dados) {
  campos <- c(
    "schema_version", "reservatorio", "niveis", "retiradas",
    "precipitacao", "cav", "evaporacao_mensal_mm", "comportas",
    "transferencias", "diagnostico", "fontes"
  )
  ausentes <- setdiff(campos, names(dados))
  if (length(ausentes) > 0L) {
    stop(
      "Arquivo do reservatorio incompleto. Campo(s) ausente(s): ",
      paste(ausentes, collapse = ", "),
      call. = FALSE
    )
  }

  if (!identical(dados$schema_version, "1.0.0")) {
    stop(
      "Versao da base nao suportada: ", dados$schema_version,
      call. = FALSE
    )
  }
  if (!is.list(dados$reservatorio) ||
      is.null(dados$reservatorio$id) ||
      is.null(dados$reservatorio$nome)) {
    stop("Metadados do reservatorio invalidos.", call. = FALSE)
  }
  if (!is.data.frame(dados$niveis) ||
      !all(c("Data", "Cota_m") %in% names(dados$niveis))) {
    stop("Serie de niveis invalida.", call. = FALSE)
  }
  if (!is.data.frame(dados$retiradas) ||
      !all(c("Data", "Retirada_m3s") %in% names(dados$retiradas))) {
    stop("Serie de retiradas invalida.", call. = FALSE)
  }
  if (!is.data.frame(dados$precipitacao) ||
      !all(c("Data", "Precipitacao_mm") %in% names(dados$precipitacao))) {
    stop("Serie de precipitacao invalida.", call. = FALSE)
  }
  if (!is.data.frame(dados$cav) ||
      !all(c("Cota_m", "Area_m2", "Volume_m3") %in% names(dados$cav))) {
    stop("Curva CAV invalida.", call. = FALSE)
  }
  if (length(dados$evaporacao_mensal_mm) != 12L) {
    stop("Serie mensal de evaporacao invalida.", call. = FALSE)
  }

  invisible(dados)
}

carregar_reservatorio_engenharia_reversa <- function(
    id,
    base_dir,
    usar_cache = TRUE
) {
  id <- suppressWarnings(as.integer(id))
  if (length(id) != 1L || is.na(id)) {
    stop("Informe um ID de reservatorio valido.", call. = FALSE)
  }

  base_normalizada <- normalizePath(
    base_dir,
    winslash = "/",
    mustWork = TRUE
  )
  chave_cache <- paste(base_normalizada, id, sep = "::")
  if (isTRUE(usar_cache) &&
      exists(chave_cache, envir = .er_cache_reservatorios, inherits = FALSE)) {
    return(get(chave_cache, envir = .er_cache_reservatorios))
  }

  catalogo <- ler_catalogo_engenharia_reversa(base_normalizada)
  linha <- catalogo[catalogo$ID == id, , drop = FALSE]
  if (nrow(linha) != 1L) {
    stop(
      "Reservatorio ID ", id, " nao encontrado no catalogo.",
      call. = FALSE
    )
  }

  arquivo <- file.path(base_normalizada, linha$Arquivo[[1L]])
  if (!file.exists(arquivo)) {
    stop(
      "Arquivo do reservatorio nao encontrado:\n", arquivo,
      call. = FALSE
    )
  }

  dados <- readRDS(arquivo)
  validar_dados_reservatorio_engenharia_reversa(dados)
  if (!identical(as.integer(dados$reservatorio$id), id)) {
    stop(
      "O ID interno do arquivo nao corresponde ao catalogo.",
      call. = FALSE
    )
  }

  if (isTRUE(usar_cache)) {
    assign(chave_cache, dados, envir = .er_cache_reservatorios)
  }
  dados
}

limpar_cache_engenharia_reversa <- function() {
  rm(list = ls(envir = .er_cache_reservatorios), envir = .er_cache_reservatorios)
  invisible(TRUE)
}

.er_locf <- function(datas_origem, valores, datas_destino) {
  ordem <- order(datas_origem)
  datas_origem <- datas_origem[ordem]
  valores <- valores[ordem]
  indice <- findInterval(datas_destino, datas_origem)
  resultado <- rep(NA_real_, length(datas_destino))
  validos <- indice > 0L
  resultado[validos] <- valores[indice[validos]]
  resultado
}

montar_retiradas_totais_engenharia_reversa <- function(dados) {
  validar_dados_reservatorio_engenharia_reversa(dados)
  retiradas <- dados$retiradas[, c("Data", "Retirada_m3s"), drop = FALSE]
  retiradas <- retiradas[!is.na(retiradas$Data), , drop = FALSE]

  if (nrow(dados$comportas) == 0L) {
    return(retiradas)
  }

  comportas <- stats::aggregate(
    Vazao_m3s ~ Data,
    data = dados$comportas,
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  datas <- sort(unique(c(retiradas$Data, comportas$Data)))
  retirada_base <- .er_locf(
    retiradas$Data,
    retiradas$Retirada_m3s,
    datas
  )
  vazao_comporta <- comportas$Vazao_m3s[match(datas, comportas$Data)]
  vazao_comporta[is.na(vazao_comporta)] <- 0

  data.frame(
    Data = datas,
    Retirada_m3s = retirada_base + vazao_comporta
  )
}

preparar_entradas_calculo_engenharia_reversa <- function(dados) {
  validar_dados_reservatorio_engenharia_reversa(dados)
  parametros <- dados$reservatorio
  colunas_niveis <- c("Data", "Cota_m")
  if ("Cota_interpolada" %in% names(dados$niveis)) {
    colunas_niveis <- c(colunas_niveis, "Cota_interpolada")
  }

  list(
    niveis = dados$niveis[, colunas_niveis, drop = FALSE],
    retiradas = montar_retiradas_totais_engenharia_reversa(dados),
    cav = dados$cav[, c("Cota_m", "Area_m2", "Volume_m3"), drop = FALSE],
    evaporacao_mensal_mm = dados$evaporacao_mensal_mm,
    cota_vertedor_m = parametros$cota_vertedor_m,
    coeficiente_vertedor = parametros$coeficiente_vertedor,
    largura_vertedor_m = parametros$largura_vertedor_m,
    precipitacao = dados$precipitacao[
      ,
      c("Data", "Precipitacao_mm"),
      drop = FALSE
    ]
  )
}

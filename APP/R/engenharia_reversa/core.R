# Modulo: nucleo de calculo da Engenharia Reversa.
#
# Este arquivo contem apenas regras de negocio e nao depende do Shiny.
# A geracao dos arquivos QIN sera implementada em uma etapa posterior.
#
# Convencao do balanco hidrico diario:
#
#   Q_afluencia =
#     Delta_armazenamento + Evaporacao + Retirada + Vertimento - Precipitacao
#
# Todas as parcelas da equacao sao retornadas em m3/s. A curva CAV deve
# informar cota em metros, area em m2 e volume em m3.

.er_colunas_obrigatorias <- function(dados, colunas, nome) {
  if (!is.data.frame(dados)) {
    stop(nome, " deve ser um data.frame.", call. = FALSE)
  }

  ausentes <- setdiff(colunas, names(dados))
  if (length(ausentes) > 0L) {
    stop(
      nome, " nao possui a(s) coluna(s): ",
      paste(ausentes, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(dados)
}

.er_converter_data <- function(x, nome = "Data") {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  if (is.numeric(x)) {
    resultado <- as.Date(x, origin = "1899-12-30")
  } else {
    texto <- trimws(as.character(x))
    resultado <- as.Date(rep(NA_character_, length(texto)))

    formatos <- c("%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d")
    for (formato in formatos) {
      pendentes <- is.na(resultado) & nzchar(texto)
      if (!any(pendentes)) break
      resultado[pendentes] <- as.Date(texto[pendentes], format = formato)
    }
  }

  invalidas <- is.na(resultado) & !is.na(x)
  if (any(invalidas)) {
    stop(
      "Existem valores invalidos na coluna ", nome, ".",
      call. = FALSE
    )
  }

  resultado
}

.er_numerico <- function(x, nome) {
  nomes <- names(x)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) x <- gsub(",", ".", trimws(x), fixed = TRUE)

  resultado <- suppressWarnings(as.numeric(x))
  names(resultado) <- nomes
  invalidos <- is.na(resultado) & !is.na(x) & nzchar(trimws(as.character(x)))

  if (any(invalidos)) {
    stop(
      "A coluna ", nome, " possui valores que nao sao numericos.",
      call. = FALSE
    )
  }

  resultado
}

validar_curva_cav <- function(cav) {
  .er_colunas_obrigatorias(
    cav,
    c("Cota_m", "Area_m2", "Volume_m3"),
    "cav"
  )

  cav <- data.frame(
    Cota_m = .er_numerico(cav$Cota_m, "Cota_m"),
    Area_m2 = .er_numerico(cav$Area_m2, "Area_m2"),
    Volume_m3 = .er_numerico(cav$Volume_m3, "Volume_m3")
  )
  cav <- cav[stats::complete.cases(cav), , drop = FALSE]
  cav <- cav[order(cav$Cota_m), , drop = FALSE]
  rownames(cav) <- NULL

  if (nrow(cav) < 2L) {
    stop("A curva CAV precisa ter pelo menos dois pontos validos.", call. = FALSE)
  }
  if (anyDuplicated(cav$Cota_m)) {
    stop("A curva CAV possui cotas duplicadas.", call. = FALSE)
  }
  if (any(cav$Area_m2 < 0) || any(cav$Volume_m3 < 0)) {
    stop("A curva CAV possui area ou volume negativo.", call. = FALSE)
  }
  if (any(diff(cav$Volume_m3) < 0)) {
    stop("O volume da curva CAV deve ser crescente com a cota.", call. = FALSE)
  }

  cav
}

.er_interpolar_linear <- function(x, eixo_x, eixo_y) {
  resultado <- rep(NA_real_, length(x))
  validos <- is.finite(x)
  if (!any(validos)) return(resultado)

  xv <- x[validos]
  indice <- findInterval(xv, eixo_x, all.inside = TRUE)
  indice <- pmin(indice, length(eixo_x) - 1L)

  x1 <- eixo_x[indice]
  x2 <- eixo_x[indice + 1L]
  y1 <- eixo_y[indice]
  y2 <- eixo_y[indice + 1L]

  resultado[validos] <- y1 + (xv - x1) * (y2 - y1) / (x2 - x1)
  pmax(resultado, 0)
}

interpolar_curva_cav <- function(cota_m, cav, retornar = c("volume", "area")) {
  retornar <- match.arg(retornar)
  cav <- validar_curva_cav(cav)
  cota_m <- .er_numerico(cota_m, "cota_m")

  valores <- if (retornar == "volume") cav$Volume_m3 else cav$Area_m2
  .er_interpolar_linear(cota_m, cav$Cota_m, valores)
}

.er_dias_no_mes <- function(datas) {
  primeiro_dia <- as.Date(format(datas, "%Y-%m-01"))
  proximo_mes <- as.Date(vapply(
    primeiro_dia,
    function(data) {
      format(seq(data, by = "month", length.out = 2L)[2L], "%Y-%m-%d")
    },
    character(1)
  ))
  as.integer(proximo_mes - primeiro_dia)
}

calcular_volume_vertido <- function(
    cota_inicial_m,
    cota_final_m,
    cota_vertedor_m,
    coeficiente_vertedor,
    largura_vertedor_m,
    segundos = 24 * 60 * 60
) {
  cota_inicial_m <- .er_numerico(cota_inicial_m, "cota_inicial_m")
  cota_final_m <- .er_numerico(cota_final_m, "cota_final_m")

  n <- max(length(cota_inicial_m), length(cota_final_m))
  cota_inicial_m <- rep_len(cota_inicial_m, n)
  cota_final_m <- rep_len(cota_final_m, n)
  resultado <- rep(NA_real_, n)

  parametros_validos <- all(is.finite(c(
    cota_vertedor_m,
    coeficiente_vertedor,
    largura_vertedor_m,
    segundos
  )))
  if (!parametros_validos) return(resultado)

  sem_dados <- !is.finite(cota_inicial_m) | !is.finite(cota_final_m)
  sem_vertimento <- !sem_dados &
    cota_inicial_m <= cota_vertedor_m &
    cota_final_m <= cota_vertedor_m
  resultado[sem_vertimento] <- 0

  candidatos <- which(!sem_dados & !sem_vertimento)
  for (i in candidatos) {
    c1 <- cota_inicial_m[i]
    c2 <- cota_final_m[i]
    duracao <- segundos

    if (isTRUE(all.equal(c1, c2))) {
      resultado[i] <- coeficiente_vertedor * largura_vertedor_m *
        duracao * (c1 - cota_vertedor_m)^(3 / 2)
      next
    }

    if (c1 < cota_vertedor_m) {
      duracao <- duracao * (c2 - cota_vertedor_m) / (c2 - c1)
      c1 <- cota_vertedor_m
    }
    if (c2 < cota_vertedor_m) {
      duracao <- duracao * (c1 - cota_vertedor_m) / (c1 - c2)
      c2 <- cota_vertedor_m
    }

    h1 <- c1 - cota_vertedor_m
    h2 <- c2 - cota_vertedor_m

    resultado[i] <- coeficiente_vertedor * largura_vertedor_m *
      (2 / 5) * duracao / (h2 - h1) *
      (h2^(5 / 2) - h1^(5 / 2))
  }

  resultado
}

.er_preparar_serie <- function(
    dados,
    coluna_valor,
    nome,
    datas_duplicadas = c("primeiro", "ultimo", "erro")
) {
  datas_duplicadas <- match.arg(datas_duplicadas)
  .er_colunas_obrigatorias(dados, c("Data", coluna_valor), nome)

  resultado <- data.frame(
    Data = .er_converter_data(dados$Data, paste0(nome, "$Data")),
    Valor = .er_numerico(dados[[coluna_valor]], coluna_valor)
  )
  resultado <- resultado[order(resultado$Data), , drop = FALSE]

  numero_duplicadas <- sum(duplicated(resultado$Data))
  if (numero_duplicadas > 0L) {
    if (datas_duplicadas == "erro") {
      stop(nome, " possui datas duplicadas.", call. = FALSE)
    }

    manter <- if (datas_duplicadas == "primeiro") {
      !duplicated(resultado$Data)
    } else {
      !duplicated(resultado$Data, fromLast = TRUE)
    }
    resultado <- resultado[manter, , drop = FALSE]
    warning(
      nome, " possui ", numero_duplicadas,
      " registro(s) com data duplicada; foi mantido o ",
      datas_duplicadas, " valor de cada data.",
      call. = FALSE
    )
  }
  if (nrow(resultado) == 0L || all(is.na(resultado$Data))) {
    stop(nome, " nao possui datas validas.", call. = FALSE)
  }

  attr(resultado, "numero_datas_duplicadas") <- numero_duplicadas
  resultado
}

.er_expandir_niveis <- function(niveis, datas) {
  indice <- match(datas, niveis$Data)
  niveis$Valor[indice]
}

.er_expandir_retiradas <- function(retiradas, datas) {
  indice <- findInterval(datas, retiradas$Data)
  resultado <- rep(NA_real_, length(datas))
  validos <- indice > 0L
  resultado[validos] <- retiradas$Valor[indice[validos]]
  resultado
}

.er_expandir_precipitacao <- function(
    precipitacao,
    datas,
    ausente = c("erro", "zero")
) {
  ausente <- match.arg(ausente)
  indice <- match(datas, precipitacao$Data)
  resultado <- precipitacao$Valor[indice]

  faltantes <- is.na(resultado)
  if (any(faltantes) && ausente == "erro") {
    stop(
      "A precipitacao possui ", sum(faltantes),
      " dia(s) ausente(s) no periodo calculado.",
      call. = FALSE
    )
  }
  resultado[faltantes] <- 0
  resultado
}

calcular_afluencia_total <- function(
    niveis,
    retiradas,
    cav,
    evaporacao_mensal_mm,
    cota_vertedor_m,
    coeficiente_vertedor,
    largura_vertedor_m,
    precipitacao = NULL,
    banda_incerteza = c("media", "inferior", "superior"),
    precisao_cota_m = 0.005,
    tratar_negativos = c("manter", "zerar"),
    precipitacao_ausente = c("erro", "zero"),
    datas_duplicadas = c("primeiro", "ultimo", "erro")
) {
  banda_incerteza <- match.arg(banda_incerteza)
  tratar_negativos <- match.arg(tratar_negativos)
  precipitacao_ausente <- match.arg(precipitacao_ausente)
  datas_duplicadas <- match.arg(datas_duplicadas)

  datas_cota_interpolada <- as.Date(character())
  if ("Cota_interpolada" %in% names(niveis)) {
    marcador <- as.logical(niveis$Cota_interpolada)
    marcador[is.na(marcador)] <- FALSE
    datas_cota_interpolada <- .er_converter_data(
      niveis$Data[marcador],
      "niveis$Data"
    )
  }

  niveis <- .er_preparar_serie(
    niveis,
    "Cota_m",
    "niveis",
    datas_duplicadas
  )
  retiradas <- .er_preparar_serie(
    retiradas,
    "Retirada_m3s",
    "retiradas",
    datas_duplicadas
  )
  cav <- validar_curva_cav(cav)

  evaporacao_mensal_mm <- .er_numerico(
    evaporacao_mensal_mm,
    "evaporacao_mensal_mm"
  )
  if (length(evaporacao_mensal_mm) != 12L ||
      any(!is.finite(evaporacao_mensal_mm)) ||
      any(evaporacao_mensal_mm < 0)) {
    stop(
      "evaporacao_mensal_mm deve conter 12 valores mensais validos em mm.",
      call. = FALSE
    )
  }
  if (!is.numeric(precisao_cota_m) ||
      length(precisao_cota_m) != 1L ||
      !is.finite(precisao_cota_m) ||
      precisao_cota_m < 0) {
    stop("precisao_cota_m deve ser um numero nao negativo.", call. = FALSE)
  }
  parametros_vertedor <- c(
    cota_vertedor_m = cota_vertedor_m,
    coeficiente_vertedor = coeficiente_vertedor,
    largura_vertedor_m = largura_vertedor_m
  )
  parametros_vertedor <- .er_numerico(
    parametros_vertedor,
    "parametros do vertedor"
  )
  if (length(parametros_vertedor) != 3L ||
      any(!is.finite(parametros_vertedor)) ||
      parametros_vertedor[["coeficiente_vertedor"]] < 0 ||
      parametros_vertedor[["largura_vertedor_m"]] < 0) {
    stop(
      "Cota, coeficiente e largura do vertedor devem ser numeros validos; ",
      "coeficiente e largura nao podem ser negativos.",
      call. = FALSE
    )
  }
  cota_vertedor_m <- parametros_vertedor[["cota_vertedor_m"]]
  coeficiente_vertedor <- parametros_vertedor[["coeficiente_vertedor"]]
  largura_vertedor_m <- parametros_vertedor[["largura_vertedor_m"]]

  if (is.null(precipitacao)) {
    precipitacao <- data.frame(
      Data = niveis$Data,
      Precipitacao_mm = 0
    )
  }
  precipitacao <- .er_preparar_serie(
    precipitacao,
    "Precipitacao_mm",
    "precipitacao",
    datas_duplicadas
  )

  diagnostico_duplicadas <- c(
    niveis = attr(niveis, "numero_datas_duplicadas"),
    retiradas = attr(retiradas, "numero_datas_duplicadas"),
    precipitacao = attr(precipitacao, "numero_datas_duplicadas")
  )

  inicio <- max(
    min(niveis$Data),
    min(retiradas$Data),
    min(precipitacao$Data)
  )
  fim <- min(
    max(niveis$Data),
    max(retiradas$Data),
    max(precipitacao$Data)
  )
  if (!is.finite(inicio) || !is.finite(fim) || inicio >= fim) {
    stop(
      "As series nao possuem pelo menos dois dias em comum.",
      call. = FALSE
    )
  }

  datas <- seq(inicio, fim, by = "day")
  cota <- .er_expandir_niveis(niveis, datas)
  retirada_m3s <- .er_expandir_retiradas(retiradas, datas)
  precipitacao_mm <- .er_expandir_precipitacao(
    precipitacao,
    datas,
    ausente = precipitacao_ausente
  )

  if (any(is.na(retirada_m3s))) {
    stop(
      "Nao foi possivel determinar a retirada em todos os dias.",
      call. = FALSE
    )
  }
  if (any(retirada_m3s < 0, na.rm = TRUE)) {
    stop("A serie de retiradas possui valores negativos.", call. = FALSE)
  }
  if (any(precipitacao_mm < 0, na.rm = TRUE)) {
    stop("A serie de precipitacao possui valores negativos.", call. = FALSE)
  }

  cota_inicial <- head(cota, -1L)
  cota_final <- tail(cota, -1L)
  datas_resultado <- head(datas, -1L)
  cota_inicial_interpolada <- head(datas %in% datas_cota_interpolada, -1L)
  cota_final_interpolada <- tail(datas %in% datas_cota_interpolada, -1L)
  retirada_resultado <- head(retirada_m3s, -1L)
  precipitacao_resultado <- head(precipitacao_mm, -1L)

  if (banda_incerteza == "inferior") {
    cota_inicial <- cota_inicial + precisao_cota_m
    cota_final <- cota_final - precisao_cota_m
  } else if (banda_incerteza == "superior") {
    cota_inicial <- cota_inicial - precisao_cota_m
    cota_final <- cota_final + precisao_cota_m
  }

  volume_inicial_m3 <- .er_interpolar_linear(
    cota_inicial,
    cav$Cota_m,
    cav$Volume_m3
  )
  volume_final_m3 <- .er_interpolar_linear(
    cota_final,
    cav$Cota_m,
    cav$Volume_m3
  )
  area_inicial_m2 <- .er_interpolar_linear(
    cota_inicial,
    cav$Cota_m,
    cav$Area_m2
  )
  area_final_m2 <- .er_interpolar_linear(
    cota_final,
    cav$Cota_m,
    cav$Area_m2
  )
  area_media_m2 <- rowMeans(cbind(area_inicial_m2, area_final_m2))

  segundos_dia <- 24 * 60 * 60
  delta_volume_m3s <- (volume_final_m3 - volume_inicial_m3) / segundos_dia

  mes <- as.integer(format(datas_resultado, "%m"))
  dias_mes <- .er_dias_no_mes(datas_resultado)
  lamina_evaporada_m <- evaporacao_mensal_mm[mes] / dias_mes / 1000
  evaporacao_m3s <- lamina_evaporada_m * area_media_m2 / segundos_dia

  volume_vertido_m3 <- calcular_volume_vertido(
    cota_inicial,
    cota_final,
    cota_vertedor_m,
    coeficiente_vertedor,
    largura_vertedor_m,
    segundos = segundos_dia
  )
  vertimento_m3s <- volume_vertido_m3 / segundos_dia
  precipitacao_m3s <- precipitacao_resultado / 1000 *
    area_media_m2 / segundos_dia

  afluencia_calculada_m3s <- delta_volume_m3s +
    evaporacao_m3s +
    retirada_resultado +
    vertimento_m3s -
    precipitacao_m3s

  afluencia_negativa_m3s <- ifelse(
    is.na(afluencia_calculada_m3s),
    NA_real_,
    pmin(afluencia_calculada_m3s, 0)
  )
  afluencia_total_m3s <- afluencia_calculada_m3s
  if (tratar_negativos == "zerar") {
    afluencia_total_m3s <- pmax(afluencia_total_m3s, 0)
  }

  fora_cav <- is.finite(cota_inicial) & is.finite(cota_final) &
    (
      cota_inicial < min(cav$Cota_m) |
      cota_inicial > max(cav$Cota_m) |
      cota_final < min(cav$Cota_m) |
      cota_final > max(cav$Cota_m)
    )
  cota_ausente <- is.na(cota_inicial) | is.na(cota_final)
  negativa <- afluencia_calculada_m3s < 0

  flag <- rep("OK", length(datas_resultado))
  flag[cota_ausente] <- "COTA_AUSENTE"
  flag[fora_cav & !cota_ausente] <- "EXTRAPOLACAO_CAV"
  flag[negativa & !cota_ausente] <- ifelse(
    flag[negativa & !cota_ausente] == "OK",
    "AFLUENCIA_NEGATIVA",
    paste(flag[negativa & !cota_ausente], "AFLUENCIA_NEGATIVA", sep = ";")
  )
  cota_interpolada <- cota_inicial_interpolada | cota_final_interpolada
  flag[cota_interpolada] <- ifelse(
    flag[cota_interpolada] == "OK",
    "COTA_INTERPOLADA",
    paste(flag[cota_interpolada], "COTA_INTERPOLADA", sep = ";")
  )

  resultado <- data.frame(
    Data = datas_resultado,
    Cota_inicial_m = cota_inicial,
    Cota_final_m = cota_final,
    Cota_inicial_interpolada = cota_inicial_interpolada,
    Cota_final_interpolada = cota_final_interpolada,
    Volume_inicial_hm3 = volume_inicial_m3 / 1e6,
    Area_media_m2 = area_media_m2,
    Delta_volume_m3s = delta_volume_m3s,
    Evaporacao_m3s = evaporacao_m3s,
    Retirada_m3s = retirada_resultado,
    Vertimento_m3s = vertimento_m3s,
    Precipitacao_m3s = precipitacao_m3s,
    Afluencia_calculada_m3s = afluencia_calculada_m3s,
    Afluencia_negativa_m3s = afluencia_negativa_m3s,
    Afluencia_total_m3s = afluencia_total_m3s,
    Flag = flag,
    stringsAsFactors = FALSE
  )

  estrutura <- delta_volume_m3s +
    evaporacao_m3s +
    retirada_resultado +
    vertimento_m3s -
    precipitacao_m3s
  residuo <- abs(afluencia_calculada_m3s - estrutura)
  if (any(residuo > 1e-10, na.rm = TRUE)) {
    stop("Falha interna na verificacao do balanco hidrico.", call. = FALSE)
  }

  attr(resultado, "configuracao") <- list(
    banda_incerteza = banda_incerteza,
    precisao_cota_m = precisao_cota_m,
    tratar_negativos = tratar_negativos,
    precipitacao_ausente = precipitacao_ausente,
    datas_duplicadas = datas_duplicadas
  )
  attr(resultado, "diagnostico") <- list(
    datas_duplicadas = diagnostico_duplicadas
  )
  class(resultado) <- c("engenharia_reversa_resultado", class(resultado))
  resultado
}

resumir_afluencia_total <- function(resultado) {
  if (!inherits(resultado, "engenharia_reversa_resultado")) {
    stop(
      "resultado deve ser produzido por calcular_afluencia_total().",
      call. = FALSE
    )
  }

  valores_validos <- resultado$Afluencia_total_m3s[
    is.finite(resultado$Afluencia_total_m3s)
  ]
  media <- if (length(valores_validos) > 0L) {
    mean(valores_validos)
  } else {
    NA_real_
  }
  maxima <- if (length(valores_validos) > 0L) {
    max(valores_validos)
  } else {
    NA_real_
  }

  data.frame(
    Data_inicial = min(resultado$Data),
    Data_final = max(resultado$Data),
    Numero_dias = nrow(resultado),
    Dias_validos = sum(is.finite(resultado$Afluencia_calculada_m3s)),
    Dias_sem_cota = sum(grepl("COTA_AUSENTE", resultado$Flag, fixed = TRUE)),
    Dias_com_extrapolacao = sum(grepl(
      "EXTRAPOLACAO_CAV",
      resultado$Flag,
      fixed = TRUE
    )),
    Dias_com_afluencia_negativa = sum(
      resultado$Afluencia_calculada_m3s < 0,
      na.rm = TRUE
    ),
    Afluencia_media_m3s = media,
    Afluencia_maxima_m3s = maxima
  )
}

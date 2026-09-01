# Modulo: atualizacao das series dinamicas pela API FUNCEME.
#
# A API e a fonte preferencial para cota e retirada. O RDS continua contendo
# os parametros estaticos e funciona como contingencia quando a rede falha.

.er_api_url_series <-
  "https://apil5.funceme.br/rpc/v1/reservatorio-series"
.er_api_url_retiradas <-
  "https://apil5.funceme.br/rest/acude/vazao"

.er_api_cache_dir <- function() {
  raiz <- Sys.getenv("LOCALAPPDATA", unset = "")
  if (nzchar(raiz)) {
    return(file.path(raiz, "SWAT_CEQUAL", "engenharia_reversa", "api"))
  }
  file.path(
    tools::R_user_dir("SWAT_CEQUAL", which = "cache"),
    "engenharia_reversa",
    "api"
  )
}

.er_api_arquivo_cache <- function(reservatorio_id, cache_dir = .er_api_cache_dir()) {
  file.path(cache_dir, paste0("reservatorio_", as.integer(reservatorio_id), ".rds"))
}

.er_api_requisitar <- function(url, query, tentativas = 1L, timeout_seg = 12) {
  resposta <- tryCatch(
    httr::RETRY(
      verb = "GET",
      url = url,
      query = query,
      times = as.integer(tentativas),
      pause_base = 1,
      pause_cap = 3,
      terminate_on = c(400, 401, 403, 404),
      quiet = TRUE,
      httr::timeout(timeout_seg),
      httr::user_agent("SWAT-CEQUAL-W2/Engenharia-Reversa")
    ),
    error = function(e) {
      stop(
        "Nao foi possivel conectar a API FUNCEME. Verifique a internet, ",
        "o certificado de seguranca do Windows e o antivirus. Detalhes: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  codigo <- httr::status_code(resposta)
  if (codigo < 200L || codigo >= 300L) {
    stop(
      "A API FUNCEME respondeu com o codigo HTTP ", codigo, ".",
      call. = FALSE
    )
  }

  texto <- httr::content(resposta, as = "text", encoding = "UTF-8")
  tryCatch(
    jsonlite::fromJSON(texto, flatten = TRUE),
    error = function(e) {
      stop(
        "A API FUNCEME retornou uma resposta que nao e JSON valido. Detalhes: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

.er_api_extrair_lista <- function(json, nome_serie) {
  dados <- tryCatch(json$data$list, error = function(e) NULL)
  if (is.null(dados) || !is.data.frame(dados) || nrow(dados) == 0L) {
    stop("A API nao retornou dados de ", nome_serie, ".", call. = FALSE)
  }
  dados
}

.er_api_numero <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(x)), fixed = TRUE)))
}

.er_api_normalizar_niveis <- function(json) {
  dados <- .er_api_extrair_lista(json, "cota")
  ausentes <- setdiff(c("data", "nivel"), names(dados))
  if (length(ausentes) > 0L) {
    stop(
      "A resposta de cota mudou de formato. Campo(s) ausente(s): ",
      paste(ausentes, collapse = ", "),
      call. = FALSE
    )
  }

  resultado <- data.frame(
    Data = as.Date(dados$data),
    Cota_m = .er_api_numero(dados$nivel)
  )
  resultado <- resultado[!is.na(resultado$Data), , drop = FALSE]
  resultado <- resultado[!duplicated(resultado$Data, fromLast = TRUE), , drop = FALSE]
  resultado[order(resultado$Data), , drop = FALSE]
}

.er_api_normalizar_retiradas <- function(json) {
  dados <- .er_api_extrair_lista(json, "retirada")
  ausentes <- setdiff(c("data", "valor"), names(dados))
  if (length(ausentes) > 0L) {
    stop(
      "A resposta de retirada mudou de formato. Campo(s) ausente(s): ",
      paste(ausentes, collapse = ", "),
      call. = FALSE
    )
  }

  # O Portal Hidrologico fornece 'valor' em L/s. O calculo usa m3/s.
  resultado <- data.frame(
    Data = as.Date(dados$data),
    Retirada_m3s = .er_api_numero(dados$valor) / 1000
  )
  resultado <- resultado[
    !is.na(resultado$Data) & !is.na(resultado$Retirada_m3s),
    ,
    drop = FALSE
  ]
  resultado <- resultado[!duplicated(resultado$Data, fromLast = TRUE), , drop = FALSE]
  resultado[order(resultado$Data), , drop = FALSE]
}

.er_api_mesclar <- function(anterior, atual, coluna_valor) {
  anterior <- anterior[, c("Data", coluna_valor), drop = FALSE]
  atual <- atual[, c("Data", coluna_valor), drop = FALSE]
  combinado <- rbind(anterior, atual)
  combinado$Data <- as.Date(combinado$Data)
  combinado <- combinado[!is.na(combinado$Data), , drop = FALSE]
  combinado <- combinado[!duplicated(combinado$Data, fromLast = TRUE), , drop = FALSE]
  combinado[order(combinado$Data), , drop = FALSE]
}

.er_api_ler_cache <- function(reservatorio_id, cache_dir = .er_api_cache_dir()) {
  arquivo <- .er_api_arquivo_cache(reservatorio_id, cache_dir)
  if (!file.exists(arquivo)) return(NULL)

  tryCatch({
    cache <- readRDS(arquivo)
    if (!is.list(cache) ||
        !identical(as.integer(cache$reservatorio_id), as.integer(reservatorio_id))) {
      return(NULL)
    }
    cache
  }, error = function(e) NULL)
}

.er_api_salvar_cache <- function(cache, cache_dir = .er_api_cache_dir()) {
  if (!dir.exists(cache_dir) &&
      !dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)) {
    warning("Nao foi possivel criar a pasta de cache da API.", call. = FALSE)
    return(invisible(FALSE))
  }

  destino <- .er_api_arquivo_cache(cache$reservatorio_id, cache_dir)
  temporario <- tempfile("er_api_", tmpdir = cache_dir, fileext = ".rds")
  on.exit(unlink(temporario, force = TRUE), add = TRUE)
  saveRDS(cache, temporario, version = 2)
  ok <- file.copy(temporario, destino, overwrite = TRUE)
  if (!isTRUE(ok)) {
    warning("Nao foi possivel atualizar o cache local da API.", call. = FALSE)
  }
  invisible(isTRUE(ok))
}

aplicar_cache_api_engenharia_reversa <- function(
    dados,
    cache_dir = .er_api_cache_dir()
) {
  id <- as.integer(dados$reservatorio$id)
  cache <- .er_api_ler_cache(id, cache_dir)
  if (is.null(cache)) {
    return(list(
      dados = dados,
      info = list(
        fonte_niveis = "RDS",
        fonte_retiradas = "RDS",
        atualizado_em = NULL,
        mensagem = "API ainda nao consultada; usando as series do RDS."
      )
    ))
  }

  if (is.data.frame(cache$niveis) && nrow(cache$niveis) > 0L) {
    dados$niveis <- .er_api_mesclar(dados$niveis, cache$niveis, "Cota_m")
  }
  if (is.data.frame(cache$retiradas) && nrow(cache$retiradas) > 0L) {
    dados$retiradas <- .er_api_mesclar(
      dados$retiradas,
      cache$retiradas,
      "Retirada_m3s"
    )
  }

  list(
    dados = dados,
    info = list(
      fonte_niveis = "cache local",
      fonte_retiradas = "cache local",
      atualizado_em = cache$atualizado_em,
      mensagem = "Usando a ultima atualizacao local da API."
    )
  )
}

atualizar_api_engenharia_reversa <- function(
    dados,
    data_fim = Sys.Date(),
    cache_dir = .er_api_cache_dir()
) {
  id <- as.integer(dados$reservatorio$id)
  data_fim <- as.Date(data_fim)
  ultima_cota <- suppressWarnings(max(as.Date(dados$niveis$Data), na.rm = TRUE))
  if (!is.finite(as.numeric(ultima_cota))) ultima_cota <- data_fim - 365
  data_inicio <- min(ultima_cota - 30, data_fim - 30)

  cache_anterior <- .er_api_ler_cache(id, cache_dir)
  erros <- character()
  fonte_niveis <- "RDS"
  fonte_retiradas <- "RDS"
  niveis_api <- if (!is.null(cache_anterior)) cache_anterior$niveis else NULL
  retiradas_api <- if (!is.null(cache_anterior)) cache_anterior$retiradas else NULL

  novos_niveis <- tryCatch({
    json <- .er_api_requisitar(
      .er_api_url_series,
      list(
        reservatorio_id = id,
        data_inicio = format(data_inicio, "%Y-%m-%d"),
        data_fim = format(data_fim, "%Y-%m-%d")
      )
    )
    .er_api_normalizar_niveis(json)
  }, error = function(e) {
    erros <<- c(erros, paste("Cota:", conditionMessage(e)))
    NULL
  })
  if (!is.null(novos_niveis)) {
    niveis_api <- if (is.data.frame(niveis_api)) {
      .er_api_mesclar(niveis_api, novos_niveis, "Cota_m")
    } else {
      novos_niveis
    }
    fonte_niveis <- "API"
  } else if (is.data.frame(niveis_api) && nrow(niveis_api) > 0L) {
    fonte_niveis <- "cache local"
  }

  novas_retiradas <- tryCatch({
    json <- .er_api_requisitar(
      .er_api_url_retiradas,
      list(reservatorio_id = id, orderBy = "data", limit = 0)
    )
    .er_api_normalizar_retiradas(json)
  }, error = function(e) {
    erros <<- c(erros, paste("Retirada:", conditionMessage(e)))
    NULL
  })
  if (!is.null(novas_retiradas)) {
    retiradas_api <- novas_retiradas
    fonte_retiradas <- "API"
  } else if (is.data.frame(retiradas_api) && nrow(retiradas_api) > 0L) {
    fonte_retiradas <- "cache local"
  }

  houve_api <- identical(fonte_niveis, "API") || identical(fonte_retiradas, "API")
  atualizado_em <- if (houve_api) Sys.time() else if (!is.null(cache_anterior)) {
    cache_anterior$atualizado_em
  } else {
    NULL
  }

  if (is.data.frame(niveis_api) && nrow(niveis_api) > 0L) {
    dados$niveis <- .er_api_mesclar(dados$niveis, niveis_api, "Cota_m")
  }
  if (is.data.frame(retiradas_api) && nrow(retiradas_api) > 0L) {
    dados$retiradas <- .er_api_mesclar(
      dados$retiradas,
      retiradas_api,
      "Retirada_m3s"
    )
  }

  if (houve_api) {
    .er_api_salvar_cache(
      list(
        schema_version = "1.0.0",
        reservatorio_id = id,
        atualizado_em = atualizado_em,
        niveis = niveis_api,
        retiradas = retiradas_api
      ),
      cache_dir
    )
  }

  mensagem <- if (length(erros) == 0L) {
    "Cota e retirada atualizadas pelo Portal Hidrologico."
  } else if (houve_api) {
    paste("Atualizacao parcial.", paste(erros, collapse = " "))
  } else {
    paste(
      "A API nao respondeu; foram mantidas as melhores series locais disponiveis.",
      paste(erros, collapse = " ")
    )
  }

  list(
    dados = dados,
    info = list(
      fonte_niveis = fonte_niveis,
      fonte_retiradas = fonte_retiradas,
      atualizado_em = atualizado_em,
      mensagem = mensagem,
      erros = erros
    )
  )
}

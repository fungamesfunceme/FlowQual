# Modulo: interface e servidor da aba de Engenharia Reversa.
#
# Este modulo apenas calcula a afluencia total ao reservatorio. A distribuicao
# dessa vazao entre rios e a escrita dos arquivos QIN serao adicionadas depois.

.er_ui_card <- function(titulo, valor, detalhe = NULL, classe = "") {
  shiny::div(
    class = paste("er-summary-card", classe),
    shiny::div(class = "er-summary-label", titulo),
    shiny::div(class = "er-summary-value", valor),
    if (!is.null(detalhe)) {
      shiny::div(class = "er-summary-detail", detalhe)
    }
  )
}

.er_formatar_numero <- function(x, digitos = 2L, sufixo = "") {
  if (length(x) == 0L || !is.finite(x[[1L]])) return("--")
  paste0(
    formatC(
      x[[1L]],
      format = "f",
      digits = digitos,
      big.mark = ".",
      decimal.mark = ","
    ),
    sufixo
  )
}

.er_formatar_data <- function(x) {
  if (length(x) == 0L || is.na(x[[1L]])) return("--")
  format(as.Date(x[[1L]]), "%d/%m/%Y")
}

.er_slug <- function(x) {
  texto <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  texto <- tolower(gsub("[^a-zA-Z0-9]+", "_", texto))
  gsub("^_+|_+$", "", texto)
}

.er_valor <- function(x, nome, padrao = NA) {
  if (is.null(x[[nome]]) || length(x[[nome]]) == 0L) return(padrao)
  x[[nome]][[1L]]
}

.er_resumir_diagnostico <- function(dados) {
  nomes_duplicadas <- grep(
    "^datas_duplicadas",
    names(dados$diagnostico),
    value = TRUE
  )
  if (length(nomes_duplicadas) > 0L) {
    valores_duplicadas <- unlist(
      dados$diagnostico[nomes_duplicadas],
      recursive = TRUE,
      use.names = FALSE
    )
    duplicadas <- suppressWarnings(
      sum(as.numeric(valores_duplicadas), na.rm = TRUE)
    )
  } else {
    duplicadas <- 0
  }
  if (length(duplicadas) == 0L || !is.finite(duplicadas)) {
    duplicadas <- 0
  }

  contar_linhas <- function(x) {
    if (is.data.frame(x)) return(nrow(x))
    if (!is.list(x) || length(x) == 0L) return(0L)
    sum(vapply(
      x,
      function(item) {
        if (is.data.frame(item)) nrow(item) else 0L
      },
      integer(1)
    ))
  }

  list(
    datas_duplicadas = as.integer(duplicadas),
    registros_transferencia = as.integer(contar_linhas(dados$transferencias))
  )
}

.er_intervalo_disponivel <- function(dados) {
  datas_nivel <- as.Date(dados$niveis$Data)
  retiradas <- montar_retiradas_totais_engenharia_reversa(dados)
  datas_retirada <- as.Date(retiradas$Data)

  datas_nivel <- datas_nivel[!is.na(datas_nivel)]
  datas_retirada <- datas_retirada[!is.na(datas_retirada)]

  if (
    length(datas_nivel) == 0L ||
      length(datas_retirada) == 0L
  ) {
    return(as.Date(c(NA, NA)))
  }

  inicio <- max(
    min(datas_nivel),
    min(datas_retirada)
  )
  fim <- max(datas_nivel)

  if (is.na(inicio) || is.na(fim) || inicio >= fim) {
    return(as.Date(c(NA, NA)))
  }
  as.Date(c(inicio, fim))
}

.er_retiradas_periodo <- function(retiradas, inicio, fim) {
  retiradas <- retiradas[
    !is.na(retiradas$Data) & !is.na(retiradas$Retirada_m3s),
    c("Data", "Retirada_m3s"),
    drop = FALSE
  ]
  retiradas$Data <- as.Date(retiradas$Data)
  retiradas <- retiradas[order(retiradas$Data), , drop = FALSE]

  indice_inicio <- findInterval(inicio, retiradas$Data)
  indice_fim <- findInterval(fim, retiradas$Data)
  if (indice_inicio == 0L || indice_fim == 0L) {
    stop(
      "Nao existe uma retirada conhecida no inicio do periodo.",
      call. = FALSE
    )
  }

  intermediarias <- retiradas[
    retiradas$Data > inicio & retiradas$Data < fim,
    ,
    drop = FALSE
  ]
  resultado <- rbind(
    data.frame(
      Data = inicio,
      Retirada_m3s = retiradas$Retirada_m3s[[indice_inicio]]
    ),
    intermediarias,
    data.frame(
      Data = fim,
      Retirada_m3s = retiradas$Retirada_m3s[[indice_fim]]
    )
  )
  resultado[!duplicated(resultado$Data, fromLast = TRUE), , drop = FALSE]
}

engenharia_reversa_tab_ui <- function() {
  shiny::tabPanel(
    title = "Engenharia Reversa",
    value = "engenharia_reversa",

    shiny::tags$style(shiny::HTML("
      .er-shell {
        --er-navy: #123149;
        --er-blue: #166b8f;
        --er-aqua: #2aa6a1;
        --er-pale: #edf7f7;
        --er-line: #d6e2e7;
        --er-text: #223744;
        padding: 20px 4px 30px;
        color: var(--er-text);
      }
      .er-hero {
        border-radius: 16px;
        padding: 24px 28px;
        margin-bottom: 18px;
        color: #fff;
        background:
          radial-gradient(circle at 88% 10%, rgba(255,255,255,.17), transparent 26%),
          linear-gradient(120deg, var(--er-navy), var(--er-blue) 65%, var(--er-aqua));
        box-shadow: 0 12px 28px rgba(18,49,73,.16);
      }
      .er-kicker {
        margin-bottom: 7px;
        font-size: 11px;
        font-weight: 800;
        letter-spacing: .15em;
        text-transform: uppercase;
        opacity: .82;
      }
      .er-hero h3 {
        margin: 0 0 8px;
        font-weight: 700;
      }
      .er-hero p {
        max-width: 850px;
        margin: 0;
        line-height: 1.5;
        opacity: .9;
      }
      .er-panel {
        min-height: 100%;
        margin-bottom: 18px;
        padding: 20px;
        border: 1px solid var(--er-line);
        border-radius: 14px;
        background: #fff;
        box-shadow: 0 6px 18px rgba(25,56,74,.06);
      }
      .er-panel-title {
        display: flex;
        gap: 10px;
        align-items: center;
        margin: 0 0 16px;
        color: var(--er-navy);
        font-size: 17px;
        font-weight: 700;
      }
      .er-step {
        display: inline-flex;
        width: 26px;
        height: 26px;
        align-items: center;
        justify-content: center;
        border-radius: 50%;
        color: #fff;
        background: var(--er-blue);
        font-size: 12px;
      }
      .er-status {
        margin: 12px 0 4px;
        padding: 11px 13px;
        border-radius: 9px;
        font-size: 13px;
        line-height: 1.45;
      }
      .er-status-ok { color: #17633d; background: #eaf7ef; border: 1px solid #bfe3cd; }
      .er-status-warn { color: #765715; background: #fff7df; border: 1px solid #ead79b; }
      .er-status-error { color: #8b2d2d; background: #fff0f0; border: 1px solid #efc4c4; }
      .er-advanced {
        margin-top: 8px;
        padding: 10px 12px;
        border: 1px solid #d8e3e9;
        border-radius: 9px;
        background: #f8fbfc;
      }
      .er-advanced > summary {
        color: var(--er-navy);
        font-weight: 700;
        cursor: pointer;
        user-select: none;
      }
      .er-advanced-help {
        margin: 10px 0;
        color: #58707d;
        font-size: 12px;
        line-height: 1.45;
      }
      .er-meta-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 10px;
      }
      .er-meta-item {
        min-height: 67px;
        padding: 11px 13px;
        border-radius: 10px;
        background: #f5f8fa;
      }
      .er-meta-label {
        color: #6c808c;
        font-size: 11px;
        font-weight: 700;
        letter-spacing: .05em;
        text-transform: uppercase;
      }
      .er-meta-value {
        margin-top: 4px;
        color: var(--er-navy);
        font-size: 15px;
        font-weight: 650;
      }
      .er-note {
        margin-top: 13px;
        padding: 12px 14px;
        border-left: 4px solid var(--er-aqua);
        border-radius: 4px 9px 9px 4px;
        color: #49616e;
        background: var(--er-pale);
        font-size: 12px;
        line-height: 1.5;
      }
      .er-actions {
        display: flex;
        gap: 10px;
        align-items: center;
        flex-wrap: wrap;
        margin-top: 9px;
      }
      .er-calculate.btn {
        padding: 10px 20px;
        border: 0;
        border-radius: 9px;
        color: #fff;
        background: var(--er-blue);
        font-weight: 700;
        box-shadow: 0 5px 12px rgba(22,107,143,.2);
      }
      .er-summary-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 12px;
        margin: 2px 0 18px;
      }
      .er-summary-card {
        min-height: 112px;
        padding: 16px;
        border: 1px solid var(--er-line);
        border-radius: 12px;
        background: #fff;
      }
      .er-summary-card.er-accent { border-top: 4px solid var(--er-aqua); }
      .er-summary-label {
        color: #6c808c;
        font-size: 11px;
        font-weight: 750;
        letter-spacing: .05em;
        text-transform: uppercase;
      }
      .er-summary-value {
        margin-top: 7px;
        color: var(--er-navy);
        font-size: 24px;
        font-weight: 750;
        line-height: 1.1;
      }
      .er-summary-detail {
        margin-top: 7px;
        color: #72858f;
        font-size: 11px;
      }
      .er-result-head {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        flex-wrap: wrap;
        margin: 8px 0 12px;
      }
      .er-result-head h4 { margin: 0; color: var(--er-navy); font-weight: 700; }
      .er-downloads { display: flex; gap: 8px; flex-wrap: wrap; }
      .er-downloads .btn { border-radius: 8px; }
      .er-arm-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 10px;
      }
      .er-arm-share {
        margin-top: 5px;
        color: #58707d;
        font-size: 12px;
      }
      .er-empty {
        margin-top: 8px;
        padding: 38px 20px;
        border: 1px dashed #b9cbd3;
        border-radius: 13px;
        color: #617783;
        background: #f8fbfc;
        text-align: center;
      }
      .er-tabs .nav-tabs > li.active > a {
        color: var(--er-blue);
        font-weight: 700;
      }
      @media (max-width: 900px) {
        .er-summary-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      }
      @media (max-width: 600px) {
        .er-meta-grid, .er-summary-grid, .er-arm-grid { grid-template-columns: 1fr; }
        .er-hero { padding: 20px; }
      }
    ")),

    shiny::div(
      class = "er-shell",
      shiny::div(
        class = "er-hero",
        shiny::div(class = "er-kicker", "Balanco hidrico diario"),
        shiny::tags$h3("Engenharia Reversa da Afluencia"),
        shiny::tags$p(
          paste(
            "Estime a vazao afluente a partir da variacao do armazenamento,",
            "evaporacao, retiradas e vertimento. A precipitacao e desconsiderada."
          )
        )
      ),

      shiny::fluidRow(
        shiny::column(
          width = 5,
          shiny::div(
            class = "er-panel",
            shiny::div(
              class = "er-panel-title",
              shiny::span(class = "er-step", "1"),
              "Selecao e configuracao"
            ),
            shiny::uiOutput("er_seletor_reservatorio"),
            shiny::uiOutput("er_status_base"),
            shiny::actionButton(
              "er_atualizar_api",
              "Atualizar cota e retirada pelo Portal",
              icon = shiny::icon("rotate"),
              class = "btn-default btn-sm"
            ),
            shiny::uiOutput("er_status_api"),
            shiny::dateRangeInput(
              "er_periodo",
              "Periodo de calculo",
              start = Sys.Date() - 30,
              end = Sys.Date(),
              format = "dd/mm/yyyy",
              separator = " ate ",
              language = "pt-BR",
              width = "100%"
            ),
            shiny::selectInput(
              "er_negativos",
              "Afluência negativa",
              choices = c(
                "Manter valor" = "manter",
                "Substituir por zero" = "zerar"
              ),
              selected = "manter",
              width = "100%"
            ),
            shiny::tags$details(
              class = "er-advanced",
              shiny::tags$summary("Opções avançadas"),
              shiny::tags$p(
                class = "er-advanced-help",
                paste(
                  "Opcional: considere a incerteza de ±0,5 cm na medição",
                  "do nível. O valor observado não aplica nenhum ajuste às cotas."
                )
              ),
              shiny::selectInput(
                "er_banda",
                "Incerteza da medição do nível",
                choices = c(
                  "Valor observado (sem ajuste)" = "media",
                  "Limite inferior da estimativa" = "inferior",
                  "Limite superior da estimativa" = "superior"
                ),
                selected = "media",
                width = "100%"
              )
            ),
            shiny::div(
              class = "er-note",
              shiny::tags$b("Precipitacao no balanco: "),
              paste(
                "desconsiderada por decisao metodologica; o calculo utiliza",
                "0 mm em todos os dias do periodo."
              )
            ),
            shiny::div(
              class = "er-actions",
              shiny::actionButton(
                "er_calcular",
                "Calcular afluencia",
                icon = shiny::icon("calculator"),
                class = "er-calculate"
              )
            )
          )
        ),
        shiny::column(
          width = 7,
          shiny::div(
            class = "er-panel",
            shiny::div(
              class = "er-panel-title",
              shiny::span(class = "er-step", "2"),
              "Dados do reservatorio"
            ),
            shiny::uiOutput("er_metadados"),
            shiny::uiOutput("er_diagnostico"),
            shiny::div(
              class = "er-note",
              shiny::tags$b("Escopo desta etapa: "),
              paste(
                "o resultado representa a afluencia total ao reservatorio.",
                "As transferencias existentes sao preservadas na base, mas ainda",
                "nao entram no balanco. A divisao por rio e o arquivo QIN serao",
                "implementados na proxima etapa."
              )
            )
          )
        )
      ),
      shiny::uiOutput("er_resultado_ui"),
      shiny::uiOutput("er_cequal_export_ui")
    )
  )
}

registrar_servidor_engenharia_reversa <- function(
    input,
    output,
    session,
    app_dir
) {
  base_dir <- localizar_base_engenharia_reversa(app_dir)
  erro_catalogo <- NULL
  catalogo <- tryCatch(
    listar_reservatorios_engenharia_reversa(base_dir),
    error = function(e) {
      erro_catalogo <<- conditionMessage(e)
      data.frame()
    }
  )

  resultado <- shiny::reactiveVal(NULL)
  erro_calculo <- shiny::reactiveVal(NULL)
  avisos_calculo <- shiny::reactiveVal(character())
  dados_api <- shiny::reactiveVal(NULL)
  info_api <- shiny::reactiveVal(NULL)

  output$er_seletor_reservatorio <- shiny::renderUI({
    if (nrow(catalogo) == 0L) {
      return(
        shiny::selectInput(
          "er_reservatorio_id",
          "Reservatorio",
          choices = character(),
          width = "100%"
        )
      )
    }

    rotulo_status <- ifelse(
      catalogo$Status == "pronto",
      "",
      " [dados incompletos]"
    )
    rotulos <- paste0(
      catalogo$Reservatorio,
      " (ID ", catalogo$ID, ")",
      rotulo_status
    )
    escolhas <- stats::setNames(as.character(catalogo$ID), rotulos)
    prontos <- catalogo$ID[catalogo$Status == "pronto"]
    selecionado <- if (length(prontos) > 0L) prontos[[1L]] else catalogo$ID[[1L]]

    shiny::selectizeInput(
      "er_reservatorio_id",
      "Reservatorio",
      choices = escolhas,
      selected = as.character(selecionado),
      options = list(
        placeholder = "Digite o nome ou o ID...",
        create = FALSE
      ),
      width = "100%"
    )
  })

  linha_catalogo <- shiny::reactive({
    shiny::req(nrow(catalogo) > 0L, input$er_reservatorio_id)
    linha <- catalogo[
      catalogo$ID == suppressWarnings(as.integer(input$er_reservatorio_id)),
      ,
      drop = FALSE
    ]
    shiny::validate(
      shiny::need(nrow(linha) == 1L, "Reservatorio nao encontrado no catalogo.")
    )
    linha
  })

  dados_reservatorio_base <- shiny::reactive({
    linha <- linha_catalogo()
    carregar_reservatorio_engenharia_reversa(
      linha$ID[[1L]],
      base_dir,
      usar_cache = TRUE
    )
  })

  dados_reservatorio <- shiny::reactive({
    linha <- linha_catalogo()
    atual <- dados_api()
    if (!is.null(atual) &&
        identical(as.integer(atual$id), as.integer(linha$ID[[1L]]))) {
      return(atual$dados)
    }
    dados_reservatorio_base()
  })

  output$er_status_base <- shiny::renderUI({
    if (!is.null(erro_catalogo)) {
      return(
        shiny::div(
          class = "er-status er-status-error",
          shiny::icon("circle-exclamation"),
          " ",
          erro_catalogo
        )
      )
    }
    if (nrow(catalogo) == 0L) return(NULL)

    linha <- linha_catalogo()
    if (identical(linha$Status[[1L]], "pronto")) {
      shiny::div(
        class = "er-status er-status-ok",
        shiny::icon("circle-check"),
        " Base pronta para o calculo."
      )
    } else {
      detalhe <- if ("Pendencias" %in% names(linha)) {
        linha$Pendencias[[1L]]
      } else {
        "Existem parametros obrigatorios ausentes."
      }
      shiny::div(
        class = "er-status er-status-warn",
        shiny::icon("triangle-exclamation"),
        " Calculo bloqueado: ",
        detalhe
      )
    }
  })

  output$er_status_api <- shiny::renderUI({
    info <- info_api()
    if (is.null(info)) return(NULL)

    ambas_api <- identical(info$fonte_niveis, "API") &&
      identical(info$fonte_retiradas, "API")
    classe <- if (ambas_api) "er-status er-status-ok" else "er-status er-status-warn"
    atualizado <- if (!is.null(info$atualizado_em) &&
        length(info$atualizado_em) > 0L &&
        !is.na(info$atualizado_em[[1L]])) {
      paste0(
        " Atualizacao: ",
        format(as.POSIXct(info$atualizado_em[[1L]]), "%d/%m/%Y %H:%M")
      )
    } else {
      ""
    }

    shiny::div(
      class = classe,
      shiny::icon(if (ambas_api) "cloud-arrow-down" else "triangle-exclamation"),
      " Cota: ", info$fonte_niveis,
      " | Retirada: ", info$fonte_retiradas, ". ",
      info$mensagem,
      atualizado
    )
  })

  shiny::observeEvent(input$er_reservatorio_id, {
    resultado(NULL)
    erro_calculo(NULL)
    avisos_calculo(character())
    dados_api(NULL)
    info_api(NULL)

    base <- tryCatch(dados_reservatorio_base(), error = function(e) NULL)
    if (is.null(base)) return()
    carregado <- aplicar_cache_api_engenharia_reversa(base)
    carregado$dados$niveis <- interpolar_lacunas_cota_engenharia_reversa(
      carregado$dados$niveis
    )
    dados_api(list(
      id = as.integer(base$reservatorio$id),
      dados = carregado$dados
    ))
    info_api(carregado$info)
    dados <- carregado$dados
    intervalo <- .er_intervalo_disponivel(dados)
    if (any(is.na(intervalo))) return()

    shiny::updateDateRangeInput(
      session,
      "er_periodo",
      start = intervalo[[1L]],
      end = intervalo[[2L]],
      min = intervalo[[1L]],
      max = intervalo[[2L]]
    )
  }, ignoreInit = FALSE)

  shiny::observeEvent(input$er_atualizar_api, {
    shiny::req(input$er_reservatorio_id)
    periodo_selecionado <- shiny::isolate(input$er_periodo)
    resultado(NULL)
    erro_calculo(NULL)
    avisos_calculo(character())

    base <- dados_reservatorio_base()
    atualizado <- shiny::withProgress(
      message = "Consultando o Portal Hidrologico...",
      value = 0.2,
      {
        on.exit(shiny::setProgress(1), add = TRUE)
        atualizar_api_engenharia_reversa(base, data_fim = Sys.Date())
      }
    )
    atualizado$dados$niveis <- interpolar_lacunas_cota_engenharia_reversa(
      atualizado$dados$niveis
    )

    dados_api(list(
      id = as.integer(base$reservatorio$id),
      dados = atualizado$dados
    ))
    info_api(atualizado$info)

    intervalo <- .er_intervalo_disponivel(atualizado$dados)
    if (!any(is.na(intervalo))) {
      inicio_atual <- intervalo[[1L]]
      fim_atual <- intervalo[[2L]]

      if (!is.null(periodo_selecionado) &&
          length(periodo_selecionado) == 2L) {
        inicio_selecionado <- as.Date(periodo_selecionado[[1L]])
        fim_selecionado <- as.Date(periodo_selecionado[[2L]])
        if (!is.na(inicio_selecionado) && !is.na(fim_selecionado)) {
          inicio_atual <- max(inicio_selecionado, intervalo[[1L]])
          fim_atual <- min(fim_selecionado, intervalo[[2L]])
        }
      }

      if (inicio_atual >= fim_atual) {
        inicio_atual <- intervalo[[1L]]
        fim_atual <- intervalo[[2L]]
      }

      shiny::updateDateRangeInput(
        session,
        "er_periodo",
        start = inicio_atual,
        end = fim_atual,
        min = intervalo[[1L]],
        max = intervalo[[2L]]
      )
    }

    if (identical(atualizado$info$fonte_niveis, "API") &&
        identical(atualizado$info$fonte_retiradas, "API")) {
      shiny::showNotification(
        "Cota e retirada atualizadas pelo Portal Hidrologico.",
        type = "message",
        duration = 5
      )
    } else {
      shiny::showNotification(
        atualizado$info$mensagem,
        type = "warning",
        duration = 10
      )
    }
  }, ignoreInit = TRUE)

  output$er_metadados <- shiny::renderUI({
    if (!is.null(erro_catalogo) || nrow(catalogo) == 0L) {
      return(
        shiny::div(
          class = "er-empty",
          "A base de reservatorios nao esta disponivel."
        )
      )
    }

    dados <- dados_reservatorio()
    reservatorio <- dados$reservatorio
    intervalo <- .er_intervalo_disponivel(dados)
    itens <- list(
      c("Nome", .er_valor(reservatorio, "nome", "--")),
      c("ID", as.character(.er_valor(reservatorio, "id", "--"))),
      c(
        "Periodo disponivel",
        paste(.er_formatar_data(intervalo[[1L]]), "a", .er_formatar_data(intervalo[[2L]]))
      ),
      c(
        "Registros de nivel",
        .er_formatar_numero(nrow(dados$niveis), digitos = 0L)
      ),
      c(
        "Cota do vertedor",
        .er_formatar_numero(
          .er_valor(reservatorio, "cota_vertedor_m"),
          2,
          " m"
        )
      ),
      c(
        "Largura do vertedor",
        .er_formatar_numero(
          .er_valor(reservatorio, "largura_vertedor_m"),
          2,
          " m"
        )
      )
    )

    shiny::div(
      class = "er-meta-grid",
      lapply(itens, function(item) {
        shiny::div(
          class = "er-meta-item",
          shiny::div(class = "er-meta-label", item[[1L]]),
          shiny::div(class = "er-meta-value", item[[2L]])
        )
      })
    )
  })

  output$er_diagnostico <- shiny::renderUI({
    if (!is.null(erro_catalogo) || nrow(catalogo) == 0L) return(NULL)
    dados <- dados_reservatorio()
    diagnostico <- .er_resumir_diagnostico(dados)
    duplicadas <- diagnostico$datas_duplicadas
    transferencia <- diagnostico$registros_transferencia
    cotas_interpoladas <- if ("Cota_interpolada" %in% names(dados$niveis)) {
      sum(dados$niveis$Cota_interpolada, na.rm = TRUE)
    } else {
      0L
    }

    shiny::div(
      class = if (duplicadas > 0L) {
        "er-status er-status-warn"
      } else {
        "er-status er-status-ok"
      },
      if (duplicadas > 0L) {
        paste0(
          duplicadas,
          " registro(s) com data duplicada; o primeiro valor sera usado. "
        )
      } else {
        "Series sem datas duplicadas detectadas. "
      },
      if (transferencia > 0L) {
        paste0(
          transferencia,
          " registro(s) de transferencia preservado(s), ainda fora do calculo."
        )
      },
      if (cotas_interpoladas > 0L) {
        paste0(
          " ", cotas_interpoladas,
          " dia(s) de cota preenchido(s) por interpolacao linear."
        )
      }
    )
  })

  shiny::observeEvent(input$er_calcular, {
    resultado(NULL)
    erro_calculo(NULL)
    avisos_calculo(character())

    calculado <- tryCatch({
      linha <- linha_catalogo()
      if (!identical(linha$Status[[1L]], "pronto")) {
        stop(
          "Este reservatorio possui dados incompletos e ainda nao pode ser calculado.",
          call. = FALSE
        )
      }
      shiny::req(input$er_periodo)
      inicio <- as.Date(input$er_periodo[[1L]])
      fim <- as.Date(input$er_periodo[[2L]])
      if (is.na(inicio) || is.na(fim) || inicio >= fim) {
        stop("Selecione um periodo com pelo menos dois dias.", call. = FALSE)
      }

      dados <- dados_reservatorio()
      entradas <- preparar_entradas_calculo_engenharia_reversa(dados)
      entradas$niveis <- entradas$niveis[
        entradas$niveis$Data >= inicio & entradas$niveis$Data <= fim,
        ,
        drop = FALSE
      ]
      entradas$precipitacao <- data.frame(
        Data = seq(inicio, fim, by = "day"),
        Precipitacao_mm = 0
      )
      entradas$retiradas <- .er_retiradas_periodo(
        entradas$retiradas,
        inicio,
        fim
      )
      entradas$banda_incerteza <- input$er_banda
      entradas$tratar_negativos <- input$er_negativos
      entradas$precipitacao_ausente <- "zero"
      entradas$datas_duplicadas <- "primeiro"

      avisos <- character()
      valor <- withCallingHandlers(
        do.call(calcular_afluencia_total, entradas),
        warning = function(w) {
          avisos <<- c(avisos, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      avisos_calculo(unique(avisos))
      valor
    }, error = function(e) {
      erro_calculo(conditionMessage(e))
      NULL
    })

    resultado(calculado)
    if (is.null(calculado)) {
      shiny::showNotification(
        paste("Nao foi possivel calcular:", erro_calculo()),
        type = "error",
        duration = 8
      )
    } else {
      shiny::showNotification(
        "Afluencia calculada com sucesso.",
        type = "message",
        duration = 4
      )
    }
  }, ignoreInit = TRUE)

  output$er_resultado_ui <- shiny::renderUI({
    erro <- erro_calculo()
    if (!is.null(erro)) {
      return(
        shiny::div(
          class = "er-status er-status-error",
          shiny::icon("circle-exclamation"),
          " ",
          erro
        )
      )
    }
    if (is.null(resultado())) {
      return(
        shiny::div(
          class = "er-empty",
          shiny::icon("water", class = "fa-2x"),
          shiny::tags$h4("Pronto para calcular"),
          shiny::tags$p(
            "Selecione o reservatorio e o periodo, revise as opcoes e clique em Calcular afluencia."
          )
        )
      )
    }

    shiny::tagList(
      shiny::uiOutput("er_resumo"),
      shiny::div(
        class = "er-panel",
        shiny::div(
          class = "er-result-head",
          shiny::tags$h4("Resultado diario"),
          shiny::div(
            class = "er-downloads",
            shiny::downloadButton(
              "er_download_csv",
              "Baixar CSV",
              icon = shiny::icon("file-csv"),
              class = "btn-default btn-sm"
            ),
            shiny::downloadButton(
              "er_download_rds",
              "Baixar RDS",
              icon = shiny::icon("download"),
              class = "btn-default btn-sm"
            )
          )
        ),
        shiny::div(
          class = "er-tabs",
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Afluencia",
              plotly::plotlyOutput("er_grafico_afluencia", height = "390px")
            ),
            shiny::tabPanel(
              "Componentes",
              plotly::plotlyOutput("er_grafico_componentes", height = "420px")
            ),
            shiny::tabPanel(
              "Tabela",
              DT::DTOutput("er_tabela")
            )
          )
        )
      )
    )
  })

  output$er_cequal_export_ui <- shiny::renderUI({
    if (is.null(resultado())) return(NULL)

    shiny::div(
      class = "er-panel",
      shiny::div(
        class = "er-panel-title",
        shiny::span(class = "er-step", "3"),
        "Exportacao para o CE-QUAL-W2"
      ),
      shiny::tags$p(
        paste(
          "Distribua a afluencia total entre os bracos pelas areas de",
          "contribuicao. A retirada corrigida sera gravada no braco 1;",
          "nos demais bracos, Qout sera zero."
        )
      ),
      shiny::numericInput(
        "er_num_bracos",
        "Quantidade de afluentes/bracos",
        value = 2,
        min = 1,
        max = 5,
        step = 1,
        width = "240px"
      ),
      shiny::uiOutput("er_areas_bracos"),
      shiny::uiOutput("er_cequal_resumo"),
      shiny::div(
        class = "er-actions",
        shiny::downloadButton(
          "er_download_cequal",
          "Baixar arquivos Qin e Qout (.zip)",
          icon = shiny::icon("file-zipper"),
          class = "er-calculate"
        )
      )
    )
  })

  numero_bracos_cequal <- shiny::reactive({
    n <- suppressWarnings(as.integer(round(input$er_num_bracos)))
    if (length(n) == 0L || is.na(n)) n <- 2L
    max(1L, min(5L, n))
  })

  output$er_areas_bracos <- shiny::renderUI({
    shiny::req(resultado())
    n <- numero_bracos_cequal()
    shiny::tagList(
      shiny::tags$p(
        class = "er-advanced-help",
        paste(
          "Informe as areas na mesma unidade (recomenda-se km2).",
          "O aplicativo calculara automaticamente a proporcao de cada braco."
        )
      ),
      shiny::div(
        class = "er-arm-grid",
        lapply(seq_len(n), function(braco) {
          shiny::numericInput(
            paste0("er_area_braco_", braco),
            paste0("Area do braco ", braco, " (km2)"),
            value = 1,
            min = 0.000001,
            step = 0.1,
            width = "100%"
          )
        })
      )
    )
  })

  areas_bracos_cequal <- shiny::reactive({
    n <- numero_bracos_cequal()
    vapply(seq_len(n), function(braco) {
      valor <- input[[paste0("er_area_braco_", braco)]]
      if (is.null(valor) || length(valor) == 0L) NA_real_ else as.numeric(valor)
    }, numeric(1))
  })

  output$er_cequal_resumo <- shiny::renderUI({
    shiny::req(resultado())
    areas <- areas_bracos_cequal()
    if (any(!is.finite(areas)) || any(areas <= 0)) {
      return(
        shiny::div(
          class = "er-status er-status-error",
          "Informe uma area maior que zero para cada braco."
        )
      )
    }

    serie <- tryCatch(
      preparar_series_cequal_engenharia_reversa(resultado(), areas),
      error = function(e) e
    )
    if (inherits(serie, "error")) {
      return(
        shiny::div(
          class = "er-status er-status-error",
          conditionMessage(serie)
        )
      )
    }

    proporcoes <- paste0(
      "Braco ", seq_along(serie$proporcoes), ": ",
      formatC(100 * serie$proporcoes, format = "f", digits = 2, decimal.mark = ","),
      "%"
    )
    shiny::div(
      class = if (serie$dias_com_na > 0L) {
        "er-status er-status-warn"
      } else {
        "er-status er-status-ok"
      },
      shiny::tags$b("Distribuicao: "),
      paste(proporcoes, collapse = " | "),
      shiny::tags$br(),
      serie$dias_corrigidos,
      " dia(s) com Qin negativo serao zerados e transferidos para Qout do braco 1.",
      if (serie$dias_com_na > 0L) {
        shiny::tagList(
          shiny::tags$br(),
          shiny::tags$b("Atencao: "),
          serie$dias_com_na,
          paste(
            " dia(s) serao exportados como NA. Corrija esses valores",
            "manualmente antes de executar o CE-QUAL-W2."
          )
        )
      }
    )
  })

  output$er_download_cequal <- shiny::downloadHandler(
    filename = function() {
      dados <- resultado()
      shiny::req(dados)
      linha <- linha_catalogo()
      anos <- format(range(as.Date(dados$Data)), "%Y")
      paste0(
        "CEQUAL_",
        .er_cequal_slug(linha$Reservatorio[[1L]]),
        "_",
        anos[[1L]],
        "_",
        anos[[2L]],
        ".zip"
      )
    },
    content = function(file) {
      dados <- resultado()
      shiny::req(dados)
      areas <- areas_bracos_cequal()
      serie <- preparar_series_cequal_engenharia_reversa(dados, areas)
      linha <- linha_catalogo()
      pasta <- tempfile("cequal_exportacao_")
      if (!dir.create(pasta, recursive = TRUE, showWarnings = FALSE)) {
        stop("Nao foi possivel preparar a pasta temporaria de exportacao.")
      }
      on.exit(unlink(pasta, recursive = TRUE, force = TRUE), add = TRUE)

      arquivos <- escrever_arquivos_cequal_engenharia_reversa(
        serie,
        linha$Reservatorio[[1L]],
        pasta
      )
      zip::zipr(
        zipfile = file,
        files = basename(arquivos),
        root = pasta,
        include_directories = FALSE
      )
    }
  )

  output$er_resumo <- shiny::renderUI({
    shiny::req(resultado())
    resumo <- resumir_afluencia_total(resultado())
    percentual_valido <- 100 * resumo$Dias_validos / resumo$Numero_dias

    conteudo <- list(
      .er_ui_card(
        "Afluencia media",
        .er_formatar_numero(resumo$Afluencia_media_m3s, 2, " m3/s"),
        paste(resumo$Dias_validos, "dias validos"),
        "er-accent"
      ),
      .er_ui_card(
        "Afluencia maxima",
        .er_formatar_numero(resumo$Afluencia_maxima_m3s, 2, " m3/s"),
        "maior valor diario"
      ),
      .er_ui_card(
        "Periodo calculado",
        paste(resumo$Numero_dias, "dias"),
        paste(
          .er_formatar_data(resumo$Data_inicial),
          "a",
          .er_formatar_data(resumo$Data_final)
        )
      ),
      .er_ui_card(
        "Cobertura valida",
        .er_formatar_numero(percentual_valido, 1, "%"),
        paste(
          resumo$Dias_sem_cota,
          "sem cota |",
          resumo$Dias_com_afluencia_negativa,
          "negativos"
        )
      )
    )

    avisos <- avisos_calculo()
    shiny::tagList(
      shiny::div(class = "er-summary-grid", conteudo),
      if (length(avisos) > 0L) {
        shiny::div(
          class = "er-status er-status-warn",
          shiny::tags$b("Avisos do calculo: "),
          paste(avisos, collapse = " ")
        )
      }
    )
  })

  output$er_grafico_afluencia <- plotly::renderPlotly({
    dados <- resultado()
    shiny::req(dados)

    plotly::plot_ly(
      data = dados,
      x = ~Data,
      y = ~Afluencia_total_m3s,
      type = "scatter",
      mode = "lines",
      line = list(color = "#166b8f", width = 1.6),
      hovertemplate = paste(
        "<b>%{x|%d/%m/%Y}</b>",
        "<br>Afluencia: %{y:.3f} m3/s",
        "<extra></extra>"
      )
    ) |>
      plotly::layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Afluencia total (m3/s)", zeroline = TRUE),
        margin = list(l = 70, r = 25, b = 55, t = 25),
        hovermode = "x unified"
      ) |>
      plotly::config(
        displaylogo = FALSE,
        locale = "pt-BR",
        modeBarButtonsToRemove = c("lasso2d", "select2d")
      )
  })

  output$er_grafico_componentes <- plotly::renderPlotly({
    dados <- resultado()
    shiny::req(dados)
    componentes <- c(
      "Delta_volume_m3s",
      "Evaporacao_m3s",
      "Retirada_m3s",
      "Vertimento_m3s",
      "Precipitacao_m3s"
    )
    rotulos <- c(
      "Variacao do armazenamento",
      "Evaporacao",
      "Retirada",
      "Vertimento",
      "Precipitacao"
    )
    cores <- c("#123149", "#db8c3c", "#8a5ca8", "#2aa6a1", "#4c83c3")

    grafico <- plotly::plot_ly()
    for (i in seq_along(componentes)) {
      grafico <- plotly::add_lines(
        grafico,
        data = dados,
        x = ~Data,
        y = dados[[componentes[[i]]]],
        name = rotulos[[i]],
        line = list(color = cores[[i]], width = 1.2),
        hovertemplate = paste0(
          "<b>%{x|%d/%m/%Y}</b><br>",
          rotulos[[i]],
          ": %{y:.3f} m3/s<extra></extra>"
        )
      )
    }
    grafico |>
      plotly::layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Componente (m3/s)", zeroline = TRUE),
        legend = list(orientation = "h", y = -0.2),
        margin = list(l = 70, r = 25, b = 90, t = 25),
        hovermode = "x unified"
      ) |>
      plotly::config(displaylogo = FALSE, locale = "pt-BR")
  })

  output$er_tabela <- DT::renderDT({
    dados <- resultado()
    shiny::req(dados)
    tabela <- data.frame(
      Data = format(dados$Data, "%d/%m/%Y"),
      Cota_inicial_m = dados$Cota_inicial_m,
      Cota_final_m = dados$Cota_final_m,
      Delta_volume_m3s = dados$Delta_volume_m3s,
      Evaporacao_m3s = dados$Evaporacao_m3s,
      Retirada_m3s = dados$Retirada_m3s,
      Vertimento_m3s = dados$Vertimento_m3s,
      Precipitacao_m3s = dados$Precipitacao_m3s,
      Afluencia_calculada_m3s = dados$Afluencia_calculada_m3s,
      Afluencia_total_m3s = dados$Afluencia_total_m3s,
      Flag = dados$Flag,
      check.names = FALSE
    )
    nomes <- c(
      "Data", "Cota inicial (m)", "Cota final (m)",
      "Variacao volume (m3/s)", "Evaporacao (m3/s)",
      "Retirada (m3/s)", "Vertimento (m3/s)",
      "Precipitacao (m3/s)", "Afluencia calculada (m3/s)",
      "Afluencia total (m3/s)", "Diagnostico"
    )
    names(tabela) <- nomes

    DT::datatable(
      tabela,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(url = "")
      )
    ) |>
      DT::formatRound(columns = nomes[2:10], digits = 4, dec.mark = ",")
  })

  nome_arquivo <- shiny::reactive({
    linha <- linha_catalogo()
    paste0(
      "engenharia_reversa_",
      .er_slug(linha$Reservatorio[[1L]]),
      "_",
      format(Sys.Date(), "%Y%m%d")
    )
  })

  output$er_download_csv <- shiny::downloadHandler(
    filename = function() paste0(nome_arquivo(), ".csv"),
    content = function(file) {
      dados <- resultado()
      shiny::req(dados)
      readr::write_excel_csv(
        as.data.frame(dados),
        file,
        na = "",
        delim = ";"
      )
    }
  )

  output$er_download_rds <- shiny::downloadHandler(
    filename = function() paste0(nome_arquivo(), ".rds"),
    content = function(file) {
      dados <- resultado()
      shiny::req(dados)
      saveRDS(dados, file)
    }
  )

  invisible(
    list(
      catalogo = catalogo,
      resultado = resultado,
      erro = erro_calculo
    )
  )
}


# COPIA LEGADA: versao de notebook preservada; nao e usada na execucao.
# Carregamento de bibliotecas e scripts auxiliares
cat("\014")

# ============================================================
# Dependências e scripts auxiliares
# ============================================================
app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(app_dir, "loadPackages.R"), local = TRUE)
source(file.path(app_dir, "R", "ReadFile_Rch_SWAT.R"), local = TRUE)
source(file.path(app_dir, "R", "ReadFile_cio_SWAT.R"), local = TRUE)
source(file.path(app_dir, "R", "io_seguro.R"), local = TRUE)
source(file.path(app_dir, "R", "contexto_simulacao.R"), local = TRUE)
source(
  file.path(
    app_dir,
    "R",
    "FUNC_simalation_calibration_otimizacao_CEQUALW2.R"
  ),
  local = TRUE
)





botao_ajuda <- function(titulo, conteudo, largura = "420px") {
  div(
    class = "help-wrapper-inline",
    
    div(
      class = "help-button-inline",
      "?"
    ),
    
    div(
      class = "help-content-inline",
      style = paste0("width:", largura, ";"),
      
      tags$div(
        class = "help-title",
        titulo
      ),
      
      conteudo
    )
  )
}

ajuda_tipos_simulacao <- function() {
  botao_ajuda(
    titulo = "Tipos de Simulação e Otimização",
    conteudo = tagList(
      tags$p(HTML(
        "<b>Tipo 1 – Hidrodinâmica:</b><br>
        Ajuste de parâmetros hidrodinâmicos básicos responsáveis pela circulação e estrutura térmica do reservatório.
        <br><span>Parâmetros: AFW, BFW, CFW, WSC, SHADE.</span>"
      )),
      
      tags$p(HTML(
        "<b>Tipo 2 – Hidrodinâmica Avançada:</b><br>
        Inclui parâmetros adicionais associados à dispersão, troca vertical e processos hidrodinâmicos avançados.
        <br><span>Parâmetros: AFW, BFW, CFW, WSC, SHADE, AX, DX, CBHE, EXH2O, BETA.</span>"
      )),
      
      tags$p(HTML(
        "<b>Tipo 3 – Fósforo:</b><br>
        Calibração dos coeficientes relacionados à dinâmica do fósforo e processos biogeoquímicos.
        <br><span>Parâmetros: AG, AR, AE, AM, AS, AHSP, ASAT, SOD.</span>"
      ))
    )
  )
}


ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("

    body {
      background-color: #f5f8fb;
      font-family: 'Segoe UI', Arial, sans-serif;
      color: #1f2d3d;
    }

    /* Painel lateral */
    .sidebar-flowqual {
      background: #f4f8fb !important;
      border: none !important;
      padding: 22px 16px !important;
      min-height: 100vh;
    }

    .sidebar-card {
      background: #ffffff;
      border-radius: 22px;
      padding: 22px;
      box-shadow: 0 10px 28px rgba(16, 47, 78, 0.08);
      border: 1px solid #e2edf3;
      min-height: calc(100vh - 45px);
    }

    .logo-area {
    display: flex;
    justify-content: center;
    align-items: center;
    margin-bottom: 14px;
      width: 100%;
    }

    .flowqual-logo {
    width: 85%;
    max-width: 390px;
     min-width: 180px;
     height: auto;
     display: block;
    }

    .app-title {
      font-size: 30px;
      font-weight: 600;
      color: #102f4e;
      margin-top: 8px;
      margin-bottom: 2px;
    }

    .app-subtitle {
      color: #52616b;
      font-size: 13px;
      margin-bottom: 18px;
    }

    .info-box {
      background: #f8fbfd;
      border: 1px solid #dfeaf1;
      border-radius: 16px;
      padding: 16px;
      line-height: 1.55;
    }

    .info-title {
      font-weight: 700;
      color: #102f4e;
      margin-bottom: 12px;
      font-size: 15px;
    }

    .info-box p {
      font-size: 13px;
      color: #2f3f4f;
      margin-bottom: 14px;
    }

    .footer-sidebar {
      color: #6b7c86;
      font-size: 12px;
      line-height: 1.5;
      margin-top: 18px;
    }

    /* SelectInput */
    label {
      color: #1f2d3d;
      font-weight: 600;
      margin-bottom: 6px;
    }

    .selectize-input {
      border-radius: 10px !important;
      border: 1px solid #d2e0e8 !important;
      padding: 9px 12px !important;
      box-shadow: none !important;
      background: #ffffff !important;
    }

    .selectize-input.focus {
      border-color: #1f6f8b !important;
      box-shadow: 0 0 0 3px rgba(31, 111, 139, 0.12) !important;
    }

    hr {
      border-top: 1px solid #e7eef3;
      margin-top: 20px;
      margin-bottom: 20px;
    }

    /* Área principal */
    .col-sm-8 {
      padding-top: 0;
    }

    /* Abas superiores */
.nav-tabs {
  border-bottom: 2px solid #0F4F9A !important;
  background: #ffffff;
  padding-left: 0;
}

/* Abas normais */
.nav-tabs > li > a {
  color: #0F4F9A !important;
  font-weight: 600;
  border: none !important;
  border-radius: 10px 10px 0 0 !important;
  padding: 12px 24px !important;
  background: #ffffff !important;
}

/* Ao passar o mouse */
.nav-tabs > li > a:hover {
  background: #EAF3FF !important;
  color: #0F4F9A !important;
  border: none !important;
}

/* Aba ativa */
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:focus,
.nav-tabs > li.active > a:hover {
  background: #EAF3FF !important;
  color: #0F4F9A !important;
  border: none !important;
  border-top: 4px solid #0F4F9A !important;
  border-radius: 10px 10px 0 0 !important;
  font-weight: 700;
}
    .aba_inativa {
      opacity: 0.35 !important;
      pointer-events: none !important;
      cursor: not-allowed !important;
    }

    /* Tela inicial */
    .welcome-container {
      min-height: 68vh;
      display: flex;
      align-items: center;
      justify-content: center;
      padding: 34px;
      background: #eef6fa;
      border-radius: 22px;
      margin: 20px;
    }
    
    .help-wrapper {
  position: relative;
  display: inline-block;
  margin-top: 8px;
  margin-bottom: 12px;
}

.help-button {
  width: 34px;
  height: 34px;
  border-radius: 50%;
  background: #EAF7FB;
  color: #0F6787;
  border: 1px solid #B9E3EF;
  display: flex;
  align-items: center;
  justify-content: center;
  font-weight: 700;
  cursor: help;
  font-size: 18px;
}

.help-button:hover {
  background: #DDF2F8;
  color: #0A4F6A;
}

.help-content {
  display: none;
  position: absolute;
  left: 0;
  top: 42px;
  width: 390px;
  max-width: 80vw;
  background: #ffffff;
  border: 1px solid #d7e7ef;
  border-radius: 16px;
  padding: 18px;
  box-shadow: 0 12px 30px rgba(16, 47, 78, 0.14);
  z-index: 9999;
  line-height: 1.5;
}

.help-wrapper:hover .help-content {
  display: block;
}

.help-title {
  font-weight: 700;
  color: #102F4E;
  font-size: 15px;
  margin-bottom: 12px;
}

.help-content p {
  font-size: 13px;
  color: #2F3F4F;
  margin-bottom: 14px;
}

.help-content span {
  color: #52616B;
  font-size: 12px;
}

.label-help-row {
  display: flex;
  align-items: center;
  gap: 8px;
  margin-bottom: 6px;
}

.label-help-text {
  margin-bottom: 0 !important;
  font-weight: 600;
  color: #1f2d3d;
}

.help-wrapper-inline {
  position: relative;
  display: inline-block;
}

.help-button-inline {
  width: 22px;
  height: 22px;
  min-width: 22px;
  min-height: 22px;
  border-radius: 50%;
  background: #EAF7FB;
  color: #0F6787;
  border: 1px solid #B9E3EF;
  display: flex;
  align-items: center;
  justify-content: center;
  font-weight: 700;
  cursor: help;
  font-size: 13px;
  line-height: 1;
}

.help-button-inline:hover {
  background: #DDF2F8;
  color: #0A4F6A;
}

.help-content-inline {
  visibility: hidden;
  opacity: 0;
  pointer-events: none;

  position: absolute;
  left: 40px;
  top: 50%;
  transform: translateY(-50%);

  width: 420px;
  max-width: 70vw;

  background: #ffffff;
  border: 1px solid #d7e7ef;
  border-radius: 16px;
  padding: 18px;
  box-shadow: 0 12px 30px rgba(16, 47, 78, 0.14);
  z-index: 9999;
  line-height: 1.5;

  transition: opacity 0.15s ease-in-out;
}

.help-wrapper-inline:hover .help-content-inline {
  visibility: visible;
  opacity: 1;
}

.help-title {
  font-weight: 700;
  color: #102F4E;
  font-size: 15px;
  margin-bottom: 12px;
}

.help-content-inline p {
  font-size: 13px;
  color: #2F3F4F;
  margin-bottom: 14px;
}

.help-content-inline span {
  color: #52616B;
  font-size: 12px;
}

    .welcome-card {
      max-width: 950px;
      width: 100%;
      background: #ffffff;
      border-radius: 24px;
      padding: 42px;
      box-shadow: 0 14px 34px rgba(16, 47, 78, 0.08);
      border: 1px solid #dfeaf1;
    }

    .welcome-badge {
      display: inline-block;
      background: #e3f2f7;
      color: #0f6787;
      font-weight: 700;
      font-size: 13px;
      letter-spacing: 0.5px;
      padding: 8px 14px;
      border-radius: 999px;
      margin-bottom: 18px;
    }

    .welcome-card h2 {
      color: #102f4e;
      font-weight: 700;
      margin-top: 0;
      margin-bottom: 16px;
      line-height: 1.25;
    }

    .welcome-text {
      color: #475866;
      font-size: 17px;
      line-height: 1.6;
      max-width: 830px;
      margin-bottom: 24px;
    }

    .welcome-alert {
      background: #edf7fb;
      border-left: 5px solid #1f6f8b;
      color: #24495c;
      padding: 14px 18px;
      border-radius: 12px;
      font-size: 15px;
      margin-bottom: 28px;
    }

    .welcome-modules {
      display: flex;
      justify-content: flex-start;
      gap: 18px;
      flex-wrap: wrap;
    }

    .module-card {
      width: 275px;
      min-height: 135px;
      background: #f8fbfd;
      border: 1px solid #dceaf1;
      border-radius: 18px;
      padding: 20px;
      transition: all 0.2s ease-in-out;
    }

    .module-card h4 {
      color: #0f6787;
      font-weight: 700;
      margin-top: 0;
      margin-bottom: 10px;
    }

    .module-card p {
      color: #4f5f6b;
      font-size: 14px;
      line-height: 1.5;
      margin-bottom: 0;
    }

    .module-card-click {
      cursor: pointer;
    }

    .module-card-click:hover {
      transform: translateY(-4px);
      box-shadow: 0 10px 24px rgba(16, 47, 78, 0.10);
      background: #ffffff;
    }

    a:has(.module-card) {
      text-decoration: none !important;
      color: inherit !important;
    }

    .btn {
      border-radius: 10px;
      font-weight: 600;
      border: none;
    }

.btn-default {
  background-color: #0F4F9A;
  color: white;
  border-color: #0F4F9A;
}

.btn-default:hover {
  background-color: #0B3F7C;
  color: white;
  border-color: #0B3F7C;
}

    pre {
      background-color: #f7fafc;
      border: 1px solid #dfeaf1;
      border-radius: 12px;
      color: #2f3f4f;
    }

  "))
  ),
  
 
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-flowqual",
      
      div(
        class = "sidebar-card",
        
        div(
          class = "logo-area",
          tags$img(
            src = "flowqual-logo.png",
            class = "flowqual-logo"
          )
        ),
        
        tags$hr(),
        
        selectInput(
          "reserv",
          "Selecione o Reservatório", 
          choices = list(
            " " = " ",
            "Acarape do Meio" = "Acarape do Meio",
            "Araras" = "Araras",
            "Banabuiú" = "Banabuiú",
            "Castanhão" = "Castanhão",
            "Curral Velho" = "Curral Velho",
            "Edson Queiroz" = "Edson Queiroz",
            "Jaburú 1" = "Jaburú 1",
            "Olho Dágua" = "Olho Dágua",
            "Orós" = "Orós",
            "Pacoti" = "Pacoti",
            "Pedras Brancas" = "Pedras Brancas",
            "Pentecostes" = "Pentecostes",
            "Rósario" = "Rósario"
          ), 
          selected = " "
        ),
        
        tags$hr(),
        

        tags$hr(),
        
        div(
          class = "footer-sidebar",
          "Desenvolvido pela Gerência de Estudos e Pesquisas em Recursos Hídricos (GEPEH) da FUNCEME"
        ),
        
        tags$hr(),
        
      
        
   
      )
    ),
  
    mainPanel(
      tabsetPanel(
        id = "abas_principais",
        selected = "inicio",
        
        tabPanel(
          "Início",
          value = "inicio",
          
          br(),
          
          div(
            class = "welcome-container",
            
            div(
              class = "welcome-card",
              
              div(
                class = "welcome-badge",
                "SWAT–CE-QUAL-W2"
              ),
              
              h2("Bem-vindo ao sistema de acoplamento de modelagem hidrológica e qualidade da água"),
              
              p(
                class = "welcome-text",
                "Esta plataforma integra ferramentas para preparação de dados, simulação, cenarização e otimização de modelos de qualidade da água em rio e reservatórios."
              ),
              
              div(
                class = "welcome-alert",
                "Selecione um reservatório no campo ao lado para habilitar os módulos do sistema."
              ),
              
              div(
                class = "welcome-modules",
                
                actionLink(
                  inputId = "card_swat",
                  label = div(
                    class = "module-card module-card-click",
                    h4("SWAT"),
                    p("Geração de arquivos de entrada a partir das saídas hidrológicas do modelo.")
                  )
                ),
                
                actionLink(
                  inputId = "card_cequal",
                  label = div(
                    class = "module-card module-card-click",
                    h4("CE-QUAL-W2"),
                    p("Configuração, simulação e análise hidrodinâmica e de qualidade da água.")
                  )
                ),
                
                
              )
            )
          )
        ),
        
         
        tabPanel("SWAT",
                 value = "swat",
                 br(),br(),
                 
              #   tags$hr(),
                 tags$p(HTML("<b>1: Selecione o diretório que contem os arquivos <i>output.rch e file.cio</i>, saídas do modelo Swat</b>")),
                 shinyDirButton("directory", "Selecione o diretório dos dados", "Selecione a pasta"),
                 verbatimTextOutput("directorypath"),
                 
                 #imageOutput('image2', width = "100%"),# width=200, height=200),
              uiOutput("espaco_imagem"),
              
                 tags$hr(),
         
                 selectInput("var", label = ("2: Selecione a variável que deseja gerar o arquivo de entrada"), 
                             choices = list(" " = " ","Vazão Afluente" = "FLOW_INcms", "Vazão Efluente" = "FLOW_OUTcms", "Temperatura" = "WTMPdegc", "PO4 , CHLA, OD - Entrada" = "PO4 , CHLA, OD - Entrada","PO4 , CHLA, OD - Saída" = "PO4 , CHLA, OD - Saída"), 
                             selected = 1),
                 #tags$p(HTML("3: Variável que deseja salvar: ")),
                 verbatimTextOutput("selectvar"),
                 tags$hr(),
                 numericInput("n_rch","3: Digite o número do braço ou sub-bacia que deseja",
                           min=1, max=1000, value=NA),
         
                 dateRangeInput('dateRange',
                                label = '4: Selecione as datas de inicio e fim do periodo a ser simulado no CEQUAL-W2',
                                start = "2016-01-01", end = "2024-01-01" , language = "pt", separator = "a",
                                #start = NULL, end = NULL , language = "pt", separator = "a",
                                format= "dd-mm-yyyy"),
                 #tags$p(HTML("4: Período selecionado: ")),
                 verbatimTextOutput("dateRangeText"),
                 tags$hr(),
                 
                 #tags$p(HTML("5: Número do braço/sub-bacia selecionado:")),
               #  verbatimTextOutput("n_rchselec"),
              textInput(
                inputId = "output_filename",
                label   = "5: Nome do arquivo de saída (OBS: formato padrão: VAR_brX_Sy)",
                value   = ""   # começa em branco
              ),
                 tags$hr(),
                 actionButton("update", "Gerar Arquivo"),
                 
              
               tableOutput("view"),
                 
        ),
        
        tabPanel("CEQUAL",
                 value = "cequal",
                 br(),
                 
                 br(),br(),
                 tags$p(HTML("<b>1: Selecione o arquivo de controle .npt do CE-QUAL-W2</b>")),
                 
                 
                 actionButton(
                   "directory_cequal",
                   "Selecione o arquivo .npt"
                 ),
                 verbatimTextOutput("directory_cequal_texto"),
                 
                 
                 splitLayout(
                   cellWidths = c("33%", "33%", "33%"),
                   numericInput("TMSTRT", "Dia inicial:", value = NA),
                   numericInput("TMEND",  "Dia final:",  value = NA),
                   numericInput("YEAR",   "Ano:",   value = NA),
                 ),
                 splitLayout(
                   cellWidths = c("33%", "33%", "33%"),
                   numericInput("ELWS", "Cota inicial(m):", value = NA),
                   #numericInput("CTMAX", "Cota máxima:", value = NA),
                   #numericInput("EBOT",  "Cota mínima:",  value = NA),
                   # numericInput("CTVER",  "Cota Vertedouro:",   value = NA),
                   
                   #     numericInput("IMX", "Numero de Segmentos:", value = NA),
                   numericInput("TEMP",  "Temperatura inicial (ºC):",  value = NA),
                   numericInput("ITSR",  "Segmento Observado:",  value = NA),
                   
                 ),
                 splitLayout(
                   cellWidths = c("33%", "33%", "33%"),
                   numericInput("PO4", "Fosfato (g/m³):", value = NA),
                   numericInput("DO",  "Oxigênio Dissolvido (g/m³)",  value = NA),
                   numericInput("ALG1",  "Algas (g/m³)",  value = NA),
                   
                 ),
                 
              #   h4("Escolha qual módulo utilizar"),
                 br(),
                 
              checkboxInput(
                "use_windows",
                "Habilitar execução por janelas (guiadas por monitoramento)",
                value = FALSE
              ),
              tags$small("As janelas são definidas entre datas consecutivas de campanhas. Em média, 90 dias. "),
              tags$hr(),
              
                 tabsetPanel(
                   
                     tabPanel("Simulação",
                              

                      tags$hr(),
                      
                
                #      h5("Métricas por janela (quando habilitado)"),
                      tableOutput("janela_metricas"),

                              tags$hr(),

                div(
                  class = "label-help-row",
                  
                  tags$label(
                    "2: Selecione o tipo de simulação",
                    class = "label-help-text"
                  ),
                  
                  ajuda_tipos_simulacao()
                ),
                
                selectInput(
                  "tipo_simulacao",
                  label = NULL,
                  choices = list(
                    "Tipo 1" = "1",
                    "Tipo 2" = "2",
                    "Tipo 3" = "3"
                  ),
                  selected = 1
                ),

                              tags$hr(),
                           

                             tags$p(HTML("<b>3: Selecione o arquivo com os parametros para simulação</b>")),
                      fileInput(
                        "param_file",
                        "Escolha o arquivo contendo os parâmetros",
                        accept = c(".prn", ".txt", ".csv"),
                        buttonLabel = "Selecionar arquivo",   # 👈 aqui você muda o texto do botão
                        placeholder = "Nenhum arquivo selecionado" # 👈 e aqui a mensagem ao lado
                      ),
                      
                             tableOutput("param_table"),

                             tags$hr(),

                             actionButton("run_exe", "Rodar EXE"),
                             verbatimTextOutput("exe_log")


                              
                              
                              
                 
                 ),
              



                tabPanel("Otimização",
                         
                         
                         fluidRow(
                           br(),
                           column(
                             width = 6,
                             div(
                               class = "label-help-row",
                               
                               tags$span(
                                 "1: Selecione o tipo de otimização",
                                 class = "label-help-text"
                               ),
                               
                               ajuda_tipos_simulacao()
                             ),
                             
                             selectInput(
                               "tipo",
                               label = NULL,
                               choices = list(
                                 "Tipo 1" = "1",
                                 "Tipo 2" = "2",
                                 "Tipo 3" = "3"
                               ),
                               selected = "1",
                               width = "100%"
                             )
                             ),
                           
                           column(
                             width = 6,
                             selectizeInput(
                               "obj_fun",
                               "2: Função matemática utilizada",
                               choices = c(
                                 "Minimizar MAE"  = "mae",
                                 "Minimizar RMSE" = "rmse",
                                 "Maximizar NSE"  = "nse",
                                 "Maximizar KGE"  = "kge"
                               ),
                               selected = "mae",
                               options = list(maxOptions = 10),
                               width = "100%"
                             )
                           )
                         ),
                         
                         
                         
                         tags$hr(),
                         
                         fluidRow(
                           column(
                             width = 6,
                             uiOutput("vars_obj_ui"),
                             br(),
                             h5(strong("4: Parâmetros da Otmização")),
                             div(
                               style = "display: flex; align-items: center; gap: 10px;",
                               tags$label("Tamanho da População:",style = "margin-bottom: 0;font-weight: normal;"),
                               numericInput("popSize", NULL, value = 30, min = 0, step = 1,width = "80px")
                             ),
                             div(
                               style = "display: flex; align-items: center; gap: 16px;",
                               tags$label("Máximo de Interações  :", style = "margin-bottom: 0;font-weight: normal;"),
                               div(
                                 style = "margin-bottom: 0;",
                                 numericInput("maxiter", NULL, value = 20, min = 0, step = 1,width = "80px")
                               )
                             ),
                             
                             div(
                               style = "display: flex; align-items: center; gap: 105px;",
                               tags$label("Mutação:", style = "margin-bottom: 0;font-weight: normal;"),
                               div(
                                 style = "margin-bottom: 0;",
                                 numericInput("pmutation", NULL, value = 0.7, min = 0, max = 1, step = 0.1,width = "80px")
                               )
                             )
                             # 
                             # popSize   <- 20
                             # maxiter   <- 20
                             # pmutation <- 0.7
                           ),
                           
                           column(
                             width = 6,
                             div(
                         
                               h5(strong("5: Limites de busca dos parâmetros")),
                               style = "width:300px;",
                               DT::DTOutput("tabela_limites")
                             )
                           
                         ),
                           
                         ),
                        
                         
                         
                         tags$hr(),
      
                         
                        
                         
                         tags$hr(),
                         
                         actionButton("run_opt", "Executar Otimização"),
                         verbatimTextOutput("opt_log"),
                         #plotlyOutput("pareto_plot", height = "700px")
                         uiOutput("pareto_plot_ui")
                         
           ),
           
           tabPanel("Cenarização",
                    br(),
                    tags$p(HTML("<b>1: Selecione a pasta que contém os arquivos do cenário de afluência</b>")),
                    shinyDirButton("directory_cenario_cequal", "Selecionar pasta do cenário", "Selecione a pasta"),
                    verbatimTextOutput("directory_cenario_cequal_text"),
                    
                    tags$hr(),
                    tags$p(HTML("<b>2: Informe o identificador do cenário</b>")),
                    textInput("cenario_id", "Identificador do cenário", value = "S1"),
                    
                    tags$hr(),
                    tags$p(HTML("<b>3: Pré-visualização dos arquivos do cenário de afluência</b>")),
                    tableOutput("cenario_preview"),
                    
                    tags$hr(),
                    fluidRow(
                      column(6, actionButton("prepare_cenario_afluencia", "Preparar cenário de afluência")),
                      column(6, actionButton("run_cenario_afluencia", "Rodar cenário"))
                    ),
                    verbatimTextOutput("cenario_log")
           ),
           
          ),
        ),
      )
    )
  )
)

server <- function(input, output, session) {

    
  diretorio_cequal_sel <- reactiveVal(NULL)
  arquivo_controle_sel <- reactiveVal(NULL)
  
  later::later(function() {
    removeModal(session = session)
  }, delay = 2)
    

  
  
  # ---- estado global de validação do arquivo ----
  param_ok <- reactiveVal(FALSE)
  projeto_ok<- reactiveVal(FALSE)
  

  observe({
    
    reservatorio_ok <- !is.null(input$reserv) &&
      !is.na(input$reserv) &&
      input$reserv != "" &&
      input$reserv != " "
    
    if (reservatorio_ok) {
      
      shinyjs::removeClass(
        selector = "#abas_principais li a[data-value='swat']",
        class = "aba_inativa"
      )
      
      shinyjs::removeClass(
        selector = "#abas_principais li a[data-value='cequal']",
        class = "aba_inativa"
      )
      
    } else {
      
      shinyjs::addClass(
        selector = "#abas_principais li a[data-value='swat']",
        class = "aba_inativa"
      )
      
      shinyjs::addClass(
        selector = "#abas_principais li a[data-value='cequal']",
        class = "aba_inativa"
      )
      
      updateTabsetPanel(
        session,
        inputId = "abas_principais",
        selected = "inicio"
      )
    }
  })
  
  observeEvent(input$card_swat, {
    
    reservatorio_ok <- !is.null(input$reserv) &&
      !is.na(input$reserv) &&
      input$reserv != "" &&
      input$reserv != " "
    
    if (reservatorio_ok) {
      updateTabsetPanel(
        session,
        inputId = "abas_principais",
        selected = "swat"
      )
    } else {
      showModal(modalDialog(
        title = "Reservatório não selecionado",
        "Selecione um reservatório antes de acessar o módulo SWAT.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  observeEvent(input$card_cequal, {
    
    reservatorio_ok <- !is.null(input$reserv) &&
      !is.na(input$reserv) &&
      input$reserv != "" &&
      input$reserv != " "
    
    if (reservatorio_ok) {
      updateTabsetPanel(
        session,
        inputId = "abas_principais",
        selected = "cequal"
      )
    } else {
      showModal(modalDialog(
        title = "Reservatório não selecionado",
        "Selecione um reservatório antes de acessar o módulo CE-QUAL-W2.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })


  
  volumes <- c(
    "Pasta do app" = app_dir,
    Home = fs::path_home(),
    "R Installation" = R.home(),
    getVolumes()
  )
  shinyDirChoose(input, "directory", roots = volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)

  
  
  
  shinyDirChoose(input, "directory_cenario_cequal", roots = volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)

  observeEvent(input$directory_cequal, {
    showNotification(
      "Abrindo o seletor de arquivos do Windows. Verifique a barra de tarefas.",
      type = "message",
      duration = 5
    )

    pasta_inicial <- diretorio_cequal_sel()
    if (is.null(pasta_inicial) || !dir.exists(pasta_inicial)) {
      pasta_inicial <- app_dir
    }

    script_seletor <- file.path(
      app_dir,
      "scripts",
      "selecionar_arquivo_npt_windows.ps1"
    )
    if (!file.exists(script_seletor)) {
      showNotification(
        "O auxiliar do seletor de arquivo .npt não foi encontrado.",
        type = "error"
      )
      return(NULL)
    }

    arquivo_selecionado <- system2(
      "powershell.exe",
      args = c(
        "-NoProfile",
        "-STA",
        "-ExecutionPolicy", "Bypass",
        "-File", shQuote(script_seletor),
        "-PastaInicial", shQuote(normalizePath(
          pasta_inicial,
          winslash = "\\",
          mustWork = TRUE
        ))
      ),
      stdout = TRUE,
      stderr = FALSE
    )

    if (
      length(arquivo_selecionado) > 0L &&
      nzchar(arquivo_selecionado[[1L]]) &&
      file.exists(arquivo_selecionado[[1L]]) &&
      identical(tolower(tools::file_ext(arquivo_selecionado[[1L]])), "npt")
    ) {
      arquivo_normalizado <- normalizePath(
        arquivo_selecionado[[1L]],
        winslash = "/",
        mustWork = TRUE
      )
      arquivo_controle_sel(arquivo_normalizado)
      diretorio_cequal_sel(dirname(arquivo_normalizado))

      showNotification(
        paste("Arquivo selecionado:", basename(arquivo_controle_sel())),
        type = "message",
        duration = 4
      )
    }
  })
  
  
  
  
  


  output$RESERV <- renderPrint({
    if (input$reserv == " ") cat("Nenhum reservatório selecionado") else cat("Reservatório:", input$reserv)
  })
  
  output$directorypath <- renderPrint({
    if (is.integer(input$directory)) {
      cat("Nenhum diretório foi selecionado")
    } else {
      parseDirPath(volumes, input$directory)

    }
  })
  
  output$directory_cequal_texto <- renderPrint({
    
    if (is.null(arquivo_controle_sel())) {
      cat("Nenhum arquivo .npt foi selecionado")
    } else {
      cat(
        "Arquivo de controle:", arquivo_controle_sel(),
        "\nDiretório CE-QUAL-W2:", diretorio_cequal_sel()
      )
    }
  })
  

  output$directory_cenario_cequal_text <- renderPrint({
    if (is.null(input$directory_cenario_cequal) || is.integer(input$directory_cenario_cequal)) {
      cat("Nenhum diretório foi selecionado")
    } else {
      parseDirPath(volumes, input$directory_cenario_cequal)
    }
  })

  

  
  observeEvent(input$directory, {
    req(input$directory)
    
    diretorio <- parseDirPath(volumes, input$directory)
    
    # só continua se diretorio não for vazio
    if (length(diretorio) == 0 || diretorio == "") {
      return(NULL)
    }
    
    cio_file <- file.path(diretorio, "file.cio")
    rch_file <- file.path(diretorio, "output.rch")
    
    if (!(file.exists(cio_file) && file.exists(rch_file))) {
      missing <- c()
      if (!file.exists(cio_file)) missing <- c(missing, "file.cio")
      if (!file.exists(rch_file)) missing <- c(missing, "output.rch")
      
      showModal(modalDialog(
        title = "Arquivos obrigatórios não encontrados",
        HTML(paste0(
          "O(s) arquivo(s) <b>", paste(missing, collapse = ", "),
          "</b> não foi(foram) encontrado(s) em:<br><br><i>", diretorio,
          "</i><br><br>Por favor, selecione novamente o diretório correto."
        )),
        easyClose = TRUE,
        footer = modalButton("Fechar")
      ))
      return(NULL)
    }
  })
  
  
  
  ####
  
  
  observeEvent(arquivo_controle_sel(), {
  #  req(input$directory_cequal)
    
    d <- diretorio_cequal_sel()
    arquivo_controle <- arquivo_controle_sel()
    if (
      is.null(d) || length(d) == 0L || d == "" ||
      is.null(arquivo_controle) || !file.exists(arquivo_controle)
    ) {
      return(NULL)
    }
    
    
    # localiza os dados TMSTR, TEND, YEAR, ITSR no arquivo de controle
    
    projeto_ok(TRUE)
 
    linhas <- ler_linhas_seguro(
      arquivo_controle,
      warn = FALSE,
      descricao = "arquivo de controle"
    )
    TMSTRT <- localiza_valor(linhas, "TMSTRT")
    TMEND  <- localiza_valor(linhas, "TMEND")
    YEAR   <- localiza_valor(linhas, "YEAR")
    ITSR   <- localiza_valor(linhas, "ITSR")
    TEMP   <- localiza_valor(linhas, "T2I")

    
    ini <- grep("^CST ICON", linhas)
    fim <- grep("^CST PRIN", linhas)
    trecho <- linhas[(ini + 1):(fim - 1)]
    # Cria um data.frame com duas colunas: nome e valor
    dados <- do.call(rbind, strsplit(trimws(trecho), "\\s+"))
    dados <- as.data.frame(dados, stringsAsFactors = FALSE)
    
   
    PO4<- dados %>%
      filter(V1 == "PO4") %>%
      pull(V2)
    
    DO<- dados %>%
      filter(V1 == "DO") %>%
      pull(V2)
    
    ALG1 <- dados %>%
      filter(V1 == "ALG1") %>%
      pull(V2)
    
 
    bth <- localiza_arquivo(linhas, "BTH FILE")
    
    if (!is.na(bth)) {
      arquivo_bth <- file.path(d, bth)
      if (file.exists(arquivo_bth)) {
        # lê o arquivo de batimetria (ajuste o separador conforme necessário)

        linhas_bth <- ler_linhas_seguro(
          arquivo_bth,
          warn = FALSE,
          descricao = "arquivo de batimetria"
        )
     

          ELWS <- localiza_bloco(linhas_bth, "ELWS")[1]
        
         
      } else {
        showNotification(paste("Arquivo de batimetria não encontrado:", arquivo_bth),
                         type = "error")
      }
    }
    
    
    
    

    #Mostra os dados encontrados na tela 
    
    if (!is.na(TMSTRT)) updateNumericInput(session, "TMSTRT", value = TMSTRT)
    if (!is.na(TMEND))  updateNumericInput(session, "TMEND",  value = TMEND)
    if (!is.na(YEAR))   updateNumericInput(session, "YEAR",   value = YEAR)
    if (!is.na(ITSR))   updateNumericInput(session, "ITSR",   value = ITSR)
    if (!is.na(ELWS))   updateNumericInput(session, "ELWS",   value = ELWS)
    if (!is.na(TEMP))   updateNumericInput(session, "TEMP",   value = TEMP)
    if (!is.na(PO4))   updateNumericInput(session, "PO4",   value = PO4)
    if (!is.na(DO))   updateNumericInput(session, "DO",   value = DO)
    if (!is.na(ALG1))   updateNumericInput(session, "ALG1",   value = ALG1)
  })
  

  
  observeEvent(input$var, {
    if (input$var != " ") {
      updateTextInput(session, "output_filename",
                      value = paste0(input$var)
      )
    }
  })
  
  
  
  

  
  
  # Renderiza tabela de Parâmetro / Valor
  output$sim_params <- renderTable({
   req(input$directory_cequal)
    # Seleciona só os componentes de length == 1
    
    scalar_lst <- lst[vapply(lst, length, integer(1)) == 1]
    
    # Monta o data.frame com Parâmetro / Valor
    data.frame(
      Parametro = names(scalar_lst),
      Valor      = unlist(scalar_lst),
      stringsAsFactors = FALSE,
      row.names  = NULL
    )
  }, rownames = FALSE, striped = TRUE, hover = TRUE)

  
  
  
  
  
  output$espaco_imagem <- renderUI({
    if (is.null(input$directory) || is.integer(input$directory)) {
      return(NULL)
    }
    
    diretorio <- parseDirPath(volumes, input$directory)
    img_path  <- file.path(diretorio, "images", "bch.png")
    
    if (!file.exists(img_path)) {
      return(NULL)
    }
    
    # carrega e codifica em base64
    img_base64 <- base64enc::dataURI(file = img_path, mime = "image/png")
    
    # insere a tag <img>
    tags$img(
      src   = img_base64,
      style = "max-width:100%; height:auto;"
    )
  })
  
  
  
  
  
  
  
  output$selectvar <- renderPrint({
    if (input$var == " ") cat("Nenhuma variável selecionada") else cat("Variável:", input$var)
  })
  
  output$dateRangeText <- renderText({
    paste("Período:", paste(as.character(input$dateRange), collapse = " a "))
  })
  
  output$n_rchselec <- renderText({ input$n_rch })
  
  
  output$param_filepath <- renderText({
    req(input$param_file)
    
    path <- input$param_file$datapath  # CORRETO: usando o input certo
    validate(need(file.exists(path), "Arquivo não encontrado."))
    
    param <- ler_linhas_seguro(path, warn = FALSE, descricao = "arquivo de parâmetros")
    paste("Primeiras linhas do arquivo:\n", paste(param, collapse = "\n"))
  })
  
  
  output$param_table <- renderTable({
     req(input$param_file)

     path <- input$param_file$datapath
     ext  <- tools::file_ext(input$param_file$name)

      guess_sep <- function(path, n = 1) {
      first_line <- ler_linhas_seguro(path, n = n, descricao = "arquivo de parâmetros")
      counts <- sapply(c(comma = ",", semicolon = ";", tab = "\t", space = " "),
                       function(d) stringr::str_count(first_line, fixed(d)))
      delim <- names(which.max(counts))
      c(comma = ",", semicolon = ";", tab = "\t", space = " ")[delim]
    }
    
   
    sep <- guess_sep(path)
    df <- switch(ext,
                 csv = read.csv(path, sep = sep, stringsAsFactors = FALSE),
                 prn = fread(path),          # data.table autodetect
                 txt = {
                   
                   read.table(path, header = TRUE, sep = sep, stringsAsFactors = FALSE)
                 },
                 stop("Formato não suportado.")
    )
    
    
    expected_cols <- switch(as.character(input$tipo_simulacao),
                            "1" = 5,
                            "2" = 10,
                            "3" = 8,
                            NA)
    
    if (!is.na(expected_cols) && ncol(df) != expected_cols) {
      showModal(modalDialog(
        title = "Arquivo inválido",
        paste0("O arquivo deve ter ", expected_cols,
               " parâmetros para o tipo de simulação selecionado (tipo ",
               input$tipo_simulacao, ")."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
  
      param_ok(FALSE)   # marca como inválido
      return(NULL)  # Interrompe a renderização
    }
    
    param_ok(TRUE)      # marca como válido
    head(df, 20)
    
    
    
    
  }, rownames = FALSE)
  
 
 
  

observeEvent(input$update, {
  diretorio <- parseDirPath(volumes, input$directory)
  
  # --- valida diretório ---
  if (is.null(diretorio) || length(diretorio) == 0 || diretorio == "") {
    showModal(modalDialog(
      title = "Diretório não selecionado",
      "Selecione um diretório antes de prosseguir.",
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
    return(NULL)
  }
  
  # --- valida arquivos obrigatórios ---
  cio_file <- file.path(diretorio, "file.cio")
  rch_file <- file.path(diretorio, "output.rch")
  if (!(file.exists(cio_file) && file.exists(rch_file))) {
    missing <- c()
    if (!file.exists(cio_file)) missing <- c(missing, "file.cio")
    if (!file.exists(rch_file)) missing <- c(missing, "output.rch")
    
    showModal(modalDialog(
      title = "Arquivos obrigatórios não encontrados",
      HTML(paste0(
        "Não foi possível iniciar a execução.<br><br>",
        "Faltando:<br><b>", paste(missing, collapse = ", "),
        "</b><br><br>Selecione novamente o diretório correto."
      )),
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
    return(NULL)
  }
  
  # --- valida variável ---
  if (is.null(input$var) || input$var == " ") {
    showModal(modalDialog(
      title = "Variável não selecionada",
      "Selecione uma variável antes de prosseguir.",
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
    return(NULL)
  }
  
  # --- valida datas ---
  if (is.null(input$dateRange) || any(is.na(input$dateRange))) {
    showModal(modalDialog(
      title = "Período não definido",
      "Defina as datas de início e fim antes de prosseguir.",
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
    return(NULL)
  }
  if (input$dateRange[1] > input$dateRange[2]) {
    showModal(modalDialog(
      title = "Datas inválidas",
      "A data inicial deve ser anterior à final.",
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
    return(NULL)
  }
  
  if (is.null(input$n_rch) || is.na(input$n_rch)) {
    showModal(modalDialog(
      title = "Número do braço não informado",
      "Digite o número do braço/sub-bacia antes de prosseguir.",
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
    return(NULL)
  }
  
  # --- lê dados ---
  withProgress(message = 'Processando dados...', value = 0, {
    incProgress(0.1, detail = "Lendo arquivos...")
    data <- ReadFile_cio_SWAT(pathin_rch = paste0(diretorio, "/"))
    data_i <- as.Date(data[1], format = "%d-%m-%Y")
    data_f <- as.Date(data[2], format = "%d-%m-%Y")
    dado_swat <- ReadRCH_SWAT(
      pathin_rch = paste0(diretorio, "/"),
      data_i = data_i,
      data_f = data_f
    )
    
    
    ###Conversão de massa de clorofila (kg) para concentração de alga(g/m3)
    ACHLA<-0.05
    CHLA_INug<-dado_swat$CHLA_INkg*1e9
    ALGA_INgm3<-CHLA_INug*ACHLA/(dado_swat$FLOW_INcms*24*3600*1000)
    dado_swat$CHLA_INkg<-ALGA_INgm3
    names(dado_swat)[names(dado_swat) == "CHLA_INkg"] <- "ALGA_INg/m3"
    
    
    CHLA_OUTug<-dado_swat$CHLA_OUTkg*1e9
    ALGA_OUTgm3<-CHLA_OUTug*ACHLA/(dado_swat$FLOW_OUTcms*24*3600*1000)
    dado_swat$CHLA_OUTkg<-ALGA_OUTgm3
    names(dado_swat)[names(dado_swat) == "CHLA_OUTkg"] <- "ALGA_OUTg/m3"
    
    
    ###Conversão de massa de oxigenio dissolvido (kg) para concentração de oxigenio (g/m3)
    DISOX_INgm3<-dado_swat$DISOX_INkg*1000/(dado_swat$FLOW_INcms*24*3600)
    dado_swat$DISOX_INkg<-DISOX_INgm3
    names(dado_swat)[names(dado_swat) == "DISOX_INkg"] <- "DISOX_INg/m3"
     
    DISOX_OUTgm3<-dado_swat$DISOX_OUTkg*1000/(dado_swat$FLOW_OUTcms*24*3600)
    dado_swat$DISOX_OUTkg<-DISOX_OUTgm3
    names(dado_swat)[names(dado_swat) == "DISOX_OUTkg"] <- "DISOX_OUTg/m3"
 
    
    ###Conversão de massa de fOSFATO (kg) para concentração de FOSFATO (g/m3)
    MINP_INgm3<-dado_swat$MINP_INkg*1000/(dado_swat$FLOW_INcms*24*3600)
    dado_swat$MINP_INkg<-MINP_INgm3
    names(dado_swat)[names(dado_swat) == "MINP_INkg"] <- "MINP_INg/m3"
    
    
    MINP_OUTgm3<-dado_swat$MINP_OUTkg*1000/(dado_swat$FLOW_OUTcms*24*3600)
    dado_swat$MINP_OUTkg<-MINP_OUTgm3
    names(dado_swat)[names(dado_swat) == "MINP_OUTkg"] <- "MINP_OUTg/m3"
    
    
  
    # --- valida n_rch ---
    range_rch <- unique(dado_swat$RCH)
  
    if (input$n_rch < min(range_rch) || input$n_rch > max(range_rch)) {
      showModal(modalDialog(
        title = "Número inválido",
        paste0("O número deve estar entre ", min(range_rch), " e ", max(range_rch)),
        easyClose = TRUE,
        footer = modalButton("Fechar")
      ))
      return(NULL)
    }
    
    # --- seleciona variável ---
    incProgress(0.3, detail = "Filtrando variáveis...")
    var_swat <- subset(dado_swat, RCH == input$n_rch)
    var_swat$date <- seq.Date(from = data_i, to = data_f, by = "day")
    
    if (input$var == "PO4 , CHLA, OD - Entrada") {
      name_var <- c("MINP_INg/m3", "ALGA_INg/m3","DISOX_INg/m3")
    } else if (input$var == "PO4 , CHLA, OD - Saída") {
      name_var <- c("MINP_OUTg/m3", "ALGA_OUTg/m3","DISOX_OUTg/m3")
    } else {
      name_var <- input$var
    }
    
    # --- valida período dentro dos dados ---
    if (input$dateRange[1] < data_i || input$dateRange[2] > data_f) {
      showModal(modalDialog(
        title = "Período inválido",
        paste0("O período deve estar entre ",
               format(data_i, "%d-%m-%Y"), " e ", format(data_f, "%d-%m-%Y")),
        easyClose = TRUE,
        footer = modalButton("Fechar")
      ))
      return(NULL)
    }
    
    # --- filtra série ---
    var_swat1 <- subset(var_swat, select = c("date", name_var))
    var_swat1 <- dplyr::filter(var_swat1, date >= input$dateRange[1], date <= input$dateRange[2])
    
   
    
    # --- gera arquivo ---
    incProgress(0.7, detail = "Gerando arquivo...")
    if (!dir.exists(file.path(diretorio, "dadosCEQUAL"))) {
      dir.create(file.path(diretorio, "dadosCEQUAL"))
    }
    
    if (is.null(input$output_filename) || input$output_filename == "") {
      showNotification("Defina um nome para o arquivo de saída", type = "error")
      return(NULL)
    }
    file_name <- file.path(diretorio, "dadosCEQUAL", paste0(input$output_filename, ".prn"))
    
    cab <- paste0(year(input$dateRange[1]), " a ", year(input$dateRange[2]),
                  ", ", input$reserv, ", Braço ", input$n_rch, ", Variável ", input$var)
    cab2 <- t(as.matrix(c("JDAY", substring(name_var, 1, 7))))
    
    juliano.ref <- as.numeric(julian(as.Date(paste0("01-01-", year(input$dateRange[1])), "%d-%m-%Y")))
    julian.inicio <- julian(input$dateRange[1]) - juliano.ref + 1
    tam <- input$dateRange[2] - input$dateRange[1] + 1
    JDAY <- sprintf("%.2f", seq(from = julian.inicio, length.out = tam, by = 1))
    
    var <- as.matrix(var_swat1[, -1])
    var <- signif(var, digits = 2)
    dado_entrada <- cbind(JDAY, var)
    
    validar_arquivo_para_gravacao(file_name, "arquivo de dados")
    write(cab, file_name)
    write("", file_name, append = TRUE)
    write.fwf(cab2, file_name, sep = "", append = TRUE,
              colnames = FALSE, rownames = FALSE,
              width = rep(8, ncol(cab2)), justify = "right", eol = "\n")
    write.fwf(as.data.frame(dado_entrada), file_name, sep = "", append = TRUE,
              colnames = FALSE, rownames = FALSE,
              width = rep(8, ncol(dado_entrada)), justify = "right", eol = "\n")
    
    incProgress(1, detail = "Finalizado")
    
    showModal(modalDialog(
      title = "Dados gerados com sucesso",
      HTML(paste0("Arquivo salvo em: <b>", dirname(file_name), "</b>")),
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })
})



  
  
  
   preparar_contexto <- function(tipo) {
   
###aqui comeca auxiliar
     
     
     
     d <- diretorio_cequal_sel()
     if (length(d) == 0 || d == "") {
       showModal(modalDialog(
         title = "Atenção",
         "Você precisa selecionar um diretório antes de continuar.",
         easyClose = TRUE,
         footer = modalButton("OK")
       ))
       return(NULL)
     }
     
  
     if (projeto_ok()==FALSE) {
       showModal(modalDialog(
         title = "Erro",
         "O diretorio selecionado não contem o arquivo de controle.",
         easyClose = TRUE,
         footer = modalButton("OK")
       ))
       return(NULL)
     }
     
     

     
 
     

     # Lê o arquivo como tabela (ajuste conforme o formato real)

  
  
     reserv_dict <- c(
       "Acarape do Meio" = "acm",
       "Araras" = "ara",
       "Banabuiú" = "ban",
       "Castanhão" = "cas",
       "Curral Velho" = "cvl",
       "Edson Queiroz" = "edq",
       "Jaburú 1" = "jab",
       "Orós" = "oro",
       "Olho Dágua" = "oda",
       "Pacoti" = "pac"
     )
  

  ##
  
  reserv_nome <- input$reserv
  res<-reserv_sigla <- reserv_dict[reserv_nome]
  diretorio_cequal <- diretorio_cequal_sel()
  
  caminho_arquivo <- file.path(
    diretorio_cequal, 
    paste0("dados_obs_", res, ".xlsx")
  )
  
  aba_esperada <- paste0("dados_obs_", res)
  
  if (!file.exists(caminho_arquivo)) {
    showModal(modalDialog(
      title = "Arquivo não encontrado",
      HTML(paste0(
        "<b>Arquivo esperado:</b> dados_obs_", res, ".xlsx<br>",
        "<b>Aba esperada:</b> ", aba_esperada, "<br><br>",
        "<b>Diretório:</b><br><i>", diretorio_cequal, "</i>"
      )),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(NULL)
  }
  
  
  if (is.null(input$reserv) || input$reserv == " ") {
    showModal(modalDialog(
      title = "Reservatório não selecionado",
      "Você deve selecionar um reservatório antes de prosseguir.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(NULL)
  }
  
  erro_leitura <- tryCatch(
    {
      validar_arquivo_para_leitura(
        caminho_arquivo,
        "planilha de dados observados"
      )
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  if (!is.null(erro_leitura)) {
    showModal(modalDialog(
      title = "Não foi possível abrir a planilha",
      erro_leitura,
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(NULL)
  }

  abas_disponiveis <- tryCatch(
    readxl::excel_sheets(caminho_arquivo),
    error = function(e) {
      showModal(modalDialog(
        title = "Planilha inválida ou indisponível",
        paste(
          "Não foi possível examinar a planilha.",
          "Feche o arquivo no Excel e verifique a sincronização do OneDrive.",
          conditionMessage(e)
        ),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      NULL
    }
  )
  if (is.null(abas_disponiveis)) {
    return(NULL)
  }
  
  if (!(aba_esperada %in% abas_disponiveis)) {
    showModal(modalDialog(
      title = "Aba não encontrada",
      HTML(paste0(
        "<b>Arquivo encontrado:</b> ", basename(caminho_arquivo), "<br>",
        "<b>Aba esperada:</b> ", aba_esperada, "<br><br>",
        "<b>Abas disponíveis:</b><br><i>",
        paste(abas_disponiveis, collapse = "<br>"),
        "</i>"
      )),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(NULL)
  }
##

  dados_sim <- data.frame(
    res          = reserv_sigla,
    ITSR          = input$ITSR,
    TMSTRT       = input$TMSTRT,
    TMEND        = input$TMEND,
    YEAR         = input$YEAR,
    ELWS         = input$ELWS,
    TEMP         = input$TEMP,
    tipo         = tipo,
    preproc      = 1, 
    PO4          = input$PO4,
    DO           = input$DO,
    ALG1         = input$ALG1,
    arquivo_controle = arquivo_controle_sel(),
    #  IMX          = input$IMX,
    #  CTMAX        = input$CTMAX, 
    #  EBOT         = input$EBOT,
    stringsAsFactors = FALSE
  )
  # preproc=dados_sim$preproc    
  
  
  dados_simulacao <- carregar_dados_simulacao(
    diretorio_cequal,
    dados_sim)

  
  
  
  criar_contexto_simulacao(
    diretorio_cequal = diretorio_cequal,
    arquivo_controle = arquivo_controle_sel(),
    reserv_sigla = reserv_sigla,
    dados_sim = dados_sim,
    dados_simulacao = dados_simulacao
  )
  
  
}  
      
      
      
  ####aqui termina auxiliar
      
####### EXECUTAR RUN SIMULAÇÃO
   
      
      observeEvent(input$run_exe, {
        
        nobj=2
        
        if (is.null(input$param_file)) {
          showModal(modalDialog(
            title = "Arquivo não encontrado",
            "Você precisa selecionar o arquivo de parâmetros antes de executar a simulação.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return(NULL)
        }
        
        #  req(input$param_file)  # garante que o arquivo existe
        path <- input$param_file$datapath
        validate(need(file.exists(path), "Arquivo não encontrado."))
        param <- ler_parametros(path)    
        
        
        
        if (!param_ok()) {
          showModal(modalDialog(
            title = "Erro",
            "O arquivo de parâmetros carregado não é compatível com o tipo selecionado.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return(NULL)
        }
        
        contexto <- preparar_contexto(input$tipo_simulacao)
        preproc <- contexto$configuracao$preproc
      
        if (is.null(contexto)) return(NULL)
        
        diretorio_cequal <- contexto$caminhos$diretorio
        
        ###inicio da simulação SEM janelas RUN
        
    if (isFALSE(input$use_windows)) {
    
     # Inicializa a matriz de erro
    
      erro_matrix <- matrix(NA, nrow = nrow(param), ncol = nobj)
      erro_list <- vector("list", nrow(param))
      
    
    
      
     for (i in 1:nrow(param)) {
    
       parametros_simul <- param[i, ]
       
       #crio os arquivos nescessario para simular com o CEQUAL-W2
       
       gera_arquivo <- preparar_arquivos_cequal(
         parametros = parametros_simul,
         contexto = contexto
       )
 
       arq_exe1 <- file.path(diretorio_cequal, "0_RunW2Pre.Bat")
       arq_exe2 <- file.path(diretorio_cequal, "0_RunW2.Bat")

       
       bat_pre <- c(
         "@echo off",
         paste0("cd /d \"", diretorio_cequal, "\""),
         paste0("\"", file.path(app_dir, "bin", "W2Pre3.7.exe"), "\"")
       )
  
       gravar_linhas_seguro(bat_pre, arq_exe1, descricao = "script de pré-processamento")
       
       
       # Conteúdo do BAT principal
       bat_run <- c(
         "@echo off",
         paste0("cd /d \"", diretorio_cequal, "\""),
         paste0("\"", file.path(app_dir, "bin", "w2_3.7_64.exe"), "\"")
       )
       gravar_linhas_seguro(bat_run, arq_exe2, descricao = "script de execução")
       
    
         if (preproc == 1) {
           shell(shQuote(arq_exe1), wait = TRUE)
         preproc <- 0
       }
     
         execucao_cequal <- executar_cequal_monitorado(
           executavel = file.path(app_dir, "bin", "w2_3.7_64.exe"),
           diretorio = diretorio_cequal
         )
         if (execucao_cequal$codigo != 0L) {
           showNotification(
             "Esta simulação falhou e seus resultados foram ignorados.",
             type = "error",
             duration = 10
           )
           next
         }
       
   
           ##analiso os resultados, salvo figuras, etc
       metricas <- processar_resultados_cequal(
         simul = i,
         salvar_figura = 1,
         salvar_resultados = 1,
         simul_aleatoria = 0,
         contexto = contexto
       )

        erro_list[[i]] <- metricas
     
       preproc <- 0
     }
   
     # Salva a lista de metricas (ajuste o caminho conforme necessário)
   
      library(dplyr)
     
     metricas_all <- bind_rows(
       lapply(erro_list, function(x) x$metricas),
       .id = "simulacao"
     )
     if (nrow(metricas_all) == 0L) {
       showNotification(
         "Nenhuma simulação produziu resultados válidos.",
         type = "error",
         duration = 10
       )
       return(NULL)
     }
     
     gravar_tabela_segura(
       metricas_all,
       caminho = file.path(
         diretorio_cequal,
         paste0("resultados_tipo", input$tipo_simulacao, "/metricas.csv")
       ),
       gravador = readr::write_csv
     )
     
     
     
     showNotification(
       ui = "Simulação concluída com sucesso.",
       type = "message",   # "default", "message", "warning", "error"
       duration = 5        # tempo (segundos) que o banner fica visível
     )
     
     
     
     ####---- fim da simulaçao sem janelas
     
     
    }else{
      
      ########--- iNICIO simulação janelas
     
      if (nrow(contexto$monitoramento) <= 1) {
        
        showModal(modalDialog(
          title = "Dados insuficientes",
          "A janela de dados selecionada possui apenas uma observação.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
        return(NULL)
      }
      
      
      
      erro_janela <- vector("list", nrow(contexto$monitoramento))
      erro_param <- vector("list", nrow(param))
      
      
      for (i in 1:nrow(param)) {
      parametros_simul <- param[i, ]
      
      for (sim_j in 1:(nrow(contexto$monitoramento)-1)){
        
        contexto_janela <- criar_contexto_janela(
          contexto = contexto,
          indice = sim_j,
          carregar_dados = carregar_dados_simulacao
        )
        gera_arquivo <- preparar_arquivos_cequal(
          parametros = parametros_simul,
          contexto = contexto_janela
        )
     
        arq_exe1 <- file.path(diretorio_cequal, "0_RunW2Pre.Bat")
        arq_exe2 <- file.path(diretorio_cequal, "0_RunW2.Bat")
        
        bat_pre <- c(
          "@echo off",
          paste0("cd /d \"", diretorio_cequal, "\""),
          paste0("\"", file.path(app_dir, "bin", "W2Pre3.7.exe"), "\"")
        )
        
        gravar_linhas_seguro(bat_pre, arq_exe1, descricao = "script de pré-processamento")
        
        
        # Conteúdo do BAT principal
        bat_run <- c(
          "@echo off",
          paste0("cd /d \"", diretorio_cequal, "\""),
          paste0("\"", file.path(app_dir, "bin", "w2_3.7_64.exe"), "\"")
        )
        gravar_linhas_seguro(bat_run, arq_exe2, descricao = "script de execução")
        
        
        if (preproc == 1) {
          shell(shQuote(arq_exe1), wait = TRUE)
          preproc <- 0
        }
        
        execucao_cequal <- executar_cequal_monitorado(
          executavel = file.path(app_dir, "bin", "w2_3.7_64.exe"),
          diretorio = diretorio_cequal
        )
        if (execucao_cequal$codigo != 0L) {
          showNotification(
            paste("A janela", sim_j, "falhou e foi ignorada."),
            type = "error",
            duration = 10
          )
          next
        }

        identificador_execucao <- paste0(i, "_jan_", sim_j)

        metricas <- processar_resultados_cequal(
          simul = identificador_execucao,
          salvar_figura = 1,
          salvar_resultados = 1,
          simul_aleatoria = 0,
          subpasta_resultados = "Janela",
          contexto = contexto_janela
        )

        erro_janela[[sim_j]] <- metricas

      }
        metricas_consolidadas <- bind_rows(
          lapply(erro_janela, function(x) x$metricas),
          .id = "janela"
        )
        if (nrow(metricas_consolidadas) == 0L) {
          showNotification(
            paste("Nenhuma janela válida foi produzida na simulação", i),
            type = "error",
            duration = 10
          )
          erro_param[[i]] <- list()
          next
        }
        metricas_consolidadas <- metricas_consolidadas %>%
          mutate(
            simulacao = i,
            janela = as.integer(janela),
            identificador = paste0(simulacao, "_jan_", janela)
          )
        
        gravar_tabela_segura(
          metricas_consolidadas,
          caminho = file.path(diretorio_cequal, paste0(
            "/resultados_tipo", input$tipo_simulacao,
            "/Janela/metricas_simul_", i, ".csv"
          )),
          gravador = readr::write_delim
        )
        
        salvar_rds_seguro(
          erro_janela,
          caminho = file.path(diretorio_cequal, paste0(
            "/resultados_tipo", input$tipo_simulacao,
            "/Janela/metricas_simul_", i, ".rds"
          ))
        )
       
        tabela_reorganizada <- metricas_consolidadas %>%
          pivot_wider(
            id_cols = c(simulacao, janela, identificador),
            names_from = variavel,
            values_from = c(RMSE, MAE, Bias, NSE, Skill_Persistencia),
            names_glue = "{.value}_{variavel}"
          )

       # print(tabela_reorganizada)
     
        
        
     
  
 
        
        
        metricas_para_graficos <- metricas_consolidadas %>%
          mutate(simulacao = janela)

        gerar_graficos_rmse(
          metricas_consolidadas = metricas_para_graficos,
          diretorio_cequal = diretorio_cequal,
          tipo = input$tipo_simulacao,
          subpasta_resultados = "Janela",
          identificador_simulacao = i
        )
    
        erro_param[[i]]<-erro_janela
      }
    
      metricas_consolidadas <- bind_rows(lapply(
        seq_along(erro_param),
        function(indice_simulacao) {
          bind_rows(
            lapply(erro_param[[indice_simulacao]], function(x) x$metricas),
            .id = "janela"
          ) %>%
            mutate(
              simulacao = indice_simulacao,
              janela = as.integer(janela),
              identificador = paste0(simulacao, "_jan_", janela)
            )
        }
      ))
      if (nrow(metricas_consolidadas) == 0L) {
        showNotification(
          "Nenhuma simulação por janelas produziu resultados válidos.",
          type = "error",
          duration = 10
        )
        return(NULL)
      }
      
      gravar_tabela_segura(
        metricas_consolidadas,
        caminho = file.path(diretorio_cequal, paste0(
          "/resultados_tipo", input$tipo_simulacao,
          "/Janela/metricas_janela_param.csv"
        )),
        gravador = readr::write_delim
      )
      
      salvar_rds_seguro(
        erro_param,
        caminho = file.path(diretorio_cequal, paste0(
          "/resultados_tipo", input$tipo_simulacao,
          "/Janela/metricas_janela_param.rds"
        ))
      )
   
   
      
    }
      
      
      
      output$exe_log <- renderText({
       paste("Simulação Concluída", collapse = "\n")
     })
   })
   
   
   
   
   

      output$cenario_preview <- renderTable({
        req(input$directory_cequal, input$directory_cenario_cequal, input$cenario_id)

        diretorio_base <- diretorio_cequal_sel()
        diretorio_cenario <- parseDirPath(volumes, input$directory_cenario_cequal)

        validate(
          need(length(diretorio_base) > 0 && diretorio_base != "", "Selecione o diretório base do CE-QUAL-W2."),
          need(length(diretorio_cenario) > 0 && diretorio_cenario != "", "Selecione a pasta do cenário.")
        )

        prev <- montar_preview_cenario_afluencia(
          diretorio_base = diretorio_base,
          diretorio_cenario = diretorio_cenario,
          scenario_id = input$cenario_id,
          arquivo_controle_base = arquivo_controle_sel()
        )

        prev$arquivos
      }, rownames = FALSE)

      observeEvent(input$prepare_cenario_afluencia, {
        diretorio_base <- diretorio_cequal_sel()
        diretorio_cenario <- parseDirPath(volumes, input$directory_cenario_cequal)

        if (length(diretorio_base) == 0 || diretorio_base == "") {
          showModal(modalDialog(
            title = "Diretório do CE-QUAL-W2 não selecionado",
            "Selecione a pasta base do projeto CE-QUAL-W2 antes de preparar a cenarização.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return(NULL)
        }

        if (length(diretorio_cenario) == 0 || diretorio_cenario == "") {
          showModal(modalDialog(
            title = "Pasta do cenário não selecionada",
            "Selecione a pasta onde estão os arquivos do cenário de afluência.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return(NULL)
        }

        if (is.null(input$cenario_id) || trimws(input$cenario_id) == "") {
          showModal(modalDialog(
            title = "Identificador do cenário não informado",
            "Informe o identificador do cenário, por exemplo S1.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return(NULL)
        }

        resultado_cenario <- tryCatch(
          preparar_cenario_afluencia_cequal(
            diretorio_base = diretorio_base,
            diretorio_cenario = diretorio_cenario,
            scenario_id = input$cenario_id,
            arquivo_controle_base = arquivo_controle_sel()
          ),
          error = function(e) e
        )

        if (inherits(resultado_cenario, "error")) {
          showModal(modalDialog(
            title = "Erro ao preparar cenário",
            HTML(gsub("\n", "<br>", resultado_cenario$message)),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          output$cenario_log <- renderText(resultado_cenario$message)
          return(NULL)
        }

        output$cenario_log <- renderText({
          paste(
            "Cenário preparado com sucesso.",
            paste0("Arquivo de controle salvo em: ", resultado_cenario$arquivo_controle_cenario),
            paste0("Arquivos copiados para a pasta raiz: ", resultado_cenario$diretorio_base),
            sep = "\n"
          )
        })

        showNotification(
          ui = paste0("Cenário ", resultado_cenario$scenario_id, " preparado com sucesso."),
          type = "message",
          duration = 5
        )
      })


      observeEvent(input$run_cenario_afluencia, {
        diretorio_base <- diretorio_cequal_sel()
        diretorio_cenario <- parseDirPath(volumes, input$directory_cenario_cequal)

        if (length(diretorio_base) == 0 || diretorio_base == "") {
          showModal(modalDialog(
            title = "Diretório do CE-QUAL-W2 não selecionado",
            "Selecione a pasta base do projeto CE-QUAL-W2 antes de rodar o cenário.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return(NULL)
        }

        if (length(diretorio_cenario) == 0 || diretorio_cenario == "") {
          showModal(modalDialog(
            title = "Pasta do cenário não selecionada",
            "Selecione a pasta do cenário antes de iniciar a simulação.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return(NULL)
        }

        if (is.null(input$cenario_id) || trimws(input$cenario_id) == "") {
          showModal(modalDialog(
            title = "Identificador do cenário não informado",
            "Informe o identificador do cenário, por exemplo S1.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return(NULL)
        }

        withProgress(message = "Rodando cenário CE-QUAL-W2...", value = 0, {
          incProgress(0.1, detail = "Preparando arquivos do cenário...")
          resultado_execucao <- tryCatch(
            executar_cenario_afluencia_cequal(
              diretorio_base = diretorio_base,
              diretorio_cenario = diretorio_cenario,
              scenario_id = input$cenario_id,
              app_dir = app_dir,
              arquivo_controle_base = arquivo_controle_sel()
            ),
            error = function(e) e
          )

          if (inherits(resultado_execucao, "error")) {
            showModal(modalDialog(
              title = "Erro ao rodar cenário",
              HTML(gsub("\n", "<br>", resultado_execucao$message)),
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
            output$cenario_log <- renderText(resultado_execucao$message)
            return(NULL)
          }

          incProgress(1, detail = "Concluído")

          output$cenario_log <- renderText({
            paste(
              paste0("Cenário ", resultado_execucao$scenario_id, " executado com sucesso."),
              paste0("Pasta de resultados: ", resultado_execucao$diretorio_resultados),
              paste0("Arquivo de controle usado: ", resultado_execucao$arquivo_controle_raiz),
              sep = "\n"
            )
          })

          showNotification(
            ui = paste0("Cenário ", resultado_execucao$scenario_id, " executado com sucesso."),
            type = "message",
            duration = 5
          )
        })
      })


      output$vars_obj_ui <- renderUI({
        
        choices_vars <- switch(input$tipo,
                               
                               "1" = c(
                                 "Cota" = "Cota",
                                 "Temperatura Superfície" = "TEMP",
                                 "Evaporação" = "EVAP"
                               ),
                               
                               "2" = c(
                                 "Cota" = "Cota",
                                 "Temperatura Superfície" = "TEMP",
                                # "Temperatura Perfil" = "TEMP",#"TEMPPERFIL",
                                 "Evaporação" = "EVAP"
                               ),
                               
                               "3" = c(
                                 "Fósforo" = "PO4",
                                 "Clorofila" = "ALG1",
                                 "Oxigênio Dissolvido" = "DO"
                               )
        )
        
        checkboxGroupInput(
          "vars_obj",
          "3: Selecione as variáveis objetivo",
          choices = choices_vars,
          selected = choices_vars[1]
        )
      })
   
   library(DT)
   
   # inicia com tipo 1
   limites_reactive <- reactiveVal(gerar_limites("1"))
   
   # atualiza quando muda o tipo
   observeEvent(input$tipo, {
     limites_reactive(gerar_limites(input$tipo))
   })
   
   # renderiza tabela
   output$tabela_limites <- DT::renderDT({
     
     tabela <- limites_reactive()
     
     tabela_exibicao <- setNames(
       tabela,
       c("Parâmetro", "Limite Inferior", "Limite Superior")
     )
     
     datatable(
       tabela_exibicao,
       editable = list(
         target = "cell",
         disable = list(columns = 0)
       ),
       rownames = FALSE,
       class = "compact",
       options = list(
         dom = 't',
         paging = FALSE,
         autoWidth = FALSE
       )
     )
     
   })   
   
  
   
   
   
   observeEvent(input$tabela_limites_cell_edit, {
     
     info <- input$tabela_limites_cell_edit
     tabela <- limites_reactive()
     
     coluna_real <- info$col + 1
     
     # Não permitir editar coluna "Parâmetro"
     if(coluna_real == 1) return(NULL)
     
     # 🔹 Se estiver vazio → vira 0
     if(info$value == "" || is.null(info$value)){
       valor <- 0
     } else {
       valor <- suppressWarnings(as.numeric(info$value))
       if(is.na(valor)) valor <- 0
     }
     
     tabela[info$row, coluna_real] <- valor
     
     limites_reactive(tabela)
     
   })
   
   resultado_otm <- reactiveVal(NULL)
   
   
   observeEvent(input$run_opt, {
     
     mostrar_pareto(FALSE)
     d <- diretorio_cequal_sel()
     if (length(d) == 0 || d == "") {
       showModal(modalDialog(
         title = "Atenção",
         "Você precisa selecionar um diretório antes de continuar.",
         easyClose = TRUE,
         footer = modalButton("OK")
       ))
       return(NULL)
     }
     
     # if (is.null(input$reserv) || input$reserv == " ") {
     #   showModal(modalDialog(
     #     title = "Reservatório não selecionado",
     #     "Você deve selecionar um reservatório antes de prosseguir.",
     #     easyClose = TRUE,
     #     footer = modalButton("OK")
     #   ))
     #   return(NULL)
     # }
     
     
     contexto <- preparar_contexto(input$tipo)
   
     
     
     
     tabela_final <- limites_reactive()
     
     # Verificação 1: lower < upper
     linhas_invalidas <- which(tabela_final$lower >= tabela_final$upper)
     
     if(length(linhas_invalidas) > 0){
       
       parametros_errados <- tabela_final$parametro[linhas_invalidas]
       
       showModal(modalDialog(
         title = "Erro nos limites",
         paste(
           "Os seguintes parâmetros possuem Limite Inferior maior ou igual ao Limite Superior:",
           paste(parametros_errados, collapse = ", ")
         ),
         easyClose = TRUE,
         footer = modalButton("Fechar")
       ))
       
       return(NULL)  # impede continuar
     }
     
     # impedir negativos
     if(any(tabela_final$lower < 0) || any(tabela_final$upper < 0)){
       
       showModal(modalDialog(
         title = "Erro nos limites",
         "Não são permitidos valores negativos.",
         easyClose = TRUE,
         footer = modalButton("Fechar")
       ))
       
       return(NULL)
     }
     
     # Se passou na validação, continua
     lower <- tabela_final$lower
     upper <- tabela_final$upper
     nvar  <- nrow(tabela_final)
     varsobj <- input$vars_obj
     nobj <- length(varsobj)
     
  
     
     popSize   <- input$popSize
     maxiter   <- input$maxiter
     pmutation <- input$pmutation
     
     
   
     
################## CHAMA FUNÇÃO DE OTIMIZAÇÃO
     fitness_wrapper <- function(x){
       Otmiza_Quali(
         x,
         contexto = contexto,
         diretorio_cequal = contexto$caminhos$diretorio,
         varsobj = input$vars_obj,
         objfun = input$obj_fun,
         windows=input$use_windows
       )
     }
     
     
     OTM <- mopsocd(
       fn = fitness_wrapper,
       varcnt = nvar,
       fncnt  = nobj,
       pMut   = pmutation,
       lowerbound = lower,
       upperbound = upper,
       opt = 0,
       popsize = popSize,
       maxgen = maxiter
     )
    
     
     ##### fim FUNÇÃO otimizacao
   
     
     # -----------------------------
     # nomes dos parâmetros e objetivos
     # -----------------------------
     limites <- gerar_limites(input$tipo)
     nomes_param <- limites$parametro
     
     info_obj <- gerar_info_objetivo(input$vars_obj, input$obj_fun)
     nomes_obj <- info_obj$nomes_obj
     
     # -----------------------------
     # monta data frames (robusto para vetor ou matriz)
     # -----------------------------
     # -----------------------------
     if (is.null(dim(OTM$paramvalues))) {
       df_param <- as.data.frame(matrix(OTM$paramvalues, nrow = 1))
     } else {
       df_param <- as.data.frame(OTM$paramvalues)
     }
     
     # -----------------------------
     # objetivos
     # -----------------------------
     if (is.null(dim(OTM$objfnvalues))) {
       df_obj <- as.data.frame(matrix(OTM$objfnvalues, nrow = 1))
     } else {
       df_obj <- as.data.frame(OTM$objfnvalues)
     }
     # -----------------------------
     # atribui nomes
     # -----------------------------
     colnames(df_param) <- nomes_param
     colnames(df_obj)   <- nomes_obj
     
     # -----------------------------
     # junta tudo
     # -----------------------------
     df_resultados <- cbind(
       Solucao = seq_len(nrow(df_param)),
       df_param,
       df_obj
     )
     
     
     

     

     # salva no reativo
     resultado_otm(df_resultados)
    
     gravar_tabela_segura(
       df_resultados,
       caminho = paste0(contexto$caminhos$diretorio, "/resultados_tipo", input$tipo, "/pareto_resultados.csv"),
       gravador = utils::write.csv,
       row.names = FALSE
     )
     
     gravar_tabela_segura(
       df_param,
       caminho = paste0(contexto$caminhos$diretorio, "/resultados_tipo", input$tipo, "/parametros_otm.csv"),
       gravador = utils::write.csv,
       row.names = FALSE
     )
     
     mostrar_pareto(TRUE)
     
     output$opt_log <- renderPrint({
       print("Otimização finalizada.")
    #   print(OTM)
     })
     
   })
   
   
   output$pareto_plot <- plotly::renderPlotly({
     
     req(resultado_otm())
     
     df <- resultado_otm()
     
     info_obj <- gerar_info_objetivo(input$vars_obj, input$obj_fun)
     nomes_obj <- info_obj$nomes_obj
     
     # não faz gráfico se houver menos de 2 objetivos
     req(length(nomes_obj) >= 2)
     
     nomes_param <- gerar_limites(input$tipo)$parametro
     
     # checagens
     if (!all(nomes_obj %in% colnames(df))) {
       stop("As colunas das funções objetivo não foram encontradas no data frame de resultados.")
     }
     
     if (!all(nomes_param %in% colnames(df))) {
       stop("As colunas dos parâmetros não foram encontradas no data frame de resultados.")
     }
     
     # texto do hover com parâmetros
     texto_param <- apply(df[, nomes_param, drop = FALSE], 1, function(x) {
       paste(
         paste(nomes_param, round(as.numeric(x), 4), sep = ": "),
         collapse = "<br>"
       )
     })
     
     # texto do hover com objetivos
     texto_obj <- apply(df[, nomes_obj, drop = FALSE], 1, function(x) {
       paste(
         paste(nomes_obj, round(as.numeric(x), 4), sep = ": "),
         collapse = "<br>"
       )
     })
     
     hover_text <- paste0(
       "Solução: ", df$Solucao,
       "<br><br><b>Parâmetros</b><br>", texto_param,
       "<br><br><b>Objetivos</b><br>", texto_obj
     )
     
     n_obj <- length(nomes_obj)
     
     # -------------------------
     # 2 objetivos -> gráfico 2D
     # -------------------------
     if (n_obj == 2) {
       
       p <- plotly::plot_ly(
         data = df,
         x = as.formula(paste0("~`", nomes_obj[1], "`")),
         y = as.formula(paste0("~`", nomes_obj[2], "`")),
         type = "scatter",
         mode = "markers",
         text = hover_text,
         hoverinfo = "text"
       ) %>%
         plotly::layout(
           title = "Frente de Pareto",
           xaxis = list(title = nomes_obj[1]),
           yaxis = list(title = nomes_obj[2])
         )
       
       return(p)
     }
     
     # -------------------------
     # 3 objetivos -> gráfico 3D
     # -------------------------
     if (n_obj == 3) {
       
       p <- plotly::plot_ly(
         data = df,
         x = as.formula(paste0("~`", nomes_obj[1], "`")),
         y = as.formula(paste0("~`", nomes_obj[2], "`")),
         z = as.formula(paste0("~`", nomes_obj[3], "`")),
         type = "scatter3d",
         mode = "markers",
         text = hover_text,
         hoverinfo = "text"
       ) %>%
         plotly::layout(
           title = "Frente de Pareto",
           scene = list(
             xaxis = list(title = nomes_obj[1]),
             yaxis = list(title = nomes_obj[2]),
             zaxis = list(title = nomes_obj[3])
           )
         )
       
       return(p)
     }
     
     # ---------------------------------------
     # acima de 3 objetivos -> parallel coords
     # ---------------------------------------
     if (n_obj > 3) {
       
       dims_obj <- lapply(nomes_obj, function(col) {
         list(
           label = col,
           values = df[[col]]
         )
       })
       
       p <- plotly::plot_ly(
         type = "parcoords",
         line = list(
           color = df[[nomes_obj[1]]]
         ),
         dimensions = dims_obj
       ) %>%
         plotly::layout(
           title = "Frente de Pareto - Coordenadas Paralelas"
         )
       
       return(p)
     }
     
     return(NULL)
   })
   
   
   mostrar_pareto <- reactiveVal(FALSE)
   
   output$pareto_plot_ui <- renderUI({
     
     if (!mostrar_pareto()) {
       return(NULL)
     }
     
     plotlyOutput("pareto_plot", height = "700px")
   })
   
   

}

shinyApp(ui = ui, server = server)

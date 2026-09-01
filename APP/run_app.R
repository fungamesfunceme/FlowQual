# Inicializador do aplicativo SWAT-CE-QUAL-W2.
#
# Execute este arquivo com Rscript ou pelo comando:
#   source("run_app.R")

localizar_pasta_script <- function() {
  argumentos <- commandArgs(trailingOnly = FALSE)
  argumento_arquivo <- grep("^--file=", argumentos, value = TRUE)

  if (length(argumento_arquivo) > 0L) {
    caminho <- sub("^--file=", "", argumento_arquivo[[1L]])
    return(dirname(normalizePath(
      caminho,
      winslash = "/",
      mustWork = TRUE
    )))
  }

  caminhos_source <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) NA_character_ else frame$ofile
    },
    character(1)
  )
  caminhos_source <- caminhos_source[!is.na(caminhos_source)]

  if (length(caminhos_source) > 0L) {
    return(dirname(normalizePath(
      tail(caminhos_source, 1L),
      winslash = "/",
      mustWork = TRUE
    )))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

app_dir <- localizar_pasta_script()
arquivo_app <- file.path(app_dir, "app_swat_cequal.R")
arquivo_dependencias <- file.path(app_dir, "loadPackages.R")

arquivos_obrigatorios <- c(arquivo_app, arquivo_dependencias)
arquivos_ausentes <- arquivos_obrigatorios[!file.exists(arquivos_obrigatorios)]
if (length(arquivos_ausentes) > 0L) {
  stop(
    "O aplicativo esta incompleto. Arquivo(s) nao encontrado(s):\n",
    paste(arquivos_ausentes, collapse = "\n"),
    call. = FALSE
  )
}

# app_swat_cequal.R usa caminhos relativos. Definir a pasta de trabalho aqui
# torna a inicializacao independente do local de onde o usuario executou R.
diretorio_anterior <- getwd()
setwd(app_dir)
on.exit(setwd(diretorio_anterior), add = TRUE)

message("Verificando as dependencias do aplicativo...")
tryCatch(
  sys.source(
    arquivo_dependencias,
    envir = environment(),
    chdir = TRUE,
    keep.source = FALSE
  ),
  error = function(erro) {
    stop(
      "Nao foi possivel preparar o ambiente do aplicativo.\n",
      conditionMessage(erro),
      call. = FALSE
    )
  }
)

options(shiny.maxRequestSize = 100 * 1024^2)

# Usa uma porta livre automaticamente, evitando falha quando a porta 4891
# ja estiver ocupada. Uma porta especifica ainda pode ser definida com:
# options(shiny.port = 4891)
porta <- getOption("shiny.port")
if (is.null(porta)) {
  porta <- httpuv::randomPort()
}

message("Iniciando o aplicativo em http://127.0.0.1:", porta)
shiny::runApp(
  appDir = shiny::shinyAppFile(arquivo_app),
  host = "127.0.0.1",
  port = porta,
  launch.browser = interactive()
)

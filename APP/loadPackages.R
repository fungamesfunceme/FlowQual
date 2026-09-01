# Dependencias do aplicativo SWAT-CE-QUAL-W2.
#
# Este arquivo pode ser executado diretamente:
#   source("loadPackages.R")
#
# Os pacotes sao instalados em R-library, dentro da pasta do aplicativo.
# Assim, a instalacao nao depende de permissao para alterar a biblioteca
# global do R do usuario.

localizar_pasta_script <- function(nome_arquivo = "loadPackages.R") {
  argumentos <- commandArgs(trailingOnly = FALSE)
  argumento_arquivo <- grep("^--file=", argumentos, value = TRUE)

  if (length(argumento_arquivo) > 0L) {
    caminho <- sub("^--file=", "", argumento_arquivo[[1L]])
    if (basename(caminho) == nome_arquivo) {
      return(dirname(normalizePath(caminho, winslash = "/", mustWork = TRUE)))
    }
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

if (!exists("app_dir", inherits = FALSE)) {
  app_dir <- localizar_pasta_script()
}
app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)

project_library <- file.path(app_dir, "R-library")
if (!dir.exists(project_library) &&
    !dir.create(project_library, recursive = TRUE, showWarnings = FALSE)) {
  stop(
    "Nao foi possivel criar a biblioteca local em:\n", project_library,
    "\nVerifique as permissoes de escrita da pasta do aplicativo.",
    call. = FALSE
  )
}
.libPaths(unique(c(project_library, .libPaths())))

# Lista unica de dependencias utilizadas pelo aplicativo e pelos scripts
# cientificos carregados por ele.
required_packages <- c(
  "base64enc", "data.table", "dplyr", "DT", "fs", "GA", "gdata",
  "ggplot2", "httr", "hydroTSM", "jsonlite", "later", "lhs", "lubridate", "openxlsx", "optimx",
  "plotly", "png", "readr", "readxl", "shiny", "shinyFiles",
  "shinyjs", "stringr", "tibble", "tidyr", "zip"
)

repositorio_cran <- getOption("repos")[["CRAN"]]
if (is.null(repositorio_cran) ||
    is.na(repositorio_cran) ||
    identical(repositorio_cran, "@CRAN@")) {
  repositorio_cran <- "https://cloud.r-project.org"
}
options(repos = c(CRAN = repositorio_cran))

pacote_disponivel <- function(pacote) {
  requireNamespace(pacote, quietly = TRUE)
}

missing_packages <- required_packages[
  !vapply(required_packages, pacote_disponivel, logical(1))
]

if (length(missing_packages) > 0L) {
  message(
    "Instalando ", length(missing_packages), " pacote(s) ausente(s): ",
    paste(missing_packages, collapse = ", ")
  )

  tryCatch(
    install.packages(
      missing_packages,
      lib = project_library,
      dependencies = c("Depends", "Imports", "LinkingTo")
    ),
    error = function(erro) {
      stop(
        "Falha durante a instalacao dos pacotes.\n",
        "Verifique a conexao com a internet, o antivirus e as permissoes ",
        "da pasta R-library.\nDetalhes: ", conditionMessage(erro),
        call. = FALSE
      )
    }
  )
}

# mopsocd e distribuido junto com o aplicativo porque nao esta no CRAN.
mopsocd_source <- file.path(app_dir, "packages", "mopsocd_0.5.1.tar.gz")
if (!pacote_disponivel("mopsocd")) {
  if (!file.exists(mopsocd_source)) {
    stop(
      "O instalador local do pacote mopsocd nao foi encontrado:\n",
      mopsocd_source,
      call. = FALSE
    )
  }

  tryCatch(
    install.packages(
      mopsocd_source,
      lib = project_library,
      repos = NULL,
      type = "source"
    ),
    error = function(erro) {
      stop(
        "Nao foi possivel instalar o pacote local mopsocd.\n",
        "Detalhes: ", conditionMessage(erro),
        call. = FALSE
      )
    }
  )
}

all_packages <- c(required_packages, "mopsocd")
unavailable_packages <- all_packages[
  !vapply(all_packages, pacote_disponivel, logical(1))
]
if (length(unavailable_packages) > 0L) {
  stop(
    "A instalacao terminou, mas estes pacotes continuam indisponiveis: ",
    paste(unavailable_packages, collapse = ", "),
    ".\nConsulte as mensagens de instalacao exibidas acima.",
    call. = FALSE
  )
}

# O codigo atual usa funcoes sem o prefixo nome_do_pacote::funcao.
# Por isso, estes pacotes precisam ser anexados antes de iniciar o Shiny.
packages_to_attach <- c(
  "shiny", "shinyFiles", "shinyjs", "dplyr", "lubridate", "plotly",
  "DT", "readr", "readxl", "gdata", "ggplot2", "stringr", "tibble",
  "tidyr"
)

invisible(lapply(packages_to_attach, function(package) {
  suppressPackageStartupMessages(
    library(package, character.only = TRUE, lib.loc = .libPaths())
  )
}))

message("Dependencias verificadas com sucesso.")

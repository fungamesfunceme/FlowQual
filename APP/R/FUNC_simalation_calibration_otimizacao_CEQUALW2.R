
library(gdata)
library(readxl)
library(ggplot2)


preparar_arquivos_cequal <- function(
    diretorio_cequal = NULL,
    dados_simulacao = NULL,
    parametros,
    contexto = NULL
){
  

  
  # --- trata parametros ---
  if (is.data.frame(parametros)) {
    # pega os valores em ordem de coluna
    parametros <- unlist(parametros[1, ], use.names = FALSE)
  }
  
  parametros <- suppressWarnings(as.numeric(parametros))

  if (any(is.na(parametros))) {
    warning("Alguns parâmetros não puderam ser convertidos para numérico: ",
            paste(parametros, collapse = ", "))
  }
  
  

 
  entrada <- preparar_entrada_simulacao(
    contexto = contexto,
    diretorio_cequal = diretorio_cequal,
    dados_simulacao = dados_simulacao
  )
  diretorio_cequal <- entrada$diretorio
  dir <- diretorio_cequal
  dados_simulacao <- entrada$dados
  carregar_campos_simulacao(
    dados_simulacao,
    c(
      "julian_day_ini", "julian_day_fim", "ano_simul", "nsegmentos",
      "seg", "preproc", "ctmax", "ctver", "ctmin", "res", "cota_obs",
      "temp_obs", "evap_obs", "fosfato_obs", "nitrito_obs", "chla_obs",
      "do_obs", "ELWS", "TEMP", "tipo", "PO4", "DO", "ALG1"
    ),
    environment()
  )
 

  
  pad_to_8 <- function(x) {
    sprintf("%-8s", x)  # Formata com 8 caracteres, alinhado à direita
  }
  
  pad_to_8d <- function(x) {
    sprintf("%8s", x)  # Formata com 8 caracteres, alinhado à direita
  }
  

  
 parametros=as.numeric(sprintf("%5.2f", parametros))

###FAER VERIFICACAO DE ACORDO COM O TIPO
  
  arquivo_controle <- dados_simulacao$arquivo_controle
  if (is.null(arquivo_controle) || !nzchar(arquivo_controle)) {
    arquivo_controle <- file.path(dir, "w2_conR.npt")
  }
  controle <- ler_linhas_seguro(arquivo_controle, warn = FALSE, descricao = "arquivo de controle")
  dir.create(paste0(dir, "/Resultados_tipo", tipo), showWarnings = FALSE, recursive = TRUE)
  

  
  # === Atualizar ELWS (vem do arquivo de batimetria) ===
  
  arquivo_bth <- localiza_arquivo(controle, "BTH FILE")
  bth_linhas <- ler_linhas_seguro(
    file.path(dir, arquivo_bth),
    warn = FALSE,
    descricao = "arquivo de batimetria"
  )
  bth_linhas2 <- substitui_bloco(bth_linhas, "ELWS", ELWS)
  novo_arquivo_bth<-paste0("bath",ano_simul,"-",julian_day_ini,".prn")
  gravar_linhas_seguro(
    bth_linhas2,
    file.path(dir, novo_arquivo_bth),
    sep = "\n",
    descricao = "arquivo de batimetria preparado"
  )
  
  idx_bath<-grep("BTH FILE", controle)
  if (length(idx_bath) > 0 && (idx_bath + 1) <= length(controle)) {
    linha_bath <- unlist(strsplit(trimws(controle[idx_bath + 1]), "\\s+"))
    linha_bath[3] <- novo_arquivo_bth  
    controle[idx_bath + 1] <- paste(sprintf("%8s", c("WB 1  ",linha_bath[3])), collapse = "")
    
  }
  gravar_linhas_seguro(controle, file.path(diretorio_cequal, "w2_con.npt"), sep = "\n")
  
  
  
  
  
  # === Atualizar TIME CON ===
  idx_time <- grep("^TIME CON", controle)
  if (length(idx_time) > 0 && (idx_time + 1) <= length(controle)) {
    linha_time <- paste(
      "        ",
      sprintf("%8.2f", as.numeric(julian_day_ini)),
      sprintf("%8.2f", as.numeric(julian_day_fim)),
      sprintf("%8.0f", as.numeric(ano_simul)),
      sep = ""
    )
    controle[idx_time + 1] <- linha_time
  }
  

  # == Atualizar TEMP
  
  idx_temp <- grep("^INIT CND", controle)
  if (length(idx_temp) > 0 && (idx_temp + 1) <= length(controle)) {
    linha_temp <- unlist(strsplit(trimws(controle[idx_temp + 1]), "\\s+"))
    linha_temp[3] <- sprintf("%.3f", as.numeric(TEMP))  
    controle[idx_temp + 1] <- paste(sprintf("%8s", c("WB 1  ",linha_temp[3:6])), collapse = "")
  
  }
  
  #ATUALIZAR PO4, DO, ALG1
  ini <- grep("^CST ICON", controle)
  fim <- grep("^CST PRIN", controle)
  trecho <- controle[(ini + 1):(fim - 1)]
  linha_po4 <- grep("^PO4", trecho)
  
  trecho[linha_po4] <- sub(
    "PO4\\s+[-0-9\\.]+",
    paste0("PO4      ", sprintf("%.3f", as.numeric(PO4))),
    trecho[linha_po4]
  )
  
  linha_do <- grep("^DO", trecho)
  trecho[linha_do] <- sub(
    "DO\\s+[-0-9\\.]+",
    paste0("DO      ", sprintf("%.3f", as.numeric(DO))),
    trecho[linha_do]
  )
  
  linha_alg1 <- grep("^ALG1", trecho)
  trecho[linha_alg1] <- sub(
    "ALG1\\s+[-0-9\\.]+",
    paste0("ALG1      ", sprintf("%.3f", as.numeric(ALG1))),
    trecho[linha_alg1]
  )
  
  controle[(ini+1):(fim-1)]<-trecho
  
  
  

  
  
  
  
#  writeLines(controle, file.path(diretorio_cequal,"w2_con.npt"), sep = "\n")
  idx_grid <- grep("^TRIB SEG", controle)
  # === Atualizar ITSR (usa seg) ===
  if (length(idx_grid) > 0 && (idx_grid + 1) <= length(controle)) {
    linha_grid <- unlist(strsplit(trimws(controle[idx_grid + 1]), "\\s+"))
    linha_grid[2] <- sprintf("%d", as.numeric(seg))  ###modifciar aqui caso tenha mais de um segmento para ser analisado
    controle[idx_grid + 1] <- paste(sprintf("%8s", linha_grid), collapse = "")
  }



  
  
  if (tipo == 1 || tipo == 2){
    # === Atualizar CCC (ON ou OFF) ===
    idx_ccc <- grep("^CST COMP", controle)
        if (length(idx_ccc) > 0 && (idx_ccc + 1) <= length(controle)) {
      linha_ccc <- unlist(strsplit(trimws(controle[idx_ccc + 1]), "\\s+"))
      linha_ccc[1] <- "OFF"
      linha_ccc<-c(" ",linha_ccc)
      controle[idx_ccc + 1] <- paste(sprintf("%8s", linha_ccc), collapse = "")
    }
   
    
    linha_ABC <- grep("HEAT EXCH", controle)
    
     #p1="WB 1          ET     OFF     OFF      ON     OFF"
      p1="WB 1        TERM     OFF     OFF      ON     OFF"  

    p2=sprintf("%07.2f", parametros[1:3])
    p2=paste(p2[1],p2[2],p2[3])
    p3=sprintf("%07.2f", 10)
    l=paste(p1,p2,p3, sep=" ")
    l

    controle[linha_ABC+1]=l
    
    
    

    
    
    if (tipo == 2){
      
      
      linha_HYDCOEF <- grep("HYD COEF", controle)
      
      elements <- unlist(strsplit(controle[linha_HYDCOEF+1], "\\s+"))
      
      
      p1=sprintf("%-9s", "WB 1")
      
      #p2=sapply(parametros[6:8],pad_to_8d)
      
      p2=sprintf("%07.2f", parametros[6:8])
      p2=paste(p2, collapse=" ")
      
      p3=sapply(elements, pad_to_8d)
      p3=paste(p3[6:10], collapse = "")
      
      controle[linha_HYDCOEF+1] <- paste(c(p1, p2, p3), collapse = "")
      
      
      
      
      linha_EXCOEF <- grep("EX COEF", controle)
      
      elements <- unlist(strsplit(controle[linha_EXCOEF+1], "\\s+"))
      
      
      p1=sprintf("%-9s", "WB 1")
      
      p2=sapply(elements, pad_to_8)
      p2[3]=sprintf("%-8s", parametros[9])
      p2[6]=sprintf("%-8s", parametros[10])
      
      p2=paste(p2[3:8], collapse = "")
      
      controle[linha_EXCOEF+1] <- paste(c(p1, p2), collapse = "")
      
      
      
    }
    
    
    gravar_linhas_seguro(controle, file.path(diretorio_cequal, "w2_con.npt"), sep = "\n")
    
   
    
    ## Arquivo WSC
    wsc <- ler_linhas_seguro(file.path(dir, "wscR.prn"), descricao = "arquivo WSC")
    linha=strsplit(wsc[4], " ")
    linha_u=as.numeric(unique(unlist(linha)))
    wsc_arquivo=linha_u[length(linha_u)]
    
    for (j in 1:length(wsc)) {
      
      if(parametros[4]>1) { parametros[4]=parametros[4]/10}
      wsc[j]=gsub(wsc_arquivo, sprintf("%02.2f",parametros[4]), wsc[j])
    }
    gravar_linhas_seguro(wsc, file.path(diretorio_cequal, "wsc.prn"), sep = "\n")
  
  
    
    ##Arquivo Shade
    
    valorshade=as.numeric(parametros[5])
 
    for (j in 1:length(valorshade)){
      valor=sprintf("%.2f", rep(valorshade, nsegmentos))
     # d <- matrix(valor, ncol = 9, byrow = FALSE)   ###eh preciso completar com nulo
      n <- length(valor)
      cols <- 9
      resto <- n %% cols
      if (resto != 0) {
        valor <- c(valor, rep(NA, cols - resto))  # completa
      }
      d <- matrix(valor, ncol = cols, byrow = TRUE)
      
      
      }

    
    cab1=paste0("Shade - Sombreamento") 
    cab2=as.character(" ")
    
    Segment = append(c("Segment"),sprintf("%.0f", seq(from=1, length.out = nsegmentos, by=1)))
    DynSh = append(c("DynSh"),sprintf("%.2f", rep(valorshade, nsegmentos)))
    
    other= matrix(data="",nrow=nsegmentos+1,ncol=28)
    
    other[1,]=c("TTEleLB", "TTEleRB","ClDisLB", "ClDiRB" , 'SRFLB1',  'SRFLB2' , 'SRFRB1' , 'SRFRB2',  'TOPO1' ,  'TOPO2' ,  'TOPO3' ,  'TOPO4' ,  'TOPO5' ,  'TOPO6' ,  'TOPO7' ,  'TOPO8',   'TOPO9'   ,'TOPO10',  'TOPO11',  'TOPO12',  'TOPO13',  'TOPO14',  'TOPO15',  'TOPO16',  'TOPO17',  'TOPO18',  'SRFJD1' , 'SRFJD2')
    
    dado_entrada=cbind(Segment,DynSh,other)
    
    arquivo_shade <- file.path(diretorio_cequal, "shade.prn")
    gravar_com_seguranca(
      arquivo_shade,
      function(destino) {
        write(cab1, destino)
        write(cab2, destino, append = TRUE)
        write.fwf(
          as.data.frame(dado_entrada),
          destino,
          sep = "",
          append = TRUE,
          colnames = FALSE,
          rownames = FALSE,
          width = rep(8, ncol(dado_entrada)),
          justify = "right",
          eol = "\n"
        )
      },
      descricao = "arquivo de sombreamento"
      )
   
    
    
  } else if (tipo == 3){
    
    
    
    # === Atualizar CCC (ON ou OFF) ===
    idx_ccc <- grep("^CST COMP", controle)
    if (length(idx_ccc) > 0 && (idx_ccc + 1) <= length(controle)) {
      linha_ccc <- unlist(strsplit(trimws(controle[idx_ccc + 1]), "\\s+"))
      linha_ccc[1] <- "ON"
      linha_ccc<-c(" ",linha_ccc)
      controle[idx_ccc + 1] <- paste(sprintf("%8s", linha_ccc), collapse = "")
    }

    
    ## Modifica parâmetros de algas
    #controle <- readLines(controle_file)
    linha_ALGALRATE <- grep("ALGAL RATE", controle)
    p1 <- "ALG1"
    AG <- sprintf("%04.5f", as.numeric(parametros[1]/10)) #Algal growth, Kag (1/day)
    AR <- sprintf("%04.5f", as.numeric(parametros[2]/10)) #Algal respiration, Kar (1/day)
    AE <- sprintf("%04.5f", as.numeric(parametros[3]/10)) #Algal excretion, Kae (1/day)
    AM <- sprintf("%04.5f", as.numeric(parametros[4]/10)) #Algal mortality, Kam (1/day)
    AS <- sprintf("%04.5f", as.numeric(parametros[5]/10)) #Algal settling, ωa (m/day)
    AHSP <- sprintf("%04.5f", as.numeric(parametros[6]/100)) #Algal half-saturation for phosphorus limited growth (mg/L)
    ASAT <- sprintf("%04.5f", as.numeric(parametros[7])) #Light saturation intensity at maximum photosynthetic rate (W/m2)
    SOD <- sprintf("%04.5f", as.numeric(parametros[8]/10)) # Sediment oxygen demand, SOD [g/(m2 day)]
    # AHSN <- sprintf("%04.5f", as.numeric(parametros[7]))
    # AHSSI <- sprintf("%04.5f", as.numeric(parametros[8]))
    
    # Pegue a linha imediatamente após "ALGAL RATE"
    linha_valores <- controle[linha_ALGALRATE + 1]
    
    # Separe em substrings, remova espaços extras e converta para numérico

    valores <- suppressWarnings(as.numeric(
      strsplit(trimws(linha_valores), "\\s+")[[1]]
    ))

    AHSN=sprintf("%04.5f", as.numeric(valores[8]))
    AHSSI=sprintf("%04.5f", as.numeric(valores[9]))
    
    l <- c(p1, AG, AR, AE, AM, AS, AHSP, AHSN, AHSSI, ASAT)
    l <- c(
      sprintf("%8s", l[1]),
      sprintf("%7s", l[-1])
    )
    
    
  
    controle[linha_ALGALRATE + 1] <- paste0(l, collapse = " ")
    
    linha_SDEMAND <- grep("S DEMAND", controle)
    linha_REAERATION <- grep("REAERATION", controle)
    SOD_aux <- unique(unlist(strsplit(controle[linha_SDEMAND+1], " ")))[2]
    
    linhas_SOD<-(controle[(linha_SDEMAND+1):(linha_REAERATION-2)])
    
    
    
    # Substituir valores em wsc com os parâmetros
    for (j in 1:length(linhas_SOD)) {
      linhas_SOD[j] <- gsub(SOD_aux, format(SOD, width = 7, justify = "left"), linhas_SOD[j])
    }
    
    
    controle[(linha_SDEMAND+1):(linha_REAERATION-2)]<-linhas_SOD
    
    
    
    gravar_linhas_seguro(controle, file.path(diretorio_cequal, "w2_con.npt"), sep = "\n")
  }
  
 #preproc=1
 #
  ## REESCREVE O NOVO ARQUIVO DE CONTROLE
  return(controle)
 
}

  
# -----------------------------------------------------------------------------
# Perfis verticais de temperatura (observado x PRF)
# -----------------------------------------------------------------------------

extrair_numeros_prf <- function(texto) {
  encontrados <- stringr::str_extract_all(
    texto,
    "[-+]?(?:[0-9]*\\.?[0-9]+)(?:[Ee][-+]?[0-9]+)?"
  )[[1]]
  suppressWarnings(as.numeric(encontrados))
}


normalizar_utf8_prf <- function(texto) {
  convertido <- suppressWarnings(iconv(texto, from = "", to = "UTF-8", sub = ""))
  invalidos <- is.na(convertido)
  if (any(invalidos)) {
    convertido[invalidos] <- suppressWarnings(iconv(
      texto[invalidos],
      from = "windows-1252",
      to = "UTF-8",
      sub = ""
    ))
  }
  convertido[is.na(convertido)] <- ""
  convertido
}


ler_prf_temperatura <- function(arquivo_prf) {
  if (!file.exists(arquivo_prf)) {
    stop("Arquivo prf.opt não encontrado: ", arquivo_prf, call. = FALSE)
  }

  linhas <- ler_linhas_seguro(
    arquivo_prf,
    warn = FALSE,
    descricao = "arquivo de perfis PRF"
  )
  # Arquivos do CE-QUAL-W2 podem trazer símbolos como ° em Windows-1252.
  # A análise usa somente marcadores ASCII, mas stringr exige texto UTF-8 válido.
  linhas <- normalizar_utf8_prf(linhas)

  linha_execucao <- grep("^Model run at", trimws(linhas), ignore.case = TRUE)[1]
  if (is.na(linha_execucao) || linha_execucao >= length(linhas)) {
    stop("Cabeçalho do prf.opt não reconhecido.", call. = FALSE)
  }

  linha_config <- linha_execucao + 1L
  config <- extrair_numeros_prf(linhas[linha_config])
  if (length(config) < 2L) {
    stop("Configuração numérica do prf.opt não reconhecida.", call. = FALSE)
  }

  kmx <- as.integer(config[1])
  n_segmentos_prf <- as.integer(config[2])
  segmentos <- as.integer(extrair_numeros_prf(linhas[linha_config + 1L]))
  segmentos <- segmentos[seq_len(min(length(segmentos), n_segmentos_prf))]

  if (!length(segmentos)) {
    stop("Nenhum segmento foi identificado no prf.opt.", call. = FALSE)
  }

  primeira_temp <- grep("^\\s*TEMP\\s+[-+]?\\d+\\s*$", linhas)[1]
  if (is.na(primeira_temp)) {
    stop("Nenhum bloco TEMP foi encontrado no prf.opt.", call. = FALSE)
  }

  numeros_cabecalho <- unlist(lapply(
    linhas[seq.int(linha_config + 2L, primeira_temp - 1L)],
    extrair_numeros_prf
  ))
  if (length(numeros_cabecalho) < kmx) {
    stop("Alturas das camadas não foram identificadas no prf.opt.", call. = FALSE)
  }
  alturas_camadas <- tail(numeros_cabecalho, kmx)

  meses <- c(
    Jan = 1L, Feb = 2L, Mar = 3L, Apr = 4L, May = 5L, Jun = 6L,
    Jul = 7L, Aug = 8L, Sep = 9L, Oct = 10L, Nov = 11L, Dec = 12L
  )
  padrao_data <- paste0(
    "^\\s*([0-9]+(?:\\.[0-9]+)?)\\s+([A-Za-z]{3})\\s+",
    "([0-9]{1,2}),\\s+([0-9]{4})\\s+([0-9]+)\\s+",
    "([-+]?[0-9]*\\.?[0-9]+)\\s+([0-9]+)\\s*$"
  )

  resultado <- list()
  data_atual <- NULL
  jday_atual <- NA_real_
  camada_superficie <- NA_integer_
  desvio_superficie <- NA_real_
  indice_temp <- 0L
  i <- primeira_temp

  while (i <= length(linhas)) {
    casamento_data <- stringr::str_match(linhas[i], padrao_data)
    if (!is.na(casamento_data[1, 1])) {
      mes <- meses[[casamento_data[1, 3]]]
      if (!is.null(mes)) {
        data_atual <- as.Date(sprintf(
          "%04d-%02d-%02d",
          as.integer(casamento_data[1, 5]), mes,
          as.integer(casamento_data[1, 4])
        ))
      }
      jday_atual <- as.numeric(casamento_data[1, 2])
      camada_superficie <- as.integer(casamento_data[1, 6])
      desvio_superficie <- as.numeric(casamento_data[1, 7])
      indice_temp <- 0L
      i <- i + 1L
      next
    }

    casamento_temp <- stringr::str_match(
      linhas[i],
      "^\\s*TEMP\\s+([-+]?\\d+)\\s*$"
    )
    if (!is.na(casamento_temp[1, 1])) {
      numero_camadas <- as.integer(casamento_temp[1, 2])

      # Os blocos anteriores à primeira data são condições iniciais. Contagens
      # negativas representam perfil indisponível e não contêm valores úteis.
      if (!is.null(data_atual)) {
        indice_temp <- indice_temp + 1L
      }
      segmento_atual <- segmentos[((max(indice_temp, 1L) - 1L) %% length(segmentos)) + 1L]

      if (!is.null(data_atual) && numero_camadas > 0L) {
        valores <- numeric()
        j <- i + 1L
        while (j <= length(linhas) && length(valores) < numero_camadas) {
          if (grepl(padrao_data, linhas[j]) ||
              grepl("^\\s*[A-Za-z][A-Za-z0-9_]*\\s+[-+]?\\d+\\s*$", linhas[j])) {
            break
          }
          valores <- c(valores, extrair_numeros_prf(linhas[j]))
          j <- j + 1L
        }

        if (length(valores) >= numero_camadas &&
            is.finite(camada_superficie) && camada_superficie >= 1L) {
          valores <- valores[seq_len(numero_camadas)]
          fim <- min(kmx, camada_superficie + numero_camadas - 1L)
          dz <- alturas_camadas[camada_superficie:fim]
          n_util <- min(length(dz), length(valores))
          dz <- dz[seq_len(n_util)]
          valores <- valores[seq_len(n_util)]

          espessura_superficial <- dz[1] - desvio_superficie
          profundidades <- numeric(n_util)
          profundidades[1] <- espessura_superficial / 2
          if (n_util > 1L) {
            for (k in 2:n_util) {
              espessuras_anteriores <- if (k > 2L) sum(dz[2:(k - 1L)]) else 0
              profundidades[k] <- espessura_superficial +
                espessuras_anteriores + dz[k] / 2
            }
          }

          resultado[[length(resultado) + 1L]] <- tibble::tibble(
            Date = data_atual,
            JDAY = jday_atual,
            Segmento = segmento_atual,
            depth = profundidades,
            value = valores
          )
        }
        i <- max(i + 1L, j)
        next
      }
    }
    i <- i + 1L
  }

  if (!length(resultado)) {
    stop(
      "O prf.opt não contém blocos TEMP válidos com valores positivos de camadas.",
      call. = FALSE
    )
  }

  dplyr::bind_rows(resultado) |>
    dplyr::filter(is.finite(depth), is.finite(value)) |>
    dplyr::arrange(Date, Segmento, depth)
}


normalizar_cabecalho_perfil <- function(x) {
  x <- normalizar_utf8_prf(as.character(x))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  gsub("[^a-z0-9]+", "_", x)
}


converter_data_hora_perfil <- function(data, hora = NULL, tz = "America/Fortaleza") {
  if (inherits(data, "POSIXt")) return(as.POSIXct(data, tz = tz))

  data_num <- suppressWarnings(as.numeric(data))
  hora_num <- if (is.null(hora)) 0 else suppressWarnings(as.numeric(hora))
  serial <- data_num + ifelse(is.finite(hora_num), hora_num, 0)
  saida <- as.POSIXct(rep(NA_real_, length(data)), origin = "1970-01-01", tz = tz)
  idx_serial <- is.finite(serial)
  saida[idx_serial] <- as.POSIXct(
    (serial[idx_serial] - 25569) * 86400,
    origin = "1970-01-01",
    tz = tz
  )

  idx_texto <- !idx_serial
  if (any(idx_texto)) {
    texto <- trimws(paste(
      as.character(data[idx_texto]),
      if (is.null(hora)) "" else as.character(hora[idx_texto])
    ))
    saida[idx_texto] <- lubridate::parse_date_time(
      texto,
      c("d/m/Y H:M:S", "d/m/Y H:M", "Y-m-d H:M:S", "m/d/Y H:M:S"),
      tz = tz,
      quiet = TRUE
    )
  }
  saida
}


ler_perfis_observados <- function(dir_meas, sim_start, sim_end,
                                  tz = "America/Fortaleza") {
  if (file.exists(dir_meas) && !dir.exists(dir_meas)) {
    arquivos <- dir_meas
  } else if (dir.exists(dir_meas)) {
    arquivos <- list.files(
      dir_meas,
      pattern = "\\.xlsx?$",
      full.names = TRUE,
      ignore.case = TRUE
    )
  } else {
    stop("Fonte de perfis observados não encontrada: ", dir_meas, call. = FALSE)
  }
  if (!length(arquivos)) {
    stop("Nenhum arquivo .xls ou .xlsx foi encontrado em ", dir_meas, call. = FALSE)
  }

  ler_arquivo <- function(arquivo) {
    abas <- readxl::excel_sheets(arquivo)
    aba_banco <- abas[tolower(abas) == "banco_perfis"]
    aba_perfil <- abas[tolower(abas) == "perfil"]
    abas_ler <- if (length(aba_banco)) {
      aba_banco[1]
    } else if (length(aba_perfil)) {
      aba_perfil[1]
    } else {
      abas
    }

    purrr::map_dfr(abas_ler, function(aba) {
      if (tolower(aba) == "banco_perfis") {
        df <- readxl::read_excel(arquivo, sheet = aba)
      } else {
        amostra <- readxl::read_excel(arquivo, sheet = aba, col_names = FALSE, n_max = 15)
        linhas <- apply(amostra, 1, function(x) {
          nomes_linha <- normalizar_cabecalho_perfil(x)
          sum(grepl("data|date|datetime|prof|depth|temp", nomes_linha))
        })
        linha_cabecalho <- which.max(linhas)
        df <- readxl::read_excel(
          arquivo,
          sheet = aba,
          skip = linha_cabecalho - 1L,
          col_names = TRUE
        )
      }

      nomes <- normalizar_cabecalho_perfil(names(df))
      localizar <- function(padrao) {
        achado <- grep(padrao, nomes)[1]
        if (is.na(achado)) NULL else achado
      }

      col_datetime <- localizar("^data_hora$|^data_horario|^datetime$")
      col_data <- localizar("^date|^data$")
      col_hora <- localizar("^time|^hora$")
      col_prof <- localizar("profundidade|^prof_|^prof$|depth")
      col_temp <- localizar("temperatura.*agua|^temp_|^temp$|temp_c")

      if (is.null(col_prof) || is.null(col_temp) ||
          (is.null(col_datetime) && is.null(col_data))) {
        return(tibble::tibble())
      }

      if (!is.null(col_datetime)) {
        dt <- converter_data_hora_perfil(df[[col_datetime]], tz = tz)
      } else {
        dt <- converter_data_hora_perfil(
          df[[col_data]],
          if (is.null(col_hora)) NULL else df[[col_hora]],
          tz = tz
        )
      }

      tibble::tibble(
        datetime_meas = dt,
        date = as.Date(dt, tz = tz),
        prof_m = suppressWarnings(as.numeric(df[[col_prof]])),
        temp_C = suppressWarnings(as.numeric(df[[col_temp]])),
        arquivo_origem = basename(arquivo),
        aba_origem = aba
      )
    })
  }

  purrr::map_dfr(arquivos, ler_arquivo) |>
    dplyr::filter(
      !is.na(date), is.finite(prof_m), is.finite(temp_C),
      date >= as.Date(sim_start), date <= as.Date(sim_end)
    ) |>
    dplyr::distinct(date, prof_m, temp_C, .keep_all = TRUE)
}


parear_perfis_temperatura <- function(df_meas, df_prf, segmento) {
  df_prf <- df_prf |> dplyr::filter(Segmento == as.integer(segmento))
  if (!nrow(df_prf)) {
    stop("O segmento ", segmento, " não possui perfis de temperatura no spr.opt.", call. = FALSE)
  }

  sim_por_dia <- split(df_prf, df_prf$Date)
  sim_por_dia <- lapply(sim_por_dia, function(x) x[order(x$depth), , drop = FALSE])

  purrr::pmap_dfr(
    list(df_meas$date, df_meas$datetime_meas, df_meas$prof_m, df_meas$temp_C),
    function(data_medida, data_hora, profundidade, temperatura) {
      perfil <- sim_por_dia[[as.character(data_medida)]]
      if (is.null(perfil) || nrow(perfil) < 2L) return(tibble::tibble())

      tibble::tibble(
        Date = as.Date(data_medida),
        Hora_Medida = data_hora,
        Profundidade_Medida = profundidade,
        Temp_Medida = temperatura,
        Temp_Simulada = stats::approx(
          x = perfil$depth,
          y = perfil$value,
          xout = profundidade,
          rule = 2,
          ties = mean
        )$y
      )
    }
  )
}


ler_spr_temperatura <- function(arquivo_spr, ano_simul, segmento) {
  if (!file.exists(arquivo_spr)) {
    stop("Arquivo spr.opt não encontrado: ", arquivo_spr, call. = FALSE)
  }

  cabecalho <- readLines(
    arquivo_spr,
    n = 1L,
    warn = FALSE,
    encoding = "windows-1252"
  )
  cabecalho <- normalizar_utf8_prf(cabecalho)
  if (!length(cabecalho) || !nzchar(trimws(cabecalho))) {
    stop(
      "O arquivo spr.opt existe, mas não contém resultados. Verifique a configuração da saída SPR no w2_con.npt.",
      call. = FALSE
    )
  }

  segmentos <- stringr::str_match_all(
    cabecalho[1],
    stringr::regex("SEG[_ ]*([0-9]+)", ignore_case = TRUE)
  )[[1]]
  segmentos <- if (nrow(segmentos)) as.integer(segmentos[, 2]) else integer()
  posicao_segmento <- match(as.integer(segmento), segmentos)
  if (is.na(posicao_segmento)) {
    stop(
      "O segmento ", segmento, " não foi encontrado no cabeçalho do spr.opt. ",
      "Segmentos disponíveis: ",
      if (length(segmentos)) paste(segmentos, collapse = ", ") else "nenhum",
      ".",
      call. = FALSE
    )
  }

  # O cabeçalho SPR possui um rótulo Elevation adicional; por isso os dados são
  # lidos sem usar o cabeçalho e as posições são calculadas pela ordem dos SEG.
  dados_spr <- suppressWarnings(readr::read_table(
    arquivo_spr,
    skip = 1L,
    col_names = FALSE,
    show_col_types = FALSE,
    progress = FALSE,
    na = c("", "NA")
  ))
  if (!nrow(dados_spr)) {
    stop(
      "O arquivo spr.opt existe, mas não contém resultados. Verifique a configuração da saída SPR no w2_con.npt.",
      call. = FALSE
    )
  }

  coluna_elevacao <- 4L + 2L * (posicao_segmento - 1L)
  coluna_valor <- coluna_elevacao + 1L
  if (ncol(dados_spr) < coluna_valor) {
    stop("Estrutura de colunas inválida no spr.opt.", call. = FALSE)
  }

  resultado <- tibble::tibble(
    Constituinte = as.character(dados_spr[[1]]),
    JDAY = suppressWarnings(as.numeric(dados_spr[[2]])),
    depth = suppressWarnings(as.numeric(dados_spr[[3]])),
    Elevation = suppressWarnings(as.numeric(dados_spr[[coluna_elevacao]])),
    value = suppressWarnings(as.numeric(dados_spr[[coluna_valor]]))
  ) |>
    dplyr::filter(
      tolower(Constituinte) == "temperature",
      is.finite(JDAY), is.finite(depth), is.finite(value),
      value > -90
    ) |>
    dplyr::mutate(
      Date = as.Date(floor(JDAY) - 1, origin = paste0(as.integer(ano_simul), "-01-01")),
      Segmento = as.integer(segmento)
    ) |>
    dplyr::select(Date, JDAY, Segmento, depth, value, Elevation) |>
    dplyr::arrange(Date, depth)

  if (!nrow(resultado)) {
    stop(
      "O spr.opt não contém linhas válidas de Temperature para o segmento ",
      segmento,
      ". Verifique a configuração da saída SPR no w2_con.npt.",
      call. = FALSE
    )
  }
  resultado
}


identificar_arquivo_perfis <- function(dir, reserv_sigla) {
  pasta_modelo <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  reserv_sigla <- tolower(trimws(as.character(reserv_sigla)[1]))
  if (!nzchar(reserv_sigla) || is.na(reserv_sigla) ||
      !grepl("^[a-z0-9]+$", reserv_sigla)) {
    stop(
      "Sigla interna do reservatório ausente ou inválida para localizar os perfis. ",
      "Pasta do modelo: '", pasta_modelo, "'.",
      call. = FALSE
    )
  }
  nome_arquivo_perfis <- paste0("Perfis_", reserv_sigla, ".xlsx")

  list(
    pasta_modelo = pasta_modelo,
    nome_arquivo = nome_arquivo_perfis,
    caminho = file.path(pasta_modelo, nome_arquivo_perfis)
  )
}


validar_arquivo_perfis_observados <- function(dir, reserv_sigla) {
  arquivo_perfis <- identificar_arquivo_perfis(dir, reserv_sigla)
  if (!file.exists(arquivo_perfis$caminho)) {
    stop(
      "Arquivo de perfis observados não encontrado. Arquivo esperado: '",
      arquivo_perfis$nome_arquivo,
      "'. Pasta pesquisada: '",
      arquivo_perfis$pasta_modelo,
      "'.",
      call. = FALSE
    )
  }
  arquivo_perfis
}


localizar_arquivo_spr_controle <- function(dir) {
  arquivo_controle <- file.path(dir, "w2_con.npt")
  linhas_controle <- ler_linhas_seguro(
    arquivo_controle,
    warn = FALSE,
    descricao = "arquivo de controle"
  )
  nome_spr <- localiza_arquivo(linhas_controle, "SPR FILE")
  if (length(nome_spr) != 1L || is.na(nome_spr) || !nzchar(nome_spr)) {
    stop(
      "Não foi possível identificar o arquivo SPR na seção 'SPR FILE' de '",
      arquivo_controle,
      "'.",
      call. = FALSE
    )
  }
  file.path(dir, nome_spr)
}


calcular_perfis_temperatura <- function(dir, segmento, reserv_sigla, ano_simul,
                                        tz = "America/Fortaleza") {
  arquivo_spr <- localizar_arquivo_spr_controle(dir)
  df_spr <- ler_spr_temperatura(arquivo_spr, ano_simul, segmento)

  arquivo_perfis <- validar_arquivo_perfis_observados(dir, reserv_sigla)
  pasta_modelo <- arquivo_perfis$pasta_modelo
  nome_arquivo_perfis <- arquivo_perfis$nome_arquivo
  fonte_perfis <- arquivo_perfis$caminho

  medidas <- ler_perfis_observados(
    fonte_perfis,
    min(df_spr$Date), max(df_spr$Date), tz
  )
  pareado <- parear_perfis_temperatura(medidas, df_spr, segmento)
  if (!nrow(pareado)) {
    stop("Não existem datas coincidentes entre o spr.opt e os perfis observados.", call. = FALSE)
  }

  rmse_diario <- pareado |>
    dplyr::mutate(Erro_Quad = (Temp_Medida - Temp_Simulada)^2) |>
    dplyr::group_by(Date) |>
    dplyr::summarise(
      RMSE = sqrt(mean(Erro_Quad, na.rm = TRUE)),
      .groups = "drop"
    )

  fo3 <- sqrt(mean(rmse_diario$RMSE^2, na.rm = TRUE))
  if (!is.finite(fo3)) stop("FO3 do perfil de temperatura é inválido.", call. = FALSE)

  list(FO3 = fo3, df_paired = pareado, rmse_diario = rmse_diario, df_spr = df_spr)
}


plot_perf_temp <- function(df_paired, xlim_temp = NULL, ylim_depth = NULL) {
  if (is.null(xlim_temp)) {
    faixa <- range(c(df_paired$Temp_Medida, df_paired$Temp_Simulada), na.rm = TRUE)
    margem <- max(diff(faixa) * 0.05, 0.5)
    xlim_temp <- faixa + c(-margem, margem)
  }
  if (is.null(ylim_depth)) {
    ylim_depth <- c(max(df_paired$Profundidade_Medida, na.rm = TRUE) * 1.05, 0)
  }

  datas <- sort(unique(as.Date(df_paired$Date)))
  plots <- list()
  for (data_atual in datas) {
    sub <- df_paired |> dplyr::filter(as.Date(Date) == as.Date(data_atual))
    if (nrow(sub) < 2L) next
    rmse_dia <- sqrt(mean((sub$Temp_Medida - sub$Temp_Simulada)^2, na.rm = TRUE))
    df_plot <- dplyr::bind_rows(
      sub |> dplyr::transmute(depth = Profundidade_Medida, temp = Temp_Medida, serie = "Medida"),
      sub |> dplyr::transmute(depth = Profundidade_Medida, temp = Temp_Simulada, serie = "Simulada")
    )
    plots[[as.character(data_atual)]] <- ggplot2::ggplot(
      df_plot,
      ggplot2::aes(x = temp, y = depth, color = serie)
    ) +
      ggplot2::geom_path(linewidth = 0.9) +
      ggplot2::geom_point(
        data = df_plot |> dplyr::filter(serie == "Medida"),
        size = 1.8
      ) +
      ggplot2::scale_x_continuous(limits = xlim_temp) +
      ggplot2::scale_y_reverse(limits = ylim_depth) +
      ggplot2::scale_color_manual(values = c(Medida = "black", Simulada = "red")) +
      ggplot2::labs(
        title = sprintf("%s | RMSE %.2f °C", format(as.Date(data_atual), "%d/%m/%Y"), rmse_dia),
        x = "Temperatura (°C)", y = "Profundidade (m)", color = NULL
      ) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(size = 9))
  }
  plots
}


processar_resultados_cequal <- function(
    simul,
    diretorio_cequal = NULL,
    dados_simulacao = NULL,
    preproc = NULL,
    salvar_figura = 1,
    salvar_resultados = 1,
    simul_aleatoria = 0,
    varsobj = c("Cota", "Tempo"),
    objfun = c("mae"),
    subpasta_resultados = NULL,
    contexto = NULL
) {

  entrada <- preparar_entrada_simulacao(
    contexto = contexto,
    diretorio_cequal = diretorio_cequal,
    dados_simulacao = dados_simulacao
  )
  diretorio_cequal <- entrada$diretorio
  dir <- diretorio_cequal
  dados_simulacao <- entrada$dados
  carregar_campos_simulacao(
    dados_simulacao,
    c(
      "julian_day_ini", "julian_day_fim", "ano_simul", "nsegmentos",
      "seg", "preproc", "ctmax", "ctver", "ctmin", "res", "cota_obs",
      "temp_obs", "evap_obs", "fosfato_obs", "nitrito_obs", "chla_obs",
      "do_obs", "tipo"
    ),
    environment()
  )
  reserv_sigla_perfil <- if (!is.null(contexto)) {
    contexto$configuracao$reservatorio
  } else {
    res
  }
  diretorio_resultados <- file.path(dir, paste0("resultados_tipo", tipo))
  if (!is.null(subpasta_resultados) && nzchar(subpasta_resultados)) {
    diretorio_resultados <- file.path(
      diretorio_resultados,
      subpasta_resultados
    )
  }
  dir.create(diretorio_resultados, recursive = TRUE, showWarnings = FALSE)
  
 




  arquivo=paste0(dir,"/tsr_1_SEG",seg,".OPT")
  arquivo_c=paste0(dir,"/w2_con.npt")
  arquivo_f=paste0(dir,"/FLOWBAL.OPT")
  arquivo_s=paste0(dir,"/shade.prn")
  arquivo_w=paste0(dir,"/wsc.prn")
  linhas <- ler_linhas_seguro(arquivo_c, warn = FALSE, descricao = "arquivo de controle")
  arquivo_bath=localiza_arquivo(linhas, "BTH FILE")
  arquivo_b= file.path(dir, arquivo_bath)
  arquivo_spr=localizar_arquivo_spr_controle(dir)

  
  
  
  txt <- ler_linhas_seguro(arquivo, descricao = "resultado TSR")
  txt <- str_replace_all(
  txt,
    "([0-9]E[-+][0-9]+)-",
    "\\1 -"
  )
  
  dados <- read_table(
    I(txt),
    skip = 11,
    show_col_types = FALSE
  )
 

  dados_flux <- suppressWarnings(
    ler_tabela_segura(
      arquivo_f,
      readr::read_csv,
      show_col_types = FALSE,
      descricao = "balanço de fluxo"
    )
  )
  
  
  if (dim(dados_flux)[1] != 0){

  dados_evap=dados_flux$volev
  dados_evap_dia=NA
  dados_evap_dia <- c(0, dados_evap[-length(dados_evap)]) - dados_evap
  #dados_evap_dia=dados_evap_dia[-length(dados_evap_dia)]
  dados[1,1]=julian_day_ini
  discretizacao<- julian_day_ini %% 1
  day=ifelse(dados[,1] %% 1 == discretizacao, 1, 0)
  dados_aux=cbind(day, dados)
  dados_modelo=dados[dados_aux[,1]==1,]
  dados_modelo2=dados[dados_aux[,1]==0,]
  
  
  
  get_col <- function(df, col, default = NA_real_) {
    if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
  }
  
  jday          <- get_col(dados_modelo, "JDAY")
  cota_model    <- get_col(dados_modelo, "ELWS")
  temp_model    <- get_col(dados_modelo2, "T2")
  temp_model2   <- get_col(dados_modelo,  "T2")
  nitrito_model <- get_col(dados_modelo, "NO3")
  chla_model    <- get_col(dados_modelo, "CHLA")
  do_model      <- get_col(dados_modelo, "DO")
  fosfato_model <- get_col(dados_modelo, "PO4") #/1000
  
  evap_model    <- dados_evap_dia / 10^6
  

  
  # ==========================
  # Funções de Métricas Padronizadas
  # ==========================
  
  # 1) Root Mean Square Error
  rmse <- function(sim, obs) {
    ok <- suppressWarnings(is.finite(sim) & is.finite(obs) & (sim >= 0) & (obs >= 0))
    if (!any(ok)) return(NA_real_)
    sqrt(mean((sim[ok] - obs[ok])^2))
  }
  
  # 2) Mean Absolute Error
  mae <- function(sim, obs) {
    ok <- suppressWarnings(is.finite(sim) & is.finite(obs) & (sim >= 0) & (obs >= 0))
    if (!any(ok)) return(NA_real_)
    mean(abs(sim[ok] - obs[ok]))
  }
  
  # 3) Bias (Erro médio)
  bias <- function(sim, obs) {
    ok <- suppressWarnings(is.finite(sim) & is.finite(obs) & (sim >= 0) & (obs >= 0))
    if (!any(ok)) return(NA_real_)
    mean(sim[ok] - obs[ok])
  }
  
  # 4) Nash–Sutcliffe Efficiency
  nse <- function(obs, sim) {
    ok <- suppressWarnings(is.finite(sim) & is.finite(obs) & (sim >= 0) & (obs >= 0))
    if (!any(ok)) return(NA_real_)
    denominador <- sum((obs[ok] - mean(obs[ok]))^2)
    if (!is.finite(denominador) || denominador == 0) return(NA_real_)
    1 - sum((sim[ok] - obs[ok])^2) / denominador
  }

  # 5) Kling-Gupta Efficiency
  kge <- function(obs, sim) {
    ok <- suppressWarnings(is.finite(sim) & is.finite(obs) & (sim >= 0) & (obs >= 0))
    if (sum(ok) < 2L) return(NA_real_)
    obs_ok <- obs[ok]
    sim_ok <- sim[ok]
    media_obs <- mean(obs_ok)
    sd_obs <- stats::sd(obs_ok)
    if (!is.finite(media_obs) || media_obs == 0 || !is.finite(sd_obs) || sd_obs == 0) {
      return(NA_real_)
    }
    r <- suppressWarnings(stats::cor(sim_ok, obs_ok))
    alpha <- stats::sd(sim_ok) / sd_obs
    beta <- mean(sim_ok) / media_obs
    if (!all(is.finite(c(r, alpha, beta)))) return(NA_real_)
    1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
  }
  
  # 6) Skill (comparação com baseline, p. ex. persistência ou interpolação)
  skill_vs <- function(sim, obs, base_pred) {
    ok <- suppressWarnings(is.finite(sim) & is.finite(obs) & is.finite(base_pred) &
      (sim >= 0) & (obs >= 0) & (base_pred >= 0))
    if (!any(ok)) return(NA_real_)
    mse_model <- mean((sim[ok] - obs[ok])^2)
    mse_base  <- mean((base_pred[ok] - obs[ok])^2)
    if (!is.finite(mse_base) || mse_base == 0) return(NA_real_)
    1 - mse_model / mse_base
  }
  
  make_persist <- function(obs) {
    if (length(obs) == 0 || all(!is.finite(obs))) return(rep(NA_real_, length(obs)))
    v0 <- obs[which(is.finite(obs) & obs >= 0)[1]]  # primeiro valor válido
    rep(v0, length(obs))
  }
  
  # Cria baseline para cada variável
  cota_obs_start      <- make_persist(cota_obs)
  temp_obs_start      <- make_persist(temp_obs)
  fosfato_obs_start   <- make_persist(fosfato_obs)
  do_obs_start        <- make_persist(do_obs)
  chla_obs_start      <- make_persist(chla_obs)
  evap_obs_start      <- make_persist(evap_obs)
  

  ###fim 5)
  
  # ==========================================
  # Cálculo de métricas por variável (ordem definida)
  # ==========================================
 
  library(tibble)
  library(readr)
  
  metricas <- tibble(
    variavel = c("Data Ini","Data fim","Cota", "Temperatura", "Evaporação", "Fosfato", "Oxigênio Dissolvido", "Clorofila-a"),
    
    RMSE = c(
      julian_day_ini,
      julian_day_fim,
      rmse(cota_model, cota_obs),
      rmse(temp_model, temp_obs),
      rmse(evap_model, evap_obs),
      rmse(fosfato_model, fosfato_obs),
      rmse(do_model, do_obs),
      rmse(chla_model, chla_obs)
    ),
    
    MAE = c(
      julian_day_ini,
      julian_day_fim,
      mae(cota_model, cota_obs),
      mae(temp_model, temp_obs),
      mae(evap_model, evap_obs),
      mae(fosfato_model, fosfato_obs),
      mae(do_model, do_obs),
      mae(chla_model, chla_obs)
    ),
    
    Bias = c(
      julian_day_ini,
      julian_day_fim,
      bias(cota_model, cota_obs),
      bias(temp_model, temp_obs),
      bias(evap_model, evap_obs),
      bias(fosfato_model, fosfato_obs),
      bias(do_model, do_obs),
      bias(chla_model, chla_obs)
    ),
    
    NSE = c(
      julian_day_ini,
      julian_day_fim,
      nse(cota_obs, cota_model),
      nse(temp_obs, temp_model),
      nse(evap_obs, evap_model),
      nse(fosfato_obs, fosfato_model),
      nse(do_obs, do_model),
      nse(chla_obs, chla_model)
    ),

    KGE = c(
      julian_day_ini,
      julian_day_fim,
      kge(cota_obs, cota_model),
      kge(temp_obs, temp_model),
      kge(evap_obs, evap_model),
      kge(fosfato_obs, fosfato_model),
      kge(do_obs, do_model),
      kge(chla_obs, chla_model)
    ),
    
    Skill_Persistencia = c(
      julian_day_ini,
      julian_day_fim,
      skill_vs(cota_model, cota_obs, base_pred = cota_obs_start),
      skill_vs(temp_model, temp_obs, temp_obs_start),
      skill_vs(evap_model, evap_obs, evap_obs_start),
      skill_vs(fosfato_model, fosfato_obs, fosfato_obs_start),
      skill_vs(do_model, do_obs, do_obs_start),
      skill_vs(chla_model, chla_obs, chla_obs_start)
    )
  )

  # O perfil é necessário para a simulação tipo 2 e para o objetivo TEMPPERFIL.
  # A ausência de um perfil válido interrompe a otimização, mas não impede que os
  # demais gráficos da simulação sejam produzidos.
  perfil_solicitado <-
    (identical(as.integer(tipo), 2L) && identical(as.integer(salvar_figura), 1L)) ||
    "TEMPPERFIL" %in% varsobj
  perfil_temperatura <- NULL
  if (perfil_solicitado) {
    perfil_temperatura <- tryCatch(
      calcular_perfis_temperatura(dir, seg, reserv_sigla_perfil, ano_simul),
      error = function(e) {
        if ("TEMPPERFIL" %in% varsobj) {
          stop(conditionMessage(e), call. = FALSE)
        }
        if (!is.null(shiny::getDefaultReactiveDomain())) {
          shiny::showNotification(
            conditionMessage(e),
            type = "warning",
            duration = 10
          )
        }
        warning(
          "Não foi possível calcular/plotar o perfil de temperatura: ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
  }

  if (!is.null(perfil_temperatura)) {
    perfil_obs <- perfil_temperatura$df_paired$Temp_Medida
    perfil_sim <- perfil_temperatura$df_paired$Temp_Simulada
    metricas <- dplyr::bind_rows(
      metricas,
      tibble::tibble(
        variavel = "Temperatura Perfil",
        RMSE = perfil_temperatura$FO3,
        MAE = mae(perfil_sim, perfil_obs),
        Bias = bias(perfil_sim, perfil_obs),
        NSE = nse(perfil_obs, perfil_sim),
        KGE = kge(perfil_obs, perfil_sim),
        Skill_Persistencia = skill_vs(
          perfil_sim,
          perfil_obs,
          make_persist(perfil_obs)
        )
      )
    )
  }
  


  
  if (simul_aleatoria==1){
    
    n <- 10
    chars <- c(LETTERS, letters, 0:9)
    simul <- paste0(sample(chars, n, replace = TRUE), collapse = "")
  } else {
    simul=simul
  }
  
  # write.table(erro_2, paste0(dir,"/resultados_tipo",tipo,"/simul_erro_",simul,".txt"))
  

  if (salvar_resultados==1){

    copiar_resultado <- function(origem, destino, descricao) {
      if (!file.exists(origem)) {
        shiny::showNotification(
          paste("Arquivo não encontrado:", descricao, "-", origem),
          type = "warning",
          duration = 8
        )
        return(FALSE)
      }

      copiado <- tryCatch(
        copiar_arquivo_seguro(origem, destino),
        error = function(e) {
          shiny::showNotification(
            conditionMessage(e),
            type = "error",
            duration = 8
          )
          FALSE
        }
      )

      if (!isTRUE(copiado)) {
        shiny::showNotification(
          paste("Não foi possível copiar:", descricao, "-", origem),
          type = "error",
          duration = 8
        )
      }

      isTRUE(copiado)
    }

    copiar_resultado(
      file.path(dir, "w2_con.npt"),
      file.path(diretorio_resultados, paste0("w2_con_simul_", simul, ".npt")),
      "arquivo de controle"
    )
    copiar_resultado(
      arquivo_b,
      file.path(diretorio_resultados, arquivo_bath),
      "arquivo de batimetria"
    )
    copiar_resultado(
      file.path(dir, "shade.prn"),
      file.path(diretorio_resultados, paste0("shade_simul_", simul, ".prn")),
      "arquivo de sombreamento"
    )
    copiar_resultado(
      file.path(dir, "FLOWBAL.OPT"),
      file.path(diretorio_resultados, paste0("FLOWBAL_simul_", simul, ".OPT")),
      "balanço de fluxo"
    )
    copiar_resultado(
      file.path(dir, paste0("tsr_1_SEG", seg, ".OPT")),
      file.path(
        diretorio_resultados,
        paste0("tsr_1_SEG_simul_", simul, ".OPT")
      ),
      "resultado temporal do segmento"
    )
    copiar_resultado(
      file.path(dir, "wsc.prn"),
      file.path(diretorio_resultados, paste0("wsc_simul_", simul, ".prn")),
      "arquivo wsc"
    )
    copiar_resultado(
      arquivo_spr,
      file.path(diretorio_resultados, paste0("spr_simul_", simul, ".opt")),
      "arquivo de perfis SPR"
    )
   
  }


  if (salvar_figura == 1){
    
 
          tam=(julian_day_fim-julian_day_ini+1)
        
          data_ini <- as.Date(julian_day_ini - 1, origin = paste0(ano_simul, "-01-01"))
          data_fim <- as.Date(julian_day_fim - 1, origin = paste0(ano_simul, "-01-01"))
          
          Data <- seq(data_ini, data_fim, by = "day")
           n <- length(Data)
           idx <- seq(1, n, by = 30) 
           col_metrica <- switch(objfun,
                                 "mae"   = "MAE",
                                 "rmse"  = "RMSE",
                                 "nse"   = "NSE",
                                 "kge"   = "KGE",
                                 "bias"  = "Bias",
                                 "skill" = "Skill_Persistencia")
           
           get_metrica <- function(nome_variavel) {
             metricas[metricas$variavel == nome_variavel, col_metrica, drop = TRUE]
           }
           
          

    if (tipo == 1 || tipo == 2){
     

           
      arquivo_figura_hd <- file.path(
        diretorio_resultados,
        paste0("simul_hd_", simul, ".png")
      )
      validar_arquivo_para_gravacao(arquivo_figura_hd, "figura da simulação")
      png(filename = arquivo_figura_hd,
          width = 1200, height = 1600, res = 150)
      
      # Layout: 3 linhas, 1 coluna
      # Ajuste de margens e espaçamento vertical entre gráficos
      par(mfrow = c(3, 1),
          mar = c(5,7,3.5,2),  # margens internas
          oma = c(1,2,1,1),    # margens externas
          mgp = c(2.5, 0.8, 0),
          las = 1,
          xaxs = "i", yaxs = "i")
      
      plot_safe(Data, cota_obs, cota_model,
                main = "Evolução da Cota de acumulação do Reservatório",
                ylab = "Cota (m)",
                valor_metrica  = get_metrica("Cota"),
                nome_metrica  = col_metrica,
                base_cex = 1.2)
      
      plot_safe(Data, temp_obs, temp_model,
                main = "Temperatura da Água do Reservatório",
                ylab = "ºC",
                valor_metrica  = get_metrica("Temperatura"),
                nome_metrica  = col_metrica,
                base_cex = 1.2,
                force_nonneg = TRUE)
      
      plot_safe(Data, evap_obs, evap_model,
                main = "Evaporação do Reservatório",
                ylab = expression("Evaporação (hm"^3*")"),
                valor_metrica  = get_metrica("Evaporação"),
                nome_metrica  = col_metrica,
                base_cex = 1.2)

      dev.off()

      if (identical(as.integer(tipo), 2L) && !is.null(perfil_temperatura)) {
        graficos_perfil <- plot_perf_temp(perfil_temperatura$df_paired)
        if (length(graficos_perfil)) {
          arquivo_figura_perfil <- file.path(
            diretorio_resultados,
            paste0("perfis_temperatura_", simul, ".png")
          )
          validar_arquivo_para_gravacao(
            arquivo_figura_perfil,
            "figura dos perfis de temperatura"
          )
          ggplot2::ggsave(
            filename = arquivo_figura_perfil,
            plot = patchwork::wrap_plots(graficos_perfil, ncol = 4),
            width = 16,
            height = max(6, ceiling(length(graficos_perfil) / 4) * 4),
            units = "in",
            dpi = 300
          )
        }
      }
      
      
  }
           
           
   else if (tipo == 3){
     
     
     
     arquivo_figura_p <- file.path(
       diretorio_resultados,
       paste0("simul_P_", simul, ".png")
     )
     validar_arquivo_para_gravacao(arquivo_figura_p, "figura da simulação")
     png(filename = arquivo_figura_p,
         width = 1500, height = 2000, res = 150)
     
     # Layout: 3 linhas, 1 coluna
     # Ajuste de margens e espaçamento vertical entre gráficos
     par(mfrow = c(6, 1),
         mar = c(5,7,3.5,2),  # margens internas
         oma = c(1,2,1,1),    # margens externas
         mgp = c(2.5, 0.8, 0),
         las = 1,
         xaxs = "i", yaxs = "i")
     
     plot_safe(Data, cota_obs, cota_model,
               main = "Evolução da Cota de acumulação do Reservatório",
               ylab = "Cota (m)",
               valor_metrica  = get_metrica("Cota"),
               nome_metrica  = col_metrica,
               base_cex = 1.2)
     
     plot_safe(Data, temp_obs, temp_model,
               main = "Temperatura da Água do Reservatório",
               ylab = "ºC",
               valor_metrica  = get_metrica("Temperatura"),
               nome_metrica  = col_metrica,
               base_cex = 1.2,
               force_nonneg = TRUE)
     
     plot_safe(Data, evap_obs, evap_model,
               main = "Evaporação do Reservatório",
               ylab = expression("Evaporação (hm"^3*")"),
               valor_metrica = get_metrica("Evaporação"),
               nome_metrica  = col_metrica,
               base_cex = 1.2)
     
     plot_safe(Data, fosfato_obs, fosfato_model,
               main = "Fosfato",
               ylab = expression("Fosfato (mg/L)"),
               valor_metrica  = get_metrica("Fosfato"),
               nome_metrica  = col_metrica,
               base_cex = 1.2)
     
     plot_safe(Data, do_obs, do_model,
               main = "Oxigênio Dissolvido",
               ylab = expression("Oxigênio Dissolvido (mg/L)"),
               valor_metrica  = get_metrica("Oxigênio Dissolvido"),
               nome_metrica  = col_metrica,
               base_cex = 1.2)
     
     plot_safe(Data, chla_obs, chla_model,
               main = "Clorofila-a",
               ylab = expression("Clorofila-a (mg/L)"),
               valor_metrica  = get_metrica("Clorofila-a"),
               nome_metrica  = col_metrica,
               base_cex = 1.2)
     
     dev.off()
     
     
   }
     
     
     
     
     
  }
  }

  return(list(
    metricas = metricas,
    perfil_temperatura = perfil_temperatura
  ))
  
}





# Compatibilidade com scripts externos que ainda utilizam os nomes antigos.
Simula_Quali <- preparar_arquivos_cequal
Simula_Quali2 <- processar_resultados_cequal

Otmiza_Quali <- function(x, contexto, diretorio_cequal, varsobj, objfun, windows=FALSE){
  penalizar_execucao <- function(motivo) {
    cat(
      "Execução penalizada:",
      motivo,
      "| Parâmetros:",
      paste(round(x, 2), collapse = ", "),
      "\n"
    )
    rep(10^6, length(varsobj))
  }

  if ("TEMPPERFIL" %in% varsobj) {
    validar_arquivo_perfis_observados(
      contexto$caminhos$diretorio,
      contexto$configuracao$reservatorio
    )
  }

  if (isFALSE(windows)) {
  
  gera_arquivo <- preparar_arquivos_cequal(
    parametros = x,
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
  
  preproc <- 0
  if (preproc == 1) {
    shell(shQuote(arq_exe1), wait = TRUE)
    preproc <- 0
  }
  
  execucao_cequal <- executar_cequal_monitorado(
    executavel = file.path(app_dir, "bin", "w2_3.7_64.exe"),
    diretorio = diretorio_cequal,
    em_otimizacao = TRUE
  )
  if (execucao_cequal$codigo != 0L) {
    return(penalizar_execucao(paste(execucao_cequal$saida, collapse = " ")))
  }
  
  resultado_simulacao <- tryCatch(
    processar_resultados_cequal(
      simul = 1,
      salvar_figura = 0,
      salvar_resultados = 0,
      simul_aleatoria = 1,
      varsobj = varsobj,
      objfun = objfun,
      contexto = contexto
    ),
    error = function(e) {
      cat("Falha ao processar resultados:", conditionMessage(e), "\n")
      NULL
    }
  )
  if (is.null(resultado_simulacao)) {
    return(penalizar_execucao("resultados ausentes ou inválidos"))
  }

  
  metricas <- resultado_simulacao$metricas
 
  
  }else{

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

    for (sim_j in 1:(nrow(contexto$monitoramento)-1)){
     
      contexto_janela <- criar_contexto_janela(
        contexto = contexto,
        indice = sim_j,
        carregar_dados = carregar_dados_simulacao
      )
      gera_arquivo <- preparar_arquivos_cequal(
        parametros = x,
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
      
      preproc <- 0
      if (preproc == 1) {
        shell(shQuote(arq_exe1), wait = TRUE)
        preproc <- 0
      }
      
      execucao_cequal <- executar_cequal_monitorado(
        executavel = file.path(app_dir, "bin", "w2_3.7_64.exe"),
        diretorio = diretorio_cequal,
        em_otimizacao = TRUE
      )
      if (execucao_cequal$codigo != 0L) {
        return(penalizar_execucao(paste(
          "janela", sim_j,
          paste(execucao_cequal$saida, collapse = " ")
        )))
      }
      
      metricas <- tryCatch(
        processar_resultados_cequal(
          simul = sim_j,
          salvar_figura = 0,
          salvar_resultados = 0,
          simul_aleatoria = 0,
          varsobj = varsobj,
          objfun = objfun,
          contexto = contexto_janela
        ),
        error = function(e) {
          cat(
            "Falha ao processar resultados da janela",
            sim_j, ":", conditionMessage(e), "\n"
          )
          NULL
        }
      )
      if (is.null(metricas)) {
        return(penalizar_execucao(paste(
          "resultados ausentes ou inválidos na janela", sim_j
        )))
      }
      
      erro_janela[[sim_j]] <- metricas
      
      
    }
    metricas <- bind_rows(
      lapply(erro_janela, function(x) x$metricas),
      .id = "simulacao"
    )
    
    metricas <- metricas |>
      dplyr::filter(!variavel %in% c("Data Ini", "Data fim")) |>
      dplyr::group_by(variavel) |>
      dplyr::summarise(
        RMSE = mean(RMSE, na.rm = TRUE),
        MAE = mean(MAE, na.rm = TRUE),
        Bias = mean(Bias, na.rm = TRUE),
        NSE = mean(NSE, na.rm = TRUE),
        KGE = mean(KGE, na.rm = TRUE),
        Skill_Persistencia = mean(Skill_Persistencia, na.rm = TRUE),
        .groups = "drop"
      )
    
    
  
  }


  info_obj <- gerar_info_objetivo(varsobj, objfun)
  variaveis_tabela <- info_obj$variaveis_tabela
  col_metrica      <- info_obj$col_metrica
  
  FO <- purrr::map_dbl(
    variaveis_tabela,
    function(nome_variavel) {
      valor <- metricas |>
        dplyr::filter(variavel == nome_variavel) |>
        dplyr::pull(.data[[col_metrica]])

      if (length(valor) != 1L) return(NA_real_)
      if (objfun %in% c("nse", "kge", "skill")) {
        valor <- -valor
      }
      as.numeric(valor)
    }
  )
  
  if (length(FO) != length(varsobj)) {
    stop("O número de objetivos retornados não coincide com o número de variáveis selecionadas.")
  }
  
  # Mantém a penalidade positiva após converter NSE, KGE e skill (métricas de
  # maximização) para o problema de minimização.
  FO[!is.finite(FO)] <- 10^6
  
  cat(
    "Variáveis:", paste(round(x, 2), collapse = ", "),
    "| Função objetivo:", paste(round(FO, 4), collapse = ", "),
    "\n"
  )
  
  return(as.numeric(FO))
}


gerar_info_objetivo <- function(varsobj, objfun) {
  
  mapa_variaveis <- c(
    Cota = "Cota",
    TEMP = "Temperatura",
    TEMPPERFIL = "Temperatura Perfil",
    EVAP = "Evaporação",
    DO   = "Oxigênio Dissolvido",
    PO4  = "Fosfato",
    ALG1 = "Clorofila-a"

  )
 
  col_metrica <- switch(objfun,
                        "mae"   = "MAE",
                        "rmse"  = "RMSE",
                        "nse"   = "NSE",
                        "kge"   = "KGE",
                        "bias"  = "Bias",
                        "skill" = "Skill_Persistencia",
                        NULL)
  
  if (is.null(col_metrica)) {
    stop("objfun inválido.")
  }
  
  variaveis_tabela <- unname(mapa_variaveis[varsobj])
  
  if (any(is.na(variaveis_tabela))) {
    stop("Há variáveis em 'varsobj' sem correspondência no mapa de variáveis.")
  }
  
  nomes_obj <- paste0(variaveis_tabela, "_", col_metrica)
  
  list(
    variaveis_tabela = variaveis_tabela,
    col_metrica = col_metrica,
    nomes_obj = nomes_obj
  )
}



nse <- function(x, y) {
  1 - (sum((x - y)^2) / sum((x - mean(x))^2))
}

carregar_dados_simulacao <- function(diretorio_cequal,dados_sim) {


 
  ##Localiza alguns dados diretor do arquivo de controle
  arquivo_controle <- dados_sim$arquivo_controle
  if (is.null(arquivo_controle) || !nzchar(arquivo_controle)) {
    arquivo_controle <- file.path(diretorio_cequal, "w2_conR.npt")
  }
  if (!file.exists(arquivo_controle)) {
    showModal(modalDialog(
      title = "Arquivo de controle não encontrado",
      HTML(paste0(
        "O arquivo de controle não foi encontrado:<br><i>",
        arquivo_controle, "</i>"
      )),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(NULL)
  }
  
  linhas <- ler_linhas_seguro(arquivo_controle, warn = FALSE, descricao = "arquivo de controle")
  ctmin   <- localiza_valor(linhas, "EBOT")
  nsegmentos   <- localiza_valor(linhas, "IMX")
  ctmax <- localiza_valor(linhas, "ELTRT") #volume-máximo
  ctver <- localiza_valor(linhas, "ESTR")  #vertedouro

  
  ##Localiza dados direto da tela, que anteriomente foram carregados do arquivo de controle e podem ter sofrido modificações
  
  res <- dados_sim$res
  seg <- dados_sim$ITSR
  julian_day_ini <- dados_sim$TMSTRT
  julian_day_fim <- dados_sim$TMEND
  ano_simul<-dados_sim$YEAR
  tipo <- dados_sim$tipo
  preproc <- dados_sim$preproc
  ELWS<-dados_sim$ELWS
  TEMP<-dados_sim$TEMP
  PO4<-dados_sim$PO4
  DO<-dados_sim$DO
 
  ALG1<-dados_sim$ALG1

  
  # Leitura dos dados observados
  caminho_arquivo <- file.path(diretorio_cequal, paste0("dados_obs_", res, ".xlsx"))
  nome_aba <- paste0("dados_obs_", res)
  
  if (!file.exists(caminho_arquivo)) {
    
    showModal(modalDialog(
      title = "Arquivo de dados observado não encontrado",
      HTML(paste0(
        "O arquivo <b>dados_obs_", res, ".xlsx</b> não foi encontrado no diretório:<br><i>",
        diretorio_cequal, "</i>"
      )),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(NULL)   # Interrompe a execução sem erro
  }


  dados_obs <- ler_tabela_segura(
    caminho_arquivo,
    readxl::read_excel,
    sheet = nome_aba,
    descricao = "planilha de dados observados"
  )

  
  dados_obs <- dados_obs[floor(julian_day_ini):julian_day_fim, ]

  
  .extract_obs_df <- function(dados_obs) {
    df <- dados_obs %>%
      filter(`TempAgua(C)` != -999) %>%
      select(JDAY, Cota, `TempAgua(C)`)
    return(df)
  }

  dados_janela=.extract_obs_df(dados_obs)

  # Variáveis observadas
  cota_obs     <- dados_obs[[3]]
  temp_obs     <- dados_obs[[6]]
  evap_obs     <- dados_obs[[7]]
  fosfato_obs  <- dados_obs[[8]]
  nitrito_obs  <- dados_obs[[9]]
  chla_obs     <- dados_obs[[12]]
  do_obs       <- dados_obs[[13]]

  dados_reservatorio<-list(
    julian_day_ini = julian_day_ini,
    julian_day_fim = julian_day_fim,
    ano_simul = ano_simul,
    preproc = preproc,
    tipo=tipo,
    ELWS=ELWS,
    TEMP=TEMP,
    ctmax = ctmax,
    ctver = ctver,
    ctmin = ctmin,
    res = res,
    seg = seg,
    nsegmentos = nsegmentos,
    cota_obs = cota_obs,
    temp_obs = temp_obs,
    evap_obs = evap_obs,
    fosfato_obs = fosfato_obs,
    nitrito_obs = nitrito_obs,
    chla_obs = chla_obs,
    do_obs = do_obs,
    dados_janela  = dados_janela,
    PO4=PO4,
    DO=DO,
    ALG1=ALG1
    ,arquivo_controle=arquivo_controle
  )

  return(dados_reservatorio)
}

localiza_valor <- function(linhas, palavra) {
  idx <- grep(paste0("\\b", palavra, "\\b"), linhas, perl = TRUE)
  if (length(idx) == 0) {
    warning(paste("Variável", palavra, "não encontrada."))
    return(NA)
  }
  
  header <- strsplit(trimws(linhas[idx[1]]), "\\s+")[[1]]
  valores <- strsplit(trimws(linhas[idx[1] + 1]), "\\s+")[[1]]
  
  # Se a linha de valores começar com "WB" ou "BR", remove os 2 primeiros itens
  if (valores[1] %in% c("WB", "BR")) {
    valores <- valores[-c(1,2)]
  }
  
  if (valores[1] %in% c("BR1")) {
    valores <- valores[-c(1)]
  }
  # Se header for maior que valores, corta o excesso da esquerda
  if (length(valores) < length(header)) {
    header <- tail(header, length(valores))
  }
  
  pos <- match(palavra, header)
  if (is.na(pos)) {
    warning(paste("Variável", palavra, "não encontrada no arquivo de controle"))
    return(NA)
  }
  
  return(as.numeric(valores[pos]))
}

localiza_arquivo <- function(linhas, palavra) {
  idx <- grep(palavra, linhas)
  if (length(idx) == 0) {
    warning(paste("Bloco", palavra, "não encontrado no arquivo."))
    return(NA)
  }
  
  # pega a linha seguinte
  valores <- strsplit(trimws(linhas[idx[1] + 1]), "\\s+")[[1]]
  
  # se tiver "WB" ou "BR" no início, remove
  if (valores[1] %in% c("WB", "BR")) {
    valores <- valores[-c(1,2)]
  }
  
  # normalmente sobra só o nome do arquivo
  return(valores[1])
}


# Função genérica para extrair um bloco do arquivo de batimetria
localiza_bloco <- function(linhas, bloco) {
  # posição onde o bloco começa
  start <- grep(paste0("^", bloco, "\\b"), linhas)
  if (length(start) == 0) {
    warning(paste("Bloco", bloco, "não encontrado"))
    return(NA)
  }
  
  # pega todas as linhas seguintes até encontrar um novo cabeçalho (linha com letras)
  end <- which(grepl("^[A-Za-z]", linhas))
  end <- end[end > start]
  if (length(end) == 0) {
    fim <- length(linhas)
  } else {
    fim <- min(end) - 1
  }
  
  # pega as linhas de valores
  valores_linhas <- linhas[(start+1):fim]
  
  # transforma em vetor numérico
  valores <- scan(text = paste(valores_linhas, collapse = " "), quiet = TRUE)
  return(valores)
}

ler_parametros <- function(path) {
  # tenta com ;
  param <- tryCatch(
    read.csv(path, header = TRUE, sep = ";", stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  
  # se só veio 1 coluna, tenta com ,
  if (!is.null(param) && ncol(param) == 1) {
    param <- tryCatch(
      read.csv(path, header = TRUE, sep = ",", stringsAsFactors = FALSE),
      error = function(e) stop("Erro ao ler arquivo de parâmetros: ", e$message)
    )
  }
  
  return(param)
}


substitui_bloco <- function(linhas, chave, novo_valor) {
  # encontra índice da chave
  idx_ini <- grep(paste0("^", chave, "$"), trimws(linhas))
  if (length(idx_ini) == 0) {
    warning(paste("Chave", chave, "não encontrada"))
    return(linhas)
  }
  
  # encontra onde termina o bloco: primeira linha vazia ou próxima seção
  idx_fim <- idx_ini + 1
  while (idx_fim <= length(linhas) && trimws(linhas[idx_fim]) != "" &&
         !grepl("^[A-Za-z]", trimws(linhas[idx_fim]))) {
    idx_fim <- idx_fim + 1
  }
  idx_fim <- idx_fim - 1
  
  # gera linhas novas com o novo valor, mantendo 10 valores por linha
  n_valores <- sum(strsplit(paste(linhas[(idx_ini+1):idx_fim], collapse=" "), "\\s+")[[1]] != "")
  novos <- rep(sprintf("%7.3f", as.numeric(novo_valor)), n_valores)
  novas_linhas <- sapply(split(novos, ceiling(seq_along(novos)/10)), paste, collapse=" ")
  
  # substitui no objeto
  linhas[(idx_ini+1):idx_fim] <- novas_linhas
  return(linhas)
}

plot_safe <- function(Data, obs, model,
                      main = "", xlab = "Data", ylab = "",
                      ylim = NULL, valor_metrica = NULL,
                      nome_metrica = NULL,
                      base_cex = 1.6,
                      force_nonneg = FALSE,
                      ylab_line = 3.5,
                      xlab_line = 3.5,
                      pch_size = 1.2) {
  
  # trata valores -999
  obs[obs == -999] <- NA
  model[model == -999] <- NA
  
  n <- min(length(Data), length(obs), length(model))
  if (n == 0) {
    warning("Séries vazias, nada a plotar.")
    return(NULL)
  }
  
  Data  <- Data[1:n]
  obs   <- obs[1:n]
  model <- model[1:n]
  
  if (is.null(ylim)) {
    ylim <- range(c(obs, model), na.rm = TRUE)
    ylim <- c(ylim[1] * 0.95, ylim[2] * 1.05)
    if (force_nonneg) ylim[1] <- max(0, ylim[1])
  }
  
  # Observados em pontos
  plot(Data, obs,
       type = "p", pch = 16, col = "#1f77b4", cex = pch_size,
       ylim = ylim,
       main = main,
       xlab = "", ylab = "",
       xaxt = "n", bty = "l",
       cex.main = base_cex * 1.2,
       cex.lab  = base_cex,
       cex.axis = base_cex * 0.9,
       font.main = 2,
       mgp = c(3, 1, 0))
  
  # Modelo em linha
  lines(Data, model, lwd = 2, col = "#d62728")
  
  if (!is.null(obs) && !all(is.na(obs)) && !all(is.na(model))) {
    box(col = "gray50", lwd = 1.2)
  }
  
  # Eixo X manual
  n_ticks <- max(2, round(n / 90))
  idx <- round(seq(1, n, length.out = n_ticks))
  
  axis(1, at = Data[idx], labels = FALSE)
  
  text(x = Data[idx],
       y = par("usr")[3] - 0.04 * diff(par("usr")[3:4]),
       labels = format(Data[idx], "%d/%m/%Y"),
       srt = 45,
       adj = 1,
       xpd = TRUE,
       cex = base_cex * 0.8)
  
  grid(nx = NA, ny = NULL, col = "gray90", lty = "dotted")
  
  legend("topleft", legend = c("Observado", "Calculado"),
         col = c("#1f77b4", "#d62728"),
         pch = c(16, NA), lty = c(NA, 1), lwd = 2,
         bty = "n", cex = base_cex)
  
  if (!is.null(valor_metrica) && !is.null(nome_metrica)) {
    mtext(paste(nome_metrica, "=", round(valor_metrica, 2)),
          side = 3, line = -2,
          adj = 1, col = "black", font = 2,
          cex = base_cex * 0.8)
  }
  
  if (ylab != "") {
    usr <- par("usr")
    y_centro <- mean(usr[3:4])
    x_pos <- usr[1] - 0.08 * diff(usr[1:2])
    
    text(x = x_pos, y = y_centro,
         labels = ylab,
         srt = 90,
         adj = 0.5,
         xpd = TRUE)
  }
  
  if (xlab != "") {
    mtext(xlab, side = 1, line = xlab_line + 1.2, cex = base_cex * 0.7)
  }
}

gerar_graficos_rmse <- function(
    metricas_consolidadas,
    diretorio_cequal,
    tipo,
    subpasta_resultados = NULL,
    identificador_simulacao = NULL
) {

  diretorio_resultados <- file.path(
    diretorio_cequal,
    paste0("resultados_tipo", tipo)
  )
  if (!is.null(subpasta_resultados) && nzchar(subpasta_resultados)) {
    diretorio_resultados <- file.path(
      diretorio_resultados,
      subpasta_resultados
    )
  }
  dir.create(diretorio_resultados, recursive = TRUE, showWarnings = FALSE)

  prefixo_simulacao <- if (
    is.null(identificador_simulacao) ||
    !nzchar(as.character(identificador_simulacao))
  ) {
    ""
  } else {
    paste0("simul_", identificador_simulacao, "_")
  }
  
  if (tipo ==1 || tipo==2){
    variaveis <- c("Cota", "Temperatura", "Evaporação")
    
    unidades <- c("m", "ºC", "hm³")
  } else {
    variaveis <- c("Cota", "Temperatura", "Evaporação", 
                   "Fosfato", "Oxigênio Dissolvido", "Clorofila-a")
    
    unidades <- c("m", "ºC", "hm³", "mg/L", "mg/L", "µg/L")
  }
  
  # ---- 1. Reorganizar as métricas em formato largo ----
  tabela_reorganizada <- metricas_consolidadas %>%
    tidyr::pivot_wider(
      id_cols = simulacao,
      names_from = variavel,
      values_from = c(RMSE, MAE, Bias, NSE, Skill_Persistencia),
      names_glue = "{.value}_{variavel}"
    )
  
  # ---- 2. Definir variáveis e suas unidades ----
 
  nomes_salvos <- gsub(" ", "_", variaveis)  # remove espaços para salvar arquivos
  
  # ---- 3. Gerar e salvar gráficos ----
  for (i in seq_along(variaveis)) {
    v <- variaveis[i]
    unidade <- unidades[i]
    coluna <- paste0("RMSE_", v)
    
    if (coluna %in% names(tabela_reorganizada)) {
      p <- ggplot2::ggplot(tabela_reorganizada, 
                           ggplot2::aes(x = factor(simulacao, levels = as.character(sort(unique(as.numeric(simulacao))))), y = .data[[coluna]])
                           ) +
        ggplot2::geom_col(fill = "#2E86DE", color = "black", width = 0.7) +
        ggplot2::geom_text(ggplot2::aes(label = round(.data[[coluna]], 3)),
                           vjust = -0.5, size = 3.5, color = "black") +
        ggplot2::labs(
          title = paste(v),
          x = "Simulação",
          y = paste0("RMSE (", unidade, ")")
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
      
      print(p)
      
      # caminho de saída
      caminho_saida <- file.path(
        diretorio_resultados,
        paste0("RMSE_", prefixo_simulacao, nomes_salvos[i], ".jpg")
      )
      
      validar_arquivo_para_gravacao(caminho_saida, "gráfico de RMSE")
      ggplot2::ggsave(
        filename = caminho_saida,
        plot = p,
        width = 8, height = 5, dpi = 300,
        bg = "white"
      )
    }
  }
  
  

  # ---- 4. Gerar boxplot individual para cada variável ----

  
  # data de referência
  data_ref <- as.Date("2016-01-01")  # ano arbitrário

  tabela_com_data <- tabela_reorganizada %>%
    mutate(
      data_ini = data_ref + (`RMSE_Data Ini` - 1),
      mes_ini  = lubridate::month(data_ini)
    )
  
  
 
  
  # ---- Tabela 1: antes de junho (jan a mai) ----
  tabela_chuvoso <- tabela_com_data %>%
    filter(mes_ini < 6)
  tabela_chuvoso <- tabela_chuvoso %>%
    select(-matches("Data Ini|Data fim"))
  
  
  
  # ---- Tabela 2: depois de julho (jul a dez) ----
  tabela_seco <- tabela_com_data %>%
    filter(mes_ini > 6)
   tabela_seco <- tabela_seco %>%
     select(-matches("Data Ini|Data fim"))
   
   

  
##### para dois boxplot
  
   
  
  for (i in seq_along(variaveis))  {
    
    nome_variavel <- paste0("RMSE_", variaveis[i])
    unidade <- unidades[i]

    # Junta as duas tabelas em formato longo
    dados_var <- bind_rows(
      tabela_chuvoso %>%
        select(simulacao, all_of(nome_variavel)) %>%
        mutate(Periodo = "Periodo Chuvoso"),
      
      tabela_seco %>%
        select(simulacao, all_of(nome_variavel)) %>%
        mutate(Periodo = "Periodo Seco")
    ) %>%
      rename(RMSE = all_of(nome_variavel))
    
    # Cria o gráfico
    p_box <- ggplot(dados_var, aes(x = Periodo, y = RMSE, fill = Periodo)) +
      geom_boxplot(width = 0.5, alpha = 0.85, color = "black") +
      scale_fill_manual(
        values = c(
          "Periodo Chuvoso" = "#00BFC4",  
          "Periodo Seco"    = "#F8766D"   
        ) 
      ) +
      stat_summary(fun = mean, geom = "point",
                   shape = 18, size = 3, color = "red") +
      labs(
        title = paste("Distribuição do RMSE –", gsub("^RMSE_","",nome_variavel)),
        x = "",
        y = paste0("RMSE (", unidade, ")")
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none"
      )
    
    print(p_box)
    
    # Caminho de saída
    caminho_box <- file.path(
      diretorio_resultados,
      paste0(
        "RMSE_boxplot_periodos_",
        prefixo_simulacao,
        nome_variavel,
        ".jpg"
      )
    )
    
    # Salva o gráfico
    validar_arquivo_para_gravacao(caminho_box, "gráfico de RMSE")
    ggsave(
      filename = caminho_box,
      plot = p_box,
      width = 7, height = 5, dpi = 300, bg = "white"
    )
  }
  
  
  
  
}


gerar_limites <- function(tipo){
  
  if(tipo == "1"){
    data.frame(
      parametro = c("AFW","BFW","CFW","WSC","SHADE"),
      lower = c(1, 1, 0, 5, 0),
      upper = c(20, 20, 3, 10, 1)
    )
  }
  
  else if(tipo == "2"){
    data.frame(
      parametro = c("AFW","BFW","CFW","WSC","SHADE",
                    "AX","DX","CBHE","EXH2O","BETA"),
      lower = c(1,1,0,5,0,0,0,0,25,0),
      upper = c(10,10,2,10,1,1,1,1,100,1)
    )
  }
  
  else if(tipo == "3"){
    data.frame(
      parametro = c("AG","AR","AE","AM","AS","AHSP","ASAT","SOD"),
      # lower = c(0,0,0,0,0,0,0,50),
      # upper = c(4,2,1,1,1,1,1,100)
      # 
      lower <- c(0, 0, 0, 0, 0, 0, 20, 2),
      upper <- c(25, 1, 1, 1, 1, 5, 100, 15)   #Os parametros estao multiplicados por 10 e 100 por conta da limitação do modelo
      
    )
  }
}




# =========================
# CENARIZAÇÃO DE AFLUÊNCIAS
# =========================

extrair_blocos_tributarios <- function(linhas, cabecalho_regex) {
  idx_ini <- grep(cabecalho_regex, linhas)
  if (length(idx_ini) == 0) {
    stop(paste("Bloco não encontrado no arquivo de controle:", cabecalho_regex))
  }

  inicio <- idx_ini[1] + 1
  fim <- inicio

  while (fim <= length(linhas) && trimws(linhas[fim]) != "") {
    fim <- fim + 1
  }

  linhas_bloco <- linhas[inicio:(fim - 1)]
  linhas_bloco <- linhas_bloco[trimws(linhas_bloco) != ""]

  dados <- lapply(linhas_bloco, function(x) {
    partes <- strsplit(trimws(x), "\\s+")[[1]]
    data.frame(
      braco = partes[1],
      arquivo = partes[2],
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(dados)
}

montar_nomes_arquivos_cenario <- function(bracos, scenario_id) {
  br_num <- tolower(gsub("[^0-9]", "", bracos))

  data.frame(
    braco = bracos,
    QINFN = paste0("FLOW_OUTcms_br", br_num, "_", scenario_id, ".prn"),
    TINFN = paste0("WTMPdegc_br", br_num, "_", scenario_id, ".prn"),
    CINFN = paste0("PO4_br", br_num, "_", scenario_id, ".prn"),
    stringsAsFactors = FALSE
  )
}

montar_preview_cenario_afluencia <- function(
    diretorio_base,
    diretorio_cenario,
    scenario_id = "S1",
    arquivo_controle_base = file.path(diretorio_base, "w2_conR.npt")
) {
  if (!file.exists(arquivo_controle_base)) {
    stop("O arquivo de controle selecionado não foi encontrado: ", arquivo_controle_base)
  }

  linhas <- ler_linhas_seguro(arquivo_controle_base, warn = FALSE, descricao = "arquivo de controle")
  qin <- extrair_blocos_tributarios(linhas, "^QIN FILE")
  tin <- extrair_blocos_tributarios(linhas, "^TIN FILE")
  cin <- extrair_blocos_tributarios(linhas, "^CIN FILE")

  preview <- qin |>
    dplyr::rename(QINFN_atual = arquivo) |>
    dplyr::left_join(tin |> dplyr::rename(TINFN_atual = arquivo), by = "braco") |>
    dplyr::left_join(cin |> dplyr::rename(CINFN_atual = arquivo), by = "braco") |>
    dplyr::left_join(montar_nomes_arquivos_cenario(qin$braco, scenario_id), by = "braco") |>
    dplyr::mutate(
      existe_QINFN = file.exists(file.path(diretorio_cenario, QINFN)),
      existe_TINFN = file.exists(file.path(diretorio_cenario, TINFN)),
      existe_CINFN = file.exists(file.path(diretorio_cenario, CINFN))
    )

  list(
    arquivos = preview,
    faltantes = unique(c(
      preview$QINFN[!preview$existe_QINFN],
      preview$TINFN[!preview$existe_TINFN],
      preview$CINFN[!preview$existe_CINFN]
    ))
  )
}

substituir_bloco_arquivos <- function(linhas, cabecalho_regex, mapa_braco_arquivo) {
  idx_ini <- grep(cabecalho_regex, linhas)
  if (length(idx_ini) == 0) {
    stop(paste("Bloco não encontrado:", cabecalho_regex))
  }

  inicio <- idx_ini[1] + 1
  fim <- inicio
  while (fim <= length(linhas) && trimws(linhas[fim]) != "") {
    fim <- fim + 1
  }

  for (i in inicio:(fim - 1)) {
    linha_trim <- trimws(linhas[i])
    if (linha_trim == "") next

    partes <- strsplit(linha_trim, "\\s+")[[1]]
    braco <- partes[1]

    if (!braco %in% names(mapa_braco_arquivo)) {
      stop(paste("Braço não encontrado no mapa de substituição:", braco))
    }

    linhas[i] <- sprintf("%-8s%s", braco, mapa_braco_arquivo[[braco]])
  }

  linhas
}

preparar_cenario_afluencia_cequal <- function(
    diretorio_base,
    diretorio_cenario,
    scenario_id = "S1",
    arquivo_controle_base = file.path(diretorio_base, "w2_conR.npt")
) {
  if (!file.exists(arquivo_controle_base)) {
    stop("O arquivo de controle selecionado não foi encontrado: ", arquivo_controle_base)
  }

  dir.create(diretorio_cenario, recursive = TRUE, showWarnings = FALSE)

  preview <- montar_preview_cenario_afluencia(
    diretorio_base = diretorio_base,
    diretorio_cenario = diretorio_cenario,
    scenario_id = scenario_id,
    arquivo_controle_base = arquivo_controle_base
  )

  # faltantes <- preview$faltantes[!is.na(preview$faltantes) & nzchar(preview$faltantes)]
  # if (length(faltantes) > 0) {
  #   stop(
  #     paste0(
  #       "Os seguintes arquivos do cenário não foram encontrados na pasta selecionada:\n",
  #       paste(faltantes, collapse = "\n")
  #     )
  #   )
  # }

  arquivo_controle_R <- file.path(
    diretorio_cenario,
    basename(arquivo_controle_base)
  )
  arquivo_controle_cenario <- file.path(diretorio_cenario, "w2_con.npt")
  ok_copy <- copiar_arquivo_seguro(
    arquivo_controle_base,
    arquivo_controle_R
  )
  if (!ok_copy) {
    stop("Não foi possível copiar o arquivo de controle para a pasta do cenário.")
  }

  linhas <- ler_linhas_seguro(arquivo_controle_R, warn = FALSE, descricao = "arquivo de controle")

  mapa_qin <- setNames(preview$arquivos$QINFN, preview$arquivos$braco)
  mapa_tin <- setNames(preview$arquivos$TINFN, preview$arquivos$braco)
  mapa_cin <- setNames(preview$arquivos$CINFN, preview$arquivos$braco)

  linhas <- substituir_bloco_arquivos(linhas, "^QIN FILE", mapa_qin)
  linhas <- substituir_bloco_arquivos(linhas, "^TIN FILE", mapa_tin)
  linhas <- substituir_bloco_arquivos(linhas, "^CIN FILE", mapa_cin)

  gravar_linhas_seguro(linhas, arquivo_controle_cenario, sep = "\n")

  arquivos_para_copiar <- list.files(diretorio_cenario, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  arquivos_para_copiar <- arquivos_para_copiar[file.info(arquivos_para_copiar)$isdir %in% FALSE]

  copias <- vapply(
    arquivos_para_copiar,
    function(origem) copiar_arquivo_seguro(origem, diretorio_base),
    logical(1)
  )

  if (!all(copias)) {
    stop("Nem todos os arquivos da pasta do cenário puderam ser copiados para a pasta raiz do CE-QUAL-W2.")
  }

  list(
    scenario_id = scenario_id,
    diretorio_base = diretorio_base,
    diretorio_cenario = diretorio_cenario,
    arquivo_controle_R = arquivo_controle_R,
    preview = preview$arquivos
  )
}



executar_cenario_afluencia_cequal <- function(
    diretorio_base,
    diretorio_cenario,
    scenario_id = "S1",
    app_dir = getwd(),
    arquivo_controle_base = file.path(diretorio_base, "w2_conR.npt")
) {
  resultado_preparo <- preparar_cenario_afluencia_cequal(
    diretorio_base = diretorio_base,
    diretorio_cenario = diretorio_cenario,
    scenario_id = scenario_id,
    arquivo_controle_base = arquivo_controle_base
  )
  
  exe_pre <- file.path(app_dir, "bin", "W2Pre3.7.exe")
  exe_run <- file.path(app_dir, "bin", "w2_3.7_64.exe")
  
  if (!file.exists(exe_pre)) {
    stop(paste("Executável não encontrado:", exe_pre))
  }
  if (!file.exists(exe_run)) {
    stop(paste("Executável não encontrado:", exe_run))
  }
  
  arquivo_controle_raiz <- arquivo_controle_base
  if (!file.exists(arquivo_controle_raiz)) {
    stop("O arquivo de controle selecionado não está disponível para executar o cenário.")
  }
  
  arq_exe1 <- file.path(diretorio_base, "0_RunW2Pre_cenario.BAT")
  arq_exe2 <- file.path(diretorio_base, "0_RunW2_cenario.BAT")
  
  bat_pre <- c(
    "@echo off",
    paste0('cd /d "', diretorio_base, '"'),
    paste0('"', exe_pre, '"')
  )
  gravar_linhas_seguro(bat_pre, arq_exe1, descricao = "script de pré-processamento")
  
  bat_run <- c(
    "@echo off",
    paste0('cd /d "', diretorio_base, '"'),
    paste0('"', exe_run, '"')
  )
  gravar_linhas_seguro(bat_run, arq_exe2, descricao = "script de execução")
  
  status_pre <- shell(shQuote(arq_exe1), wait = TRUE)
  if (!identical(status_pre, 0L)) {
    stop(paste("Erro ao executar o pré-processamento do cenário. Código de retorno:", status_pre))
  }
  
  status_run <- executar_cequal_monitorado(
    executavel = exe_run,
    diretorio = diretorio_base,
    script_monitor = file.path(
      app_dir,
      "scripts",
      "executar_cequal_monitorado.ps1"
    )
  )$codigo
  if (!identical(status_run, 0L)) {
    stop(paste("Erro ao executar a simulação do cenário. Código de retorno:", status_run))
  }
  
  diretorio_resultados <- file.path(diretorio_cenario, paste0("resultados_", scenario_id))
  dir.create(diretorio_resultados, recursive = TRUE, showWarnings = FALSE)
  
 # metricas<- Simula_Quali2(simul=i,diretorio_cequal=diretorio_resultados, dados_simulacao=dados_simulacao, preproc=preproc, salvar_figura=1,salvar_resultados=1,simul_aleatoria=0 )
  
  
  padroes_resultado <- c(
    "*.OPT", "*.opt", "*.csv", "*.npt", "*.NPT",
    "*.prn", "*.PRN", "*.dat", "*.DAT", "*.log", "*.LOG"
  )
  
  arquivos_resultado <- unique(unlist(lapply(
    padroes_resultado,
    function(pat) list.files(diretorio_base, pattern = glob2rx(pat), full.names = TRUE)
  )))
  
  arquivos_resultado <- arquivos_resultado[file.info(arquivos_resultado)$isdir %in% FALSE]
  print(arquivos_resultado)
  if (length(arquivos_resultado) > 0) {
    copias_resultado <- vapply(
      arquivos_resultado,
      function(origem) copiar_arquivo_seguro(origem, diretorio_resultados),
      logical(1)
    )
    
    if (!all(copias_resultado)) {
      warning("Nem todos os arquivos de resultado puderam ser copiados para a pasta do cenário.")
    }
  }
  
  linhas_controle <- ler_linhas_seguro(
    arquivo_controle_raiz,
    warn = FALSE,
    descricao = "arquivo de controle"
  )
  ano_simul <- localiza_valor(linhas_controle, "YEAR")
  
  arquivo_figura <- gerar_figura_cenario_qualidade(
    diretorio_resultados = diretorio_resultados,
    ano_simul = ano_simul
  )
  
  
  list(
    scenario_id = scenario_id,
    diretorio_base = diretorio_base,
    diretorio_cenario = diretorio_cenario,
    diretorio_resultados = diretorio_resultados,
    arquivo_controle_raiz = arquivo_controle_raiz,
    arquivo_figura = arquivo_figura,
    resultado_preparo = resultado_preparo
  )
}









localizar_arquivo_tsr <- function(diretorio) {
  arqs <- list.files(
    diretorio,
    pattern = "tsr",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  arqs <- arqs[grepl("\\.opt$", arqs, ignore.case = TRUE)]
  
  if (length(arqs) == 0) {
    stop("Nenhum arquivo TSR (.OPT) foi encontrado na pasta de resultados.")
  }
  
  info <- file.info(arqs)
  arqs[which.max(info$mtime)]
}

ler_tsr_resultado <- function(arquivo_tsr) {
  txt <- ler_linhas_seguro(arquivo_tsr, warn = FALSE, descricao = "resultado TSR")
  txt <- stringr::str_replace_all(
    txt,
    "([0-9]E[-+][0-9]+)-",
    "\\1 -"
  )
  
  readr::read_table(
    I(txt),
    skip = 11,
    show_col_types = FALSE
  )
}

classificar_iet_reservatorio <- function(iet) {
  dplyr::case_when(
    is.na(iet)      ~ NA_character_,
    iet <= 47       ~ "Ultraoligotrófico",
    iet <= 52       ~ "Oligotrófico",
    iet <= 59       ~ "Mesotrófico",
    iet <= 63       ~ "Eutrófico",
    iet <= 67       ~ "Supereutrófico",
    TRUE            ~ "Hipereutrófico"
  )
}

gerar_figura_cenario_qualidade <- function(diretorio_resultados,ano_simul,nome_saida = "cenario_qualidade_reservatorio.png") {
  
  arquivo_tsr <- localizar_arquivo_tsr(diretorio_resultados)
  dados <- ler_tsr_resultado(arquivo_tsr)
  
  if (!"JDAY" %in% names(dados)) stop("A coluna JDAY não foi encontrada no arquivo TSR.")
  if (!"ELWS" %in% names(dados)) stop("A coluna ELWS não foi encontrada no arquivo TSR.")
  if (!"PO4"  %in% names(dados)) stop("A coluna PO4 não foi encontrada no arquivo TSR.")
  
  # =========================
  # Datas e séries a partir do w2_con.npt + TSR
  # =========================
  arquivo_controle <- file.path(diretorio_resultados, "w2_con.npt")
  if (!file.exists(arquivo_controle)) {
    stop("O arquivo w2_con.npt não foi encontrado na pasta de resultados do cenário.")
  }
  
  linhas_controle <- ler_linhas_seguro(
    arquivo_controle,
    warn = FALSE,
    descricao = "arquivo de controle"
  )
  
  ano_simul   <- localiza_valor(linhas_controle, "YEAR")
  tmstrt_ctrl <- localiza_valor(linhas_controle, "TMSTRT")
  tmend_ctrl  <- localiza_valor(linhas_controle, "TMEND")
  
  if (!is.finite(ano_simul)) {
    stop("Não foi possível ler o YEAR no arquivo w2_con.npt.")
  }
  
  # Consolida o TSR em uma linha por dia
  # Mantém a última linha disponível de cada dia juliano
  dados_plot <- dados %>%
    dplyr::mutate(
      JDAY_NUM = suppressWarnings(as.numeric(JDAY)),
      JDAY_DIA = floor(JDAY_NUM)
    ) %>%
    dplyr::filter(is.finite(JDAY_NUM), is.finite(JDAY_DIA)) %>%
    dplyr::group_by(JDAY_DIA) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(JDAY_DIA)
  
  if (nrow(dados_plot) == 0) {
    stop("Não foi possível consolidar a série diária a partir do arquivo TSR.")
  }
  
  jday <- as.numeric(dados_plot$JDAY_DIA)
  
  # Usa YEAR e JDAY do controle para montar a data correta
  data <- as.Date(jday - 1, origin = paste0(as.integer(ano_simul), "-01-01"))
  
  cota_m  <- suppressWarnings(as.numeric(dados_plot$ELWS))
  po4_mgL <- suppressWarnings(as.numeric(dados_plot$PO4))
  
  # Prioriza TP do próprio TSR
  if ("TP" %in% names(dados_plot)) {
    tp_ugL <- suppressWarnings(as.numeric(dados_plot$TP))
  } else {
    tp_ugL <- po4_mgL * 1000
  }
  
  # Mantém apenas registros válidos
  ok <- is.finite(jday) &
    !is.na(data) &
    is.finite(cota_m) &
    is.finite(po4_mgL) &
    is.finite(tp_ugL)
  
  jday    <- jday[ok]
  data    <- data[ok]
  cota_m  <- cota_m[ok]
  po4_mgL <- po4_mgL[ok]
  tp_ugL  <- tp_ugL[ok]
  
  # Restringe ao período da simulação definido no w2_con.npt
  if (is.finite(tmstrt_ctrl) && is.finite(tmend_ctrl)) {
    ok_periodo <- jday >= floor(tmstrt_ctrl) & jday <= floor(tmend_ctrl)
    jday    <- jday[ok_periodo]
    data    <- data[ok_periodo]
    cota_m  <- cota_m[ok_periodo]
    po4_mgL <- po4_mgL[ok_periodo]
    tp_ugL  <- tp_ugL[ok_periodo]
  }
  
  if (length(data) == 0) {
    stop("Após aplicar o período do w2_con.npt, a série temporal ficou vazia.")
  }
  
  tp_mgL <- tp_ugL / 1000
  
  # IET de reservatório a partir do fósforo total (ug/L)
  iet <- ifelse(
    is.finite(tp_ugL) & tp_ugL > 0,
    10 * (6 - ((1.77 - 0.42 * log(tp_ugL)) / log(2))),
    NA_real_
  )
  
  classe_iet <- classificar_iet_reservatorio(iet)
  
  # Limites CONAMA 357/2005 para fósforo total em águas doces lênticas
  lim_classe_1 <- 0.02
  lim_classe_2 <- 0.03
  lim_classe_3 <- 0.05
  
  caminho_figura <- file.path(diretorio_resultados, nome_saida)
  
  validar_arquivo_para_gravacao(caminho_figura, "figura do cenário")
  png(caminho_figura, width = 1400, height = 1800, res = 150)
  
  par(
    mfrow = c(4, 1),
    mar = c(5, 7, 3, 2),
    oma = c(1, 1, 1, 1),
    las = 1
  )
  
  plot(
    data, cota_m,
    type = "l", lwd = 2,
    main = "Cota do Reservatório",
    xlab = "",
    ylab = "Cota (m)"
  )
  grid()
  
  plot(
    data, po4_mgL,
    type = "l", lwd = 2,
    main = "Fosfato",
    xlab = "",
    ylab = "PO4 (mg/L)"
  )
  grid()
  
  plot(
    data, tp_mgL,
    type = "l", lwd = 2,
    main = "Fósforo Total",
    xlab = "",
    ylab = "PT (mg/L)"
  )
  abline(h = lim_classe_1, lty = 2)
  abline(h = lim_classe_2, lty = 2)
  abline(h = lim_classe_3, lty = 2)
  legend(
    "topleft",
    legend = c(
      "Classe 1 = 0,02 mg/L",
      "Classe 2 = 0,03 mg/L",
      "Classe 3 = 0,05 mg/L"
    ),
    lty = c(2, 2, 2),
    bty = "n",
    cex = 0.9
  )
  grid()
  
  plot(
    data, iet,
    type = "l", lwd = 2,
    main = "Índice de Estado Trófico (IET - reservatório)",
    xlab = "Data",
    ylab = "IET"
  )
  abline(h = c(47, 52, 59, 63, 67), lty = 3)
  grid()
  
  dev.off()
  
  resumo <- data.frame(
    Data = data,
    JDAY = jday,
    Cota_m = cota_m,
    PO4_mgL = po4_mgL,
    PT_ugL = tp_ugL,
    PT_mgL = tp_mgL,
    IET = iet,
    Classe_IET = classe_iet,
    stringsAsFactors = FALSE
  )
  print(resumo)
  
  gravar_tabela_segura(
    resumo,
    file.path(diretorio_resultados, "serie_qualidade_cenario.csv"),
    readr::write_csv
  )
  
  return(caminho_figura)
}

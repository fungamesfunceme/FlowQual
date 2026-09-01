ReadRCH_SWAT <- function(pathin_rch,
                         data_i,
                         data_f) {
  
 
  
  arq=list.files(pathin_rch)
  existe1=sum(match(arq,"output.rch"), na.rm=T)
  existe2=sum(match(arq,"file.cio"), na.rm=T)
  
  existe=existe1+existe2
  
  if (existe==2){
  cabeca_rch <- scan(file = paste0(pathin_rch, "output.rch"), what = character(),
                     skip = 8, nlines =  1)
  
  cabeca_rch <- c("NAMES_RCH",cabeca_rch)
  
 
  cabeca_rch <- c("NAMES_RCH","RCH","GIS","DAY","AREAkm2","FLOW_INcms",
                                    "FLOW_OUTcms","EVAPcms","TLOSScms","SED_INtons","SED_OUTtons","SEDCONCmg.L","ORGN_INkg",
                                    "ORGN_OUTkg","ORGP_INkg","ORGP_OUTkg","NO3_INkg","NO3_OUTkg","NH4_INkg","NH4_OUTkg",
                                     "NO2_INkg","NO2_OUTkg","MINP_INkg","MINP_OUTkg","CHLA_INkg","CHLA_OUTkg","CBOD_INkg",
                                     "CBOD_OUTkg","DISOX_INkg","DISOX_OUTkg","SOLPST_INmg","SOLPSTOUTmg","SORPST_INmg","SORPSTOUTmg",
                                     "REACTPSTmg","VOLPSTmg","SETTLPSTmg","RESUSPPSTmg","DIFFUSEPSTmg","REACBEDPSTmg","BURYPSTmg",
                                     "BED_PSTmg","BACTP_OUTct","BACTLP_OUTct","CMETAL.1kg","CMETAL.2kg","CMETAL.3kg","TOT_Nkg",
                                     "TOT_Pkg","NO3ConcMg.l","WTMPdegc")  
  

  dados_rch_swat_dy <- read.table(file = paste0(pathin_rch, "output.rch"), 
                                  skip = 9)
  
  
  
#  print(str(dados_rch_swat_dy))
  colnames(dados_rch_swat_dy) <- cabeca_rch
  
  ## Substituindo "MON" por "DAY" no head    
  colnames(dados_rch_swat_dy)[colnames(dados_rch_swat_dy) == "MON"] <- "DAY"
  
  date <- as.Date(paste0(seq.Date(as.Date(data_i),
                                     as.Date(data_f), by = "day")))
  
  
  
  
  
  
  
  
  
  # 
  # print(date)
  # print(dado_swat)
  
  dado_swat <- data.frame(date, dados_rch_swat_dy)
  #print(dado_swat)
  }

  else{
    dado_swat=0
  }
  
  return(dado_swat)
}



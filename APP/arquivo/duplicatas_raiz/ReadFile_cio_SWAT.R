# COPIA LEGADA: preservada para comparacao com a versao consolidada em R/.

ReadFile_cio_SWAT<-function (pathin_rch){
  data.swat=numeric()
  
 # pathin_rch = paste0(diretorio,"/","Reservatorios/Edson Queiroz/")
#pathin_rch="C:/Users/daniel.cid/Área de Trabalho/Reservatórios/Acarape do Meio/"  
#pathin_rch="C:/Users/daniel.cid/OneDrive/FUNCEME/Qualidade da agua/AcomplaSwatCequal/Acarape do Meio/"
filecio <- read.delim(file = paste0(pathin_rch, "file.cio"))

# for (i in 1:length(filecio)){
#   
#   paste(filecio[7,1])
#   
#   str_sub(filecio, "Beginning")
# }


##NBYR : Number of years simulated
l_ano=which(stringr::str_detect(t(filecio), "NBYR")==TRUE)
aux=as.character(stringr::str_trim(filecio[l_ano,1]))
aux=stringr::str_replace_all(aux," ","")
ano_sim=as.integer(strsplit(aux,"[|]")[[1]][1])



##IYR : Beginning year of simulation
l_ano=which(stringr::str_detect(t(filecio), "IYR")==TRUE)
aux=as.character(stringr::str_trim(filecio[l_ano,1]))
aux=stringr::str_replace_all(aux," ","")
ano_ini=as.integer(strsplit(aux,"[|]")[[1]][1])

##IDAF : Beginning julian day of simulation
l_dia=which(stringr::str_detect(t(filecio), "IDAF")==TRUE)
aux=as.character(stringr::str_trim(filecio[l_dia,1]))
aux=stringr::str_replace_all(aux," ","")
dia_ini=as.integer(strsplit(aux,"[|]")[[1]][1])


##IDAL : Ending julian day of simulation
l_dia=which(stringr::str_detect(t(filecio), "IDAL")==TRUE)
aux=as.character(stringr::str_trim(filecio[l_dia,1]))
aux=stringr::str_replace_all(aux," ","")
dia_fim_jul=as.integer(strsplit(aux,"[|]")[[1]][1])

##NYSKIP: number of years to skip output printing/summarization
l_ano=which(stringr::str_detect(t(filecio), "NYSKIP")==TRUE)
aux=as.character(stringr::str_trim(filecio[l_ano,1]))
aux=stringr::str_replace_all(aux," ","")
ano_skip=as.integer(strsplit(aux,"[|]")[[1]][1])

ano_fim=ano_ini+ano_sim-1

aux_dia=seq(from=as.Date(paste0("01-01-",(ano_ini+ano_skip)), format="%d-%m-%Y"), length.out=as.integer(dia_ini), by="day")
data_ini=aux_dia[length(aux_dia)]

dia_fim_aux=seq(from=as.Date(paste0("01-01-",ano_fim), format="%d-%m-%Y"), length.out=dia_fim_jul, by="day")
dia_fim=dia_fim_aux[length(dia_fim_aux)]
dayfim=day(dia_fim)
monthfim=month(dia_fim)


aux_dia_fim=seq(from=data_ini, length.out=ano_sim-ano_skip, by="year")

data_fim=as.Date(paste0(year(aux_dia_fim[length(aux_dia_fim)]),"-",monthfim,"-",dayfim), format="%Y-%m-%d")

data.swat=c(data_ini,data_fim)
return(data.swat)

}




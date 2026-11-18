



dados_simulacao=read.table(paste0(dir,"/dados_simulacao.txt"), sep="=")
res=dados_simulacao[1,2]
seg=as.numeric(dados_simulacao[2,2])
nsegmentos=as.numeric(dados_simulacao[3,2])
julian_day_ini=as.numeric(dados_simulacao[4,2])
julian_day_fim=as.numeric(dados_simulacao[5,2])
ctmax=as.numeric(dados_simulacao[6,2])
ctver=as.numeric(dados_simulacao[7,2])
ctmin=as.numeric(dados_simulacao[8,2])
tipo=as.numeric(dados_simulacao[9,2])
preproc=as.numeric(dados_simulacao[10,2])  


## DADOS OBSERVADOS

dados_obs = read_excel(paste0(dir,"/dados_obs_",res,".xlsx"), sheet = paste0("dados_obs_",res))
dados_obs=dados_obs[(floor(julian_day_ini):julian_day_fim),]

cota_obs=dados_obs[,3]
temp_obs=dados_obs[,6]                
evap_obs=dados_obs[,7]
fosfato_obs=dados_obs[,8]
nitrito_obs=dados_obs[,9]
chla_obs=dados_obs[,12]
do_obs=dados_obs[,13]



library(lhs)


dir="C:/Users/daniel.cid/OneDrive/FUNCEME/Qualidade_da_agua/2024_1/Edson Queiroz/CEQUAL_2016_2021"   ###COLOCAR O CAMINHO DA PASTA DOS ARQUIVOS DO CEQUAL
setwd(dir)


#### An?lise sensibilidade ####
{
nIter=500


#### Intervalo de calibra??o dos par?metros ####
#lower <- as.vector(parametros_automatico_tab$Min)
#lower <- lower * 100
lower=c(0.1,0.001,0.05,0.05,0.5,0.001,0.001,0, 60)



#upper <- as.vector(parametros_automatico_tab$Max)
#upper <- upper * 100
upper=c(3,1,0.5,0.5,1.5,0.01,0.01,0, 100)


paramRange <- cbind(lower, upper)



##### Nome dos par?metros ####
nomes_parametros <- c("AG","AR","AE","AM","AS","AHSP","AHSN","AHSSI","ASAT")

# Adicionando o novo elemento na primeira posi??o
nomes <- c("Rodada", nomes_parametros)







#### Hipercubo latino ####

lhsRange <- function(nIter, paramRange){
  
  nParam <- nrow(paramRange)
  paramSampling <- randomLHS(nIter, nParam)
  
  for (i in 1:nParam){
    paramSampling[,i] <- paramRange[i,1] +  paramSampling[,i] * 
      (paramRange[i,2] - paramRange[i,1])
  }
  
  paramSampling <- cbind(c(1:nrow(paramSampling)), paramSampling)
  return(paramSampling)
}





paramSampling <- lhsRange(nIter,paramRange)

colnames(paramSampling) <- nomes

# Selecionar as colunas de 2 em diante (todas exceto a primeira)
entradas <- paramSampling[, -1]

write.csv(entradas, paste0(dir,"/parametros_lhs.csv"))
}

#######


#####RODAR CEQUAL-W2


######


library(readr)
parametros_P_sensibilidade <- read_csv("C:/Users/daniel.cid/OneDrive/Funceme/Qualidade_da_agua/2024_1/Edson Queiroz/CEQUAL_2016_2021/parametros_P_lhs_1000.csv")
erro_matrix_P <- read_table2("C:/Users/daniel.cid/OneDrive/Funceme/Qualidade_da_agua/2024_1/Edson Queiroz/CEQUAL_2016_2021/Resultados/erro_matrix_P.txt")

paramSampling=parametros_P_sensibilidade[,1:10]
resultado_FO=erro_matrix_P[,4]

# Table with parameter and objective function values
tableSensitivity <- paramSampling
tableSensitivity[,1] <- resultado_FO

colnames(tableSensitivity) <- c("objFunction", nomes_parametros)
tableSensitivity <- as.data.frame(tableSensitivity)

tableSensitivity <- summary(lm(formula = objFunction ~ ., tableSensitivity))[4]$coefficients[,3:4]

# Remove the first row because it is the intercept
tableSensitivity <- tableSensitivity[-c(1),]

# Assign result to the global variables

tableSensitivity <- as.data.frame(tableSensitivity)

# Creat data frame with the results from parameter sensi. analysis
resultado_final <<- data.frame(Parameter = rownames(tableSensitivity),
                               t_stat = tableSensitivity[,1],
                               absolute_t_stat = abs(tableSensitivity[,1]),
                               p_value = tableSensitivity[,2])


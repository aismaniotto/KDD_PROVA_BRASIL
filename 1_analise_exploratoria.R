##### Pacotes necessários #####
library(dplyr)
library(readr)
library(ggplot2)

#### Diretorios #####
dir_root <- getwd() 

dir_dados <- paste0(dir_root,"/DATA")
dir_dados_pre_processados <- paste0(dir_dados,"/pre_processados")
dir_plot <-  paste0(dir_root,"/PLOT")

##### Carregar data frames #####
df_alunos_2015_melhores_cidades <- 
  read_delim(file = paste0(dir_dados_pre_processados, 
                    "/df_alunos_2015_melhores_cidades.csv"), 
             delim = ";", 
             escape_double = FALSE, 
             trim_ws = TRUE)

df_alunos_2015_piores_cidades <- 
  read_delim(file = paste0(dir_dados_pre_processados, 
                           "/df_alunos_2015_piores_cidades.csv"), 
             delim = ";", 
             escape_double = FALSE, 
             trim_ws = TRUE)

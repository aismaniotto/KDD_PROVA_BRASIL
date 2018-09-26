##### Pacotes necessários #####
# manipulação dos dados
library(dplyr)
library(tidyr)
# importação de dados
library(readr)
# graficos
library(ggplot2)
# mineração de dados
library(party)
library(arules)
library(C50)
library(DMwR)

#### Diretorios #####
dir_root <- getwd() 

dir_dados <- paste0(dir_root,"/DATA")
dir_dados_pre_processados <- paste0(dir_dados,"/pre_processados")
dir_plot <-  paste0(dir_root,"/PLOT")
dir_plot_mineracao_dados <-  paste0(dir_plot,"/mineracao_dados")

# Cria diretorio para os gráficos, caso não exista
dir.create(dir_plot_mineracao_dados, recursive = TRUE)

##### Carregar data frames #####
df_alunos_2015_melhores_cidades <- 
  read.csv2(file = paste0(dir_dados_pre_processados, 
                          "/df_alunos_2015_melhores_cidades.csv"),
            sep = ";",
            stringsAsFactors = TRUE)

df_alunos_2015_piores_cidades <- 
  read.csv2(file = paste0(dir_dados_pre_processados, 
                          "/df_alunos_2015_piores_cidades.csv"),
            sep = ";",
            stringsAsFactors = TRUE)


# C5.0 (supervisionado) ========================================================
# Árvore de decisão


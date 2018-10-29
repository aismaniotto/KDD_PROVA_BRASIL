##### Pacotes necessários #####
# manipulação dos dados
library(dplyr)
library(tidyr)
# manipulação de texto
library(stringr)
# importação de dados
library(readr)
# utils.R
source("utils.R")

#### Diretorios #####
dir_root <- getwd() 

dir_dados <- paste0(dir_root,"/DATA")
dir_dados_pre_processados <- paste0(dir_dados,"/pre_processados")
dir_dados_minerados <- paste0(dir_dados,"/minerados")
dir_dados_visu_natural <- paste0(dir_dados_minerados,"/visualizacao_natural")

# Cria diretorio para os gráficos, caso não exista
dir.create(dir_dados_visu_natural, recursive = TRUE)

##### Carregar data frames #####
apriori_melhores_cidades <- 
  read.csv2(file = paste0(dir_dados_minerados, 
                          "/apriori_regras_melhores_cidades"))

apriori_piores_cidades <- 
  read.csv2(file = paste0(dir_dados_minerados, 
                          "/apriori_regras_piores_cidades"))

c50_melhores_cidades <- 
  read.csv2(file = paste0(dir_dados_minerados, 
                          "/C50_regras_melhores"))

c50_piores_piores <- 
  read.csv2(file = paste0(dir_dados_minerados, 
                          "/C50_regras_piores"))

df_indice_questoes <- 
  read.csv2(file = paste0(dir_dados_pre_processados, 
                          "/df_indice_questoes.csv"),
            sep = ";",
            strip.white = TRUE,
            na.strings = c("", " ", NA),
            stringsAsFactors = FALSE)

# Apriori: regras ==============================================================
# Melhores cidades
apriori_melhores_cidades_mod <- 
  apresentar_regras_apriori(apriori_melhores_cidades)

write.table(x = apriori_melhores_cidades_mod, 
            file = paste0(dir_dados_visu_natural, 
                          "/apriori_regras_melhores_cidades_natural"),
            row.names = FALSE,
            sep = ";")

# Piores cidades
apriori_piores_cidades_mod <- 
  apresentar_regras_apriori(apriori_piores_cidades)

write.table(x = apriori_piores_cidades_mod, 
            file = paste0(dir_dados_visu_natural, 
                          "/apriori_regras_piores_cidades_natural"),
            row.names = FALSE,
            sep = ";")

# Naives Bayes =================================================================
# Necessário ter carregado os data frames dos modelos naive bayes
## source("2_mineração_dados.R")
# Melhores ciedades
melhores_cidades_nb_model$tables

i <- 1
max <- length(melhores_cidades_nb_model$tables)

melhores_cidades_tabela_nv <- NULL

while(i <= max){

  a0 <- melhores_cidades_nb_model$tables[[i]]
  a1 <- cbind(a0, Proficiencia = rownames(a0))
  a2 <- as_data_frame(a1, stringsAsFactors = FALSE)
  
  a3 <- a2 %>% 
    mutate(Proficiencia = 
             factor(x = Proficiencia, 
                    levels = c("Insuficiente", "Basico", "Proficiente", "Avancado"),
                    labels = c("Insuficiente", "Basico", "Proficiente", "Avancado"))) %>% 
    arrange(Proficiencia)
  
  a4 <- a3 %>% 
    gather(Opcao, Porcentagem, -Proficiencia) %>% 
    mutate(Questao = NA) %>% 
    select(Questao, Opcao, Proficiencia, Porcentagem)
  
  opcoes_old <- a4 %>% 
    distinct(Opcao) %>% 
    filter(Opcao != "Nao Informado") %>%
    unlist() %>% 
    as.vector()
  
  opcoes_new <- troca_letra_por_opcao(idchar, opcoes_old)
  
  # TODO: Realizar troca das opções em letra pelas opções por extenso
  ## opcoes_old por opcoes_new
  
  a4[1,1] <- troca_idchar_por_enunciado(
    names(melhores_cidades_nb_model$tables[i]))
  
  melhores_cidades_tabela_nv <- bind_rows(melhores_cidades_tabela_nv, a4)
    
  i <- i + 1
}




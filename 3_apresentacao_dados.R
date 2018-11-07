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
                          "/apriori_regras_melhores_cidades.csv"))

apriori_piores_cidades <- 
  read.csv2(file = paste0(dir_dados_minerados, 
                          "/apriori_regras_piores_cidades.csv"))

c50_melhores_cidades <- 
  read.csv2(file = paste0(dir_dados_minerados, 
                          "/C50_regras_melhores.csv"))

c50_piores_piores <- 
  read.csv2(file = paste0(dir_dados_minerados, 
                          "/C50_regras_piores.csv"))

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
                          "/apriori_regras_melhores_cidades_natural.csv"),
            row.names = FALSE,
            sep = ";")

# Piores cidades
apriori_piores_cidades_mod <- 
  apresentar_regras_apriori(apriori_piores_cidades)

write.table(x = apriori_piores_cidades_mod, 
            file = paste0(dir_dados_visu_natural, 
                          "/apriori_regras_piores_cidades_natural.csv"),
            row.names = FALSE,
            sep = ";")

# Naives Bayes =================================================================
# Necessário ter carregado os data frames dos modelos naive bayes
## source("2_mineração_dados.R")
# Melhores ciedades

i <- 1
max <- length(melhores_cidades_nb_model$tables)

melhores_cidades_tabela_nv <- NULL

while(i <= max){

  # Pega a tabela
  a0 <- melhores_cidades_nb_model$tables[[i]]
  # Transforma o nome da linha em uma nova coluna
  ## Possui o valor de proficiencia
  a1 <- cbind(a0, Proficiencia = rownames(a0))
  # Converte para dataframe
  a2 <- as_data_frame(a1, stringsAsFactors = FALSE)
  # Transforma a coluna de proficiencia em factor para poder ordenar
  a3 <- a2 %>% 
    mutate(Proficiencia = 
             factor(x = Proficiencia, 
                    levels = c("IN", "BA", "PR", "AV"),
                    labels = c("Insuficiente", "Basico", "Proficiente", "Avançado"))) %>% 
    arrange(Proficiencia)
  # Muda a posição horientação do dataframe, de uma forma que fique bom analisar
  a4 <- a3 %>% 
    gather(Opcao, Porcentagem, -Proficiencia) %>% 
    mutate(Questao = NA) %>% 
    select(Questao, Opcao, Proficiencia, Porcentagem) %>% 
    arrange(Questao, Proficiencia, Opcao)
  
  # Reune as opções utilizadas nessa questão
  opcoes_old <- a4 %>% 
    distinct(Opcao) %>% 
    filter(Opcao != "NI") %>%
    unlist() %>% 
    as.vector()

  # Usa as opções em letra e pega a opção verbosa (opção completa)
  opcoes_new <- 
    troca_letra_por_opcao(names(melhores_cidades_nb_model$tables[i]), 
                          opcoes_old)
  
  # Na coluna de opções, troca a letra pela opção completa
  a5 <- a4
  a5$Opcao <- plyr::mapvalues(a4$Opcao, opcoes_old, opcoes_new)
  
  # Atribui o enunciado da questão à primeira coluna
  a5[,1] <- troca_idchar_por_enunciado(
    names(melhores_cidades_nb_model$tables[i]))
  
  # "Cola" esse dataframe no dataframe principal que vai reunir todos
  melhores_cidades_tabela_nv <- bind_rows(melhores_cidades_tabela_nv, a5)
    
  i <- i + 1
}

write.table(x = melhores_cidades_tabela_nv, 
            file = paste0(dir_dados_visu_natural, 
                          "/naive_bayes_melhores_cidades_natural.csv"),
            row.names = FALSE,
            sep = ";")

# Piores cidades
i <- 1
max <- length(piores_cidades_nb_model$tables)

piores_cidades_tabela_nv <- NULL

while(i <= max){
  
  # Pega a tabela
  a0 <- piores_cidades_nb_model$tables[[i]]
  # Transforma o nome da linha em uma nova coluna
  ## Possui o valor de proficiencia
  a1 <- cbind(a0, Proficiencia = rownames(a0))
  # Converte para dataframe
  a2 <- as_data_frame(a1, stringsAsFactors = FALSE)
  # Transforma a coluna de proficiencia em factor para poder ordenar
  a3 <- a2 %>% 
    mutate(Proficiencia = 
             factor(x = Proficiencia, 
                    levels = c("IN", "BA", "PR", "AV"),
                    labels = c("Insuficiente", "Basico", "Proficiente", "Avançado"))) %>% 
    arrange(Proficiencia)
  # Muda a posição horientação do dataframe, de uma forma que fique bom analisar
  a4 <- a3 %>% 
    gather(Opcao, Porcentagem, -Proficiencia) %>% 
    mutate(Questao = NA) %>% 
    select(Questao, Opcao, Proficiencia, Porcentagem) %>% 
    arrange(Questao, Proficiencia, Opcao)
  
  # Reune as opções utilizadas nessa questão
  opcoes_old <- a4 %>% 
    distinct(Opcao) %>% 
    filter(Opcao != "NI") %>%
    unlist() %>% 
    as.vector()
  
  # Usa as opções em letra e pega a opção verbosa (opção completa)
  opcoes_new <- 
    troca_letra_por_opcao(names(piores_cidades_nb_model$tables[i]), 
                          opcoes_old)
  
  # Na coluna de opções, troca a letra pela opção completa
  a5 <- a4
  a5$Opcao <- plyr::mapvalues(a4$Opcao, opcoes_old, opcoes_new)
  
  # Atribui o enunciado da questão à primeira coluna
  a5[,1] <- troca_idchar_por_enunciado(
    names(piores_cidades_nb_model$tables[i]))
  
  # "Cola" esse dataframe no dataframe principal que vai reunir todos
  piores_cidades_tabela_nv <- bind_rows(piores_cidades_tabela_nv, a5)
  
  i <- i + 1
}

write.table(x = piores_cidades_tabela_nv, 
            file = paste0(dir_dados_visu_natural, 
                          "/naive_bayes_piores_cidades_natural.csv"),
            row.names = FALSE,
            sep = ";")

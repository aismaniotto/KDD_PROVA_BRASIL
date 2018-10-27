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
library(e1071)

#### Diretorios #####
dir_root <- getwd() 

dir_dados <- paste0(dir_root,"/DATA")
dir_dados_pre_processados <- paste0(dir_dados,"/pre_processados")
dir_dados_minerados <- paste0(dir_dados,"/minerados")
dir_plot <-  paste0(dir_root,"/PLOT")
dir_plot_mineracao_dados <-  paste0(dir_plot,"/mineracao_dados")

# Cria diretorio, caso não exista
dir.create(dir_plot_mineracao_dados, recursive = TRUE)
dir.create(dir_dados_minerados, recursive = TRUE)

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


# C5.0 (Classificação) =========================================================
set.seed(500)
# Configuração para o algoritmo
## Utilizando poda
c5_0_config <- C5.0Control(noGlobalPruning = FALSE)

## Melhores cidades ------------------------------------------------------------
# Árvore de decisão
vars <- df_alunos_2015_melhores_cidades %>% 
  select(TX_RESP_Q013_alunos:TX_RESP_Q057_alunos) %>% 
  names

# Removendo entradas que prejudicam/confundem o algoritmo
## "TX_RESP_Q013_alunos" -> Na sua casa tem computador?
## "TX_RESP_Q026_alunos" -> Com qual frequência seus pais, ou responsáveis por 
### você, vão à reunião de pais?
## "TX_RESP_Q054_alunos" -> Você faz o dever de casa de Matemática?
vars <- vars[c(-1, -5, -15)]

tree_mod_melhores <- C5.0(x = df_alunos_2015_melhores_cidades[,vars],
                         y = df_alunos_2015_melhores_cidades$nivel_proficiencia,
                         control = c5_0_config)

summary(tree_mod_melhores)
jpeg(paste0(dir_plot_mineracao_dados, "/melhores_cidades_c50.jpeg"),
     quality = 100,
     width = 6000,
     height = 2500)
    
plot(tree_mod_melhores)
dev.off()

# Regras
rules_mod_melhores <- C5.0(x = df_alunos_2015_melhores_cidades[,vars],
                           y = df_alunos_2015_melhores_cidades$nivel_proficiencia,
                           control = c5_0_config,
                           rules = TRUE)
summary(rules_mod_melhores)
write(rules_mod_melhores$rules, file = paste0(dir_dados_minerados,
                                       "/C50_regras_melhores"))

## Piores cidades --------------------------------------------------------------
# Árvore de decisão
vars <- df_alunos_2015_piores_cidades %>% 
  select(TX_RESP_Q013_alunos:TX_RESP_Q057_alunos) %>% 
  names

# Removendo entrada que prejudica/confunde o algoritmo
## "TX_RESP_Q043_alunos" -> Em dias de aula, quanto tempo você gasta assistindo 
## à TV, navegando na internet ou jogando jogos eletrônicos?
vars <- vars[-10]

tree_mod_piores <- C5.0(x = df_alunos_2015_piores_cidades[,vars],
                        y = df_alunos_2015_piores_cidades$nivel_proficiencia, 
                        control = c5_0_config)

summary(tree_mod_piores)
jpeg(paste0(dir_plot_mineracao_dados, "/piores_cidades_c50.jpeg"), 
     quality = 100,
     width = 6000, 
     height = 2500)
plot(tree_mod_piores)
dev.off()

# Regras
rules_mod_piores <- C5.0(x = df_alunos_2015_piores_cidades[,vars],
                         y = df_alunos_2015_piores_cidades$nivel_proficiencia,
                         control = c5_0_config,
                         rules = TRUE)
summary(rules_mod_piores)
write(rules_mod_piores$rules, file = paste0(dir_dados_minerados,
                                            "/C50_regras_piores"))

# Apriori (Associação) =========================================================
#### Melhores cidades ----------------------------------------------------------
df_melhores_questionario_nivel_prof <- 
  df_alunos_2015_melhores_cidades %>% 
  select(TX_RESP_Q013_alunos:TX_RESP_Q057_alunos)

rules_melhores <- apriori(df_melhores_questionario_nivel_prof, 
                          parameter = list(minlen=2,
                                           supp = 0.6,
                                           conf = 0.8,
                                           target = "rules")
)  
df_rules_melhores <- as(rules_melhores, "data.frame")
df_rules_melhores2 <- 
  df_rules_melhores %>% 
  separate(col = rules, into = c("rules_LHS", "rules_RHS"), sep = " => ")

write.table(x =  df_rules_melhores, 
            file = paste0(dir_dados_minerados, 
                          "/apriori_regras_melhores_cidades"), 
            row.names = FALSE,
            sep = ";")

#### Piores cidades ------------------------------------------------------------
df_piores_questionario_nivel_prof <- 
  df_alunos_2015_piores_cidades %>% 
  select(TX_RESP_Q013_alunos:TX_RESP_Q057_alunos) 

rules_piores <- apriori(df_piores_questionario_nivel_prof,
                        parameter = list(minlen=2,
                                         supp = 0.5,
                                         conf = 0.8,
                                         target = "rules")
)   
df_rules_piores <- as(rules_piores, "data.frame")
df_rules_piores2 <- 
  df_rules_piores %>% 
  separate(col = rules, into = c("rules_LHS", "rules_RHS"), sep = " => ")

write.table(x = df_rules_piores, 
            file = paste0(dir_dados_minerados, 
                          "/apriori_regras_piores_cidades"),
            row.names = FALSE,
            sep = ";")

# Naives Bayes (Classificação) =================================================
## Melhores cidades ------------------------------------------------------------
df_alunos_2015_melhores_cidades_nb <- 
  df_alunos_2015_melhores_cidades %>% 
  select(nivel_proficiencia, TX_RESP_Q013_alunos:TX_RESP_Q057_alunos)

melhores_cidades_nb_model <- 
  naiveBayes(nivel_proficiencia ~ ., data = df_alunos_2015_melhores_cidades_nb)

melhores_cidades_NB_Predictions <- 
  predict(melhores_cidades_nb_model, 
          df_alunos_2015_melhores_cidades_nb)

melhores_cidades_confusion <- 
  table(melhores_cidades_NB_Predictions,
        df_alunos_2015_melhores_cidades_nb$nivel_proficiencia)

## Piores cidades --------------------------------------------------------------
df_alunos_2015_piores_cidades_nb <- 
  df_alunos_2015_piores_cidades %>% 
  select(nivel_proficiencia, TX_RESP_Q013_alunos:TX_RESP_Q057_alunos)

piores_cidades_nb_model <- 
  naiveBayes(nivel_proficiencia ~ ., data = df_alunos_2015_piores_cidades_nb)

piores_cidades_NB_Predictions <- 
  predict(piores_cidades_nb_model, 
          df_alunos_2015_piores_cidades_nb)

piores_cidades_confusion <- 
  table(piores_cidades_NB_Predictions,
        df_alunos_2015_piores_cidades_nb$nivel_proficiencia)


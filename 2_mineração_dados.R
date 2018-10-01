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
## Melhores cidades ------------------------------------------------------------
# Árvore de decisão
vars <- df_alunos_2015_melhores_cidades %>% 
  select(TX_RESP_Q013_alunos:TX_RESP_Q057_alunos) %>% 
  names

tree_mod_melhores <- C5.0(x = df_alunos_2015_melhores_cidades[,vars],
                         y = df_alunos_2015_melhores_cidades$nivel_proficiencia)

summary(tree_mod_melhores)
jpeg(paste0(dir_plot_mineracao_dados, "/melhores_cidades_c50.jpeg"),
     quality = 100,
     width = 32000,
     height = 2000)
    
plot(tree_mod_melhores)
dev.off()

# Regras
rules_mod_melhores <- C5.0(x = df_alunos_2015_melhores_cidades[,vars],
                           y = df_alunos_2015_melhores_cidades$nivel_proficiencia,
                           rules = TRUE)
summary(rules_mod_melhores)

## Piores cidades --------------------------------------------------------------
# Árvore de decisão
vars <- df_alunos_2015_piores_cidades %>% 
  select(TX_RESP_Q013_alunos:TX_RESP_Q057_alunos) %>% 
  names

tree_mod_piores <- C5.0(x = df_alunos_2015_piores_cidades[,vars],
                        y = df_alunos_2015_piores_cidades$nivel_proficiencia)

summary(tree_mod_piores)
jpeg(paste0(dir_plot_mineracao_dados, "/piores_cidades_c50.jpeg"), 
     quality = 100,
     width = 16000, 
     height = 1000)
plot(tree_mod_piores)
dev.off()

# Regras
rules_mod_piores <- C5.0(x = df_alunos_2015_piores_cidades[,vars],
                         y = df_alunos_2015_piores_cidades$nivel_proficiencia,
                         rules = TRUE)
summary(rules_mod_piores)

# Apriori (não-supervisionado) =================================================
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




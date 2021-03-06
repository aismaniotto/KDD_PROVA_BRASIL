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

#### Diretorios #####
dir_root <- getwd() 

dir_dados <- paste0(dir_root,"/DATA")
dir_dados_pre_processados <- paste0(dir_dados,"/pre_processados")
dir_plot <-  paste0(dir_root,"/PLOT")
dir_plot_analise_exploratoria <-  paste0(dir_plot,"/analise_exploratoria")

# Cria diretorio para os gráficos, caso não exista
dir.create(dir_plot_analise_exploratoria, recursive = TRUE)

##### Carregar data frames #####
# df_alunos_2015_melhores_cidades <-
#   read_delim(file = paste0(dir_dados_pre_processados,
#                            "/df_alunos_2015_melhores_cidades.csv"),
#              delim = ";",
#              escape_double = FALSE,
#              trim_ws = TRUE)
df_alunos_2015_melhores_cidades <- 
  read.csv2(file = paste0(dir_dados_pre_processados, 
                          "/df_alunos_2015_melhores_cidades.csv"),
            sep = ";",
            stringsAsFactors = TRUE)

# df_alunos_2015_piores_cidades <- 
#   read_delim(file = paste0(dir_dados_pre_processados, 
#                            "/df_alunos_2015_piores_cidades.csv"), 
#              delim = ";", 
#              escape_double = FALSE, 
#              trim_ws = TRUE)
df_alunos_2015_piores_cidades <- 
  read.csv2(file = paste0(dir_dados_pre_processados, 
                          "/df_alunos_2015_piores_cidades.csv"),
            sep = ";",
            stringsAsFactors = TRUE)

#### Exploração dos dados #####
# Sumarização de alguns atributos
df_alunos_2015_melhores_cidades %>% 
  select(nivel_proficiencia) %>% 
  group_by(nivel_proficiencia) %>% 
  count()

df_alunos_2015_piores_cidades %>% 
  select(nivel_proficiencia) %>% 
  group_by(nivel_proficiencia) %>% 
  count()

# Graficos de qtd de alunos por proficiencia
proficiencia_plot_melhores_cidades <- df_alunos_2015_melhores_cidades %>%
  mutate(nivel_proficiencia = 
           factor(x = nivel_proficiencia, 
                  levels = c("IN", "BA", "PR", "AV"),
                  labels = c("Insuficiente", "Basico", "Proficiente", "Avançado"))) %>% 
  arrange(nivel_proficiencia) %>% 
  ggplot(aes(x = factor(1), fill = nivel_proficiencia)) +
  geom_bar(width = 1) + 
  coord_polar(theta = "y") + 
  ylab("qtd_alunos") + 
  xlab("")

proficiencia_plot_piores_cidades <- df_alunos_2015_piores_cidades %>%
  mutate(nivel_proficiencia = 
           factor(x = nivel_proficiencia, 
                  levels = c("IN", "BA", "PR", "AV"),
                  labels = c("Insuficiente", "Basico", "Proficiente", "Avançado"))) %>% 
  arrange(nivel_proficiencia) %>% 
  ggplot(aes(x = factor(1), fill = nivel_proficiencia)) +
  geom_bar(width = 1) + 
  coord_polar(theta = "y") + 
  ylab("qtd_alunos") + 
  xlab("")

## Salvando o gráfico
jpeg(paste0(dir_plot_analise_exploratoria, 
            "/proficiencia_plot_melhores_cidades.jpeg"), 
     quality = 100,
     width = 800)
proficiencia_plot_melhores_cidades
dev.off()
jpeg(paste0(dir_plot_analise_exploratoria, 
            "/proficiencia_plot_piores_cidades.jpeg"), 
     quality = 100,
     width = 800)
proficiencia_plot_piores_cidades
dev.off()

# Associação (não supervisionado) ==============================================
## Regras de associação
### Apriori
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

# Classificação (supervisionado) ===============================================
## Árvores de inferência condicional
# nivel_proficiencia ou ind_aprendizado_adequado
formula <- nivel_proficiencia ~ TX_RESP_Q013_alunos + 
  TX_RESP_Q015_alunos + TX_RESP_Q019_alunos + TX_RESP_Q023_alunos +
  TX_RESP_Q026_alunos + TX_RESP_Q027_alunos + TX_RESP_Q028_alunos + 
  TX_RESP_Q029_alunos + TX_RESP_Q030_alunos + TX_RESP_Q043_alunos +
  TX_RESP_Q044_alunos + TX_RESP_Q045_alunos + TX_RESP_Q048_alunos + 
  TX_RESP_Q053_alunos + TX_RESP_Q054_alunos + TX_RESP_Q055_alunos + 
  TX_RESP_Q057_alunos

### Melhors cidades ------------------------------------------------------------
melhores_cidades_ctree <- ctree(formula = formula, 
                                data = df_alunos_2015_melhores_cidades)
jpeg(paste0(dir_plot_analise_exploratoria, "/melhores_cidades_plot_ctree.jpeg"), 
     quality = 100,
     width = 1350)
plot(melhores_cidades_ctree)
dev.off()

### Piores cidades -------------------------------------------------------------
piores_cidades_ctree <- ctree(formula = formula, 
                                data = df_alunos_2015_piores_cidades)
jpeg(paste0(dir_plot_analise_exploratoria, "/piores_cidades_plot_ctree.jpeg"), 
     quality = 100,
     width = 1000)
plot(piores_cidades_ctree)
dev.off()


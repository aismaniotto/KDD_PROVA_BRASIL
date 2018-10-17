##### Pacotes necessários #####
# manipulação dos dados
library(dplyr)
library(tidyr)
# manipulação de texto
library(stringr)
# importação de dados
library(readr)

#### Diretorios #####
dir_root <- getwd() 

dir_dados <- paste0(dir_root,"/DATA")
dir_dados_pre_processados <- paste0(dir_dados,"/pre_processados")
dir_dados_minerados <- paste0(dir_dados,"/minerados")

# Cria diretorio para os gráficos, caso não exista
dir.create(dir_dados_minerados, recursive = TRUE)

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

c50_melhores_piores <- 
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
## Separação das perguntas e respostas em colunas distintas
# Isso facilitará a transformação dos simbolos nas questões:
## Ou seja, o simbolico "TX_RESP_XX" no enunciado da questão e a opção "A", "B",
## "C", etc nas opções disponiveis na questão

### Separação básica das condições para o resultado
apriori_melhores_cidades2 <- 
  apriori_melhores_cidades %>% 
  mutate(rules = gsub("\\{|\\}", "",rules)) %>% 
  separate(rules, c("rules_a", "gera", "rules_b"), " ") %>% 
  mutate(id = row_number())

### Inicio dos trabalhos, começando pelas regras da esqueda
rules_a <- apriori_melhores_cidades2 %>% 
  select(rules_a, id)

#### Loop utilizado para saber quantas novas colunas serão criadas:
# Para os casos onde a regra é composta pela condição de mais de uma regra
i <- 1
max <- rules_a %>% nrow()
max_commas <- 0
while (i <= max) {
  
  commas <- str_count(rules_a$rules_a[[i]], ",")
  
  if (commas > max_commas) {
    max_commas <- commas
  }
  
  i <- i + 1
}

#### Definição fantasia dos nomes das novas colunas
novas_colunas <- c(1:(max_commas + 1)) %>% 
  paste0("rule")

#### Separação em novas colunas
rules_a2 <- 
  rules_a %>% 
  separate(rules_a, novas_colunas, ",")

##### Loop utilizado para colocar a questão em uma coluna e a resposta em outra
rules_a3 <- rules_a2
i <- 1
max <- novas_colunas %>% length()
while (i <= max) {
  
  rules_a3 <- 
    rules_a3 %>% 
    separate(!!novas_colunas[i], 
             c(paste0(novas_colunas[i], "_questao"),
               paste0(novas_colunas[i], "_resposta")),
             "=")
  
  i <- i + 1
}

###### Troca das opções categoricas nas opções "verbosas" da prova
# O sufixo "_alunos" é herança da seleção dos dados, onde, apesar de ter sido
# utilizado apenas os dados dos alunos, era possível utilizar outros dados 
# disponibilizados juntamente à esses, como por exemplo, os dados da escola
df_indice_questoes_aux <- 
  df_indice_questoes %>% 
  mutate(id_Char_aux = paste0(id_Char,"_alunos"))

df_indice_questoes_questao <- 
  df_indice_questoes_aux %>% 
  select(id_Char_aux, Enunciado)

df_indice_questoes_opcao <- 
  df_indice_questoes_aux %>%
  select(id_Char_aux, A:L) %>% 
  gather(key = opcao, value = verbosidade, -id_Char_aux) %>% 
  filter(!is.na(verbosidade)) %>% 
  arrange(id_Char_aux)

rules_a4 <- rules_a3
i <- 1
max <- novas_colunas %>% length()

while (i <= max) {
  
  col_questao <- paste0(novas_colunas[i], "_questao")
  col_resposta <- paste0(novas_colunas[i], "_resposta")
  
  rules_a4 <- 
    rules_a4 %>% 
    left_join(df_indice_questoes_opcao, 
              by = setNames(c('id_Char_aux', 'opcao'), 
                            c(col_questao, col_resposta)
              )
    ) %>% 
    left_join(df_indice_questoes_questao,
              by = setNames(c('id_Char_aux'), c(col_questao))
    ) %>%
    mutate(!!col_resposta := verbosidade, 
           !!col_questao := Enunciado) %>% 
    select(-verbosidade, -Enunciado)
  
  i <- i + 1
}





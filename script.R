##### Pacotes necessários #####
library(dplyr)
library(readr)

#### Diretorios #####
dir_root <- getwd() 

dir_dados <- paste0(dir_root,"/DATA")
dir_proeficiencia_estados <- paste0(dir_dados,"/proficiencia/estados")
dir_proeficiencia_cidades <- paste0(dir_dados,"/proficiencia/cidades")
dir_prova_brasil <- paste0(dir_dados,"/prova_brasil_2015")

##### Carregar data frames #####
# Todos os dados foram obtidos através da plataforma QEdu 
## Outros dados
df_municipios <- read_csv(paste0(dir_dados,"/municipios.csv"))
### Ajuste na coluna de nome do municipio, padronizando em caixa alta
df_municipios <- df_municipios %>% 
  mutate(Nome_Municipio = toupper(Nome_Municipio))
#### Criação do data frame de estados
df_estados <- df_municipios %>% 
  select(Cod_UF = UF, Nome_UF) %>% 
  distinct() 

## Dados sobre proeficiencia
df_proeficiencia_estados_qedu <- 
  read_csv(paste0(dir_proeficiencia_estados,
                  "/Aprendizado_nos_estados_do_Brasil_QEdu.csv"))

arr_df_proeficiencia_cidades_qedu <- lapply(df_estados$Nome_UF, 
                                            function(x) {
                                              # Carrega arquivo
                                              t <- read_csv(
                                                paste0(
                                                  dir_proeficiencia_cidades,
                                                  "/Aprendizado_nas_cidades_",
                                                  x,
                                                  "_QEdu.csv"))
                                              # Adiciona coluna com nome do estado
                                              t <- t %>% 
                                                mutate(Estado = x)
                                              return(t)
                                            }
)

## Dados prova brasil 2015
df_alunos_2015 <- 
  read_csv(paste0(dir_prova_brasil,"/2015_fonte_TS_ALUNO_9EF.csv"))
df_diretor_2015 <- 
  read_csv(paste0(dir_prova_brasil,"/2015_fonte_TS_DIRETOR.csv"))
df_escola_2015 <- 
  read_csv(paste0(dir_prova_brasil,"/2015_fonte_TS_ESCOLA.csv"))
df_professor_2015 <- 
  read_csv(paste0(dir_prova_brasil,"/2015_fonte_TS_PROFESSOR.csv"))

## Data frame temporario, utilizado devido ao desempenho computacional
df_alunos_2015_partial <- 
  read_csv(paste0(dir_prova_brasil,"/2015_fonte_TS_ALUNO_9EF.csv"),
            n_max = 100)

#### Preparação nos data frames #####
# Transformando a lista de data frames com a proeficiencia de cada cidade
# em um único data frame
df_proeficiencia_cidades_qedu <- do.call(rbind,
                                         arr_df_proeficiencia_cidades_qedu)
# removendo array do ambiente
rm(arr_df_proeficiencia_cidades_qedu)

# Adição do código do estado no df_proeficiencia_estados_qedu
df_proeficiencia_estados_qedu <- 
  df_proeficiencia_estados_qedu %>% 
  left_join(df_estados, by = c("Estado" = "Nome_UF"))

# Adição dos dados de cidade ao df_proeficiencia_cidades_qedu
df_proeficiencia_cidades_qedu <- 
  df_proeficiencia_cidades_qedu %>% 
  mutate(Cidade = toupper(Cidade)) %>% 
  left_join(df_municipios, by = c('Cidade' = 'Nome_Municipio',
                                  'Estado' = 'Nome_UF')) %>% 
  rename(cod_cidade = Municipio)


#### Renames data frames ####
# Carregar nomes em um vetor auxiliar
aux_proeficiencia_estados <- names(df_proeficiencia_estados_qedu)
aux_proeficiencia_cidades <- names(df_proeficiencia_cidades_qedu)
aux_names_alunos <- names(df_alunos_2015)
aux_names_professor <- names(df_professor_2015)
aux_names_diretor <- names(df_diretor_2015)
aux_names_escola <- names(df_escola_2015)

# Capitalizando todos os nomes
aux_proeficiencia_estados <- toupper(aux_proeficiencia_estados)
aux_proeficiencia_cidades <- toupper(aux_proeficiencia_cidades)

# Remoção de simbolos
aux_proeficiencia_estados <- gsub(" \\(.+\\)","",aux_proeficiencia_estados)
aux_proeficiencia_cidades <- gsub(" \\(.+\\)","",aux_proeficiencia_cidades)

# Remoção de acentos, sinais e espaços
aux_proeficiencia_estados <- chartr("ÉÇÃÁ. ","ECAA__",aux_proeficiencia_estados)
aux_proeficiencia_cidades <- chartr("ÉÇÃÁ. ","ECAA__",aux_proeficiencia_cidades)

# Adicionar respectivo suffixo
aux_names_alunos <- paste0(aux_names_alunos, '_alunos')
aux_names_professor <- paste0(aux_names_professor, '_professor')
aux_names_diretor <- paste0(aux_names_diretor, '_diretor')
aux_names_escola <- paste0(aux_names_escola, '_escola')

# Aplicando novos nomes das colunas
names(df_proeficiencia_estados_qedu) <- aux_proeficiencia_estados
names(df_proeficiencia_cidades_qedu) <- aux_proeficiencia_cidades
names(df_alunos_2015) <- aux_names_alunos
names(df_professor_2015) <- aux_names_professor
names(df_diretor_2015) <- aux_names_diretor
names(df_escola_2015) <- aux_names_escola
## temp
names(df_alunos_2015_partial) <- aux_names_alunos

# #### Montando fullDataSet ####
# fullDataFrame <- df_alunos_2015_partial %>% 
#   left_join(df_professor_2015,
#             by = c('ID_ESCOLA_alunos' = 'ID_ESCOLA_professor',
#                    'ID_TURMA_alunos' = 'ID_TURMA_professor',
#                    'ID_SERIE_alunos' = 'ID_SERIE_professor')) %>% 
#   # existem escolas sem diretor
#   left_join(df_diretor_2015,
#             by = c('ID_ESCOLA_alunos' = 'ID_ESCOLA_diretor')) %>% 
#   left_join(df_escola_2015, 
#             by = c('ID_ESCOLA_alunos' = 'ID_ESCOLA_escola'))

#### Limpeza dos dados ####
# Seleção apenas dos alunos que preencheram a prova
df_alunos_2015_prova_preenchida <- df_alunos_2015 %>% 
  filter(IN_PREENCHIMENTO_PROVA_alunos == 1)

#### TODO:  finalização da limpeza dos dados  ####

#### TODO: Ao final da limpeza de dados, atribuir por último a esse data frame
df_alunos_2015_clean <- df_alunos_2015_prova_preenchida

#### Filtro dos dados para os estados e cidades determinados #####
# Seleção dos estados a serem utilizados
## Os cinco com melhor taxa de aprendizado
melhores_estados <- df_proeficiencia_estados_qedu %>% 
  mutate(PERCENTUAL_APRENDIZADO_ADEQUADO = 
           as.numeric(PERCENTUAL_APRENDIZADO_ADEQUADO)) %>% 
  arrange(desc(PERCENTUAL_APRENDIZADO_ADEQUADO)) %>% 
  select(COD_UF, ESTADO, PERCENTUAL_APRENDIZADO_ADEQUADO) %>% 
  head(5)
## Os cinco com piores taxas de aprendizado
piores_estados <- df_proeficiencia_estados_qedu %>% 
  mutate(PERCENTUAL_APRENDIZADO_ADEQUADO = 
           as.numeric(PERCENTUAL_APRENDIZADO_ADEQUADO)) %>% 
  arrange(PERCENTUAL_APRENDIZADO_ADEQUADO) %>% 
  select(COD_UF, ESTADO, PERCENTUAL_APRENDIZADO_ADEQUADO) %>% 
  head(5)

### As dez melhores cidades dos 5 melhores estados
melhores_cidades <- df_proeficiencia_cidades_qedu %>% 
  filter(ESTADO %in% melhores_estados$ESTADO) %>% 
  mutate_at(vars(PERCENTUAL_APRENDIZADO_ADEQUADO,
                 PERCENTUAL_INSUFICIENTE,
                 PERCENTUAL_BASICO,
                 PERCENTUAL_PROFICIENTE,
                 PERCENTUAL_AVANCADO),
            as.numeric) %>%
  arrange(desc(PERCENTUAL_APRENDIZADO_ADEQUADO),
          desc(PERCENTUAL_AVANCADO),
          desc(PERCENTUAL_PROFICIENTE),
          desc(PERCENTUAL_BASICO),
          desc(PERCENTUAL_INSUFICIENTE))%>% 
  select(COD_MUNICIPIO_COMPLETO,
         CIDADE,
         ESTADO,
         PERCENTUAL_APRENDIZADO_ADEQUADO) %>% 
  head(10)

### As dez piores cidades dos 5 piores estados
# Foi utilizado mais de um critério no arrange para desempate.
piores_cidades <- df_proeficiencia_cidades_qedu %>%
  filter(ESTADO %in% piores_estados$ESTADO) %>%
  mutate_at(vars(PERCENTUAL_APRENDIZADO_ADEQUADO,
                 PERCENTUAL_INSUFICIENTE,
                 PERCENTUAL_BASICO,
                 PERCENTUAL_PROFICIENTE,
                 PERCENTUAL_AVANCADO),
            as.numeric) %>%
  arrange(PERCENTUAL_APRENDIZADO_ADEQUADO,
          PERCENTUAL_INSUFICIENTE,
          PERCENTUAL_BASICO,
          PERCENTUAL_PROFICIENTE,
          PERCENTUAL_AVANCADO) %>%
  select(COD_MUNICIPIO_COMPLETO,
         CIDADE,
         ESTADO,
         PERCENTUAL_APRENDIZADO_ADEQUADO) %>%
  head(10)

# Criando data frames com os dados respectivos ao filtro cidades
df_alunos_2015_melhores_cidades <- 
  df_alunos_2015_clean %>% 
  filter(ID_UF_alunos %in% melhores_cidades$COD_MUNICIPIO_COMPLETO)

df_alunos_2015_piores_cidades <- 
  df_alunos_2015_clean %>% 
  filter(ID_UF_alunos %in% piores_cidades$COD_MUNICIPIO_COMPLETO)

  
  

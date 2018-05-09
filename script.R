##### Pacotes necessários #####
library(dplyr)

#### Diretorios #####
dir_root <- getwd() 

dir_dados <- paste0(dir_root,"/DATA")
dir_proeficiencia_estados <- paste0(dir_dados,"/proficiencia/estados")
dir_proeficiencia_cidades <- paste0(dir_dados,"/proficiencia/cidades")

##### Carregar data frames #####
# Todos os dados foram obtidos através da plataforma QEdu 
## Outros dados
df_municipios <- read.csv(paste0(dir_dados,"/municipios.csv"), 
                          stringsAsFactors = FALSE)
df_estados <- df_municipios %>% 
  select(UF, Nome_UF) %>% 
  distinct() 

## Dados sobre proeficiencia
df_proeficiencia_estados_qedu <- 
  read.csv(paste0(dir_proeficiencia_estados,
                  "/Aprendizado_nos_estados_do_Brasil_QEdu.csv"),
           stringsAsFactors = FALSE)

arr_df_proeficiencia_cidades_qedu <- lapply(df_estados$Nome_UF, 
                                            function(x) {
                                              # Carrega arquivo
                                              t <- read.csv(
                                                paste0(
                                                  dir_proeficiencia_cidades,
                                                  "/Aprendizado_nas_cidades_",
                                                  x,
                                                  "_QEdu.csv"),
                                                stringsAsFactors = FALSE)
                                              # Adiciona coluna com nome do estado
                                              t <- t %>% 
                                                mutate(Estado = x)
                                              return(t)
                                            }
)

## Dados prova brasil 2015
df_alunos_2015 <- 
  read.csv(paste0(dir_dados,"/prova_brasil_2015/2015_fonte_TS_ALUNO_9EF.csv"),
           stringsAsFactors = FALSE)
df_diretor_2015 <- 
  read.csv(paste0(dir_dados,"/prova_brasil_2015/2015_fonte_TS_DIRETOR.csv"),
           stringsAsFactors = FALSE)
df_escola_2015 <- 
  read.csv(paste0(dir_dados,"/prova_brasil_2015/2015_fonte_TS_ESCOLA.csv"),
           stringsAsFactors = FALSE)
df_professor_2015 <- 
  read.csv(paste0(dir_dados,"/prova_brasil_2015/2015_fonte_TS_PROFESSOR.csv"),
           stringsAsFactors = FALSE)

## Data frame temporario, utilizado devido ao desempenho computacional
df_alunos_2015_partial <- 
  read.csv(paste0(dir_dados,"/prova_brasil_2015/2015_fonte_TS_ALUNO_9EF.csv"),
           nrows = 100,
           stringsAsFactors = FALSE)

#### Preparação nos data frames #####
# Transformando a lista de data frames com a proeficiencia de cada cidade
# em um único data frame
df_proeficiencia_cidades_qedu <- do.call(rbind,
                                         arr_df_proeficiencia_cidades_qedu)
# removendo array do ambiente
rm(arr_df_proeficiencia_cidades_qedu)

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

# Remoção de extra pontos
aux_proeficiencia_estados <- gsub("\\.+",".",aux_proeficiencia_estados)
aux_proeficiencia_cidades <- gsub("\\.+",".",aux_proeficiencia_cidades)

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

#### Montando fullDataSet ####
fullDataFrame <- df_alunos_2015_partial %>% 
  left_join(df_professor_2015,
            by = c('ID_ESCOLA_alunos' = 'ID_ESCOLA_professor',
                   'ID_TURMA_alunos' = 'ID_TURMA_professor',
                   'ID_SERIE_alunos' = 'ID_SERIE_professor')) %>% 
  # existem escolas sem diretor
  left_join(df_diretor_2015,
            by = c('ID_ESCOLA_alunos' = 'ID_ESCOLA_diretor')) %>% 
  left_join(df_escola_2015, 
            by = c('ID_ESCOLA_alunos' = 'ID_ESCOLA_escola'))

#### Seleção das variaveis (Redução Vertical) ####
#### TODO

#### Filtro dos dados (Redução Horizontal) #####
# Seleção dos estados a serem utilizados
## Os cinco com melhor taxa de aprendizado
melhores_estados_5 <- df_proeficiencia_estados_qedu %>% 
  mutate(PERCENTUAL_APRENDIZADO_ADEQUADO_ = 
           as.numeric(PERCENTUAL_APRENDIZADO_ADEQUADO_)) %>% 
  arrange(desc(PERCENTUAL_APRENDIZADO_ADEQUADO_)) %>% 
  select(ESTADO, PERCENTUAL_APRENDIZADO_ADEQUADO_) %>% 
  head(5)
## Os quatro com piores taxas de aprendizado
piores_estados_4 <- df_proeficiencia_estados_qedu %>% 
  mutate(PERCENTUAL_APRENDIZADO_ADEQUADO_ = 
           as.numeric(PERCENTUAL_APRENDIZADO_ADEQUADO_)) %>% 
  arrange(PERCENTUAL_APRENDIZADO_ADEQUADO_) %>% 
  select(ESTADO, PERCENTUAL_APRENDIZADO_ADEQUADO_) %>% 
  head(5)

# Criando data frames com os dados respectivos ao filtro de estados
  
  
  
  

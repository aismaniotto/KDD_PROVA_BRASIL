##### Pacotes necessários #####
library(dplyr)
library(readr)
library(ggplot2)

#### Diretorios #####
dir_root <- getwd()

dir_dados <- paste0(dir_root, "/DATA")
dir_dados_pre_processados <- paste0(dir_dados, "/pre_processados")
dir_plot <-  paste0(dir_root, "/PLOT")
dir_proeficiencia_estados <- paste0(dir_dados, "/proficiencia/estados")
dir_proeficiencia_cidades <- paste0(dir_dados, "/proficiencia/cidades")
dir_prova_brasil <- paste0(dir_dados, "/prova_brasil_2015")

# Cria diretorio para os gráficos, caso não exista
dir.create(dir_plot, recursive = TRUE)
dir.create(dir_dados_pre_processados, recursive = TRUE)

##### Carregar data frames #####
# Todos os dados foram obtidos através da plataforma QEdu 
## Outros dados
df_municipios <- read_csv(paste0(dir_dados, "/municipios.csv"))
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
  read_csv(paste0(dir_prova_brasil, "/TS_ALUNO_9EF.csv"))
# df_diretor_2015 <- 
#   read_csv(paste0(dir_prova_brasil,"/TS_DIRETOR.csv"))
# df_escola_2015 <- 
#   read_csv(paste0(dir_prova_brasil,"/TS_ESCOLA.csv"))
# df_professor_2015 <- 
#   read_csv(paste0(dir_prova_brasil,"/TS_PROFESSOR.csv"))

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
  left_join(df_municipios, by = c("Cidade" = "Nome_Municipio",
                                  "Estado" = "Nome_UF")) %>% 
  rename(cod_cidade = Municipio)


#### Renames data frames ####
# Carregar nomes em um vetor auxiliar
aux_proeficiencia_estados <- names(df_proeficiencia_estados_qedu)
aux_proeficiencia_cidades <- names(df_proeficiencia_cidades_qedu)
aux_names_alunos <- names(df_alunos_2015)
# aux_names_professor <- names(df_professor_2015)
# aux_names_diretor <- names(df_diretor_2015)
# aux_names_escola <- names(df_escola_2015)

# Capitalizando todos os nomes
aux_proeficiencia_estados <- toupper(aux_proeficiencia_estados)
aux_proeficiencia_cidades <- toupper(aux_proeficiencia_cidades)

# Remoção de simbolos
aux_proeficiencia_estados <- gsub(" \\(.+\\)", "", aux_proeficiencia_estados)
aux_proeficiencia_cidades <- gsub(" \\(.+\\)", "", aux_proeficiencia_cidades)

# Remoção de acentos, sinais e espaços
aux_proeficiencia_estados <- chartr("ÉÇÃÁ. ", 
                                    "ECAA__", 
                                    aux_proeficiencia_estados)
aux_proeficiencia_cidades <- chartr("ÉÇÃÁ. ", 
                                    "ECAA__", 
                                    aux_proeficiencia_cidades)

# Adicionar respectivo suffixo
aux_names_alunos <- paste0(aux_names_alunos, "_alunos")
# aux_names_professor <- paste0(aux_names_professor, "_professor")
# aux_names_diretor <- paste0(aux_names_diretor, "_diretor")
# aux_names_escola <- paste0(aux_names_escola, "_escola")

# Aplicando novos nomes das colunas
names(df_proeficiencia_estados_qedu) <- aux_proeficiencia_estados
names(df_proeficiencia_cidades_qedu) <- aux_proeficiencia_cidades
names(df_alunos_2015) <- aux_names_alunos
# names(df_professor_2015) <- aux_names_professor
# names(df_diretor_2015) <- aux_names_diretor
# names(df_escola_2015) <- aux_names_escola

#### Limpeza dos dados: dados Proficiência ####
# Transformação dos dados para o tipo numeric
df_proeficiencia_estados_qedu_clean <-
  df_proeficiencia_estados_qedu %>% 
  mutate_at(vars(PERCENTUAL_APRENDIZADO_ADEQUADO,
                 PERCENTUAL_INSUFICIENTE,
                 PERCENTUAL_BASICO,
                 PERCENTUAL_PROFICIENTE,
                 PERCENTUAL_AVANCADO),
            as.numeric)
  
# Transformação dos dados para o tipo numeric
df_proeficiencia_cidades_qedu_clean <-
  df_proeficiencia_cidades_qedu %>% 
  mutate_at(vars(PERCENTUAL_APRENDIZADO_ADEQUADO,
                 PERCENTUAL_INSUFICIENTE,
                 PERCENTUAL_BASICO,
                 PERCENTUAL_PROFICIENTE,
                 PERCENTUAL_AVANCADO),
            as.numeric) 

#### Limpeza dos dados: dados Prova Brasil ####
# Seleção apenas dos alunos que preencheram a prova
df_alunos_2015_filter <- df_alunos_2015 %>% 
  filter(IN_PREENCHIMENTO_PROVA_alunos == 1,
         IN_PREENCHIMENTO_QUESTIONARIO_alunos == 1,
         IN_PROFICIENCIA_alunos == 1,
         IN_PROVA_BRASIL_alunos == 1) %>% 
  select(-IN_PREENCHIMENTO_PROVA_alunos,
         -IN_PREENCHIMENTO_QUESTIONARIO_alunos,
         -IN_PROFICIENCIA_alunos,
         -IN_PROVA_BRASIL_alunos)

# Seleção das questões do questionário conceitual que serão trabalhadas
df_alunos_2015_select1 <- df_alunos_2015_filter %>% 
  select(ID_PROVA_BRASIL_alunos:DESVIO_PADRAO_MT_SAEB_alunos,
         # Na sua casa tem computador?
         TX_RESP_Q013_alunos,
         # Na sua casa tem quartos para dormir?
         TX_RESP_Q015_alunos,
         # Até que série sua mãe, ou a mulher responsável por você, estudou?
         TX_RESP_Q019_alunos,
         # Até que série seu pai, ou o homem responsável por você, estudou?
         TX_RESP_Q023_alunos,
         # Com qual frequência seus pais, ou responsáveis por você, vão à 
         # reunião de pais?
         TX_RESP_Q026_alunos,
         # Seus pais ou responsáveis incentivam você a estudar?
         TX_RESP_Q027_alunos,
         # Seus pais ou responsáveis incentivam você a fazer o dever de casa 
         # e/ou os trabalhos da escola?
         TX_RESP_Q028_alunos,
         # Seus pais ou responsáveis incentivam você a ler?
         TX_RESP_Q029_alunos,
         # Seus pais ou responsáveis incentivam você a ir a escola e/ou não 
         # faltar às aulas?
         TX_RESP_Q030_alunos,
         # Em dia de aula, quanto tempo você gasta assistindo à TV, navegando 
         # na internet ou jogando jogos eletrônicos?
         TX_RESP_Q043_alunos,
         # Em dias de aula, quanto tempo você gasta fazendo trabalhos 
         # domésticos (ex.: lavando louça, limpando o quintal etc.)
         TX_RESP_Q044_alunos,
         # Atualmente você trabalha fora de casa (recebendo ou não salário)?
         # TX_RESP_Q045_alunos,
         # Você já foi reprovado?
         TX_RESP_Q048_alunos,
         # Você gosta de estudar Matemática?
         TX_RESP_Q053_alunos,
         # Você faz o dever de casa de Matemática?
         TX_RESP_Q054_alunos,
         # O(A) professor(a) corrige o dever de casa de Matemática?
         TX_RESP_Q055_alunos,
         # Quando você terminar o 9o ano(8a série), o que você pretende fazer?
         TX_RESP_Q057_alunos)

# Remoção das colunas que não serão utilizadas
df_alunos_2015_select2 <- df_alunos_2015_select1 %>% 
  select(-ID_PROVA_BRASIL_alunos,
         -ID_REGIAO_alunos,
         -ID_AREA_alunos,
         -ID_ESCOLA_alunos,
         -ID_DEPENDENCIA_ADM_alunos,
         -ID_LOCALIZACAO_alunos,
         -ID_TURMA_alunos,
         -ID_TURNO_alunos,
         -ID_SERIE_alunos,
         # -ID_ALUNO_alunos,
         -IN_SITUACAO_CENSO_alunos,
         -ID_CADERNO_alunos,
         -ID_BLOCO_1_alunos,
         -ID_BLOCO_2_alunos,
         -TX_RESP_BLOCO_1_LP_alunos,
         -TX_RESP_BLOCO_1_MT_alunos,
         -TX_RESP_BLOCO_2_LP_alunos,
         -TX_RESP_BLOCO_2_MT_alunos,
         -ESTRATO_ANEB_alunos,
         -PESO_ALUNO_LP_alunos,
         -PESO_ALUNO_MT_alunos,
         -PROFICIENCIA_LP_alunos,
         -DESVIO_PADRAO_LP_alunos,
         -PROFICIENCIA_LP_SAEB_alunos,
         -DESVIO_PADRAO_LP_SAEB_alunos,
         -PROFICIENCIA_MT_alunos,
         -DESVIO_PADRAO_MT_alunos,
         -DESVIO_PADRAO_MT_SAEB_alunos)

## Preenchimento das respostas não preenchidas (NA) por "NA"
df_alunos_2015_clean <- df_alunos_2015_select2 %>% 
  mutate_at(vars(TX_RESP_Q013_alunos:TX_RESP_Q057_alunos),
            funs(if_else(is.na(.),"Não Informado",.)))

#### Filtro dos dados para os estados e cidades determinados #####
# Gráfico de proficiência dos estados
proficiencia_plot_estado <- df_proeficiencia_estados_qedu_clean %>%   
  ggplot(aes(reorder(ESTADO, PERCENTUAL_APRENDIZADO_ADEQUADO),
             PERCENTUAL_APRENDIZADO_ADEQUADO)) + 
  geom_bar(stat = "identity") + 
  coord_flip()
## Salvando o gráfico
jpeg(paste0(dir_plot, "/proficiencia_plot_estado.jpeg"), 
     quality = 100,
     width = 800)
proficiencia_plot_estado
dev.off()

# Seleção dos estados a serem utilizados
## Os cinco com melhor taxa de aprendizado
melhores_estados <- df_proeficiencia_estados_qedu %>% 
  arrange(desc(PERCENTUAL_APRENDIZADO_ADEQUADO)) %>% 
  select(COD_UF, ESTADO, PERCENTUAL_APRENDIZADO_ADEQUADO) %>% 
  head(5)
## Os cinco com piores taxas de aprendizado
piores_estados <- df_proeficiencia_estados_qedu %>% 
  arrange(PERCENTUAL_APRENDIZADO_ADEQUADO) %>% 
  select(COD_UF, ESTADO, PERCENTUAL_APRENDIZADO_ADEQUADO) %>% 
  head(5)

### As dez melhores cidades dos 5 melhores estados
melhores_cidades <- df_proeficiencia_cidades_qedu_clean %>% 
  filter(ESTADO %in% melhores_estados$ESTADO) %>% 
  arrange(desc(PERCENTUAL_APRENDIZADO_ADEQUADO),
          desc(PERCENTUAL_AVANCADO),
          desc(PERCENTUAL_PROFICIENTE),
          desc(PERCENTUAL_BASICO),
          desc(PERCENTUAL_INSUFICIENTE)) %>% 
  select(COD_MUNICIPIO_COMPLETO,
         CIDADE,
         ESTADO,
         PERCENTUAL_APRENDIZADO_ADEQUADO,
         PERCENTUAL_INSUFICIENTE,
         PERCENTUAL_BASICO,
         PERCENTUAL_PROFICIENTE,
         PERCENTUAL_AVANCADO) %>% 
  head(10)

### As dez piores cidades dos 5 piores estados
# Foi utilizado mais de um critério no arrange para desempate.
piores_cidades <- df_proeficiencia_cidades_qedu_clean %>%
  filter(ESTADO %in% piores_estados$ESTADO) %>%
  arrange(PERCENTUAL_APRENDIZADO_ADEQUADO,
          PERCENTUAL_INSUFICIENTE,
          PERCENTUAL_BASICO,
          PERCENTUAL_PROFICIENTE,
          PERCENTUAL_AVANCADO) %>%
  select(COD_MUNICIPIO_COMPLETO,
         CIDADE,
         ESTADO,
         PERCENTUAL_APRENDIZADO_ADEQUADO,
         PERCENTUAL_INSUFICIENTE,
         PERCENTUAL_BASICO,
         PERCENTUAL_PROFICIENTE,
         PERCENTUAL_AVANCADO) %>%
  head(10)

# Criando data frames com os dados respectivos ao filtro cidades
df_alunos_2015_melhores_cidades <- 
  df_alunos_2015_clean %>%
  filter(ID_MUNICIPIO_alunos %in% melhores_cidades$COD_MUNICIPIO_COMPLETO)

df_alunos_2015_piores_cidades <- 
  df_alunos_2015_clean %>% 
  filter(ID_MUNICIPIO_alunos %in% piores_cidades$COD_MUNICIPIO_COMPLETO)

#### Transformação de dados ####
# Criação das colunas indicando o nível de proficiência do estudante
## Nível proficiente 
### Insuficiente, Básico, Proficiente ou Avançado
#### Insuficiente: 0 a 224 pontos
#### Básico: 225 a 299 pontos
#### Proficiente: 300 a 349 pontos
#### Avançado: Igual ou maior que 350
## Aprendizado adequado 
### Sim = Nível proficiente ou avançado; Não = Nível básico ou insuficiente
df_alunos_2015_melhores_cidades <- 
  df_alunos_2015_melhores_cidades %>% 
  mutate(nivel_proficiencia = 
           case_when(
             PROFICIENCIA_MT_SAEB_alunos < 225 ~ "Insuficiente",
             PROFICIENCIA_MT_SAEB_alunos >= 225 &
               PROFICIENCIA_MT_SAEB_alunos < 300 ~ "Básico",
             PROFICIENCIA_MT_SAEB_alunos >= 300 &
               PROFICIENCIA_MT_SAEB_alunos < 350 ~ "Proficiente",
             PROFICIENCIA_MT_SAEB_alunos >= 350  ~ "Avançado"
           ),
         ind_aprendizado_adequado = 
           case_when(
             nivel_proficiencia == "Insuficiente" | 
               nivel_proficiencia == "Básico" ~ "Não",
             nivel_proficiencia == "Proficiente" | 
               nivel_proficiencia == "Avançado" ~ "Sim"
           )
         )

df_alunos_2015_piores_cidades <- 
  df_alunos_2015_piores_cidades %>% 
  mutate(nivel_proficiencia = 
           case_when(
             PROFICIENCIA_MT_SAEB_alunos < 225 ~ "Insuficiente",
             PROFICIENCIA_MT_SAEB_alunos >= 225 &
               PROFICIENCIA_MT_SAEB_alunos < 300 ~ "Básico",
             PROFICIENCIA_MT_SAEB_alunos >= 300 &
               PROFICIENCIA_MT_SAEB_alunos < 350 ~ "Proficiente",
             PROFICIENCIA_MT_SAEB_alunos >= 350  ~ "Avançado"
           ),
         ind_aprendizado_adequado = 
           case_when(
             nivel_proficiencia == "Insuficiente" | 
               nivel_proficiencia == "Básico" ~ "Não",
             nivel_proficiencia == "Proficiente" | 
               nivel_proficiencia == "Avançado" ~ "Sim"
           )
  )

# Exportando dados pré-processados
write.table(x = df_alunos_2015_melhores_cidades,
            file = paste0(dir_dados_pre_processados,
                   "/df_alunos_2015_melhores_cidades.csv"),
            row.names = FALSE,
            sep = ";")
write.table(x = df_alunos_2015_piores_cidades,
            file = paste0(dir_dados_pre_processados,
                   "/df_alunos_2015_piores_cidades.csv"),
            row.names = FALSE,
            sep = ";")
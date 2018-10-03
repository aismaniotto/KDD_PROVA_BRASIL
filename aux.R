#### Verificação da divergência entre quantidade de diretores e de escolas ####
# Verifica se existem id_escola repetido na df_escola_2015
# 0 linhas
df_escola_2015$ID_ESCOLA_escola[duplicated(df_escola_2015$ID_ESCOLA_escola)]

# Verificar se existe alguma escola sem diretor
## 2051 linhas
### mesmo resultado da diferença entre as duas tabelas
#### existem 2051 escolas sem diretor
df_escola_2015 %>% 
  anti_join(df_diretor_2015,
            by = c('ID_ESCOLA_escola' = 'ID_ESCOLA_diretor'))

# Verifica se existe algum diretor em duas escolas
## 0 linhas
df_diretor_2015$ID_ESCOLA_diretor[duplicated(df_diretor_2015$ID_ESCOLA_diretor)]

#### Criação de indice para questões ####
# Criado inicialmente de forma manual a partir do dicionário do ANEB
df_indice_questoes <- 
  read.csv2(file = paste0(dir_dados_pre_processados, 
                          "/df_questoes_aux.csv"),
            sep = ";",
            strip.white = TRUE,
            na.strings = c("", " ", NA),
            stringsAsFactors = FALSE)

df_indice_questoes$qtd_opcoes <- 
  rowSums(
    !is.na(
      df_indice_questoes[
        c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")]
      )
    )

df_indice_questoes <- 
  df_indice_questoes %>% 
  mutate(id_Questao = gsub("Questão ","",id_Questao),
         id_Questao = as.numeric(id_Questao))

# df_indice_questoes %>% str()

write.table(x = df_indice_questoes,
            file = paste0(dir_dados_pre_processados,
                          "/df_indice_questoes.csv"),
            row.names = FALSE,
            sep = ";")





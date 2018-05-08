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
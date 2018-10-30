apresentar_regras_apriori <- function(df_regras_apriori) {
  ## Separação das perguntas e respostas em colunas distintas
  # Isso facilitará a transformação dos simbolos nas questões:
  ## Ou seja, o simbolico "TX_RESP_XX" no enunciado da questão e a opção "A", "B",
  ## "C", etc nas opções disponiveis na questão
  
  ### Separação básica das condições para o resultado
  df_regras_apriori2 <- 
    df_regras_apriori %>% 
    mutate(rules = gsub("\\{|\\}", "",rules)) %>% 
    separate(rules, c("rules_a", "gera", "rules_b"), " ")
  # %>% 
  #   mutate(id = rownames())
  df_regras_apriori2$id <- seq.int(nrow(df_regras_apriori2))
  
  ### Transformação dos dados "simbolos" para o estado natural deles
  # Regras da esquerda
  rules_a <- df_regras_apriori2 %>% 
    select(rules = rules_a, id)
  rules_a_mod <- adequar_coluna_regras(rules_a) 
  
  # Regras da direita (resultado)
  rules_b <- df_regras_apriori2 %>% 
    select(rules = rules_b, id)
  rules_b_mod <- adequar_coluna_regras(rules_b) 
  
  df_regras_apriori3 <- 
    df_regras_apriori2 %>% 
    left_join(rules_a_mod, by = "id") %>% 
    mutate(rules_a = rules) %>% 
    select(-rules) %>% 
    left_join(rules_b_mod, by = "id") %>% 
    mutate(rules_b = rules) %>% 
    select(-rules, -id)
  
  return(df_regras_apriori3)
}


adequar_coluna_regras <- function(df_col_regras) {
  
  #### Loop utilizado para saber quantas novas colunas serão criadas:
  # Para os casos onde a regra é composta pela condição de mais de uma regra
  i <- 1
  max <- df_col_regras %>% nrow()
  max_commas <- 0
  while (i <= max) {
    
    commas <- str_count(df_col_regras$rules[[i]], ",")
    
    if (commas > max_commas) {
      max_commas <- commas
    }
    
    i <- i + 1
  }
  
  #### Definição fantasia dos nomes das novas colunas
  novas_colunas <- c(1:(max_commas + 1)) %>% 
    paste0("rule")
  col_questao <- paste0(novas_colunas, "_questao")
  col_resposta <- paste0(novas_colunas, "_resposta")
  
  #### Separação em novas colunas
  df_col_regras2 <- 
    df_col_regras %>% 
    separate(rules, novas_colunas, ",")
  
  ##### Loop utilizado para colocar a questão em uma coluna e a resposta em outra
  df_col_regras3 <- df_col_regras2
  i <- 1
  max <- novas_colunas %>% length()
  while (i <= max) {
    
    df_col_regras3 <- 
      df_col_regras3 %>% 
      separate(!!novas_colunas[i], 
               c(col_questao[i], col_resposta[i]), "=")
    
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
  
  df_col_regras4 <- df_col_regras3
  i <- 1
  max <- novas_colunas %>% length()
  
  while (i <= max) {
    
    df_col_regras4 <- 
      df_col_regras4 %>% 
      left_join(df_indice_questoes_opcao, 
                by = setNames(c('id_Char_aux', 'opcao'), 
                              c(col_questao[i], col_resposta[i])
                )
      ) %>% 
      left_join(df_indice_questoes_questao,
                by = setNames(c('id_Char_aux'), c(col_questao[i]))
      ) %>%
      mutate(!!col_resposta[i] := verbosidade, 
             !!col_questao[i] := Enunciado) %>% 
      select(-verbosidade, -Enunciado)
    
    i <- i + 1
  }
  
  ##### Iniciar reunião das colunas, agora com os dados renomeados
  ## Unindo questão e resposta
  df_col_regras5 <- df_col_regras4
  i <- 1
  max <- novas_colunas %>% length()
  while (i <= max) {
    
    df_col_regras5 <- 
      df_col_regras5 %>% 
      unite(!!novas_colunas[i], 
            !!col_questao[i], 
            !!col_resposta[i], 
            sep = "=")
    
    i <- i + 1
  }
  
  ## Unindo as regras
  df_col_regras6 <- 
    df_col_regras5 %>% 
    mutate_at(
      vars(novas_colunas),
      funs(paste0("{",.,"}"))
    ) %>% 
    unite(rules, !!novas_colunas, sep = ",") %>% 
    mutate(rules, rules = gsub(",\\{NA=NA\\}", "", rules))
  
  return(df_col_regras6)
}

troca_idchar_por_enunciado <- function(idchar) {
  df_indice_questoes_mod <- df_indice_questoes %>% 
    mutate(id_Char = paste0(id_Char, "_alunos"),
           Enunciado = paste0(id_Questao, " - ", Enunciado))
  
  enunciado <- df_indice_questoes_mod$Enunciado[
    df_indice_questoes_mod$id_Char == idchar]
  
  enunciado <- as.character(enunciado)
  
  return(enunciado)
}

troca_letra_por_opcao <- function(idchar, opcoes) {
  df_indice_questoes_mod <- df_indice_questoes %>% 
    mutate(id_Char = paste0(id_Char, "_alunos"),
           Enunciado = paste0(id_Questao, " - ", Enunciado))
  
  i <- 1
  max <- length(opcoes)
  opcoes_new <- NULL
  
  while (i <= max) {
    opcoes_new[i] <- df_indice_questoes_mod[df_indice_questoes_mod$id_Char == idchar,
                           opcoes[i]]

    i <- i + 1
  }
  
  return(opcoes_new)
}

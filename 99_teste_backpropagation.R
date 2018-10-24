library(neuralnet)
library(nnet)
library(dplyr)

dir_root <- getwd() 

dir_dados <- paste0(dir_root,"/DATA")
dir_dados_pre_processados <- paste0(dir_dados,"/pre_processados")

df_alunos_2015_melhores_cidades <- 
  read.csv2(file = paste0(dir_dados_pre_processados, 
                          "/df_alunos_2015_melhores_cidades.csv"),
            sep = ";",
            stringsAsFactors = FALSE)

df_indice_questoes <- 
  read.csv2(file = paste0(dir_dados_pre_processados, 
                          "/df_indice_questoes.csv"),
            sep = ";",
            strip.white = TRUE,
            na.strings = c("", " ", NA),
            stringsAsFactors = FALSE)

df_melhores_mod <- df_alunos_2015_melhores_cidades %>% 
  select(nivel_proficiencia, TX_RESP_Q013_alunos:TX_RESP_Q057_alunos) %>% 
  mutate_at(vars(TX_RESP_Q013_alunos:TX_RESP_Q057_alunos),
            funs(
              case_when(
                . == "A" ~ 1,
                . == "B" ~ 2,
                . == "C" ~ 3,
                . == "D" ~ 4,
                . == "E" ~ 5,
                . == "F" ~ 6,
                . == "G" ~ 7,
                . == "H" ~ 8, 
                . == "I" ~ 9,
                . == "J" ~ 10,
                . == "K" ~ 11,
                . == "L" ~ 12, 
                . == "Nao Informado" ~ 0
              )
            )
  )


# Encode as a one hot vector multilabel data
df_melhores_mod2 <- cbind(df_melhores_mod[, 2:18],
                          class.ind(as.factor(df_melhores_mod$nivel_proficiencia)))
# Set labels name
names(df_melhores_mod2) <- c(names(df_melhores_mod)[2:18],
                             "Avancado","Basico","Insuficiente","Proficiente")

# Criar array com as questões selecionadas
questoes <- c("TX_RESP_Q013", "TX_RESP_Q015", "TX_RESP_Q019", "TX_RESP_Q023", 
              "TX_RESP_Q026", "TX_RESP_Q027", "TX_RESP_Q028", "TX_RESP_Q029",
              "TX_RESP_Q030", "TX_RESP_Q043",  "TX_RESP_Q044", "TX_RESP_Q045",
              "TX_RESP_Q048", "TX_RESP_Q053", "TX_RESP_Q054", "TX_RESP_Q055", 
              "TX_RESP_Q057")

## Loop usado para transformar a opção em peso, para que seja possível aplicar
## a distância euclidiana nas questões
i = 1
i_max = length(questoes)

df_melhores_mod3 <- df_melhores_mod2
while (i <= i_max) {
  
  peso <- df_indice_questoes %>% 
    filter(id_Char == questoes[i]) %>% 
    select(qtd_opcoes) %>% 
    as.integer()
  
  indice <- paste0(questoes[i], "_alunos")
  
  df_melhores_mod3[indice] <- (df_melhores_mod3[indice] * 12) / peso
  
  i <- i + 1 
}

formula <- Avancado + Basico + Insuficiente + Proficiente ~ TX_RESP_Q013_alunos + 
  TX_RESP_Q015_alunos + TX_RESP_Q019_alunos + TX_RESP_Q023_alunos +
  TX_RESP_Q026_alunos + TX_RESP_Q027_alunos + TX_RESP_Q028_alunos + 
  TX_RESP_Q029_alunos + TX_RESP_Q030_alunos + TX_RESP_Q043_alunos +
  TX_RESP_Q044_alunos + TX_RESP_Q045_alunos + TX_RESP_Q048_alunos + 
  TX_RESP_Q053_alunos + TX_RESP_Q054_alunos + TX_RESP_Q055_alunos + 
  TX_RESP_Q057_alunos

set.seed(100)
index <- sample(1:nrow(df_melhores_mod3), round(0.90*nrow(df_melhores_mod3)))
train_cv <- df_melhores_mod3[index, ]
test_cv <- df_melhores_mod3[-index, ]


nn <- neuralnet(formula = formula,
                data = train_cv,
                hidden = 50,
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign = "minimal")

# Compute predictions
pr.nn <- neuralnet::compute(nn, test_cv[, 1:17])
# Extract results
pr.nn_ <- pr.nn$net.result

# Accuracy (test set)
original_values <- max.col(test_cv[, 18:21])
pr.nn_2 <- max.col(pr.nn_)
mean(pr.nn_2 == original_values)


# #############################################################################
# # Set seed for reproducibility purposes
# set.seed(500)
# # 10 fold cross validation
# k <- 10
# # Results from cv
# outs <- NULL
# # Train test split proportions
# proportion <- 0.90 # Set to 0.995 for LOOCV
# 
# # Crossvalidate, go!
# for(i in 1:k)
# {
#   index <- sample(1:nrow(df_melhores_mod3), round(proportion*nrow(df_melhores_mod3)))
#   train_cv <- df_melhores_mod3[index, ]
#   test_cv <- df_melhores_mod3[-index, ]
#   nn_cv <- neuralnet(formula = formula,
#                      data = train_cv,
#                      hidden = 50,
#                      act.fct = "logistic",
#                      linear.output = FALSE)
#   
#   # Compute predictions
#   pr.nn <- compute(nn_cv, test_cv[, 1:17])
#   # Extract results
#   pr.nn_ <- pr.nn$net.result
#   # Accuracy (test set)
#   original_values <- max.col(test_cv[, 18:21])
#   pr.nn_2 <- max.col(pr.nn_)
#   outs[i] <- mean(pr.nn_2 == original_values)
# }
# 
# mean(outs)

##### Pacotes necessários #####
# manipulação dos dados
library(dplyr)
# importação de dados
library(readr)
# algoritmo kohonen
library(kohonen)

#### Diretorios #####
dir_root <- getwd() 

dir_dados <- paste0(dir_root,"/DATA")
dir_dados_pre_processados <- paste0(dir_dados,"/pre_processados")

##### Carregar data frames #####
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

##### Preparação dos dados #####

df_2015_mod <- 
  df_alunos_2015_melhores_cidades %>% 
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
  ) %>% 
  mutate(nivel_proficiencia, 
         nivel_proficiencia = 
           case_when(
             nivel_proficiencia == "Insuficiente" ~ 1,
             nivel_proficiencia == "Basico" ~ 2,
             nivel_proficiencia == "Proficiente" ~ 3,
             nivel_proficiencia == "Avancado" ~ 4,
           )
  ) %>% 
  mutate(ind_aprendizado_adequado, 
         ind_aprendizado_adequado = 
           case_when(
             ind_aprendizado_adequado == "Nao" ~ 1,
             ind_aprendizado_adequado == "Sim" ~ 2
           )
  )

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

df_2015_mod2 <- df_2015_mod
while (i <= i_max) {
  
  peso <- df_indice_questoes %>% 
    filter(id_Char == questoes[i]) %>% 
    select(qtd_opcoes) %>% 
    as.integer()
    
  indice <- paste0(questoes[i], "_alunos")
  
  df_2015_mod2[indice] <- (df_2015_mod2[indice] * 12) / peso
  
  i <- i + 1 
}

#### Casos de exceções, onde é necessário fazer ajuste manualmente.
# ~ 

#### Aplicação da rede neural kohonen #####
#### Não supervisionado
df_2015_mod3 <- 
  df_2015_mod2 %>% 
  select(nivel_proficiencia, 
         TX_RESP_Q013_alunos:TX_RESP_Q057_alunos)

data_train_matrix <- as.matrix(scale(df_2015_mod3))
som_grid <- somgrid(xdim = 17, ydim=17, topo="hexagonal")

som_model <- som(data_train_matrix, 
                    grid=som_grid, rlen=1000, 
                    alpha=c(0.05,0.01), keep.data = TRUE)

# plot(som_model, type="changes")
# plot(som_model, type="count")
# plot(som_model, type="dist.neighbours")
# plot(som_model, type="codes")
# plot(som_model, type = "property", property = som_model$codes[[1]][,1], 
#      main=names(som_model$data)[4])

#### Supervisionado #############################
# Data Split
set.seed(123)
ind <- sample(2, nrow(df_2015_mod3), replace = T, prob = c(0.7, 0.3))
train <- df_2015_mod3[ind == 1,]
test <- df_2015_mod3[ind == 2,]

# Transformação para matrix
trainX <- as.matrix(train[,-1])
testX <- as.matrix(test[,-1])
trainY <- factor(train[,1])


Y <- factor(test[,1])
test[,1] <- 0
testXY <- list(independent = testX, dependent = test[,1])

# Classification & Prediction Model
set.seed(222)
map1 <- xyf(trainX,
            classvec2classmat(factor(trainY)),
            grid = 
              somgrid(xdim = 17, 
                      ydim = 17, 
                      topo = "hexagonal", 
                      toroidal = FALSE),
            rlen = 100,
            dist.fcts = "euclidean")

plot(map1)

# Prediction
pred <- predict(map1, newdata = testXY)
table(Predicted = pred1$predictions[[2]], Actual = Y)


# Cluster Boundaries
par(mfrow = c(1,2))
plot(map1, 
     type = 'codes',
     main = c("Codes X", "Codes Y"))
map1.hc <- cutree(hclust(dist(map1$codes[[2]])), 4)
add.cluster.boundaries(map1, map1.hc)
par(mfrow = c(1,1))


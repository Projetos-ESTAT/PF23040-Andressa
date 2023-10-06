source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

#------------------------Arrumando a casa--------------------------

#Puxando o banco

cad_andressa <- read_csv("banco/cad_andressa_att.csv")

#Mudando nome das variáveis 
colnames(cad_andressa) <- c(    "Pergunta_01",
                                "Pergunta_02",
                                "Pergunta_03",
                                "Pergunta_04",
                                "Pergunta_05",
                                "Pergunta_06",
                                "Pergunta_07",
                                "Pergunta_08",
                                "Pergunta_09",
                                "Pergunta_10",
                                "Pergunta_11",
                                "Pergunta_12",
                                "Pergunta_13",
                                "Pergunta_14",
                                "Pergunta_15",
                                "Pergunta_16",
                                "Pergunta_17",
                                "Pergunta_18",
                                "Pergunta_19"
)


#Filtrando membros que responderam que não possuiam curso, pois a cliente queria saber a quantidade: 53
cad_andressa %>% 
  filter((Pergunta_06 %in% c("Não possuo."))) %>% 
  count()

#Retira linhas que tenha CTE e não possuo na pergunta 06, a pedido da cliente
#Foi de 160 linhas para 107 linhas
cad_andressa <- cad_andressa %>%
  filter(!(Pergunta_06 %in% c("CTE", "Não possuo.")))



#Estabelecendo Caminho para salvar os gráficos
caminho_lucas <- "resultados"

#--------------------Distribuição de Socorristas e Não-Socorristas--------------

unique(cad_andressa$Pergunta_04)

#Alterando o campo pergunta para 4 para ter apenas socorristas e não socorristas (se o campo tem a palavra
#socorrista ele atribui socorrista se não atribui não socorrista)

cad_andressa <- cad_andressa %>% 
  mutate(Pergunta_04 = ifelse(grepl("Socorrista", Pergunta_04), "Socorrista", "Não Socorrista"))

#plotando o gráfico de barras dessa distribuição

pergunta04 <- cad_andressa %>%
  filter(!is.na(Pergunta_04)) %>%
  count(Pergunta_04) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(pergunta04) +
  aes(
    x = fct_reorder(Pergunta_04, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Função", y = "Frequência") +
  theme_estat()+
  scale_y_continuous(breaks = seq(from = 0, to = 150, by = 50), limits=c(0, 150))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-04.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-04.png"), width = 158, height = 93, units = "mm")

#-------------------quais segmentos ocorreram mais lesões----------------------
#Pergunta 10

unique(cad_andressa$Pergunta_10)

class(cad_andressa$Pergunta_10)

pergunta10 <- cad_andressa %>% 
  select(Pergunta_10) %>% 
  filter(!is.na(Pergunta_10))

# Definir o vetor de segmentos
segmentos <- c(
  "Coluna cervical",
  "Coluna dorsal",
  "Coluna lombar",
  "Braço",
  "Antebraço",
  "Punho/mão",
  "Ombro",
  "Joelho",
  "Quadril",
  "Tornozelo/pé"
)

# Inicializar um vetor para armazenar as contagens
contagens <- integer(length(segmentos))

# Loop for para contar as correspondências
for (i in 1:length(segmentos)) {
  contagens[i] <- sum(sapply(pergunta10, function(x) str_count(x, segmentos[i])))
}

# Criar um novo dataframe para exibir as contagens
contagens_segmento <- data.frame(segmento = segmentos, quantidade = contagens)

#transformando em factor
contagens_segmento$segmento <- as.factor(contagens_segmento$segmento)

# #Redefinindo o vetor para dar um espacinho em algumas variaveis e o gráfico ficar menos pior
# 
# segmentos <- c(
#   "Coluna cervical",
#   "Coluna dorsal",
#   "Coluna lombar",
#   "Braço",
#   "Antebraço",
#   "Punho/mão",
#   "Ombro",
#   "Joelho",
#   "Quadril",
#   "Tornozelo/pé"
# )
# 
# contagens_segmento$segmento <- segmentos

#plotando o gráfico
contagemSegmento <- contagens_segmento %>%
  filter(!is.na(segmento)) %>%
  mutate(
    freq = quantidade/sum(quantidade)
  ) %>%
  mutate(
    freq = gsub("\\.", ",", scales::percent(freq)),
    label = paste(quantidade, " (", freq, ")", sep = "")
  )


ggplot(contagemSegmento) +
  aes(
    x = fct_reorder(segmento, quantidade, .desc = T),
    y = quantidade,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 2.5
  ) +
  labs(x = "Segmento", y = "Frequência") +
  theme_estat()+
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 20), limits=c(0, 110))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) #+
  # theme(axis.text.x = element_text(size = 8))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-10.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-10.png"), width = 158, height = 93, units = "mm")



#---------------quantidade de lesão para homens e mulheres----------------------
#Pergunta_12: ja sofreu lesão
#Pergunta 1: Masculino e feminino

class(cad_andressa$Pergunta_12)

cad_andressa$Pergunta_01 <- as.factor(cad_andressa$Pergunta_01)
cad_andressa$Pergunta_12 <- as.factor(cad_andressa$Pergunta_12)

#166 OBSERVAÇÕES apenas 70 responderam sim a pergunta 12 e 96 disseram não. 
pergunta01_12 <- cad_andressa %>% 
  select(Pergunta_01, Pergunta_12) %>% 
  filter(Pergunta_12 == 'Sim') %>% 
  group_by(Pergunta_12, Pergunta_01) %>% 
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

porcentagens <- str_c(pergunta01_12$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(pergunta01_12$freq, " (", porcentagens, ")"))


ggplot(pergunta01_12) +
  aes(
    x = fct_reorder(Pergunta_01, freq, .desc = T),
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Sexo", y = "Frequência") +
  theme_estat()+
  scale_y_continuous(breaks = seq(from = 0, to = 50, by = 10), limits=c(0, 50))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-01-12.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-01-12.png"), width = 158, height = 93, units = "mm")



#--------------------------Tipo de Lesão-------------------------------
#Pergunta_14: Qual foi o tipo de lesão?

pergunta14 <- cad_andressa %>% 
  select(Pergunta_14) %>% 
  filter(!is.na(Pergunta_14))

#Extraindo todas as informações das linhas e colocando em um vetor para identificar todos os valores diferentes
vetor_pergunta_14 <- unlist(strsplit(pergunta14$Pergunta_14, ", "))
print(vetor_pergunta_14)
unique(vetor_pergunta_14)


#Vetor com todos os valores unicos que aparecem no vetor_pergunta_14

# Definir o vetor de lesao
lesao <- c("Lesão ligamentar",
           "Dor crônica na coluna",
           "Estiramento muscular",
           "Tendinite",
           "Condromalácia patelar",
           "Degenerações do disco intervertebral",
           "Desgaste/dor articular",
           "Hérnia inguinal (apesar de não estar incluído em musculoesqueléticas)",
           "Protrusão discal",
           "Lombalgia",
           "Fascite plantar",
           "Síndrome do túnel do carpo",
           "Desconforto na lombar",
           "hérnia de disco")


# Inicializar um vetor para armazenar as contagens
contagens_lesao <- integer(length(lesao))


# Loop for para contar as correspondências usando expressões regulares
for (i in 1:length(lesao)) {
  contagens_lesao[i] <- sum(sapply(pergunta14, function(x) str_detect(x, fixed(lesao[i]))))
}

# Criar um novo dataframe para exibir as contagens
contagens_lesao_df <- data.frame(lesao = lesao, quantidade = contagens_lesao)

contagens_lesao_df$lesao <- c("Lesão ligamentar",
                                "Dor crônica na coluna",
                                "Estiramento muscular",
                                "Tendinite",
                                "Condromalácia patelar",
                                "Degenerações do disco intervertebral",
                                "Desgaste articular",
                                "Hérnia inguinal (apesar de não estar incluído em musculoesqueléticas)",
                                "Protrusão discal",
                                "Lombalgia",
                                "Fascite plantar",
                                "Síndrome do túnel do carpo",
                                "Desconforto na lombar",
                                "hérnia de disco")

#Plotando gráfico

#OBS: Serão retirados valores que aparecem apenas uma vez, serão eles Lesão completa do ligamento do Escafóide-Semilunar no punho esquerdo
#Hérnia inguinal (apesar de não estar incluído em musculoesqueléticas), Inflamação do nervo ciático, Protrusão discal, Lombalgia, Fascite plantar


contagens_lesao_df %>%
  filter(quantidade > 2) %>%
  mutate(
    freq = quantidade/sum(quantidade)
  ) %>%
  mutate(
    freq = gsub("\\.", ",", scales::percent(freq)),
    label = paste(quantidade, " (", freq, ")", sep = "")
  ) %>% 
ggplot() +
  aes(
    x = fct_reorder(lesao, quantidade, .desc = T),
    y = quantidade,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.2, hjust = -0.1,
    size = 2.8
  ) +
  labs(x = "Lesão", y = "Frequência") +
  theme_estat() +
  coord_flip()+
  scale_x_discrete(labels = wrap_format(13))+
  scale_y_continuous(breaks = seq(from = 0, to = 30, by = 5), limits=c(0, 32))

  # scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  # theme(axis.text.x = element_text(size = 8))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-14.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-14.png"), width = 158, height = 93, units = "mm")



#------Relação entre prática de atividade física e se já sofreu alguma lesão----

#Pergunta 7:Pratica alguma atividade física?
#Pergunta 12: Você já sofreu alguma lesão musculoesquelética durante o serviço de UR?



pergunta7_12 <- cad_andressa %>% 
  select(Pergunta_07, Pergunta_12) %>% 
  group_by(Pergunta_07, Pergunta_12) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

porcentagens <- str_c(pergunta7_12$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(pergunta7_12$freq, " (", porcentagens, ")"))

colnames(pergunta7_12) <- c("Pergunta_07", 
                            "Lesão musculoesquelética",
                            "freq",
                            "freq_relativa"
                            )

ggplot(pergunta7_12) +
  aes(
    x = fct_reorder(Pergunta_07, freq, .desc = T),
    y = freq,
    fill = `Lesão musculoesquelética`,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Pratica de Atividade Física", y = "Frequência") +
  theme_estat()+
  scale_y_continuous(breaks = seq(from = 0, to = 70, by = 10), limits=c(0, 70))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-07-12.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-07-12.png"), width = 158, height = 93, units = "mm")

#-----Relação entre o fato de já ter recebido treinamento e se já sofreu alguma lesão.-----
#Pergunta 8) Você recebeu treinamento adequado para minimizar os riscos de lesões no serviço de 
#UR?
#Pergunta 12: Você já sofreu alguma lesão musculoesquelética durante o serviço de UR?

  pergunta_08_12 <- cad_andressa %>% 
  select(Pergunta_08, Pergunta_12) %>% 
  group_by(Pergunta_08, Pergunta_12) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

porcentagens <- str_c(pergunta_08_12$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(pergunta_08_12$freq, " (", porcentagens, ")"))

colnames(pergunta_08_12) <- c("Treinamento", 
                            "Lesão musculoesquelética",
                            "freq",
                            "freq_relativa"
)

ggplot(pergunta_08_12) +
  aes(
    x = fct_reorder(`Treinamento`, freq, .desc = T),
    y = freq,
    fill = `Lesão musculoesquelética`,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Treinamento para minimizar os riscos de lesões", y = "Frequência") +
  theme_estat()+
  scale_y_continuous(breaks = seq(from = 0, to = 70, by = 10), limits=c(0, 70))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-08-12.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-08-12.png"), width = 158, height = 93, units = "mm")





#-------Relação entre os anos de experiência na UR e se já sofreu alguma lesão.-------
#Pergunta 05: Você tem quantos anos de experiência na UR?
#Pergunta 12: Você já sofreu alguma lesão musculoesquelética durante o serviço de UR?

pergunta05_12 <- cad_andressa %>% 
  select(Pergunta_05, Pergunta_12) %>% 
  group_by(Pergunta_05, Pergunta_12) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )


colnames(pergunta05_12) <- c("Anos de Experiência", 
                            "Lesão musculoesquelética",
                            "freq",
                            "freq_relativa"
)

ordem_personalizada <- c(
  "Menos de 1 ano",
  "De 1 a 5 anos",
  "De 6 a 10 anos",
  "De 11 a 15 anos",
  "De 16 a 20 anos",
  "De 21 a 25 anos",
  "De 26 a 30 anos"
)

pergunta05_12$`Anos de Experiência` <- factor(pergunta05_12$`Anos de Experiência`, levels = ordem_personalizada, ordered = TRUE)

porcentagens <- str_c(pergunta05_12$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(pergunta05_12$freq, " (", porcentagens, ")"))


ggplot(pergunta05_12) +
  aes(
    # x = fct_reorder(`Anos de Experiência`, freq, .desc = T),
     x = `Anos de Experiência`,
    y = freq,
    fill = `Lesão musculoesquelética`,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.4, hjust = -0.08,
    size = 2.5
    # vjust = -0.5, hjust = 0,
    # size = 2.5,
    # angle = 40
  ) +
  labs(x = "Anos de Experiência na UR", y = "Frequência") +
  theme_estat()+
  coord_flip()+
  scale_y_continuous(breaks = seq(from = 0, to = 50, by = 10), limits=c(0, 50))
  # scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  # theme(axis.text.x = element_text(size = 8))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-05-12.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-05-12.png"), width = 158, height = 93, units = "mm")



#Ela quer saber também porcentagem dos que precisaram ser afastados(Pergunta 17)
#Então dos 70 que responderam a pergunta 17 54 (77,14%) respondeu que precisou ser afastado 
pergunta17 <- cad_andressa %>% 
  select(Pergunta_17) %>% 
  filter(!is.na(Pergunta_17)) %>% 
  group_by(Pergunta_17) %>% 
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )



#-------Relação entre a prática de atividade física com o fato sentir dor/desconforto-------
#gráfico "extra"

#Pergunta07: Pratica alguma atividade física
#Pergunta09: Sente dor ou desconforto 


pergunta_07_09 <- cad_andressa %>% 
  select(Pergunta_07,Pergunta_09) %>%
  group_by(Pergunta_07, Pergunta_09) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

colnames(pergunta_07_09) <- c("Pergunta_07",
                              "Sente Dor",
                              "freq",
                              "freq_relativa"
                              
  
)


porcentagens <- str_c(pergunta_07_09$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(pergunta_07_09$freq, " (", porcentagens, ")"))

ggplot(pergunta_07_09) +
  aes(
    x = fct_reorder(Pergunta_07, freq, .desc = T),
    y = freq,
    fill = `Sente Dor`,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Pratica de Atividade Física", y = "Frequência") +
  theme_estat()+
  scale_y_continuous(breaks = seq(from = 0, to = 70, by = 10), limits=c(0, 70))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-07-09.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-07-09.png"), width = 158, height = 93, units = "mm")






#---------------------Movimentos que favorecem dor/desconforto------------------
#Pergunta 11: Quais movimentos favorecem a dor/desconforto?

# Filtrar a pergunta 11
pergunta11 <- cad_andressa %>% 
  select(Pergunta_11) %>% 
  filter(!is.na(Pergunta_11))

# Extraindo todas as informações das linhas e colocando em um vetor para identificar todos os valores diferentes
vetor_pergunta_11 <- unlist(strsplit(pergunta11$Pergunta_11, ", "))
print(vetor_pergunta_11)
unique(vetor_pergunta_11)

# Vetor com todos os valores únicos que aparecem no vetor_pergunta_11

# Definir o vetor de movimentos de desconforto
mov_desconf <- c("Ao elevar a maca/prancha com a vítima",
                 "Ao elevar/transportar a vítima em seus braços",
                 "Na posição de imobilização da coluna cervical da vítima",
                 "Na imobilização de membros",
                 "Ao realizar RCP",
                 "Ao realizar movimentações de rolamento e elevação da vítima (à cavalero)",
                 "Ao realizar algum movimento de apoio do corpo da vitima dentro da UR.",
                 "Durante o transporte de vítimas em prédios sem elevador.",
                 "Muito tempo abaixado ou de joelhos",
                 "Mudar paciente de maca.",
                 "Ao final do dia após ter passado muito tempo sentada (devido ao banco dianteiro da UR)",
                 "Ao descer e subir na viatura",
                 "Subindo e descendo da viatura")

# Inicializar um vetor para armazenar as contagens
contagens_mov_desconf <- integer(length(mov_desconf))

# Loop for para contar as correspondências usando expressões regulares
for (i in 1:length(mov_desconf)) {
  contagens_mov_desconf[i] <- sum(sapply(pergunta11, function(x) str_detect(x, fixed(mov_desconf[i]))))
}

# Criar um novo dataframe para exibir as contagens
contagens_mov_desconf_df <- data.frame(mov_desconf = mov_desconf, quantidade = contagens_mov_desconf)


# Vetor com as abreviações correspondentes
abreviacoes <- c(
  "Maca/ Prancha",
  "Elevar/ Transportar",
  "Imobilização Cervical",
  "Imobilização de Membros",
  "RCP",
  "Elevação à cavaleiro",
  "Ao realizar algum movimento de apoio do corpo da vitima dentro da UR.",
  "Durante o transporte de vítimas em prédios sem elevador.",
  "Muito tempo abaixado ou de joelhos",
  "Mudar paciente de maca.",
  "Ao final do dia após ter passado muito tempo sentada (devido ao banco dianteiro da UR)",
  "Ao descer e subir na viatura",
  "Subindo e descendo da viatura"
)

contagens_mov_desconf_df$mov_desconf <- abreviacoes


#plotando o gráfico
contagens_mov_desconf_df <- contagens_mov_desconf_df %>%
  mutate(
    freq = quantidade/sum(quantidade)
  ) %>%
  mutate(
    freq = gsub("\\.", ",", scales::percent(freq)),
    label = paste(quantidade, " (", freq, ")", sep = "")
  )


#Foram retirados os movimentos de dor e desconforto que foram listados apenas 1 vez pois representavam menos de 1% 
contagens_mov_desconf_df %>% 
  filter(quantidade > 1) %>% 
  ggplot() +
  aes(
    x = fct_reorder(mov_desconf, quantidade, .desc = T),
    y = quantidade,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = 0.5,
    size = 3
  ) +
  labs(x = "Movimentos que Favorecem Dor/Desconforto", y = "Frequência") +
  theme_estat()+
  scale_x_discrete(labels = wrap_format(13))
  
  
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-11.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-11.png"), width = 158, height = 93, units = "mm")

  




#----------Relação de quem respondeu “SIM” para questão 9 com a questão 15, além de correlacionar com a prática de atividade física.-------
#filtrar pergunta 9 e relacionar pergunta 15 com pergunta 7


pergunta_07_15 <- cad_andressa %>% 
  select(Pergunta_09, Pergunta_07, Pergunta_15) %>% 
  filter(Pergunta_09 == "Sim", !is.na(Pergunta_15))

#125 pessoas responderam Sim para a pergunta 09, entretanto 63 pessoas deixaram em branco a pergunta 15, restando então 62 observações para relacionar a pergunta 07 com a pergunta 15


pergunta_07_15 <- pergunta_07_15 %>%
    mutate(Pergunta_15 = case_when(
      Pergunta_15 %>% str_detect("Sim. E piorou após atuar na UR.") ~ "Sim. Piorou após UR",
      Pergunta_15 %>% str_detect("Sim. E manteve após atuar na UR.") ~ "Sim. Manteve após UR.",
      Pergunta_15 %>% str_detect("Não possuía.") ~ "Não possuía.",
      Pergunta_15 %>% str_detect("Não sei.") ~ "Não sei."
  )) %>%
  group_by(Pergunta_15, Pergunta_07) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

colnames(pergunta_07_15)[2] <- "Prática de Atividade Física"

porcentagens <- str_c(pergunta_07_15$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(pergunta_07_15$freq, " (", porcentagens, ")"))

ggplot(pergunta_07_15) +
  aes(
    x = fct_reorder(Pergunta_15, freq, .desc = T),
    y = freq,
    fill = `Prática de Atividade Física`,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.4, hjust = -0.08,
    size = 3
  ) +
  labs(x = "Histórico de Dores Antes de Trabalhar na UR", y = "Frequência") +
  theme_estat()+
  coord_flip()+
  scale_x_discrete(labels = wrap_format(13))+
  #guides(fill = guide_legend(nrow = 2))+
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 5), limits=c(0, 20))

  
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  #theme(axis.text.x = element_text(size = 8),
         #legend.text = element_text(size = 9))

ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-15-07.pdf"), width = 198, height = 93, units = "mm")
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-15-07.png"), width = 180, height = 93, units = "mm")



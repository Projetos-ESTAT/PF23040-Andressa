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

cad_andressa <- read_csv("banco/cad_andressa.csv")

#Mudando nome das variáveis 
colnames(cad_andressa) <- c("data_hora", 
                                "Pergunta_01",
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
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), limits=c(0, 100))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-04.pdf"), width = 158, height = 93, units = "mm")

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
  scale_y_continuous(breaks = seq(from = 0, to = 70, by = 10), limits=c(0, 70))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-10.pdf"), width = 158, height = 93, units = "mm")



#---------------quantidade de lesão para homens e mulheres----------------------
#Pergunta_12: ja sofreu lesão
#Pergunta 1: Masculino e feminino

class(cad_andressa$Pergunta_12)

cad_andressa$Pergunta_01 <- as.factor(cad_andressa$Pergunta_01)
cad_andressa$Pergunta_12 <- as.factor(cad_andressa$Pergunta_12)

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
  scale_y_continuous(breaks = seq(from = 0, to = 40, by = 10), limits=c(0, 40))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-01-12.pdf"), width = 158, height = 93, units = "mm")



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
lesao <- c(
  "Lesão ligamentar",                                                     
  "Estiramento muscular",                                                 
  "Tendinite",                                                            
  "Degenerações do disco intervertebral",                                 
  "Dor crônica na coluna",                                                
  "Desgaste/dor articular",                                               
  "Lesão completa do ligamento do Escafóide-Semilunar no punho esquerdo", 
  "Hérnia inguinal (apesar de não estar incluído em musculoesqueléticas)",
  "Protrusão discal",                                                     
  "Lombalgia",                                                            
  "Condromalácia patelar" 
)

# Inicializar um vetor para armazenar as contagens
contagens_lesao <- integer(length(lesao))


# Loop for para contar as correspondências usando expressões regulares
for (i in 1:length(lesao)) {
  contagens_lesao[i] <- sum(sapply(pergunta14, function(x) str_detect(x, fixed(lesao[i]))))
}

# Criar um novo dataframe para exibir as contagens
contagens_lesao_df <- data.frame(lesao = lesao, quantidade = contagens_lesao)



#Plotando gráfico

#OBS: Serão retirados valores que aparecem apenas uma vez, serão eles Lesão completa do ligamento do Escafóide-Semilunar no punho esquerdo
#Hérnia inguinal (apesar de não estar incluído em musculoesqueléticas), Inflamação do nervo ciático, Protrusão discal, Lombalgia, Fascite plantar


contagens_lesao_df %>%
  # filter(quantidade > 2) %>%
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
    vjust = -0.5, # hjust = .5,
    size = 2.5
  ) +
  labs(x = "Lesão", y = "Frequência") +
  theme_estat()+
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 5), limits=c(0, 20))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(axis.text.x = element_text(size = 8))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-14.pdf"), width = 158, height = 93, units = "mm")





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
  scale_y_continuous(breaks = seq(from = 0, to = 50, by = 10), limits=c(0, 50))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-07-12.pdf"), width = 158, height = 93, units = "mm")


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
  scale_y_continuous(breaks = seq(from = 0, to = 50, by = 10), limits=c(0, 50))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-08-12.pdf"), width = 158, height = 93, units = "mm")





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
    vjust = -0.5, hjust = 0.5,
    size = 1.8
    # vjust = -0.5, hjust = 0,
    # size = 2.5,
    # angle = 40
  ) +
  labs(x = "Anos de Experiência na UR", y = "Frequência") +
  theme_estat()+
  scale_y_continuous(breaks = seq(from = 0, to = 50, by = 10), limits=c(0, 50))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(axis.text.x = element_text(size = 8))
ggsave(filename = file.path(caminho_lucas, "colunas-uni-freq-05-12.pdf"), width = 158, height = 93, units = "mm")


# jogos brasil, xbox one br, jogos mobile br/pt, uff niteroi, ilha dos estudos, ilha da macacada
#http://dsgeek.com/2014/09/19/Customizingggplot2charts.html

# OBS: Colocar titulo nas figuras pra apresentar, mas remover o titulo no relatorio, o titulo deve vir escrito abaixo da figura

# Pacotes -----------------------------------------------------------------

library(readxl)
library(janitor)
library(dplyr)
library(ggplot2)
library(magrittr)

# Bases -------------------------------------------------------------------

raw_data <- read_excel("data/Perfil da Comunidade Gamer (respostas).xlsx") %>% clean_names

dados <- raw_data %>% filter(com_que_frequencia_costuma_jogar_games!="Nunca")

# Visualizacoes -----------------------------------------------------------

# Com que frequencia costuma jogar games?

raw_data %>% 
  group_by(com_que_frequencia_costuma_jogar_games) %>% 
  summarise(freq = n()) %>% 
  ggplot(., aes(x = reorder(com_que_frequencia_costuma_jogar_games, -freq), y = freq)) +
  geom_bar(stat = 'identity', fill = "#0f0559") +
  labs(x = "", y = "Frequência Absoluta", title = "Com que frequência costuma jogar games?") +
  geom_text(aes(label = freq), nudge_y = 10) +
  annotate("text", label = paste("Total de respostas =", 911), x = 3.5, y = 600, size = 4) +
  theme_minimal()

# Genero

dados %>% 
  group_by(genero) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x = genero, y = freq, fill = genero)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = freq), nudge_y = 20) +
  scale_fill_manual(values = c("royalblue", "pink", "orange")) +
  labs(x = "", y = "Frequência Absoluta", title = "Gênero") +
  theme_minimal() +
  theme(legend.position = "None")

# Idade por Genero

dados %>% 
  select(idade_anos, genero) %>% 
  mutate(idade_anos = ifelse( idade_anos<9, NA, idade_anos )) %>% 
  ggplot(aes(x = genero, y = idade_anos)) +
  geom_boxplot(aes(fill = genero)) +
  scale_fill_manual(values = c("royalblue", "pink", "orange")) +
  labs(x = "", y = "Idade", title = "Idade (anos)") +
  theme_minimal() +
  theme(legend.position = "None")

# Situacao atual

dados %>% 
  group_by(situacao_atual) %>% 
  summarise(freq = n()) %>%
  ggplot(aes(x = reorder(situacao_atual, -freq), y = freq)) +
  geom_bar(stat = 'identity', fill = "#800000") +
  geom_text(aes(label = freq), nudge_y = -10, color = "white") +
  labs(x = "", y = "Frequência Absoluta", title = "Situação atual") +
  coord_flip()

# Grau de escolaridade 

dados %>% 
  group_by(grau_de_escolaridade) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x = grau_de_escolaridade, y = freq)) +
  geom_bar(stat = 'identity', fill = "#800000") +
  geom_text(aes(label = freq), nudge_y = -10, color = "white") +
  labs(x = "", y = "Frequência Absoluta", title = "Grau de escolaridade") +
  coord_flip() +
  theme_minimal()

# Qual plataforma prefere usar para jogar

dados %>% 
  group_by(qual_plataforma_prefere_usar_para_jogar) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x = reorder(qual_plataforma_prefere_usar_para_jogar, -freq), y = freq)) +
  geom_bar(stat = 'identity', fill = "#800000") +
  geom_text(aes(label = freq), nudge_y = -10, color = "white") +
  labs(x = "", y = "Frequência Absoluta", title = "Qual plataforma prefere usar para jogar") +
  coord_flip() +
  theme_minimal()

# Voce conhece os componentes do seu computador

# AJEITAR ESSA DESGRACA
dados %>% 
  filter(!is.na(voce_conhece_os_componentes_do_seu_computador_processador_placa_de_video_e_memoria_ram)) %>% 
  group_by(voce_conhece_os_componentes_do_seu_computador_processador_placa_de_video_e_memoria_ram) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x = "", y = freq, fill = voce_conhece_os_componentes_do_seu_computador_processador_placa_de_video_e_memoria_ram)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = freq), nudge_y = -10, color = "white") +
  scale_fill_manual(values = c("#12f45f", "#800020")) +
  labs(x = "", y = "", 
       title = "Você conhece os componentes do seu computador \n (Processador, Placa de Vídeo e Memória RAM) ?") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.title =element_blank())

# Quantos gigabytes de memoria RAM

dados %>% 
  filter(!is.na(quantos_gigabytes_de_memoria_ram)) %>% 
  group_by(quantos_gigabytes_de_memoria_ram) %>% 
  summarise(freq = n()) %>% 
  mutate(quantos_gigabytes_de_memoria_ram = factor(quantos_gigabytes_de_memoria_ram, levels = c(paste(c(1,2,4,6,8,12,16, "mais de 16"), "GB")))) %>% 
  ggplot(aes(x = quantos_gigabytes_de_memoria_ram, y = freq)) +
  geom_bar(stat = 'identity', fill = "#800000") +
  geom_text(aes(label = freq), nudge_y = 5) +
  labs(x = "", y = "Frequência Absoluta", title = "Quantos Gigabytes de memória RAM?") +
  theme_minimal()

# Qual a marca do seu porcessador

dados %>% 
  filter(!is.na(qual_a_marca_do_seu_processador)) %>% 
  group_by(qual_a_marca_do_seu_processador) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x = "", y = freq, fill = qual_a_marca_do_seu_processador)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = freq), nudge_y = -10, color = "white") +
  scale_fill_manual(values = c("#12f45f", "#800020")) +
  labs(x = "", y = "", 
       title = "Qual a marca do seu porcessador?") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.title =element_blank())

# Qual a marca da sua placa de video

dados %>% 
  filter(!is.na(qual_a_marca_da_sua_placa_de_video)) %>% 
  group_by(qual_a_marca_da_sua_placa_de_video) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x = "", y = freq, fill = qual_a_marca_da_sua_placa_de_video)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = freq), nudge_y = -10, color = "white") +
  scale_fill_manual(values = c("#12f45f", "#800020")) +
  labs(x = "", y = "", 
       title = "Qual a marca da sua placa de video?") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.title =element_blank())

# Quantos gigabtes possui sua placa de video

## quem colocou 1060 2 ou 4 gb mudar pra 3

# A maoria das sua peças (computador) foram compradas em

dados %>% 
  filter(!is.na(a_maioria_das_suas_pecas_foram_compradas_em)) %>% 
  group_by(a_maioria_das_suas_pecas_foram_compradas_em) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x = "", y = freq, fill = a_maioria_das_suas_pecas_foram_compradas_em)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = freq), nudge_y = -10, color = "white") +
  scale_fill_manual(values = c("#12f45f", "#800020")) +
  labs(x = "", y = "", 
       title = "A maioria das suas peças foram compradas em:") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.title = element_blank())

# Qual console você possui


# O seu console mais recente foi comprado em

dados %>% 
  filter(!is.na(o_seu_console_mais_recente_foi_comprado_em)) %>% 
  group_by(o_seu_console_mais_recente_foi_comprado_em) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x = "", y = freq, fill = o_seu_console_mais_recente_foi_comprado_em)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = freq), nudge_y = -10, color = "white") +
  scale_fill_manual(values = c("#12f45f", "#800020")) +
  labs(x = "", y = "", 
       title = "O seu console mais recente foi comprado em:") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.title = element_blank())

# Qual seu estilo de jogo favorito


# Qual seu jogo preferido


# Em media, quanto costuma gastas mensalmente com jogos

dados %>% 
  filter(!is.na(em_media_quanto_costuma_gastar_mensalmente_com_jogos)) %>% 
  group_by(em_media_quanto_costuma_gastar_mensalmente_com_jogos) %>% 
  summarise(freq = n()) %>% 
  mutate(em_media_quanto_costuma_gastar_mensalmente_com_jogos = 
           factor(em_media_quanto_costuma_gastar_mensalmente_com_jogos, 
                  levels = c("Nada", "R$1 a R$50", "R$51 a R$100", "R$101 a R$150", "R$151 a R$200", "mais de R$200"))) %>% 
  ggplot(aes(x = em_media_quanto_costuma_gastar_mensalmente_com_jogos, y = freq)) +
  geom_bar(stat = 'identity', fill = "#800000") +
  geom_text(aes(label = freq), nudge_y = -10, color = "white") +
  labs(x = "", y = "Frequência Absoluta", title = "Em média, quanto costuma gastar mensalmente com jogos?") +
  theme_minimal()















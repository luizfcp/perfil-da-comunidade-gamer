
# jogos brasil, xbox one br, jogos mobile br/pt, uff niteroi, ilha dos estudos, ilha da macacada

#http://dsgeek.com/2014/09/19/Customizingggplot2charts.html

# OBS: Colocar titulo nas figuras pra apresentar, mas remover o titulo no relatorio, o titulo deve vir escrito abaixo da figura

# Pacotes -----------------------------------------------------------------

library(readxl)
library(janitor)
library(dplyr)
library(ggplot2)
library(magrittr)
library(scales)
library(tidyr)
library(stringr)
library(egg)
library(purrr)

# Bases -------------------------------------------------------------------

raw_data <- read_excel("data/Perfil da Comunidade Gamer (respostas).xlsx") %>% clean_names

dados <- raw_data %>% 
  filter(com_que_frequencia_costuma_jogar_games!="Nunca" & idade_anos > 9)


# Visualizacoes -----------------------------------------------------------


# Com que frequencia costuma jogar games? ---------------------------------

raw_data %>% 
  group_by(com_que_frequencia_costuma_jogar_games) %>% 
  summarise(freq = n()) %>% 
  ggplot(., aes(x = reorder(com_que_frequencia_costuma_jogar_games, -freq), y = freq)) +
  geom_bar(stat = 'identity', fill = "#0f0559") +
  labs(x = "", y = "Frequência Absoluta", title = "Com que frequência costuma jogar games?") +
  geom_label(aes(label = freq)) +
  annotate("text", label = paste("Total de respostas =", 911), x = 3.5, y = 600, size = 4)


# Genero ------------------------------------------------------------------

dados %>% 
  group_by(genero) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x = genero, y = freq)) +
  geom_bar(aes(fill = genero), stat = 'identity') +
  geom_label(aes(label = freq)) +
  scale_fill_manual(values = c("royalblue", "pink", "orange")) +
  labs(x = "", y = "Frequência Absoluta", title = "Gênero") +
  theme(legend.position = "None")


# Idade por Genero --------------------------------------------------------

dados %>% 
  select(idade_anos, genero) %>% 
  ggplot(aes(x = genero, y = idade_anos)) +
  geom_boxplot(aes(fill = genero)) +
  scale_fill_manual(values = c("royalblue", "pink", "orange")) +
  labs(x = "", y = "Idade", title = "Idade (anos)") +
  theme(legend.position = "None")


# Situacao atual ----------------------------------------------------------

dados %>% 
  group_by(situacao_atual) %>% 
  summarise(freq = n()) %>%
  ggplot(aes(x = reorder(situacao_atual, -freq), y = freq)) +
  geom_bar(stat = 'identity', fill = "#800000") +
  geom_label(aes(label = freq)) +
  labs(x = "", y = "Frequência Absoluta", title = "Situação atual") +
  coord_flip()

# ## Paretto
# dados %>% 
#   group_by(situacao_atual) %>% 
#   summarise(freq = n()) %>%
#   ungroup() %>% 
#   arrange(-freq) %>% 
#   mutate(acumulado = cumsum(freq)) %>% 
#   ggplot(aes(x = reorder(situacao_atual, -freq))) +
#   geom_bar(aes(y = freq), stat = 'identity', fill = "#800000") +
#   geom_point(aes(y = acumulado)) +
#   geom_path(aes(y = acumulado, group = 1)) +
#   geom_text(aes(y = freq, label = freq), nudge_y = -20, color = "white") +
#   labs(x = "", y = "Frequência Absoluta", title = "Situação atual") +
#   coord_flip()


# Grau de escolaridade  ---------------------------------------------------

dados %>% 
  group_by(grau_de_escolaridade) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x = grau_de_escolaridade, y = freq)) +
  geom_bar(stat = 'identity', fill = "#800000") +
  geom_label(aes(label = freq)) +
  labs(x = "", y = "Frequência Absoluta", title = "Grau de escolaridade") +
  coord_flip() 


# Qual plataforma prefere usar para jogar ---------------------------------

dados %>% 
  group_by(qual_plataforma_prefere_usar_para_jogar) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x = reorder(qual_plataforma_prefere_usar_para_jogar, -freq), y = freq)) +
  geom_bar(stat = 'identity', fill = "#800000") +
  geom_label(aes(label = freq)) +
  labs(x = "", y = "Frequência Absoluta", title = "Qual plataforma prefere usar para jogar") +
  coord_flip() 

# ## Paretto
# dados %>% 
#   group_by(qual_plataforma_prefere_usar_para_jogar) %>% 
#   summarise(freq = n()) %>%
#   ungroup() %>% 
#   arrange(-freq) %>% 
#   mutate(acumulado = cumsum(freq)) %>% 
#   ggplot(aes(x = reorder(qual_plataforma_prefere_usar_para_jogar, -freq))) +
#   geom_bar(aes(y = freq), stat = 'identity', fill = "#800000") +
#   geom_point(aes(y = acumulado)) +
#   geom_path(aes(y = acumulado, group = 1)) +
#   geom_text(aes(y = freq, label = freq), nudge_y = -20, color = "white") +
#   labs(x = "", y = "Frequência Absoluta", title = "Qual plataforma prefere usar para jogar") +
#   coord_flip()


# Voce conhece os componentes do seu computador ---------------------------

dados %>% 
  filter(!is.na(voce_conhece_os_componentes_do_seu_computador_processador_placa_de_video_e_memoria_ram)) %>% 
  group_by(voce_conhece_os_componentes_do_seu_computador_processador_placa_de_video_e_memoria_ram) %>% 
  summarise(freq = n()) %>%
  ungroup() %>% 
  mutate(ypos = (cumsum(freq) - 0.5*freq)+10,
         prop = freq/sum(freq),
         prop_lab = paste0(freq, "\n", percent(prop)),
         voce_conhece_os_componentes_do_seu_computador_processador_placa_de_video_e_memoria_ram = 
           factor(voce_conhece_os_componentes_do_seu_computador_processador_placa_de_video_e_memoria_ram, levels = c("Sim", "Não"))) %>% 
  ggplot(aes(x = "", y = freq, fill = voce_conhece_os_componentes_do_seu_computador_processador_placa_de_video_e_memoria_ram)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(y = ypos, label = prop_lab), nudge_y = -10, color = "white") +
  scale_fill_manual(values = c("#800020", "#247f59")) +
  labs(x = "", y = "", 
       title = "Você conhece os componentes do seu computador \n (Processador, Placa de Vídeo e Memória RAM) ?") +
  coord_polar("y") +
  theme_void() +
  theme(legend.title = element_blank())


# Quantos gigabytes de memoria RAM ----------------------------------------

dados %>% 
  filter(!is.na(quantos_gigabytes_de_memoria_ram)) %>% 
  group_by(quantos_gigabytes_de_memoria_ram) %>% 
  summarise(freq = n()) %>% 
  mutate(quantos_gigabytes_de_memoria_ram = factor(quantos_gigabytes_de_memoria_ram, levels = c(paste(c(1,2,4,6,8,12,16, "mais de 16"), "GB")))) %>% 
  ggplot(aes(x = quantos_gigabytes_de_memoria_ram, y = freq)) +
  geom_bar(stat = 'identity', fill = "#800000") +
  geom_label(aes(label = freq)) +
  labs(x = "", y = "Frequência Absoluta", title = "Quantos Gigabytes de memória RAM?")


# Qual a marca do seu porcessador -----------------------------------------

dados %>% 
  filter(!is.na(qual_a_marca_do_seu_processador)) %>% 
  group_by(qual_a_marca_do_seu_processador) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  mutate(ypos = (cumsum(freq) - 0.5*freq),
         prop = freq/sum(freq),
         prop_lab = paste0(freq, "\n", percent(prop)),
         qual_a_marca_do_seu_processador = factor(qual_a_marca_do_seu_processador, levels = c("Intel", "AMD"))) %>% 
  ggplot(aes(x = "", y = freq, fill = qual_a_marca_do_seu_processador)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(y = ypos, label = prop_lab), nudge_y = -10, color = "white") +
  scale_fill_manual(values = c("#800020", "#247f59")) +
  labs(x = "", y = "", 
       title = "Qual a marca do seu porcessador?") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.title = element_blank())


# Qual a marca da sua placa de video --------------------------------------

dados %>% 
  filter(!is.na(qual_a_marca_da_sua_placa_de_video)) %>% 
  group_by(qual_a_marca_da_sua_placa_de_video) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  mutate(ypos = (cumsum(freq) - 0.5*freq)+8,
         prop = freq/sum(freq),
         prop_lab = paste0(freq, "\n", percent(prop)),
         qual_a_marca_da_sua_placa_de_video = factor(qual_a_marca_da_sua_placa_de_video, levels = c("Nvidia", "AMD"))) %>% 
  ggplot(aes(x = "", y = freq, fill = qual_a_marca_da_sua_placa_de_video)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(y = ypos, label = prop_lab), nudge_y = -10, color = "white") +
  scale_fill_manual(values = c("#800020", "#247f59")) +
  labs(x = "", y = "", 
       title = "Qual a marca da sua placa de video?") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.title = element_blank())


# # Quantos gigabtes possui sua placa de video ----------------------------

# ## quem colocou 1060 2 ou 4 gb mudar pra 3
# 
# ######### final


# A maoria das sua peças (computador) foram compradas em ------------------

dados %>% 
  filter(!is.na(a_maioria_das_suas_pecas_foram_compradas_em)) %>% 
  group_by(a_maioria_das_suas_pecas_foram_compradas_em) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  mutate(ypos = (cumsum(freq) - 0.5*freq),
         prop = freq/sum(freq),
         prop_lab = paste0(freq, "\n", percent(prop)),
         a_maioria_das_suas_pecas_foram_compradas_em = factor(a_maioria_das_suas_pecas_foram_compradas_em, levels = paste("Loja", c("virtual", "física")))) %>% 
  ggplot(aes(x = "", y = freq, fill = a_maioria_das_suas_pecas_foram_compradas_em)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(y = ypos, label = prop_lab), nudge_y = -10, color = "white") +
  scale_fill_manual(values = c("#800020", "#247f59")) +
  labs(x = "", y = "", 
       title = "A maioria das suas peças foram compradas em:") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.title = element_blank())


# Qual console você possui ------------------------------------------------

dados %>% 
  select(qual_console_voce_possui) %>% 
  filter(!is.na(qual_console_voce_possui)) %>% 
  mutate(
    qual_console_voce_possui = qual_console_voce_possui %>% 
      str_to_lower() %>% 
      str_replace_all(" e ", ", ") %>%
      str_replace_all(" ", "_") %>%
      str_replace_all(",", " ") %>% 
      str_split(" ")
  ) %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(value = value %>% str_replace_all("_", " ") %>% str_trim()) %>% 
  filter(str_detect(value, "^a|^d|^master|^3|^super|^nin|^switch|^w|^pl|^ps|^x|^game") & 
           !str_detect(value, "^xbox$|^playstation$|^nintendo$")) %>% 
  mutate(
    value = case_when(
      str_detect(value, "^wii$")                 ~ str_replace_all(value, "wii", "nintendo wii"),
      str_detect(value, "^wii u$")               ~ str_replace_all(value, "wii u", "nintendo wii u"),
      str_detect(value, "^nintendo wii$")        ~ str_replace_all(value, "nintendo wii", "nintendo wii"),
      str_detect(value, "^nintendo wiiu$")       ~ str_replace_all(value, "nintendo wiiu", "nintendo wii u"),
      str_detect(value, "^nintendo wii u$")      ~ str_replace_all(value, "nintendo wii u", "nintendo wii u"),
      str_detect(value, "^switch$")              ~ str_replace_all(value, "switch", "nintendo switch"),
      str_detect(value, "^nintendo ds$")         ~ str_replace_all(value, "nintendo ds", "nintendo ds"),
      str_detect(value, "^dsi$")                 ~ str_replace_all(value, "dsi", "nintendo dsi"),
      str_detect(value, "^3ds xl$")              ~ str_replace_all(value, "3ds xl", "nintendo 3ds xl"),
      str_detect(value, "^3do$")                 ~ str_replace_all(value, "3do", "nintendo 3ds"),
      str_detect(value, "^3ds$")                 ~ str_replace_all(value, "3ds", "nintendo 3ds"),
      str_detect(value, "^3ds \\(hackeado\\)$")  ~ str_replace_all(value, "3ds \\(hackeado\\)", "nintendo 3ds"),
      str_detect(value, "^nintendo 3ds$")        ~ str_replace_all(value, "nintendo 3ds", "nintendo 3ds"),
      str_detect(value, "^nintendo64$")          ~ str_replace_all(value, "nintendo64", "nintendo 64"),
      str_detect(value, "^gamecube$")            ~ str_replace_all(value, "gamecube", "nintendo gamecube"),
      str_detect(value, "^master sistem$")       ~ str_replace_all(value, "master sistem", "master system 1"),
      str_detect(value, "^master system$")       ~ str_replace_all(value, "master system", "master system 1"),
      str_detect(value, "^master system 2$")     ~ str_replace_all(value, "master system 2", "master system 2"),
      str_detect(value, "^ps1$")                 ~ str_replace_all(value, "ps1", "playstation 1"),
      str_detect(value, "^ps2$")                 ~ str_replace_all(value, "ps2", "playstation 2"),
      str_detect(value, "^play 1$")              ~ str_replace_all(value, "play 1", "playstation 1"),
      str_detect(value, "^dreamcast$")           ~ str_replace_all(value, "dreamcast", "sega dreamcast"),
      str_detect(value, "^game boy$")            ~ str_replace_all(value, "game boy", "game boy"),
      str_detect(value, "^game boy advance$")    ~ str_replace_all(value, "game boy advance", "game boy advance"),
      str_detect(value, "^game boy color$")      ~ str_replace_all(value, "game boy color", "game boy color"),
      TRUE ~ value
    )
  ) %>% 
  group_by(value) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  mutate(value = value %>% factor(., levels = sort(., decreasing = T))) %>% 
  ggplot(aes(x = value, y = freq)) +
  geom_bar(stat = 'identity', fill = "#800000") +
  labs(x = "Console (Video Game)", y = "Frequência Absoluta", 
       title = "Console que os respospondentes disseram possuir") +
  coord_flip() +
  geom_label(aes(label = freq))


# O seu console mais recente foi comprado em ------------------------------

dados %>% 
  filter(!is.na(o_seu_console_mais_recente_foi_comprado_em)) %>% 
  group_by(o_seu_console_mais_recente_foi_comprado_em) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  mutate(ypos = (cumsum(freq) - 0.5*freq),
         prop = freq/sum(freq),
         prop_lab = paste0(freq, "\n", percent(prop)),
         o_seu_console_mais_recente_foi_comprado_em = factor(o_seu_console_mais_recente_foi_comprado_em, levels = paste("Loja", c("virtual", "física")))) %>% 
  ggplot(aes(x = "", y = freq, fill = o_seu_console_mais_recente_foi_comprado_em)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(y = ypos, label = prop_lab), nudge_y = -10, color = "white") +
  scale_fill_manual(values = c("#800020", "#247f59")) +
  labs(x = "", y = "", 
       title = "O seu console mais recente foi comprado em:") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.title = element_blank())


# Qual seu estilo de jogo favorito ----------------------------------------

## As 3 plataformas
estilo_jogo_as3 <- 
  dados %>% 
  select(qual_seu_estilo_de_jogo_favorito) %>% 
  mutate(
    qual_seu_estilo_de_jogo_favorito = qual_seu_estilo_de_jogo_favorito %>% 
      str_to_lower() %>% 
      str_remove_all("\\(monster hunter") %>%
      str_replace_all(" e ", ", ") %>%
      # str_replace_all("/", ", ") %>%
      str_replace_all(" \\- ", ": ") %>%
      str_replace_all(" ", "_") %>%
      str_replace_all(",", " ") %>% 
      str_replace_all("\\.", " ") %>% 
      str_split(" ")
  ) %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(value = value %>% str_replace_all("_", " ") %>% str_trim()) %>%
  filter(!str_detect(value, "^jogos|4x|^sexo|etc|^todo|^dormir|^é|^harry|\\)")) %>%
  mutate(
    value = case_when(
      str_detect(value, "^rts") ~ "rts: estratégia em tempo real",
      # str_detect(value, "^ark$") ~ str_replace_all(value, "ark", "ark survival evolved"),
      TRUE ~ value
    )
  ) %>% 
  group_by(value) %>% 
  summarise(freq = n()) %>%
  arrange(-freq) %>% 
  head(10) %>% 
  ungroup() %>% 
  mutate(plataforma = "As 3 plataformas")

## Mobile
estilo_jogo_mobile <- 
  dados %>% 
  filter(qual_plataforma_prefere_usar_para_jogar=="Outros (Celular)") %>% 
  select(qual_seu_estilo_de_jogo_favorito) %>% 
  mutate(
    qual_seu_estilo_de_jogo_favorito = qual_seu_estilo_de_jogo_favorito %>% 
      str_to_lower() %>% 
      str_remove_all("\\(monster hunter") %>%
      str_replace_all(" e ", ", ") %>%
      # str_replace_all("/", ", ") %>%
      str_replace_all(" \\- ", ": ") %>%
      str_replace_all(" ", "_") %>%
      str_replace_all(",", " ") %>% 
      str_replace_all("\\.", " ") %>% 
      str_split(" ")
  ) %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(value = value %>% str_replace_all("_", " ") %>% str_trim()) %>%
  filter(!str_detect(value, "^jogos|4x|^sexo|etc|^todo|^dormir|^é|^harry|\\)")) %>%
  mutate(
    value = case_when(
      str_detect(value, "^rts") ~ "rts: estratégia em tempo real",
      # str_detect(value, "^ark$") ~ str_replace_all(value, "ark", "ark survival evolved"),
      TRUE ~ value
    )
  ) %>% 
  group_by(value) %>% 
  summarise(freq = n()) %>%
  arrange(-freq) %>% 
  head(10) %>% 
  ungroup() %>% 
  mutate(plataforma = "Outros (Celular)")

## Console
estilo_jogo_console <- 
  dados %>% 
  filter(qual_plataforma_prefere_usar_para_jogar=="Console (video game)") %>% 
  select(qual_seu_estilo_de_jogo_favorito) %>% 
  mutate(
    qual_seu_estilo_de_jogo_favorito = qual_seu_estilo_de_jogo_favorito %>% 
      str_to_lower() %>% 
      str_remove_all("\\(monster hunter") %>%
      str_replace_all(" e ", ", ") %>%
      # str_replace_all("/", ", ") %>%
      str_replace_all(" \\- ", ": ") %>%
      str_replace_all(" ", "_") %>%
      str_replace_all(",", " ") %>% 
      str_replace_all("\\.", " ") %>% 
      str_split(" ")
  ) %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(value = value %>% str_replace_all("_", " ") %>% str_trim()) %>%
  filter(!str_detect(value, "^jogos|4x|^sexo|etc|^todo|^dormir|^é|^harry|\\)")) %>%
  mutate(
    value = case_when(
      str_detect(value, "^rts") ~ "rts: estratégia em tempo real",
      # str_detect(value, "^ark$") ~ str_replace_all(value, "ark", "ark survival evolved"),
      TRUE ~ value
    )
  ) %>% 
  group_by(value) %>% 
  summarise(freq = n()) %>%
  arrange(-freq) %>% 
  head(10) %>% 
  ungroup() %>% 
  mutate(plataforma = "Console (video game)")

## PC
estilo_jogo_pc <- 
  dados %>% 
  filter(qual_plataforma_prefere_usar_para_jogar %in% c("Desktop (PC de mesa)", "Notebook")) %>%
  select(qual_seu_estilo_de_jogo_favorito) %>% 
  mutate(
    qual_seu_estilo_de_jogo_favorito = qual_seu_estilo_de_jogo_favorito %>% 
      str_to_lower() %>% 
      str_remove_all("\\(monster hunter") %>%
      str_replace_all(" e ", ", ") %>%
      # str_replace_all("/", ", ") %>%
      str_replace_all(" \\- ", ": ") %>%
      str_replace_all(" ", "_") %>%
      str_replace_all(",", " ") %>% 
      str_replace_all("\\.", " ") %>% 
      str_split(" ")
  ) %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(value = value %>% str_replace_all("_", " ") %>% str_trim()) %>%
  filter(!str_detect(value, "^jogos|4x|^sexo|etc|^todo|^dormir|^é|^harry|\\)")) %>%
  mutate(
    value = case_when(
      str_detect(value, "^rts") ~ "rts: estratégia em tempo real",
      # str_detect(value, "^ark$") ~ str_replace_all(value, "ark", "ark survival evolved"),
      TRUE ~ value
    )
  ) %>% 
  group_by(value) %>% 
  summarise(freq = n()) %>%
  arrange(-freq) %>% 
  head(10) %>% 
  ungroup() %>% 
  mutate(plataforma = "Desktop/Notebook")

plot <- estilo_jogo_mobile %>% 
  bind_rows(estilo_jogo_console) %>%
  bind_rows(estilo_jogo_pc) %>%
  bind_rows(estilo_jogo_as3) %>%
  nest(-plataforma) %>% 
  mutate(
    plot = map2(
      plataforma, data,
      ~ .y %>% 
        ggplot(aes(x = reorder(value, -freq), y = freq)) +
        geom_bar(stat = "identity", fill = "#800000") +
        coord_flip() +
        labs(x = "", y = "", subtitle = .x) +
        theme_minimal() +
        geom_label(aes(label = freq))
    )
  ) %$% plot

ggarrange(plots=plot, left="Estilo de jogo", bottom="Frequência Absoluta", 
          top="Estilo de jogo preferido dos respondentes separados por plataforma", ncol=2)


# Qual seu jogo preferido -------------------------------------------------

## Mobile
jogo_preferido_mobile <- 
  dados %>% 
  filter(qual_plataforma_prefere_usar_para_jogar=="Outros (Celular)") %>%
  select(qual_seu_jogo_preferido) %>% 
  mutate(
    qual_seu_jogo_preferido = qual_seu_jogo_preferido %>% 
      str_to_lower() %>% 
      str_remove_all("\\(atualmente\\)|mas gosto muito de") %>%
      str_replace_all(" e ", ", ") %>%
      str_replace_all(" ou ", ", ") %>%
      str_replace_all(" ", "_") %>%
      str_replace_all(",", " ") %>% 
      str_split(" ")
  ) %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(value = value %>% str_replace_all("_", " ") %>% str_trim()) %>% 
  filter(!str_detect(value, "não possuo|são muitos|não sei|nenhum|estou sem computador")) %>%
  mutate(
    value = case_when(
      str_detect(value, "^coc$")                 ~ str_replace_all(value, "coc", "clash of clans"),
      str_detect(value, "^yu-gi-oh duel links$") ~ str_replace_all(value, "yu-gi-oh duel links", "yu-gi-oh! duel links"),
      str_detect(value, "^dls 2019$")            ~ str_replace_all(value, "dls 2019", "dream league soccer 2019"),
      str_detect(value, "^pugb$")                ~ str_replace_all(value, "pugb", "pubg"),
      TRUE ~ value
    )
  ) %>% 
  group_by(value) %>% 
  summarise(freq = n()) %>%
  arrange(-freq) %>% 
  head(10) %>% 
  ungroup() %>% 
  mutate(plataforma = "Outros (Celular)")

## Console
jogo_preferido_console <-
  dados %>% 
  filter(qual_plataforma_prefere_usar_para_jogar=="Console (video game)") %>%
  select(qual_seu_jogo_preferido) %>% 
  mutate(
    qual_seu_jogo_preferido = qual_seu_jogo_preferido %>% 
      str_to_lower() %>% 
      str_remove_all("varios \\. mais atualmente|\\.|\\(2018\\)|\\(mega drive\\)|franquia|em geral|no momento|\\(snes\\)|\\(todos\\)|qualquer|atualmente|\\(nintendo 64\\)|no xbox\\:|principalmente") %>%
      str_replace_all(" e ", ", ") %>%
      str_replace_all(" ou ", ", ") %>%
      str_replace_all(" / ", ", ") %>%
      str_replace_all(" & ", ", ") %>%
      str_replace_all(" ", "_") %>%
      str_replace_all(",", " ") %>% 
      str_split(" ")
  ) %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(value = value %>% str_replace_all("_", " ") %>% str_trim()) %>% 
  filter(!str_detect(value, "^qual|pc\\)|^amanhã|^não|nenhum|atualmente nenhum|^mirtal|^todos|^legacy|gosto de todos|^mário$|^2$|^pergunta|^que eu sempre|^difícil")) %>% 
  mutate(
    value = case_when(
      str_detect(value, "^zelda$")                                    ~ str_replace_all(value, "zelda", "the legend of zelda"),
      str_detect(value, "^a  de zelda$")                              ~ str_replace_all(value, "a  de zelda", "the legend of zelda"),
      str_detect(value, "^zelda breath of the wild$")                 ~ str_replace_all(value, "zelda breath of the wild", "the legend of zelda: breath of the wild"),
      str_detect(value, "^the legende of zelda: ocarina of time$")    ~ str_replace_all(value, "the legende of zelda: ocarina of time", "the legend of zelda: ocarina of time"),
      str_detect(value, "^zelda ocarina of time$")                    ~ str_replace_all(value, "zelda ocarina of time", "the legend of zelda: ocarina of time"),
      str_detect(value, "^the legend of zelda: ocarina of the time$") ~ str_replace_all(value, "the legend of zelda: ocarina of the time", "the legend of zelda: ocarina of time"),
      str_detect(value, "^the legend of zelda - twilight princess$")  ~ str_replace_all(value, "the legend of zelda - twilight princess", "the legend of zelda: twilight princess"),
      str_detect(value, "^o tony hawk pro skater 4$")                 ~ str_replace_all(value, "o tony hawk pro skater 4", "tony hawk pro skater 4"),
      str_detect(value, "^assassin\\´s creed$")                       ~ str_replace_all(value, "assassin\\´s creed", "assassins creed"),
      str_detect(value, "^assassin\\'s creed$")                       ~ str_replace_all(value, "assassin\\'s creed", "assassins creed"),
      str_detect(value, "^assasins creed$")                           ~ str_replace_all(value, "assasins creed", "assassins creed"),
      str_detect(value, "^ac black flag$")                            ~ str_replace_all(value, "ac black flag", "assassins creed black flag"),
      str_detect(value, "^rdr1$")                                     ~ str_replace_all(value, "rdr1", "red dead redemption 1"),
      str_detect(value, "^rdr2$")                                     ~ str_replace_all(value, "rdr2", "red dead redemption 2"),
      str_detect(value, "^read dead 2$")                              ~ str_replace_all(value, "read dead 2", "red dead redemption 2"),
      str_detect(value, "^rede dead redemption 2$")                   ~ str_replace_all(value, "rede dead redemption 2", "red dead redemption 2"),
      str_detect(value, "^nba2k19$")                                  ~ str_replace_all(value, "nba2k19", "nba 2k19"),
      str_detect(value, "^bf$")                                       ~ str_replace_all(value, "bf", "battlefield"),
      str_detect(value, "^bf4$")                                      ~ str_replace_all(value, "bf4", "battlefield 4"),
      str_detect(value, "^grand theft auto$")                         ~ str_replace_all(value, "grand theft auto", "gta"),
      str_detect(value, "^grand thef auto v$")                        ~ str_replace_all(value, "grand thef auto v", "gta v"),
      str_detect(value, "^grand theft auto v$")                       ~ str_replace_all(value, "grand theft auto v", "gta v"),
      str_detect(value, "^gta 5$")                                    ~ str_replace_all(value, "gta 5", "gta v"),
      str_detect(value, "^r6$")                                       ~ str_replace_all(value, "r6", "rainbow six siege"),
      str_detect(value, "^raimbow six$")                              ~ str_replace_all(value, "raimbow six", "rainbow six siege"),
      str_detect(value, "^rainbow six$")                              ~ str_replace_all(value, "rainbow six", "rainbow six siege"),
      str_detect(value, "^rainbown six$")                             ~ str_replace_all(value, "rainbown six", "rainbow six siege"),
      str_detect(value, "^rainbow six suege$")                        ~ str_replace_all(value, "rainbow six suege", "rainbow six siege"),
      str_detect(value, "^rainbowsixsiege$")                          ~ str_replace_all(value, "rainbowsixsiege", "rainbow six siege"),
      str_detect(value, "^wow$")                                      ~ str_replace_all(value, "wow", "world of warcraft"),
      str_detect(value, "^dark souls 1$")                             ~ str_replace_all(value, "dark souls 1", "dark souls"),
      str_detect(value, "^destiny 1/2$")                              ~ str_replace_all(value, "destiny 1/2", "destiny 2"),
      str_detect(value, "^hoje é apex$")                              ~ str_replace_all(value, "hoje é apex", "apex legends"),
      str_detect(value, "^apex$")                                     ~ str_replace_all(value, "apex", "apex legends"),
      str_detect(value, "^twd$")                                      ~ str_replace_all(value, "twd", "the walking dead"),
      str_detect(value, "^tomb rider$")                               ~ str_replace_all(value, "tomb rider", "tomb raider"),
      str_detect(value, "^cod$")                                      ~ str_replace_all(value, "cod", "call of duty"),
      str_detect(value, "^calor of dutty$")                           ~ str_replace_all(value, "calor of dutty", "call of duty"),
      str_detect(value, "^black ops ii$")                             ~ str_replace_all(value, "black ops ii", "call of duty: black ops ii"),
      str_detect(value, "^news for speed most wanted$")               ~ str_replace_all(value, "news for speed most wanted", "need for speed most wanted"),
      str_detect(value, "^the elder scrools v - skyrim$")             ~ str_replace_all(value, "the elder scrools v - skyrim", "the elder scrolls v: skyrim"),
      str_detect(value, "^the witcher 3$")                            ~ str_replace_all(value, "the witcher 3", "the witcher 3 wild hunter"),
      str_detect(value, "^the wicher 3 wild hunter$")                 ~ str_replace_all(value, "the wicher 3 wild hunter", "the witcher 3 wild hunter"),
      str_detect(value, "^the witcher 3 wild hunt$")                  ~ str_replace_all(value, "the witcher 3 wild hunt", "the witcher 3 wild hunter"),
      str_detect(value, "^fifa soccer$")                              ~ str_replace_all(value, "fifa soccer", "fifa"),
      str_detect(value, "^f1 2018$")                                  ~ str_replace_all(value, "f1 2018", "formula 1"),
      str_detect(value, "^forza$")                                    ~ str_replace_all(value, "forza", "forza horizon"),
      str_detect(value, "^teken$")                                    ~ str_replace_all(value, "teken", "tekken"),
      str_detect(value, "^pokemon$")                                  ~ str_replace_all(value, "pokemon", "pokémon"),
      str_detect(value, "^megaman x$")                                ~ str_replace_all(value, "megaman x", "mega man x"),
      str_detect(value, "^kingdonm hearts 3$")                        ~ str_replace_all(value, "kingdonm hearts 3", "kingdom hearts 3"),
      TRUE ~ value
    )
  ) %>% 
  group_by(value) %>% 
  summarise(freq = n()) %>%
  arrange(-freq) %>% 
  head(10) %>% 
  ungroup() %>% 
  mutate(plataforma = "Console (video game)")

## PC
jogo_preferido_pc <-
  dados %>% 
  filter(qual_plataforma_prefere_usar_para_jogar %in% c("Desktop (PC de mesa)", "Notebook")) %>%
  select(qual_seu_jogo_preferido) %>% 
  mutate(
    qual_seu_jogo_preferido = qual_seu_jogo_preferido %>% 
      str_to_lower() %>%
      str_remove_all("saga|offline|online|no monento eu jogo|qualquer|!|no momento|pq e gratis <3|\\.|atualmente|a serie| o |entre|entre outros|franquia|competitivo - |casual - |\\(jogo muitos\\, não tenho um preferido\\) tem tempo q jogo mais uns e tempo q jogo outros|") %>%
      str_replace_all(" e ", ", ") %>%
      str_replace_all(" ou ", ", ") %>%
      str_replace_all(" / ", ", ") %>%
      str_replace_all(" // ", ", ") %>%
      str_replace_all("/", ", ") %>%
      str_replace_all(" & ", ", ") %>%
      str_replace_all("; ", ", ") %>%
      str_replace_all(" ", "_") %>%
      str_replace_all(",", " ") %>% 
      str_split(" ")
  ) %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(value = value %>% str_replace_all("_", " ") %>% str_trim()) %>% 
  filter(!str_detect(value, "^cada|etc|minha cama|mas |masmelhor|^não|^nao|^num|^nem|^nenhum|^todos|^outros|^gosto|^l2")) %>% 
  mutate(
    value = case_when(
      str_detect(value, "^ark$") ~ str_replace_all(value, "ark", "ark survival evolved"),
      str_detect(value, "ark survival evolved^$") ~ str_replace_all(value, "ark survival evolved", "ark survival evolved"),
      str_detect(value, "^assasin\\'s creed$") ~ str_replace_all(value, "assasin\\'s creed", "assasins creed"),
      str_detect(value, "^bf4$") ~ str_replace_all(value, "bf4", "battlefield 4"),
      str_detect(value, "^cod bo2$") ~ str_replace_all(value, "cod bo2", "call of duty: black ops ii"),
      str_detect(value, "^cod black ops ii$") ~ str_replace_all(value, "cod black ops ii", "call of duty: black ops ii"),
      str_detect(value, "^counter") ~ "counter-strike: global offensive",
      str_detect(value, "^cs$") ~ "counter-strike",
      str_detect(value, "^cs") ~ "counter-strike: global offensive",
      str_detect(value, "^pubg") ~ "pubg",
      str_detect(value, "^dota2$") ~ str_replace_all(value, "dota2", "dota 2"),
      str_detect(value, "^ets2$") ~ str_replace_all(value, "ets2", "euro truck simulator 2"),
      str_detect(value, "^fallout new vegas$") ~ str_replace_all(value, "fallout new vegas", "fallout: new vegas"),
      str_detect(value, "^fortinite$") ~ str_replace_all(value, "fortinite", "fortnite"),
      str_detect(value, "^grand fantasia gfpt$") ~ str_replace_all(value, "grand fantasia gfpt", "	grand fantasia"),
      str_detect(value, "^grande theft auto 5$") ~ str_replace_all(value, "grande theft auto 5", "gta v"),
      str_detect(value, "^grand theft auto v$") ~ str_replace_all(value, "grand theft auto v", "gta v"),
      str_detect(value, "^gta 5") ~ "gta v",
      str_detect(value, "^gta:mta$") ~ str_replace_all(value, "gta:mta", "gta"),
      str_detect(value, "^gtav$") ~ str_replace_all(value, "gtav", "gta v"),
      str_detect(value, "^half life$") ~ str_replace_all(value, "half life", "half-life"),
      str_detect(value, "^kh2$") ~ str_replace_all(value, "kh2", "kingdom hearts 2"),
      str_detect(value, "^lea") ~ "league of legends",
      str_detect(value, "^lol") ~ "league of legends",
      str_detect(value, "^monsters hunter") ~ str_replace_all(value, "monsters hunter", "monster hunter"),
      str_detect(value, "^monster hunter world") ~ str_replace_all(value, "monster hunter world", "monster hunter"),
      str_detect(value, "^wow") ~ "world of warcraft",
      str_detect(value, "^world os warcraft$") ~ str_replace_all(value, "world os warcraft", "world of warcraft"),
      str_detect(value, "^word of warcraft$") ~ str_replace_all(value, "word of warcraft", "world of warcraft"),
      str_detect(value, "^world  of warcraft$") ~ str_replace_all(value, "world  of warcraft", "world of warcraft"),
      str_detect(value, "^wot$") ~ str_replace_all(value, "wot", "world of tanks"),
      str_detect(value, "^worl of tanks$") ~ str_replace_all(value, "worl of tanks", "world of tanks"),
      str_detect(value, "^tíbia$") ~ str_replace_all(value, "tíbia", "tibia"),
      str_detect(value, "^witcher 3$") ~ str_replace_all(value, "witcher 3", "the witcher 3"),
      str_detect(value, "^zelda$") ~ str_replace_all(value, "the legend of zelda", "the legend of zelda"),
      str_detect(value, "^tes") ~ "the elder scrolls v: skyrim",
      str_detect(value, "^skyrim$") ~ str_replace_all(value, "skyrim", "the elder scrolls v: skyrim"),
      str_detect(value, "^r6$") ~ str_replace_all(value, "r6", "rainbow six siege"),
      str_detect(value, "^rainbow six$") ~ str_replace_all(value, "rainbow six", "rainbow six siege"),
      TRUE ~ value
    )
  ) %>% 
  group_by(value) %>% 
  summarise(freq = n()) %>%
  arrange(-freq) %>% 
  head(10) %>% 
  ungroup() %>% 
  mutate(plataforma = "Desktop/Notebook")
      
plot <- jogo_preferido_mobile %>% 
  bind_rows(jogo_preferido_console) %>%
  bind_rows(jogo_preferido_pc) %>%
  nest(-plataforma) %>% 
  mutate(
    plot = map2(
      plataforma, data,
      ~ .y %>% 
        ggplot(aes(x = reorder(value, -freq), y = freq)) +
        geom_bar(stat = "identity", fill = "#800000") +
        coord_flip() +
        labs(x = "", y = "", subtitle = .x) +
        theme_minimal() +
        geom_label(aes(label = freq))
    )
  ) %$% plot

ggarrange(plots=plot, left="Nome do Jogo", bottom="Frequência Absoluta", 
          top="Jogo preferido dos respondentes separados por plataforma", ncol=3)


# Em media, quanto costuma gastar mensalmente com jogos -------------------

dados %>% 
  filter(!is.na(em_media_quanto_costuma_gastar_mensalmente_com_jogos)) %>% 
  group_by(em_media_quanto_costuma_gastar_mensalmente_com_jogos) %>% 
  summarise(freq = n()) %>% 
  mutate(em_media_quanto_costuma_gastar_mensalmente_com_jogos = 
           factor(em_media_quanto_costuma_gastar_mensalmente_com_jogos, 
                  levels = c("Nada", "R$1 a R$50", "R$51 a R$100", "R$101 a R$150", "R$151 a R$200", "mais de R$200"))) %>% 
  ggplot(aes(x = em_media_quanto_costuma_gastar_mensalmente_com_jogos, y = freq)) +
  geom_bar(stat = 'identity', fill = "#800000") +
  geom_label(aes(label = freq)) +
  labs(x = "", y = "Frequência Absoluta", title = "Em média, quanto costuma gastar mensalmente com jogos?")


## tentar explicar com variaveis 



# # Merge placas de video PREÇO ---------------------------------------------
# 
# ## quem colocou 1060 2 ou 4 gb mudar pra 3
# 
# library(purrr)
# 
# temp <- 
#   dados %>% 
#   select(qual_plataforma_prefere_usar_para_jogar, qual_o_modelo_da_sua_placa_de_video, quantos_gigabytes_possui_sua_placa_de_video) %>% 
#   filter(!is.na(qual_o_modelo_da_sua_placa_de_video) & qual_plataforma_prefere_usar_para_jogar=="Desktop (PC de mesa)") %>% 
#   select(-qual_plataforma_prefere_usar_para_jogar) %>% 
#   mutate_at(1:2, ~ .x %>% str_to_lower()) %>% 
#   nest(- qual_o_modelo_da_sua_placa_de_video) %>% 
#   select(qual_o_modelo_da_sua_placa_de_video) %$% 
#   qual_o_modelo_da_sua_placa_de_video
# 
# aux <- 
#   temp[which(str_detect(temp, "^[ahrng17]"))][-which(str_detect(temp, "^[ahrng17]ã"))] %>% 
#   tibble(qual_o_modelo_da_sua_placa_de_video = .)
# 
# # dados %>% 
# #   mutate(qual_o_modelo_da_sua_placa_de_video = str_to_lower(qual_o_modelo_da_sua_placa_de_video)) %>% 
# #   inner_join(aux) %>% xlsx::write.xlsx("mergegpu2.xlsx", row.names = F)
# 
# base_aux <- read_excel("../data/baseGPU.xlsx")
# base_aux2 <- read_excel("../data/mergegpu2.xlsx") %>% 
#   mutate(qual_a_marca_da_sua_placa_de_video = str_to_lower(qual_a_marca_da_sua_placa_de_video))
# 
# base_com_gpu <- 
#   base_aux %>%
#   mutate(Gbs = ifelse(Gbs>8, "mais de 8", Gbs),
#          Gbs = paste(Gbs, "GB")) %>% 
#   unite("modelo_serie", Modelo, Serie, sep = " ") %>% 
#   left_join(base_aux2, by = c("Marca"="qual_a_marca_da_sua_placa_de_video",
#                               "modelo_serie"="qual_o_modelo_da_sua_placa_de_video", 
#                               "Gbs"="quantos_gigabytes_possui_sua_placa_de_video")) %>% 
#   filter(!is.na(carimbo_de_data_hora))
# 
# base_com_gpu %>% write.csv2("../data/base_com_gpu.csv", row.names = F)
# base_com_gpu %>% 
#   mutate(Preço = as.numeric(Preço)) %>% 
#   as.data.frame() %>% 
#   xlsx::write.xlsx2("../data/base_com_gpu.xlsx", row.names = F)


# # Testes Ki-Quadrado ------------------------------------------------------
# 
# ## Quanto gasta mensalmente
# ## ki quadrado com preço varivavel principal, com escolaridade, idade, sexo, situacao
# 
# ##http://www.leg.ufpr.br/lib/exe/fetch.php/disciplinas:ce001:teste_do_qui-quadrado.pdf
#   
# ki <- 
#   dados$em_media_quanto_costuma_gastar_mensalmente_com_jogos %>%
#   table(dados$grau_de_escolaridade) %>% 
#   chisq.test()
# 
# tab = 
#   table(
#     dados$em_media_quanto_costuma_gastar_mensalmente_com_jogos,
#     dados$grau_de_escolaridade
#   )
# 
# ki <- chisq.test(tab, correct = T)
# 
# ## Como p-value é menor que 0.05, rejeitamos H0 ou seja,
# ##  o gasto medio mensal é dependente do grau de escolaridade.


# Making off --------------------------------------------------------------

# Qual console você possui, qual placada de video você possui, qual seu jogo preferido (	dinossauro do chrome sem internet), qual estilo de jogo preferido, idade


## http://hutsons-hacks.info/pareto-chart-in-ggplot2
library(leaflet);library(tidyverse);library(rgdal);library(htmltools)

BD = read_csv("https://github.com/luizfcp/perfil-da-comunidade-gamer/raw/master/data/Perfil%20da%20Comunidade%20Gamer%20(respostas)%20-%20Respostas%20ao%20formul%C3%A1rio%201.csv")

estados = BD %>% 
  group_by(NM_ESTADO = `Estado de residência?`) %>% 
  summarise(freq = n()) %>% 
  filter(NM_ESTADO!="NA") %>% 
  mutate(NM_ESTADO = str_sub(.$NM_ESTADO,  end = str_length(.$NM_ESTADO)-4 )) %>%
  mutate(NM_ESTADO = str_to_upper(.$NM_ESTADO)) %>% 
  select(NM_ESTADO,freq) %>% 
  add_row(NM_ESTADO = "TOCANTINS", freq = 0) %>% 
  add_row(NM_ESTADO = "RORAIMA", freq = 0) %>% 
  mutate(NM_ESTADO = str_trim(NM_ESTADO))


mapa = readOGR(dsn="/cloud/project/shape",
               use_iconv = TRUE,
               layer="BRUFE250GC_SIR",
               encoding = "UTF-8")

mapa@data = mapa@data %>% 
  inner_join(estados,by = "NM_ESTADO")



bin = c(0,
        quantile(mapa@data$freq,0.25),
        quantile(mapa@data$freq,0.50),
        quantile(mapa@data$freq,0.75),
        quantile(mapa@data$freq,0.95),
        quantile(mapa@data$freq,1))
pal = colorBin(palette = "viridis", domain =  mapa@data$freq, bins = bin)
labels = paste0("<p>","Unidade de Federação: ",mapa@data$NM_ESTADO,"</p>",
                "<p>","Respondentes: ",round(mapa@data$freq,digits= 3),"</p>")

fig = leaflet(mapa) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addPolygons(
    weight = 1,
    smoothFactor = 0.5,
    color = "white",
    fillOpacity = 0.8,
    fillColor = pal(mapa@data$freq),
    highlightOptions = highlightOptions(color = "black", weight = 3,
                                        bringToFront = FALSE),
    label = lapply(labels, HTML)
  ) %>% 
  addLegend(pal = pal,
            values = mapa@data$freq,
            opacity = 0.7,
            position = "bottomright",
            title = "Respondentes")

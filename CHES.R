#### EJES DE LOS PARTIDOS ######################################################

## Librerías 

library(haven) # Para csv
library(tidyverse) # Sintaxis
library(ggplot2) # Para graficar
library(plotly) # Para graficar
library(ggalt) # Para graficar
library(ggrepel) # para graficar

## Vector de colores

paises <- c("Alemania"="#B180FF", "España"="purple", "Reino Unido"="#47009A",
            "Dinamarca"="#FF0000", "Suecia"="#FF6D6D", "Noruega"="#7A0202", "Finlandia"="#C76767", "Paises Bajos"="#C62D2D", "Belgica"="#FA9E9E", "Austria"="#8F0000", "Francia"="#FF1C00",
            "Italia"="darkorange")

## Datset

CHES_r <- read_csv("CHES_data.csv") %>% # Cargo la base de datos
  select(-electionyear) %>% # Desseleccionar el año de las elecciones
  mutate(country = case_when(country==2 ~ "Dinamarca", # Nombro los países
                             country==16 ~ "Suecia",
                             country==13 ~ "Austria",
                             country==1 ~ "Belgica",
                             country==14 ~ "Finlandia",
                             country==6 ~ "Francia",
                             country==10 ~ "Paises Bajos",
                             country==3 ~ "Alemania",
                             country==8 ~ "Italia",
                             country==5 ~ "España",
                             country==11 ~ "Reino Unido",
                             TRUE ~ NA_character_)) %>% # Los que no haya renombrado los mando a NA
  mutate(family = case_when(family==1 ~ "Radical Right", # Selección de aprtidos nombrando a aquellos que me interesan y el resto en NA
                            TRUE ~ NA_character_)) %>%
  filter(!is.na(country)) %>% # Eliminar los países en NA
  filter(family == "Radical Right") %>% # Filtrar a partidos de extrema derecha
  group_by(party) %>% # Agrupar por partido
  summarize(media_eje_cultural = mean(galtan, na.rm = TRUE), # Posición cultural media
            media_eje_economico = mean(lrecon, na.rm = TRUE), # Posición económica media
            max_voto = max(vote, na.rm = TRUE)) %>% # Coger el máximo de votos
  left_join(select(read_csv("CHES_data.csv"), party, family, country), by = "party") %>% # Unir por partido
  distinct(party, .keep_all = TRUE) %>% # Quedarme con un solo partido
  select(-family) # Deselecciono familia



## Gráfico

CHES_r %>%
  mutate(country = case_when(country==2 ~ "Dinamarca", # Selección de paíse
                             country==16 ~ "Suecia",
                             country==13 ~ "Austria",
                             country==1 ~ "Belgica",
                             country==14 ~ "Finlandia",
                             country==6 ~ "Francia",
                             country==10 ~ "Paises Bajos",
                             country==3 ~ "Alemania",
                             country==8 ~ "Italia",
                             country==5 ~ "España",
                             country==11 ~ "Reino Unido",
                             TRUE ~ NA_character_)) %>% # Todos los demás NA
  ggplot(aes(y = media_eje_cultural, x = media_eje_economico, label = party, color = country)) + # Establezco los ejes, las label y que el color sea por país
  geom_point(aes(size = max_voto), shape = 16, alpha = 0.3) + # Que el tamaño del punto sea el máximo de votos, forma 16 y transparencia 0.3
  labs(x = "Eje Económico", y = "Eje Cultural", color = "País", size = "Máximo Voto") + # Etiquetamos
  geom_hline(yintercept=5, linetype="solid", color="black") + # Dibujo el eje vertica
  geom_vline(xintercept=5, linetype="solid", color="black") + # Dibujo el eje horizontal
  theme_minimal() + # Personalizo a un tema limpio
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9, color="transparent")) + # Quiero la leyenda vacía
  scale_color_manual(values=paises, labels = c("Dinamarca", "Suecia", "Austria", # Establezco manualmente la leyenda para que sea de los colores que quiero
                                               "Belgica", "Finlandia", "Francia",
                                               "Paises Bajos", "Alemania", "Italia", 
                                               "España")) +
  guides(size = guide_legend(title = "Voto", color = "black", # Parámetros de la leyenda de tamaño
                             label.theme = element_text(color = "black")), # Color de la leyenda de tamaño
         color = guide_legend(title = "",
                              nrow = 3,
                              override.aes = list(label = c("Dinamarca", "Suecia", "Austria", 
                                                            "Belgica", "Finlandia", "Francia",
                                                            "Paises Bajos", "Alemania", "Italia", 
                                                            "España")))) +
  geom_text_repel(point.padding = NA, segment.color = NA, min.segment.length = 0.2) # Que me ponga las etiquetas en cada partido

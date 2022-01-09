library(tidyverse)
library(mxmaps)
library(scales)
library(viridis)

#get data from github repo
df <-read_csv("https://raw.githubusercontent.com/carranco-sga/Mexico-COVID-19/master/Mexico_COVID19_CTD.csv",
              show_col_types = FALSE)

#first filter
df_P <- df %>% select(-contains("_"))

#get last observation position
n <- df_P %>% nrow()

#get date 
fecha_ <- df_P$Fecha[n]

#get value
value <- unlist(df_P[n,] %>% select(2:33), use.names = FALSE) -
    unlist(df_P[n-1,] %>% select(2:33), use.names = FALSE)

# vector to tibble
df_plot<-as_tibble(value)

#add region
df_plot$region <- c(1:32)

#title
titulo <- sprintf("Casos de covid al %s \n total= %s",fecha_,format(sum(value),big.mark=","))

#init map
map<-mxstate_choropleth(
    df_plot, # data frame
    num_colors = 1 #single color
    )

#add title
map <- map + ggtitle(titulo)

#change fill to viridis
map <-map +
    scale_fill_viridis(
        "Casos",
        option = "magma",
        na.value = "black",
        breaks = breaks_log(base = 2),
        trans = "log"
    )


#add theme
map <- map +
    theme( 
        plot.title = element_text(
            color="blue", 
            size=22, 
            face="bold.italic",
            hjust = 0.5
            )
    )

#add caption
map <- map +
    labs(caption = "Datos oficiales de los Comunicados Técnicos Diarios (CTD)\npublicado por la Secretaría de Salud Federal.")

#map
map

#save map
ggsave(sprintf("casos_covid.jpg"), height = 4.9, width = 8.57,units = "in")

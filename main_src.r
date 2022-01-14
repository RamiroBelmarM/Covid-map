library(tidyverse)
library(mxmaps)
library(scales)
library(viridis)

#get data from github repo
df <-read_csv("https://raw.githubusercontent.com/carranco-sga/Mexico-COVID-19/master/Mexico_COVID19_CTD.csv",
              show_col_types = FALSE)

#save csv
write.csv(df, file=gzfile("datos_covid.csv.gz"))


#first filter
df_P <- df %>% select(-contains("_"))

#get last observation position
n <- df_P %>% nrow()

#get date 
fecha_ <- format(as.Date(df_P$Fecha[n]), "%d-%m-%Y")

#get value
value <- unlist(
        df_P[n,] %>% select(2:33), 
        use.names = FALSE
    ) - unlist(
            df_P[n-1,] %>% select(2:33), 
            use.names = FALSE
        )

# vector to tibble
df_plot<-as_tibble(value)

#add region
df_plot$region <- c(1:32)

#title
titulo <- sprintf(
    "Casos diarios de covid\n (%s = %s casos)",
    fecha_,
    format(
        sum(value),
        big.mark=","
        )
    )

#caption
cap_ <- labs(
    caption = 
        "Datos oficiales de los Comunicados Técnicos Diarios (CTD)\npublicado por la Secretaría de Salud Federal."
    )


#init map
map<-mxstate_choropleth(
    df_plot, # data frame
    num_colors = 1 #single color
    )

#add title
map <- map + 
    ggtitle(titulo)

#change fill to viridis
map <-map +
    scale_fill_viridis(
        "Casos",
        option = "magma",
        na.value = "black",
        breaks = breaks_log(base = 10),
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
    cap_

#map
map

#save map
ggsave(
    sprintf("casos_covid.jpg"), 
    height = 4.0, 
    width = 6.8,
    units = "in"
    )



#other plots of interest

#get data from main df
df_P <- df %>%  
    select(Fecha)

df_P$value <- df$Pos

#

# init plot
gg <- ggplot(df_P) +
    aes(x=Fecha, y= value) +
    geom_line(size=1)



#add title and labs
gg <- gg +
    ggtitle(
        sprintf(
            "Casos de covid acumulados a %s\n Acumulado = %s", 
            fecha_,
            format(
                df_P$value[n],
                big.mark=","
                ) 
            )
        ) +
    xlab("Fecha") +
    ylab("Observaciones")
    
#add theme
gg <- gg +
    theme_minimal() +
    theme(
        plot.title = element_text(
            color="red", 
            size=22, 
            face="bold.italic", 
            hjust = 0.5
            ),
        axis.title.x = element_text(
            color="blue", 
            size=14, 
            face="bold"
            ),
        axis.title.y = element_text(
            color="brown", 
            size=14, 
            face="bold"
            )
    )
    
#show plot
gg + cap_

#save map
ggsave(
    sprintf("acumulados_covid.jpg"),  
    height = 4.0, 
    width = 6.8,
    units = "in"
    )


# daily cases
# mutate df_P
df_P$diarios <- c(0, diff(df_P$value))

#init plot
gg <-ggplot(df_P) +
    aes(x=Fecha, y= diarios) +
    geom_point(color="brown")+
    geom_line()

#add labs and title
gg <- gg  +
    ggtitle(
        sprintf(
            "Historial de casos diarios de covid \n %s = %s casos", 
            fecha_,
            format(
                df_P$diarios[n],
                big.mark=","
                ) 
            )
        ) +
    xlab("Fecha") +
    ylab("Observaciones")


#add theme
gg <- gg + 
    theme_minimal() +
    theme(
        plot.title = element_text(
            color="red", 
            size=22, 
            face="bold.italic", 
            hjust = 0.5
            ),
        axis.title.x = element_text(
            color="blue", 
            size=14, 
            face="bold"
            ),
        axis.title.y = element_text(
            color="brown", 
            size=14, 
            face="bold"
            )
    )

#show plot
gg + cap_    

#save map
ggsave(
    sprintf("diarios_covid.jpg"), 
    height = 4.0, 
    width = 6.8,
    units = "in"
    )
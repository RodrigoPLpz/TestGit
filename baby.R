# Limpiamos espacio de trabajo.
rm (list = ls())
setwd("~")

# Instanciamos librerias.
require(foreign)
library(dplyr)
require(tidyverse)
require(readstata13)
require(reshape2)
require(readxl)
require(stats)
require(treemap)

# Declarar la ruta de la carpeta donde trabajamos.
dir1 <- "E:\\RodriGo\\Cursos\\devf\\intDS\\exercices\\baby\\Out"
dir2 <- "E:/RodriGo/Cursos/devf/intDS/exercices/baby/Graphs"

# Cargar la base de datos.
baby <- read.csv(paste(dir1,"daba.csv", sep = "/"), stringsAsFactors = FALSE)
# El total de bebes nacidos en 2016 fue de 2080253.

# Verificar la estrucutura de la base de datos
str(baby)
# El total de bebes nacidos para los estados de mi población de estudio es de 713384
# México ocupa el primer lugar en embarazo adolescente a nivel mundial. 
# ¿Cuál es el promedio de edad de una mujer que da a luz?
# ¿Qué estados tienen más mamás adolescentes?


# De los estados que seleccione, cuáles son los estados donde hay más nacidos.

tempo <- baby %>%
         select(id,entidad_nacimiento) %>%
         mutate(total = 1) %>%
         group_by(entidad_nacimiento) %>%
         summarise(total_nac = sum(total))

gr <- ggplot(tempo,aes(x = entidad_nacimiento, y = total_nac)) + geom_point(color="blue", size=5.5) +
      geom_text(aes(x = entidad_nacimiento, y = total_nac, label = paste(entidad_nacimiento, sep = ",")),
             data = tempo[tempo$total_nac>=1,], hjust=0, vjust=-2) +
      labs(title = "Número de nacimientos por Estado", subtitle = "2016", x="Estados", y="Total de nacidos") +
      theme(axis.text.x = element_blank())

ggsave(paste(dir2,"1.png",sep = "/"),plot = gr, width = 10, height = 10)      

# Cantidad de nacidos por entidad federativa y el promedio de edad de las mamás.

tempo <- baby %>%
         select(id,edad_madre,entidad_nacimiento) %>%
         mutate(total = 1) %>%
         group_by(entidad_nacimiento) %>%
         summarise(total_nac = sum(total), prom_edad = round(mean(edad_madre, na.rm = T),0))

gr <- ggplot(tempo,aes(x = entidad_nacimiento, y = total_nac)) + geom_bar(stat = "identity", fill = "#2c7fb8") +
      geom_text(aes(label = total_nac, vjust = -2)) + geom_text(aes(label = prom_edad, vjust = -0.5)) +
      labs(title = "Número de nacimientos y promedio de edad de las madres  por Estado", subtitle = "2016", x = "Estado", y = "Total de nacidos")

ggsave(paste(dir2,"2.png",sep = "/"), plot = gr, width = 10, height = 10)


# Por rango de edad y promedio de edad.

tempo <- baby %>%
         select(id,edad_madre,entidad_nacimiento,rango_edad) %>%
         mutate(total = 1) %>%
         group_by(rango_edad) %>%
         summarise(total_nac = sum(total), prom_edad = round(mean(edad_madre, na.rm = T),0)) %>%
         arrange(prom_edad)

gr <- ggplot(tempo[1:5,],aes(x = rango_edad, y = total_nac)) + geom_bar(stat = "identity", fill = "#0056d3") +
      geom_text(aes(label = total_nac, vjust = -2)) + geom_text(aes(label = prom_edad, vjust = -0.5)) +
      labs(title = "Rango de edad de las madres  por  cantidad de nacidos", subtitle = "2016", x = "Rango de edad", y = "Total nacidos")

ggsave(paste(dir2,"3.png",sep = "/"), plot = gr, width = 12, height = 10)

# Por rango de edad y entidad.

tempo <- baby %>%
         select(id, edad_madre, rango_edad, entidad_nacimiento) %>%
         mutate(total = 1) %>%
         group_by(entidad_nacimiento, rango_edad) %>%
         summarise(total_nac = sum(total)) %>%
         ungroup() %>%
         group_by(entidad_nacimiento) %>%
         mutate(totales = sum(total_nac), porcentaje = round(total_nac/totales*100,2))

gr <- ggplot(tempo, aes(x = entidad_nacimiento, y = porcentaje, fill = rango_edad)) +
      geom_bar(stat ="identity") +
      geom_text(aes(label = porcentaje), position = position_stack(vjust = 0.4), angle = 45) + 
      scale_fill_manual(values = c("#084466","#00a898","#5ebf8c","#b6d887","#fcc179","#cb2800")) +
      labs(title="Porcentaje de madres por rango de edad y Estado", subtitle = "2016", 
           x = "Estado", y = "% Rangos de edad", fill="Rangos de edad") +
      coord_flip() + theme_bw()

ggsave(paste(dir2, "4.png", sep="/"), plot=gr, width=12, height=10)

# Madres adolescentes de 12 - 20 años por entidad federativa.

tempo <- baby%>%
        select(id,edad_madre,entidad_nacimiento,cod_entidad) %>%
        filter(edad_madre > 11 & edad_madre < 21) %>%
        group_by(cod_entidad,entidad_nacimiento) %>%
        mutate(total = 1) %>%
        summarise(total_nac = sum(total)) %>%
        ungroup() %>%
        mutate(totales = sum(total_nac), porcentaje = round(total_nac/totales*100,2))
   
png(paste(dir2, "5.png", sep = "/"), width = 12, height = 12, units = "in",res = 300)
treemap(tempo, index ="entidad_nacimiento", vSize = "porcentaje", vColor = "index", type = "index",
        title = "Porcentaje de madres adolecentes de 12 a 20 años por Estado - 2016", palette = "RdYlBu",
        title.legend = "", border.col = "grey", border.lwd=0.5)
dev.off()

#svg(paste(dir2, "6.svg", sep = "/"), width = 12, height = 12)
#treemap(tempo, index ="entidad_nacimiento", vSize = "porcentaje", vColor = "index", type = "index",
        #title = "Porcentaje de mámas adolecentes por estado - 2016", palette = "RdYlBu",
        #title.legend = "", border.col = "grey", border.lwd=0.5)
#dev.off()

# Madres adolescentes de 12 - 20 años por entidad federativa. Descartando los casos de niñas embarazadas.
tempo <- baby%>%
         select(id, edad_madre, rango_edad,entidad_nacimiento) %>%
         filter(edad_madre > 11 & edad_madre < 21 ) %>%
         group_by(entidad_nacimiento,rango_edad) %>%
         mutate(total = 1) %>%
         summarise(total_nac = sum(total)) %>%
         ungroup() %>%
         mutate(totales = sum(total_nac), porcentaje = round(total_nac/totales*100,2))

gr <- ggplot(tempo, aes(x = rango_edad, y = porcentaje)) +
      geom_bar(stat = "identity", fill = "#bf360c") + geom_text(aes(label = porcentaje ), vjust = -0.3, size = 6) +
      facet_grid(rango_edad~entidad_nacimiento) + 
      labs(title = "Porcentaje de madres adolecentes de 12 a 20 años por Estado - 2016", x = "Rango de edad", 
           y = "Porcentaje") +
      theme_gray()+ theme(axis.text.x = element_text(angle = 15))

ggsave(paste(dir2,"6.png", sep = "/"), plot = gr, width = 22, height = 12)

# Madres adolescentes de 12 - 20 años por cantidad de bebes nacidos
# Por categoria de edad. Quitandole a la categoria de 19 - 25 años las edades de 19 a 20.
tempo <- baby%>%
         select(id, edad_madre, rango_edad) %>%
         filter(edad_madre > 11 & edad_madre < 21) %>%
         group_by(edad_madre,rango_edad) %>%
         mutate(total = 1) %>%
         summarise(total_nac = sum(total)) %>%
         ungroup() %>%
         mutate(totales = sum(total_nac), porcentaje = round(total_nac/totales*100,2))

gr <- ggplot(tempo, aes(x = rango_edad, y = total_nac, fill = rango_edad)) + geom_boxplot() +
      labs(title = "Madres adolescentes de 12 - 20 años por cantidad de bebes nacidos y rango de edad",
           x = "Rango de edad", y = "Total de nacidos", fill="") + theme_bw() + 
      theme(axis.text.x = element_blank(),legend.position = "bottom")

ggsave(paste(dir2,"7.png",sep = "/"), plot = gr, width = 11, height = 10)

summary(tempo$total_nac[tempo$rango_edad == "12 - 18 aÃ±os"])

summary(tempo$total_nac[tempo$rango_edad == "19 - 25 aÃ±os"])

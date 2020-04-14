# M�xico ocupa el primer lugar en embarazo adolescente a nivel mundial. 

# Limpiamos espacio de trabajo.
rm (list = ls())
setwd("~")

# Instanciamos librerias.
require(foreign)
require(plyr)
require(tidyverse)
require(readstata13)
require(reshape2)
require(readxl)
require(stats)
require(dplyr)

# Declarar la ruta de la carpeta donde trabajamos
dir1 <- "E:\\RodriGo\\Cursos\\devf\\intDS\\exercices\\baby\\Inp"
dir2 <- "E:\\RodriGo\\Cursos\\devf\\intDS\\exercices\\baby\\Out"

# Cargar la base de datos.
# Registros de Nacimientos 2016 de SALUD - De 2016-01-01 a 2016-12-31
# Fuente: https://cutt.ly/Xwo5Pgc
baby <- read.csv(paste(dir1,"sinac2016DatosAbiertos.csv", sep = "/"), stringsAsFactors = FALSE)

# Verificar la estrucutura de la base de datos
str(baby)
nrow(baby) #2080253
# Reducir dataset para la poblacion de estudio.
# Un estado con menor indice de pobreza influye en un mayor n�mero de mam�s adolescentes.
# Un estado con mayor desarrollo economico y mayor acceso a educaci�n tiene un menor n�mero de mam�s adolescentes.
# Seleccione tres estados con un indice de pobreza bajo y tres con un indice de pobreza alto.
#### Estado de M�xcio = 49.6%
#### Yucatan = 41.9 %
#### Queretaro = 31.1 % 
#### CDMX = 28.4%
#### Jalisco = 31.8 %
#### Nuevo Leon = 20.4%
# Por productividad. Trabajan y aportan dinero al pa�s."NO ES LA MISMA CANTIDAD DE GENTE QUE VIVE EN ESOS ESTADOS"
#### Queretaro
#### Nuevo Leon
#### Jalisco 
#### CDMX
#### Estado de M�xcio
#### Yucatan

data <- select(baby,fecha_nac_madre:edad_madre,estado_conyugal:entidad_residencia_madre,numero_embarazos:hijos_sobrevivientes,
              trimestre_recibio_primera_consulta,total_consultas_recibidas,afiliacion_serv_salud:trabaja_actualmente,
              fecha_nacimiento_nac_vivo :semanas_gestacion_nac_vivo,producto_de_un_embarazo,procedimiento_utilizado,
              lugar_de_nacimiento,quien_atendio_parto,entidad_nacimiento) %>%
          filter(entidad_nacimiento == "DISTRITO FEDERAL" | entidad_nacimiento == "MEXICO"| entidad_nacimiento == "QUERETARO  DE ARTEAGA"| entidad_nacimiento == "JALISCO"| entidad_nacimiento == "YUCATAN"| entidad_nacimiento == "NUEVO LEON")

# Corregir datos
str(data)
nrow(data)
tail(data)
# UNION LIBRE - UNI�"N LIBRE
data$estado_conyugal <- ifelse(data$estado_conyugal == "UNI�"N LIBRE","UNION LIBRE",data$estado_conyugal)
head(data$estado_conyugal,500)

# UNICO - �sNICO
data$producto_de_un_embarazo <- ifelse(data$producto_de_un_embarazo == "�sNICO","UNICO",data$producto_de_un_embarazo)
head(data$producto_de_un_embarazo,500)


data$procedimiento_utilizado <- ifelse(data$procedimiento_utilizado == "EUT�"CICO","EUTOCICO",data$procedimiento_utilizado)
head(data$procedimiento_utilizado,500)

data$procedimiento_utilizado <- ifelse(data$procedimiento_utilizado == "CES�\u0081REA","CESAREA",data$procedimiento_utilizado)
head(data$procedimiento_utilizado,500)


data$lugar_de_nacimiento <- ifelse(data$lugar_de_nacimiento == "UNIDAD M�???DICA PRIVADA","UD. MEDICA PRIVADA",data$lugar_de_nacimiento)
head(data$lugar_de_nacimiento,500)


data$lugar_de_nacimiento <- ifelse(data$lugar_de_nacimiento == "SECRETAR�\u008dA DE SALUD","SRIA. DE SALUD",data$lugar_de_nacimiento)
head(data$lugar_de_nacimiento,500)


data$lugar_de_nacimiento <- ifelse(data$lugar_de_nacimiento == "V�\u008dA P�sBLICA","VIA PUBLICA",data$lugar_de_nacimiento)
tail(data$lugar_de_nacimiento,500)

data$quien_atendio_parto <- ifelse(data$quien_atendio_parto == "M�???DICO","MEDICO",data$quien_atendio_parto)
head(data$quien_atendio_parto,500)


data$quien_atendio_parto <- ifelse(data$quien_atendio_parto == "PERSONA AUTORIZADA POR LA SECRETAR�\u008dA DE SALUD", "PER. AUT. SRIA. SALUD",data$quien_atendio_parto)
head(data$quien_atendio_parto,500)


data$quien_atendio_parto <- ifelse(data$quien_atendio_parto == "PERSONA AUTORIZADA POR LA SECRETAR�\u008dA DE SALUD", "PER. AUT. SRIA. SALUD",data$quien_atendio_parto)
select(data,quien_atendio_parto) %>%
  filter(quien_atendio_parto=="PER. AUT. SRIA. SALUD")

data$ocupacion_habitual_madre <- ifelse(data$ocupacion_habitual_madre == "TRABAJADORES DE LA EDUCACI�"N", "SECT. EDUCACION",data$ocupacion_habitual_madre)
select(data,ocupacion_habitual_madre) %>%
  filter(ocupacion_habitual_madre=="SEC. EDUCACION")

data$ocupacion_habitual_madre <- ifelse(data$ocupacion_habitual_madre == "EMPLEADOS DE SECTORES P�sBLICO Y PRIVADO", "SECT. PUBLICO Y PRIVADO",data$ocupacion_habitual_madre)
select(data,ocupacion_habitual_madre) %>%
  filter(ocupacion_habitual_madre=="SECT. PUBLICO Y PRIVADO")

# Agregue una columna con un identificador y reordeno con id como primera variable.
data <- data %>%
        mutate(id = row_number())

data <- select(data,id,everything())

str(data)
# Agregar columna con codigo INEGI  a los estados de estudio.
data <- data%>%
        mutate(cod_entidad = ifelse(entidad_nacimiento == "DISTRITO FEDERAL",9,
                              ifelse(entidad_nacimiento == "MEXICO",15,
                               ifelse(entidad_nacimiento == "QUERETARO  DE ARTEAGA",22,
                                ifelse(entidad_nacimiento == "JALISCO",14,
                                 ifelse(entidad_nacimiento == "YUCATAN",31,
                                  ifelse(entidad_nacimiento == "NUEVO LEON",19,NA)))))))

# Agregar rangos de edad segun ciclo de vida de un humano.
# 10 - 11 a�os / ni�os  - entre 10 y 15 a�os comienza la mestruacion para la mayoria de las ni�as
# 12 - 18 a�os / adolescentes
# 19 - 35 a�os / juventud
#### 19 - 25 / Preparatoria - Universidad
#### 26 - 34 / Poder adquisitivo - Realizacion laboral
# 35 - 50 a�os / madurez
data <- data %>%
        mutate(rango_edad = ifelse(edad_madre %in% 10:11, "10 - 11 a�os",
                             ifelse(edad_madre %in% 12:18, "12 - 18 a�os",
                              ifelse(edad_madre %in% 19:25, "19 - 25 a�os",
                               ifelse(edad_madre %in% 26:34, "26 - 34 a�os",
                                ifelse(edad_madre %in% 35:50, "35 - 50 a�os",NA))))))

# Se encontraro datos erroneos al momento de la captura de las edades. Hay edades de 999. Las vamos a sustituir por NA.
data$edad_madre <- ifelse(data$edad_madre>60,NA,data$edad_madre)
data[data$edad_madre>70,"edad_madre"]

# Exportamos la base de estudio

write.csv(data, paste(dir2, "daba.csv", sep="/"), row.names = F, fileEncoding = "UTF-8")

suma <- 5+6
resta <- 10-20
multiplicación <- 2*5

#Usuarios de Exclusivos de Aguascalientes
Aguascalientes <- dplyr::filter(COVID19MEXICO, ENTIDAD_NAC=='01')

Pabellon <- subset(Aguascalientes, select = c(EDAD, SEXO, MUNICIPIO_RES))
Hombres_Pabe <- dplyr::filter(Pabellon, MUNICIPIO_RES==6)
Pabellon <- dplyr::filter(COVID19MEXICO, MUNICIPIO_RES=='006')

#¿Cuál es la edad media de mujeres que enfermaron de covid?
M_Covid <- dplyr::filter(COVID19MEXICO, SEXO == 1 & TIPO_PACIENTE == 2)
Prom_M_Edad <- mean(M_Covid$EDAD)
print(Prom_M_Edad)

#¿Cuál es la edad media de hombres que enfermaron de covid?
H_Covid <- dplyr::filter(COVID19MEXICO, SEXO == 2 & TIPO_PACIENTE == 2)
Prom_H_Edad <- mean(H_Covid$EDAD)
print(Prom_H_Edad)

#¿Cuántos hombres enfermaron en el municipio de Pabellón de Arteaga?
H_Covid_Pabe <- dplyr::filter(COVID19MEXICO, MUNICIPIO_RES=='006' & SEXO==2 & TIPO_PACIENTE==2)
Num_H_Pabe <- length(H_Covid_Pabe$EDAD)
print(Num_H_Pabe)




#¿Cuál fue el municipio de Aguascalientes que tuvo la mayor cantidad de mujeres enfermas de COVID?
AGS <- dplyr::filter(COVID19MEXICO, ENTIDAD_NAC=='01')

# Agrupar por municipio y contar la cantidad de datos en cada grupo
municipio_count <- AGS %>% 
  group_by(MUNICIPIO_RES) %>% 
  summarise(count = n())

# Ordenar de manera descendente y tomar el primer municipio (el que tiene el mayor recuento)
municipio_con_mas_datos <- municipio_count %>% 
  arrange(desc(count)) %>% 
  slice(1)

# Mostrar el municipio con la mayor cantidad de datos
print(municipio_con_mas_datos)



#¿Cuántos pacientes ambulatorios hubo en el país?
P_Ambu <- dplyr::filter(COVID19MEXICO, PAIS_ORIGEN==97 & TIPO_PACIENTE==1)
Num_P_Ambu <- length(P_Ambu$EDAD)
print(Num_P_Ambu)

#Grafique con ggplot una gráfica con los pacientes de cada sector.
library(ggplot2)
sector_counts <- table(COVID19MEXICO$SECTOR)

# Crear un data frame con los recuentos
sector_df <- data.frame(SECTOR = as.numeric(names(sector_counts)),
                        Frecuencia = as.numeric(sector_counts))

ggplot(sector_df, aes(x = factor(SECTOR), y = Frecuencia, fill = factor(SECTOR))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#FFC0CB", "#FFD700", "#FFA07A","#ADD8E6", "#98FB98", "#FFB6C1","#87CEFA", "#F08080", "#FFDAB9","#FFE4E1", "#FFA500", "#20B2AA","#F0E68C", "#B0C4DE"), # Colores personalizados
                    name = "SECTOR") +
  theme_minimal() + 
  labs(x = "SECTOR", y = "Frecuencia",  
       title = "Recuento de Sectores") +  
  theme(plot.title = element_text(hjust = 0.5))


#¿Cuál es la edad máxima en la que se presenta una persona infectada?
Persona_MAXEdad <- dplyr::summarise(COVID19MEXICO, max(EDAD))
print(Persona_MAXEdad)
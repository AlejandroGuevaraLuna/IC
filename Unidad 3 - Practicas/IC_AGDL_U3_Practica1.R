#Promedio de mujeres que enfermaron de covid:
M_Covid <- dplyr::filter(COVID19MEXICO, SEXO == 1 & TIPO_PACIENTE == 2)
Prom_M_Edad <- mean(M_Covid$EDAD)
print(Prom_M_Edad)

#Minicipio más afectado dentro de Aguascalientes:
Persona_MAXEdad <- dplyr::summarise(COVID19MEXICO, max(EDAD))
print(Persona_MAXEdad)
library(ggplot2)

sustainable_energy_db <- read.csv("sustainable_energy_argentina.csv")
summary_db <- summary(sustainable_energy_db)

separador <- function(arg) {
  cat(paste("-------",arg,"-------\n"))
  cat("\n")
}
cat("Esta variable indica el año en el que se basará la predicción, en este caso, es 2030")
anio_prediccion = 2030  

cat("Para obtener una predicción final, con todas las variables del estudio, es necesario realizar un modelo predictivo para cada tipo de dato de la información obtenida de la base de datos.")

separador("Modelo de Predicción para la Salida de Electricidad Renovable por año en Argentina (GWh)")

cat("A continuación, se usa la función lm de R que me permite hacer un modelo lineal, para saber qué tanta es la relación entre una variable con las demas.
    Primero comienzo con la variable Salida de Electricidad Renovable.") 

regresion_salida_elec_renovable <- lm( REN.ELECTRICITY.OUTPUT ~ TIME  + TOTAL.ELECTRICITY.OUTPUT +  PRIMARY.ENERGY.INTENSITY +
                                         + RE.CONSUMPTION  + TOTAL.FINAL.ENERGY.CONSUM, data=sustainable_energy_db) 

cat("La función summary nos da un resumen del modelo lineal que nos permite analizar segun la cantidad de asteriscos de cada dato, que tan fuerte es la dependencia con 
    el dato que se está justamente analizando, para posteriormente realizar un ajuste del modelo y quitar las variables no relevantes.")

summary(regresion_salida_elec_renovable)

cat("En este caso el dato con mayor dependencia es el Consumo de Energía Renovable, pero al ser el primer modelo predictivo del estudio, no es posible
    usar dicho dato ni ningún otro porque no se han obtenido aún, por lo tanto se concluye por tomar la variable Tiempo, que se refiere al Año.")

regresion_salida_elec_renovable_ajustado <- lm( REN.ELECTRICITY.OUTPUT ~ TIME 
                                                , data=sustainable_energy_db)   
summary(regresion_salida_elec_renovable_ajustado)

cat("La salida de arriba nos da el resumen con el ajuste ya realizado, y nos da un porcentaje de efectividad del 81% (el valor R-squared), lo que se considera considerablemente alto, asi que
    se procede a realizar la parte final de este modelo predictivo, usando la función predict de R que hace todo el trabajo por nosotros y nos devuelve el valor predecido:")

datos_prediccion_salida_elec_renovable_ajustado <- data.frame(TIME=anio_prediccion)
salida_electricidad_renovable = as.double(predict(regresion_salida_elec_renovable_ajustado, datos_prediccion_salida_elec_renovable_ajustado))
cat("El resultado de la variable Salida de Electricidad Renovable para el año",anio_prediccion,"es de",salida_electricidad_renovable,"GWh")

cat("A continuación se representa un gráfico en el que se pueden ver los valores de la Salida de Electricidad Renovable por año, desde 1990 hasta 2015 se representa
    cada año con el punto negro y, de manera intuitiva, se puede saber si sube o baja dicho valor. El último punto es la representación del dato predecido.")

anios = c(sustainable_energy_db$TIME,anio_prediccion)
elec_renov = c(sustainable_energy_db$REN.ELECTRICITY.OUTPUT,salida_electricidad_renovable)

df <- data.frame(anios= anios,elec_renov=elec_renov)

ggplot(df,aes(x=anios,y=elec_renov)) +
  geom_point(na.rm = FALSE, stat = "identity") +
  geom_line(na.rm = FALSE, stat = "identity") + 
  xlab("Año") + 
  ylab("Salida de electricidad renovable") +
  scale_x_continuous(limits = c(1990, anio_prediccion), breaks = seq(1990, anio_prediccion, by = 5)) 

cat("Hasta aquí llegaría la primera parte de este estudio, ahora se hará el mismo procedimiento con los demas datos. Para no ser repetitivo se omitirán las explicaciones anteriormente
    realizadas, solo habrá aclaraciones cuando sea necesario.")

separador("Modelo de Predicción para el Consumo de Energia Renovable por año en Argentina (Terajoule-TJ)")

regresion_consumo_ER_energy <- lm( RE.CONSUMPTION ~ TIME  + TOTAL.ELECTRICITY.OUTPUT +  PRIMARY.ENERGY.INTENSITY +
                                           + REN.ELECTRICITY.OUTPUT  + TOTAL.FINAL.ENERGY.CONSUM, data=sustainable_energy_db) 
summary(regresion_consumo_ER_energy)

cat("Al ya haber obtenido el valor de Salida de Electricidad Renovable para el año",anio_prediccion,", se usa esta misma para obtener ahora el dato Consumo de Energia Renovable,
    conjunta con el año.")

regresion_consumo_ER_ajustado <- lm( RE.CONSUMPTION ~ REN.ELECTRICITY.OUTPUT + TIME, data=sustainable_energy_db) 
summary(regresion_consumo_ER_ajustado)

cat("En el resumen se observa una efectividad del 77%, por lo que se procede a finalizar este modelo predictivo. nuevamente con la función predict")

datos_prediccion_consumo_ER <- data.frame(TIME=anio_prediccion,REN.ELECTRICITY.OUTPUT=salida_electricidad_renovable)
cons_energia_renovable = as.double(predict(regresion_consumo_ER_ajustado, datos_prediccion_consumo_ER))

cat("El resultado de la variable Consumo de Energia Renovable para el año",anio_prediccion,"es de",cons_energia_renovable,"Terajoules")
cat("Gráfico con respecto a años anteriores")

re_consum = c(sustainable_energy_db$RE.CONSUMPTION,cons_energia_renovable)

df <- data.frame(anios= anios,re_consum=re_consum)

ggplot(df,aes(x=anios,y=re_consum)) +
  geom_point(na.rm = FALSE, stat = "identity") +
  geom_line(na.rm = FALSE, stat = "identity") + 
  xlab("Año") + 
  ylab("Consumo de Energía Renovable")+
  scale_x_continuous(limits = c(1990, anio_prediccion), breaks = seq(1990, anio_prediccion, by = 5)) 

separador("Modelo de Predicción para la Salida Total de Electricidad por año en Argentina (GWh)")

regresion_salida_elec_total<- lm(TOTAL.ELECTRICITY.OUTPUT ~ TIME  + REN.ELECTRICITY.OUTPUT +  PRIMARY.ENERGY.INTENSITY +
                                   RE.CONSUMPTION +  + TOTAL.FINAL.ENERGY.CONSUM, data=sustainable_energy_db) 
summary(regresion_salida_elec_total)

regresion_salida_elec_total_ajustado <- lm(TOTAL.ELECTRICITY.OUTPUT ~ TIME + RE.CONSUMPTION, data=sustainable_energy_db) 
summary(regresion_salida_elec_total_ajustado)

cat("Como vemos en el resumen, este modelo nos da un 99% de efectividad, por lo que ya estamos listos para realizar la predicción")

datos_prediccion_salida_elec_total <- data.frame(TIME=anio_prediccion,RE.CONSUMPTION=cons_energia_renovable)
salida_total_electricidad = as.double(predict(regresion_salida_elec_total_ajustado, datos_prediccion_salida_elec_total))
cat("El resultado de la variable Salida Total de Electricidad para el año",anio_prediccion,"es de",salida_total_electricidad,"GWh")

cat("A continuación, el grafico de este dato en función de los años:")

total_elec = c(sustainable_energy_db$TOTAL.ELECTRICITY.OUTPUT,salida_total_electricidad)

df <- data.frame(anios= anios,total_elec=total_elec)

ggplot(df,aes(x=anios,y=total_elec)) +
  geom_point(na.rm = FALSE, stat = "identity") +
  geom_line(na.rm = FALSE, stat = "identity") + 
  xlab("Año") + 
  ylab("Salida Total de Electricidad")+
  scale_x_continuous(limits = c(1990, anio_prediccion), breaks = seq(1990, anio_prediccion, by = 5)) 

separador("Modelo de Predicción para el Total Final de Consumo de Energia por año en Argentina (Terajoules - TJ)")

regresion_total_cons_energia <- lm(TOTAL.FINAL.ENERGY.CONSUM ~ TIME + PRIMARY.ENERGY.INTENSITY +  REN.ELECTRICITY.OUTPUT +  TOTAL.ELECTRICITY.OUTPUT +
                                     RE.CONSUMPTION , data=sustainable_energy_db) 
summary(regresion_total_cons_energia)

regresion_total_cons_energia_ajustado <- lm(TOTAL.FINAL.ENERGY.CONSUM ~ TIME, data=sustainable_energy_db) 
summary(regresion_total_cons_energia_ajustado)

cat("Hasta ahora, nada nuevo. Despues del ajuste y eliminación de las variables no relevantes, obtenemos un resumen de modelo lineal con 95% de efectividad.")

datos_prediccion_total_cons_energia <- data.frame(TIME=anio_prediccion)
total_final_consumo_energia = as.double(predict(regresion_total_cons_energia_ajustado, datos_prediccion_total_cons_energia))
cat("El resultado de la variable Total Final de Consumo de Energia para el año",anio_prediccion,"es de",total_final_consumo_energia,"Terajoules")
cat("Su respectivo gráfico es el siguiente:")

total_fin_cons = c(sustainable_energy_db$TOTAL.FINAL.ENERGY.CONSUM,total_final_consumo_energia)

df <- data.frame(anios= anios,total_fin_cons=total_fin_cons)

ggplot(df,aes(x=anios,y=total_fin_cons)) +
  geom_point(na.rm = FALSE, stat = "identity") +
  geom_line(na.rm = FALSE, stat = "identity") + 
  xlab("Año") + 
  ylab("Total Final de Consumo de Energía")+
  scale_x_continuous(limits = c(1990, anio_prediccion), breaks = seq(1990, anio_prediccion, by = 5)) 

separador("Modelo de Predicción para el Nivel de Intensidad de Energia Primaria por año en Argentina (GWh)")

regresion_intensidad_energ_prim <- lm(PRIMARY.ENERGY.INTENSITY ~ TIME + TOTAL.FINAL.ENERGY.CONSUM +  REN.ELECTRICITY.OUTPUT +  TOTAL.ELECTRICITY.OUTPUT +
                                        RE.CONSUMPTION , data=sustainable_energy_db) 
summary(regresion_intensidad_energ_prim)

regresion_intensidad_energ_prim_ajustado <- lm(PRIMARY.ENERGY.INTENSITY ~ TIME + REN.ELECTRICITY.OUTPUT +  TOTAL.ELECTRICITY.OUTPUT +
                                                 TOTAL.FINAL.ENERGY.CONSUM, data=sustainable_energy_db) 
summary(regresion_intensidad_energ_prim_ajustado)

cat("Si bien este modelo lineal nos da un 61% de efectividad, se sigue considerando bastante efectivo.")

datos_prediccion_intensidad_energ_prim_ajustado <- data.frame(TIME=anio_prediccion,TOTAL.FINAL.ENERGY.CONSUM=total_final_consumo_energia,
                                                              REN.ELECTRICITY.OUTPUT=salida_electricidad_renovable,TOTAL.ELECTRICITY.OUTPUT=salida_total_electricidad)
nivel_intensidad_ener_primaria = as.double(predict(regresion_intensidad_energ_prim_ajustado, datos_prediccion_intensidad_energ_prim_ajustado))
cat("El resultado de la variable Nivel de Intensidad de Energía Primaria para el año",anio_prediccion,"es de",nivel_intensidad_ener_primaria,"GWh")
cat("Y su gráfico se pinta de la siguiente manera:")

intens_energ = c(sustainable_energy_db$PRIMARY.ENERGY.INTENSITY,nivel_intensidad_ener_primaria)

df <- data.frame(anios= anios,intens_energ=intens_energ)

ggplot(df,aes(x=anios,y=intens_energ)) +
  geom_point(na.rm = FALSE, stat = "identity") +
  geom_line(na.rm = FALSE, stat = "identity") + 
  xlab("Año") + 
  ylab("Nivel de Intensidad de Consumo de Energía")+
  scale_x_continuous(limits = c(1990, anio_prediccion), breaks = seq(1990, anio_prediccion, by = 5)) 

cat("A diferencia de los anteriores datos, aqui se observa un significante descenso respecto a años anteriores.")

cat("Ahora si, despues de terminar los modelos predictivos con cada dato, se concluye este estudio construyendo una matriz resultado que almacene cada dato predecido
    con el fin de unificarlos.")

matriz_resultado <- matrix(c(nivel_intensidad_ener_primaria,salida_electricidad_renovable,cons_energia_renovable,salida_total_electricidad,total_final_consumo_energia),nrow=1,ncol=5)
colnames(matriz_resultado) <- c("Niv. de Intens. de Energía Primaria (GWh)","Salida de Elect. Renovable (GWh)",
                                "Cons. de Energía Renovable (TJ)","Salida Tot. de Elect. (GWh)","Tot. Final de Cons. de Energía (TJ)")
rownames(matriz_resultado) <- paste("Año",anio_prediccion)

cat("Recapitulando, estos fueron los resultados obtenidos en este pequeño estudio:")
cat("Valores predecidos para el año ",anio_prediccion,"en Argentina")
cat(colnames(matriz_resultado)[1],": ",matriz_resultado[1])
cat(colnames(matriz_resultado)[2],": ",matriz_resultado[2])
cat(colnames(matriz_resultado)[3],": ",matriz_resultado[3])
cat(colnames(matriz_resultado)[4],": ",matriz_resultado[4])
cat(colnames(matriz_resultado)[5],": ",matriz_resultado[5])




sustainable_energy_db <- read.csv("sustainable_energy_argentina.csv")
summary_db <- summary(sustainable_energy_db)

separador <- function(arg) {
  cat(paste("-------",arg,"-------\n"))
  cat("\n")
}

anio_prediccion = 2030  # Modificar este valor para obtener distintos resultados.

# ----------------------------Modelo de Predicción para la Salida de Electricidad Renovable por año en Argentina (GWh)------------------
separador("Modelo de Predicción para la Salida de Electricidad Renovable por año en Argentina (GWh)")

# Genero regresion lineal 

regresion_salida_elec_renovable <- lm( REN.ELECTRICITY.OUTPUT ~ TIME  + TOTAL.ELECTRICITY.OUTPUT +  PRIMARY.ENERGY.INTENSITY +
                                         + RE.CONSUMPTION  + TOTAL.FINAL.ENERGY.CONSUM, data=sustainable_energy_db) 
summary(regresion_salida_elec_renovable)

# Ajusto la regresion para quitar variables no relevantes

regresion_salida_elec_renovable_ajustado <- lm( REN.ELECTRICITY.OUTPUT ~ TIME 
                                                , data=sustainable_energy_db)   
summary(regresion_salida_elec_renovable_ajustado)

# Este modelo tiene un 81% de efectividad.

# Construcción del modelo de predicción

datos_prediccion_salida_elec_renovable_ajustado <- data.frame(TIME=anio_prediccion)
salida_electricidad_renovable = as.double(predict(regresion_salida_elec_renovable_ajustado, datos_prediccion_salida_elec_renovable_ajustado))
salida_electricidad_renovable

# ---------------------------- Modelo de Predicción para el Consumo de Energia Renovable por año en Argentina (Terajoule-TJ)----------------------------------
separador("Modelo de Predicción para el Consumo de Energia Renovable por año en Argentina (Terajoule-TJ)")

# Genero regresion lineal 

regresion_consumo_ER_energy <- lm( RE.CONSUMPTION ~ TIME  + TOTAL.ELECTRICITY.OUTPUT +  PRIMARY.ENERGY.INTENSITY +
                                           + REN.ELECTRICITY.OUTPUT  + TOTAL.FINAL.ENERGY.CONSUM, data=sustainable_energy_db) 
summary(regresion_consumo_ER_energy)

# Ajusto la regresion para quitar variables no relevantes

regresion_consumo_ER_ajustado <- lm( RE.CONSUMPTION ~ REN.ELECTRICITY.OUTPUT + TIME, data=sustainable_energy_db) 
summary(regresion_consumo_ER_ajustado)

# Este modelo tiene un 77% de efectividad.

# Construcción del modelo de predicción

datos_prediccion_consumo_ER <- data.frame(TIME=anio_prediccion,REN.ELECTRICITY.OUTPUT=salida_electricidad_renovable)
cons_energia_renovable = as.double(predict(regresion_consumo_ER_ajustado, datos_prediccion_consumo_ER))
cons_energia_renovable
# ---------------------------- Modelo de Predicción para la Salida Total de Electricidad por año en Argentina (GWh) ------------------------------------------------
separador("Modelo de Predicción para la Salida Total de Electricidad por año en Argentina (GWh)")

# Genero regresion lineal 

regresion_salida_elec_total<- lm(TOTAL.ELECTRICITY.OUTPUT ~ TIME  + REN.ELECTRICITY.OUTPUT +  PRIMARY.ENERGY.INTENSITY +
                                   RE.CONSUMPTION +  + TOTAL.FINAL.ENERGY.CONSUM, data=sustainable_energy_db) 
summary(regresion_salida_elec_total)

# Ajusto la regresion para quitar variables no relevantes

regresion_salida_elec_total_ajustado <- lm(TOTAL.ELECTRICITY.OUTPUT ~ TIME + RE.CONSUMPTION, data=sustainable_energy_db) 
summary(regresion_salida_elec_total_ajustado)

# Este modelo tiene un 99% de efectividad.

# Construcción del modelo de predicción

datos_prediccion_salida_elec_total <- data.frame(TIME=anio_prediccion,RE.CONSUMPTION=cons_energia_renovable)
salida_total_electricidad = as.double(predict(regresion_salida_elec_total_ajustado, datos_prediccion_salida_elec_total))
salida_total_electricidad

# ---------------------------- Modelo de Predicción para el Total Final de Consumo de Energia por año en Argentina (Terajoules - TJ) ------------------------------------------------------
separador("Modelo de Predicción para el Total Final de Consumo de Energia por año en Argentina (Terajoules - TJ)")

# Genero regresion lineal 

regresion_total_cons_energia <- lm(TOTAL.FINAL.ENERGY.CONSUM ~ TIME + PRIMARY.ENERGY.INTENSITY +  REN.ELECTRICITY.OUTPUT +  TOTAL.ELECTRICITY.OUTPUT +
                                     RE.CONSUMPTION , data=sustainable_energy_db) 
summary(regresion_total_cons_energia)

# Ajusto la regresion para quitar variables no relevantes

regresion_total_cons_energia_ajustado <- lm(TOTAL.FINAL.ENERGY.CONSUM ~ TIME, data=sustainable_energy_db) 
summary(regresion_total_cons_energia_ajustado)

# Este modelo tiene un 95% de efectividad.

# Construcción del modelo de predicción

datos_prediccion_total_cons_energia <- data.frame(TIME=anio_prediccion)
total_final_consumo_energia = as.double(predict(regresion_total_cons_energia_ajustado, datos_prediccion_total_cons_energia))
total_final_consumo_energia

# ---------------------------- Modelo de Predicción para el Nivel de Intensidad de Energia Primaria por año en Argentina (GWh) ---------------------------------------------------------
separador("Modelo de Predicción para el Nivel de Intensidad de Energia Primaria por año en Argentina (GWh)")
# Este modelo sirve para medir la eficiencia de la energía.

# Genero regresion lineal 

regresion_intensidad_energ_prim <- lm(PRIMARY.ENERGY.INTENSITY ~ TIME + TOTAL.FINAL.ENERGY.CONSUM +  REN.ELECTRICITY.OUTPUT +  TOTAL.ELECTRICITY.OUTPUT +
                                        RE.CONSUMPTION , data=sustainable_energy_db) 
summary(regresion_intensidad_energ_prim)

# Ajusto la regresion para quitar variables no relevantes

regresion_intensidad_energ_prim_ajustado <- lm(PRIMARY.ENERGY.INTENSITY ~ TIME + REN.ELECTRICITY.OUTPUT +  TOTAL.ELECTRICITY.OUTPUT +
                                                 TOTAL.FINAL.ENERGY.CONSUM, data=sustainable_energy_db) 
summary(regresion_intensidad_energ_prim_ajustado)

# Este modelo tiene un 61% de efectividad.

# Construcción del modelo de predicción

datos_prediccion_intensidad_energ_prim_ajustado <- data.frame(TIME=anio_prediccion,TOTAL.FINAL.ENERGY.CONSUM=total_final_consumo_energia,
                                                              REN.ELECTRICITY.OUTPUT=salida_electricidad_renovable,TOTAL.ELECTRICITY.OUTPUT=salida_total_electricidad)
nivel_intensidad_ener_primaria = as.double(predict(regresion_intensidad_energ_prim_ajustado, datos_prediccion_intensidad_energ_prim_ajustado))
nivel_intensidad_ener_primaria


# Construyendo matriz de resultado

matriz_resultado <- matrix(c(nivel_intensidad_ener_primaria,salida_electricidad_renovable,cons_energia_renovable,salida_total_electricidad,total_final_consumo_energia),nrow=1,ncol=5)
colnames(matriz_resultado) <- c("Niv. de Intens. de Energía Primaria (GWh)|","Salida de Elect. Renovable (GWh) |",
                                "Cons. de Energía Renovable (TJ)|","Salida Tot. de Elect. (GWh)|","Tot. Final de Cons. de Energía (TJ)|")
rownames(matriz_resultado) <- paste("Año",anio_prediccion)
matriz_resultado

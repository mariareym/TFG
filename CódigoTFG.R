  #María Rey 
   #Trabajo de fin de Grado: Economía
   #Título: Análisis integral de la Sostenibilidad de la Deuda Públicca en España

  #Importamos librería readxl
  library(readxl) 
  library(dplyr)
  library(tidyr)
  library(mFilter)
  library(tseries)
  library(ggplot2)
  library(forecast)
  library(lmtest)
  library(sandwich)
  library(stargazer)
  
  #Parte1 - Carga de Datos: Importar datos:Base de datos trimestral de la economía española REMSDB y histórico del tipo de interés 

  #Carga de las bases de datos
  datos_BDREMS_23 <- "C:/Users/mrmma/OneDrive/Documentos/Complutense/Trabajos de Fin de Grado/TFG Economía/Datos/BDREMS.xlsx" 
  datos_tintmedio <- "C:/Users/mrmma/OneDrive/Documentos/Complutense/Trabajos de Fin de Grado/TFG Economía/Datos/historico_t.int.medio.xlsx" 
  datos_saldoprimario <- "C:/Users/mrmma/OneDrive/Documentos/Complutense/Trabajos de Fin de Grado/TFG Economía/Datos/Trimestral D.Completo (ev.historica)GrupoAdmonResponsive (4).xlsx"
  datos_deudaprivada <- "C:/Users/mrmma/OneDrive/Documentos/Complutense/Trabajos de Fin de Grado/TFG Economía/Datos/datosCesta1103193537.xls"
  datos_intpagados <- "C:/Users/mrmma/OneDrive/Documentos/Complutense/Trabajos de Fin de Grado/TFG Economía/Datos/intpagados_anual.xlsx"
  
  saldoprimario <- read_excel(datos_saldoprimario)
  BDREMS23 <- read_excel(datos_BDREMS_23, sheet = "BDREMS_23") 
  tintmedio <- read_excel(datos_tintmedio, skip = 4) 
  deudaprivada <- read_excel(datos_deudaprivada, skip = 7)
  intpagados <- read_excel(datos_intpagados)

  saldoprimario <- data.frame(saldoprimario)
  BDREMS23 <- data.frame(BDREMS23) 
  tintmedio <- data.frame(tintmedio)
  deudaprivada <- data.frame(deudaprivada)
  intpagados <- data.frame(intpagados)

  #Parte2 - Limpieza de Datos 
  
  intpagados <- intpagados %>%
    rename(
      int_pagados_anual = "FormatoImporte"
    )
    
  BDREMS23 <- BDREMS23 %>% 
  #renombramos variables 
  rename(
    Año = "Base.de.Datos.del.modelo.REMS...............Versión.mayo.2023",
    Trimestre = "...2") %>% 
  #tiramos las observaciones redundantes
  slice(-1, -2)
 
  tintmedio <- tintmedio %>% 
  #renombramos variables 
  rename(
    Mes = "FECHA",
    TIPO.INTERES.MEDIO = "TIPO.INTERES", 
    DEUDA.ASUMIDA = "ASUMIDA",
    TIPO.INTERES.MEDIO.TOTAL = "MEDIO.TOTAL.",
    ByO.Indexados.Tipo.Int.Real = "Indexados", 
    Rend.Efect.Medio.Infl.Estimada = "REND..EFEC.Medio") %>%
  #desechamos observaciones que tengan missing values 
  na.omit(tintmedio) %>%

  #creamos una variable que recoja el año correspondiente a cada observación 
  mutate(Año = rep(2001:2022, each=12)) 

  #Creamos variable trimestre numérica en tintmedio para fusionar las bases de datos en una única 
  asignar_trimestre <- function(Mes) {
    if (Mes %in% c("Enero", "Febrero", "Marzo")) {
      return(1)
    } else if (Mes %in% c("Abril", "Mayo", "Junio")) {
      return(2)
    } else if (Mes %in% c("Julio", "Agosto", "Septiembre")) {
      return(3)
    } else {
      return(4)  # Octubre, Noviembre y Diciembre
    }
  }
  
   #Creamos variable que a cada trimestre se le asigna el mes para fusionar bases de datos 
  #Duda: Estamos suponiendo que el dto trimestral se coge del último mes del trimestre (es una media?)
  asignar_mes <- function(Trimestre) {
    if (Trimestre == 1) {
      return("Marzo")
    } else if (Trimestre == 2) {
      return("Junio")
    } else if (Trimestre == 3) {
      return("Septiembre")
    } else {
      return("Diciembre") 
    }
  }
  
  tintmedio <- tintmedio %>%
   mutate(Trimestre = sapply(Mes, asignar_trimestre)) 
  
  BDREMS23 <- BDREMS23 %>%
   mutate(Mes = sapply(Trimestre, asignar_mes)) 
  
  saldoprimario <- saldoprimario %>%
    #eliminamos la primera obervación, redundante.
    slice(-1) 

  for (i in 1:4) {
    tri = as.character(i)
    saldoprimario$Trimestre <- ifelse(grepl(tri, saldoprimario$Trimestre), i, saldoprimario$Trimestre)
  }
  
  rm(i, tri)
  
  deudaprivada <- deudaprivada %>%
    rename(
      Año = "X2023",
      "1" = "X752789",
      "2" = "X761450", 
      "3" = "...4", 
      "4" = "...5"
    )
  deudaprivada <- pivot_longer(deudaprivada, cols = c("1", "2", "3", "4"), names_to = "Trimestre", values_to = "Deuda.Privada")
  
  #Parte3 - Fusión de bases de datos
  #Excluimos las observaciones de tintmedio que no tienen coincidencias (al ser los datos del main trimestrales)
  
  merge1 <- merge(BDREMS23, tintmedio, by = c("Mes", "Año"), all.x = TRUE, all.y = FALSE)
  merge1 <- merge1 %>%
    rename(Trimestre = "Trimestre.x")
  
  merge2 <- merge(merge1, saldoprimario, by = c("Trimestre", "Año"), all.x = TRUE, all.y = FALSE)
  
  BASE <- merge(merge2, deudaprivada, by = c("Trimestre", "Año"), all.x = TRUE, all.y = FALSE)
  
  rm(merge1, merge2)
      
  #Parte4 - Consolidamos Base de Datos con las variables que nos interesan
  
  BASE <- BASE %>%
    select(Año, Trimestre, PRODUCTO.INTERIOR.BRUTO.pm, PRODUCTO.INTERIOR.BRUTO.pm..pr..corr.., Deflactor.PIBpm, Deuda.AAPP, TIPO.INTERES.MEDIO, EMPLEOS.NO.FINANCIEROS, SALDO.PRIMARIO, B.9.CAPACIDAD.....O.NECESIDAD.....DE.FINANCIACIÓN, TOTAL.RECURSOS.DE.LAS.AA.PP., TOTAL.EMPLEOS.DE.LAS.AA.PP., Deuda.Privada, Subvenciones..D.3., Prestaciones.por.desesmpleo, Tipo.de.cambio.nominal...51)

  #Parte5 - Preparamos Base de Datos definitiva para el análisis. Construimos variables y completamos las existentes. 
  
  BASE <- BASE %>%
    rename(
      PIBreal = "PRODUCTO.INTERIOR.BRUTO.pm", 
      PIBnom = "PRODUCTO.INTERIOR.BRUTO.pm..pr..corr..", 
      deflactor = "Deflactor.PIBpm", 
      saldo_primario = "SALDO.PRIMARIO",
      tintnom = "TIPO.INTERES.MEDIO", 
      deuda = "Deuda.AAPP", 
      int_pagados = "EMPLEOS.NO.FINANCIEROS", 
      cn_fin = "B.9.CAPACIDAD.....O.NECESIDAD.....DE.FINANCIACIÓN", 
      total_ingresos_aapp = "TOTAL.RECURSOS.DE.LAS.AA.PP.",
      total_gastos_aapp = "TOTAL.EMPLEOS.DE.LAS.AA.PP.",
      deuda_priv = "Deuda.Privada", 
      subvenciones = "Subvenciones..D.3.",
      prest_desempleo = "Prestaciones.por.desesmpleo",
      tipocambio_dolar_euro = "Tipo.de.cambio.nominal...51"
    ) 
  
  for (col in names(BASE)) {
    BASE[[col]] <- as.numeric(as.character(BASE[[col]]))
  }
  rm(col)
  
  #construimos variables de interés para el análisis
  BASE <- BASE %>%
    group_by(Año) %>%
    mutate(PIBnom_anual = sum(PIBnom, na.rm = TRUE),
           PIBreal_anual = sum(PIBreal, na.rm = TRUE),
           totrecursos_anual = sum(total_ingresos_aapp, na.rm = TRUE),
           totempleos_anual = sum(total_gastos_aapp), na.rm = TRUE,
           sp_anual = totrecursos_anual - totempleos_anual, 
           deuda_anual = sum(deuda, na.rm = TRUE))%>%
    ungroup() %>%
    mutate(
      sp_pibn = saldo_primario/PIBnom_anual * 100,
      balance_primario = total_ingresos_aapp - total_gastos_aapp,
      deuda_pibn = deuda / PIBnom_anual * 100, 
      cn_fin_pibn = cn_fin / PIBnom_anual * 100, 
      bp_pibn = balance_primario / PIBnom_anual * 100, 
      intpagados_pib = int_pagados / PIBnom_anual * 100,
      intpagados_deuda = int_pagados / deuda * 100,
      gasto_pib = total_gastos_aapp / PIBnom_anual * 100,
      ingresos_pib = total_ingresos_aapp / PIBnom_anual * 100,
      sub_pibn = subvenciones / PIBnom_anual * 100,
      prest_desempleo_pibn = prest_desempleo / PIBnom_anual * 100,
      deuda_real = deuda / deflactor,
      gasto_real = total_gastos_aapp / deflactor) %>%
    arrange(Año, Trimestre) %>%
    mutate(
      deflactor_anterior = lag(deflactor), 
      tasa_inflacion = (deflactor - deflactor_anterior) / deflactor_anterior * 100) %>%
    mutate(
      deuda_pibn_anterior = lag(deuda_pibn),
      bp_pibn_anterior = lag(bp_pibn),
      pibr_anterior = lag(PIBreal),
      deuda_anterior = lag(deuda_anual),
      pibr_anterior_anual = lag(PIBreal_anual),
      tc_pibr = (PIBreal - pibr_anterior) / pibr_anterior * 100,
      tc_pibr_anual = (PIBreal_anual - pibr_anterior_anual) / pibr_anterior_anual * 100,
      tc_deuda_anual = (deuda_anual - deuda_anterior) / deuda_anterior * 100, 
      deuda_sq = deuda_pibn^2,
      q1 = ifelse(Trimestre==1,1,0),
      q2 = ifelse(Trimestre==2,1,0),
      q3 = ifelse(Trimestre==3,1,0))
  
    #BASE$tintnom <- coalesce(BASE$tintnom, BASE$intpagados_deuda) 
    
    BASE <- BASE %>%
      mutate(tint_real = tintnom - tasa_inflacion,
             ln_deuda_pibn = log(deuda_pibn),
             ln_PIBr = log(PIBreal),
             ln_gasto = log(gasto_pib), 
             ln_gastor = log(gasto_real)) %>%
      filter(Año >1994, Año < 2023) 
    
    #formateamos fecha 
    BASE <- BASE %>%
      mutate(fecha = paste("1", Trimestre*3, Año, sep = "-"), 
             fecha = as.Date(fecha, format = "%d-%m-%Y"))
    
    #calculamos el ciclo y la tendencia del log del PIBreal con el filtro HP 
    y_hpresult <- hpfilter(BASE$ln_PIBr, freq=1600)
    g_hpresult <- hpfilter(BASE$ln_gasto, freq=1600)
    gr_hpresult <- hpfilter(BASE$ln_gastor, freq=1600)
    
    BASE <- BASE %>%
      mutate(
        tendencia_gasto = g_hpresult$trend,
        ciclo_gasto = g_hpresult$cycle,
        tendencia_PIBr = y_hpresult$trend,
        ciclo_PIBr = y_hpresult$cycle, 
        tendencia_gastor = gr_hpresult$trend,
        ciclo_gastor = gr_hpresult$cycle
      )
    rm(y_hpresult, g_hpresult, gr_hpresult)
  
  
  # Parte 6 - Análisis Descriptivo 
    
    #evolución deuda real y deuda nominal
    
    ggplot(data = BASE, aes(x = fecha)) +
      geom_line(aes(y = deuda_real, color = "Deuda Real"), linewidth = 0.8) +
      geom_line(aes(y = deuda, color = "Deuda Bruta"), linewidth = 1) +
      theme_minimal() + 
      theme(axis.title = element_text(size = 8), axis.title.x = element_text(margin = margin(t = 6)),
            axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid")) +
      labs(
        colour = NULL,
        x = "Año",
        y = "Millones de euros") + 
      scale_color_manual(values = c("Deuda Real" = "sandybrown", "Deuda Bruta" = "cyan4"))
    
    #graficamos variables anuales
    
    BASE_anual <- BASE %>%
        filter(Trimestre <= 1)
    
    # tasa de crecimiento de la deuda publica vs pib   
    
    ggplot(data = BASE_anual, aes(x = fecha)) +
      geom_line(aes(y = tc_deuda_anual, color = "Deuda"), size = 0.8) +
      geom_line(aes(y = tc_pibr_anual, color = "PIB real"), size = 1) +
      geom_hline(yintercept = 0, color = "red", size = 1, linetype = "dashed") +
      theme_minimal() + 
      theme(axis.title = element_text(size = 8), axis.title.x = element_text(margin = margin(t = 6)),
            axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid"),
            legend.position = "right")  + 
      labs(
        colour = NULL,
        x = "Año",
        y = "Tantos por uno") + 
      scale_color_manual(values = c("Deuda" = "sandybrown", "PIB real" = "cyan4")) + 
      ggtitle("Tasas de crecimiento Deuda y PIB")

    
    
    #evolución deuda/pib desde 1995.
    
    ggplot(data = BASE, aes(x = fecha)) +
      geom_line(aes(y = deuda_pibn), color = "cyan4", size = 0.8) +
      theme_minimal() + 
      theme(legend.position ="none", 
            axis.title = element_text(size = 8), 
            axis.title.x = element_text(margin = margin(t = 6)),
            axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid")) + 
      labs(
        colour = NULL,
        x = "Año",
        y = "%") +
      ggtitle("Deuda/PIB")
    
    #evolución intereses pagados/pib
    
    ggplot(data = intpagados, aes(x = Año)) +
      geom_line(aes(y = int_pagados_anual, color = "Intereses de la deuda"), color = "cyan4", size = 0.8) +
      theme_minimal() + 
      theme(legend.position ="none", 
            axis.title = element_text(size = 8), 
            axis.title.x = element_text(margin = margin(t = 6)),
            axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid")) + 
      labs(
        colour = NULL,
        x = "Año",
        y = "Millones de euros")+
      ggtitle("Intereses pagados deuda")
    
    #evolución saldo primario
    
    ggplot(data = BASE, aes(x = fecha)) +
      geom_line(aes(y = bp_pibn, color = "Balance primario"), color = "cyan4", size = 0.8) +
      geom_hline(yintercept = 0, color = "red", size = 1, linetype = "dashed") +
      theme_minimal() + 
      theme(legend.position ="none", 
            axis.title = element_text(size = 8), 
            axis.title.x = element_text(margin = margin(t = 6)),
            axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid")) + 
      labs(
        colour = NULL,
        x = "Año",
        y = "%") + 
      ggtitle("Balance Primario/PIB")
    
    #evolución ingresos aapp vs gastos aapp
    
    ggplot(data = BASE, aes(x = fecha)) +
      geom_line(aes(y = total_gastos_aapp, color = "Gastos"), size = 0.8) +
      geom_line(aes(y = total_ingresos_aapp, color = "Ingresos"), size = 0.8) +
      theme_minimal() + 
      theme(axis.title = element_text(size = 8), axis.title.x = element_text(margin = margin(t = 6)),
            axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid"),
            legend.position ="right",) + 
      labs(
        colour = NULL,
        x = "Año",
        y = "Millones de euros") +
      scale_color_manual(values = c("Ingresos" = "sandybrown", "Gastos" = "cyan4"))+
      ggtitle("Ingresos y Gastos AAPP")
    
    #evolución tipo de interés nominal y real pagado por las aapp españolas 
  
    ggplot(data = BASE, aes(x = fecha)) +
      geom_line(aes(y = tintnom, color = "Tipo de interés nominal"), size = 1) +
      geom_line(aes(y = tint_real, color = "Tipo de interés real"), size = 0.6) +
      theme_minimal() + 
      theme(axis.title = element_text(size = 8), axis.title.x = element_text(margin = margin(t = 6)),
            axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid")) + 
      labs(
        colour = NULL,
        x = "Año",
        y = "Tantos por uno")+
      scale_color_manual(values = c("Tipo de interés nominal" = "sandybrown", "Tipo de interés real" = "cyan4"))+
      ggtitle("Tipos de interés")
    
    #evolución tasa inflación
    
    ggplot(data = BASE, aes(x = fecha)) +
      geom_line(aes(y = tasa_inflacion, color = "Tasa de inflación"), color = "cyan4", size = 0.8) +
      theme_minimal() + 
      theme(axis.title = element_text(size = 8), axis.title.x = element_text(margin = margin(t = 6)),
            axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid")) + 
      labs(
        colour = NULL,
        x = "Año",
        y = "Tantos por uno")+
      ggtitle("Tasa inflación trimestral")
    

    #evolución prestaciones por desempleo en % del PIB 
    
    ggplot(data = BASE, aes(x = fecha)) +
      geom_line(aes(y = prest_desempleo_pibn, color = "Prestaciones por desempleo en % del PIB"), color = "cyan4", linewidth = 0.8) +
      theme_minimal() + 
      theme(legend.position ="none", 
            axis.title = element_text(size = 8), 
            axis.title.x = element_text(margin = margin(t = 6)), 
            axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid")) + 
      labs(
        colour = NULL,
        x = "Año",
        y = "%")
    
    #scatter saldo primario en %pib vs deuda en %pib 
    
    ggplot(data = BASE, aes(x = deuda_pibn, y = sp_pibn)) + 
      geom_point(color = "purple4") + 
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() + 
      labs(
        x = "Deuda en % del PIB",
        y = "Saldo Primario en % del PIB"
      )
      
    #scatter outputgap y saldo primario
    
    ggplot(data = BASE, aes(x = ciclo_PIBr, y = sp_pibn)) + 
      geom_point(color = "purple4") + 
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() + 
      labs(
        x = "Output gap",
        y = "Saldo Primario en % del PIB"
      )
    
    
    #scatter saldo primario en % pib y gasto en % pib 
    
    ggplot(data = BASE, aes(x = gasto_pib, y = sp_pibn)) + 
      geom_point(color = "purple4") + 
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() + 
      labs(
        x = "Gasto en % del PIB",
        y = "Saldo Primario en % del PIB"
      )
    
    ggplot(data = BASE, aes(x = ciclo_gastor, y = sp_pibn)) + 
      geom_point(color = "purple4") + 
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() + 
      labs(
        x = "Ciclo gasto real",
        y = "Saldo Primario en % del PIB"
      )
    
    
    #scatter saldo primario inflación 
    
    ggplot(data = BASE, aes(x = tasa_inflacion, y = sp_pibn)) + 
      geom_point(color = "purple4") + 
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() + 
      labs(
        x = "Tasa de inflación",
        y = "Saldo Primario en % del PIB"
      )
    
    #scatter saldo primario inflación 
    
    ggplot(data = BASE, aes(x = prest_desempleo_pibn, y = sp_pibn)) + 
      geom_point(color = "purple4") + 
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() + 
      labs(
        x = "Prestaciones por desempleo en % del PIB",
        y = "Saldo Primario en % del PIB"
      )

    
  #Parte7 - Análisis
    
     #contrastamos la presencia de raíces unitarias/estacionariedad en los datos 
    
    adf_deuda_pibn <- adf.test(BASE$deuda_pibn)
    print(adf_deuda_pibn)
    
    adf_ciclo_pibr <- adf.test(BASE$ciclo_PIBr)
    print(adf_ciclo_pibr)
    
    adf_sp_pibn <- adf.test(BASE$sp_pibn)
    print(adf_sp_pibn)
    
    adf_ciclo_gastor <- adf.test(BASE$ciclo_gastor)
    print(adf_ciclo_gastor)
    
    adf_prestdesempleo <- adf.test(BASE$prest_desempleo_pibn)
    print(adf_prestdesempleo)
    
    adf_inflacion <- adf.test(BASE$tasa_inflacion)
    print(adf_inflacion)
      
    #declaramos panel de datos
    BASE <- ts(BASE, start = c(1995,  03), frequency = 4)
    
    #diferenciamos series de interés y restringimos base de datos para la estimación
    sp_pibn_dif = diff(BASE[,"sp_pibn"], lag = 1)
    deuda_pibn_dif = diff(BASE[,"deuda_pibn"], lag = 1)
    sub_pibn_dif = diff(BASE[,"sub_pibn"], lag = 1)
    tasa_inflacion_dif = diff(BASE[,"tasa_inflacion"], lag = 1)
    prest_desempleo_dif = diff(BASE[,"prest_desempleo"], lag = 1)
    deuda_sq_dif = diff(BASE[,"deuda_sq"], lag = 1)
    deuda_pibn_anterior_dif = diff(BASE[,"deuda_pibn_anterior"], lag = 1)
  
    #contrastamos la estacionariedad de las series diferenciadas
    
    adf_deuda_pibn_dif <- adf.test(deuda_pibn_dif)
    print(adf_deuda_pibn_dif)

    adf_sp_pibn_dif <- adf.test(sp_pibn_dif)
    print(adf_sp_pibn_dif)
    
    adf_inflacion_dif <- adf.test(tasa_inflacion_dif)
    print(adf_inflacion_dif)
    
    adf_prest_desempleo_dif <- adf.test(prest_desempleo_dif)
    print(adf_prest_desempleo_dif)
      
    #Model0: Básico
    
    modelo0 <- lm(sp_pibn ~ deuda_pibn, data = BASE)
    summary(modelo0)
    vcov0 <- vcovHAC(modelo0)
    
    coeftest(modelo0, vcovHAC(modelo0))
    
    residuos0 <- residuals(modelo0)
    modelo_arima_residuos <- Arima(residuos0, order = c(1,0,0), include.constant = FALSE)
    summary(modelo_arima_residuos)
    
    ts.plot(residuals(modelo0))
    
    adf_res0 <- adf.test(residuos0)
    print(adf_res0)
    
    acf(residuos0)
    
      ## controlando estacionalidad
    modelo0s <- lm(sp_pibn ~ deuda_pibn+q1+q2+q3, data = BASE)
    summary(modelo0s)
    vcov0s <- vcovHAC(modelo0s)
    
    coeftest(modelo0s, vcovHAC(modelo0s))
    
    residuos0s <- residuals(modelo0s)
    modelo_arima_residuoss <- Arima(residuos0s, order = c(1,0,0), include.constant = FALSE)
    summary(modelo_arima_residuoss)

    
    adf_res0s <- adf.test(residuos0s)
    print(adf_res0s)
    
    acf(residuos0s)
    
    #Model1: Nuestro
    
    modelo1 <- lm(sp_pibn ~ deuda_pibn + ciclo_PIBr + prest_desempleo_pibn, data = BASE)
    summary(modelo1)
    vcov1 <- vcovHAC(modelo1)
    
    coeftest(modelo1, vcovHAC(modelo1))
    
    ts.plot(residuals(modelo1))
    
    bg_test1 <- bgtest(modelo1, order = 2)
    print(bg_test1)
    
    acf(residuals(modelo1))
    
      ##controlando estacionalidad
    modelo1s <- lm(sp_pibn ~ deuda_pibn + ciclo_PIBr + prest_desempleo_pibn+q1+q2+q3, data = BASE)
    summary(modelo1s)
    vcov1s <- vcovHAC(modelo1s)
    
    coeftest(modelo1s, vcovHAC(modelo1s))
    
    bg_test1s <- bgtest(modelo1s, order = 2)
    print(bg_test1s)
    
    acf(residuals(modelo1s))
    
       #Model1: Nuestro_corregido
    
    modelo1c <- lm(sp_pibn ~ deuda_pibn + ciclo_PIBr + tipocambio_dolar_euro, data = BASE)
    summary(modelo1c)
    vcov1c <- vcovHAC(modelo1c)
    
    coeftest(modelo1c, vcovHAC(modelo1c))
    
    ts.plot(residuals(modelo1c))
    
    bg_test1c <- bgtest(modelo1c, order = 2)
    print(bg_test1c)
    
    acf(residuals(modelo1c))
    
       ##controlando estacionalidad
    modelo1cs <- lm(sp_pibn ~ deuda_pibn + ciclo_PIBr + tipocambio_dolar_euro +q1+q2+q3, data = BASE)
    summary(modelo1cs)
    vcov1cs <- vcovHAC(modelo1cs)
    
    coeftest(modelo1cs, vcovHAC(modelo1cs))
    
    bg_test1cs <- bgtest(modelo1cs, order = 2)
    print(bg_test1cs)
    
    acf(residuals(modelo1cs))
    
    
    #Model2: Propuesta de Bohn 
    
    set.seed(123) # Para reproducibilidad
    n <- length(BASE[,"sp_pibn"])
    paseo_aleatorio <- rep(NA, n)
    paseo_aleatorio[1] <- rnorm(1) # Valor inicial aleatorio
    
    for(i in 2:n) {
      paseo_aleatorio[i] <- paseo_aleatorio[i-1] + rnorm(1)
    }
    
    modelo2 <- lm(sp_pibn ~ deuda_pibn + ciclo_PIBr + paseo_aleatorio, data = BASE)
    summary(modelo2)
    vcov2 <- vcovHAC(modelo2)
    
    coeftest(modelo2, vcovHAC(modelo2))
    ts.plot(residuals(modelo2))
    
    residuos <- residuals(modelo2)
    modelo_arima_residuos <- Arima(residuos, order = c(1,0,0), include.constant = FALSE)
    summary(modelo_arima_residuos) 
    
    bg_test2 <- bgtest(modelo2, order = 2)
    print(bg_test2)
    
    acf(residuals(modelo2))
    
      ##controlando estacionalidad
    
    modelo2s <- lm(sp_pibn ~ deuda_pibn + ciclo_PIBr + paseo_aleatorio +q1+q2+q3, data = BASE)
    summary(modelo2s)
    vcov2s <- vcovHAC(modelo2s)
    
    coeftest(modelo2s, vcovHAC(modelo2s))
    
    residuoss <- residuals(modelo2s)
    modelo_arima_residuos <- Arima(residuoss, order = c(1,0,0), include.constant = FALSE)
    summary(modelo_arima_residuoss) 
    
    bg_test2s <- bgtest(modelo2s, order = 2)
    print(bg_test2s)
    
    acf(resid(modelo_arima_residuoss))
    
    
    #Model3: Lukkezen and Rojas. Mendoza and Ostry
    
    modelo3 <- lm(sp_pibn ~ deuda_pibn + ciclo_PIBr + ciclo_gastor, data = BASE)
    summary(modelo3)
    vcov3 <- vcovHAC(modelo3)
    
    coeftest(modelo3, vcovHAC(modelo3))
    ts.plot(residuals(modelo3))
    
    bg_test3 <- bgtest(modelo3, order = 2)
    print(bg_test3)
    
    acf(residuals(modelo3))
    
    modelo3a <- lm(sp_pibn ~ deuda_pibn + ciclo_PIBr + ciclo_gasto + deuda_sq, data = BASE)
    summary(modelo3a)
    
      ##controlando estacionalidad
    
    modelo3s <- lm(sp_pibn ~ deuda_pibn + ciclo_PIBr + ciclo_gastor +q1+q2+q3, data = BASE)
    summary(modelo3s)
    vcov3s <- vcovHAC(modelo3s)
    
    coeftest(modelo3s, vcovHAC(modelo3s))
    
    bg_test3s <- bgtest(modelo3s, order = 2)
    print(bg_test3s)
    
    acf(residuals(modelo3s))
    
    #Model4: ECB - Afonso & Toffano 
    
    modelo4 <- lm(sp_pibn ~ deuda_pibn + ciclo_PIBr + ciclo_gastor + tasa_inflacion, data = BASE)
    summary(modelo4)
    vcov4 <- vcovHAC(modelo4)
    
    coeftest(modelo4, vcov = NeweyWest(modelo4, lag = 4))
    ts.plot(residuals(modelo4))
    
    bg_test4 <- bgtest(modelo4, order = 2)
    print(bg_test4)
    
    acf(residuals(modelo4))
    
      ##controlando estacionalidad
    
    modelo4s <- lm(sp_pibn ~ deuda_pibn + ciclo_PIBr + ciclo_gastor + tasa_inflacion +q1+q2+q3, data = BASE)
    summary(modelo4s)
    vcov4s <- vcovHAC(modelo4s)
    
    coeftest(modelo4s, vcovHAC(modelo4s))
    
    bg_test4s <- bgtest(modelo4s, order = 2)
    print(bg_test4s)
    
    acf(residuals(modelo4s))
    
    # Exportamos resultados modelos con stargazer 

    stargazer(modelo0, modelo1, modelo1c, modelo2, modelo3, modelo4, se = list(sqrt(diag(vcov0)), sqrt(diag(vcov1)), sqrt(diag(vcov1c)), sqrt(diag(vcov3)), sqrt(diag(vcov4))),type = "latex", out = "tablamodelos.tex")
    stargazer(modelo0s, modelo1s, modelo1cs, modelo2s, modelo3s, modelo4s, se = list(sqrt(diag(vcov0s)), sqrt(diag(vcov1s)), sqrt(diag(vcov1cs)), sqrt(diag(vcov3s)), sqrt(diag(vcov4s))),type = "latex", out = "tablamodelos.tex")
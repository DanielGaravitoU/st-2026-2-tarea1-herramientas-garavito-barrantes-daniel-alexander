library(tibble)
library(dplyr)

leer_serie <- function(x, fuente, unidad) {
  
  # Validamos que tipo de dato es: si es ts o ruta csv
  if (is.ts(x)) {
    
    # Creamos las columnas para la base de datos
    y <- as.numeric(x)
    t <- 1:length(x)
    inicio <- start(x)
    frecuencia <- frequency(x)
    
    # Construimos la fecha
    fecha_inicio <- as.Date(
      # Le damos formato año-periodo-dia
      paste(inicio[1], inicio[2], 1, sep = "-")
    )
    
    # Calculamos cada cuánto tiempo avanza la serie
    meses <- 12 / frecuencia
    
    # Construimos las fechas
    fecha <- seq(
      fecha_inicio,
      by = paste(meses, "months"),
      length.out = length(x)
    )
    
    # Construimos la tabla
    datos <- tibble(t, fecha, y)
    
  } else if (is.character(x)) {
    
    # Es una ruta con una base csv
    datos <- read.csv(x)
    
    # Validamos que las columnas fecha y valor estén en la base
    if ("fecha" %in% names(datos) && "valor" %in% names(datos)) {
      
      # Convertimos el dataframe a tibble para trabajar mejor con algunas librerías
      datos <- as_tibble(datos)
      
      # Creamos las columnas de la base
      datos$fecha <- as.Date(datos$fecha)
      datos <- datos[order(datos$fecha), ]
      datos$y <- datos$valor
      datos$t <- 1:nrow(datos)
      
      # Validamos frecuencia y si están espaciadas
      diferencia <- as.numeric(diff(datos$fecha))
      
      if (any(diferencia <= 0)) {
        stop("Las fechas deben ser crecientes")
      }
      
      dif_pro <- median(diferencia)
      
      if (dif_pro == 1) {
        frecuencia <- 365
      } else if (dif_pro %in% 28:31) {
        frecuencia <- 12
      } else if (dif_pro %in% 89:92) {
        frecuencia <- 4
      } else if (dif_pro %in% 360:366) {
        frecuencia <- 1
      } else if (dif_pro == 7) {
        frecuencia <- 52
      } else {
        stop("No se pudo inferir la frecuencia de la serie")
      }
      
      # Permitimos un margen de error ya que no todos los meses tienen la misma cantidad de días
      if ((max(diferencia) - min(diferencia)) > 3) {
        stop("Las fechas no son equiespaciadas")
      }
      
      datos <- tibble(
        t = 1:nrow(datos),
        fecha = datos$fecha,
        y = datos$valor
      )
      
    } else {
      stop("El archivo debe contener las columnas fecha y valor")
    }
    
  } else {
    stop("x debe ser un objeto ts o una ruta a un archivo csv")
  }
  
  # Atributos de los datos
  attr(datos, "frecuencia") <- frecuencia
  attr(datos, "fuente") <- fuente
  attr(datos, "unidad") <- unidad
  
  return(datos)
}


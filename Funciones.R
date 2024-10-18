## funciopn para integrar pico
fun.int <- function(m, izq, der){
  # m[,1] = wl
  # ,[,2...n] = spectra
  m %>% filter(between(wl, izq, der)) %>% select(!wl) %>% map(sum)
}


# Función para sumar cada fila de una matriz en grupos de n columnas
sumar_filas_por_grupos <- function(matriz, n, wl = T) {
  if(wl == T){
    # Calcular el número de grupos de columnas
    wl <- matriz$wl
    m <- matriz[,2:ncol(matriz)]
    
    num_grupos <- ncol(m) / n
    
    # Verificar que el número de columnas sea múltiplo de n
    if (ncol(m) %% n != 0) {
      stop("El número de columnas no es múltiplo de n")
    }
    
    # Sumar las filas por cada grupo de n columnas
    resultado <- sapply(1:num_grupos, function(i) {
      cols <- ((i - 1) * n + 1):(i * n)  # Rango de columnas para el grupo actual
      rowSums(m[, cols])  # Sumar las filas en el rango de columnas
    })
    
    # Convertir el resultado en una matriz si es necesario
    resultado_matriz <- as.matrix(resultado)
    data.frame(wl, resultado_matriz)
  } else{
    m <- matriz
    
    num_grupos <- ncol(m) / n
    
    # Verificar que el número de columnas sea múltiplo de n
    if (ncol(m) %% n != 0) {
      stop("El número de columnas no es múltiplo de n")
    }
    
    # Sumar las filas por cada grupo de n columnas
    resultado <- sapply(1:num_grupos, function(i) {
      cols <- ((i - 1) * n + 1):(i * n)  # Rango de columnas para el grupo actual
      rowSums(m[, cols])  # Sumar las filas en el rango de columnas
    })
    
    # Convertir el resultado en una matriz si es necesario
    resultado_matriz <- as.matrix(resultado)
    data.frame(resultado_matriz)
  }
}


promediar_grupos_aleatorios <- function(vector, n) {
  # Verificar que la longitud del vector sea divisible por 5
  if (length(vector) %% n != 0) {
    stop("La longitud del vector debe ser múltiplo de 5.")
  }
  
  # Mezclar aleatoriamente las posiciones del vector
  posiciones_aleatorias <- sample(length(vector))
  
  # Dividir las posiciones en grupos de 5 y calcular el promedio para cada grupo
  promedios <- sapply(split(vector[posiciones_aleatorias], 
                            rep(1:(length(vector) / n), each = n)), mean)
  
  return(promedios)
}

fun.WL.calibration <-  function(old, ref){
    ## ref debe ser una longitud de onda que tenga 10 posiciones limpias a cada lado
    ## 
    ## funcion de correccion
    old$wl <- round(old$wl,4)
    new <- list()
    for (i in 1:(ncol(old)-1)) {
        
        df <- data.frame(old[,c(1,i+1)]) # wl,Xn
        p <- which(df$wl == ref)
        window <- df[(p-5):(p+5),]
        max_wl <- window[which(window[,2] == max(window[,2])),1]
        
        if(max_wl != ref){
            delta <- ref - max_wl
            if(delta < 0){
                df$wl <- df$wl + delta
            }else{
                df$wl <- df$wl - delta
            }
        }
        
        new[[i]] <- df
    }
    new
}

# ref <- 145 # wl de pico
# df <- data.frame(wl = 101:200, libs = c(101:150,149:100))
# p <- which(df$wl == ref)
# window <- df[(p-10):(p+10),]
# max_wl <- window[which(window[,2] == max(window[,2])),1]
# 
# if(max_wl != ref){
#     delta <- ref - max_wl
#     if(delta < 0){
#         df$wl <- df$wl + delta
#     }else{
#         df$wl <- df$wl - delta
#     }
# }

N <- 663804

#Define initial data obtained by the chart
limits <- c(0, 2, 10, 50, 100)    #Interval limits
heigths <- c(3.22, 2.42, 1.34, 0.41)


#Calculate width of each interval 
width <- diff(limits)

#Calculate relative frequencies
rel_freq <- width * heigths

#Calculate Absolute frequencies
abs_freq <- round((rel_freq / 100) * N)

#Create freq chart
tabla_frequencias <- data.frame(
  Intervalo = c("[0, 2)", "[2, 10)", "[10, 50)", "[50, 100)"), 
  Ancho = width, 
  Altura = heigths, 
  Frec_Relativa = rel_freq, 
  Frec_Absoluta = abs_freq
)

print(tabla_frequencias)


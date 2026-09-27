xi <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
ni <- c(2, 9, 14, 20, 18, 15, 9, 6, 4, 2, 1)
N <- sum(ni)

weighted_mean <- function(x, n, N){
  total_sum <- sum(x * n)
  
  return(total_sum / N)
}



mode <- function(x, n){
  #Find index of highest frequency
  highest_freq_index <- which.max(n)
  
  #Get the value at that position 
  return(x[highest_freq_index])
}



median <- function(x, n){
  #Calculate the cumulative frequency
  cum_freq <- cumsum(n)
  
  #Find the total number of observations (the last cumulative frequency)
  total_n <- tail(cum_freq, 1)
  
  #Find the index where the cumulative frequency first crosses the midpoint
  median_index <- which(cum_freq >= total_n / 2)[1]
  
  #Get the value at that position
  result <- x[median_index]
  
  return(result)
}




quartiles <- function(x, n, q_number){
  #Calculate the cumulative frequencies
  cum_freq <- cumsum(n)
  
  #Get the total number of observations (the final sum)
  total_n <- tail(cum_freq, 1)
  
  if(q_number == 1){
    target <- total_n * 0.25
  } else if(q_number == 2){
    target <- total_n * 0.5
  } else if(q_number == 3){
    target <- total_n * 0.75
  } else{
    stop("The quartile number must be 1, 2, or 3")
  }
  
  #Search for the first value that is greater or equal to the objective
  index <- which(cum_freq >= target)[1]
  
  return(x[index])
}


mean <- weighted_mean(xi, ni, N)
mode <- mode(xi, ni)
median <-median(xi, ni)
q3 <- quartiles(xi, ni, 3)
q1 <- quartiles(xi, ni, 1)

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
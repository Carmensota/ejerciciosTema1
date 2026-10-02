median <- function(x){
  x_ordered <- sort(x[!is.na(x)])
  n <- length(x_ordered)
  if(n %% 2 == 1){
    return (x_ordered[(n+1) / 2])
  } else{
    return((x_ordered[n/2] + x_ordered[(n/2) + 1])/2)
  }
}


quartil <- function(x, p){
  x_ordered <- sort(x[!is.na(x)])
  n <- length(x_ordered)   #number of values
  pos <- p * (n+1)         #Calculates the theoretical position of the quartil
  k <- floor(pos)          #obtains the integer part of the position
  d <- pos - k             #obtains the decimal part of the position
  
  if(k < 1) return (x_ordered[1])    #if the integer part is less than 1, returns the first element
  if(k>=n) return(x_ordered[n])      #if the integer part is greater than the number of elements, returns the element n
  
  return(x_ordered[k] + d * (x_ordered[k+1] - x_ordered[k]))
}

interquartileRange <- function(q3, q1){
  return (q3 - q1)
}


standardDeviation <- function(x){
  x_clear <- x[!is.na(x)]
  m <- mean(x_clear)
  n <- length(x_clear)
  return(sqrt(sum((x_clear - m)^2) / n))
}
#Standard deviation = absolute dispersion


coefficientOfVariation <- function(standard_deviation, mean){
  return (standard_deviation / mean)
}
#Coefficient of variation = relative dispersion



centralMoment <- function(x, k){
  x_clear <- x[!is.na(x)]
  n <- length(x_clear)
  m <- mean(x_clear)
  
  return(sum((x_clear - m)^k)/n)
}


asymmetry <- function(x){
  m3 <- centralMoment(x, 3)
  sigma <- standardDeviation(x)
  return(m3 / (sigma^3))
}


kurtosis <- function(x){
  m4 <- centralMoment(x, 4)
  sd <- standardDeviation(x)
  
  return((m4/(sd^4)) - 3)
}








xi <- sort(unique(data))
N_total <- length(data)

ni <- sapply(xi, function(val) sum(data == val))

Ni <- cumsum(ni)

fi <- round(ni/N_total, 4)
Fi <- round(cumsum(fi), 4)

frequencies_table <- data.frame(
  Grade_xi = xi, 
  Frec_Abs_ni = ni, 
  Frec_Abs_Acum_Ni = Ni, 
  Frec_Rel_fi = fi, 
  frec_Rel_Acum_Fi = Fi
)

print(frequencies_table)

barplot(    #bar chart
  height = ni, 
  names.arg = xi, 
  xlab = "grades", 
  ylab = "absolute frequency",
  ylim = c(0, max(ni) + 2), 
  border = "black"
)







#FUNCTIONS WHEN WE ONLY HAVE XI AND NI
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






#OBTAIN INFORMATION FROM HISTOGRAM
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











#A partir de tabla con intervalos #Create vectors with data from the table
intervals <- c("(20, 50]", "(50, 60]", "(60, 70]", "(70, 80]", "(80, 100]")
xi <- c(35, 55, 65, 75, 90)
ni <- c(2, 8, 24, 18, 28)
a_i <- c(30, 10, 10, 10, 20)


N <- sum(ni)
Ni <- cumsum(ni)
fi <- ni / N
Fi <- cumsum(fi)
hi <- ni / a_i


#Construct table structured as Data Frame
freq_table <- data.frame(
  Interval = intervals, 
  Class = xi, 
  Frec_Abs = ni, 
  Frec_Acum = Ni, 
  Freq_Rel = fi, 
  Freq_Rel_Acum = Fi, 
  Amplitud = a_i, 
  Densidad = round(hi, 3)
)


print(freq_table)


#Generate histogram for intervals
#We define the limits of each interval 
limits <- c(20, 50, 60, 70, 80, 100)

data_simulated <- rep(xi, times = ni)

hist(data_simulated, 
     breaks = limits, 
     freq = FALSE, 
     col = "lightblue", 
     border = "black", 
     xaxt = "n")

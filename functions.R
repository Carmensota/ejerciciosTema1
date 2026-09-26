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



coefficientOfVariation <- function(standard_deviation, mean){
  return(standard_deviation / mean)
}



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
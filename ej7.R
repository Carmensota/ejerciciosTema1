#Create values for example
a <- 5
b <- 3

x <- c(10, 12, 15, 18, 20, 22, 25, 30)

y <- a + b * x

mean_x <- mean(x)
mean_y <- mean(y)

#Verification of third central moment: mu_3(y) = b^3* mu_3(x)
centralMoment <- function(x, k){
  x_clear <- x[!is.na(x)]
  n <- length(x_clear)
  m <- mean(x_clear)
  
  return(sum((x_clear - m)^k)/n)
}

mu3_x <- centralMoment(x, 3)

mu3_y <- centralMoment(y, 3)

mu3_y_theoretical <- (b^3) * mu3_x

#Verification of forth central moment
mu4_x <- centralMoment(x, 4)
mu4_y <- centralMoment(y, 4)

mu4_y_theoretical <- (b^4) * mu4_x
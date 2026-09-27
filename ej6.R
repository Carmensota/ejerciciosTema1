#Create vectors with data from the table
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
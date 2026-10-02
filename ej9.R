wages <- c(800, 1100, 1200, 1400, 1600, 1700)

mean <- mean(wages)
print(mean)

#To create the new variable, we have to subtract the mean to every value of the variable 
y <- wages-1300
print(y)

mean_y <- mean(y)
print(mean_y)
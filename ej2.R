data <- c(5, 2, 4, 9, 5, 7, 4, 5, 6, 5, 7, 7, 5, 5, 2, 10, 5, 6, 5, 4, 
          5, 8, 8, 4, 0, 8, 4, 8, 6, 6, 3, 6, 7, 6, 6, 7, 6, 7, 3, 5, 
          6, 9, 6, 1, 4, 6, 3, 5, 5, 6, 7)


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
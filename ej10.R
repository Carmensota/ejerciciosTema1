meanA <- 1495
meanB <- 1875

sdA <- 280
sdB <- 310 

#Absolute dispersion = standard deviation
if(sdA > sdB){ 
  print("Greater absolute dispersion in A")
} else{
  print("Greater absolute dispersion in B")
}

#Relative dispersion = CV = sd / mean
CV_A <- sdA / meanA
CV_B <- sdB / meanB

if(CV_A > CV_B ){
  print("Greater relative dispersion in A")
} else{
  print("Greater relative dispersion in B")
}
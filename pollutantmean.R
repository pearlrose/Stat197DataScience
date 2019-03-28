getwd()
setwd("C:/Users/pearl/Desktop/specdata")

#pollutantmean function----
pollutantmean<- function(directory,pollutant,n=1:332){
  
  #assigning all the names of the csv files in the directory
  x<- dir(directory,pattern = ".csv")  
  #binding all the csv files in specdata
  z<- read.csv(x[1])
  for(i in 2:332){
    z <- rbind(z, read.csv(x[i]))
  }
  
  p <- z[z$ID==n[1],]
  for(i in n[-1]){#if length of n is 1, the loop won't run
    p<- rbind(p,z[z$ID==i,])
  }
  
  mean(p[,pollutant],na.rm = T)#mean 
  
}
#verify
pollutantmean("C:/Users/pearl/Desktop/specdata","sulfate",1:10)
pollutantmean("C:/Users/pearl/Desktop/specdata","nitrate",70:72)
pollutantmean("C:/Users/pearl/Desktop/specdata", "nitrate",23)

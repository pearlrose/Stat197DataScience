#complete function----

complete <- function(directory, id=1:332){
  x<- dir(directory,pattern = ".csv")  
  #binding all the csv files in specdata
  z<- read.csv(x[1])
  for(i in 2:332){
    z <- rbind(z, read.csv(x[i]))
  }
  
  p=NA
  g<- table(z[complete.cases(z),4])
  for(i in 1:332){#for monitors with no complete cases
    if(length(g[names(g)==as.character(i)])==1 ){
      p[i] <-g[names(g)==as.character(i)] 
    }
    else{p[i] <- NA}
  }
  data.frame(id,nobs=p[id])
}

complete("C:/Users/pearl/Desktop/specdata",1)
complete("C:/Users/pearl/Desktop/specdata",c(2,4,8,10,12))
complete("C:/Users/pearl/Desktop/specdata",30:25)
complete("C:/Users/pearl/Desktop/specdata",3)
complete("C:/Users/pearl/Desktop/specdata",c(286,332,291,292))
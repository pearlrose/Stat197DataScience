#corr function
corr <- function(directory, threshold=0){
  x<- dir(directory,pattern = ".csv")  
  #binding all the csv files in specdata
  z<- read.csv(x[1])
  for(i in 2:332){
    z <- rbind(z, read.csv(x[i]))
  }
  
  o=0
  correlation=NA
  for(i in 1:332){
    if(sum(complete.cases(z[z$ID==i,]))> threshold){  #checks if the complete cases of a monitor is greater than the threshold
      o <- z[z$ID==i,]                                #assign all observations of monitor i to o 
      c<- o[complete.cases(o),]                       #assign all complete cases to c
      correlation[i]<-cor(c$sulfate,c$nitrate)        #assign correleation of sulfate and nitrate from monitor i to correlation
    }
  }
correlation[!is.na(correlation)]                      #prints correlation of all complete cases that is not NA
  
}


cr <- corr("C:/Users/pearl/Desktop/specdata",150)
head(cr)
summary(cr)

cr <- corr("C:/Users/pearl/Desktop/specdata",400)
head(cr)
summary(cr)

cr <- corr("C:/Users/pearl/Desktop/specdata",5000)
head(cr)
summary(cr)
length(cr)

cr <- corr("C:/Users/pearl/Desktop/specdata")
cr
summary(cr)
length(cr)
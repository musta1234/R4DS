# Aug 2018: treeWAS on cgSNPs
# clear R's brain
rm( list = ls())

# load libraries
preq = c( "dplyr", "ggplot2", "ggfortify", "readxl", "tidyr", 
          "readr", "stats", "ade4", "ape", "adegenet", 
          "phangorn", "outbreaker2", "igraph", "treeWAS",
          "tidyverse", "haven", "Matrix", "foreign",
          "readstata13", "maps", "mapdata", "ggpubr", "gitcreds" )
#for (y in preq) install.packages(y, dep = TRUE)
sapply(preq, library, character.only=T)

rm( list = ls())

rm( list = ls())
myloc <- "C:/Users/MM/Documents/DataScienceCourses/myRprojectFolder/DataScience/specdata/"

pollutantmean <- function(directory = myloc, pollutant, id=1:332 ) {
  setwd(directory)
  temp = list.files(directory, pattern="*.csv")
  myfiles = lapply(temp[id], read.csv)
  test = lapply(temp, read.csv)
  myvec = numeric()
  for (i in 1:length(test)) {
    myvec = append(myvec, test[[i]][[pollutant]])
    #print(i, myvec)
  }
  return(mean(myvec, na.rm = T))
}
complete <- function(directory = myloc, id = 1:332 ) {
  setwd(directory)
  temp = list.files(directory, pattern="*.csv")
  #myfiles = lapply(temp[id], read.csv)
  test = lapply(temp[id], read.csv)

  mydf = data.frame(matrix(ncol = 2, nrow = length(test)))
  colnames(mydf) <- c("id", "nobs")
  
  for (i in 1:length(test)) {
    mydf$id[i] = test[[i]]$ID[1]
    mydf$nobs[i] = sum(complete.cases(test[[i]] ))
      #print(i, myvec)
  }
  return(mydf)
}
correl <- function(directory = myloc, threshold = 0 ) {
  setwd(directory)
  id = 1:332
  temp = list.files(directory, pattern="*.csv")
  myfiles = lapply(temp[id], read.csv)
  test = lapply(temp, read.csv)
  mydf = data.frame(matrix(ncol = 2, nrow = length(id)))
  colnames(mydf) <- c("id", "nobs")
  corr_vec = numeric()
  for (i in 1:length(test)) {
    mydf$id[i] = test[[i]]$ID[1]
    mydf$nobs[i] = sum(complete.cases(test[[i]] ))
    if (sum(complete.cases(test[[i]] )) > threshold) {
      x <- test[[i]][complete.cases(test[[i]]) , c(2,3)]
      corr_vec = append(corr_vec, cor(x[ ,1], x[ , 2]))
      
      }
    }
  return(corr_vec)
  }
  

correl() 

cr <- correl()                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- correl(threshold = 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

set.seed(42)
cc <- complete(id = 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cc <- complete(id=54)
print(cc$nobs)


cc <- complete( id = c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

pollutantmean(myloc, "sulfate", id = 1:10)
pollutantmean(myloc, "nitrate", id = 70:72)
pollutantmean(myloc, "sulfate", id = 34)
pollutantmean(myloc, "nitrate")

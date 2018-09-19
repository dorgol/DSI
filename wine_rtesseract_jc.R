# Playing with the settings for processing the images in Rtesseract

wd = "~/Documents/DSI/Wine_DataFest/"
setwd(wd)

# 0. Setup 
library(Rtesseract)
library(tidyverse)

tmp = getLines("~/Documents/DSI/Wine_DataFest/SampleCatalogPages/UCD_Lehmann_0011.jpg", hor = 100, vert = 100)
tmp = GetText("~/Documents/DSI/Wine_DataFest/SampleCatalogPages/UCD_Lehmann_0011.jpg")
tmp = GetBoxes("~/Documents/DSI/Wine_DataFest/SampleCatalogPages/UCD_Lehmann_0047.jpg")


# Evaluate "Get Boxes" output relative to "truth"

FullBoxes = readRDS("FullBoxes.rds")
isPriceCol <- function(getboxoutput1) {
  prices =  getboxoutput1[isPrice(getboxoutput1$text),]  
  prices = filter(prices, confidence > 50)
  prices$center = (prices$left + prices$right)/2
  clust = list(
  L = sapply(1:max(1, min(10, (nrow(prices)-2))), function(x) {
      pam1 = pam(prices$left, k = x)
        obj1 = pam1$objective[[2]] #evaluation of cluster objective
        min1 = min(pam1$clusinfo[,1]) #minimum cluster size
      return(c(obj1, min1))
    }),
  R = sapply(1:max(1, min(10, (nrow(prices)-2))), function(x) {
     pam1 = pam(prices$right, k = x)
      obj1 = pam1$objective[[2]] #evaluation of cluster objective
      min1 = min(pam1$clusinfo[,1]) #minimum cluster size
     c(obj1, min1)
   }),
  Ce = sapply(1:max(1, min(10, (nrow(prices)-2))), function(x) {
     pam1 = pam(prices$center, k = x)
      obj1 = pam1$objective[[2]] #evaluation of cluster objective
      min1 = min(pam1$clusinfo[,1]) #minimum cluster size
     c(obj1, min1)
   })
  )
  
  #left or right align?
  just = with(clust, max(apply(rbind(clust[["L"]][1,],
                                     clust[["R"]][1,],
                                     clust[["Ce"]][1,]),
                               2, which.min)))
  k = which.min(clust[[just]][1, clust[[just]][2,]>1])
  just = switch(just, "left", "right", "center")
  
  column1 = prices[[just]]
  clust1 = pam(column1, k)
  
  #largest cluster
  which.max(clust1$clusinfo[,1])
  lm(prices[[just]] ~ top, data = prices, subset = clust1$clustering == which.max(clust1$clusinfo[,1]))
  kmeans(prices, centers = 3)
  
  #re-align
  
  
  #look for everything in the columns that's a number

  
  
  
}



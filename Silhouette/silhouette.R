rm(list=ls())
library("cluster", lib.loc="~/Library/R/3.1/library")
#load data
data(iris)
iris$Species <- NULL
irisMatrix <- as.matrix(iris)

#inputMatrix is the data that you want clustered using PAM and then for the silhouette to be calculated
#k is the number of clusters you want when running PAM
silhouette <- function(inputMatrix,k){
  colnames(inputMatrix) <- NULL
  rownames(inputMatrix) <- NULL
  #Run PAM
  runPam <- pam(inputMatrix,k,FALSE,"euclidean")
  dataVector <- runPam$clustering
  
  #Cluster which will store each row that belongs to each specific cluster in it's own unique cluster vector
  clusterList <-list()
  for(i in 1:k){
    clusterList[[i]] <- assign(paste("cluster",i,sep=""),vector()) #Creates the k number of cluster lists to store the data point row number in      
  }
  
  for(i in 1:length(dataVector)){
    clustNumber <- dataVector[i]
    clusterList[[clustNumber]] <- c(clusterList[[clustNumber]],i)
  }
  
  
  #Vector that will store the Si values of each row in the matrix
  siList <- vector()
  
  #Solve for Si of every single point in the input matrix
  #Solve for ai first

  siResults <- vector()
  
  for(num in 1:k){
    currentCluster <- clusterList[[num]]
    clusterList2 <- clusterList
    clusterList2[[num]] <- NULL          #Get the other remaining clusters in order to calculate bi
    for(i in 1:length(currentCluster)){  #i is every sample in the current cluster
      distA <- vector()                  #Store distances between current sample and all other samples within cluster
      index <- currentCluster[i]
      getRow <- inputMatrix[index,]
      otherSamples <- currentCluster[-i] #Removes the CURRENT sample, so that distance to all OTHER samples can be calculated
      for(x in 1:length(otherSamples)){
        getIndex <- otherSamples[x]
        nextRow <- inputMatrix[getIndex,]
        eucDist <- dist(rbind(getRow,nextRow))
        distA <- c(distA,eucDist)
      }#end A loop
      
      ai <- mean(distA) #A VALUE FOR THE CURRENT OBJECT
      
      #start B loop
      
      #List will contain vectors.Each vector will contain the distance between the current sample object to each object in the other cluster
      otherClustAvg <-list()  
      for(i in 1:(k-1)){
        otherClustAvg[[i]] <- assign(paste("cluster",i,sep=""),vector())       
      }
      
      #Loop through all remaining clusters to solve for bi
      for(clusterIndex in 1:(k-1)){   
        otherCluster <- clusterList2[[clusterIndex]]
        for(y in 1:length(otherCluster)){   #go through all objects in other cluster, calculate distance b/w these objects and current object
          numIndex <- otherCluster[y]
          otherRow <- inputMatrix[numIndex,]
          distEuc <- dist(rbind(getRow,otherRow))
          otherClustAvg[[clusterIndex]] <- c(otherClustAvg[[clusterIndex]],distEuc)
        }#end of objects in other cluster
      }#end of other cluster
      
    #Figure out which cluster has the lowest average
    avgB <- vector()
    for(sample in 1:(k-1)){
      distValues <- otherClustAvg[[sample]]
      averageDist <- mean(distValues)
      avgB <- c(avgB,averageDist)
    }
    
    bi <- min(avgB) #B VALUE FOR THE CURRENT OBJECT
    
    #CALCULATE S VALUE FOR CURRENT OBJECT
    si <- (bi-ai) / (max(ai,bi))  
    siResults <- c(siResults,si)
    }#end of current object (i)
  }#end of cluster
  
  
  #Calculate average silhouette for each cluster
  averageClusterSil <- list()
  for(i in 1:k){
    averageClusterSil[[i]] <- assign(paste("cluster",i,sep=""),vector())       
  }
  

  for(z in 1:k){
    thisCluster <- clusterList[[z]] #Get clusters one by one
    for(item in 1:length(thisCluster)){
      rowNumber <- thisCluster[item]
      getSiValue <- siResults[rowNumber]
      averageClusterSil[[z]] <- c(averageClusterSil[[z]], getSiValue)
    }
  }
  
  
  finalAverages <- vector() #Final average of each cluster
  for(number in 1:k){
    clust <- mean(averageClusterSil[[number]])
    finalAverages <- c(finalAverages,clust)
  }
  
  
  avgSilhouette <- mean(siResults)   #Total average silhouette for entire data set
  
  finalResults <- list(siValues=siResults,overallAverage=avgSilhouette,clusterSilhouette=finalAverages)
  return(finalResults)
  
}#End of function


sil <- silhouette(irisMatrix,4)
sil



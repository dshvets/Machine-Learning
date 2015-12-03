rm(list=ls())
data(iris)
iris$Species <- NULL
irisMatrix <- as.matrix(iris)    #convert to matrix

#Note that the data matrix for input must have the samples as rows and the variables as columns. 
#Set any columns/rows that are not desired to be used for the analysis as NULL before using them with the k-means function

#Steps of k-means:
#1. Randomly choose the first k centroids
#2. Calculate the euclidean distance from each point to the centroid
#3. Group the points so that they are with the closest centroid
#4. Re-calculate the new centroid based on the average


#K-means Function:
#x = matrix input, k = # of clusters, max=maximum # iterations
kmeans <- function(matrix,k,max){
  
  colnames(matrix) <- NULL      #Set column names to NULL so that they don't show up in the analysis
  numSamples <- nrow(matrix)
  numVariables <- ncol(matrix)
  centroidRow <- vector()  #initialize vector for saving randomly selected centroid row number
  
  
  for(i in 1:k){                 #choose the k number of rows that will be centroids
    centroid <- sample(1:numSamples,1)   #randomly select the row number from the matrix input
    centroidRow <- c(centroidRow, centroid)
  }
  
  allCentroids <- list()  #This vector will store vectors of the coordinates of the centroids
  index <- 1
  for(i in centroidRow){
    getCentroid <- matrix[i,]
    #print(is.vector(getCentroid)) #Ensure that this returns TRUE, therefore every row is saved as a vector when extracted from the matrix
    allCentroids[[index]] <- getCentroid
    index <- index +1
  }
  
  #Start while loop that will run until the number of iterations reaches the max
  #Iterations are counted with the count variable that is initialized at 1
  count <- 1
  while(count <= max ){
    
    #Initialize the cluster lists, there will be k of them, they will store the samples from irisMatrix 
    clusterList <- list()
    for(i in 1:k){
      clusterList[[i]] <- assign(paste("cluster",i,sep=""),vector()) #Creates the k number of cluster lists to store the data point row number in      
    }
    
    #Calculate the distance from each sample point to each centroid
    for(a in 1:numSamples){
      getSample <- irisMatrix[a,]
      distanceList <- vector()
      for(z in 1:k){
        getCentroid <- allCentroids[[z]]
        eucDist <- dist(rbind(getSample,getCentroid))
        distanceList <- c(distanceList,eucDist)
      }
      minimumDistance <- min(distanceList)
      getIndex <- match(minimumDistance,distanceList)
      #Depending on the index of the minimum distance, that determines the cluster to which that sample belongs to.
      #Here it gets appended to the proper cluster vector
      clusterList[[getIndex]] <- c(clusterList[[getIndex]],a)
    }    
    
    updatedCentroids <- list() #List of updated centroids
    
    for(index in 1:k){
      cluster <- clusterList[[index]]
      sampleList <- list()  #list will contain all coordinates of all samples within this cluster
      num <- 1
      for(r in 1:length(cluster)){
        rowNumber <- cluster[r] #Getting the row number
        getCoordinates <- irisMatrix[rowNumber,] #Getting the sample coordinates
        sampleList[[num]] <- getCoordinates #Appending sample coordinates to list
        num <- num +1
      }
      inputMatrix <- do.call(cbind,sampleList) #Column bind all of the vectors contained in sampleList
      rownames(inputMatrix) <- NULL 
      newCentroid <- rowMeans(inputMatrix,na.rm=TRUE) #Calculate the mean of all samples in current cluster
      updatedCentroids[[index]] <- newCentroid #Add the new cluster centroid to the list of updated centroids
    }
    allCentroids <- updatedCentroids   #Update the centroids
    count <- count +1
  }#End of while loop
  
  #Below is preparation for output so that using $, the desired values/data can be extracted
  #Get size of clusters
  clusterSizes <- vector()
  for(clust in 1:k){
    len <- length(clusterList[[clust]])
    clusterSizes <- c(clusterSizes,len)
  }
  
  #For clusterSamples, it returns the row numbers in the original data that belong to each cluster
  finalResults <- list(clustCenters=allCentroids, sizes=clusterSizes,totalIterations = max, clusterSamples=clusterList)
  return(finalResults)
  
}#End of function



run <- kmeans(irisMatrix,4,15)
run



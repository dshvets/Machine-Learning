rm(list=ls())
data(iris)
irisMatrix <- as.matrix(iris)

#holdout method with stratification ensuring that  each class is represented with approximately
#equal proportions in both the training and the testing datasets.
#typically: 2/3 for training, 1/3 for testing
holdout <- function(matrix,trainPerc,testPerc){
  labels <- unique(matrix[,5]) #get unique labels from data set
  numLabels <- length(labels)
  
  x <-0
  while(x < 1){
        
    numSamples <- nrow(matrix) #integer of the number of samples
    samplesVector <- sample(1:numSamples,numSamples)
    trainingNum <- trainPerc * numSamples  # get the number of samples to be used for training
    training <- sample(1:numSamples,trainingNum) 
    trainMatrix <- matrix[training,] #create testing matrix
    testing <- setdiff(samplesVector,training) #Get samples that are not in the training set, use these for testing
    testMatrix <- matrix[testing,]
    
    testSpeciesCount <- list()
    for(i in 1:numLabels){
      testSpeciesCount[[i]] <- assign(paste("test",i,sep=""),vector())   #ORDER: SETOSA,VERSICOLOR,VIRGINICA     
    }
    trainSpeciesCount <- list()
    for(i in 1:numLabels){
      trainSpeciesCount[[i]] <- assign(paste("train",i,sep=""),vector()) #ORDER: SETOSA,VERSICOLOR,VIRGINICA
    }
    
    testSpecies <- testMatrix[,5]
    trainSpecies <- trainMatrix[,5]
    
    #Following code is all for STRATIFICATION
    
    #Loop through all test species to calculate ratios
    for(i in 1:length(testSpecies)){
      species <- testSpecies[i]
      if(species == "setosa"){
        testSpeciesCount[[1]] <- c(testSpeciesCount[[1]], 1)
      }
      else if(species == "versicolor"){
        testSpeciesCount[[2]] <- c(testSpeciesCount[[2]], 1)
      }
      else if(species == "virginica"){
        testSpeciesCount[[3]] <- c(testSpeciesCount[[3]], 1)
      }
    }
 
    #Loop through all training species to calculate ratios
    for(z in 1:length(trainSpecies)){
      species <- trainSpecies[z]
      if(species == "setosa"){
        trainSpeciesCount[[1]] <- c(trainSpeciesCount[[1]], 1)
      }
      else if(species == "versicolor"){
        trainSpeciesCount[[2]] <- c(trainSpeciesCount[[2]], 1)
      }
      else if(species == "virginica"){
        trainSpeciesCount[[3]] <- c(trainSpeciesCount[[3]], 1)
      }
    }
    
    testRatios <- vector()
    trainRatios <- vector()
    
    for(a in 1:numLabels){
      testRatios <- c(testRatios,(length(testSpeciesCount[[a]])/length(testSpecies)))
    }
    for(b in 1:numLabels){
      trainRatios <- c(trainRatios,(length(trainSpeciesCount[[b]])/length(trainSpecies)))
    }
    
    #Vector will save T if the ratios are close enough, F if they are not close enough
    passFail <- vector()
    
    #Check the ratios as part of stratification, ensure that all ratios are no more than .05 apart
    for(y in 1:numLabels){
      test <- testRatios[y]
      train <- trainRatios[y]
      diff <- abs(test-train)
      if(diff < 0.05){
        passFail <- c(passFail, "T")
      }
      else{
        passFail <- c(passFail,"F")
      }
    }
    
    if(is.element("F",passFail)){
      x <- 0
    }else{
      x <- x+ 1
    }
    
  }#end of while loop
  
  #function returns the training matrix and the testing matrix
  finalResults <- list(trainingSet = trainMatrix, testingSet = testMatrix)
  return(finalResults)
}#end of function



run <- holdout(irisMatrix,.75,.25)
run
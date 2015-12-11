rm(list=ls())
data(iris)
irisMatrix <- as.matrix(iris)

crossVal <- function(matrix,k){
  
  counter <-0
  while(counter < 1){
    
    colnames(matrix) <- NULL 
    allLabels <- matrix[,5]
    labels <- unique(matrix[,5]) #get unique labels from data set
    numLabels <- length(labels)
    numSamples <- nrow(matrix) #integer of the number of samples
    samplesVector <- sample(1:numSamples,numSamples)
    
    #Calculate golden standard ratio of the original data for comparison to the fold ratios
    goldenRatios <- vector()
    for(x in 1:numLabels){
      label <- labels[x]
      num <- length(which(allLabels == label))
      ratio <- num/numSamples
      goldenRatios <- c(goldenRatios,ratio)
    }

    #kfoldList will hold row numbers for each separate "fold" of the data set
    kfoldList <- list()
    for(i in 1:k){
      kfoldList[[i]] <- assign(paste("fold",i,sep=""),vector())   
    }
    
    #first k-fold set must be chosen before loop can start to set the rest of the folds
    perK <- floor(numSamples/k) #round down using floor; number of rows per fold
    
    for(x in 1:k){
      nextFold <- sample(samplesVector,perK)
      kfoldList[[x]] <- c(kfoldList[[x]],nextFold)
      if(x == k){
        break
      }else{
        nextVector <- setdiff(samplesVector,nextFold)
        samplesVector <- nextVector
      }
    }
    
    foldSpecies <- list() #list to store the species of each fold
    for(a in 1:k){
      foldSpecies[[a]] <- vector()  
    }
    
    #loop gets every species type for each fold, to ensure equal representation in each k fold
    for(y in 1:k){ #loop through each fold
      getFold <- kfoldList[[y]]
      for(z in 1:length(getFold)){ #loop through each row of original data represented by each fold
        getIndex <- getFold[z]
        getRowSpecies <- matrix[getIndex,5]
        foldSpecies[[y]] <- c(foldSpecies[[y]],getRowSpecies)
      }
    }
    
    ratioSpecies <- list() #nested list of vectors to store the species ratios of each fold
    for(a in 1:k){
      ratioSpecies[[a]] <- list(vector(),vector(),vector())  
    }
    
    #Loop to calculate all species ratios for each fold
    for(fold in 1:k){
      currentFold <- foldSpecies[[fold]]
      for(x in 1:perK){
        currentFoldSpecies <- currentFold[x]
        for(lab in 1:numLabels){
          currentLabel <- labels[lab]
          if(currentFoldSpecies == currentLabel){
            ratioSpecies[[fold]][[lab]] <- c(ratioSpecies[[fold]][[lab]],currentLabel) 
          }
        }
      }
    }
    
    
    finalRatios <- list() #nested list of vectors to store the species ratios of each fold
    for(a in 1:k){
      finalRatios[[a]] <- list(vector(),vector(),vector())  
    }
    
    #Calculate actual ratios and prepare to compare them so that each fold has equal representation of the species
    for(i in 1:k){
      getFold <- ratioSpecies[[i]]
      for(z in 1:numLabels){
        numSpecies <- length(getFold[[z]])
        speciesRatio <- numSpecies/perK
        finalRatios[[i]][[z]] <- c(finalRatios[[i]][[z]],speciesRatio)
      }
    }
    
    #The ratios are compared to the original data set ratios to ensure that they are efficiently representing the data in each individual fold
    
    checkRatios <- vector()
    
    for(f in 1:k){
      thisFold <- finalRatios[[f]]
      for(i in 1:numLabels){
        thisRatio <- unlist(thisFold[i])
        rightRatio <- goldenRatios[i]
        diff <- abs(thisRatio-rightRatio)
        if(diff < 0.1){
          checkRatios <- c(checkRatios,"T")
        }else{
          checkRatios <- c(checkRatios,"F")
        }
      }
    }
    
    if(is.element("F",checkRatios)){
      counter <- 0
    }else{
      counter <- counter+ 1
    }
    
  }#end while loop
  
  #Final results will return k number of vectors, the vectors contain the row numbers from the
  #original data matrix which can be used for subsequent classification
  finalResults <- list(kSampleSets = kfoldList )
  return(finalResults)
  

}#end function



run <- crossVal(irisMatrix,4)



## Machine-Learning
Here are my implementations of several important machine learning methods in R. Each R script uses the *Iris* dataset provided in R, and contains a function that can be run on any dataset given that it has the appropraite format.   


Method | Explanation
------------- | -------------
K-means | Clustering *n* data points into *k* distinct clusters. User inputs the number of iterations until they want the optimization to end.  
Silhouette | A method for validation of consistency within clusters of data. 
Cross-Validation | Cross validation is the process of model evaluation. In this case I implemented the k-fold cross validation method. 
Holdout | A simple form of cross validation where the data is separated into a testing and training data set. A model is fit using the training set, then tested with the testing data set, then the errors that are made are accumulated to give the mean absolute test set error, which is used to evaluated the model. 

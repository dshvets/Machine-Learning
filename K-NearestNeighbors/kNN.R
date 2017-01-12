
#DATA PREP
in_file_name <- "/Users/dahlia-shvets/Desktop/Fall.2015/Mach.Learning/case_study_output.csv"
dataIn <- read.csv(file=in_file_name,
                   header=T,
                   sep=",")
# rows are peaks, columns are values associated with each sample
all_col_names <- colnames(dataIn)
# remove dots in column names with a white space
all_col_names <- gsub(pattern="\\.", replacement=" ", x=all_col_names, perl=T)
colnames(dataIn) <- all_col_names
variable_names <- dataIn[, 1]
# extract the peak area columns
tf <- grepl(pattern="NSCLC_", x=all_col_names) & grepl(pattern="area", x=all_col_names)
II <- which(tf==T)
tf <- grepl(pattern="^SCLC_", x=all_col_names) & grepl(pattern="area", x=all_col_names)
JJ <- which(tf==T)
# get the peak area data
data <- cbind(dataIn[, II], dataIn[, JJ])
sample_names <- colnames(data)
crop_name <- function(s) {
  ind <- regexpr(pattern="_POS", text=s)
  return(substr(x=s, start=1, stop=ind-1))
}
sample_names_cropped <- sapply(sample_names, crop_name)
colnames(data) <- sample_names_cropped
# 2. Filter variables
tf <- grepl(pattern="row number of detected peaks", x=all_col_names)
II <- which(tf==T)
JJ <- which(dataIn[, II] == 40) # select variables that are detected in all of the samples
data_for_analysis <- data[JJ, ]
data_for_analysis <- as.data.frame(t(data_for_analysis))
colnames(data_for_analysis) <- variable_names[JJ]


#Actual Code

#requires original data matrix and k as input
knn <- function(matrix,k){
  
  
}



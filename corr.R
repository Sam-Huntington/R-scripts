corr <- function(directory, threshold = 0){

	output <- numeric()
	names <- dir(directory, pattern = ".csv")

	## loop through directory
	for (i in 1:length(names)){
		file = paste(directory,"/",names[i], sep = "")
		temp_table <- read.csv(file, header = TRUE)
		temp_complete <- complete.cases(temp_table)
		temp_t <- temp_table[temp_complete,]	

		## if number of complete cases exceeds threshold, calc correlation and add to output
		if(nrow(temp_t)>threshold){
			cor <- cor(temp_t[,2],temp_t[,3])
			output <- c(output, cor)
		}
	}
	output
}
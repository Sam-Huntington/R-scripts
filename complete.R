complete <- function(directory, id = 1:332) {
	  
	x <- integer(max(id))
	y <- integer(max(id))	
	j = 1

	for (i in id){	
		## add zeros to name for picking out of files
		if (i < 10) { 	
			id_num = paste("00",i,sep = "")
		} else if (i < 100) {
			id_num = paste("0",i,sep = "")
		} else {
			id_num = paste(i,sep = "")
		}

		## locate file and store complete cases in table
		filepath = 	paste(directory, "/", id_num, ".csv", sep = "")
		temp_table <- read.csv(filepath, header=TRUE)
		temp_complete <- complete.cases(temp_table)

		## if table has >0 complete cases, populate vectors
		if (nrow(temp_table[temp_complete,])>0){
			x[j] = i
			y[j] = nrow(temp_table[temp_complete,])
			j = j+1
		}	
	}
	
	## replace zeros with NAs, then eliminate NAs with complete.cases function 
	x[x==0] <- NA
	y[y==0] <- NA
	outputs <- data.frame(id=x,nobs=y)
	outputs <- outputs[complete.cases(outputs),]
	outputs
}
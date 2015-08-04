pollutantmean <- function(directory, pollutant, id = 1:332) {

	## define initial data frame which will hold data from all csv files
	combined_data <- data.frame(Date= factor(),sulfate=numeric(),nitrate=numeric(),ID=integer())

	## loop through files and build new data frame
	for (i in id){	
		## add zeros to name for picking out of files
		if (i < 10) { 	
			id_num = paste("00",i,sep = "")
		} else if (i < 100) {
			id_num = paste("0",i,sep = "")
		} else {
			id_num = paste(i,sep = "")
		}

		filepath = 	paste(directory, "/", id_num, ".csv", sep = "")
		temp_table <- read.csv(filepath, header=TRUE)
		combined_data <- rbind(combined_data, temp_table)
	}


	## take the mean of new data frame
	if (pollutant == "sulfate") {
		mean(combined_data[,2],na.rm = TRUE)
	} else if (pollutant == "nitrate"){
		mean(combined_data[,3], na.rm = TRUE)
	} else {
		print("Function failed to exectute - please check the arguments")
	}
	
}
pollutantmean <- function(directory, pollutant, id = 1:332) {
        library(stringr)
        data <- data.frame()
        
        for (i in id) {
                filename <- str_pad(as.character(i), width=3, pad="0")
                data <- rbind(data, read.csv(file.path(directory, paste(filename, "csv", sep = "."))))
        }
        mean(data[[pollutant]], na.rm = TRUE)
}

corr <- function(directory, threshold = 0) {
        library(stringr)

        obs <- c()
        
        for (i in 1:332) {
                file.name <- str_pad(as.character(i), width=3, pad="0")
                file.cont <- read.csv(file.path(directory, paste(file.name, "csv", sep = ".")))
                if (nrow(file.cont[!is.na(file.cont$sulfate) & !is.na(file.cont$nitrate),]) >= threshold) {
                        obs <- c(obs, cor(file.cont$sulfate, file.cont$nitrate, use = "na.or.complete"))
                }
        }
        as.vector(obs)
}

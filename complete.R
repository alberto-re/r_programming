complete <- function(directory, id = 1:332) {
        library(stringr)

        obs <- c()
        
        for (i in id) {
                file.name <- str_pad(as.character(i), width=3, pad="0")
                file.cont <- read.csv(file.path(directory, paste(file.name, "csv", sep = ".")))
                obs <- c(obs, c(i, nrow(file.cont[!is.na(file.cont$sulfate) & !is.na(file.cont$nitrate),])))
        }
        obs <- matrix(obs, ncol = 2, byrow = TRUE)
        obs <- as.data.frame(obs)
        names(obs) <- c("id", "nobs")
        obs
}

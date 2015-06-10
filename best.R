best <- function(state, outcome) {
    ## Read outcome data
    outcome.data <- read.csv("outcome-of-care-measures.csv")
    outcome.data[, 11] <- as.double(as.character(outcome.data[, 11]))
    outcome.data[, 17] <- as.double(as.character(outcome.data[, 17]))
    outcome.data[, 23] <- as.double(as.character(outcome.data[, 23]))

    ## Check that state and outcome are valid
    valid.outcome <- c("heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% valid.outcome)) {
        stop("invalid outcome")
    }
    if (!(state %in% levels(outcome.data$State))) {
        stop("invalid state")
    }

    ## Return hospital name in that state with lowest 30-day death rate

    # Filter by state
    outcome.data.state <- outcome.data[outcome.data$State == state,]

    # Filter by income
    if (outcome == "heart attack") {
        min.rate <- min(outcome.data.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm=TRUE)
        best <- outcome.data.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min.rate
    }
    if (outcome == "heart failure") {
        min.rate <- min(outcome.data.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm=TRUE)
        best <- outcome.data.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min.rate
    }
    if (outcome == "pneumonia") {
        min.rate <- min(outcome.data.state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.rm=TRUE)
        best <- outcome.data.state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min.rate
    }

    # Return the best one
    sort(as.vector(outcome.data.state[best,]$Hospital.Name))[1]
}

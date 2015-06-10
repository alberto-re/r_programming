rankhospital <- function(state, outcome, num = "best") {

    ## Read outcome data
    outcome.data <- read.csv("outcome-of-care-measures.csv")

    # Convert factors to double
    outcome.data[, outcome.cols["heart attack"]] <- as.double(as.character(outcome.data[, outcome.cols["heart attack"]]))
    outcome.data[, outcome.cols["heart failure"]] <- as.double(as.character(outcome.data[, outcome.cols["heart failure"]]))
    outcome.data[, outcome.cols["pneumonia"]] <- as.double(as.character(outcome.data[, outcome.cols["pneumonia"]]))

    outcome.cols <- c(11, 17, 23)
    names(outcome.cols) <- c("heart attack", "heart failure", "pneumonia")

    ## Check that state and outcome are valid
    if (!(outcome %in% names(outcome.cols))) {
        stop("invalid outcome")
    }
    if (!(state %in% levels(outcome.data$State))) {
        stop("invalid state")
    }

    # Filter by state
    outcome.data.state <- outcome.data[outcome.data$State == state,]

    # Leave out NAs
    outcome.data.state <- outcome.data.state[!is.na(outcome.data.state[outcome.cols[outcome]]),]

    # Check that num, if numeric, is between one and the number
    # of hospitals in the desired state
    if (is.numeric(num) && num > nrow(outcome.data.state)) {
        return(c("NA"))
    }

    # Order ascending by outcome, then by hospital name alphabetically
    outcome.data.state <- outcome.data.state[order(outcome.data.state[, outcome.cols[outcome]], outcome.data.state$Hospital.Name), ]

    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (num == "best") { num = 1 }
    if (num == "worst") { num = nrow(outcome.data.state) }
    return(as.vector(outcome.data.state[num, ]$Hospital.Name))
}

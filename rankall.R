rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome.data <- read.csv("outcome-of-care-measures.csv")

    outcome.cols <- c(11, 17, 23)
    names(outcome.cols) <- c("heart attack", "heart failure", "pneumonia")

    # Convert factors to double
    outcome.data[, outcome.cols["heart attack"]] <- as.double(as.character(outcome.data[, outcome.cols["heart attack"]]))
    outcome.data[, outcome.cols["heart failure"]] <- as.double(as.character(outcome.data[, outcome.cols["heart failure"]]))
    outcome.data[, outcome.cols["pneumonia"]] <- as.double(as.character(outcome.data[, outcome.cols["pneumonia"]]))

    # Convert hospital's name into strings
    outcome.data$Hospital.Name <- as.character(outcome.data$Hospital.Name)

    ## Check that the outcome is valid
    if (!(outcome %in% names(outcome.cols))) {
        stop("invalid outcome")
    }

    # Prepare the data frame for storing results
    hospitals <- character(0)
    states <- sort(as.character(levels(outcome.data$State)))

    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    for (state in states) {
        hospitals.in.state <- outcome.data[outcome.data$State == state,]

        # Leave out NAs
        hospitals.in.state <- hospitals.in.state[!is.na(hospitals.in.state[outcome.cols[outcome]]),]

        # Check that num, if numeric, is between one and the number
        # of hospitals in the desired state
        if (is.numeric(num) && num > nrow(hospitals.in.state)) {
            hospitals <- c(hospitals, "NA")
            next
        }
        if (num == "best") { row_index = 1 }
        if (num == "worst") { row_index = nrow(hospitals.in.state) }
        hospitals.in.state <- hospitals.in.state[order(hospitals.in.state[, outcome.cols[outcome]], hospitals.in.state$Hospital.Name), ]
        hospitals <- c(hospitals, hospitals.in.state[row_index, ]$Hospital.Name)
    }
    return(data.frame(hospital=hospitals, state=states))
}

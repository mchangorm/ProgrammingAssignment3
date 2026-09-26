rankall <- function(outcome, num = "best") {
    ## Read CSV file
    datarates <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## change the outcome to lowercase
    outcome <- tolower(outcome)

    ## Get all states defined in CSV file
    validstates <- unique(datarates[["State"]])

    ## Check that the outcomes conform to one of
    ## “heart attack”, “heart failure”, or “pneumonia”.
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("Invalid outcome")
    }

    colnum <- 0

    if (outcome == "heart attack") {
        colnum <- 11
    } else if (outcome == "heart failure") {
        colnum <- 17
    } else if (outcome == "pneumonia") {
        colnum <- 23
    }

    ## Grab all the outcome data
    ## and subet to only relevant columns
    outcome_data1 <- datarates[, c(2, 7, colnum)]
    colnames(outcome_data1) <- c("hospital", "state", "rate")
    outcome_data1$rate <- as.numeric(outcome_data1$rate)

    outcome_data1 <- outcome_data1[!is.na(outcome_data1$rate), ]

    # Split the data frame into different lists of data frame, 1 for each state
    state_list <- split(outcome_data1, outcome_data1$state)

    # Loop over each data frame. Sort and extract rank
    results_list <- lapply(state_list, function(state_df) {
        ordered_results <- state_df[order(state_df$rate, state_df$hospital), ]

        if (num == "best") {
            rank <- 1
        } else if (num == "worst") {
            rank <- nrow(outcome_data1)
        } else if (num > nrow(outcome_data1)) {
            return(NA)
        } else {
            rank <- as.numeric(num)
        }
        return(ordered_results[rank, "hospital"])
    })

    results_list

    output <- data.frame(
        hospital <- unlist(results_list),
        state <- names(results_list),
        stringsAsFactors = FALSE
    )

#    return(output)
}

rankall <- function(outcome, num = "best") {
    ## 1. Read outcome data and check validity
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Map out the required column indices
    col_idx <- switch(outcome, 
                      "heart attack"  = 11, 
                      "heart failure" = 17, 
                      "pneumonia"     = 23, 
                      stop("invalid outcome"))

    ## 2. Subset to relevant columns and coerce rate to numeric
    # Keep Hospital Name (2), State (7), and the outcome column
    df <- data[, c(2, 7, col_idx)]
    colnames(df) <- c("hospital", "state", "rate")
    df$rate <- as.numeric(df$rate) # Suppresses warnings about NAs introduced
    
    ## 3. Remove rows with missing outcome data
    df <- df[!is.na(df$rate), ]

    ## 4. Split the data frame by state
    # This creates a named list of data frames, one for each state
    state_list <- split(df, df$state) 

    ## 5. Loop over each state to sort and extract the requested rank
    results_list <- lapply(state_list, function(state_df) {
        
        # Tie-breaker logic: order primarily by rate (ascending), secondarily by hospital name (alphabetical)
        ordered_df <- state_df[order(state_df$rate, state_df$hospital), ]
        
        # Parse the 'num' ranking input
        rank_idx <- if (num == "best") {
            1
        } else if (num == "worst") {
            nrow(ordered_df) # The last row is the worst
        } else {
            as.numeric(num)
        }
        
        # Return the target hospital name (returns NA if rank_idx > total hospitals in that state)
        return(ordered_df[rank_idx, "hospital"])
    })

    ## 6. Format the output to match Assignment requirements
    # Convert the resulting list back into a 2-column data frame
    output <- data.frame(
        hospital = unlist(results_list),
        state = names(results_list),
        stringsAsFactors = FALSE
    )
    
    return(output)
}

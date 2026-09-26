library(dplyr)

rankall <- function(outcome, num = "best")
{
    ## Read CSV file
    datahosp <- read.csv("outcome-of-care-measures.csv", colClasses="character")

    ## change the outcome to lowercase
    outcome <- tolower(outcome)

    ## Get all states defined in CSV file
    validstates <- unique(datahosp[["State"]])

    ## Check that the outcomes conform to one of 
   ## “heart attack”, “heart failure”, or “pneumonia”.
    if (!outcome %in% c("heart attack", "heart failure","pneumonia"))
    {
        stop("Invalid outcome")
    }

    ## We need to grab all columns with the name 30 day mortality
    ## and return the index of the column
    columnindex <- c(grep("^Hospital.*Death*", names(datahosp)))
    #print(columnindex) #11 17 23

    if (outcome == "heart attack")
    {
        colnum <- 11
    }
    else if (outcome == "heart failure")
    {
        colnum <- 17
    }
    else if (outcome == "pneumonia")
    {
        colnum <- 23
    }

    ## Grab all the outcome data
    outcome_data1 <- subset(datahosp, datahosp[,colnum] != "Not Available")

    if (num == "best")
    {
        rank <- 1
    }
    else if (num == "worst")
    {
        rank <- length(outcome_data1[,colnum])
    }
    else if (num > length(outcome_data1[,colnum]))
    {
        return(NA)
    }
    else
    {
        rank <- num
    }

    # Split the data frame into different lists of data frame, 1 for each state
    state_list <- split(outcome_data1,outcome_data1$State)

    # Loop over each data frame. Sort and extract rank
    results_list <- lapply(state_list, function(state_df)
                     {
                        ordered_results <- state_df[order(state_df[,colnum],state_df$State),]
                        return(ordered_results[rank,"hospital"])
                     }

    )

    results_list
    output <- data.frame(
        hospital <- unlist(results_list),
        state <- names(results_list),
        stringsAsFactors = FALSE
    )

    return(output)
}

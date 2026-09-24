#library(data.table)

best <- function(state, outcome)
{
    ## Read CSV file
    datahosp <- read.csv("outcome-of-care-measures.csv")

    ## change the outcome to lowercase
    outcome <- tolower(outcome)

    ## Get all states defined in CSV file
    validstates <- unique(datahosp[["State"]])

    ## Check if state entered is valid 
    if (!state %in% validstates)
    {
        stop("Invalid US state entered")
    }

   ## Check that the outcomes conform to one of 
   ## “heart attack”, “heart failure”, or “pneumonia”.
    if (!outcome %in% c("heart attack", "heart failure","pneumonia"))
    {
        stop("Invalid outcome")
    }

    # Initialise those not available rates to 0
    datahosp[datahosp == "Not Available"] <- 0

    

}

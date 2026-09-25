library(dplyr)

rankhospital <- function(state, outcome, num = "best")
{
    ## Read CSV file
    datahosp <- read.csv("outcome-of-care-measures.csv", colClasses="character")

    ## change the outcome to lowercase
    outcome <- tolower(outcome)

    ## Get all states defined in CSV file
    validstates <- unique(datahosp[["State"]])

    ## Check if state entered is valid 
    if (!state %in% validstates)
    {
        stop("Invalid state entered")
    }

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
    outcome_data1 <- subset(datahosp, State == state & datahosp[,colnum] != "Not Available")


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


    ## Sort the data in terms of ascending order of the selected mortality column
    ## This is done in ascending order
    mortalityrate_ranked <- sort(as.numeric(outcome_data1[,colnum]))
    #hosp_mortalityrate_ranked <- subset(outcome_data1,as.numeric(outcome_data1[,colnum]) == mortalityrate_ranked)]
    hosp_mortalityrate_ranked <- subset(outcome_data1,
                                    as.numeric(outcome_data1[,colnum]) == mortalityrate_ranked[rank]
    )
    # Return element in 1st row and 2nd column of data frame (which is the hospital name)
    return(hosp_mortalityrate_ranked[1,2])
}
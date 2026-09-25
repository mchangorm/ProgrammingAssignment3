#require(plyr)
library(dplyr)

best <- function(state, outcome)
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

    
    outcome_data1 <- subset(datahosp, State == state & datahosp[,colnum] != "Not Available")
    #print(outcome_data1)

    minimum_mortalityrate <- min(as.numeric(outcome_data1[,colnum]))
    hosp_mini_mortalityrate <- subset(outcome_data1,as.numeric(outcome_data1[,colnum]) == minimum_mortalityrate)
    return(hosp_mini_mortalityrate[,2])

    #mortality_rates <- datahosp[,c(2,7,columnindex)]
    #names(mortality_rates)[3:5] <- c("heart attack", "heart failure", "pneumonia")
    ## Force the mortality rates numeric
    #mortality_rates[,3:5] <- sapply(mortality_rates[,3:5],as.numeric)
    ## Need to transform NA values to zero
    #mortality_rates[mortality_rates == 0] <- NA
    ## Let's start the selection process!

}

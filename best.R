require(plyr)

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

    ## We need to grab all columns with the name 30 day mortality
    ## and return the index of the column
    columnindex <- c(grep("^Hospital.*Death*", names(datahosp)))
    #print(columnindex)

    mortality_rates <- datahosp[,c(2,7,columnindex)]
    #head(mortality_rates)

    ## Force the mortality rates numeric
    mortality_rates[,3:5] <- sapply(mortality_rates[,3:5],as.numeric)

    ## Need to transform NA values to zero
    ##mortality_rates[mortality_rates == 0] <- NA

    ## Let's start the selection process!
    selected_state <- mortality_rates[mortality_rates$State == state,]
    head(selected_state)

    ## Once selected, let's order
    ## Use the arrange function in dlypr package to sort
    ## by the outcome. Also, push na values last
    selection_ordered <- arrange(selected_state, selected_state[,outcome],Hospital.Name)
    head(selection_ordered)


}

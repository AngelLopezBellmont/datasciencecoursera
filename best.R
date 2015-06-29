
getHospitalName <- function(mydata, col_num, state) {
            
            # this 2 lines make the same: mydata[mydata[, 7]==state, ]  equal mydata[mydata$State == state, ]
            #state_subset <- mydata[mydata$State == state, ]
            state_subset <- mydata[mydata[, 7]==state, ] 
            
            #we get the exact colum depens of the disease == col_num
            colum_disease <- state_subset[, col_num]
            
            # get the min value removing the NA with na.rm=T   T is TRUE
            min_value <- min(colum_disease, na.rm=T) 
            
            # get row= index  where the colum outcome_arr hat the value min
            get_row <- which(colum_disease == min_value)
            
            #get from state_subset the row min_index and colum 2 that is the name of the hospital
            hosp_name <- state_subset[get_row, 2]
            #hosp_name <- state_subset[get_row, "Hospital.Name"]   # Name column 2 == Hospital.Name
            return(hosp_name)
}

best <- function(state, outcome) {
            ## Read outcome data
            ## Check that state and outcome are valid
            ## Return hospital name in that state with lowest 30-day death
            ## rate
            
            # read the data file
            directory <- "outcome-of-care-measures.csv"
            mydata <- read.csv(directory, colClasses="character")
            # change mydata type from character to numeric
            mydata[, 11] <- as.numeric(mydata[, 11]) # heart attack
            mydata[, 17] <- as.numeric(mydata[, 17]) # heart failure
            mydata[, 23] <- as.numeric(mydata[, 23]) # pneumonia
            valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
            
            if (!state %in% mydata$State) 
            {
                        stop("Sorry,invalid state name abreviation. Try again")
            } 
            else if(!outcome %in% valid_outcomes) {
                        
                        stop("Sorry,invalid outcome. Invalid deseis Try again")
            } 
            
            
            if(outcome == "heart attack")        { myColumn = 11 }
            else if (outcome == "heart failure") { myColumn = 17 } 
            else  (outcome =="pneumonia")        { myColumn = 23 }
            
            hosp_name <- getHospitalName(mydata, myColumn, state)
            
            result <- hosp_name
            return(result)
            
}

# tests
#best("TX", "heart attack")
#best("TX", "heart failure")
#best("MD", "heart attack")
#best("MD", "pneumonia")
#best("BB", "heart attack")
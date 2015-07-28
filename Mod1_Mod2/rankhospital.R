
# 2015.06.26 Angel Lopez Bellmont
#rankhospital("MD", "heart failure", 5)
rankhospital  <- function (state, outcome, num_ranking)
{
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
            if(!outcome %in% valid_outcomes) {
                        
                        stop("Sorry,invalid outcome. Invalid deseis Try again")
            } 
            
            if(outcome == "heart attack")        { myColumn = 11 }
            else if (outcome == "heart failure") { myColumn = 17 } 
            else if (outcome =="pneumonia")      { myColumn = 23 }
            
            
            hosp_name <- getHospitalNameRanking(mydata, myColumn, state, num_ranking)
            
            result <- hosp_name
            return(result)
}


getHospitalNameRanking <- function (mydata, myColumn, state, num_ranking)
{
            stateSubset <- mydata[mydata[, 7]==state,] 
            
            #we take NA from column myColumn
            stateSubset <- subset( stateSubset, !is.na(stateSubset[,myColumn]) )
            
            #we sort here the subset state for myColumn y despues por nombre
            stateSubset_sort <- stateSubset[ order(stateSubset[,myColumn], stateSubset[,2]), ]
                        
                        
                        
                        
            #colum_disease <- state_subset[, myColumn]
            
            #myvec[!is.na(myvec)]
            #colum_disease[!is.na(colum_disease)]
            
            #colum_disease_sort <- sort (colum_disease)
            
            typeVariable = typeof(num_ranking) # just checking the type of variable
            
            if(typeof(num_ranking) == "character"){
                     
                        if(num_ranking == "best"){ hosp_name <- stateSubset_sort[1, 2] }
                        if(num_ranking == "worst"){hosp_name <- stateSubset_sort[nrow(stateSubset_sort), 2] }   
            }
            
            if(typeof(num_ranking) == "double"){
                        hosp_name <- stateSubset_sort[num_ranking, 2]
                        
                        if( num_ranking >= nrow(stateSubset_sort))
                        {
                               return (NA)     
                        }
            }
            
            return(hosp_name)
                        
}


#rankhospital("MN", "heart attack", 5000)
#rankhospital("TX", "heart failure", 4)
#rankhospital("MD", "heart attack", "worst")

#rankhospital("CA", "heart failure", 10)  
#HUNTINGTON BEACH HOSPITAL






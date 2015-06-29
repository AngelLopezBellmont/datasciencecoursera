
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
            state_subset <- mydata[mydata[, 7]==state,] 
            colum_disease <- state_subset[, myColumn]
            
            #myvec[!is.na(myvec)]
            colum_disease[!is.na(colum_disease)]
            
            colum_disease_sort <- sort (colum_disease)
            
            typeVariable = typeof(num_ranking) # just checking the type of variable
            
            if(typeof(num_ranking) == "character"){
                     
                        if(num_ranking == "best"){ranking_value <- min(colum_disease, na.rm=T) }
                        if(num_ranking == "worst"){ranking_value <- max(colum_disease, na.rm=T) }   
            }
            
            if(typeof(num_ranking) == "double"){
                        ranking_value <- colum_disease_sort[(num_ranking)]
                        
                        if( num_ranking >=length(colum_disease))
                        {
                               return (NA)     
                        }
            }
            
            
            
            get_row <- which(colum_disease == ranking_value)
            hosp_name <- state_subset[get_row, 2]
            
            # we have to inverse sort to get the num_ranking
            hosp_name <- sort(hosp_name, decreasing = TRUE)
            hosp_name <- hosp_name[1]
            return(hosp_name)
                        
}


#rankhospital("MN", "heart attack", 5000)
#rankhospital("TX", "heart failure", 4)
#rankhospital("MD", "heart attack", "worst")






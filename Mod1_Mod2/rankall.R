
rankall  <- function (outcome, num_ranking)
{
             directory <- "outcome-of-care-measures.csv"
             mydata <- read.csv(directory, colClasses="character")     
             
             # change mydata type from character to numeric
             mydata[, 11] <- as.numeric(mydata[, 11]) # heart attack
             mydata[, 17] <- as.numeric(mydata[, 17]) # heart failure
             mydata[, 23] <- as.numeric(mydata[, 23]) # pneumonia
             valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
             
             if(!outcome %in% valid_outcomes) {
                         
                         stop("Sorry,invalid outcome. Invalid deseis Try again")
             } 
             
             if(outcome == "heart attack")        { myColumn = 11 }
             else if (outcome == "heart failure") { myColumn = 17 } 
             else if (outcome =="pneumonia")        { myColumn = 23 }
             
             
             hosp.names.state.Frame <- getAllHospitalNameRanking(mydata, myColumn, num_ranking)
             
             myresult <- hosp.names.state.Frame
            return (myresult)
}



#if we can get the one list with the name os the states, mylist <- list(all the states)
# then we can use lapply(mylist, getHospitalNameRanking). getHospitalNameRanking from the exercices before.
# but i dont see now how we can send the parameter state into getHospitalNameRanking
# upss I just read in the .pdf that we should not use rankhospital, from the previous exercises.
#But ok I am not going to use rankhospital but getHospitalNameRanking another function that I created.

 getAllHospitalNameRanking <- function (mydata, myColumn, num_ranking)
 {
             VectorWithAllStateName <-  getVectorWithAllStateName(mydata)
             #name.Hospital <-  getHospitalNameRanking (mydata, myColumn, VectorWithAllStateName[1], num_ranking)
             
             #mydataframe <- data.frame("hospital"=character(), "state"=character())
             #mydataframe <- data.frame(character(), character())
             mydataframe <- data.frame("hospital", "state")
             
             c1 <- "hallo999"
             v1 <- vector(mode="character")
             v2 <- vector(mode="character")
             
             for (i in 1:length(VectorWithAllStateName)){
             #for (i in 1:4){
                     #c1 <- c(c1, VectorWithAllStateName[i])
                     name.Hospital <- getHospitalNameRanking (mydata, myColumn, VectorWithAllStateName[i], num_ranking)
                    
                     #newrow = c(name.Hospital, VectorWithAllStateName[i])
                     newrow = c(VectorWithAllStateName[i], VectorWithAllStateName[i])
                     
                     v1 <- c( v1, name.Hospital)
                     v2 <- c( v2, VectorWithAllStateName[i])
                    
                     #mydataframe = rbind(mydataframe,newrow)
               
             }
             
             #mat1 <- matrix(c(v1,v2),nrow=length(v1))
             #as.data.frame(mat1)
             df1 <- data.frame(v1,v2)
             #c("hospital", "state")))
             df1 <- setNames(df1, c("hospital", "state"))
            
             #return(newrow)
             return(df1)
             
 }
 
 #mydata <- read.csv(directory, colClasses="character")     
 getVectorWithAllStateName <- function (mydata)
 {
             # we get the vector with all the state name
             c7 <- mydata[ ,7]
             stateNameVect <-  c7[1]
             
             for(i in 2:length(c7))
             {
                   if( !c7[i] %in% stateNameVect)
                   {
                         stateNameVect <- c ( stateNameVect , c7[i])
                   }
             }
             
             stateNameVect <- sort(stateNameVect, decreasing = FALSE)
             return(stateNameVect)
 }
 
 getHospitalNameRanking <- function (mydata, myColumn, state, num_ranking)
 {
             stateSubset <- mydata[mydata[, 7]==state,] 
             
             #we take NA from column myColumn
             stateSubset <- subset( stateSubset, !is.na(stateSubset[,myColumn]) )
             
             #we sort here the subset state for myColumn y despues por nombre
             stateSubset_sort <- stateSubset[ order(stateSubset[,myColumn], stateSubset[,2]), ]
             
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


 #mistake in 10 ??? 
 #rankall("heart failure", 10)
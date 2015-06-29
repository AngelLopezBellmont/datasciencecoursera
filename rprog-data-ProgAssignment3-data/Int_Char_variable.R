#outcomeCM<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
#outcomeCM<-read.csv("outcome-of-care-measures.csv") 

#c1 <-c(1:10)

#a  <- c1[5]

f1 <- function(d)
{
            
            t =  typeof(d)
          
            if(typeof(d) == "double")
            {
                        d = d+1
            }
          
            if(typeof(d) != "double")
            {
                      d = " new value"
            }
            
            if(typeof(d) == "character")
            {
                        d = "is character"
            }
            
            return (d)
}


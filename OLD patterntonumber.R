#create function "patttonumb" with input "pattern"
#output: number of the pattern in lexicographic order
 patttonumb <- function(pattern){
    
    #define k as the length of the "pattern"
     k <- nchar(pattern)
     #create empty vector patternx
     patternx <- {}
     
     #seperate the pattern into the single letters using "strplit"
     #input "pattern" and length = "" = null
     #since strsplit puts new values into a list we use "unlist" to get rid of it
     #ref: https://stat.ethz.ch/R-manual/R-devel/library/base/html/strsplit.html
     patternx <- unlist(strsplit(pattern, ""))

     #replace values of characters "A","C","G","T" to corresponding numeric values "0","1","2","3"
     patternx <- replace(patternx,patternx=="A",0)
     patternx <- replace(patternx,patternx=="C",1)
     patternx <- replace(patternx,patternx=="G",2)
     patternx <- replace(patternx,patternx=="T",3)
       
     #since those are still stored as characters, change them to numeric using as.numeric
         patternx <- as.numeric(patternx)
       
         #multiply numbers according to their weight in base 4
         #weight is base^(length-pos)
         #start at position x=1, end at length of text "k"
         #pos "x" * base "4" ^ (length "k" - pos "x")
         #rewrite value into that position x of patternx
             for (i in x<-1:k){
                patternx[x] <- patternx[x]*(4^(k-x))
                #add up the single digits to the sum
                #this sum is now the value in base 10
                #output sum of patternx
                return (sum(patternx))
            }
         }

   
 #first dataset 
 patttonumb("AGT")
 
 #second dataset
 patttonumb("CTTCTCACGTACAACAAAATC")
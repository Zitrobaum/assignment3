#define function fastfreq words
#in: "text" dna string | "k" length of pattern
#out: most frequent combination of bases with length k in string "text", with number of appearance
#dependencies: functions: compfreq, patt2num, sym2num, num2patt, num2sym
fastfreqwords <- function(text,k){
  #initialize "freqpatt" with empty values
  freqpatt <- {}
  #using function compfreq, define "freqarray" with the same inputs as in "fastfreqwords"
  freqarray <- compfreq(text,k)
  #output: array with the counted amount of patterns in order with length 4^k
  #using function max, find the maximal count value in the array. ref: https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/Extremes
  #define that as variable "maxcount"
   maxcount <- max(freqarray)
  #give out maxcount, this is the maximum appearance of the most frequent patterns (optional)
  print(maxcount)
  
  #for every position in the freqarray from start= i to stop= max length 4^k
  for (i in 1:4^k){
    #check if the value of that position is equivalent to the maximum value maxcount (condition)
    if (freqarray[i] == maxcount){
      #if the condition is met, write this number into "pattern" using function "num2patt"
      #input number is i-1 because the loop starts at 1 instead of 0, length of pattern k is given in "fastfreqwords"
    pattern <- num2patt(i-1,k)
    #output: bases of pattern
    #write the pattern into a new position i in "freqpatt"
    freqpatt [i] <- pattern
    }
  }
  #since this introduces NA's,  overwrite freqpatt with only those values of freqpatt, that are not NA's
  #using is.na ref:https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/NA
  freqpatt <- freqpatt[!is.na(freqpatt)]
  #output: all the most frequent patterns and the number of appearance inside the "text" with length "k"
  return(freqpatt)
}

#example dataset
fastfreqwords("AAGCAAAGGTGGG",2)
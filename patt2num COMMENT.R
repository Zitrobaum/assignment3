#defining function "sym2num"
#input character "symbol" | output number from 0 to 3
sym2num <- function(symbol){
  #return  corresponding numeric values if the input letter is A,C,G or T
  # A -> 0  
  if(symbol=="A")
    return(0)
  #C -> 1
  if(symbol=="C")
    return(1)
  #G -> 2
  if(symbol=="G")
    return(2)
  #T -> 3
  if(symbol=="T")
    return(3)
  #no output if the symbol is none of the above
}

#define function "patt2num"
#in: text "pattern" of any length
#out: number of that pattern in lexicographic order of all patterns with that length
#dependance: function "sym2num"
patt2num <- function(pattern){
  #if input pattern is empty return value 0
  if (pattern=="")
    return(0)
  #define k as the length of the pattern using nchar ref: https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/nchar
  k <- nchar(pattern)
  #define symbol as the last character of the pattern using substr. ref: https://stat.ethz.ch/R-manual/R-devel/library/base/html/substr.html
  #since k = length, start=k and stop=k give the last character
  symbol <- substr(pattern,k,k)
  #define prefix as every character except for the last one (symbol)
  #start at 1 ,stop one char before the end k-1
  prefix <- substr(pattern,1,k-1)

  #multiply prefix by 4 and put it back through the function
  #add number corresponding to "symbol" using function "sym2num"
  #each extra character in "pattern" gets multiplied by 4 again now
  #output sum of all the "symbol"s (but with increasing powers of 4)
  #repeat until "pattern" is empty using if condition above
  return(4*patt2num(prefix)+sym2num(symbol))
  #output: value of the pattern in lexicographic order
}

#first dataset
patt2num("AGT")
#second dataset
patt2num("CTTCTCACGTACAACAAAATC")

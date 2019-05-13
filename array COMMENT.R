#define function "compfreq" with inputs "text" - total dna sequence to be searched
#"k" - length of patterns 
#output frequency of patterns in lexicographic order.
#dependencies: functions "patt2num" and "sym2num"
compfreq <- function(text,k){
  #initialize an empty array "freqarray"
  freqarray <- {}
  #fill this array with 0's for the total amount of possible pattern combinations 4^k
  #start at 1 since arrays cant have any value in the 0 position
  for (i in 1:4^k)
    freqarray[i] <- 0
  
  #for all i from 0 to the length the of text minus the length of the pattern do the following..
for (i in 0:(nchar(text)-k)){
  #define "pattern" as the characters of the text at the current position i with the length k using substr. ref:https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/substr
  #start i+1 because again we start at 0 but in "text" there is no position 0
  pattern <- substr(text,i+1,i+k)
  #that pattern now gets fed into the function "patt2num" to return the lexicographic ordered position of the pattern with length k
  j <- patt2num(pattern)
  #in this position of the pattern, increase the count of the array by 1
  #again start at j+1 because there cant be a value at 0
  freqarray[j+1] <- freqarray[j+1] +1
}
  #output: array with the counted amount of patterns in order with length 4^k
  return(freqarray)
}

#first dataset
compfreq("ACGCGGCTCTGAAA",2)
#second dataset
compfreq("ACTTCGCCTAAGTCATTTATCCCGTGGTACGACGCTCCCTTACAGTCTTATATCCCGGTATATACGCAGAAATGCCTACGTCCCCTCGTCCCACACACCAGGGAAGCTGAAATCGCTCATCTACTATGCGTGTACTTCCGGACGAAATCGTCGTCGGCTTCTGTCTGGCGCTGGAGATCCGGGCTTCTTGAGGGACACACCCATTATGACCGTTACAGGACTTACAACTACTCTGAGCAATGATGGTGCTCTGTAACGAACAAACGCACTCACCTCTGTTTCCTGTATGACATCCTCAAATGGATCGACCGTGATGTACTGAGCGAATAAGTGCGGATTACATTTATAGTCAGCTACATTTATTCGCCGCTCGGAGCAGAGTATAATGAATTTATACCACTTGTTAGACTCCTTCTCGCATTTAGCCCCTACCGCAAGTCGGAGCGTTGGGGTGCAATAGAGTTTTCAGTATCTACGTACCGTTAAGTCTCTCGCGTTCTTTCAGCAGGCATCAATATGTTGCTTGCTGTGGGGTCGGGTGGGGCGGAGAGCCAATAAAGTGCATCGGAATTGGCTGCCCTCCTACGAATCCGCAAGATGCGGTGATGCTACGTGATTATGACTACTAGCTTAGTCCC",6)


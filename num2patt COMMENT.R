#define function "num2sym"
#in: number with values 0 to 3
#out corresponding characters A,C,G,T
num2sym <- function(number){
  #if input is 0, give out "A"
  if(number==0)
    return("A")
  #if 1 -> C
  if(number==1)
    return("C")
  #if 2 -> G
  if(number==2)
    return("G")
  #if 3 -> T
  if(number==3)
    return("T")
  #no output if number is not one of the above
}

#define function "num2patt"
#in: number "n", length of pattern to be created "k"
#dependancies: function "num2sym"
num2patt <- function(n,k){
  #if the pattern is only k=1 base long, 
  #return that symbol using "num2sym"
  if (k == 1) 
    return(num2sym(n))
#define "prefind" as the number divided by 4 without any decimals using "%/%" ref operators: https://stat.ethz.ch/R-manual/R-devel/library/base/html/Arithmetic.html    
prefind <- n %/% 4
#define r as the rest of that division using "%%" 
r <- n %% 4   #dividing by 4 because we convert the input from base 10 into base 4
#define "symbol" as the symbol of the rest using function "num2sym" on the number r
symbol <- num2sym(r)  #symbol is in base 4, so num2sym always works

#feed prefind pack into the function, but lower k by 1 each time
#until k = 1 then it gets stopped using the if condition above
#write it as new variable "prefpatt"
prefpatt <- num2patt(prefind,k-1)
#paste: combines vectors into character vectors with seperation "sep=". ref: https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/paste
#combine the vectors prefpatt and symbol without any seperation 
#output: corresponding base sequence of the length k with the index n
return (paste(prefpatt,symbol,sep=""))
}

#first dataset
num2patt(45,4)
#second dataset
num2patt(5353,7)
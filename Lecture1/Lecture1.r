# source("../lib/vFun.r")
# v for Van or Visualize, as you prefer

vF2C    = function(f)   { 5 / 9 * (f - 32) }
vC2F    = function(c)   { 9 / 5 *  c + 32  }

vMod    = function(n, m) { n - m * floor(n/m) }
vShell  = function(command) {cat(system(command, intern=T), sep='\n')}
vShow   = function(s,u) {cat(s, noquote(paste(u)),'\n')}
vComment = function(U) { cat(U, "\n"); }

vPrintVector = function(U)
{
    if(!is.vector(U)) { warning("vPrintVector(U): argument U is not a Vector.") }
    cat(deparse(substitute(U)), noquote(paste(U)),'\n');
}

vPrintMatrix = function(U)
{
    if(!is.matrix(U)) { warning("vPrintMatrix(U): argument U is not a Matrix.") }
    cat(deparse(substitute(U)));
    prmatrix(U,quote=F,right=T)
}

vPrintLabelAndVector = function(label, U)
{
    if(!is.vector(U)) { warning("vPrintVector(U): argument U is not a Vector.") }
    cat(label, noquote(paste(U)),'\n');
}

vPrintLabelAndMatrix = function(label, U)
{
    if(!is.matrix(U)) { warning("vPrintMatrix(U): argument U is not a Matrix.") }
    cat(label);
    prmatrix(U,quote=F,right=T)
}

vPrint = function(U)
{
    label = deparse(substitute(U))
    if(is.vector(U)) { vPrintLabelAndVector(label, U) }
    if(is.matrix(U)) { vPrintLabelAndMatrix(label, U) }
}

vMatrix = function(s, M)
{
    m = dim(M)[1]
    n = dim(M)[2]
    image(1:m,1:m,
          flip.matrix(rotate180.matrix((M))),
          axes=FALSE, ann=FALSE,
          col = gray.colors(m*n, 0, 1, 2.2))
    mtext(side = 1, text = s, line = 1) # manual text
}

vIdentityMatrix = function(size) { diag(size); }

# Smart package loader adapted from: https://rpubs.com/aagarwal29/179912
vLoadList = function(packageList)
{
    newPackages = packageList[!(packageList %in% installed.packages()[,"Package"])]
    # Install the ones that aren't there!
    if(length(newPackages)) install.packages(newPackages)
    # Load them
    lapply(packageList,function(x){library(x,character.only=TRUE)})
    # paste(version$version.string)
    cat("Loaded Library Versions: ")
    paste(packageList, packageVersion(packageList), collapse=' ')
}

vUnloadLibs = function() { pacman::p_unload(pacman::p_loaded(), character.only = TRUE) }
vUnloadObjs = function() { rm(list=ls()) }
vUnloadAll  = function() { vUnloadLibs(); vUnloadObjs(); }

x = c(88, 5, 12, 13);
x
x = c(x[1:3], 168, x[4]); # insert 168 at position 4 by reassignment
x

x = 1:4
x
length(x)

x = c() # Zero length vector
x
length(x)
if (length(x)==0) { print("x is empty") }

z = 3 # No declaration required, because no allocation needed
z

# This won't work because allocation is needed
# y[1] = 5
# y[2] = 12

y = vector(length=2)
y[1] = 5
y[2] = 12
y

y = c(5,12)
y

x = c(1,5)
vShow("x is: ", x)
class(x)
x = "abc"
vShow("x is: ", x)
class(x)

1:5 + 10:20 # Shorter vector is replicated so add can be performed

2+3
"+"(2,3)

x = c(1, 2, 4)
y = c(5, 0,-1)
x + y
x - y
x * y
x / y

x = c(12,5,13)
x
x + 4           # EXAM question that 4 gets instanced across the vector add because + is a "function"
(x + 4) - 4

y = c(1.2, 3.9, 0.4, 0.12);
v = 3:4
vShow("y         is:", y)
vShow("c(1,3)    is:", c(1,3))

vShow("y[c(1,3)] is:", y[c(1,3)])
vShow("y[2:3]    is:", y[2:3])

vShow("v = 3:4   is:", v)

vShow("y[v]      is:", y[v])

x = c(4,2,17,5)
y = x[c(1,1,3)] # duplicate subscript allowed
y

z = c(5, 12,13)
vShow("                 z is:", z)
vShow("exclude first element:  ", z[-1]) # negative numbers exclude their cognate indexed items

vShow("                       z is:", z)
vShow("exclude elements 1 and 2 is:     ", z[-1:-2]) # exclude elements 1 through 2

z = c(5,12,13)
z[1:(length(z)-1)] # OR
z[-length(z)]      # Equivalent construct

5:8 # Forwards

5:1 # Backwards

for(i in length(x):1) { print(x[i])} # For loop syntax in R

i = 2

1:i-1 # this performs 1:i, then subtracts 1

1:(i-1) # this subtracts 1 from the current value of i, then generates the sequence.

3:8

seq(from=12,to=30,by=3) # sequence operator, nice

seq(1.1, 2.0, length=10)

seq(1.1, 2.0, by=0.1)

x=c()

for(i in 1:length(x)) { print(i)} # BAD - false iterations

for(i in seq(x)) { print(i)} # CORRECT - no iterations for the empty vector

x = rep(8,4)
x
rep(c(5,12,13),3)
rep(1:3,2)

rep(1:3,2)

rep(c(5,12,13), each=2) # interleaves

x = 1:10

x > 8 # WOW

any(x > 8) # > is a function

all(x > 8) # any and all are symbolic

"b" > "v" # lexicographic comparison, cool!

"b" > "a"

vals = c("medium", "low", "high", "low")
vals[1] > vals[3]

sorted = sort(vals)
vShow("sorted: ", sorted)

y = c(1.2, 3.9, 0.4)

z = round(y) # functions are vectorized in R

z

x = c(5, NA, 12)

mode(x[1])
mode(x[2])
class(x)

y = c("abc", "def", NA)
mode(y[2])
mode(y[3])

y = c(1:2, "z", NA, 3:5)
mode(y[2])
mode(y[3])

z = c(5,2,-3,8); z

w = z[z*z > 8]; w # Preserve all values whose square is greater than 8

z*z

j = z*z > 8 ; j

y = c(1,2,30,5)

y[j]

u = z[z>0]; u # give me all the elements of z greater than zero, or give me the positive elements of z

x = c(6,1:3,NA,12); x

x[x > 5]  # BAD, because <NA> is allowed to satisfy the constraint, because it 'could'

subset(x,x>5) # GOOD, because the constraint is enforced as 'must'

z = c(5,2,-3,8)

which(z*z > 8) # gives the position or index that satisfy the conditional filter

x = c(5,2,9,12); x
ifelse(x > 6, 2*x,3*x)

x = 1:10
y = 11:20
ifelse(vMod(x,2),x,y)

g = c("M","F", "F","I","M","M","F") # vectorized if-then-else

ifelse(g == "M",1,ifelse(g == "F",2,3))

m = which(g == "M"); m
f = which(g == "F"); f
i = which(g == "I"); i

x = 1:3
y = c(1,3,4)
x == y # Test the vector component-wise

all(x == y) # Test the whole vector, if a single element differs return a single FALSE  

# use this to deduce a projection that is equivalent

identical(x,y) # faster since it can stop as soon as a mismatch is detected

x = c(1,2,4)

names(x)

names(x) = c("a", "b", "ab")

names(x)

x

names(x) = NULL # Clear the names on x
x

c(5,2,"abc")

w = c(5,2,list(a=1,b=4)); w

getwd() #hit Run or press Ctrl+Enter to run this line

#List the files in the working directory
dir()
list.files()
vShell('ls')

######################################################
###Arithmetic in R

#Just type in the entire formula
5 + 6
9 - 3
4 * 8
12 / 3
2^4

(5 + 9)/2 + 12^2

sqrt(9) #sqrt is a function calculates the square root

log(10)
log2(4) #But we do not have log3 or log5
log(9, base = 3) #set the base

#Variable/object, name and value
x = 5; x

y <- 10; y  #We can state more than one commands in one line by ";"

n = "black"; print(n)

sumN <- 5 + 1 * 4

print(sumN)

######################################################
#Data Types in R
######################################################
#Vector
vec = c(1,2,3,7,8,9,10,14)

vec

vec[1:3]

vec[4:8]

length(vec) #the number of elements in vector

rm(vec) #remove the variable

vec1 = c(10:20)

vec1

vec2 = seq(1,10,1)

vec2

vec3 = seq(10,20,2)

vec3

vec4 = seq(1,2, by = 0.1)

vec4

rm(vec1, vec2, vec3, vec4)

color = c("red","blue","yellow","green","purple")

#Reverse the elements
rev(color)

color[c(1,3,5)] #indicate the elements by positions

color1 = rep("red", 8)

color1

color2 = c(c("bule","green"), rep("red",2), rep("purple",3))

color2

#days
days_vector = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

days_vector[3]

days_vector[c(3,5)]

days_vector[3:5]

rev(days_vector)

#A logical vector
l = c(TRUE, TRUE, FALSE, T, F) #note, no quotes needed

print(l) #print the vector

#Vector calculation
a = c(1,2,3,4,5)

a * 2

a * a

b = c(7,3,9,3,4)

a + b

#Try this
b = c(6,5,4,7,8,9)
vShow("a is: ", a)
vShow("b is: ", b)
vShow("a + b is: ", a+b)

a + b

ls() #list all the variables 



# rm(list = ls())



# this can be sourced as
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

packageList = c("base","repr","plotly","gplots","ggplot2","knitr","plot3D","Thermimage","BBmisc","dplyr","pacman")

vLoadList(packageList)

X=matrix(1:16, nrow=4, byrow=FALSE) # makes it row major order
Y=t(X)
Z=max(X) - abs(X-Y) # hilight the termwise entries that are most alike
N=Z/max(Z)
X;Y;Z;N

options(repr.plot.width=6, repr.plot.height=2) # square matrix
par(mar = c(4, .5, .5, .5))                    #
par(mfrow = c(1, 4))                           # four figures on one row

vMatrix("X",                X)
vMatrix("Y = t(X)",         Y)
vMatrix("Z = max(X)-|X-Y|", Z)
vMatrix("N = Z/max(Z)",     N)


p = plot_ly(z = ~N) %>% add_surface()
p

x = c(0:10,10:0)
vShow("x is: ", x)
cat("length(x) is: ", length(x), '\n')
cat("x[1] is: ", x[1], '\n')
cat("x[length(x)/2] is: ", x[length(x)/2])

data(Orange)
paste("There are ",
        Orange$Tree[length(Orange$Tree)], "trees and",
        nrow(Orange), "measurements in Orange data frame.")
head(Orange)

vShow("Orange$Tree", Orange$Tree)

ggplot(data = Orange, mapping = aes(x = age, y = circumference)) +
    geom_point(alpha = 0.7, color = "red")

write.csv(Orange, file = "OrangeDat.csv")

df = read.csv("OrangeDat.csv")
head(df)

df2 = read.csv2("OrangeDat.csv")
head(df2)

write.table(Orange, "OrangeDat.txt", row.names=F, col.names=T)

df3 = read.table("OrangeDat.txt")

head(df3)

save(Orange, file="OrangeDat.RData")
load("OrangeDat.RData", verbose=T)

# data()

save(Orange, file="/Users/van/Desktop/Lecture1.RData")

i = 2
paste("The type of i is", class(i), "and value of i is", i)

a = pi
paste("The type of a is", class(a), "and value of a is", a)

B = T
paste("The type of B is", class(B), "and value of B is", B)

s = "the big brown bear"
paste("The type of s is", class(s), "and value of s is", s)
paste("The substring of s starting at 5 is", substring(s, 5))
paste("The 9th character of s is ", substring(s, 9, 9))

if(sqrt(4) == 2)
{
    paste("The sqrt(4) is 2")
} else # the closing brace and else must appear on the same line!
{
    paste ("The sqrt(4) is not 2")
}


x = array(1:10) # Explicit declaration as array
cat(class(x), x, '\n')
for(i in 1:10)
    x[i]=i*i
cat(class(x), x, '\n')
x = 1:10 # Implicit declaration as vector
cat(class(x),x, '\n')
for(i in 1:10)
    x[i]=i*i
cat(class(x), x, '\n')

i = 1
while (i < 6)
{
    print(i)
    i = i+1
} 

x = 1
repeat
{
    print(x)
    
    x = x + 1
    
    if (x == 6) break
}

x = 1
repeat
{
#  if (x == 3) next
    print(x)
    x <- x + 1
    if (x == 6) break
}

for (val in 1:5)
{
    if (val == 3) next
    
    print(val)
}

# source("../lib/vLib.R") # loaded above

# Scalar Execution
paste(32, '°F is', vF2C(32), '°C')

# Vector Execution
celsiusTemps = c(0, 37, 100) # c is concatenate, not celsius
paste(celsiusTemps, '°C is', vC2F(celsiusTemps), '°F')
paste(vC2F(celsiusTemps), '°F is', celsiusTemps, '°C')

x = 1:8;
cat("x is", x, '\n');
cat("mean(x) is", mean(x), '\n')
cat("var(x) is", var(x), '\n')
cat("sd(x) is", sd(x), '\n')
cat("sd(x) is", signif(sd(x),3), '\n')
cat("median(x) is", median(x), '\n')
cat("range(x) is", range(x), '\n')
cat("sum(x) is", sum(x), '\n')
cat("min(x) is", min(x), '\n')
cat("max(x) is", max(x), '\n')

# Create a vector with missing data
x = c(1:4, NA, 6:8)
vShow("x is:", x)
s = factor(is.na(x), labels = c('F', ' T') )
vShow("b is:", s)
cat("mean with missing data: ", mean(x), '\n')
cat("mean with missing data excluded: ", mean(x,na.rm=T))




u = 1:4
ls()
vShow("u is: ", u)
vShow("removing u", rm(u))
vShow("The existence of u is", exists("u"))

setwd(".")                                 # Don't change directory for this example.
vShow("Current Directory is:", getwd())    # Get the working directory (current directory in Unix).
data.frame("Van.Files"=dir())              # List the files in a dataframe.

example(sqrt)

vShell("pwd");
vShell("ls"); 

# vUnloadAll()

head(iris)

# Plotting (This example takes about 15 seconds in a cold start on my iMac)
library(ggplot2)
options(repr.plot.width=6, repr.plot.height=6) # square matrix
ggplot(data=(iris), aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point(size=3)

# Run this once, it is platform dependent, 64 bit for iMac, 32 bit for laptop
#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
# BiocManager::install(version = "3.10") # for iMac
# BiocManager::install() # for laptop

# These lines are less platform dependent
# BiocManager::install("Biostrings")
# BiocManager::install("Biobase")
# BiocManager::install("biocViews")

BiocManager::valid() # initially shows 83 packages out of date, lather, rinse, repeat

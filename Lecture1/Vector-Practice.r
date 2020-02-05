getwd() #hit Run or press Ctrl+Enter to run this line



#Create a new directory within the working directory
dir.create("NEW_Directory")

#List the files in the working directory
dir()
list.files()

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
x = 5

x

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

a + b

ls() #list all the variablts 

rm(list = ls())
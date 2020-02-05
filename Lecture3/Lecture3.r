# name value pairs like Python dictionary

# List allwos for different datatypes, character, numeric and logical
# Can have list of lists for database.


# vector would coerce evertying into same datum type

j = list(name="joe", salary=55000, union=T)

j

# component names are called tags

j$sal # can abbreviate to whatever extent is possible without causing ambiguity

j$name; j$union

j$s

jalt = list("joe", 55000, T)

jalt

jalt[1]; jalt[2]; jalt[3];

z = vector(mode="list")

z

z[["abc"]] = 3; # notice double bracket operator

z

j$salary

j[["salary"]]

j[[2]]

#named access, double-bracket access with name, double-bracket access with index

j[1:2]

j[1:3]

j2 = j[2]

j2

class(j2)

str(j2) # tells you what kind of 'structure'

# note the difference bewtween single and double bracket indexiting

z = list(a="abc", b=12)

z

z$c = "sailing"

z[[4]] = 28 # make a note of this and send to Dr. Yang topic, naming after the fact.

z[5:7] = c(F,T,T)

z

z$d = 28

z

# delete by setting to NULL

z$b = NULL

z

c(list("Joe", 55000, T),list(5)) # c works with lists as well as vectors

j

length(j) ; # gives number of name value pairs in the list

names(j)

ulj = unlist(j)

ulj

class(ulj)

ulj[1]

ulj[2]

z = list(a=5,b=12,c=13)

y = unlist(z)

class (z); class(y)

y

z

w = list(a=5,b="xyz")

wu = unlist(w)

class(wu)

w

wu

wu[1]; wu[2]

# vectos coerced to highest type in NULL < raw < logical < integer < real < complex < character < list

# watch types when applying the unlist operator

names(wu)

names(wu) 

names(wu) = c("d", "e")

wu

names(wu[1])= ("f")

wu

wun = unname(wu)

wun

class(wun)

is.vector(wun)

w = list(a=5, b="xyz")

wu = unlist(w)

wn = unname(w)

wu

is.list(wu)

is.vector(wu)

is.list(wn)

is.vector(wn)

# list apply, applies a function to a list

foo = lapply(list(1:3, 25:29), median)

is.list(foo); is.vector(foo)

L = list(1:3,25:29)

L

S = sapply(list(1:3,25:29), median)

S

#use unlist on lapply to get sapply result

g = c("M", "F", "F", "I", "M", "M", "F")

g

lapply(c("M","F","I"), function(gender) which(g==gender))

#

# read data from file
# store in R object
# do your processing/ analysis



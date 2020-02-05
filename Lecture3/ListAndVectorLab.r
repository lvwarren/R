# List and Vector Lab
# Author: L. Van Warren
# Date: Feb 2, 2020

# Van formatting library, functions prefixed with 'v', usually in vFun.R
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

vComment = function(U)
{
    cat(U, "\n");
}

# ============ Vector Exercise 1 ============
a <- c(1,5,4,3,6); vPrint(a)
b <- c(3,5,2,1,9); vPrint(b)
# Boolean vector is true when a <= b
a <= b

# ============ Vector Exercise 2 ============
x <- c(12:4); vPrint(x)
y <- c(0,1,2,0,1,2,0,1,2); vPrint(y)
# Print the INDICES of the vector y where y is non-zero and division is finite.
which(!is.finite(x/y))
# Print the VALUES  of the vector y where y is non-zero and division is finite.
y[c(which(!is.finite(x/y)))]
# Print the VALUES  of the vector x where y is non-zero and division is finite.
x[c(which(!is.finite(x/y)))]

# ============ Vector Exercise 3 ============
x <- c(1,2,3,4); vPrint(x)
# Check x for missing values using !is.na(x)")
!is.na(x)
# Check x for values greater than zero using predicate x > 0:
x > 0
# Add 2 to every value of x and save in k")
k = x+2; vPrint(k)
# Run original expression: (x+2)[(!is.na(x)) & x > 0] -> k
(x+2)[(!is.na(x)) & x > 0] -> k; vPrint(k)
# Run simplified equivalent: h = x+2
h = x+2
vPrint(h)

# ============ Vector Exercise 4 ============
x <- c(2, 4, 6, 8); vPrint(x)
y <- c(TRUE, TRUE, FALSE, TRUE); vPrint(y)
vPrint(x[y])
vPrint(sum(x[y]))

# ============ Vector Exercise 5 ============
x <- c(34, 56, 55, 87, NA, 4, 77, NA, 21, NA, 39); vPrint(x)
cat("What is is.na(x) vector?")
is.na(x)
cat("How many TRUE values in is.na(x) vector?")
vPrint(sum(is.na(x)))
answers = list(a="count(is.na(X))",
               b="length(is.na(x))",
               c="sum(is.na(x))",
               d="count(!is.na(x))",
               e="sum(!is.na(x))")
cat("The statement which will count the number of NA values in x is:")
answers[which(answers %in% "sum(is.na(x))")]


# ============ List Exercise 1 ============
p <- c(2,7,8); vPrint(p)
q <- c("A", "B", "C"); vPrint(q)
x <- list(p, q); vPrint(x)
vPrint(x[2])
answers = list(a=NULL,
               b=c("A", "B", "C"),
               c="7")
cat("The value of x[2] is:")
answers[which(answers %in% x[2])]

# ============ List Exercise 2 ============
w <- c(2, 7, 8); vPrint(w)
v <- c("A", "B", "C"); vPrint(v)
x <- list(w, v); x
# Since vectors are immutable we must replace vector v
x[[2]] = c("K", "B", "C"); x

# ============ List Exercise 3 ============
a <- list ("x"=5, "y"=10, "z"=15); vPrint(a)
answers = list(a="sum(a)",
               b="sum(list(a))",
               c="sum(unlist(a))")
cat("The statement which will give the sum of all elements in a is:")
answers[which(answers %in% "sum(unlist(a))")]
# Running this on the list gives a sum of: "
sum(unlist(a))

# ============ List Exercise 4 ============
Newlist <- list(a=1:10, b="Good morning", c="Hi")
Newlist
Newlist$a = Newlist$a + 1 
Newlist

# ============ List Exercise 5 ============
b <- list(a=1:10, c="Hello", d="AA")
b$a[c(1,3:10)]

# ============ List Exercise 6 ============
x <- list(a=5:10, c="Hello", d="AA")
x
x$z = "New Item"
x

# ============ List Exercise 7 ============
y <- list("a", "b", "c")
y
names(y) = c("one", "two", "three")
y

# ============ List Exercise 8 ============
x <- list(y=1:10, t="Hello", f="TT", r=5:20)
x
length(x$r)

# ============ List Exercise 9 ============
string <- "Grand Opening"
s = strsplit(string," ")[[1]]
list(s[1],s[2])


# ============ List Exercise 10 (bonus) ============
y <- list("a", "b", "c")
q <- list("A", "B", "C", "a", "b", "c")
d = setdiff(q,y)
d



# source("../lib/vFun.r") # Support Routines
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

M = matrix(c(1:10),nrow=5,ncol=2, dimnames=list(c("a","b","c","d","e"),c("A","B")));
vPrint(M);
vPrint(t(apply(M,2,max)))

x = 1:3
y = 4:6
z = 7:9
A = cbind(x,y,z)
rownames(A) = c("a", "b", "c")
vPrint(A)
A

M = matrix(8:11, nrow=2); vPrint(M)
v = c(1:10); vPrint(v)

dim(v)                                    ; # Dimension of a vector is NULL, does    print
cat("      dim(v): ",        dim(v), "\n"); # Dimension of a vector is NULL, doesn't print
cat("      dim(M): ",        dim(M), "\n"); 
cat("     mode(M): ",       mode(M), "\n");
cat("    class(M): ",      class(M), "\n");
cat("is.matrix(M): ",  is.matrix(M), "\n");
cat(" which(M > 2): ", which(M > 9), "\n"); # Which INDICES of flattened Y hold values > 9?

M = matrix(1:25,nrow=5)

letters = paste(LETTERS, collapse = ""); vPrint(letters) # collapse should be separator

for (i in 1:nrow(M))   
    for (j in 1:ncol(M))
    {
        k = (i-1)*nrow(M) + j
        M[i,j] = substring(letters,k,k)
    }
vPrint(M)
letters
M

# The overdetermined case.
# Notice that nrow and ncol attributes are SINGULAR, not PLURAL.
Y  = matrix(1:4,nrow=2,ncol=2); vPrint(Y);    # Simple
Y. = matrix(c(1,2,3,4),nrow=2); vPrint(Y.);   # Vector 
vPrint(Y == Y.)                               # Test for equivalence.
Y[2,2] = 0                                # Perturb an entry.
vPrint(Y)
vPrint(Y == Y.)                               # 
Y.. = matrix(1:4,nrow=2,byrow=T); vPrint(Y..) # Row major order
vPrint(Y == Y..)                              # Test

Y = matrix(1:4, nrow=2); vPrint(Y)
vPrint(3*Y) # termwise multiplication of constant

vPrint(Y+Y) # termwise matrix addition

vPrint(Y*Y) # termwise matrix multiply

vPrint(Y %*% Y) # full matrix multiply

k = 6
I4 = vIdentityMatrix(k)
vPrint(k*I4)
det(k*I4)
k^k

cat("Construct a Matrix using cbind and four column vectors:\n")
Z =cbind(1:4, c(1,1,0,0), c(1,0,1,0)); vPrint(Z)

cat("Drop first column of Z using Z[,2:3]:\n")  # use to delete any row or column
DC1Z = Z[,2:3]; vPrint(DC1Z)

cat("Drop first column of Z using Z[,-1]:\n")  # use to delete any row or column
DC1Z = Z[,-1]; vPrint(DC1Z)

cat("Prevent Z from reverting to a vector using    drop=F argument")
vPrint(Z[,1, drop=F])

cat("Confirm Z      reverts to a vector without drop=F argument")
u=Z[,1]
vPrint(u)
cat("is.matrix(u) is: ", is.matrix(u), "\n")

cat("Confirm Z        stays    a matrix with    drop=F argument")
u=Z[,1, drop=F]
vPrint(u)
cat("is.matrix(u) is: ", is.matrix(u), "\n")

# This example is irrelevant because Z won't revert to a vector with two columns!
# Z[1:2,, drop=FALSE]             # EVERYTHING hinges on the presence of that second comma, 
# is.matrix(Z[1:2,, drop=FALSE])  # for the drop=FALSE to be HEARD or registered.

# Assignment to submatrices
cat("Create a 3 x 2 matrix called Y:\n")
Y = matrix(1:6,nrow=3); vPrint(Y)

cat("Create the vector (1 3):\n")
vPrint(c(1,3))
cat("Create a 2x2 matrix called Z:\n")
Z = matrix(c(1,1,8,12),nrow=2); vPrint(Z)

cat("The submatrix of Y to be given the values from Z:\n")
vPrint(Y[c(1,3),])
Y[c(1,3),] = Z; vPrint(Y)

cat("We can use negative subscripts to exclude rows or columns.\n")
cat("Consider Y: \n")
Y = matrix(1:6,nrow=3); vPrint(Y)
cat("Y[-2,] excludes the second row: \n");
vPrint(Y[-2,])

cat("Filtering can be done with matrices, just as with vectors:\n")
cat("Consider X: \n")
X = cbind(1:3,2:4); vPrint(X)
cat("Consider the filter predicate:")
X[,2] >= 3
cat("What is the output of X[X[,2] >= 3,]?\n")
vPrint(X[X[,2] >= 3,])

cat("Consider M:\n")
M = matrix(1:6, nrow=3); vPrint(M)
cat("What is A when A = M[M[,1] > 1 & M[,2] > 5,]\n")
cat("Dissecting M column 1 is M[,1] is:\n")
vPrint(M[,1,drop=F])
cat("as a conditional M[,1] > 1 we have:\n")
vPrint(M[,1,drop=F] > 1)
cat("as a conditional M[,2] > 5 we have:\n")
vPrint(M[,2,drop=F] > 5)
cat("Anding Both Condition we have\n")
vPrint(M[,1,drop=F] > 1 & M[,2,drop=F] > 5)
cat("So now we have the vector that selects the rows of M\n")
cat(M[,1] > 1 & M[,2] > 5, "\n")
cat("So A gets the last row of M\n")
A = M[M[,1] > 1 & M[,2] > 5,]; A
cat("is.matrix(A): ", is.matrix(A))

cat("Given M:\n")
M = cbind(c(5,2,9),c(-1,10,11)); vPrint(M)
cat("which(M > 2) asks which INDICES in M are > 2. They are: ", which(M>2), "\n")
cat("if M vectorized is ", as.vector(M), "\n")
cat("then the values of M where the condition is met are ")
vPrint(M[which(M>2)])


Y = vIdentityMatrix(4); vPrint(Y)
X = matrix(1:4,nrow=2); vPrint(X)
Y[c(1,4),] = X # EXAM
vPrint(Y)

# Filtering Redux
X = matrix(c(1,2,3,2,3,4), nrow=3); vPrint(X)

X[,2] >= 3

vPrint(X[X[,2] >= 3,]) # total exam question, go over the meaning of 3,

Z = cbind(c(10,20,30),c(40,50,60)); vPrint(Z)

# apply(M, dimcode, f, args_of_f)
apply(Z,2,mean) # take the mean of the columns
apply(Z,1,mean) # take the mean of the rows

f = function(x) { x*c(1/2,1/4) };f # cool trick: naming a function prints it
f(1)


# looks like a dot product when you transpose it!
Y = t(apply(Z,1,f)); Y # Apply fills the result columnwise into dimcode=ROWS

vPrint(Y == t(t(Y)))

vPrint(Z)
f = function(U) U*c(2,3) # U can be a scalar, vector or Matrix
vPrint(f(c(1,2)))
vPrint(c(1,2)*c(2,3)) # this is a vector termwise multiply
vPrint(c(1,2)/c(2,3))
vPrint(c(1:3)*Z)
vPrint(Z*c(1:3))
# this flattens Z, then alternates multiplying its elements by 1 and 2 respectively
# the termwise products appear commutative!

vPrint(Z)
f = function(X) X/c(2,8)
apply(Z,2,f) # denominator becomes (2,8,2) and recycling yuck
cat("the termwise looks like:\n")
vPrint(cbind(c(2,8,2),c(2,8,2)))
vPrint(Z/cbind(c(2,8,2),c(2,8,2)))

# copymaj example
X = cbind(c(1,1,1,0),c(0,1,0,1),c(1,1,0,1),c(1,1,1,1),c(0,0,1,0)); vPrint(X)
copymaj = function(U,d) { if( mean(U[1:d])) 1 else 0}
result = 
    cbind(
        as.matrix(apply(X,1,copymaj,1)),
        as.matrix(apply(X,1,copymaj,2)),
        as.matrix(apply(X,1,copymaj,3)),
        as.matrix(apply(X,1,copymaj,4)),
        as.matrix(apply(X,1,copymaj,5))
    )
vPrint(result)

x = c(1, 2, 3); vPrint(x)
x = c(x, 4, 5, 6); vPrint(x)

M = matrix(1:4, nrow = 2); vPrint(M)
M = rbind(M, c(3,6)); vPrint(M)
M = cbind(M, 7:9); vPrint(M)
M[,2] = 4:6; vPrint(M)

M = rbind(c(1,2),c(3,4)); vPrint(M) # row ordered
M = cbind(c(1,2),c(3,4)); vPrint(M) # col ordered

M = matrix(1:9,nrow=3); vPrint(M)
M = M[1:2,]; vPrint(M)
M = M[,1:2]; vPrint(M)

M = matrix(1:9,nrow=3); vPrint(M)
M = M[-2,  ]; vPrint(M)
M = M[  ,-2]; vPrint(M)

# Check type of matrix with dim(), class() and is.matrix()
Z = matrix(c(1,2,3,4,1,1,0,0,1,0,1,0),nrow=4); vPrint(Z) # generate matrix
Z = Z[,-2:-3]; vPrint(Z)                                 # loses rank of matrix and demoted to vector
cat("dim(Z) is"); dim(Z);                                # NULL won't print inside cat()! this is a bug
cat("is.matrix(Z) is", is.matrix(Z), "\n")
cat("class(Z) is", class(Z), "\n")

# Method 1: Preservation of rank with drop=FALSE
Z = matrix(c(1,2,3,4,1,1,0,0,1,0,1,0),nrow=4); vPrint(Z) # generate matrix
Z = Z[,-2:-3, drop=F]; vPrint(Z)                         # drop=F preserved matrix
cat("is.matrix(Z) ",is.matrix(Z), "\n")

# Method 2: Reinstatment of lost rank using as.matrix()
Z = matrix(c(1,2,3,4,1,1,0,0,1,0,1,0),nrow=4); vPrint(Z) # generate matrix
Z = Z[,-2:-3]; vPrint(Z)                                 # loses rank of matrix and demoted to vector
cat("is.matrix(Z) is", is.matrix(Z), "\n")
cat("Z = as.matrix(Z)\n")
Z = as.matrix(Z)                                         # reinstatement of rank                             
vPrint(Z)
cat("is.matrix(Z) ", is.matrix(Z), "\n")

v = c(1:10); vPrint(v)
cat("attributes(v): "); attributes(v)
M = matrix(1:9, nrow=3); vPrint(M)
cat("attributes(M): "); attributes(M)
cat("nrow(M): ", nrow(M), "\n");
cat("ncol(M): ", ncol(M), "\n");



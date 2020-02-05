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

# 
# test suite implementation - tests, test generator, generics, and commands
#

# class name for the objects representing the generics
GENERIC = "generic"

# returns true, if the given header command is a generic AST. A generic is either an assignment to a generic name, or an assignment to a dependent generic, where the generic dependencies are listed as arguments to the generic ( a(b) <- is a generic a, dependent on generic b). Anything else is not considered a generic and will be interpreted as either condition or a check
is.genericAST <- function(ast) {
    (length(ast) == 3 && (identical(ast[[1]], as.name("<-")) || identical(ast[[1]], as.name("->"))))
}

# returns a generic object created from the given ast. Assumes the ast is already confirmed to be a generic. The generic is an object of class "generic" and has the following fields: name is the name of the generic, dependencies is either NULL for an independent generic, or a vector of names of the generics this one dependes on and the expr field contains the AST of the value of the generic. 
generic <- function(ast) {
    stopifnot(is.genericAST(ast))
    result <- list()
    class(result) <- GENERIC
    result$expr = ast[[3]]
    if (identical(mode(ast[[2]]),"name")) {
        # we are dealing with an independent generic
        result$name <- as.character(ast[[2]])
        result$dependencies <- NULL
    } else {
        # we are dealing with a dependent generic
        ast <- ast[[2]]
        if (length(ast) == 1)
            stop("A dependent generic must have at least one dependency generic")
        result$name <- as.character(ast[[1]])
        result$dependencies = as.character(ast[-1])
    }
    result
}





test <- function(name, ...) {
    header <- eval(substitute(alist(...)))
    if (length(header) < 1)
        stop("At least the name and the code of the test must be supplied. Code is the last argument supplied.")
    code <- header[[length(header)]]
    # if the code is a block, skip the block
    if (is.call(code) && length(code) != 0 && identical(as.character(code[[1]]),"{"))
        code <- code[-1]
    header <- header[-length(header)]
    # now split given headers to conditions, checks and generics
    code
    
}




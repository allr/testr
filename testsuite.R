# 
# test suite implementation - tests, test generator, generics, and commands
#

# class name for the objects representing the generics
GENERIC <- "generic"
INDEPENDENT_GENERIC <- "independent.generic"
DEPENDENT_GENERIC <- "dependent.generic"


contains <- function(lst, element) {
    !is.null(lst[[element]])
}






# returns true, if the given header command is a generic AST. A generic is either an assignment to a generic name, or an assignment to a dependent generic, where the generic dependencies are listed as arguments to the generic ( a(b) <- is a generic a, dependent on generic b). Anything else is not considered a generic and will be interpreted as either condition or a check
is.genericAST <- function(ast) {
    (length(ast) == 3 && (identical(ast[[1]], as.name("<-")) || identical(ast[[1]], as.name("->"))))
}

# returns a generic object created from the given ast. Assumes the ast is already confirmed to be a generic. The generic is an object of class "generic" and has the following fields: name is the name of the generic, dependensOn is either NULL for an independent generic, or a vector of names of the generics this one dependes on and the expr field contains the AST of the value of the generic. 
XXXgeneric <- function(ast) {
    stopifnot(is.genericAST(ast))
    result <- list()
    class(result) <- GENERIC
    expr = ast[[3]]
    if (identical(mode(ast[[2]]),"name")) {
        # we are dealing with an independent generic
        result$name <- as.character(ast[[2]])
        result$dependsOn <- NULL
        result$expr <- eval(expr)
    } else {
        # we are dealing with a dependent generic
        ast <- ast[[2]]
        # is it reasonable to have multiple dependencies? I do not think so because the level of dependency can be
        # specified by the order of its arguments
        if (length(ast) != 2)
            stop("A dependent generic must have at least precisely one dependency generic")
        result$name <- as.character(ast[[1]])
        result$dependsOn = as.character(ast[-1])
        # the expression list of dependent generics cannot be evaluated yet as it may contain the independent generics 
        result$expr <- expr
    }
    result
}

# separates the given header to independent and dependent generics and other commands. The commands can later become either checks or conditions, but these may require evaluation with generics substituted by their proper values and the evaluation of the commands (and therefore the distinction between checks and conditions) is deferred till exact values for the generics are known. The dependencies vector is added to all independent generics on which some generics depends. 
separateGenerics <- function(header) {
    independentGenerics <- list()
    dependentGenerics <- list()
    commands <- list()
    for (h in header) {
        if (is.genericAST(h)) {
            g <- generic(h)
            if (identical(g$dependsOn, NULL))
                independentGenerics[[g$name]] <- g
            else
                dependentGenerics[[g$name]] <- g
        } else {
            commands <- c(commands, list(h))
        }
    }
    # for each independent generics add dependent generics to field dependencies
    for (i in 1:length(dependentGenerics)) {
        dn <- dependentGenerics[[i]]
        ig <- independentGenerics[[dn$dependsOn]]
        if (identical(ig$dependencies, NULL))
            ig$dependencies <- dn$name
        else
            ig$dependencies <- c(ig$dependencies, dn$name)
        independentGenerics[[dn$dependsOn]] <- ig
    }
    
    return(list(independentGenerics = independentGenerics, dependentGenerics = dependentGenerics, commands = commands))
}

# returns the number of tests to be generated using the generics, that is the list of all independent generics 
genericSizesXXX <- function(independentGenerics) {
    result <- rep(0, length(independentGenerics))
    for (i in 1:length(independentGenerics))
        result[[i]] <- length(g[[i]]$expr)
    result
}


generateTestsXXX <- function(name, code, independentGenerics, dependentGenerics, commands) {
    # first get the vector of maximal values for different generics and compute the number of tests out of it
    max <-  numberOfTests(independentGenerics)
    n <- prod(max)
    # create the list of values of independent and dependent generics
    ig <- rep(1, length(independentGenerics))
    dg <- rep(1, length(dependentGenerics))
    # now in a loop generate the tests by first determining the values of the independent generics and adding them to
    # the environment, then using this to determine the values of the dependent generics 
    lapply(seq_len(n), function(i) {
        # first get the values of the independent arguments to the env
        env <- list()
        for (j in 1:length(independentGenerics)) {
            g = independentGenerics[[j]]
            env[[g$name]] <- g$expr[[(ig[[j]])]]
        }
        # now add the values of the dependent arguments, whose expressions must first be evaluated with the environment
        # already obtained from independent arguments
        for (j in 1:length(dependentGenerics)) {
            g = dependentGenerics[[j]]
            # TODO THIS DOES NOT PROPERLY 
            expr <- g$expr
            expr <- eval(substitute(substitute(expr, env), list(expr = expr)))
            env[[g$name]] <- expr[[(dg[[j]] %% length(expr)) + 1]]
        }
        # now deparse the code to string with substitutions where available
        code = deparse(eval(substitute(substitute(code, env), list(code = code))))
        # TODO this is probably a dirty hack, but I need a way to be able to 
        print(code)
            
        
        
    })    
    
}


testXXX <- function(name, ...) {
    header <- eval(substitute(alist(...)))
    if (length(header) < 1)
        stop("At least the name and the code of the test must be supplied. Code is the last argument supplied.")
    code <- header[[length(header)]]
    # if the code is a block, skip the block
    #if (is.call(code) && length(code) != 0 && identical(as.character(code[[1]]),"{"))
    #    code <- code[-1]
    header <- header[-length(header)]
    # now split given header to independent and dependent generics and commands
    header <- separateGenerics(header)
    # and generate the tests themselves
    generateTests(name, code, header$independentGenerics, header$dependentGenerics, header$commands)
    
}


#' Creates a generic out of its arguments. 
#' 
#' The result will be an object of type generic. It will remember its name and list of expressions that form its values. If it is dependent generic, the field dependsOn will contain the name of the generic this one depends on.
#' 
#' \TODO more documentation on the generic
#' 
#'  @param name Name of the generic, either name, or a string
#'  @param dependsOn Optional name of a generic this one depends on. If not given, the generic is independent
#'  @param ... comma separated list of generic values
#'  
#'  @examples
#'  \code{
#'  generic(name=a, 1, 2, 3, 4, 5)
#'  generic(name="b", dependsOn=a, 1, 2, 3)
#'  }  
generic <- function(name, ..., dependsOn = NULL) {
    # create the object and set its class properly
    result <- list()
    # get the expressions of the generic argument
    result$expr <- eval(substitute(alist(...)))
    # enavluate the name and dependsOn if present to character vectors and store them to the object
    result$name <- as.character(substitute(name))
    if (! missing("dependsOn")) {
        result$dependsOn <- as.character(substitute(dependsOn))
        class(result) <- c(GENERIC, DEPENDENT_GENERIC)
    } else {
        class(result) <- c(GENERIC, INDEPENDENT_GENERIC)
    }
    result
}

#' Returns the length of the generic, that is the number of distinct values it can take.
length.generic <- function(g) {
    length(g$expr)
}

#' Returns whether the given object is a generic or not
is.generic <- function(o) {
    inherits(o, GENERIC)
}

#' Returns true if the object is an independent generic.
#' 
#' Apart from inheriting from generic.independent, independent generics must also lack the field dependsOn (but this is not tested by this method)
is.independentGeneric <- function(o) {
    inherits(o, INDEPENDENT_GENERIC)
}

#' Returns true if the object is a dependent generic. 
#' 
#' Apart from inheriting from generic.dependent, dependent generics must also have the field dependsOn that has the name of the generic it depends on. However, the method only tests the class of the object. 
is.dependentGeneric <- function(o) {
    inherits(o, DEPENDENT_GENERIC)
}

is.check <- function(o) {
    FALSE
}

is.condition <- function(o) {
    FALSE
}

# header separation ----------------------------------------------------------------------------------------------------

#' Separates the given headers into generics, conditions and checks. 
#' 
#' The header is a list of ASTs for commands passed to the test builder. It evaluates them and decides if they are independent generics, dependent generics, conditions or checks. It creates these lists and returns them in a list. For each dependent generics also adds its name to the dependents field of the independent generic it depends on. This is later used in generic expansion. 
separateTestHeader <- function(header) {
    iGens = list()
    dGens = list()
    conditions = list()
    checks = list()
    for (i in header) {
        o = eval(i)
        if (is.independentGeneric(o)) {
            # for independent generics only check their name for duplicity and add it to the list of independent generics
            if (contains(iGens, o$name) || contains(dGens, o$name))
                stop("Generic with name ", o$name, " already exists for the test. Generics must have unique names.")
            iGens[[o$name]] <- o
        } else if (is.dependentGeneric(o)) {
            # for dependent generics, check the name for duplicty, add the generic to the list of dependent generics and
            # then update the dependents field of the independent generic the dependent one depends on. 
            if (contains(iGens, o$name) || contains(dGens, o$name))
                stop("Generic with name ", o$name, " already exists for the test. Generics must have unique names.")
            if (!contains(iGens, o$dependsOn))
                stop("Generic ", o$name, " depends on generic ", o$dependsOn, " which is not known. All independent generics must be defined first")
            dGens[[o$name]] <- o
            g <- iGens[[o$dependsOn]]
            g$dependents <- c(g$dependents, list(o$name))
            iGens[[g$name]] <- g
        } else if (is.condition(o)) {
            # TODO
        } else if (is.check(o)) {
            # TODO
        } else {
            stop("Command ",deparse(i), " not found")
        }
    }
    list(independentGenerics = iGens, dependentGenerics = dGens, conditions = conditions, checks = checks)
}

# Substitute and replacemets -------------------------------------------------------------------------------------------


#' A more complex form of substitute. 
#' 
#' TODO This maybe calling testSubstitute2 is what should be done here, or renaming testSubstitute2 to testSubstitute completely because when used in test expansion the AST will already be evaluated and the initial substitute will not help us. 
#' 
#' TODO the string replacement is not 100% identical to the substitute and does not allow % to appear in the string on its own if a valid generic name is after it, change to allow %% to become % and to only match each occurence of % in the string once. The way it is now if %a is replaced for say $haha and there is also haha generic, the final replacement will be to haha generic's value
testSubstitute <- function(ast, env = list()) {
    #ast = substitute(ast)
    if (identical(typeof(ast), "character")) {
        for (i in 1:length(env)) {
            ast <- gsub(paste("%", names(env)[[i]], sep=""), env[[i]], ast)
        }
        ast
    } else {
        testSubstitute2(ast, env)
    }
}

# TODO the %a% must be put in the environment as well if this simple way should work
testSubstitute2 <- function(ast, env) {
    if (is.symbol(ast)) {
        aName <- as.character(ast)
        if (contains(env, aName))
            env[[aName]]
        else
            ast
    } else if (is.call(ast)) {
        aName <- as.character(ast[[1]])
        if (contains(env, aName))
            ast[[1]] <- as.name(env[[aName]])
        for (i in 2:length(ast))
            ast[[i]] <- testSubstitute2(ast[[i]], env)
        ast
    } else if (is.pairlist(ast)) {
        # for pairlists do not substitute at all
        #for (i in 1: length(ast))
        #    ast[[i]] <- testSubstitute2(ast[[i]], env)
        ast
    } else if (is.double(ast) || is.logical(ast) || is.integer(ast) || is.complex(ast) || is.character(ast)) {
        ast # return the ast with no modification
    } else {
        stop("unknown type ", typeof(ast))
    }
        
}

# Test expansion -------------------------------------------------------------------------------------------------------

# TODO This code is not the pretties and most likely not the best R usage either. I must revisit this when more is done.

#' For given test name, separated headers and its code creates a list of tests that can be generated out of it using the generics in headers. 
expandTest <- function(name, header, code ) {
    ig <- header$independentGenerics
    dg <- header$dependentGenerics
    # now create a vector of max sizes
    iMax <- sapply(seq_len(length(ig)), function(i) { length(ig[[i]]) })
    dMax <- sapply(seq_len(length(dg)), function(i) { length(dg[[i]]) })
    # number of tests is the product of independent generics' sizes
    n <- prod(iMax)
    # create vector of independent and dependent positions
    iPos <- rep(1, length(ig))
    dPos <- rep(1, length(dg))
    # generate the tests in a loop
    for (t in 1:n) {
        env <- list()
        # evaluate the independent generic's values
        for (i in 1:length(ig)) {
            g <- ig[[i]]
            value <- eval(g$expr[[iPos[[i]]]])
            env[[g$name]] <- value
            env[[paste("%",g$name, "%", sep="")]] <- value
        }
        # now evaluate the dependent generic's values, taking the existing environment into account
        for (i in 1:length(dg)) {
            g <- dg[[i]]
            value <- g$expr[[dPos[[i]]]]
            value <- eval(testSubstitute(value,env))
            env[[g$name]] <- value
            env[[paste("%",g$name, "%", sep="")]] <- value
        }
        # TODO do something with checks and conditions !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        # finally substitute the code we have with the generic values
        testCode = testSubstitute(code, env)
        print(testCode)
        # TODO create a test of the the bunch and report it
        
        # increment the generics
        cont <- TRUE
        j <- length(iPos)
        while (cont == TRUE && j >= 1) {
            print(j)
            if (iPos[[j]] < iMax[[j]]) {
                cont <- FALSE
                iPos[[j]] <- iPos[[j]] + 1
            } else {
                iPos[[j]] <- 1
            }
            # now increment all dependent generics if required
            for (dName in ig[[j]]$dependents) {
                dIndex <- which(names(dg) == dName)[[1]] 
                if (dPos[[dIndex]] < dMax[[dIndex]])
                    dPos[[dIndex]] <- dPos[[dIndex]] + 1
                else
                    dPos[[dIndex]] <- 1
            }
            j <- j - 1
        }
    }
}



#' Creates new test in a testfile.
#' 
#' This function creates a new test, that is characterized by at least its name and code. The name must be the first argument given and the code is the last one. Any arguments in between are interpreted as test header commands, which can either be generics (created by a call to generic), or specialized conditions and checks that determine the result of the test exection.
#' 
#' The result of this function is a list of all primitive tests that can be created from the particular headers and code (that is using generics).
#' 
test <- function(name = NULL, ...) {
    # first process the arguments, get the header, code and name part of it 
    header <- eval(substitute(alist(...)))
    if (length(header) < 1)
        stop("At least the name and the code of the test must be supplied. Code is the last argument supplied.")
    code <- header[[length(header)]]
    header <- header[-length(header)]
    # now process the headers to their proper objects, that is evaluate each header
    header <- separateTestHeader(header)
    expandTest(name, header, code)
}


#test(
#    "the name of the test",
#    generic(a,1,2),
#    o(a+1),
#    startup({
#        
#    }),
#    teardown({
#        
#   }),
#    
#{
#    a + 1
#}
#    )




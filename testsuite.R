# Copyright (c) 2013, Purdue University. All rights reserved.
# DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
#
# This code is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License version 2 only, as
# published by the Free Software Foundation.
#
# This code is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# version 2 for more details (a copy is included in the LICENSE file that
# accompanied this code).
#
# You should have received a copy of the GNU General Public License version
# 2 along with this work; if not, write to the Free Software Foundation,
# Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
#
# Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
# or visit www.oracle.com if you need additional information or have any
# questions.



#' Checks that the given function has a proper signature for a generic content function. 
#' 
#' A generic content function takes the index of the value to be returned that must be named i, the generic itself named g, and the environment of the current test called env. The environment is a list containing values of all previously defined generics in the particular test instance. 
#' The function returns true if the given function has three arguments, named i, g and env. False otherwise.  
is.genericContentFunction <- function(f) {
    identical(names(formals(f)), c("i", "g", "env"))
}


#' Creates new custom generic.
#' 
#' Default value for length 
#' 
customGeneric <- function(name, contentFunction, ..., dependsOn = NULL, length = 1) {
    name <- as.character(name)
    if (! is.genericContentFunction(contentFunction))
        stop("Generic content function must take exactly three arguments named i, g and env.")
    result <- eval(substitute(alist(...)))
    # check that only named arguments were supplied to the list
    n <- names(result)
    if (length(n) != length(result))
        stop("Only named arguments can be supplied to the generic object apart from name, length and content function")
    # check that none of the arguments supplied corresponds with reserved values for the generics. Since name, length
    # contentFunction and dependsOn are all listed as arguments to this function clash can only happen for dependents
    if ("dependents" %in% n)
        stop("'dependents' is a reserved generic field and cannot be supplied explicitly")
    # get the name (convert its substitute to character if it is not yet)
    if (typeof(name) == "character")
        result$name <- name
    else
        result$name <- as.character(substitute(name))
    # get the other arguments in the result object
    result$contentFunction <- contentFunction
    if (! identical(dependsOn, NULL)) {
        result$dependsOn <- as.character(dependsOn)
    } else {
        if (missing(length))
            stop("Length must be defined for an independent generic")
    }
    length <- as.integer(length)
    if (length < 1)
        stop("Generic must have length of at least one")
    result$length <- length
    class(result) <- "generic"
    result
}

#' Shorthand for customGeneric function. 
#' 
#' @aliases customGeneric
#' @seealso customGeneric
cg <- customGeneric

#' Creates new generic that is defined by a list of its values.
generic <- function(name, ..., dependsOn = NULL) {
    ex <- eval(substitute(alist(...)))
    # convert name to character if required
    if (typeof(name) != "character")
        name <- as.character(substitute(name))
    # we must do the eval & substitute here so that the actual ASTs supplied to this function will be passed to the
    # underlying custom generic too
    eval(substitute(customGeneric(name, function(i,g,env) { g$expr[[i]] }, length = length(expr), dependsOn = dependsOn, expr = ex), list(ex = ex)))
}

#' A shorthand for simple generic function
g <- generic

is.generic <- function(o) {
    identical(class(o), "generic")
}

is.dependent.generic <- function(g) {
    "dependsOn" %in% names(g)
}

is.independent.generic <- function(g) {
    ! "dependsOn" %in% names(g)
}

length.generic <- function(g) {
    g$length
}

is.dependent <- function(g) {
    UseMethod("is.dependent", g)
}

is.independent <- function(g) {
    UseMethod("is.independent", g)
}














# 
# test suite implementation - tests, test generator, generics, and commands
#

# class name for the objects representing the generics
XGENERIC <- "generic"
XINDEPENDENT_GENERIC <- "independent.generic"
XDEPENDENT_GENERIC <- "dependent.generic"


contains <- function(lst, element) {
    !is.null(lst[[element]])
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
genericX <- function(name, ..., dependsOn = NULL) {
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
length.genericX <- function(g) {
    length(g$expr)
}

#' Returns whether the given object is a generic or not
is.genericX <- function(o) {
    inherits(o, GENERIC)
}

#' Returns true if the object is an independent generic.
#' 
#' Apart from inheriting from generic.independent, independent generics must also lack the field dependsOn (but this is not tested by this method)
is.independentGenericX <- function(o) {
    inherits(o, INDEPENDENT_GENERIC)
}

#' Returns true if the object is a dependent generic. 
#' 
#' Apart from inheriting from generic.dependent, dependent generics must also have the field dependsOn that has the name of the generic it depends on. However, the method only tests the class of the object. 
is.dependentGenericX <- function(o) {
    inherits(o, DEPENDENT_GENERIC)
}

is.checkX <- function(o) {
    FALSE
}

is.conditionX <- function(o) {
    FALSE
}

# header separation ----------------------------------------------------------------------------------------------------

#' Separates the given headers into generics, conditions and checks. 
#' 
#' The header is a list of ASTs for commands passed to the test builder. It evaluates them and decides if they are independent generics, dependent generics, conditions or checks. It creates these lists and returns them in a list. For each dependent generics also adds its name to the dependents field of the independent generic it depends on. This is later used in generic expansion. 
separateTestHeaderX <- function(header) {
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
testSubstituteX <- function(ast, env = list()) {
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
testSubstitute2X <- function(ast, env) {
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
expandTestX <- function(name, header, code ) {
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
testX <- function(name = NULL, ...) {
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




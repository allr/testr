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
customGeneric <- function(name, contentFunction, ..., dependsOn = NA, length = 1) {
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
    if (! identical(dependsOn, NA)) {
        result$dependsOn <- as.character(dependsOn)
    } else {
        result$dependsOn <- NA
        if (missing(length))
            stop("Length must be defined for an independent generic")
    }
    result$dependents <- NA # make sure the value will be there when needed to speed things up
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
generic <- function(name, ..., dependsOn = NA) {
    ex <- eval(substitute(alist(...)))
    # convert name to character if required
    if (typeof(name) != "character")
        name <- as.character(substitute(name))
    # we must do the eval & substitute here so that the actual ASTs supplied to this function will be passed to the
    # underlying custom generic too
    eval(substitute(customGeneric(name, function(i,g,env) { g$expr[[i]] }, length = length(ex), dependsOn = dependsOn, expr = ex), list(ex = ex)))
}

#' A shorthand for simple generic function
g <- generic

is.generic <- function(o) {
    inherits(o, "generic")
}

is.dependent.generic <- function(g) {
    ! is.na(g$dependsOn)
}

is.independent.generic <- function(g) {
    is.na(g$dependsOn)
}

length.generic <- function(g) {
    g$length
}

is.dependent <- function(o) {
    UseMethod("is.dependent", o)
}

is.independent <- function(o) {
    UseMethod("is.independent", o)
}

# TODO This should do something more
check <- function(checkFunction, ...) {
    result <- eval(substitute(alist(...)))
    class(result) <- "check"    
}

# TODO this should do something more
condition <- function(conditionFunction, ...) {
    result <- eval(substitute(alist(...)))
    class(result) <- "condition"    
    
}

is.check <- function(o) {
    inherits(o, "check")
}

is.condition <- function(o) {
    inherits(o, "condition")
}

test <- function(..., name = NA) {
    # convert name to character if required, preserve name null if unnamed
    if (typeof(name) != "character")
        name <- as.character(substitute(name))
    if (length(name) == 0)
        name <- NA
    # get the arguments in dots and separate them as code commands and code (code being the last one)
    commands <- eval(substitute(alist(...)))
    if (length(commands) == 1)
        stop("The test must have at least a code specified.")
    code <- commands[[length(commands)]]
    commands <- commands[-length(commands)]
    # now that we have the commands, make them into generics, checks and conditions
    separatedCommands <- separateCommands(commands)
    # now ennumerate the tests 
    enumerateTests(name, code, separatedCommands)
}

separateCommands <- function(commands) {
    result <- list(
        independentGenerics = list(),
        dependentGenerics = list(),
        checks = list(),
        conditions = list(),
        test = list()
        )
    cmdNames = names(commands)
    for (i in 1:length(commands)) {
        # first check if it is a named argument, in which case its AST will be stored in the test itself
        if (identical(cmdNames[[i]],"")) {
            # TODO the reserved names will change 
            if (cmdNames[[i]] %in% c("independentGenerics", "dependentGenerics", "checks", "conditions", "code", "env"))
                stop("Cannot use reserved name ",cmdNames[[i]]," in test field creation")
            test[[cmdNames[[i]]]] <- commands[[i]]
        } else {
            cmd <- eval(commands[[i]])
            if (is.generic(cmd)) {
                if (cmd$name %in% c(names(result$independentGenerics), names(result$dependentGenerics)))
                    stop("Generic ", cmd$name, " already defined for the test.")
                if (is.independent(cmd)) { # for independent generics, only store them
                    result$independentGenerics[[cmd$name]] <- cmd
                } else { # for dependent generics update the master generic's dependents list
                    result$dependentGenerics[[cmd$name]] <- cmd
                    if (! cmd$dependsOn %in% names(result$independentGenerics))
                        stop("Generic ", cmd$name, " depends on unknown generic ", cmd$dependsOn)
                    # add the dependent generic to the master list and replace it
                    ig <- result$independentGenerics[[cmd$dependsOn]]
                    ig$dependents <- c(ig$dependents, cmd$name)
                    result$independentGenerics[[ig$name]] <- ig
                }
            } else if (is.check(cmd)) {
                result$checks <- c(result$checks, cmd)
            } else if (is.condition(cmd)) {
                result$conditions <- c(result$conditions, cmd)
            } else {
                stop("Only generic, check, or condition can be passed as an argument to a test")
            }
        }
    }
    result
}

enumerateTests <- function(name, code, separatedCommands) {
    increaseGeneric <- function(i = length(ig)) {
        g <- ig[[i]]
        # first increase dependent generics
        if (!is.na(g$dependents)) for (dg in g$dependents)
            if (dPos[[dg$name]] == dMax[[dg$name]])
                dPos[[dg$name]] <<- 1
            else
                dPos[[dg$name]] <<- dPos[[dg$name]] + 1
        # now increase the generic itself and perform recursive increase ifoverflow
        if (iPos[[i]] == iMax[[i]]) {
            iPos[[i]] <<- 1
            if (i > 1)
                increaseGeneric(i-1)
        } else {
            iPos[[i]] <<- iPos[[i]] + 1
        }
    }
    # shorthands
    ig <- separatedCommands$independentGenerics
    dg <- separatedCommands$dependentGenerics
    # determine the number of tests to be produced and max lengths for the independent generics
    iMax <- sapply(seq_len(length(ig)), function(i) { length(ig[[i]]) })
    names(iMax) <- names(ig)
    n <- prod(iMax)
    # determine the max value for dependent generics
    dMax <- sapply(seq_len(length(dg)), function(i) { length(dg[[i]]) })
    names(dMax) <- names(dg)
    # generate generic values vectors
    iPos <- rep(1, length(ig))
    names(iPos) <- names(ig)
    dPos <- rep(1, length(dg))
    names(dPos) <- names(dg)
    # enumerate the n tests and advance the generics 
    for (t in 1:n) {
        # first determine the values of the generics and store them to the environment list
        env <- list()
        for (g in ig)
            env[[g$name]] <- g$contentFunction(iPos[[g$name]], g, env)
        for (g in dg) {
            env[[g$name]] <- g$contentFunction(dPos[[g$name]], g, env)
        }
        # now we have the generic values in place, get the substitued code
        codeStr <- eval(substitute(testSubstitute(code, env), list(code = code)))
        if (typeof(codeStr) != "character") {
            codeStr <- deparse(codeStr)
            if (identical(code[[1]], as.name("{")))
                codeStr <- codeStr[c(-1, -length(codeStr))]
        }
        print(codeStr)
        # create the test
        
        # increase the generics
        increaseGeneric()
    }
}





testSubstitute <- function(code, env) {
    c = substitute(code)
    if (typeof(c) == "character") {
        c <- gsub("%%",as.character(as.raw(0)), c)
        for (n in names(env))
            if (typeof(env[[n]]) == "character")
                c <- gsub(paste("%", n, sep = ""), env[[n]], c)
            else
                c <- gsub(paste("%", n, sep = ""), deparse(env[[n]]), c)
        gsub(as.character(as.raw(0)), "%", c)
    } else {
        # duplicate the env values for the operators as well
        for (n in names(env)) 
            env[[paste("%", n, "%", sep = "")]] <- env[[n]]
        testSubstituteAST(c, env)
    }
}

testSubstituteAST <- function(ast, env) {
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

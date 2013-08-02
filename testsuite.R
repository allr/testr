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

library("testthat")


# Generators -------------------------------------------------------------------------------------------------------------
context("generators")

#' Checks that the given function has a proper signature for a generator content function. 
#' 
#' A generator content function takes the index of the value to be returned that must be named i, the generator itself named g, and the environment of the current test called env. The environment is a list containing values of all previously defined generators in the particular test instance. 
#' The function returns true if the given function has three arguments, named i, g and env. False otherwise.  
is.generatorContentFunction <- function(f) {
    identical(names(formals(f)), c("i", "g", "env"))
}

test_that("is.generatorContentFunction", {
    # non-function is FALSE
    expect_false(is.generatorContentFunction(1))
    # function with different arg count is false
    expect_false(is.generatorContentFunction(function(i, g, env, a, b) { }))
    # function with different arg names is false (permutation)
    expect_false(is.generatorContentFunction(function(g, i, env) { }))
    # correct function is true
    expect_true(is.generatorContentFunction(function(i, g, env) { }))
})

#' Creates new custom generator.
#' 
#' Default value for length 
#' 
customGenerator <- function(name, contentFunction, ..., dependsOn = NULL, length = 1) {
    if (! is.generatorContentFunction(contentFunction))
        stop("Generator content function must take exactly three arguments named i, g and env.")
    result <- eval(substitute(alist(...)))
    # check that only named arguments were supplied to the list
    n <- names(result)
    if (length(n) != length(result) || ("" %in% n))
        stop("Only named arguments can be supplied to the generator object apart from name, length and content function")
    # check that none of the arguments supplied corresponds with reserved values for the generators. Since name, length
    # contentFunction and dependsOn are all listed as arguments to this function clash can only happen for dependents
    if ("dependents" %in% n)
        stop("'dependents' is a reserved generator field and cannot be supplied explicitly")
    # get the name (convert its substitute to character if it is not yet)
    name <- substitute(name)
    if (typeof(name) == "character")
        result$name <- name
    else
        result$name <- as.character(name)
    # get the other arguments in the result object
    result$contentFunction <- contentFunction
    dependsOn <- substitute(dependsOn)
    if (! is.null(dependsOn)) {
        result$dependsOn <- as.character(dependsOn)
    } else {
        result$dependsOn <- NULL
        if (missing(length))
            stop("Length must be defined for an independent generator")
    }
    length <- as.integer(length)
    if (length < 1)
        stop("Generator must have length of at least one")
    result$length <- length
    class(result) <- "generator"
    result
}

test_that("customGenerator name and dependsOn can be both strings and names and will be stringified", {
    g <- customGenerator(a, function(i, g, env) { }, dependsOn = b )
    expect_identical(typeof(g$name), "character")
    expect_identical(typeof(g$dependsOn), "character")
    expect_identical(g$name, "a")
    expect_identical(g$dependsOn, "b")
    g2 <- customGenerator("a", function(i, g, env) { }, dependsOn = "b" )
    expect_identical(typeof(g2$name), "character")
    expect_identical(typeof(g2$dependsOn), "character")
    expect_identical(g2$name, "a")
    expect_identical(g2$dependsOn, "b")
})

test_that("compulsory argument values", {
    expect_error(customGenerator(a))
    expect_error(customGenerator(name = a, length = 2))
    expect_error(customGenerator(a, function(i, g, env) { }))
    expect_error(customGenerator(a, function(i, g, env) { }, 1))
    f <- function(i, g, env) { }
    g <- customGenerator(a, f, length = 10)
    expect_equal(g$name, "a")
    expect_equal(g$length, 10)
    expect_equal(g$contentFunction, f)
})

test_that("optional argument values", {
    g <- customGenerator("a", function(i, g, env) { }, length = 1 )
    expect_true(is.null(g$dependsOn))
    g <- customGenerator(a, function(i, g, env) { }, dependsOn = b)
    expect_equal(g$length, 1)
})

test_that("contentFunction can be supplied and works", {
    content <- c(1,20,3,40,5,6)
    g <- customGenerator(a, function(i, g, env) { content[[i]]}, length = 6)
    expect_equal(g$contentFunction(1, g, list()), content[[1]])
    expect_equal(g$contentFunction(2, g, list()), content[[2]])
    expect_equal(g$contentFunction(3, g, list()), content[[3]])
    expect_equal(g$contentFunction(4, g, list()), content[[4]])
    expect_equal(g$contentFunction(5, g, list()), content[[5]])
    expect_equal(g$contentFunction(6, g, list()), content[[6]])
})

test_that("additional arguments can be supplied and are not evaluated", {
    g <- customGenerator(a, function(i, g, env) { }, length = 10, foo = "foo", bar = "bar", fooBar = c("foo", "bar"))  
    expect_equal(g$foo, "foo")
    expect_equal(g$bar, "bar")
    expect_equal(g$fooBar, substitute(c("foo", "bar")))
})

test_that("additional arguments cannot be dependents", {
    expect_error(customGenerator(a, function(i, g, env) { }, length = 1, dependents = 2))
})

#' Shorthand for customGenerator function. 
#' 
#' @aliases customGenerator
#' @seealso customGenerator
cg <- customGenerator

test_that("cg shorthand for customGenerator works", {
    expect_equal(cg(a, function(i, g, env) { i }, length = 10), customGenerator(a, function(i, g, env) { i }, length = 10))
})

#' Creates new generator that is defined by a list of its values.
generator <- function(name, ..., dependsOn = NULL) {
    ex <- eval(substitute(alist(...)))
    # convert name to character if required
    name <- substitute(name)
    dependsOn <- substitute(dependsOn)
    if (typeof(name) != "character")
        name <- as.character(name)
    if (!is.null(dependsOn))
        dependsOn <- as.character(dependsOn)
    # we must do the eval & substitute here so that the actual ASTs supplied to this function will be passed to the
    # underlying custom generator too
    eval(substitute(customGenerator(name, function(i,g,env) { g$expr[[i]] }, length = length(ex), dependsOn = dependsOn, expr = ex), list(ex = ex, name = name, dependsOn = dependsOn)))
}

test_that("generator propagates name and depends on correctly", {
    g <- generator(a, 1, 2, 3)
    expect_equal(g$name, "a")
    expect_true(is.null(g$dependsOn))
    g <- generator("a", 1, 2, 3, dependsOn = b)
    expect_equal(g$name, "a")
    expect_equal(g$dependsOn, "b")
    g <- generator(g, 1, 2, 3, dependsOn = "b")
    expect_equal(g$name, "g")
    expect_equal(g$dependsOn, "b")
})

test_that("generator sets the length correctly and values are propagated to expr field", {
    g <- generator(a, 1)
    expect_equal(g$length, 1)
    expect_equal(g$expr, list(1))
    g <- generator(a, 1, 2, 3)
    expect_equal(g$length, 3)
    expect_equal(g$expr, list(1, 2, 3))
})

test_that("generator expr is not evaluated", {
    g <- generator(a, 1, c(1, 2), as.name("+"))
    expect_equal(g$length, 3)
    expect_equal(g$expr, list(1, substitute(c(1, 2)), substitute(as.name("+"))))
})

test_that("generator content function returns correct values", {
    g <- generator(a, 1, 2, 3, 4, 5, 6)
    expect_equal(g$contentFunction(1, g, list()), 1)
    expect_equal(g$contentFunction(2, g, list()), 2)
    expect_equal(g$contentFunction(3, g, list()), 3)
    expect_equal(g$contentFunction(4, g, list()), 4)
    expect_equal(g$contentFunction(5, g, list()), 5)
    expect_equal(g$contentFunction(6, g, list()), 6)
})

#' A shorthand for simple generator function
g <- generator

test_that("g as shorthand for generator works", {
    expect_equal(g(a, 1, 2), generator(a, 1, 2))
})

is.generator <- function(o) {
    inherits(o, "generator")
}

test_that("is.generator", {
    expect_false(is.generator(1))
    expect_true(is.generator(g(a, 1, 2, 4)))
    expect_true(is.generator(g(a, 1, 2, dependsOn = b)))
    expect_true(is.generator(customGenerator(a, function(i, g, env) { }, dependsOn = b)))
    expect_true(is.generator(customGenerator(a, function(i, g, env) { }, length = 10)))
})

is.dependent.generator <- function(g) {
    !is.null(g$dependsOn)
}


is.independent.generator <- function(g) {
    is.null(g$dependsOn)
}

is.dependent <- function(o) {
    UseMethod("is.dependent", o)
}

is.independent <- function(o) {
    UseMethod("is.independent", o)
}


test_that("dependent and independent generator distinction works", {
    g <- generator(a, 1, 2, 3)    
    expect_false(is.dependent(g))
    expect_true(is.independent(g))
    g <- customGenerator(a, function(i, g, env) { }, length = 3)
    expect_false(is.dependent(g))
    expect_true(is.independent(g))
    
    g <- generator(a, 1, 2, 3, dependsOn = b)    
    expect_true(is.dependent(g))
    expect_false(is.independent(g))
    g <- customGenerator(a, function(i, g, env) { }, dependsOn = b)
    expect_true(is.dependent(g))
    expect_false(is.independent(g))
})

length.generator <- function(g) {
    g$length
}

test_that("generator length reflects the length described, not the length of the list", {
    g <- generator(a, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    expect_equal(length(g), 10)
    expect_equal(length(names(g)), 4)
    g <- customGenerator(a, function(i, g, env) { }, length = 11)
    expect_equal(length(g), 11)
    expect_equal(length(names(g)), 3)
})


# checks ---------------------------------------------------------------------------------------------------------------

is.check <- function(o) {
    inherits(o, "check")
}

# TODO This should do something more
check <- function(checkFunction, ...) {
    result <- eval(substitute(alist(...)))
    class(result) <- "check"    
}

# conditions -----------------------------------------------------------------------------------------------------------

is.condition <- function(o) {
    inherits(o, "condition")
}

# TODO this should do something more
condition <- function(conditionFunction, ...) {
    result <- eval(substitute(alist(...)))
    class(result) <- "condition"    
}


# substitution ---------------------------------------------------------------------------------------------------------

context("substitution")

testSubstitute <- function(code, env) {
    c = substitute(code)
    if (typeof(c) == "character") {
        c <- gsub("%%",as.character(as.raw(0)), c)
        if (length(env) != 0) for (n in sort(names(env), decreasing = TRUE))
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
        if (aName %in% names(env))
            env[[aName]]
        else
            ast
    } else if (is.call(ast)) {
        aName <- as.character(ast[[1]])
        if (aName %in% names(env))
            ast[[1]] <- as.name(env[[aName]])
        for (i in 2:length(ast))
            ast[[i]] <- testSubstituteAST(ast[[i]], env)
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

test_that("substitution works for strings into strings", {
    expect_equal(
        testSubstitute("a = a", list()),
        "a = a"
    )
    expect_equal(
        testSubstitute("a = a", list(a = "1")),
        "a = a"
    )
    expect_equal(
        testSubstitute("a = %%a", list(a = "1")),
        "a = %a"
    )
    expect_equal(
        testSubstitute("a = %a", list(a = "a+b")),
        "a = a+b"
    )
    expect_equal(
        testSubstitute("%a %%%b", list(a = "1", b = "c(1,2)")),
        "1 %c(1,2)"
    )
    expect_equal( # order does not matter for substitutions of prefixed elements, longest will take precedence
        testSubstitute("a = %aa", list(a = "a", aa = "b")),
        "a = b"
    )
    expect_equal( # unmatched % are left in the string without any warnings
        testSubstitute("a = %a", list()),
        "a = %a"
    )
})

test_that("substitution of ASTs into string", {
    expect_equal(
        testSubstitute("a = %%a", list(a = 1)),
        "a = %a"
    )
    expect_equal(
        testSubstitute("a = %%a", list(a = quote(1))),
        "a = %a"
    )
    expect_equal(
        testSubstitute("a = %a", list(a = quote(a+b))),
        "a = a + b"
    )
    expect_equal(
        testSubstitute("%a %%%b", list(a = 1, b = quote(c(1,2)))),
        "1 %c(1, 2)"
    )
    expect_equal( # order does not matter for substitutions of prefixed elements, longest will take precedence
        testSubstitute("a = %aa", list(a = quote(a), aa = quote(b))),
        "a = b"
    )
})

test_that("substitution of strings and ASTs into string", {
    expect_equal(
        testSubstitute("a %b %a %c", list( a= "+", b = quote(as.name("+")), c="c(1,2)")),
        "a as.name(\"+\") + c(1,2)"
    )
})

test_that("substitution works on ASTs replaced with ASTs", {
    expect_equal(
        testSubstitute(a + b + c %x% d, list()),
        quote(a + b + c %x% d)
    )
    expect_equal(
        testSubstitute(a + b + c %x% d, list(e = 1)),
        quote(a + b + c %x% d)
    )
    expect_equal(
        testSubstitute(a + b + c %x% d, list(a = 1)),
        quote(1 + b + c %x% d)
    )
    expect_equal(
        testSubstitute(a + b + c %x% d, list(a = 1, b = quote(c(1, 2)))),
        quote(1 + c(1, 2) + c %x% d)
    )
    expect_equal(
        testSubstitute(a + b + c %x% d, list(a = 1, b = quote(c(1, 2)), c = quote(5 + 6))),
        quote(1 + c(1, 2) + (5 + 6) %x% d)
    )
})

test_that("substition of operators in ASTs work with characters or names, or anything else too", {
    expect_equal(
        testSubstitute(1 %a% 2, list(a = "*")),
        quote(1 * 2)
    )
    expect_equal(
        testSubstitute(1 %a% 2, list(a = as.name("*"))),
        quote(1 * 2)
    )
})

test_that("substitution of strings into ASTs preserves the string nature", {
    expect_equal(
        testSubstitute(a + b, list(a = "1")),
        quote("1" + b)
    )
    expect_equal(
        testSubstitute(c(a, b), list(a = "c(1, 2)", b = "c(3, 4)")),
        quote(c("c(1, 2)", "c(3, 4)"))
    )
})

test_that("substitution of strings ansd ASTs into ASTs", {
    expect_equal(
        testSubstitute(c(a, b), list(a = "c(1, 2)", b = c(3, 4))),
        quote(c("c(1, 2)", c(3, 4)))
    )
})


# test generator -------------------------------------------------------------------------------------------------------

context("test generator")

test <- function(..., name = NULL) {
    # convert name to character if required, preserve name null if unnamed
    if (typeof(name) != "character")
        name <- as.character(substitute(name))
    if (length(name) == 0)
        name <- NULL
    # get the arguments in dots and separate them as code commands and code (code being the last one)
    commands <- eval(substitute(alist(...)))
    if (length(commands) == 1)
        stop("The test must have at least a code specified.")
    code <- commands[[length(commands)]]
    commands <- commands[-length(commands)]
    # now that we have the commands, make them into generators, checks and conditions
    separatedCommands <- separateCommands(commands)
    # now ennumerate the tests 
    enumerateTests(name, code, separatedCommands)
}

separateCommands <- function(commands) {
    result <- list(
        independentGenerators = list(),
        dependentGenerators = list(),
        checks = list(),
        conditions = list(),
        test = list()
        )
    cmdNames = names(commands)
    for (i in 1:length(commands)) {
        # first check if it is a named argument, in which case its AST will be stored in the test itself
        if (!identical(cmdNames[[i]],"") && !is.null(cmdNames[[i]])) {
            if (cmdNames[[i]] %in% c("name",
                                     "env",
                                     "code",
                                     "originalCode",
                                     "conditions",
                                     "checks",
                                     "independentGenerators",
                                     "dependentGenerators",
                                     "generatorValues"
                                     ))
                stop("Cannot use reserved name ",cmdNames[[i]]," in test field creation")
            test[[cmdNames[[i]]]] <- commands[[i]]
        } else {
            cmd <- eval(commands[[i]])
            if (is.generator(cmd)) {
                if (cmd$name %in% c(names(result$independentGenerators), names(result$dependentGenerators)))
                    stop("Generator ", cmd$name, " already defined for the test.")
                if (is.independent(cmd)) { # for independent generators, only store them
                    result$independentGenerators[[cmd$name]] <- cmd
                } else { # for dependent generators update the master generator's dependents list
                    result$dependentGenerators[[cmd$name]] <- cmd
                    if (! cmd$dependsOn %in% names(result$independentGenerators))
                        stop("Generator ", cmd$name, " depends on unknown generator ", cmd$dependsOn)
                    # add the dependent generator to the master list and replace it
                    ig <- result$independentGenerators[[cmd$dependsOn]]
                    ig$dependents <- c(ig$dependents, cmd$name)
                    result$independentGenerators[[ig$name]] <- ig
                }
            } else if (is.check(cmd)) {
                result$checks <- c(result$checks, cmd)
            } else if (is.condition(cmd)) {
                result$conditions <- c(result$conditions, cmd)
            } else {
                stop("Only generator, check, or condition can be passed as an argument to a test")
            }
        }
    }
    result
}

enumerateTests <- function(name, code, separatedCommands) {
    increaseGenerator <- function(i = length(ig)) {
        g <- ig[[i]]
        # first increase dependent generators
        for (dg in g$dependents)
            if (dPos[[dg$name]] == dMax[[dg$name]])
                dPos[[dg$name]] <<- 1
            else
                dPos[[dg$name]] <<- dPos[[dg$name]] + 1
        # now increase the generator itself and perform recursive increase ifoverflow
        if (iPos[[i]] == iMax[[i]]) {
            iPos[[i]] <<- 1
            if (i > 1)
                increaseGenerator(i-1)
        } else {
            iPos[[i]] <<- iPos[[i]] + 1
        }
    }
    # shorthands
    ig <- separatedCommands$independentGenerators
    dg <- separatedCommands$dependentGenerators
    # determine the number of tests to be produced and max lengths for the independent generators
    iMax <- sapply(seq_len(length(ig)), function(i) { length(ig[[i]]) })
    names(iMax) <- names(ig)
    n <- prod(iMax)
    # determine the max value for dependent generators
    dMax <- sapply(seq_len(length(dg)), function(i) { length(dg[[i]]) })
    names(dMax) <- names(dg)
    # generate generator values vectors
    iPos <- rep(1, length(ig))
    names(iPos) <- names(ig)
    dPos <- rep(1, length(dg))
    names(dPos) <- names(dg)
    # enumerate the n tests and advance the generators 
    tests <- list()
    for (t in 1:n) {
        # first determine the values of the generators and store them to the environment list
        env <- list()
        for (g in ig)
            env[[g$name]] <- g$contentFunction(iPos[[g$name]], g, env)
        for (g in dg) {
            env[[g$name]] <- g$contentFunction(dPos[[g$name]], g, env)
        }
        # now we have the generator values in place, get the substitued code
        codeStr <- eval(substitute(testSubstitute(code, env), list(code = code)))
        if (typeof(codeStr) != "character") {
            codeStr <- deparse(codeStr)
            if (length(code) > 1)
                if (identical(code[[1]], as.name("{")))
                    codeStr <- codeStr[c(-1, -length(codeStr))]
        }
        # create the test
        test <- c(
            list(
                # if changing these, also change the check for reserved names in separateCommands function above
                name = name,
                env = env, 
                code = codeStr, 
                originalCode = code, 
                conditions = separatedCommands$conditions,
                checks = separatedCommands$checks, 
                independentGenerators = separatedCommands$independentGenerators,
                dependentGenerators = separatedCommands$dependentGenerators,
                generatorValues = c(iPos, dPos)
                ),
            separatedCommands$test
        )
        class(test) <- "testInstance"
        tests <- c(tests, list(test))
        # increase the generators
        increaseGenerator()
    }
    tests
}

print.testInstance <- function(t) {
    cat("Test", t$name, "\n")
    cat("Generators:\n")
    print(t$generatorValues)
    cat("Code:\n", t$code, "\n")
}

test_that("Tests are returned in hierarchical list", {
    x = test(name="haha", g(a, 1, 2, 3, 4, 5), a)
    expect_equal(length(x), 5)
})

test_that("Symbol can be substitued", {
    x = test(name="haha", g(a, 1, 2, 3, 4, 5), a)
    expect_equal(x[[1]]$code, "1")
})


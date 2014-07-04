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

context("generators")


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

test_that("cg shorthand for customGenerator works", {
    expect_equal(cg(a, function(i, g, env) { i }, length = 10), customGenerator(a, function(i, g, env) { i }, length = 10))
})

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

test_that("g as shorthand for generator works", {
    expect_equal(g(a, 1, 2), generator(a, 1, 2))
})

test_that("is.generator", {
    expect_false(is.generator(1))
    expect_true(is.generator(g(a, 1, 2, 4)))
    expect_true(is.generator(g(a, 1, 2, dependsOn = b)))
    expect_true(is.generator(customGenerator(a, function(i, g, env) { }, dependsOn = b)))
    expect_true(is.generator(customGenerator(a, function(i, g, env) { }, length = 10)))
})

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

test_that("generator length reflects the length described, not the length of the list", {
    g <- generator(a, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    expect_equal(length(g), 10)
    expect_equal(length(names(g)), 4)
    g <- customGenerator(a, function(i, g, env) { }, length = 11)
    expect_equal(length(g), 11)
    expect_equal(length(names(g)), 3)
})

test_that("generator length reflects the length described, not the length of the list", {
    g <- generator(a, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    expect_equal(length(g), 10)
    expect_equal(length(names(g)), 4)
    g <- customGenerator(a, function(i, g, env) { }, length = 11)
    expect_equal(length(g), 11)
    expect_equal(length(names(g)), 3)
})

# substitution ---------------------------------------------------------------------------------------------------------

context("substitution")

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

test_that("simple test works", {
    tests <<- list()
    test(name = "foobar",
        o = 3,
        1 + 2
    )
    expect_equal(length(tests), 1)
    t <- tests[[1]]
    expect_equal(t$name, "foobar")
    expect_equal(t$o, 3)
    expect_false("w" %in% names(t))
    expect_false("e" %in% names(t))
})

test_that("independent generator works and output may be missing", {
    tests <<- list()
    test(name = "test",
         g(a, 1, 2, 3, 4),
         a
    )
    expect_equal(length(tests), 4)
    t <- tests[[1]]
    expect_equal(t$name, "test [a = 1]")
    expect_equal(t$code, "1")
    expect_false("o" %in% names(t))
    expect_false("w" %in% names(t))
    expect_false("e" %in% names(t))
    t <- tests[[2]]
    expect_equal(t$name, "test [a = 2]")
    expect_equal(t$code, "2")
    expect_false("o" %in% names(t))
    expect_false("w" %in% names(t))
    expect_false("e" %in% names(t))
    t <- tests[[3]]
    expect_equal(t$name, "test [a = 3]")
    expect_equal(t$code, "3")
    expect_false("o" %in% names(t))
    expect_false("w" %in% names(t))
    expect_false("e" %in% names(t))
    t <- tests[[4]]
    expect_equal(t$name, "test [a = 4]")
    expect_equal(t$code, "4")
    expect_false("o" %in% names(t))
    expect_false("w" %in% names(t))
    expect_false("e" %in% names(t))
})

test_that("multiple independent generators", {
    tests <<- list()
    test(name = "test",
         g(a, 1, 2, 3),
         g(b, 1, 2, 3),
         g(c, "+","-","*"),
         a %c% b
    )
    expect_equal(length(tests), 27)
    expect_equal(tests[[1]]$generatorValues, c(a = 1, b = 1, c = 1))
    expect_equal(tests[[2]]$generatorValues, c(a = 1, b = 1, c = 2))
    expect_equal(tests[[3]]$generatorValues, c(a = 1, b = 1, c = 3))
    expect_equal(tests[[4]]$generatorValues, c(a = 1, b = 2, c = 1))
    expect_equal(tests[[8]]$generatorValues, c(a = 1, b = 3, c = 2))
    expect_equal(tests[[11]]$generatorValues, c(a = 2, b = 1, c = 2))
    expect_equal(tests[[11]]$code, "2 - 1")
})

test_that("dependent generator works", {
    tests <<- list()
    test(
        g(a, 1, 2, 3),
        g(b, 3, 2, 1, dependsOn = a),
        a + b
    )
    expect_equal(length(tests), 3)
    expect_equal(tests[[1]]$generatorValues, c(a = 1, b = 1))
    expect_equal(tests[[2]]$generatorValues, c(a = 2, b = 2))
    expect_equal(tests[[3]]$generatorValues, c(a = 3, b = 3))
})

test_that("output if code is executed and generators are replaced", {
    tests <<- list()
    test(
        g(a, 1, 2, 3),
        g(b, 3, 2, 1, dependsOn = a),
        o = a + b,
        a + b
    )
    expect_equal(length(tests), 3)
    expect_equal(tests[[1]]$o, 4)
    expect_equal(tests[[2]]$o, 4)
    expect_equal(tests[[3]]$o, 4)
})

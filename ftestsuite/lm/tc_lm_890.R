
expected <- structure(list(yield = c(49.5, 62.8, 46.8, 57, 59.8, 58.5, 55.5, 
56, 62.8, 55.8, 69.5, 55, 62, 48.8, 45.5, 44.2, 52, 51.5, 49.8, 
48.8, 57.2, 59, 53.2, 56), N = structure(c(1L, 2L, 1L, 2L, 2L, 
2L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 
2L, 1L, 1L), .Label = c("0", "1"), class = "factor"), P = structure(c(2L, 
2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 
1L, 1L, 2L, 1L, 2L, 2L, 1L), .Label = c("0", "1"), class = "factor"), 
    K = structure(c(2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 
    2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 1L), .Label = c("0", 
    "1"), class = "factor")), .Names = c("yield", "N", "P", "K"
), terms = yield ~ N + P + K + N:P + N:K + P:K + N:P:K, row.names = c(NA, 
24L), class = "data.frame")
test(id=1089, code={
require(stats)
lm(data = npk, formula = yield ~ N + P + K + N:P + N:K + P:K + 
    N:P:K, method = "model.frame", singular.ok = TRUE)}, o = expected)
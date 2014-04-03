test(name = "partial matching with exact NA",
  w = "partial match", # or any substring you are expecting to find
  {
    a = list(foo = 1, foolish = 2, bar = 3)
    a[["b", exact = NA]]
  }
)

test(name = "haha", 
  g(a, 1, 2, 3, 4),
  g(b, c(1,2), c(2,3), c(3,4)),
  g(c, "+","-"),
  a + b
)

test( 1 + 2, o = 3)

test(stop("haha"), e = c("Error","haha"))

test(
  g(a, 1, 2, 3),
  g(b, 2, 3, 4, dependsOn = a),
  a + 1,
  o = b
)

library(drake)
f <- function(y=4) {
    2* y
}
drake_plan(
    x = f(4),
    z = x+5
)

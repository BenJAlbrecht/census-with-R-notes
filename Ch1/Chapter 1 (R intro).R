# Analyzing US Census Data
# Chapter 1


# Use as a calculator
2 + 3

# Assignment
x <- 100 * 2

# Object names can be any unquoted letter and number
# so long as the first character is a letter
class(x)

# Example of string class
y <- "census"
class(y)

# data frames
df <- data.frame(
  v1 = c(2, 5, 1, 7, 4),
  v2 = c(10, 2, 4, 2, 1),
  v3 = c("a", "b", "c", "d", "e")
)
df

# functions
multiply <- function(x, y) {
  x * y
}
multiply(232, 7)


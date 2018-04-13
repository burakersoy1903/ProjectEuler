# Problem 1
# Multiples of 3 and 5
# If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
# The sum of these multiples is 23.
# Find the sum of all the multiples of 3 or 5 below 1000.

# My favorite: Approach 3, Approach 4, and Approach 7

# ////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Approach 1

# Create an empty vector of desired length where the results of the division will be stored
# NA is replicated 999 times
x.1 <- rep(NA, 999) 
# For each integer from 1 to 999 (we want numbers below 1000), divide the integer # by either
# 3 or 5 and take the modulus. 
# If the integer, say “i” is completely divisible by either 3 or 5, assign  that value to 
# the ith element of vector x; otherwise assign i’th value of x to equal to zero
# “%%” is the modulus function, “||” is the symbol for the “or” command, 
# x.1[i] calls the ith element of vector x.1
for (i in 1:999){
    if (i %% 3 == 0 || i %% 5 == 0) {x.1[i] <- i} else {x.1[i] <- 0}} 
# Take all the non-zero values of x, i.e. those values for which the integer was perfectly 
# divisible by either 3 or 5 and assign it to a separate vector
# This assigns all the nonzero rows of vector x.1 to a vector y.1
y.1 <- x.1[x.1 != 0]
# Take a summation of these values and this is the desired output.
# “sum” finds the sum of all the elements of vector y.1
sum(y.1)

# Remove all objects except for functions
rm(list = setdiff(ls(), lsf.str()))

# ////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Approach 2

# Take numbers from 1 to 333 and multiply each by 3 to get multiples of 3.
# We only take numbers till 333 because we have to find the sum of multiples of 3 (and 5) 
# that are less than 1000.
x.2 <- 0
for (i in 1:333){x.2[i] <- i*3}
# head shows the first few rows of the object
head(x.2) 
# For multiples of 5, we take numbers till 199 and the last multiple of 5 below 1000 comes to be 995
y.2 <- 0
for (j in 1:199){y.2[j] <- j*5}
head(y.2)
# 15 is a common multiple of 3 and 5, and hence it will get included twice – once when we 
# add the numbers that are multiples of 3 and once when we add numbers that are multiples 
# of 5. So we need to subtract these 15 and its multiples.
z.2 <- 0
for (k in 1:66){z.2[k] <- k*15}
head(z.2)
# Final sum
sum(x.2) + sum(y.2) - sum(z.2)

# Remove all objects except for functions
rm(list = setdiff(ls(), lsf.str()))

# ////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Approach 3

# Generate a regular sequence
num <- seq(1, 999)
# find the multiples of 3
multiples.3 <- num[num %% 3 == 0]
# find the multiples of 5
multiples.5 <- num[num %% 5 == 0]
# Create the variable that holds both multiples of 3 and 5
# There is one issue though — the multiples of 3*5 would be added twice. So, they should be subtracted. 
# The sum = (the sum of multiples of 3) + (the sum of multiples of 5) – (the sum of multiples of 15).
# I use union() here instead of subtraction of multiples of 15.
# union, intersect, setdiff and setequal will discard any duplicated values in the arguments, 
# and they apply as.vector to their arguments (and so in particular coerce factors to character vectors).
num.request <- union(multiples.3, multiples.5)
# Final sum
result <- sum(num.request)
# Let's print it
cat("The sum of all the multiples of 3 or 5 below 1000 is ", result, ".\n", sep="")

# Remove all objects except for functions
rm(list = setdiff(ls(), lsf.str()))

# ////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Approach 4

# Create variable
answer <- 0
# Loop through 1 to 999
# Sum values that are multiples of either 3 or 5 but not both due to or (|)
for (i in 1:999) {
    if (i %% 3 == 0 | i %% 5 == 0) {
        answer <- answer + i}
}
# Let's print it
print(answer)

# Remove all objects except for functions
rm(list = setdiff(ls(), lsf.str()))

# ////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Approach 5

# Create variable, Loop through 1 to 999, and 
# Sum values that are multiples of either 3 or 5 but not both due to or (|)
# in one single line of code. WOW-R!
answer <- sum((1:999)[((1:999) %% 3 == 0) | ((1:999) %% 5 == 0)])
# Let's print it
print(answer)

# Remove all objects except for functions
rm(list = setdiff(ls(), lsf.str()))

# ////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Approach 6

# Create variable, Loop through 1 to 999, and 
# Sum values that are multiples of either 3 or 5 but not both due to or (|)
# in one single line of code.
# Let's use some different R data stuructures and functions instead of math skills. Fancy!
answer <- sum(unique(c(seq(3, 999, 3), seq(5, 999, 5))))
# Let's print it
print(answer)

# Remove all objects except for functions
rm(list = setdiff(ls(), lsf.str()))

# ////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Approach 7

# I do not know why we are timing ourselves, but it is cool to see some time functions
t <- proc.time()
# Same solution from Approach 2
# However, we wrote our own function instead of commands
SumDivBy <- function(n, m) {
    p <- m %/% n * n # Round to multiple of n
    return (p * (1 + (p / n)) / 2)
}
# 15 is a common multiple of 3 and 5, and hence it will get included twice – once when we 
# add the numbers that are multiples of 3 and once when we add numbers that are multiples 
# of 5. So we need to subtract these 15 and its multiples.
answer <- SumDivBy(3, 999) + SumDivBy(5, 999) - SumDivBy(15, 999)
# Let's print it
print(answer)
# Stop timer :D
print(proc.time()-t)

# Remove all objects except for functions
rm(list = setdiff(ls(), lsf.str()))

# ////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Approach 8

# I do not know why we are timing ourselves, but it is cool to see some time functions
t <- proc.time()
# Combined solution from Approach 4 and Approach 8
# Fancy to see that we can assign funtion results to variables
answer <- sum(sapply(1:999, function(x){
    if(x%%5 == 0) return(x)
    if(x%%3 == 0) return(x)
    else return(0)
}
)
)
# Let's print it
print(answer)
# Stop timer :D
print(proc.time()-t)

# Remove all objects except for functions
rm(list = setdiff(ls(), lsf.str()))
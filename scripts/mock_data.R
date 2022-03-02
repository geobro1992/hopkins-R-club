###########################################################
## an introduction to data types and creating mock datasets
###########################################################

#########################################
# VECTORS are objects with dimensions 1xN  
alphabet = c('a', 'b', 'c', 'd', 'e')
numbers = c(1, 2, 3, 4, 5)
numbers2 = factor(c(1, 2, 3, 4, 5))
binary = c(1, 1, 0, 0, 0)
people = c("Anne", "Zebedy", "Mike", "Anne", "Mike")

# simple functions to summarize vectors
mean(numbers)
median(numbers)
summary(numbers)


# SIDENOTE ------------------------------------
# we can create our own functions if none exist
sum(numbers)
length(numbers)

sum(numbers)/length(numbers)

manual.mean = function(x){
  sum(x)/length(x)
}

manual.mean(numbers)
manual.mean(binary)
#-----------------------------------------------


# sorting is trivial if you don't want anything complicated
sort(binary)
sort(people)

sort(binary, decreasing = TRUE)
sort(people, decreasing = TRUE)

?sort


# summarizing text data
summary(people) # doesn't really give us anything useful

table(people)
unique(people)
length(unique(people))


##########################################
# MATRICES are objects with dimensions NxN  
mat1 = rbind(binary, numbers)
mat2 = cbind(binary, numbers)


# check dimensions
dim(mat1)
dim(mat2)


# mean is no longer useful because it applies to all values in the matrix
mean(mat1)
mean(mat2)


# if we want to summarize we need to specify parts of the matrix
mean(mat1[1,]) # mean of just the first row
mean(mat2[1,]) # mean of just the first row
mean(mat2[,1]) # mean of just the first column


# we can combine numbers and words in a matrix, but not particulalry useful
mat3 = rbind(binary, alphabet)
mat4 = cbind(binary, alphabet)


# matrix and vectors are limited because all data within them has to be the same type, so... 


#############
# data frames

# create a dataset manually using base R
df1 <- data.frame(numbers = c(1, 2, 3, 4, 5),
                  alphabet = c('a', 'b', 'c', 'd', 'e'),
                  binary = c(1, 1, 0, 0, 0), 
                  people = c("Anne", "Zebedy", "Mike", "Anne", "Mike"))


# summarizing individual columns in the same way as vectors
table(df1$people)
summary(df1$binary)


# formatting individual columns
class(df1$number)
df1$ID = as.factor(df1$number)
class(df1$number)


# adding individual columns
df1$state = c("VA", "VA", "VA", "VA", "VA")


# some useful base functions to check databases
colnames(df1)
str(df1)
summary(df1)
head(df2)
tail(df2)


# create a dataset with built in functions using base R
letters
LETTERS
month.abb
rep("VA", 12)

1:24
seq(from = 1, to = 24, by = 1)
seq(from = 1, to = 24, by = 2)
seq(from = 1, to = 24, by = 0.5)

seq(from = 23, to = 1, by = -2)
rev(seq(from = 1, to = 24, by = 2))

seq(from = 1, to = 2, length.out = 10)


rep(seq(from = 1, to = 24, by = 2), 2)
sort(rep(seq(from = 1, to = 24, by = 2), 2))


df2 <- data.frame(odds = seq(1,24,by=2), 
                  alphabet = LETTERS[1:12],
                  calendar = month.abb,
                  state = rep("VA", 12))



# add numeric columns by sampling form a distribution using base R
df2$c = sample(1:5, length(df2[,1]), replace = TRUE)     # random samples from a given vector

df2$d = rnorm(length(df2[,1]))                           # random draws from a normal distribution

df2$e = rnorm(length(df2[,1]), mean = 10, sd = 2)        # random draws from a normal distribution with specified mean and sd

df2$f = ifelse(rnorm(length(df2[,1])) < 0, 0, 1)     # using ifelse() to create a binary variable

df2$g = ifelse(df2$d < 0, 0, 1)                      # using ifelse() to create a binary variable



########################################
## lists can be a collection of anything

list1 = list(numbers, binary, mat2, df1, "bob")
list2 = list(numbers, binary, mat2)
# you can create a list by splitting a data frame
list3 <- split(df1, people)


# for loops perform the same action multiple times
for( i in 1:length(list2) ){
  
  mu = manual.mean(list2[[i]])
  
  print(mu)
  
}

# we can perform actions on a certain part of each list element 
for( i in 1:length(list3) ){
  
  mu = manual.mean(list3[[i]]$numbers)
  
  print(mu)
  
}


# we can perform actions on a certain part of each list element 
for( i in 1:length(list3) ){
  
  status = ifelse(sum(list3[[i]]$binary) > 0, "occupied", "unoccupied")

  print(status)
  
}


# for() loops are very useful for quickly iterating over a list, but because R prefers to store everything as a new object with each loop iteration
# loops can become quite slow if they are complex, or running many processes and many iterations. 
# As an alternative lapply() and the apply family of functions more broadly can be used as an alternative to loops. 
# lapply() runs operations on lists of items, similar to the for() loops above. 
# To replicate the previous for() loops, you can run:
lapply(list2, mean)
lapply(list3, function(x) { manual.mean(x$numbers) } )
lapply(list3, function(x) { ifelse(sum(x$binary) > 0, "occupied", "unoccupied") } )



#####################
# now it's your turn!

# 1. make your own mock dataset, aim for something more realistic with 100 rows of data

# 2. make your own function to calculate the mode - you may need google for help

# 3. write your own for loop to calculate the mode for subsets of your data frame

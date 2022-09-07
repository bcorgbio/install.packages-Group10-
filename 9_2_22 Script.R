#Part 1
#1 - Project 1: Weightd Dice
#2 - The Very Basics
100:130
#This is a vector - the colon returns every integer between two integers (creates a sequence of numbers)
#You can cancel any command that is taking too long to run by pressing ctrl+c (it might also take a long time to cancel this command haha)
((3*(17+2))-6)/3
#Now, let's create a die vector
a<-1
a
#So, if you run the above, a returns 1
die<-1:6
die
#Now, we have created a die vector
ls()
#The above function will tell you what object names you have already used
die-1
die/2
die*die
#R uses element-wise execution, so two vectors will get lined up to each other for the operations to be run
1:2
1:4
die
die+1:2
die+1:4
#Since the length of the second vector does not line up with the length of the first one, the operation can not be performed
die%*%die
#Above is called inner multiplication between the vectors
die%o%die
#Above is called outer multiplication between the vectors
round(3.1415)
factorial(3)
mean(1:6)
mean(die)
round(mean(die))
sample(x=1:4,size=2)
#Above gives you a random sample of two numbers between 1 and 4
sample(die,1)
#Above gives you a random sample of one number within die
round(3.1415, digits=2)
#That will give you two digits after the decimal place
#If you don't know the order of the function "thing" use args(function)
args(round)
sample(die, size=2, replace=TRUE)
#Above is called sampling with replacement
dice<-sample(die, size=2, replace=TRUE)
dice
sum(dice)
roll()
#I don't have the roll function, but you can use the function to the right of "dice" to reroll over and over
#Making your own function:
roll<-function(){
  die<-1:6
  dice<-sample(die, size=2, replace=TRUE)
  sum(dice)
}
#Make sure to indent a line between the braces of code
roll()
roll2<-function(bones){
  dice<-sample(bones, size=2, replace=TRUE)
  sum(dice)
}
roll2()
roll2(bones=1:4)
#You can replace bones each time you run the function, which makes this useful for different sizes of die for example
roll2<-function(bones=1:6){
  dice<-sample(bones, size=2, replace=TRUE)
  sum(dice)
}
#You can also just set bones to a default value, like I did above, to prevent getting an error, so the default is regular die size
roll2()
#3 - Packages and Help Pages
library(ggplot2)
qplot
x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
x
y <- x^3
y
qplot(x, y)
#We plotted a quick plot above
x <- c(1, 2, 2, 2, 3, 3)
qplot(x, binwidth = 1)
x2 <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4)
qplot(x2, binwidth = 1)
x3 <- c(0, 1, 1, 2, 2, 2, 3, 3, 4)
replicate(3, 1 + 1)
replicate(10, roll())
rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)
#This is how we go from a small histogram to a larger more useful set of data
?sqrt
#If you ever need help, just type a quesiton mark and then the function you are confused about
roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE, 
                 prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dice)
}
#Above is a rewritten verison of the roll function for a set of weighted dice
rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)
#In the comparison of the histograms, you can clearly see the weighted difference
#Part 2
#4 Project 2: Playing Cards
#5 R Objects
die<-c(1,2,3,4,5,6)
#Above is an atomic vector
is.vector(die)
#Above tests whether something is an atomic vector
length(die)
#Length determines the length of an atomic vector
int<-1L
#A capital L creates an integer vector
text<-"ace"
#Quotation marks creates a character vector
int <- c(1L, 5L)
text <- c("ace", "hearts")
sum(int)
sum(text)
typeof(die)
#Doubles store regular numbers like the die vector
typeof(int)
#Integer vectors store integers or numbers that can be written without a decimal
text<-c("Hello","World")
typeof(text)
#Character vectors store strings of characters
logic<-c(TRUE, FALSE, TRUE)
typeof(logic)
#Logical vectors store true's and false's
comp<-c(1 + 1i, 1 + 2i, 1 + 3i)
typeof(comp)
#Complex vecots store complex numbers
raw(3)
typeof(raw(3))
#Raw vectors store bytes of data
attributes(die)
#Will tell you the type of attributes your vecotr has, NULL for atomic vectors
names(die)
names(die) <- c("one", "two", "three", "four", "five", "six")
names(die)
attributes(die)
die
names(die)<-NULL
dim(die)<-c(2,3)
die
#Above is how you create matrices with your data
m <- matrix(die, nrow = 2)
m
m <- matrix(die, nrow = 2, byrow = TRUE)
m
ar <- array(c(11:14, 21:24, 31:34), dim = c(2, 2, 3))
ar
hand1 <- c("ace", "king", "queen", "jack", "ten", "spades", "spades", 
           "spades", "spades", "spades")

matrix(hand1, nrow = 5)
matrix(hand1, ncol = 2)
dim(hand1) <- c(5, 2)
hand2 <- c("ace", "spades", "king", "spades", "queen", "spades", "jack", 
           "spades", "ten", "spades")

matrix(hand2, nrow = 5, byrow = TRUE)
matrix(hand2, ncol = 2, byrow = TRUE)
now <- Sys.time()
now
unclass(now)
mil <- 1000000
mil
class(mil) <- c("POSIXct", "POSIXt")
mil
gender <- factor(c("male", "female", "female", "male"))
typeof(gender)
as.character(gender)
card <- c("ace", "hearts", 1)
card
sum(c(TRUE, TRUE, FALSE, FALSE))
list1 <- list(100:130, "R", list(TRUE, FALSE))
list1
card <- list("ace", "hearts", 1)
card
df <- data.frame(face = c("ace", "two", "six"),  
                 suit = c("clubs", "clubs", "clubs"), value = c(1, 2, 3))
df
typeof(df)
class(df)
str(df)
df <- data.frame(face = c("ace", "two", "six"),  
                 suit = c("clubs", "clubs", "clubs"), value = c(1, 2, 3),
                 stringsAsFactors = FALSE)
deck <- data.frame(
  face = c("king", "queen", "jack", "ten", "nine", "eight", "seven", "six",
           "five", "four", "three", "two", "ace", "king", "queen", "jack", "ten", 
           "nine", "eight", "seven", "six", "five", "four", "three", "two", "ace", 
           "king", "queen", "jack", "ten", "nine", "eight", "seven", "six", "five", 
           "four", "three", "two", "ace", "king", "queen", "jack", "ten", "nine", 
           "eight", "seven", "six", "five", "four", "three", "two", "ace"),  
  suit = c("spades", "spades", "spades", "spades", "spades", "spades", 
           "spades", "spades", "spades", "spades", "spades", "spades", "spades", 
           "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", 
           "clubs", "clubs", "clubs", "clubs", "clubs", "diamonds", "diamonds", 
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", 
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "hearts", 
           "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", 
           "hearts", "hearts", "hearts", "hearts", "hearts"), 
  value = c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 10, 9, 8, 
            7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 
            10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
)
head(deck)
read.csv("DataForLabs/deck.csv")
head(deck)
write.csv(deck, file = "cards.csv", row.names = FALSE)
head(deck)
deck[1,1]
deck[1, c(1,2,3)]
new<-deck[1, c(1,2,3)]
new
vec<-c(6,1,3,6,10,5)
vec[1:3]
deck[1:2,1:2]
deck[-(2:52),1:3]
deck[c(-1,1), 1]
deck[0,0]
deck[1,]
deck[1,c(TRUE, TRUE, FALSE)]
rows <- c(TRUE, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, 
          F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, 
          F, F, F, F, F, F, F, F, F, F, F, F, F, F)
deck[rows,]
deck[1, c("face", "suit", "value")]
deck[ , "value"]
deal <- function(cards) {
  cards[1,]
}
library(tidyverse)
deal(deck)
deal(deck)
deck2<-deck[1:52,]
head(deck2)
deck3<-deck[c(2,1,3:52),]
head(deck3)
random<-sample(1:52,size=52)
deck4<-deck[random,]
head(deck4)
shuffle <- function(cards) { 
  random <- sample(1:52, size = 52)
  cards[random, ]
}
deal(deck)
deck2 <- shuffle(deck) 
deal(deck2)
#The above function will now output a new card with each shuffle!
deck$value
mean(deck$value)
median(deck$value)
lst <- list(numbers = c(1, 2), logical = TRUE, strings = c("a", "b", "c"))
lst
lst[1]
sum(lst[1])
lst$numbers
sum(lst$numbers)
lst[[1]]
#If you subset a list with single bracket, R will return a smaller list, if you subset with double-bracket, R will return just the values that were inside an element of the list
lst["numbers"]
deckcopy<-deck
vec <- c(0, 0, 0, 0, 0, 0)
vec
vec[1] <- 1000
vec
vec[c(1, 3, 5)] <- c(1, 1, 1)
vec
vec[4:6] <- vec[4:6] + 1
vec
vec[7] <- 0
vec
deckcopy$new <- 1:52
head(deckcopy)
deckcopy$new <- NULL
head(deckcopy)
deckcopy[c(13, 26, 39, 52), ]
deckcopy[c(13, 26, 39, 52), 3]
deckcopy$value[c(13, 26, 39, 52)]
deckcopy$value[c(13, 26, 39, 52)] <- c(14, 14, 14, 14)
deckcopy$value[c(13, 26, 39, 52)] <- 14
head(deckcopy, 13)
head(deck2)
vec
vec[c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)]
a %in% b 
c(1,2) %in% c(1, 3, 5)
#The above function means is a in the function b 
deckcopy$face
#Above we extracts the face column with R's $ notation
deckcopy$face=="ace"
#Now, we can use the == operator to test whether each value is equal to ace
sum(deckcopy$face=="ace")
deck3$face=="ace"
deck3$value[deck3$face=="ace"]
deck3$value[deck3$face=="ace"]<-14
head(deck3)
deck4<-deck
deck4$value<-0
head(deck4,13)
deck4$suit=="hearts"
deck4$value[deck4$suit == "hearts"]
deck4$value[deck4$suit == "hearts"] <- 1
deck4$value[deck4$suit == "hearts"]
deck4[deck4$face == "queen", ]
deck4[deck4$suit == "spades", ]
a <- c(1, 2, 3)
b <- c(1, 2, 3)
c <- c(1, 2, 4)
a == b
b == c
a == b & b == c
deck4$face == "queen" & deck4$suit == "spades"
queenOfSpades <- deck4$face == "queen" & deck4$suit == "spades"
deck4[queenOfSpades, ]
deck4$value[queenOfSpades]
deck4$value[queenOfSpades] <- 13
deck4[queenOfSpades, ]
w <- c(-1, 0, 1)
x <- c(5, 15)
y <- "February"
z <- c("Monday", "Tuesday", "Friday")
w > 0
10 < x & x < 20
y == "February"
all(z %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", 
             "Saturday", "Sunday"))
deck5 <- deck
head(deck5, 13)
facecard <- deck5$face %in% c("king", "queen", "jack")
deck5[facecard, ]
deck5$value[facecard] <- 10
head(deck5, 13)
1 + NA
NA == 1
c(NA, 1:50)
mean(c(NA, 1:50))
mean(c(NA, 1:50), na.rm = TRUE)
NA == NA
c(1, 2, 3, NA) == NA
is.na(NA)
vec <- c(1, 2, 3, NA)
is.na(vec)
deck5$value[deck5$face == "ace"] <- NA
head(deck5, 13)
deal(deck)
as.environment("package:stats")
globalenv()
baseenv()
emptyenv()
parent.env(globalenv())
parent.env(emptyenv())
ls(emptyenv())
ls(globalenv())
head(globalenv()$deck, 3)
assign("new", "Hello Global", envir = globalenv())
globalenv()$new
environment()
new<-"Hello Active"
new
roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}
show_env <- function(){
  list(ran.in = environment(), 
       parent = parent.env(environment()), 
       objects = ls.str(environment()))
}
show_env()
show_env()
environment(show_env)
environment(parenvs)
show_env <- function(){
  a <- 1
  b <- 2
  c <- 3
  list(ran.in = environment(), 
       parent = parent.env(environment()), 
       objects = ls.str(environment()))
}
show_env()
foo <- "take me to your runtime"
show_env <- function(x = foo){
  list(ran.in = environment(), 
       parent = parent.env(environment()), 
       objects = ls.str(environment()))
}
show_env()
deal <- function() {
  deck[1, ]
}
environment(deal)
deal()
DECK <- deck
deck <- deck[-1, ]
head(deck, 3)
deal <- function() {
  card <- deck[1, ]
  deck <- deck[-1, ]
  card
}
deal <- function() {
  card <- deck[1, ]
  assign("deck", deck[-1, ], envir = globalenv())
  card
}
deal()
shuffle <- function(cards) { 
  random <- sample(1:52, size = 52)
  cards[random, ]
}
head(deck, 3)
a <- shuffle(deck)
head(a, 3)
shuffle <- function(){
  random <- sample(1:52, size = 52)
  assign("deck", DECK[random, ], envir = globalenv())
}
shuffle()
deal()
setup <- function(deck) {
  DECK <- deck
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = globalenv())
    card
  }
  
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = globalenv())
  }
}
setup <- function(deck) {
  DECK <- deck
  
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = globalenv())
    card
  }
  
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = globalenv())
  }
  
  list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup(deck)
deal <- cards$deal
shuffle <- cards$shuffle
deal
shuffle
environment(deal)
environment(shuffle)
setup <- function(deck) {
  DECK <- deck
  
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = parent.env(environment()))
    card
  }
  
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = parent.env(environment()))
  }
  
  list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup(deck)
deal <- cards$deal
shuffle <- cards$shuffle
rm(deck)
shuffle()
deal()
deal()
#Project 3: Slot Machine
#11.3 for Loops
for (value in c("My", "first", "for", "loop")) {
  print("one run")
}
for (value in c("My", "second", "for", "loop")) {
  print(value)
}
value
for (word in c("My", "second", "for", "loop")) {
  print(word)
}
for (string in c("My", "second", "for", "loop")) {
  print(string)
}
for (i in c("My", "second", "for", "loop")) {
  print(i)
}
for (value in c("My", "third", "for", "loop")) {
  value
}
chars <- vector(length = 4)
words <- c("My", "fourth", "for", "loop")

for (i in 1:4) {
  chars[i] <- words[i]
}
combos$prize <- NA
head(combos, 3)
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}
head(combos, 3)
sum(combos$prize * combos$prob)
score <- function(symbols) {
  
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  
  # identify case
  # since diamonds are wild, only nondiamonds 
  # matter for three of a kind and all bars
  slots <- symbols[symbols != "DD"]
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B", "BB", "BBB")
  
  # assign prize
  if (diamonds == 3) {
    prize <- 100
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[slots[1]])
  } else if (all(bars)) {
    prize <- 5
  } else if (cherries > 0) {
    # diamonds count as cherries
    # so long as there is one real cherry
    prize <- c(0, 2, 5)[cherries + diamonds + 1]
  } else {
    prize <- 0
  }
  
  # double for each diamond
  prize * 2^diamonds
}
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}
sum(combos$prize * combos$prob)
## ------------------------------------------------------
## TM.R - R Script for Text Mining course.

# Author: Bruno Conte Leite @2019
# brunoconteleite@gmail.com

## ------------------------------------------------------

#install.packages("tm")
#install.packages("pdftools")
#install.packages("textstem")
#install.packages("wordcloud")
#install.packages("foreign")
#install.packages("RColorBrewer")

library(tm)
library(textstem)

getwd() # learn which is the current working directory
setwd("~/Dropbox/Teaching/Course TM-WS/")

## ------------------------------------------------------
## R BASICS FOR TEXT MINING:

# ------
# creating vectors:
1:10
c("a", "b", "c", "abc", "def")
vector <- c("a", "b", "c", "abc", "def")
vector[2]
vector[-2]
vector <- rep(vector, 2)

# creating datasets - data.frame function:
dset <- data.frame(a = 1:10, b = 101:110, words = vector)
# be careful with factors in R!
class(dset$words)
as.numeric(dset$words)

dset <- data.frame(a = 1:10, b = 101:110, words = vector, stringsAsFactors=F)
class(dset$words)
as.numeric(dset$words)

dset[1,2]
dset[1, 'b']

dset[,2]
dset[,'b']
dset$b

# plotting:
plot(dset$a, dset$b)

# appending data:
dset <- rbind(dset, c(3,6, "bruno"))

# creating lists (sets):
dlist <- list(a = c("a", "b", "c", "abc", "def"), b = dset)
dlist[[1]]
dlist$a

# removing elements from environment:

ls() # list of elements
rm(dset)
rm(dlist)
rm(list = ls()) # cleaning environment

# ------
# creating strings:
str <- "text mining"
str

# pasting (concateneting) strings:
paste(str, 'is cool')
paste(str, 'is cool', sep = '')
paste0(str, 'is cool')

# splitting strings:

# by a reference string:
strsplit(str, ' ')

# within subparts of strings:
substr(str,1,4)
nchar(str)
substr(str, 6, nchar(str))

# splitting string vectors:
str <- c(str, 'is not cool')
# see how output is organised as a list:
strsplit(str, ' ')
strsplit(str, ' ')[[1]][2]
strsplit(str, ' ')[[2]][2]

# ------
# finding words in the text:

# using grep(l):
grep('cool', str)
grepl('cool', str)
str[grepl('cool', str)] # elements that contain 'cool'
str[!grepl('cool', str)] # the opposite
sum(grepl('cool', str))

# using %in%:
str %in% 'text'
str %in% 'text mining'

# other functions that are more flexible/versatile:
# regmatches(), gregexpr(), ... 

# ------
# Basic text cleaning:

# finding/removing duplicates:
str <- c(str, rep("text mining", 4), "Bruno, stop teaching it!", "        ECONOMICS")
duplicated(str)
!duplicated(str)
str[!duplicated(str)]
str <- str[!duplicated(str)] # removing duplicates

# some lower case, remove puntuation, remove useless white space:
str <- tolower(str) # to lower caption
str <- removePunctuation(str) # remove punctuation
str <- stripWhitespace(str) # remove exceeding whitespace

# substituting:
str3 <- gsub('bruno', '', str)

# lemmatising/stemmising:
str <- lemmatize_strings(str)
str <- stemDocument(str)

# tokenizing the text (transforming it into a vector of words):
token <- scan_tokenizer(str)

# removing stopwords:
stopwords()
token %in% stopwords()
token[token %in% stopwords()]
token[!token %in% stopwords()]

token <- token[!token %in% stopwords()]

# ------
# How to load texts:
text <- readLines('data/all_recipes.txt')
text[1:50]

# If texts are in pdf format:
library(pdftools)
text.pdf <- pdf_text('data/all_recipes.pdf')
text.pdf <- strsplit(text.pdf, '\n')
text <- ""
for (i in 1:length(text.pdf)) {
  text <- c(text, text.pdf[[i]])
}
# if pdf file has no text recognition: ocr(), writeTiff(), ...

rm(list = ls())

## ------------------------------------------------------
## SOME BASICS OF TEXT MINING:

# How to load texts:
text <- readLines('data/all_recipes.txt')

# Finding patterns to split each recipe. Find the identifier in
# the 'INGREDIENTS' word (note that each text starts 3 lines above
# that word):

grep('INGREDIENTS', text) # rows that contain the word of interest
grepl('INGREDIENTS', text) # rows that contain the word of interest; logical function

text[grep('INGREDIENTS', text)]
text[grepl('INGREDIENTS', text)] # same output
text[grep('INGREDIENTS', text, ignore.case = T)] # watch out for case of the letters!

# indexing start/end of each subtext. I create a dataset for that:
start <- grep('INGREDIENTS', text)-6
end <- grep('INGREDIENTS', text)-8

# dataset with indexes:
indexes <- data.frame(a = start, b = c(end[-1], length(text)))

# writing each recipe individually with write() function:
for (i in 1:nrow(indexes))  {
  write(text[indexes[i,1]:indexes[i,2]], file = paste0('output/recipe_', i, '.txt'))
}

## ------------------------------------------------------
## COOLER TEXT MINING:
## We will create a cross section of recipes with its
## characteristics and a word cloud with the most 
## frequent ingridients.
rm(list = ls())
# Loading the texts and indexes again:
text <- readLines('data/all_recipes.txt')
start <- grep('INGREDIENTS', text)-6
end <- grep('INGREDIENTS', text)-8
indexes <- data.frame(a = start, b = c(end[-1], length(text)))


# ------------
# Dataset of recipes. Suppose we want recipe name, date,
# number of ingredients and how much it has of tomato,
# mozzarella, parmesan and olives.

# First step: create 'empty' and replacement dsets:
dset <- data.frame(date = as.character(NA), ingred = as.numeric(NA), tomato = as.numeric(NA), mozz = as.numeric(NA), parm = as.numeric(NA), olive = as.numeric(NA))
emptydset <- dset[-1,]

for (i in 1:nrow(indexes))	{
	
	txt <- text[indexes[i,1]:indexes[i,2]]
	
	junk <- dset
	junk$date <- strsplit(txt[grep('Date:', txt)], ' ')[[1]][2]
	junk$ingred <- (grep('PREPARATION', txt)-2) - (grep('INGREDIENTS', txt)+2) + 1
	
	# finding ingridients and cleaning (why not clean before?)
	ingred <- txt[(grep('INGREDIENTS', txt)+2):(grep('PREPARATION', txt)-2)]
	ingred <- tolower(ingred)
	ingred <- removePunctuation(ingred)
	ingred <- removeNumbers(ingred)
	ingred <- lemmatize_strings(ingred)
	ingred <- stemDocument(ingred)
	
	junk$tomato <- sum(grepl('tomat', ingred))
	junk$mozz <- sum(grepl('mozzar', ingred))
	junk$parm <- sum(grepl('parmes', ingred))
	junk$olive <- sum(grepl('oliv', ingred))
	
	# appending to empty dset:
	emptydset <- rbind(emptydset, junk)
	rm(junk)
	
}
dset <- emptydset
rm(emptydset)

# exporting it to Stata/csv:
write.table(dset, file = 'output/pizzadata.txt', col.names = T, row.names = F, sep="\t", dec = ".") # csv
library(haven)
write_dta(dset, path = 'output/pizzadata.dta') # stata

# Stupid example - OLS regression (you need to install stargazer and lfe packages):
stargazer::stargazer(
  lfe::felm(
    ingred ~ tomato + mozz + parm + olive, data = dset
    )
  , type = 'text')

# ------------
# Creating a word cloud of most used ingridients. We
# need to create a frequency dataset of words. We will
# do it in the least elegant and most 'intuitive' way.

# To create the frequency table, I will transform each
# text in a token to later aggregate (collapse) by count.

# empty vector to add ingredients into:
ingreds <- ""

# looping over all recipes and getting their vectors
# of ingredients:
for (i in 1:nrow(indexes))	{
	
	# loading and getting ingredients only as before:	
	txt <- text[indexes[i,1]:indexes[i,2]]
	ingred <- txt[(grep('INGREDIENTS', txt)+2):(grep('PREPARATION', txt)-2)]
	# cleaning it as before:
	ingred <- tolower(ingred)
	ingred <- removePunctuation(ingred)
	ingred <- removeNumbers(ingred)
	ingred <- lemmatize_strings(ingred)
	ingred <- stemDocument(ingred)
	# tokenizing and removal of stopwords and duplicates:
	ingred <- scan_tokenizer(ingred)
	ingred <- ingred[!ingred %in% stopwords()]
	ingred <- ingred[!duplicated(ingred)]
	ingred <- ingred[nchar(ingred)>1]
	ingred <- ingred[!ingred %in% ""]
	
	# appending to the ingreds vector:
	ingreds <- c(ingreds, ingred)
	rm(ingred)
	
}
ingreds <- ingreds[-1] # removing first element with empty string.

# Creating a dataset and collapsing it:
word.freq <- data.frame(words = ingreds, count = 1, stringsAsFactors = F)
rm(ingreds)

# collapsing it (this is not the most elegant nor efficient way
# of doing it. See data.table package for more.)
word.freq <- aggregate(list(word.freq$count), list(word.freq$words), sum)
colnames(word.freq) <- c('word', 'freq')

# Create the wordcloud (look at wordcloud package documentation/
# online for more info):

library(wordcloud)
library(RColorBrewer) # for colloring wordclouds

# wordclouds:
wordcloud(words = word.freq$word, freq = word.freq$freq)
wordcloud(words = word.freq$word, freq = word.freq$freq
	, random.order=F) # centering most frequent words
wordcloud(words = word.freq$word, freq = word.freq$freq
	, random.order=F
	, max.words=100)	# setting also a maximum number of words in the cloud
wordcloud(words = word.freq$word, freq = word.freq$freq
	, min.freq=10		# setting minimum frequency instead (put before random order!)
	, random.order=F)
wordcloud(words = word.freq$word, freq = word.freq$freq
	, random.order=F
	, max.words=100
	, colors=brewer.pal(8, "Dark2") )	# adding colors (see RColorBrewer for more color schemes)

# saving it as a .png image:
png("output/wordcloud.png", width=1280,height=800) # open the png 'device'
wordcloud(words = word.freq$word, freq = word.freq$freq
	, random.order=F
	, max.words=100
	, colors=brewer.pal(8, "Dark2") )
dev.off()

## --------------------------------------------------------------------------------
## HANDS-ON EXERCISE: 

# FIRST EXERCISE: Create a dataset with name of pizzas, number of characters in the
# preparation section (proxy for complexity of recipe), and with dummies for whether
# it is necessary to boil, to fry, to cook. Create also a dummy for the existence of
# pitta bread (!) in the recipe. Follow the following steps:

# 1. Create the empty and replacement datasets to fill with the data;
# 2. Loop over the recipes (use the previously indexes object) and match
# 	 the 'ingredients' part of the text;
# 3. Look for/calculate the desired information and store it in the
# 	 replacement dataset;
# 4. Append it to the final dataset; export it to .dta once done.

# SECOND EXERCISE: Create a wordcloud with the 100 most frequent words in the preparation
# section of the recipes. Follow the wordcloud exercise done above, usgin the
# parameter max.words=100.


# SOLUTIONS - FIRST EXERCISE:

# 1. First step: create 'empty' and replacement dsets:
dset <- data.frame(name = as.character(NA), numchars = as.numeric(NA), boil = as.numeric(NA), fry = as.numeric(NA), cook = as.numeric(NA), pitta = as.numeric(NA))
emptydset <- dset[-1,]

# 2. Looping over individual recipes:
for (i in 1:nrow(indexes))	{
	
	txt <- text[indexes[i,1]:indexes[i,2]]
	
	# 3. Matching and storing needed information:
	junk <- dset
	junk$name <- txt[3]
	
	prepar <- txt[(grep('PREPARATION', txt)+2):length(txt)]
	prepar <- tolower(prepar)
	prepar <- removePunctuation(prepar)
	prepar <- removeNumbers(prepar)
	prepar <- lemmatize_strings(prepar)
	prepar <- stemDocument(prepar)
	
	junk$numchars <- sum(nchar(prepar))
	junk$boil <- as.numeric(sum(grepl('boil', prepar))>0)
	junk$fry <- as.numeric(sum(grepl('fry', prepar))>0)
	junk$cook <- as.numeric(sum(grepl('cook', prepar))>0)
	junk$pitta <- as.numeric(sum(grepl('pitta', prepar))>0)
	
	# appending to empty dset:
	emptydset <- rbind(emptydset, junk)
	rm(junk)
	
}
dset <- emptydset
rm(emptydset)

# 4. Exporting it to Stata:
library(foreign)
write.dta(dset, file = 'output/pizzacrazydata.dta', convert.factors = 'string')

# SOLUTION - SECOND EXERCISE:
# empty vector to add preparation text into:
prepars <- ""

# looping over all recipes and getting their vectors
# of ingredients:
for (i in 1:nrow(indexes))	{
	
	# loading and getting ingredients only as before:	
	txt <- text[indexes[i,1]:indexes[i,2]]
	prepar <- txt[(grep('INGREDIENTS', txt)+2):(grep('PREPARATION', txt)-2)]
	# cleaning it as before:
	prepar <- tolower(prepar)
	prepar <- removePunctuation(prepar)
	prepar <- removeNumbers(prepar)
	prepar <- lemmatize_strings(prepar)
	prepar <- stemDocument(prepar)
	# tokenizing and removal of stopwords and duplicates:
	prepar <- scan_tokenizer(prepar)
	prepar <- prepar[! prepar %in% stopwords()]
	prepar <- prepar[!duplicated(prepar)]
	prepar <- prepar[nchar(prepar)>1]
	prepar <- prepar[! prepar %in% ""]
	
	# appending to the ingreds vector:
	prepars <- c(prepars, prepar)
	rm(prepar)
	
}
prepar <- prepars[-1] # removing first element with empty string.

# Creating a dataset and collapsing it:
word.freq <- data.frame(words = prepars, count = 1, stringsAsFactors = F)
rm(prepars)

# collapsing it (this is not the most elegant nor efficient way
# of doing it. See data.table package for more.)
word.freq <- aggregate(list(word.freq$count), list(word.freq$words), sum)
colnames(word.freq) <- c('word', 'freq')

png("output/wordcloud_preparation.png", width=1280,height=800) # open the png 'device'
wordcloud(words = word.freq$word, freq = word.freq$freq
	, random.order=F
	, max.words=100)
dev.off()













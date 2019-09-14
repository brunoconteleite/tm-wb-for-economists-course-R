## ------------------------------------------------------
## WEBSCRAP.R - R Script for Web Scraping course.

# Author: Bruno Conte Leite @2018
# brunoconteleite@gmail.com

## ------------------------------------------------------

install.packages('rvest')
library(rvest)
rm(list = ls()) # clear environment

setwd("~/Dropbox/Teaching/Course TM-WS/")

# Visual inspection of the pages we are interested in scraping:
# https://www.bbcgoodfood.com/search/recipes?query=pizza

# To construct our dataset, we need to get the link to each of the
# pages (already collected):
links <- readLines('data/links.txt')

# Illustration with first link - using R and Google Chrome for scraping:
url <- links[1]

# Loading webpage into R:
web <- read_html(url)

# direct X-Path from Chrome:
nodes <- html_nodes(web, xpath = '//*[@id="node-95122"]/header/div[2]/div[2]/div/section[2]/span')
html_text(nodes)

# Note: html_nodes() allows for many other 'searches', e.g. all tables
# in a page, all images, all forms, ...

## --------------------------------------------------
## FIRST EXERCISE: SCRAPING RECIPES (NAME, INGREDIENTS
## AND METHOD):

# 1. Identify objects of interest (on chrome);
# 2. Test on R whether we match it;
# 3. loop over all links, save each as a recipe file.

# ------
# 1./2.:
# Name object:
node <- html_nodes(web, xpath = '//*[@id="node-95122"]/header/div[2]/div[1]/h1')
html_text(node)
# Ingredient object:
node <- html_nodes(web, xpath = '//*[@id="recipe-ingredients"]/div')
html_text(node)
# alternative (following html structure):
node <- html_nodes(scrap, xpath = '//*[@class="ingredients-list__content"]/ul/li')
html_text(node)
# Similar for method section:
node <- html_nodes(scrap, xpath = '//*[@id="recipe-method"]')
html_text(node)
# following HTML structure:
node <- html_nodes(scrap, xpath = '//*[@id="recipe-method"]/div/ol/li')
html_text(node)

# ------
# 3. Looping over all links for recipes. Let us store them all in one single
# text file. For that, create an empty string vector and append text into it.
# I will separate each part with a tile and each recipe with a '***' line.

text <- ""
for (l in 1:length(links))  {
  
  # loading the page:
  scrap <- read_html(links[l])
  
  # Writing header:
  text <- c(text
            , '****************************************'
            , paste0('Recipe n. ', l, ':')
            , '')
  
  # Getting name of the pizza:
  text <- c(text
                , html_text(html_nodes(scrap, xpath = '//*[@class="recipe-header__title"]'))
                , "") # adding the name of the pizza to the text
  
  # Inserting a 'random' date (for exposition means only):
  text <- c(text
            , paste0("Date: 2013/", sample(month.abb,1, replace = F), '/01')
            , "")

  # Getting ingredients of the pizza:
  text <- c(text
                , "INGREDIENTS"
                , ""
                , html_text(html_nodes(scrap, xpath = '//*[@class="ingredients-list__content"]/ul/li'))
                , "")
  
  # Getting preparation of the pizza:
  text <- c(text
                , "PREPARATION"
                , ""
                , html_text(html_nodes(scrap, xpath = '//*[@class="method"]/ol/li'))
                , "")
  # write(text, file = paste0('data/recipe_', l, '.txt'))
  print(l)
  
}
write(text, file = 'data/all_recipes2.txt')

## --------------------------------------------------
## SECOND EXERCISE: CREATING A DATASET WITH NUTRITIONAL
## INFORMATION FROM EACH PIZZA:

# 1. Setup which data we want to obtain;
# 2. Create the empty and replacement dataset;
# 3. identify and match the objects of interests in R;
# 4. Loop over all links storing the info in the
#    replacement dset, append to the empty dataset;
# 5. Export it in .csv and .dta format.

# --------
# 1./2. Suppose we want a dataset with nutritional info
#       of the pizzas in the website. The empty dataset
#       must contain all the info we want to store later:

dset <- data.frame(name = as.character(NA)
                        , kcal = as.character(NA)
                        , fat = as.character(NA)
                        , saturates = as.character(NA)
                        , carbs = as.character(NA)
                        , sugars = as.character(NA)
                        , fibre= as.character(NA)
                        , protein= as.character(NA)
                        , salt= as.character(NA))
emptydset <- dset[-1,]

# --------
# 3. Matching nutritional data (name was obtained before):

scrap <- read_html(links[1])
node <- html_nodes(scrap, xpath = '//*[@class="nutrition__value"]')
html_text(node) # interestingly, it contains all we want in one vector!

# --------
# 3. Looping over all links; storing in a dataset:
for (l in 1:length(links))  {
  
  # loading the page:
  scrap <- read_html(links[l])
  
  # Creating the dataset with nutritional info:
  temp <- dset # temporary dataset to store the nutritional data
  # getting the data:
  name <- html_text(html_nodes(scrap, xpath = '//*[@class="recipe-header__title"]'))
  nutritional <- html_text(html_nodes(scrap, xpath = '//*[@class="nutrition__value"]'))
  
  # replacing in the temp data:
  temp$name <- name
  temp$kcal <- nutritional[1]
  temp$fat <- nutritional[2]
  temp$saturates <- nutritional[3]
  temp$carbs<- nutritional[4]
  temp$sugars <- nutritional[5]
  temp$fibre <- nutritional[6]
  temp$protein <- nutritional[7]
  temp$salt <- nutritional[8]
  
  # appending to the final dataset:
  emptydset <- rbind(emptydset, temp)
  rm(temp)
  rm(nutritional)
  
  cat("\014") #clear console
  print(emptydset)
  Sys.sleep(2)
  
  # cat(paste0('Retrieving data (', l, '/', length(links), ')...\n'))
  
}
dset <- emptydset
rm(emptydset)

# exporting it as .csv (for matlab, for instance):
write.table(dset, file = 'output/pizzanutritionaldata.txt', col.names = T, row.names = F, sep="\t", dec = ".")


## --------------------------------------------------
## THIRD EXERCISE: FILLING FORMS AND RETRIEVING OUTCOME

# 1. Start a HTML session; Identify form structure
#    and how to fill it out;
# 2. Filling and submiting the form; (demonstrate it in Chrome!)
# 3. Re-scraping outcome web content and store it as a dset;
# 4. If it works, loop over all days in august, saving it 
#    in a convenient way.

# -----
# Suppose we want to obtain the data FedInvest
# (https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate.htm).
# We need though to fill out forms and obtain the outcome table.

# ps: source of this 'tutorial':
# https://stackoverflow.com/questions/27631460/how-can-i-post-a-simple-html-form-in-r

# 1. Identifying the form structure of the page with html_form():

url <- "https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate.htm"

scrap <- html_session(url) # NOTICE DIFFERENCE WITH READ_HTML!!!

# looking for the form:
empty.form <- html_form(scrap)

# 2. Filling and submitting the form. We store the empty one and
# replace it with the desired values. This is tricky and varies a
# lot from website to website.
full.form <- set_values(empty.form[[2]]
                        , priceDate.year=2017
                        , priceDate.month=8
                        , priceDate.day=2) # watch out how data should be put in the form!

# to submit and retrieve the info, open a new scrap session with
# the command submit_form():
scrap2 <- submit_form(scrap, full.form)

# 3. Re-scraping it -- obtaining the table of contents with
#    html_nodes(, "table"):
tables <- html_nodes(scrap2, "table")
# as a data frame:
tables <- html_table(tables)
dset <- data.frame(tables[[1]])


# EXERCISE: CREATE AND EXPORT A UNIQUE DATASET WITH THE
# PRICE DATA FOR JUNE, AUGUST AND OCTOBER OF 2017 AND
# EXPORT IT IN STATA OR .CSV FORMAT.

# Hint: use a loop with the month numbers!

# for (i in c(6,8,9)) {
#   
#   scrap <- html_session(url)
#   
#   form <- set_values(html_form(scrap)[[2]]
#                      , priceDate.year=2017
#                      , priceDate.month=i
#                      , priceDate.day=1)
#   
#   if (i==6) {
#     dset <- data.frame(html_table(submit_form(scrap, form))[[1]])
#     dset$month <- month.name[i]
#   } else  {
#     temp <- data.frame(html_table(submit_form(scrap, form))[[1]])
#     temp$month <- month.name[i]
#     dset <- rbind(dset, temp)
#     rm(temp)
#   }
#   
# }
# 
# # Saving it:
# library(foreign)
# write.dta(dset, file = 'output/fedinvestdata.dta', convert.factors = 'string')


# ## --------------------------------------------------
# ## EXTRA: HOW TO GET THE LIST OF LINKS FROM THE PIZZA
# ## WEBSITE:
# 
url <- "https://www.bbcgoodfood.com/search/recipes?query=pizza"
scrap <- read_html(url)
nodes <- html_nodes(scrap, xpath = '//*[@class="teaser-item__title"]/a')
links <- html_attr(nodes, 'href')
links <- paste0('https://www.bbcgoodfood.com', links)


## --------------------------------------------------
## FORTH EXERCISE: USING API PLATFORMS TO RETRIEVE DATA
## WEB:

# Needed libraries:
library(httr)
#library(rjson)

# 1. Retrieve (geographical) information of a website
# using IP Location Finder (https://tools.keycdn.com/geo, 
# read instructions!):

# Note: this is an illustrational example, the output is 
# not retrieved correctly. This is used as a simple example
# of http requests. Contact me if you want to use this in
# real applications!

# API requests done with GET() function:
request <- GET('https://tools.keycdn.com/geo.json?host={http://barcelonagse.eu}')
# content of the request (do comparison on the internet browser!):
out <- content(request)

# suppose we want the City where the website is stored:
city <- out$data$geo$city

# 2. A nicer (more economics-related) application: exchange
# rates from https://fixer.io/. It requires, though, an API
# access key.

# get a free one at: https://fixer.io/product
a.key <- '49ba9a0f6797ad0fcd3a67c230f69923'

# format of request:
# http://data.fixer.io/api/DATE?access_key=YOURKEY&symbols=RATESWEWANT

# supose we want BRL, USD, CAD and GBP in 2008-01-31.use
# paste0() function together with GET() request function -- it
# helps to looping if one wants to create a time-series:
date <- '2008-01-31'
rates <- 'BRL,USD,CAD,GBP'
request <-GET(paste0(
                'http://data.fixer.io/api/',
                date, 
                '?access_key=', 
                a.key, 
                '&symbols=', 
                rates
                )
              )


## --------------------------------------------------
# Exercise: create a time series of the same exchange rates
# from Jan-2008 to Dec-2012 (monthly).

# 1. Create a vector of dates (hint: use as.character(seq.Date())
# function);
# 2. Loop over it storing it as a dataset with the data.frame
# function:

dates <- as.character(seq.Date(from = as.Date('2008-01-01'), to = as.Date('2012-12-01'), by = 'months'))

for (date in dates) {
  request <-GET(paste0(
    'http://data.fixer.io/api/',
    date,
    '?access_key=',
    a.key,
    '&symbols=',
    rates
  )
  )

  cont <- content(request) # content
  cont <- data.frame(cont$rates)
  cont$date <- as.Date(date)

  if (date==dates[1]) {
    df <- cont
  } else  {
    df <- rbind(df, cont)
  }
  rm(list = c('cont', 'request'))
  Sys.sleep(.3) # to make sure we do not exceed 3 request/second!
}

# bonus: plotting output:

library(data.table)
library(ggplot2)

data <- melt(df, id.var = 'date') # reshaping it (long)
ggplot(data) + geom_line(aes(date, value, colour = variable), size = 1.3) + theme_bw()


















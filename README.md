# Text Mining and Web Scraping for Economics

 Here you find the instructions (and some old material) for the BGSE course of Text Mining and Web Scraping for Economists in R. All the material from previous versions of the course can also be downloaded as a .zip here. Up-to-date version of the material will be sent privately to the students.

 ## About R and RStudio:
 The course is done in R, though the material is also provided in Python for reference only. For the course, you must have R installed in your computer. Optionally, you can install RStudio, which is a nice IDE platform to work with R. Classes are going to be done in both environments so make students familiar with both of them.

 ## Instructions for the course:
 To make sure we have things in place, open R (or Rstudio) and type:
```r
install.packages("tm")
install.packages("pdftools")
install.packages("textstem")
install.packages("wordcloud")
install.packages("foreign")
install.packages("RColorBrewer")
install.packages("rvest")
```
This will download and install the main packages we will need in the course. You might face a prompt window asking you to choose a repository from which download the installation packages. Just choose the one you prefer.

If, when installing a package, you face an error of the type:
```r
“package 'tm' is not available (for R version 3.5.0)”
```
it means that the repository you chose does not have the latest version of the packages. To correct that, change the repository by typing:
```r
setRepositories()
```
I personally use the one from Madrid or A Coruna and never had any problems.

After installation, type:
```r
library(tm)
library(rvest)
```
If you do not face an error message, you are set to go! :) Note: Warning messages, even if coloured in red, are not a problem!

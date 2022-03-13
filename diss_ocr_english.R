###############################################################################################
## Imagined Economics: An Analysis of Non-state Actor Economic Messaging:                    ##
##    Supplementary code                                                                     ##
##                                                                                           ##
## Author: Ayse Lokmanoglu                                                                   ##
##                                                                                           ##
## To Cite:                                                                                  ##
## Lokmanoglu, A. (2021).                                                                    ##
## Imagined Economics: An Analysis of Non-state Actor Economic Messaging                     ##
## [Dissertation]. https://scholarworks.gsu.edu/communication_diss/102                       ##
##                                                                                           ##
##                                                                                           ##
###############################################################################################

options(stringsAsFactors = F)

library(stringr)
library(tm)
library(lubridate)
library(tidytext)
library(dplyr)
library(pkgconfig)
install.packages("pdftools")
install.packages("qpdf")
library(rjson)
library(pdftools)
library(tesseract)
library(tidyr)
library(tidyverse)
library(tm)
library(WriteXLS)
library(csv)
library(tidytext)
library(tidyverse)
#library(textclean)


##change your pdf name and add the page numbers to pages = 1:XX, you can also add file names

rmpng <- pdf_convert("DATA/rumiyah.pdf", format = "png", pages = 1:590, filenames = NULL,
                     dpi = 72, antialias = TRUE, opw = "", upw = "", verbose = TRUE)
tesseract_download("eng")
image_join(rmpng)
image_browse(rmpng)
eng<-tesseract("eng")
textrum <- tesseract::ocr(rmpng, engine = eng)
typeof(textrum)
textrum<-data.frame(textrum)
head(textrum)
length(textrum)
textrum$pagenumber<-seq(1,nrow(textrum))
write.csv(textrum, file = "rumtextraw.csv")

#combinedabandrum
colnames(textrum)[1]<-"text"

#cleandataeng
##create a backup and clean column names
dataEN<-textrum %>%
  janitor::clean_names()

Sys.time()
cleanrum<-textclean::check_text(dataEN$text)
Sys.time()

##clean following the recommendations from text clean
dataEN$text <- tolower(dataEN$text)
####cleaning ones
dataEN$text<-removeNumbers(dataEN$text)
dataEN$text<-removePunctuation(dataEN$text)
dataEN$index<-seq(1,nrow(dataEN))
write.csv(dataEN, file = "engtext.csv")
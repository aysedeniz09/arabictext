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

library(stringi)
library(stringr)
library(qdap)
library(tm)
library(wordcloud)
library(ggplot2)
library(lubridate)
library(mallet)
library(quanteda)
library(ldatuning)
library(topicmodels)
library(xlsx)
library(textcat)
library(parallel)
library(RSQLite)
library(doParallel)
library(scales)
library(tidyr)
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
library(arabicStemR)
library(lmtest)
library(tidytext)
library(tidyverse)
library("rlm")
library("lm.beta")
library("sqliter")
library("caret")
library(car)
library("corrplot")
library(cld2)
library("Hmisc")
library("PerformanceAnalytics")
library(sandwich)
library("tseries")
library("vars")
library("TSA")
library(foreign)
library(stringi)
library(stringr)
library(qdap)
library(tm)
library(wordcloud)
library(ggplot2)
library(lubridate)
library(mallet)
library(quanteda)
library(ldatuning)
library(topicmodels)
library(xlsx)
library(textcat)
library(parallel)
library(RSQLite)
library(doParallel)
library(scales)
library(tidyr)
library(tidytext)
library(dplyr)
library(igraph)
library(foreign)
library(lsa)
library(corpustools)
library(data.table)
library(ggpubr)
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

##cleantext ##my file is labeled as textAR
##change name
dataAR<-textAR
###rename columns to text and date
colnames(dataAR)[1]<-"text"
colnames(dataAR)[5]<-"date"
##download stopwords from google stopwords
stopwords<-readLines("DATA/stop-words_arabic_1_ar.txt")
dataAR$text<-arabicStemR::cleanChars(dataAR$text)
dataAR$text<-arabicStemR::removeNewlineChars(dataAR$text)
dataAR$text<-arabicStemR::removeDiacritics(dataAR$text)
dataAR$text<-arabicStemR::removeNumbers(dataAR$text)
dataAR$text<-arabicStemR::fixAlifs(dataAR$text)
dataAR$text<-arabicStemR::removePunctuation(dataAR$text)

save.image("dis_AR_ADL.rdata")
##descriptive
length(which(dataAR$magazine == "alnaba"))

#startldaAR
####TESTISTHECLEANMAINDATA##DATAISBACKUP
###create an index column
dataAR<-tibble::rowid_to_column(dataAR, "index")

###back up again
dataAR<-testAR

mycorpus<-corpus(testAR)

dfm <- dfm(mycorpus,tolower = FALSE, remove_punct = FALSE,remove_numbers=FALSE,
           remove = stopwords,stem = FALSE,
           remove_separators=TRUE, include_docvars=TRUE) 
dim(dfm)
docnames(dfm)<-dfm@docvars$index
dfm2<-dfm_trim(dfm, max_docfreq = 0.95, min_docfreq=0.0001,docfreq_type="prop")
dtm_lda <- convert(dfm2, to = "topicmodels", docvars = dfm2@docvars)
full_data<-dtm_lda
n<-nrow(full_data)

###############################################################################################
## Russian Twitter Accounts and the Partisan Polarization of Vaccine Discourse, 2015-2017:   ##
##    Supplementary code                                                                     ##
##                                                                                           ##
## Author: Dror Walter                                                                       ##
##                                                                                           ##
## To Cite:                                                                                  ##
## Walter D., Ophir Y. & Hall Jamieson, K. (2020)                                            ##
## Russian Twitter Accounts and the Partisan Polarization of Vaccine Discourse, 2015-2017.   ##
## American Journal of Public Health. http://dx.doi.org/10.2105/AJPH.2019.305564             ##
## First published Online (19 March 2020)                                                    ##
##                                                                                           ##
###############################################################################################

#### Crossfold: ####
print(Sys.time())
MainresultDF<-data.frame(k=c(1),perplexity=c(1),myalpha=c("x"))
MainresultDF<-MainresultDF[-1,]
candidate_alpha<- c(0.01, 0.05, 0.1, 0.2, 0.5) # we choose variaty of alphas
candidate_k <- c(2,5,10,15,20,25,30,35,40,45,50,55,60) # candidates for how many topics
for (eachalpha in candidate_alpha) { 
  print ("now running ALPHA:")
  print (eachalpha)
  print(Sys.time())
  #----------------5-fold cross-validation, different numbers of topics----------------
  cluster <- makeCluster(detectCores(logical = TRUE) - 2) # leave one CPU spare...
  registerDoParallel(cluster)
  
  clusterEvalQ(cluster, {
    library(topicmodels)
  })
  
  folds <- 5
  splitfolds <- sample(1:folds, n, replace = TRUE)
  candidate_k <- c(2, 3, 4, 5, 10, 20, 25, 30, 35, 40, 50, 60, 70, 80, 90, 100) # candidates for how many topics
  
  #clusterExport(cluster, c("full_data", "burnin", "iter", "keep", "splitfolds", "folds", "candidate_k"))
  clusterExport(cluster, c("full_data", "splitfolds", "folds", "candidate_k"))
  
  # we parallelize by the different number of topics.  A processor is allocated a value
  # of k, and does the cross-validation serially.  This is because it is assumed there
  # are more candidate values of k than there are cross-validation folds, hence it
  # will be more efficient to parallelise
  system.time({
    results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
      k <- candidate_k[j]
      print(k)
      results_1k <- matrix(0, nrow = folds, ncol = 2)
      colnames(results_1k) <- c("k", "perplexity")
      for(i in 1:folds){
        train_set <- full_data[splitfolds != i , ]
        valid_set <- full_data[splitfolds == i, ]
        
        fitted <- LDA(train_set, k = k, method = "Gibbs",
                      #control = list(alpha=eachalpha/k,burnin = burnin, iter = iter, keep = keep) )
                      control = list(alpha=eachalpha) )
        
        results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
      }
      return(results_1k)
    }
  })
  stopCluster(cluster)
  
  results_df <- as.data.frame(results)
  results_df$myalpha<-as.character(eachalpha)
  MainresultDF<-rbind(MainresultDF,results_df)
}
save.image("dis_AR_ADLfindk.rdata")
print ("DONE!!!")
print(Sys.time())

MainresultDF$kalpha=paste0(as.character(MainresultDF$k),MainresultDF$myalpha) 
ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.5)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.2)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.1)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.05)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.01)]),linetype = "dotted")

ggplot(MainresultDF)+geom_line(aes(x=k, y=mean(perplexity),color=myalpha))
ggplot(MainresultDF)+geom_smooth(se = FALSE, aes(x=k, y=perplexity,color=myalpha))

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  geom_smooth(se = TRUE, aes(x=k, y=perplexity,color=myalpha))


p<-ggplot(MainresultDF, aes(x = k, y = perplexity))
Alpha<-MainresultDF$myalpha
p+geom_point(aes(color=Alpha),size=0.1)+geom_smooth(se = FALSE, aes(color=Alpha))+
  ggtitle("5-fold cross-validation of topic modelling (5% of data)",
          "(ie five different models fit for each candidate number of topics)") +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  scale_color_discrete(name = "Alpha Levels")+
  xlab("K (Number of Topics)")+
  ylab("Perplexity")
# Identify 2nd derivative max point on perplexity  
MainresultDF_MYALPHA<-MainresultDF[MainresultDF$myalpha==0.01,]
cars.spl <- with(MainresultDF_MYALPHA, smooth.spline(k, perplexity, df = 3))
plot(with(cars, predict(cars.spl, x = MainresultDF_MYALPHA$k, deriv = 2)), type = "l")
abline(v=25)
save.image("disADL_findk_AR.rdata")

####LDA
lda <- LDA(dtm_lda,25,
           method = "Gibbs",
           control = list(alpha=0.05,seed=125231)) 
LDAfit<-lda
datacolnum=1
topterms<-data.frame(terms(lda, 25))
extract_topic_xls<-function (eachLDA) {
  LDAfit<-eachLDA}
mybeta<-data.frame(LDAfit@beta)
colnames(mybeta)<-LDAfit@terms
mybeta<-t(mybeta)
colnames(mybeta)<-seq(1:ncol(mybeta))
mybeta=exp(mybeta)
nwords=50
topwords <- mybeta[1:nwords,]
for (i in 1:LDAfit@k) {
  tempframe <- mybeta[order(-mybeta[,i]),]
  tempframe <- tempframe[1:nwords,]
  tempvec<-as.vector(rownames(tempframe))
  topwords[,i]<-tempvec} 
rownames(topwords)<-c(1:nwords)
kalpha<-paste0(as.character(LDAfit@k),"_",gsub("\\.","",as.character(LDAfit@alpha)))
write.csv(topwords, "Arabictopwords 2.27.21.csv")
save.image("dis_AR_ADL.rdata")
#### Get Frex (unique) words
#### get the beta
mybeta<-data.frame(LDAfit@beta)
colnames(mybeta)<-LDAfit@terms
mybeta<-t(mybeta)
colnames(mybeta)<-seq(1:ncol(mybeta))
mybeta=exp(mybeta)
#### apply FREX formula below
myw=0.3
word_beta_sums<-rowSums(mybeta)
my_beta_for_frex<-mybeta
for (m in 1:ncol(my_beta_for_frex)) {
  for (n in 1:nrow(my_beta_for_frex)) {
    my_beta_for_frex[n,m]<-1/(myw/(my_beta_for_frex[n,m]/word_beta_sums[n])+((1-myw)/my_beta_for_frex[n,m]))
  }
  print (m)
}
nwords=50
topwords <- my_beta_for_frex[1:nwords,]
for (i in 1:LDAfit@k) {
  tempframe <- my_beta_for_frex[order(-my_beta_for_frex[,i]),]
  tempframe <- tempframe[1:nwords,]
  tempvec<-as.vector(rownames(tempframe))
  topwords[,i]<-tempvec
}
rownames(topwords)<-c(1:nwords)
kalpha<-paste0(as.character(LDAfit@k),"_",gsub("\\.","",as.character(LDAfit@alpha)))
write.csv(topwords, "ArabictopFREX 2.27.21.csv")
##toptexs
length(lda@documents)
length(dfm@Dimnames$docs)
metadf<-testAR
meta_theta_df<-cbind(metadf[,"text"],LDAfit@gamma)
ntext=30
toptexts <- mybeta[1:ntext,]
for (i in 1:LDAfit@k) {
  print(i)
  tempframe <- meta_theta_df[order(-as.numeric(meta_theta_df[,i+1])),]
  tempframe <- tempframe[1:ntext,]
  tempvec<-as.vector(tempframe[,1])
  toptexts[,i]<-tempvec
}
rownames(toptexts)<-c(1:ntext)
toptexts<-data.frame(toptexts)
WriteXLS(toptexts, ExcelFileName = "ArabicTopTexts 2.27.21.xlsx", SheetNames = NULL, perl = "perl",
         verbose = FALSE, Encoding = c("UTF-8", "latin1", "cp1252"),
         row.names = TRUE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = FALSE, BoldHeaderRow = FALSE,
         na = "",
         FreezeRow = 0, FreezeCol = 0,
         envir = parent.frame())

##toptextlocation
meta_theta_df2<-cbind(metadf[,"location"],LDAfit@gamma)
ntext=100
toptextspage <- mybeta[1:ntext,]
for (i in 1:LDAfit@k) {
  print(i)
  tempframe <- meta_theta_df2[order(-as.numeric(meta_theta_df2[,i+1])),]
  tempframe <- tempframe[1:ntext,]
  tempvec<-as.vector(tempframe[,1])
  toptextspage[,i]<-tempvec
}
rownames(toptextspage)<-c(1:ntext)
toptextspage<-data.frame(toptextspage)
WriteXLS(toptextspage, ExcelFileName = "Dis Arabic Top Text Location 4.23.21.xlsx", SheetNames = NULL, perl = "perl",
         verbose = FALSE, Encoding = c("UTF-8", "latin1", "cp1252"),
         row.names = TRUE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = FALSE, BoldHeaderRow = FALSE,
         na = "",
         FreezeRow = 0, FreezeCol = 0,
         envir = parent.frame())
save.image("dis_AR_ADL.rdata")

##topicnames
mynames<-c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 
           'X11', 'X12', 'X13', 'X14', 'X15', 'X16', 'X17', 'X18',
           'X19', 'X20')
mynames<-Topic_Names_AN$short_name

save.image("DisADL.rdata")


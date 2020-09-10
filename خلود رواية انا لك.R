# libraries 
library(tidyverse)
library(skimr)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(readxl)
library(lubridate)
library(tm)
library(wordcloud2)
library(arabicStemR)
library(wordcloud)

# reading the dataset :
df <- read_excel("/Users/dell/Desktop/bayan-proj/SaudiNovelsMetadata.xlsx",sheet = 1)
df

# discovering the dataset ------------------------------------------
str(df)
skim(df)
head(df)
dim(df)
colnames(df)

#repleace the value
df$Author[which(is.na(df$Author))] <-"undetermaint"
df$`Parts, if available`[which(is.na(df$`Parts, if available`))] <-0
df$Year[which(is.na(df$Year))] <-"undetermaint"

#  checks if any of columns in the dataset have null values - should print False ----------
any((is.na(df)))


##Duplicates
#print number of duplicates in the dataset 
sum(duplicated(df))
# Show unique repeat entries
unique(df[duplicated(df),])
# Original data with repeats removed.
df<-df[!duplicated(df),]

#-----
#-----
#----------------------
#The number of novels during the year:
df %>% 
  filter(df$Year !="undetermaint")%>%
  unnest(Year) %>%
  count(Year, sort = TRUE) %>%
  ggplot( aes(x=reorder(Year,-n), y=n)) + 
  geom_bar(stat = "identity")+ 
  labs(x="Year", y="count") + 
  ggtitle("The number of novels during the year ")+ coord_flip()+theme_classic()
#-----
#-----
#----------------------

df %>% 
  filter(df$Author !="undetermaint")%>%
  unnest(Author) %>%
  count(Author, sort = TRUE) %>%
  top_n(n = 10, n)%>%
  ggplot( aes(x=reorder(Author,-n), y=n)) + 
  geom_bar(stat = "identity")+ 
  labs(x="Author", y="count") + 
  ggtitle("Top 10 authors for novels ")+ coord_flip()+ theme_classic()



#All novels written by the author "»ﬁ«Ì« ‘ « " : 
s<-df %>% 
  filter(df$Author =="»ﬁ«Ì« ‘ « ")
s

#--------------------------------------
#---------------------------------------

#Read story "«‰« ·ﬂ ":
arabic_text<-readLines("rr.txt",encoding = "UTF-8",warn = F)
arabic_text

arabic_text=arabic_text[arabic_text!=""]
arabic_text

##start Cleaning :

arabic_text<-removePunctuation(arabic_text)
arabic_text<-removeNumbers(arabic_text)
arabic_text<-removeNewlineChars(arabic_text)
arabic_text<-stripWhitespace(arabic_text)
arabic_text<-fixAlifs(arabic_text)
arabic_text<-cleanChars(arabic_text)
arabic_text<-cleanLatinChars(arabic_text)
arabic_text<-removeArabicNumbers(arabic_text)
arabic_text<-removeDiacritics(arabic_text)
arabic_text<-removeFarsiNumbers(arabic_text)



normalize_arabic <- function(x) {
  arabic_text <- x
  arabic_text <- gsub("\\p{P}", " ", arabic_text, perl = TRUE) # Remove punctuation
  # Remove leading whitespace, remove extra spaces, remove non-letter, non-space characters
  arabic_text <- gsub('^ ', '', stripWhitespace(gsub('[^\\p{L}\\p{Zs}]', '', arabic_text, perl = TRUE)))
  arabic_text <- stripWhitespace(gsub('\\x{0623}|\\x{0622}|\\x{0625}|\\x{0671}|\\x{0672}|\\x{0673}', '«', text_temp)) 
  # Normalize alefs with hamzas
  arabic_text <- gsub('\\p{Zs}\\x{0648}*\\x{0627}\\x{0644}(?=\\p{L})', ' ', arabic_text, perl = TRUE) 
  # Remove leading alef lam with optional leading waw
  arabic_text <- gsub('^\\x{0627}\\x{0644}(?=\\p{L})', '', arabic_text, perl = TRUE) 
  # Remove leading alef lam at start of string
  arabic_text <- gsub('\\p{Zs}\\x{0648}*\\x{0644}{2,}(?=\\p{L})', ' ', arabic_text, perl = TRUE) 
  # Remove leading double lam at start of string
  arabic_text <- gsub('\\p{Zs}\\x{0648}*\\x{0643}\\x{0627}\\x{0644}(?=\\p{L})', ' ', arabic_text, perl = TRUE) 
  # Remove leading kaf alef lam with optional waw
  arabic_text <- gsub('\\p{Zs}\\x{0648}*\\x{0628}\\x{0627}\\x{0644}(?=\\p{L})', ' ', arabic_text, perl = TRUE) 
  # Remove leading baa alef lam with optional waw
  arabic_text <- gsub('\\p{Zs}\\x{0648}*\\x{0641}\\x{0627}\\x{0644}(?=\\p{L})', ' ', arabic_text, perl = TRUE) 
  # Remove leading faa alef lam with optional waw
  arabic_text <- gsub('\\p{Zs}\\x{0648}*\\x{0627}{2,}\\x{0644}*(?=\\p{L})', ' ', arabic_text, perl = TRUE) 
  # Remove leading double alef with optional lam with optional leading waw
  arabic_text <- gsub('(?<=\\p{L})\\x{0647}(?=\\p{Zs})', ' ', arabic_text, perl = TRUE) 
  # Remove trailing haa
  arabic_text <- gsub('(?<=\\p{L})\\x{0649}(?=\\p{Zs})', 'Ì', arabic_text, perl = TRUE) 
  # Normalize ending yeh
  arabic_text <- gsub('(?<=\\p{L})\\x{064A}{2,}\\x{0646}(?=\\p{Zs})', '', arabic_text, perl = TRUE) 
  # Remove trailing yeh yeh noon
  arabic_text <- gsub('(?<=\\p{L})\\x{064A}\\x{0648}\\x{0646}(?=\\p{Zs})', '', arabic_text, perl = TRUE) 
  # Remove trailing yeh waw noon
  arabic_text <- gsub('(?<=\\p{L})\\x{0647}\\x{0647}*(?=\\p{Zs})', '', arabic_text, perl = TRUE) 
  # Remove trailing haa or haa alef
  arabic_text <- gsub('(?<=\\p{L})\\x{0647}\\x{0645}\\x{0627}*(?=\\p{Zs})', '', arabic_text, perl = TRUE) 
  # Remove trailing haa meem and haa meem alef
  arabic_text <- gsub('(?<=\\p{Zs})\\p{L}(?=\\p{Zs})', '', arabic_text, perl = TRUE) 
  # Remove single letters such as waw and those produced by above normalization
  arabic_text <- stripWhitespace(gsub('(\\p{Zs}$)|(^\\p{Zs})', '', arabic_text, perl = TRUE)) 
  # Remove added, leading, trailing whitespace
  return(arabic_text)
}

arabic_text<-gsub("∆","Ì" ,arabic_text)
arabic_text<-gsub("ƒ","Ê", arabic_text)
arabic_text<-gsub("…","Â" ,arabic_text)


#dataframe for text:
myartxt1<- data.frame(arabic_text,stringsAsFactors = F)
myartxt1$doc_id<-c(1:nrow(myartxt1))
myartxt1$text= myartxt1$arabic_text

#detet empty rows:
myartxt1=myartxt1[,c(2,3,1)]
myartxt1$text=transliterate(myartxt1$text)

#creat arabic text corpus :
str(myartxt1)

myartxt1Corpus = Corpus(VectorSource(myartxt1$arabic_text))

inspect(myartxt1Corpus[1:5])
qTerms = DocumentTermMatrix(myartxt1Corpus)
qTerms
inspect(qTerms[1:7,1000:1005])
findFreqTerms(qTerms,100)

freq = sort(colSums(as.matrix(qTerms)),decreasing = T)
head(freq, 10)
wf = data.frame(word=names(freq), freq=freq)
library(ggplot2)
wfplot = subset(wf,freq>300)
ggplot(wfplot, aes(word, freq)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45, hjust = 1))

#wfplot1 = subset(wf,freq<90)
#ggplot(wfplot1, aes(word, freq)) +
#  geom_bar(stat="identity")+
#  theme(axis.text.x=element_text(angle=45, hjust = 1))

#Plot words that occur at least 50 times.
set.seed(114)
wordcloud(names(freq), freq, min.freq=50, scale=c(5,.5),colors=brewer.pal(6,"Dark2"), rot.per=0.2)


set.seed(142)
wordcloud(names(freq), freq, min.freq = 50,colors=brewer.pal(8, "Set2"),random.order=FALSE)

##Word Length
#We would like to know more about the word length in the story.
#First lets get all words in a data frame "words" and word length in "wLen"
words = as.matrix(colnames(qTerms))
wLen = data.frame(nletters=nchar(words))
ggplot(wLen, aes(x=nletters))+
  geom_histogram(binwidth=1) +
  geom_vline(xintercept=mean(nchar(words)),
             colour="green", size=1, alpha=.5)+
  labs(x="Number of Letters", y="Number of Words")
#This shows that on average word sizes are close to (5 - 6) letters





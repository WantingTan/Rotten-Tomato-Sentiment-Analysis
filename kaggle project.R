library(readr)
library(dplyr)
library(tidytext)
library(pander)

train <- read_tsv("C:/Users/Wanti/OneDrive/Desktop/kaggle project/data/train.tsv")
head(train)
#-----------------------------------------
#number of comments across categories
distrib_plot<-train %>% 
  ggplot(aes(Sentiment)) + 
  geom_histogram(binwidth=0.5,color="black",fill="orange",alpha=0.4)+
  ggtitle("Comment Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) 

distrib_plot

#------- Naive Method -------------------------
#only keep longest sentence of each SentenceID
train_data=NULL
for ( i in 1:max(train$SentenceId)){
  a<-train[which(train$SentenceId==i),]
  b<-a[which(length(a$Phrase)==max(length(a$Phrase))),]
  train_data<-rbind(train_data,b)
}
head(train_data)

# get score for each complete sentence
contribution <- function(SC) {
  contributions <- SC %>%
    unnest_tokens(word, Phrase) %>%
    count(word,sort = TRUE) %>%
    ungroup() %>%
    
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(), contribution = sum(score))
  
  con_table<-contributions %>%
    top_n(20, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    head(8) 
  
  pander(con_table)
}

contribution(train_data)

calculate_sentiment <- function(review_text)
{
  sentiment_lines  =  review_text %>%  # considering only English text
    unnest_tokens(word, Phrase) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(PhraseId,SentenceId) %>%
    summarize(sentiment = mean(score),words = n()) %>%
    ungroup()
  
  return(sentiment_lines)
}

sentiment_lines = calculate_sentiment(train_data)
pander(sentiment_lines[1:10,])

# join two tables together
sum_table<-merge(x=train_data,y=sentiment_lines,by="PhraseId")
head(sum_table)
dim(sum_table) #6304 vs 8544

# print out sentences don't have score
sent_noscore<-anti_join(train_data,sentiment_lines)
dim(sent_noscore) #2240
head(sent_noscore$Phrase)
# check whether they don't have score
score_noscore<-calculate_sentiment(sent_noscore)
head(score_noscore)

##-------------------------------------------------------------
# using score to classify them into 5 types
x<-sum_table$sentiment
ygroup<-sum_table$Sentiment
mydata=cbind(x,ygroup)
mydata=data.frame(mydata)
#plot
par(mfrow=c(1,2))
hist(mydata$x)
hist(mydata$ygroup)

#plot
mydata1=mydata
colnames(mydata1)=c("x","customer rating")
score_plot<-mydata1 %>% 
  ggplot(aes(x)) + 
  geom_density(aes(group=`customer rating`,colour=`customer rating`,fill=`customer rating`), alpha=0.3)+
  ggtitle("Performance of Sentiment Score") +
  xlab("Sentiment Score")+
  theme(plot.title = element_text(hjust = 0.5)) 
score_plot

mydata$ygroup=factor(mydata$ygroup)

#decision tree
library(rpart)
set.seed(12)
tree4 <- rpart(ygroup ~., data = mydata, method="class", na.action = na.rpart)
summary(tree4)
plotcp(tree4)
min_cp=tree4$cptable[which.min(tree4$cptable[,"xerror"]),"CP"]
tree4<-prune(tree4,cp=min_cp)

library(rpart.plot)
prp(tree4)

tree4.pred.train<- predict(tree4, newdata = mydata, type = 'class')
table(tree4.pred.train, mydata$ygroup)
train.err.treel4<-sum(tree4.pred.train!=train$class)/length(tree4.pred.train)
train.err.treel4


#KNN
library(caret)
set.seed(12)
trControl<-trainControl(method="cv",number=5)
knn.fit<-train(ygroup~.,
               method="knn",
               tuneGrid=expand.grid(k=1:10),
               trControl=trControl,
               data=mydata)
knn.fit

#----------------------------------------------------------------------
# clean data
library(tm)
for (i in 1:length(train)){
  train_corpus=Corpus(VectorSource(as.vector()))
}
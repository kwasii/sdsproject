library(syuzhet)
library(vader)
library(modeest)

## load the csv files
#gt_tweets <- read.csv("hand_annotated.csv", stringsAsFactors = F)
gt_tweets <- read.csv("hand_annotated_new.csv", stringsAsFactors = F)

## get average ratings
gt_tweets$mean <- rowMeans(subset(gt_tweets, select = c(daniel, priyanka, florian, dario)))


#########################################################################################
#########################################################################################





## convert each user's rating into class
gt_tweets$daniel_c <- "Neutral"
gt_tweets$daniel_c[gt_tweets$daniel< -0.5] <- "Negative"
gt_tweets$daniel_c[gt_tweets$daniel> 0.5] <- "Positive"

gt_tweets$priya_c <- "Neutral"
gt_tweets$priya_c[gt_tweets$priyanka< -0.5] <- "Negative"
gt_tweets$priya_c[gt_tweets$priyanka> 0.5] <- "Positive"

gt_tweets$florian_c <- "Neutral"
gt_tweets$florian_c[gt_tweets$florian< -0.5] <- "Negative"
gt_tweets$florian_c[gt_tweets$florian> 0.5] <- "Positive"

gt_tweets$dario_c <- "Neutral"
gt_tweets$dario_c[gt_tweets$dario< -0.5] <- "Negative"
gt_tweets$dario_c[gt_tweets$dario> 0.5] <- "Positive"


##get the majority among votes
v = data.frame(gt_tweets$daniel_c, gt_tweets$priya_c, gt_tweets$florian_c, gt_tweets$dario_c)
temp_c = data.frame(apply(v,1,function(x) names(which.max(table(x)))))
colnames(temp_c) <- "mode_c"
gt_tweets = cbind(gt_tweets, temp_c)


## vader and syuzhet on tweets
biden_vader = vader_df(gt_tweets$text)
gt_tweets$VADER <- biden_vader$compound

gt_tweets$Syuzhet <- get_sentiment(gt_tweets$text) 


## classification on vader, syuzhet and manually annotated.
## Convert the compound score into classes by using -0.5 and 0.5 as thresholds.
gt_tweets$VADERclass <- "Neutral"
gt_tweets$VADERclass[gt_tweets$VADER< -0.5] <- "Negative"
gt_tweets$VADERclass[gt_tweets$VADER> 0.5] <- "Positive"

## compute classes from Syuzhet using -0.5 and 0.5 as thresholds.
gt_tweets$Syuzhetclass <- "Neutral"
gt_tweets$Syuzhetclass[gt_tweets$Syuzhet< -0.5] <- "Negative"
gt_tweets$Syuzhetclass[gt_tweets$Syuzhet> 0.5] <- "Positive"

## As with VADER and Syuzhet, compute classes from hand annotated using -0.5 and 0.5 as thresholds.
gt_tweets$mean_c <- "Neutral"
gt_tweets$mean_c[gt_tweets$mean< -0.5] <- "Negative"
gt_tweets$mean_c[gt_tweets$mean> 0.5] <- "Positive"

## First, let's calculate the accuracy for all three classifiers
N <- nrow(gt_tweets)
cat(" Accuracy for Vader ")
sum(gt_tweets$mean_c==gt_tweets$VADERclass)/N
cat(" Accuracy for Syuzhet ")
sum(gt_tweets$mean_c==gt_tweets$Syuzhetclass)/N

## Then we can study the precision of the positive class
TP <-  gt_tweets$mean_c=="Positive"
cat(" Precision of Vader for Positive class ")
sum(TP & gt_tweets$VADERclass=="Positive")/sum(gt_tweets$VADERclass=="Positive")
cat(" Precision of Syuzhet for Positive class ")
sum(TP & gt_tweets$Syuzhetclass=="Positive")/sum(gt_tweets$Syuzhetclass=="Positive")

## the recall of the positive class
cat(" Recall of Vader for Positive class ")
sum(TP & gt_tweets$VADERclass=="Positive")/sum(TP)
cat(" Recall of Syuzhet for Positive class ")
sum(TP & gt_tweets$Syuzhetclass=="Positive")/sum(TP)

## Let's compare to the negative class by computing also precision and recall:
TN <-  gt_tweets$mean_c=="Negative"
cat(" Precision of Vader for Negative class ")
sum(TN & gt_tweets$VADERclass=="Negative")/sum(gt_tweets$VADERclass=="Negative")
cat(" Precision of Syuzhet for Negative class ")
sum(TN & gt_tweets$Syuzhetclass=="Negative")/sum(gt_tweets$Syuzhetclass=="Negative")

cat(" Recall of Vader for Negative class ")
sum(TN & gt_tweets$VADERclass=="Negative")/sum(TN)
cat(" Recall of Syuzhet for Negative class ")
sum(TN & gt_tweets$Syuzhetclass=="Negative")/sum(TN)

########################################################################################
## confusion matrix for vader

#install.packages('e1071', dependencies=TRUE)
#install.packages('caret', dependencies=TRUE)


library(caret)

# calculate the confusion matrix for vader
cm <- confusionMatrix(data = as.factor(gt_tweets$VADERclass), reference = as.factor(gt_tweets$mean_c))
cm


p <- ggplot(as.data.frame(cm$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Prediction",y = "Reference") +
  scale_x_discrete(labels=c("Negative","Neutral","Positive")) +
  scale_y_discrete(labels=c("Positive","Neutral","Negative")) + 
  ggtitle("Confusion matrix of Vader with mean choice")

p + theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5))

# calculate the confusion matrix for Syuzhet
cm2 <- confusionMatrix(data = as.factor(gt_tweets$Syuzhetclass), reference = as.factor(gt_tweets$mean_c))
cm2

p2 <- ggplot(as.data.frame(cm2$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Prediction",y = "Reference") +
  scale_x_discrete(labels=c("Negative","Neutral","Positive")) +
  scale_y_discrete(labels=c("Positive","Neutral","Negative")) + 
  ggtitle("Confusion matrix of Syuzhet with mean choice")

p2 + theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5))



######################################################################################
######################################################################################
## precision, recall and confusion matrix for mode class

## First, let's calculate the accuracy for all three classifiers
N <- nrow(gt_tweets)
cat(" Accuracy for Vader with mode as GT ")
sum(gt_tweets$mode_c==gt_tweets$VADERclass)/N
cat(" Accuracy for Syuzhet with mode as GT")
sum(gt_tweets$mode_c==gt_tweets$Syuzhetclass)/N

## Then we can study the precision of the positive class
TP <-  gt_tweets$mode_c=="Positive"
cat(" Precision of Vader for Positive class ")
sum(TP & gt_tweets$VADERclass=="Positive")/sum(gt_tweets$VADERclass=="Positive")
cat(" Precision of Syuzhet for Positive class ")
sum(TP & gt_tweets$Syuzhetclass=="Positive")/sum(gt_tweets$Syuzhetclass=="Positive")

## the recall of the positive class
cat(" Recall of Vader for Positive class ")
sum(TP & gt_tweets$VADERclass=="Positive")/sum(TP)
cat(" Recall of Syuzhet for Positive class ")
sum(TP & gt_tweets$Syuzhetclass=="Positive")/sum(TP)

## Let's compare to the negative class by computing also precision and recall:
TN <-  gt_tweets$mode_c=="Negative"
cat(" Precision of Vader for Negative class ")
sum(TN & gt_tweets$VADERclass=="Negative")/sum(gt_tweets$VADERclass=="Negative")
cat(" Precision of Syuzhet for Negative class ")
sum(TN & gt_tweets$Syuzhetclass=="Negative")/sum(gt_tweets$Syuzhetclass=="Negative")

cat(" Recall of Vader for Negative class ")
sum(TN & gt_tweets$VADERclass=="Negative")/sum(TN)
cat(" Recall of Syuzhet for Negative class ")
sum(TN & gt_tweets$Syuzhetclass=="Negative")/sum(TN)

########################################################################################
## confusion matrix for majority choice as ground-truth


# calculate the confusion matrix for vader
cm3 <- confusionMatrix(data = as.factor(gt_tweets$VADERclass), reference = as.factor(gt_tweets$mode_c))
cm3


p3 <- ggplot(as.data.frame(cm$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Prediction",y = "Reference") +
  scale_x_discrete(labels=c("Negative","Neutral","Positive")) +
  scale_y_discrete(labels=c("Positive","Neutral","Negative")) + 
  ggtitle("Confusion matrix of Vader with majority choice")

p3 + theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5))

# calculate the confusion matrix for Syuzhet
cm4 <- confusionMatrix(data = as.factor(gt_tweets$Syuzhetclass), reference = as.factor(gt_tweets$mode_c))
cm4

p4 <- ggplot(as.data.frame(cm2$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Prediction",y = "Reference") +
  scale_x_discrete(labels=c("Negative","Neutral","Positive")) +
  scale_y_discrete(labels=c("Positive","Neutral","Negative")) + 
  ggtitle("Confusion matrix of Syuzhet with majority choice")

p4 + theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5))




######################################################################################
######################################################################################


### save gt_tweets
write.csv(gt_tweets, file = 'gt_tweets.csv')




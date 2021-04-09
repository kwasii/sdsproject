library(syuzhet)
library(vader)

## load the csv files
gt_tweets <- read.csv("hand_annotated.csv", stringsAsFactors = F)

## get average ratings
gt_tweets$mean <- rowMeans(subset(gt_tweets, select = c(daniel, priyanka, florian, dario)))

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
gt_tweets$GTclass <- "Neutral"
gt_tweets$GTclass[gt_tweets$mean< -0.5] <- "Negative"
gt_tweets$GTclass[gt_tweets$mean> 0.5] <- "Positive"

## First, let's calculate the accuracy for all three classifiers
N <- nrow(gt_tweets)
sum(gt_tweets$GTclass==gt_tweets$VADERclass)/N
sum(gt_tweets$GTclass==gt_tweets$Syuzhetclass)/N

## Then we can study the precision of the positive class
TP <-  gt_tweets$GTclass=="Positive"
sum(TP & gt_tweets$VADERclass=="Positive")/sum(gt_tweets$VADERclass=="Positive")
sum(TP & gt_tweets$Syuzhetclass=="Positive")/sum(gt_tweets$Syuzhetclass=="Positive")

## the recall of the positive class
sum(TP & gt_tweets$VADERclass=="Positive")/sum(TP)
sum(TP & gt_tweets$Syuzhetclass=="Positive")/sum(TP)

## Let's compare to the negative class by computing also precision and recall:
TN <-  gt_tweets$GTclass=="Negative"
sum(TN & gt_tweets$VADERclass=="Negative")/sum(gt_tweets$VADERclass=="Negative")
sum(TN & gt_tweets$Syuzhetclass=="Negative")/sum(gt_tweets$Syuzhetclass=="Negative")

sum(TN & gt_tweets$VADERclass=="Negative")/sum(TN)
sum(TN & gt_tweets$Syuzhetclass=="Negative")/sum(TN)

########################################################################################
## confusion matrix for vader

#install.packages('e1071', dependencies=TRUE)
#install.packages('caret', dependencies=TRUE)


library(caret)

# calculate the confusion matrix for vader
cm <- confusionMatrix(data = as.factor(gt_tweets$VADERclass), reference = as.factor(gt_tweets$GTclass))

draw_confusion_matrix <- function(cm) {
  
  total <- sum(cm$table)
  res <- as.numeric(cm$table)
  
  # Generate color gradients. Palettes come from RColorBrewer.
  greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
  redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  getColor <- function (greenOrRed = "green", amount = 0) {
    if (amount == 0)
      return("#FFFFFF")
    palette <- greenPalette
    if (greenOrRed == "red")
      palette <- redPalette
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
  }
  
  # set the basic layout
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  classes = colnames(cm$table)
  rect(150, 430, 240, 370, col=getColor("green", res[1]))
  text(195, 435, classes[1], cex=1.2)
  rect(250, 430, 340, 370, col=getColor("red", res[3]))
  text(295, 435, classes[2], cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=getColor("red", res[2]))
  rect(250, 305, 340, 365, col=getColor("green", res[4]))
  text(140, 400, classes[1], cex=1.2, srt=90)
  text(140, 335, classes[2], cex=1.2, srt=90)
  
  # add in the cm results
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}

draw_confusion_matrix(cm)



p <- ggplot(as.data.frame(cm$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Prediction",y = "Reference") +
  scale_x_discrete(labels=c("Negative","Neutral","Positive")) +
  scale_y_discrete(labels=c("Positive","Neutral","Negative")) + 
  ggtitle("Confusion matrix of Vader")

p + theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5))

# calculate the confusion matrix for Syuzhet
cm2 <- confusionMatrix(data = as.factor(gt_tweets$Syuzhetclass), reference = as.factor(gt_tweets$GTclass))

p2 <- ggplot(as.data.frame(cm2$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Prediction",y = "Reference") +
  scale_x_discrete(labels=c("Negative","Neutral","Positive")) +
  scale_y_discrete(labels=c("Positive","Neutral","Negative")) + 
  ggtitle("Confusion matrix of Syuzhet")

p2 + theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5))



######################################################################################

### save gt_tweets
write.csv(gt_tweets, file = 'gt_tweets.csv')




load("StateZipScores.RData")





load("tweets_final.RData")

#run over tweets
biden_vader = vader_df(tweets_final$text)
tweets_final$VADER <- biden_vader$compound


tweets_final$Syuzhet <- get_sentiment(tweets_final$text) 

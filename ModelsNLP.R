library(stringr)
library(quanteda)
library(tm)
library(quanteda.textmodels)
library(caret)

lirycs_genre <- read.csv("./Data/lyrics-genre.csv")

# Select ramdomly the train set and the test
smp_size <- floor(0.75 * nrow(lirycs_genre))
set.seed(123)
train_ind <- sample(seq_len(nrow(lirycs_genre)), size = smp_size)
train <- lirycs_genre[train_ind, ]
test <- lirycs_genre[-train_ind, ]
train[seq_len(nrow(train)), 4] <- "train"
names(train)[4] <- "type_data"
test[seq_len(nrow(test)), 4] <- "test"
names(test)[4] <- "type_data"

lirycs_genre_sep <- rbind(train, test)

corpus <- corpus(lirycs_genre_sep$lyrics, docvars = data.frame(set = lirycs_genre_sep$type_data, genre = lirycs_genre_sep$genre))

# TF-IDF
dfmat <- dfm(tokens(corpus,remove_punct = TRUE))
dfmat <- dfm_remove(dfmat, stopwords("en"))

dfmat_train <- dfm_subset(dfmat, set == "train")
dfmat_test <- dfm_subset(dfmat, set == "test")

# MODEL multinomial Bernoulli
nbPredictions <- function(dist){
  #Compute a nb (Naive Bayes) model
  multi <- textmodel_nb(dfmat_train,
                        dfmat_train$genre,
                        distribution = dist)
  #Predictions with the model
  pred <- predict(multi, newdata = dfmat_test)
  #Compute the confusion matriz for our prediction
  confM <- confusionMatrix(table(pred, docvars(dfmat_test)$genre))
  #Accuracy is the number of labels (strings) that match...
  my_acc_coincidences <- sum(as.character(pred) == as.character(docvars(dfmat_test)$genre))
  #...divided by the total number of labels
  my_acc_total <- length(as.character(pred))
  my_acc <- my_acc_coincidences/my_acc_total
  my_acc #Sale 0.82876. Con "multinomial" era 0.81304
  genres <- c('Funk-Carioca', 'Hip-Hop', 'Pop', 'Rock', 'Samba', 'Sertanejo')
  genres <- c('Hip Hop', 'Pop', 'Rock')
  i <- 2
  precision <- 0
  recall <- 0
  for (genre in genres){
    precision <- precision + confM$byClass[i,'Pos Pred Value']
    recall <- recall + confM$byClass[i,'Recall']
    i <- i + 1
  }
  precision <- precision / length(genres)
  recall <- recall / length(genres)
  list(acc = my_acc, p = precision, r = recall)
}
# MODEL SVM
svmPredictions <- function(x, weight){ #weight can be "uniform", "docfreq" or "termfreq".
  #Instead of using all the documents marked as train, I take only x documents
  dfmat_train <- dfm_sample(dfm_subset(dfmat, set == "train"), x )
  dfmat_test <- dfm_subset(dfmat, set == "test")
  multi <- textmodel_svm(dfmat_train,
                         dfmat_train$genre,
                         weight = weight)
  pred <- predict(multi, newdata = dfmat_test)
  confM <- confusionMatrix(table(pred, docvars(dfmat_test)$genre))
  my_acc_coincidences <- sum(as.character(pred) == as.character(docvars(dfmat_test)$genre))
  my_acc_total <- length(as.character(pred))
  my_acc <- my_acc_coincidences/my_acc_total
  genres <- c('Hip Hop', 'Pop', 'Rock')
  i <- 2
  precision <- 0
  recall <- 0
  for (genre in genres){
    precision <- precision + confM$byClass[i,'Pos Pred Value']
    recall <- recall + confM$byClass[i,'Recall']
    i <- i + 1
  }
  precision <- precision / length(genres)
  recall <- recall / length(genres)
  list(acc = my_acc, p = precision, r = recall)
}
uniform <- svmPredictions(9000, "uniform")
docfreq <- svmPredictions(9000, "docfreq")
multinomial <- nbPredictions("multinomial")
bernoulli <- nbPredictions("Bernoulli")

# GRAPH
x <- c('Uniform', 'Uniform','Uniform',  'Docfreq', 'Docfreq','Docfreq', 'Multinomial', 'Multinomial','Multinomial', 'Bernoulli', 'Bernoulli', 'Bernoulli')
type_measure <- c("Accuracy", "Precision", "Recall", "Accuracy", "Precision",  "Recall", "Accuracy", "Precision",
                  "Recall", "Accuracy","Precision","Recall")
measures <- c(uniform$acc, uniform$p, uniform$r, docfreq$acc,  docfreq$p, docfreq$r, multinomial$acc, multinomial$p, multinomial$r, bernoulli$acc, bernoulli$p, bernoulli$r)
representation <- data.frame("Model" = x, "Measures" = type_measure, "Values" = measures)
ggplot(data = representation, aes(x=Model, y=Values, fill=Measures)) +
  labs(title="Graph from model using TFM")+
  geom_bar(stat="identity",position=position_dodge())

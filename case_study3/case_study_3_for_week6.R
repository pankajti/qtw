getwd()
setwd("/Users/pankaj/dev/git/smu/qtw/case_study3/Week_5_Materials_2")

load("data.Rda") 

length(emailDFrp)

library(tidyverse)

plot(colSums(is.na(emailDFrp)))

df1$id = seq(1, length(emailDFrp$isRe))

df1 %>% 
  ggplot(mapping = aes(x= id, y = perCaps , color = isSpam))+
  geom_line()

df1 %>% 
  ggplot(mapping = aes(x= id, y = perCaps , color = isSpam))+
  geom_line()

df1 %>% 
  ggplot(mapping = aes(x= isRe,   fill = isSpam))+
  geom_bar()

df1 %>% 
  ggplot(mapping = aes(x=id,  y = ..density.. , color = isSpam))+
  geom_density(  )


df1 %>% 
  ggplot(mapping = aes(x= id, color = isSpam))+
  geom_density( y = df1$perCaps )



library(AppliedPredictiveModeling)
library(caret)


transparentTheme(trans = .4)
featurePlot(x = df1$perCaps, 
            y = df1$isSpam )


ggdens

emailDFrp %>% 
  ggplot(mapping = aes(x= perCaps))+
  geom_line()


names(emailDFrp)

emailDFrp[is.na(emailDFrp$isSpam)]

emailDFrp[is.na(emailDFrp)]

df1 = emailDFrp


df1 <- melt(emailDFrp,"row.names")

require(ggplot2)
require(reshape2)
library(rpart)


set.seed(2568)
n <- nrow(emailDFrp)
train <- sort(sample(1:n, floor(n/2)))

# Training data will be:

emailDFrp.train <- emailDFrp[train,]

# negate the indices to get the test data:
emailDFrp.test <- emailDFrp[-train,]


emailDFrp.rp1 <-rpart(isSpam ~ isRe+underscore ,                         # formula
                data = emailDFrp,                       # data frame
                subset = train,                       # indices of training set
                method = "class",                     # classification tree
                parms = list(split = "information"),  # use entropy/deviance
                maxsurrogate = 0,                     # since no missing values
                cp = 0,                               # no size penalty
                minsplit = 5,                         # Nodes of size 5 (or 
                # more) can be split,
                minbucket = 2,                        # provided each sub-node
                # contains at least 2 obs.
)


pred.rpint1 <- predict(emailDFrp.rp1,
                       newdata = emailDFrp[-train,],
                       type = "class")

Accuracy(y_pred = pred.rpint1, y_true = emailDFrp$isSpam[-train])


#
#  Classification table on the test data
#

table(emailDFrp$isSpam[-train], pred.rpint1)

confusionMatrix(table(emailDFrp$isSpam[-train], pred.rpint1))

emailDFrp.rp <-rpart(isSpam ~ . ,                         # formula
                     data = emailDFrp,                       # data frame
                     subset = train,                       # indices of training set
                     method = "class",                     # classification tree
                     parms = list(split = "information"),  # use entropy/deviance
                     maxsurrogate = 0,                     # since no missing values
                     cp = 0,                               # no size penalty
                     minsplit = 5,                         # Nodes of size 5 (or 
                     # more) can be split,
                     minbucket = 2,                        # provided each sub-node
                     # contains at least 2 obs.
)



pred.rpint <- predict(emailDFrp.rp,
                      newdata = emailDFrp[-train,],
                      type = "class")

table(emailDFrp$isSpam[-train], pred.rpint)


summary(emailDFrp.rp)

plot(emailDFrp.rp, 
     uniform=TRUE,
     compress=TRUE,
     margin = .2)
text(emailDFrp.rp, 
     use.n=TRUE, 
     all = TRUE,
     fancy = TRUE)
fancyRpartPlot(emailDFrp.rp, main="Decision Tree Graph")

#
#  Classification table on the test data
#



library(h2o)
h2o.init()

emailDHex = as.h2o(emailDFrp)

# split into train and validation
splits = h2o.splitFrame(data = emailDHex, ratios = .8, seed = 1234)
trainHex = splits[[1]]
validHex = splits[[2]]
predictors = c("isRe")
predictors = names(emailDFrp)[c(seq(2,29))]

response = c("isSpam")
# Build and train the model:

emailD_2tree <- h2o.randomForest(x = predictors,
                                  y = response,
                                  ntrees = 5,
                                  max_depth = 5,
                                  min_rows = 20,
                                  binomial_double_trees = TRUE,
                                  training_frame = trainHex,
                                  validation_frame = validHex)
 

emailDH2oTree2 = h2o.getModelTree(model = emailD_2tree, tree_number = 2)

library(data.tree)


library(DiagrammeR)

emailDataTree = createDataTree(emailDH2oTree2)

GetEdgeLabel <- function(node) {return (node$edgeLabel)}
GetNodeShape <- function(node) {switch(node$type, 
                                       split = "diamond", leaf = "oval")}
GetFontName <- function(node) {switch(node$type, 
                                      split = 'Palatino-bold', 
                                      leaf = 'Palatino')}
SetEdgeStyle(emailDataTree, fontname = 'Palatino-italic', 
             label = GetEdgeLabel, labelfloat = TRUE,
             fontsize = "26", fontcolor='royalblue4')
SetNodeStyle(emailDataTree, fontname = GetFontName, shape = GetNodeShape, 
             fontsize = "26", fontcolor='royalblue4',
             height="0.75", width="1")

SetGraphStyle(emailDataTree, rankdir = "TD", dpi=70.)

plot(emailDataTree, output = "graph")

emailDataTree



h2o.shutdown()



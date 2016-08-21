library('datasets')
library('Matrix')
library('randomForest')
library('pROC')

logloss <- function(y, p) {
  N <- length(y)
  -1/N*sum((y*log(p) + (1-y)*log(p)))
}

df <- as.data.frame(Titanic)
df['Freq'] <- NULL

for (v in c('Class', 'Sex', 'Age')) {
  df[v] <- as.factor(df[, v])
}
df['Survived'] = as.numeric(df[, 'Survived']) - 1

dm <- model.matrix(Survived ~ Class + Sex + Age - 1, data=df)

rf_R <- randomForest(x=dm, y=df$Survived, ntree=100, mtry=2, sampSize=nrow(df))
preds_R <- predict(rf_R)

logloss(df$Survived, preds_R)
auc(df$Survived, preds_R)

preds_rand <- sample(nrow(df))
logloss(df$Survived, preds_rand)
auc(df$Survived, preds_rand)

write.csv(cbind(dm, Survived=df$Survived), file = "./titanic_design_matrix.csv", row.names=FALSE)

preds_scala <- c(0.45736904761904784, 0.5432857142857141, 0.5188095238095239, 0.5011904761904763, 0.4494047619047619, 0.4891666666666667, 0.5470595238095238, 0.48819047619047623, 0.46745238095238106, 0.44416666666666665, 0.5072857142857142, 0.4848333333333333, 0.5498809523809524, 0.4981904761904761, 0.48636904761904776, 0.576, 0.45736904761904784, 0.5432857142857141, 0.5188095238095239, 0.5011904761904763, 0.4494047619047619, 0.4891666666666667, 0.5470595238095238, 0.48819047619047623, 0.46745238095238106, 0.44416666666666665, 0.5072857142857142, 0.4848333333333333, 0.5498809523809524, 0.4981904761904761, 0.48636904761904776, 0.576)
logloss(df$Survived, preds_scala)
auc(df$Survived, preds_scala)


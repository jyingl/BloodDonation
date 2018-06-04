dataset <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/blood-transfusion/transfusion.data', 
                      header = TRUE,sep=',')

names(dataset) <- c("Recency", "Frequency","Amount","FirstDonation","Donate")

# Encoding the target feature as factor
dataset$Donate = factor(dataset$Donate, levels = c(0, 1))

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Donate ~ .,
                 family = binomial,
                 data = dataset)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response')
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(dataset[, 5], y_pred > 0.5)
accuracy = (cm[1,1]+cm[2,2])/748
print(accuracy)

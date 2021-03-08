# This script validates for predicting the management.

########################## Load data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("app_data_clean.Rda")


########################## Variables to be considered in the analysis

var_names_full <- c("Age", "BMI", "Sex", "Height", "Weight", 
                    "AS", "PAS", 
                    "Visibility of Appendix", "Appendix Diameter", "Migration of Pain", "Tenderness in RLQ", 
                    "Rebound Tenderness", "Cough Tenderness", "Psoas Sign",
                    "Nausea/Vomitting", "Anorexia", "Body Temperature", "WBC Count", "Neutrophil Percentage", 
                    "Ketones in Urine", "Erythrocytes in Urine", "WBC in Urine", "CRP",
                    "Dysuria", "Stool", "Peritonitis/Abdominal Guarding", "Free Intraperitoneal Fluid",
                    "Appendix Layers", "Target Sign", "Appendix Perfusion",
                    "Surrounding Tissue Reaction", "Path. Lymph Nodes",
                    "Mesenteric Lymphadenitis", "Bowel Wall Thickening", "Ileus", "Coprostasis",
                    "Meteorism", "Enteritis")
# Uncomment an approrpiate feature set:
# Full model
vars_incl <- c("Age", "BMI", "Sex", "Height", "Weight", "AppendicitisComplications", 
               "AlvaradoScore", "PediatricAppendicitisScore", #"SecondaryCondition", (not pre-treatment!)
               "AppendixOnSono", "AppendixDiameter", "MigratoryPain", "LowerAbdominalPainRight", 
               "ReboundTenderness", "CoughingPain", "PsoasSign", # "AbdominalGuarding", (kicked out!)
               "Nausea", "AppetiteLoss", "BodyTemp", "WBCCount", "NeutrophilPerc", 
               "KetonesInUrine", "ErythrocytesInUrine", "WBCInUrine", "CRPEntry",
               "Dysuria", "Stool", "Peritonitis", "FreeFluids",  
               "AppendixWallLayers", "Kokarde", "TissuePerfusion", #"AppendixPerforation", (this variable is operative!)
               "SurroundingTissueReaction", "PathLymphNodes",
               "MesentricLymphadenitis", "BowelWallThick", "Ileus", "FecalImpaction",
               "Meteorism", "Enteritis")
# Without US
#vars_incl <- c("Age", "BMI", "Sex", "Height", "Weight", "AppendicitisComplications", 
#               "AlvaradoScore", "PediatricAppendicitisScore", #"SecondaryCondition", (not pre-treatment!)
#               "MigratoryPain", "LowerAbdominalPainRight", # "AppendixOnSono", "AppendixDiameter", (US!)
#               "ReboundTenderness", "CoughingPain", "PsoasSign", # "AbdominalGuarding", (kicked out!)
#               "Nausea", "AppetiteLoss", "BodyTemp", "WBCCount", "NeutrophilPerc", 
#               "KetonesInUrine", "ErythrocytesInUrine", "WBCInUrine", "CRPEntry",
#               "Dysuria", "Stool", "Peritonitis") #, "FreeFluids" (US!)
# Without peritonitis
#vars_incl <- c("Age", "BMI", "Sex", "Height", "Weight", "AppendicitisComplications", 
#               "AlvaradoScore", "PediatricAppendicitisScore", #"SecondaryCondition", (not pre-treatment!)
#               "AppendixOnSono", "AppendixDiameter", "MigratoryPain", "LowerAbdominalPainRight", 
#               "ReboundTenderness", "CoughingPain", "PsoasSign", # "AbdominalGuarding", (kicked out!)
#               "Nausea", "AppetiteLoss", "BodyTemp", "WBCCount", "NeutrophilPerc", 
#               "KetonesInUrine", "ErythrocytesInUrine", "WBCInUrine", "CRPEntry",
#               "Dysuria", "Stool", "FreeFluids", #"Peritonitis",
#               "AppendixWallLayers", "Kokarde", "TissuePerfusion", #"AppendixPerforation", (this variable is operative!)
#               "SurroundingTissueReaction", "PathLymphNodes",
#               "MesentricLymphadenitis", "BowelWallThick", "Ileus", "FecalImpaction",
#               "Meteorism", "Enteritis")
# Without US & peritonitis
#vars_incl <- c("Age", "BMI", "Sex", "Height", "Weight", "AppendicitisComplications", 
#               "AlvaradoScore", "PediatricAppendicitisScore", #"SecondaryCondition", (not pre-treatment!)
#               "MigratoryPain", "LowerAbdominalPainRight", # "AppendixOnSono", "AppendixDiameter", (US!)
#               "ReboundTenderness", "CoughingPain", "PsoasSign", # "AbdominalGuarding", (kicked out!)
#               "Nausea", "AppetiteLoss", "BodyTemp", "WBCCount", "NeutrophilPerc", 
#               "KetonesInUrine", "ErythrocytesInUrine", "WBCInUrine", "CRPEntry",
#               "Dysuria", "Stool") #, "FreeFluids" (US!), "Peritonitis"
# Without AS, PAS
#vars_incl <- c("Age", "BMI", "Sex", "Height", "Weight", "AppendicitisComplications",
#               "AppendixOnSono", "AppendixDiameter", "MigratoryPain", "LowerAbdominalPainRight", 
#               "ReboundTenderness", "CoughingPain", "PsoasSign", # "AbdominalGuarding", (kicked out!)
#               "Nausea", "AppetiteLoss", "BodyTemp", "WBCCount", "NeutrophilPerc", 
#               "KetonesInUrine", "ErythrocytesInUrine", "WBCInUrine", "CRPEntry",
#               "Dysuria", "Stool", "Peritonitis", "FreeFluids",  
#               "AppendixWallLayers", "Kokarde", "TissuePerfusion", #"AppendixPerforation", (this variable is operative!)
#               "SurroundingTissueReaction", "PathLymphNodes",
#                "MesentricLymphadenitis", "BowelWallThick", "Ileus", "FecalImpaction",
#               "Meteorism", "Enteritis")
app.data.comp <- app.data[!is.na(app.data$DiagnosisByCriteria), vars_incl]


########################## Imputation

# Are there missing values in the response variable?
summary(app.data.comp$AppendicitisComplications)
# Perform imputation using the kNN method
# NB: make sure to exclude the response variable from imputation!
# Including it might create spurious correlations and, consequently, make CV results too optimistic!
set.seed(1799)
app.data.comp.imputed <- kNN(app.data.comp[, -which(colnames(app.data.comp) == "AppendicitisComplications")])[, 1 : (ncol(app.data.comp) - 1)]
app.data.comp.imputed$AppendicitisComplications <- app.data.comp$AppendicitisComplications


########################## Logistic regression

# Fit the model
logreg.comp <- glm(AppendicitisComplications ~ ., data = app.data.comp.imputed, family = "binomial")
summary(logreg.comp)

# Look at the training set performance
pred <- prediction(predict(logreg.comp, type = "response"), app.data.comp.imputed$AppendicitisComplications)
perf <- performance(pred, "tpr", "fpr")
# ROC curve
plot(perf, colorize = TRUE, asp = 1)
# AUROC
(auc_train <- performance(pred, measure = "auc")@y.values[[1]])
# PR curve
perf <- performance(pred, measure = "prec", x.measure = "rec")
perf@y.values[[1]][1] <- 1.0
plot(perf, colorize = TRUE, asp = 1)
# AUPR
(aupr_train <- computeArea(perf@x.values[[1]], perf@y.values[[1]]))

# K-fold CV
set.seed(1799)
K <- 10
aucs.logreg <- numeric(K)
auprs.logreg <- numeric(K)
specs.logreg <- numeric(K)
sens.logreg <- numeric(K)
ppv.logreg <- numeric(K)
npv.logreg <- numeric(K)
accs.logreg <- numeric(K)
balaccs.logreg <- numeric(K)
app.data.comp.1 <- app.data.comp[as.numeric(app.data.comp$AppendicitisComplications) == 1, ]
app.data.comp.2 <- app.data.comp[as.numeric(app.data.comp$AppendicitisComplications) == 2, ]
fold_membership.1 <- sample(c(rep(1 : K, floor(nrow(app.data.comp.1) / K)),  
                            1 : (nrow(app.data.comp.1) %% K)), size = nrow(app.data.comp.1), 
                          replace = FALSE)
fold_membership.2 <- sample(c(rep(1 : K, floor(nrow(app.data.comp.2) / K)),  
                              1 : (nrow(app.data.comp.2) %% K)), size = nrow(app.data.comp.2), 
                            replace = FALSE)
cm_overall <- NA
for (k in 1 : K) {
  # We perform separate imputation for train and test sets!
  train_flags.1 <- fold_membership.1 != k
  train_flags.2 <- fold_membership.2 != k
  d.k <- rbind(app.data.comp.1[train_flags.1, ], app.data.comp.2[train_flags.2, ])
  d.k.imputed <- kNN(d.k[, -which(colnames(d.k) == "AppendicitisComplications")])[, 1 : (ncol(d.k) - 1)]
  d.k.imputed$AppendicitisComplications <- d.k$AppendicitisComplications
  logreg.k <- glm(AppendicitisComplications ~ ., data = d.k.imputed, family = "binomial")
  d.test <- rbind(app.data.comp.1[!train_flags.1, ], app.data.comp.2[!train_flags.2, ])
  d.test.imputed <- kNN(d.test[, -which(colnames(d.test) == "AppendicitisComplications")])[, 1 : (ncol(d.test) - 1)]
  d.test.imputed$AppendicitisComplications <- d.test$AppendicitisComplications
  pred <- prediction(predict(logreg.k, d.test.imputed, type = "response"), d.test.imputed$AppendicitisComplications)
  perf <- performance(pred, "tpr", "fpr")
  auc_test <- performance(pred, measure = "auc")@y.values[[1]]
  perf <- performance(pred, measure = "prec", x.measure = "rec")
  perf@y.values[[1]][1] <- perf@y.values[[1]][2]
  aupr_test <- computeArea(perf@x.values[[1]], perf@y.values[[1]])
  aucs.logreg[k] <- auc_test
  auprs.logreg[k] <- aupr_test
  cm_test <- confusionMatrix(factor(as.numeric(predict(logreg.k, d.test.imputed, type = "response") >= 0.5) + 1, levels = c(1, 2), labels = c("no", "yes")), 
                               factor(as.numeric(d.test.imputed$AppendicitisComplications), levels = c(1, 2), labels = c("no", "yes")))
  specs.logreg[k] <- unname(cm_test$byClass["Specificity"])
  sens.logreg[k] <- unname(cm_test$byClass["Sensitivity"])
  ppv.logreg[k] <- unname(cm_test$byClass["Pos Pred Value"])
  npv.logreg[k] <- unname(cm_test$byClass["Neg Pred Value"])
  accs.logreg[k] <- unname(cm_test$overall["Accuracy"])
  balaccs.logreg[k] <- unname(cm_test$byClass["Balanced Accuracy"])
  if (is.na(cm_overall)) {
    cm_overall <- cm_test$table
  } else {
    cm_overall <- cm_overall + cm_test$table
  }
}
c(mean(aucs.logreg), sd(aucs.logreg), mean(aucs.logreg) - 2 * sd(aucs.logreg), mean(aucs.logreg) + 2 * sd(aucs.logreg))
c(mean(auprs.logreg), sd(auprs.logreg), mean(auprs.logreg) - 2 * sd(auprs.logreg), mean(auprs.logreg) + 2 * sd(auprs.logreg))
c(mean(sens.logreg), sd(sens.logreg), mean(sens.logreg) - 2 * sd(sens.logreg), mean(sens.logreg) + 2 * sd(sens.logreg))
c(mean(specs.logreg), sd(specs.logreg), mean(specs.logreg) - 2 * sd(specs.logreg), mean(specs.logreg) + 2 * sd(specs.logreg))
c(mean(ppv.logreg), sd(ppv.logreg), mean(ppv.logreg) - 2 * sd(ppv.logreg), mean(ppv.logreg) + 2 * sd(ppv.logreg))
c(mean(npv.logreg, na.rm = TRUE), sd(npv.logreg, na.rm = TRUE), mean(npv.logreg, na.rm = TRUE) - 2 * sd(npv.logreg, na.rm = TRUE), 
  mean(npv.logreg, na.rm = TRUE) + 2 * sd(npv.logreg, na.rm = TRUE))
c(mean(accs.logreg), sd(accs.logreg), mean(accs.logreg) - 2 * sd(accs.logreg), mean(accs.logreg) + 2 * sd(accs.logreg))
c(mean(balaccs.logreg), sd(balaccs.logreg), mean(balaccs.logreg) - 2 * sd(balaccs.logreg), mean(balaccs.logreg) + 2 * sd(balaccs.logreg))
cm_overall


########################## Random forest

# Fit the model
rf.comp <- randomForest(AppendicitisComplications ~ ., data = app.data.comp.imputed, importance = TRUE)

# Examine variable importance
oldpars <- par()
par(mar = c(15, 5, 5, 5))
barplot(rf.comp$importance[, 3], las = 2)
par(mar = oldpars$mar)

# Variability of variable importance values
set.seed(1799)
B <- 300
imps_b <- matrix(nrow = B, ncol = ncol(app.data.comp.imputed) - 1)
colnames(imps_b) <- row.names(rf.comp$importance)
for (b in 1 : B) {
  idx <- 1 : nrow(app.data.comp.imputed)
  idx_b <- sample(idx, size=length(idx), replace = TRUE)
  app.data.comp.imputed_b <- app.data.comp.imputed[idx_b, ]
  rf_b <- randomForest(AppendicitisComplications ~ ., data = app.data.comp.imputed_b, importance = TRUE)
  imps_b[b, ] <- (rf_b$importance[, 1] + rf_b$importance[, 2]) / 2
}
oldpars <- par()
jpeg("varimp_comp.jpg", width = 2200, height = 1300, 
     pointsize = 32)
par(mar = c(15, 5, 2, 2))
boxplot(as.data.frame(imps_b), cex=0.1, las=2, ylab = "Bootstrapped RF Variable Importance", 
        names = var_names_full, notch = TRUE, cex.axis = 1.2, cex.lab = 1.2, 
        lwd = 2)
mtext("c", side = 3, adj = 0, line = 1.2, cex = 1.2, font = 2)
dev.off()
par(mar = oldpars$mar)

# Look at the training set performance
pred <- prediction(predict(rf.comp, type = "prob")[, 2], app.data.comp.imputed$AppendicitisComplications)
perf <- performance(pred, "tpr", "fpr")
# ROC curve
plot(perf, colorize = TRUE, asp = 1)
# AUROC
(auc_train <- performance(pred, measure = "auc")@y.values[[1]])
# PR curve
perf <- performance(pred, measure = "prec", x.measure = "rec")
perf@y.values[[1]][1] <- 1.0
plot(perf, colorize = TRUE, asp = 1)
# AUPR
(aupr_train <- computeArea(perf@x.values[[1]], perf@y.values[[1]]))

# K-fold CV
set.seed(1799)
K <- 10
aucs.rf <- numeric(K)
auprs.rf <- numeric(K)
specs.rf <- numeric(K)
sens.rf <- numeric(K)
ppv.rf <- numeric(K)
npv.rf <- numeric(K)
accs.rf <- numeric(K)
balaccs.rf <- numeric(K)
fold_membership <- sample(c(rep(1 : K, floor(nrow(app.data.comp) / K)),  1 : (nrow(app.data.comp) %% K)), size = nrow(app.data.comp), replace = FALSE)
cm_overall <- NA
rocs <- list()
prcs <- list()
prcs_rand <- list()
for (k in 1 : K) {
  # We perform separate imputation for train and test sets!
  train_flags <- fold_membership != k
  d.k <- app.data.comp[train_flags, ]
  d.k.imputed <- kNN(d.k[, -which(colnames(d.k) == "AppendicitisComplications")])[, 1 : (ncol(d.k) - 1)]
  d.k.imputed$AppendicitisComplications <- d.k$AppendicitisComplications
  # NB: in R, randomForest is balanced by default (i.e. it uses the loss function with classes weighted by their inverse frequencies)
  rf.k <- randomForest(AppendicitisComplications ~ ., data = d.k.imputed)
  d.test <- app.data.comp[!train_flags, ]
  d.test.imputed <- kNN(d.test[, -which(colnames(d.test) == "AppendicitisComplications")])[, 1 : (ncol(d.test) - 1)]
  d.test.imputed$AppendicitisComplications <- d.test$AppendicitisComplications
  pred <- prediction(predict(rf.k, d.test.imputed, type = "prob")[, 2], d.test.imputed$AppendicitisComplications)
  perf <- performance(pred, "tpr", "fpr")
  rocs[[k]] <- perf
  auc_test <- performance(pred, measure = "auc")@y.values[[1]]
  perf <- performance(pred, measure = "prec", x.measure = "rec")
  perf@y.values[[1]][1] <- perf@y.values[[1]][2]
  prcs[[k]] <- perf
  aupr_test <- computeArea(perf@x.values[[1]], perf@y.values[[1]])
  aucs.rf[k] <- auc_test
  auprs.rf[k] <- aupr_test
  # Random classifier
  pred_rand <- prediction(sample(predict(rf.k, d.test.imputed, type = "prob")[, 2]), d.test.imputed$AppendicitisComplications)
  perf <- performance(pred_rand, measure = "prec", x.measure = "rec")
  prcs_rand[[k]] <- perf
  cm_test <- confusionMatrix(predict(rf.k, d.test.imputed, type = "response"), d.test.imputed$AppendicitisComplications)
  specs.rf[k] <- unname(cm_test$byClass["Specificity"])
  sens.rf[k] <- unname(cm_test$byClass["Sensitivity"])
  ppv.rf[k] <- unname(cm_test$byClass["Pos Pred Value"])
  npv.rf[k] <- unname(cm_test$byClass["Neg Pred Value"])
  accs.rf[k] <- unname(cm_test$overall["Accuracy"])
  balaccs.rf[k] <- unname(cm_test$byClass["Balanced Accuracy"])
  if (is.na(cm_overall)) {
    cm_overall <- cm_test$table
  } else {
    cm_overall <- cm_overall + cm_test$table
  }
}
c(mean(aucs.rf), sd(aucs.rf), mean(aucs.rf) - 2 * sd(aucs.rf), mean(aucs.rf) + 2 * sd(aucs.rf))
c(mean(auprs.rf), sd(auprs.rf), mean(auprs.rf) - 2 * sd(auprs.rf), mean(auprs.rf) + 2 * sd(auprs.rf))
c(mean(sens.rf), sd(sens.rf), mean(sens.rf) - 2 * sd(sens.rf), mean(sens.rf) + 2 * sd(sens.rf))
c(mean(specs.rf), sd(specs.rf), mean(specs.rf) - 2 * sd(specs.rf), mean(specs.rf) + 2 * sd(specs.rf))
c(mean(ppv.rf), sd(ppv.rf), mean(ppv.rf) - 2 * sd(ppv.rf), mean(ppv.rf) + 2 * sd(ppv.rf))
c(mean(npv.rf), sd(npv.rf), mean(npv.rf) - 2 * sd(npv.rf), mean(npv.rf) + 2 * sd(npv.rf))
c(mean(accs.rf), sd(accs.rf), mean(accs.rf) - 2 * sd(accs.rf), mean(accs.rf) + 2 * sd(accs.rf))
c(mean(balaccs.rf), sd(balaccs.rf), mean(balaccs.rf) - 2 * sd(balaccs.rf), mean(balaccs.rf) + 2 * sd(balaccs.rf))
cm_overall
# Plot ROC curves
plot(0.5, 0.5, pch = "", xlim = c(0, 1), ylim = c(0, 1), asp = 1.0, 
     xlab = "False Positive Rate", ylab = "True Positive Rate")
for (k in 1 : K) {
  lines(rocs[[k]]@x.values[[1]], rocs[[k]]@y.values[[1]], col = makeTransparent("red"),
        lwd = 2)
}
abline(a = 0, b = 1, col = "gray", lty = 2, lwd = 2)
# Plot PR curves
plot(0.5, 0.5, pch = "", xlim = c(0, 1), ylim = c(0, 1), asp = 1.0, 
     xlab = "Recall", ylab = "Precision")
for (k in 1 : K) {
  lines(prcs[[k]]@x.values[[1]], prcs[[k]]@y.values[[1]], col = makeTransparent("red"),
        lwd = 2)
}
for (k in 1 : K) {
  lines(prcs_rand[[k]]@x.values[[1]], prcs_rand[[k]]@y.values[[1]], col = makeTransparent("gray"),
        lwd = 2, lty = 2)
}

partialPlot(rf.comp, x.var = "CRPEntry", pred.data = app.data.comp.imputed)
plot(app.data.comp.imputed$AppendicitisComplications ~ app.data.comp.imputed$CRPEntry, xlab = "CRP at Entry, mg/l", ylab = "Complicated Appendicitis?")


########################## Gradient boosting

# Fit the model
ntr <- 100
gb.comp <- gbm(as.numeric(AppendicitisComplications)-1 ~ ., data = app.data.comp.imputed, n.trees = ntr)

# Examine variable importance
s <- summary.gbm(gb.comp, plotit = FALSE)
oldpars <- par()
par(mar = c(15, 5, 5, 5))
barplot(height = s[, 2], names.arg = as.character(s[, 1]), las = 2)
par(mar = oldpars$mar)

# Look at the training set performance
pred <- prediction(predict(gb.comp, type = "response", n.trees = ntr), app.data.comp.imputed$AppendicitisComplications)
perf <- performance(pred, "tpr", "fpr")
# ROC curve
plot(perf, colorize = TRUE, asp = 1)
abline(a = 0, b = 1, col = "gray", lty = 2)
# AUROC
(auc_train <- performance(pred, measure = "auc")@y.values[[1]])
# PR curve
perf <- performance(pred, measure = "prec", x.measure = "rec")
perf@y.values[[1]][1] <- 1.0
plot(perf, colorize = TRUE, asp = 1)
# AUPR
(aupr_train <- computeArea(perf@x.values[[1]], perf@y.values[[1]]))

# Stratified K-fold CV
set.seed(1799)
ntr <- 100
K <- 10
aucs.gb <- numeric(K)
auprs.gb <- numeric(K)
specs.gb <- numeric(K)
sens.gb <- numeric(K)
ppv.gb <- numeric(K)
npv.gb <- numeric(K)
accs.gb <- numeric(K)
balaccs.gb <- numeric(K)
fold_membership <- sample(c(rep(1 : K, floor(nrow(app.data.comp) / K)),  1 : (nrow(app.data.comp) %% K)), size = nrow(app.data.comp), replace = FALSE)
cm_overall <- NA
for (k in 1 : K) {
  # We perform separate imputation for train and test sets!
  train_flags <- fold_membership != k
  d.k <- app.data.comp[train_flags, ]
  d.k.imputed <- kNN(d.k[, -which(colnames(d.k) == "AppendicitisComplications")])[, 1 : (ncol(d.k) - 1)]
  d.k.imputed$AppendicitisComplications <- d.k$AppendicitisComplications
  gb.k <- gbm(as.numeric(AppendicitisComplications)-1 ~ ., data = d.k.imputed, n.trees = ntr)
  d.test <- app.data.comp[!train_flags, ]
  d.test.imputed <- kNN(d.test[, -which(colnames(d.test) == "AppendicitisComplications")])[, 1 : (ncol(d.test) - 1)]
  d.test.imputed$AppendicitisComplications <- d.test$AppendicitisComplications
  pred <- prediction(predict(gb.k, d.test.imputed, type = "response", n.trees = ntr), d.test.imputed$AppendicitisComplications)
  perf <- performance(pred, "tpr", "fpr")
  auc_test <- performance(pred, measure = "auc")@y.values[[1]]
  perf <- performance(pred, measure = "prec", x.measure = "rec")
  perf@y.values[[1]][1] <- perf@y.values[[1]][2]
  aupr_test <- computeArea(perf@x.values[[1]], perf@y.values[[1]])
  aucs.gb[k] <- auc_test
  auprs.gb[k] <- aupr_test
  cm_test <- confusionMatrix(factor(as.numeric(predict(gb.k, d.test.imputed, type = "response", n.trees = ntr) >= 0.5) + 1, levels = c(1, 2), labels = c("no", "yes")),  d.test.imputed$AppendicitisComplications)
  specs.gb[k] <- unname(cm_test$byClass["Specificity"])
  sens.gb[k] <- unname(cm_test$byClass["Sensitivity"])
  ppv.gb[k] <- unname(cm_test$byClass["Pos Pred Value"])
  npv.gb[k] <- unname(cm_test$byClass["Neg Pred Value"])
  accs.gb[k] <- unname(cm_test$overall["Accuracy"])
  balaccs.gb[k] <- unname(cm_test$byClass["Balanced Accuracy"])
  if (is.na(cm_overall)) {
    cm_overall <- cm_test$table
  } else {
    cm_overall <- cm_overall + cm_test$table
  }
}
c(mean(aucs.gb), sd(aucs.gb), mean(aucs.gb) - 2 * sd(aucs.gb), mean(aucs.gb) + 2 * sd(aucs.gb))
c(mean(auprs.gb), sd(auprs.gb), mean(auprs.gb) - 2 * sd(auprs.gb), mean(auprs.gb) + 2 * sd(auprs.gb))
c(mean(sens.gb), sd(sens.gb), mean(sens.gb) - 2 * sd(sens.gb), mean(sens.gb) + 2 * sd(sens.gb))
c(mean(specs.gb), sd(specs.gb), mean(specs.gb) - 2 * sd(specs.gb), mean(specs.gb) + 2 * sd(specs.gb))
c(mean(ppv.gb), sd(ppv.gb), mean(ppv.gb) - 2 * sd(ppv.gb), mean(ppv.gb) + 2 * sd(ppv.gb))
c(mean(npv.gb), sd(npv.gb), mean(npv.gb) - 2 * sd(npv.gb), mean(npv.gb) + 2 * sd(npv.gb))
c(mean(accs.gb), sd(accs.gb), mean(accs.gb) - 2 * sd(accs.gb), mean(accs.gb) + 2 * sd(accs.gb))
c(mean(balaccs.gb), sd(balaccs.gb), mean(balaccs.gb) - 2 * sd(balaccs.gb), mean(balaccs.gb) + 2 * sd(balaccs.gb))
cm_overall


########################## Statistical tests for model comparison
t.test(x = aucs.rf, y = aucs.logreg, paired = TRUE)
t.test(x = auprs.rf, y = auprs.logreg, paired = TRUE)
t.test(x = aucs.gb, y = aucs.logreg, paired = TRUE)
t.test(x = auprs.gb, y = auprs.logreg, paired = TRUE)


########################## Baseline: Criterion from Imaoka et al.

# AUROC
pred <- prediction(((app.data.comp.imputed$BodyTemp >= 37.4) + (app.data.comp.imputed$CRPEntry >= 47) + (app.data.comp.imputed$FreeFluids == "yes")) * 1.0, app.data.comp.imputed$AppendicitisComplications)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, asp = 1)
abline(a = 0, b = 1, col = "gray", lty = 2)
(auc_base <- performance(pred, measure = "auc")@y.values[[1]])
# AUPR
perf <- performance(pred, measure = "prec", x.measure = "rec")
perf@y.values[[1]][1] <- 1.0
plot(perf, colorize = TRUE, asp = 1)
(aupr_base <- computeArea(perf@x.values[[1]], perf@y.values[[1]]))


########################## Baseline: Random classifier
n_perms <- 10000
sens.rand <- 0
spec.rand <- 0
ppv.rand <- 0
npv.rand <- 0
for (i in 1:n_perms) {
  rand_perm <- sample(app.data.comp.imputed$AppendicitisComplications, 
                      size = length(app.data.comp.imputed$AppendicitisComplications), replace = FALSE)
  cm <- confusionMatrix(rand_perm,  app.data.comp.imputed$AppendicitisComplications)
  sens.rand <- sens.rand + unname(cm$byClass["Sensitivity"])
  spec.rand <- spec.rand + unname(cm$byClass["Specificity"])
  ppv.rand <- ppv.rand + unname(cm$byClass["Pos Pred Value"])
  npv.rand <- npv.rand + unname(cm$byClass["Neg Pred Value"])
}
(sens.rand / n_perms)
(spec.rand / n_perms)
(ppv.rand / n_perms)
(npv.rand / n_perms)
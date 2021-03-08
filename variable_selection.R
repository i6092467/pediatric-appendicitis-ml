# Script for variable selection using random forests.
# NB: this script may take a while to run!

########################## Load data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("app_data_clean.Rda")


##############################################################################
########################## Disease Status ####################################
##############################################################################

########################## Variables to be considered in the analysis

vars_incl <- c("Age", "BMI", "Sex", "Height", "Weight", "DiagnosisByCriteria", 
               "AlvaradoScore", "PediatricAppendicitisScore", #"SecondaryCondition", (not pre-treatment!)
               "AppendixOnSono", "AppendixDiameter", "MigratoryPain", "LowerAbdominalPainRight", 
               "ReboundTenderness", "CoughingPain", "PsoasSign", #"AbdominalGuarding", (kicked out!)
               "Nausea", "AppetiteLoss", "BodyTemp", "WBCCount", "NeutrophilPerc", 
               "KetonesInUrine", "ErythrocytesInUrine", "WBCInUrine", "CRPEntry",
               "Dysuria", "Stool", "Peritonitis", "FreeFluids", 
               "AppendixWallLayers", "Kokarde", "TissuePerfusion", #"AppendixPerforation", (this variable is operative!)
               "SurroundingTissueReaction", "PathLymphNodes",
               "MesentricLymphadenitis", "BowelWallThick", "Ileus", "FecalImpaction",
               "Meteorism", "Enteritis")
app.data.dis <- app.data[!is.na(app.data$DiagnosisByCriteria), vars_incl]


########################## Variable selection based on RF importance
ps <- 1 : (length(vars_incl) - 1)
aucs.p <- numeric(length(ps))
aucs.sd.p <- numeric(length(ps))
auprs.p <- numeric(length(ps))
auprs.sd.p <- numeric(length(ps))
bestvars <- list()
for (p in ps) {
  print(paste0("Fitting RF based on ", p, " best predictor(s)"))
  # K-fold CV
  set.seed(1799)
  K <- 10
  aucs.rf <- numeric(K)
  auprs.rf <- numeric(K)
  specs.rf <- numeric(K)
  sens.rf <- numeric(K)
  accs.rf <- numeric(K)
  balaccs.rf <- numeric(K)
  fold_membership <- sample(c(rep(1 : K, floor(nrow(app.data.dis) / K)),  
                              1 : (nrow(app.data.dis) %% K)), size = nrow(app.data.dis), 
                            replace = FALSE)
  cm_overall <- NA
  for (k in 1 : K) {
    # We perform separate imputation for train and test sets!
    train_flags <- fold_membership != k
    d.k <- app.data.dis[train_flags, ]
    d.k.imputed <- kNN(d.k[, -which(colnames(d.k) == "DiagnosisByCriteria")])[, 1 : (ncol(d.k) - 1)]
    d.k.imputed$DiagnosisByCriteria <- d.k$DiagnosisByCriteria
    rf.k <- randomForest(DiagnosisByCriteria ~ ., data = d.k.imputed, importance = TRUE)
    rf.vimp <- (importance(rf.k)[, 1] + importance(rf.k)[, 2]) / 2
    imp.vars <- names(tail(sort(rf.vimp), p))
    bestvars[[p]] <- imp.vars
    rf.k <- randomForest(DiagnosisByCriteria ~ ., data = d.k.imputed[, c(imp.vars, 
                                                                         "DiagnosisByCriteria")])
    d.test <- app.data.dis[!train_flags, ]
    d.test.imputed <- kNN(d.test[, -which(colnames(d.test) == "DiagnosisByCriteria")])[, 1 : (ncol(d.test) - 1)]
    d.test.imputed$DiagnosisByCriteria <- d.test$DiagnosisByCriteria
    pred <- prediction(predict(rf.k, d.test.imputed, type = "prob")[, 2], 
                       d.test.imputed$DiagnosisByCriteria)
    perf <- performance(pred, "tpr", "fpr")
    auc_test <- performance(pred, measure = "auc")@y.values[[1]]
    perf <- performance(pred, measure = "prec", x.measure = "rec")
    perf@y.values[[1]][1] <- 1.0
    aupr_test <- computeArea(perf@x.values[[1]], perf@y.values[[1]])
    aucs.rf[k] <- auc_test
    auprs.rf[k] <- aupr_test
    cm_test <- confusionMatrix(predict(rf.k, d.test.imputed, type = "response"), 
                               d.test.imputed$DiagnosisByCriteria)
    specs.rf[k] <- unname(cm_test$byClass["Specificity"])
    sens.rf[k] <- unname(cm_test$byClass["Sensitivity"])
    accs.rf[k] <- unname(cm_test$overall["Accuracy"])
    balaccs.rf[k] <- unname(cm_test$byClass["Balanced Accuracy"])
    if (is.na(cm_overall)) {
      cm_overall <- cm_test$table
    } else {
      cm_overall <- cm_overall + cm_test$table
    }
  }
  aucs.p[p] <- mean(aucs.rf)
  auprs.p[p] <- mean(auprs.rf)
  aucs.sd.p[p] <- sd(aucs.rf)
  auprs.sd.p[p] <- sd(auprs.rf)
}

# Plot performance for different numbers of predictors
jpeg("varselec_dis_auroc.jpg", width = 1800, height = 1300, pointsize = 40)
plot(ps, aucs.p, xlab = "Number of Predictors", ylab = "AUROC in 10-fold CV", pch = "", 
     ylim = c(0.65, 1), xaxt = "n", main = "Diagnosis: AUROC")
# Error bars are 95% confidence t-intervals
segments(ps, aucs.p - qt(p = 0.975, df = 9) * aucs.sd.p / sqrt(10), ps, 
         aucs.p + qt(p = 0.975, df = 9) * aucs.sd.p / sqrt(10), lwd = 3)
lines(ps, aucs.p, col = "red", lwd = 3)
points(ps, aucs.p, col = "red", cex = 1.5, lwd = 3)
axis(side = 1, at = ps[seq(1, length(vars_incl), 3)])
legend("bottomright", legend = c("Average AUROC", "95% confidence interval"), 
       col = c("red", "black"), pch = c(1, NA), lwd = c(3, 3), lty = c(NA, 1))
mtext("a", side = 3, adj = 0, line = 1.2, cex = 1.2, font = 2)
dev.off()

jpeg("varselec_dis_aupr.jpg", width = 1800, height = 1300, pointsize = 40)
plot(ps, auprs.p, xlab = "Number of Predictors", ylab = "AUPR in 10-fold CV", pch = "", 
     ylim = c(0.7, 1), xaxt = "n", main = "Diagnosis: AUPR")
# Error bars are 95% confidence t-intervals
segments(ps, auprs.p - qt(p = 0.975, df = 9) * auprs.sd.p / sqrt(10), ps, 
         auprs.p + qt(p = 0.975, df = 9) * auprs.sd.p / sqrt(10), lwd = 3)
lines(ps, auprs.p, col = "red", lwd = 3)
points(ps, auprs.p, col = "red", cex = 1.5, lwd = 3)
axis(side = 1, at = ps[seq(1, length(vars_incl), 3)])
legend("bottomright", legend = c("Average AUPR", "95% confidence interval"), 
       col = c("red", "black"), pch = c(1, NA), lwd = c(3, 3), lty = c(NA, 1))
mtext("b", side = 3, adj = 0, line = 1.2, cex = 1.2, font = 2)
dev.off()


##############################################################################
########################## Treatment Assignment ##############################
##############################################################################

########################## Variables to be considered in the analysis

vars_incl <- c("Age", "BMI", "Sex", "Height", "Weight", "TreatmentGroupBinar", 
               "AlvaradoScore", "PediatricAppendicitisScore", 
               "AppendixOnSono", "AppendixDiameter", "MigratoryPain", "LowerAbdominalPainRight", #"SecondaryCondition", (not pre-treatment!)
               "ReboundTenderness", "CoughingPain", "PsoasSign", #"AbdominalGuarding", (kicked out!)
               "Nausea", "AppetiteLoss", "BodyTemp", "WBCCount", "NeutrophilPerc", 
               "KetonesInUrine", "ErythrocytesInUrine", "WBCInUrine", "CRPEntry",
               "Dysuria", "Stool", "Peritonitis", "FreeFluids", 
               "AppendixWallLayers", "Kokarde", "TissuePerfusion", #"AppendixPerforation", (this variable is operative!)
               "SurroundingTissueReaction", "PathLymphNodes",
               "MesentricLymphadenitis", "BowelWallThick", "Ileus", "FecalImpaction",
               "Meteorism", "Enteritis")
app.data.trt <- app.data[!is.na(app.data$DiagnosisByCriteria), vars_incl]


########################## Variable selection based on RF importance
ps <- 1 : (length(vars_incl) - 1)
aucs.p <- numeric(length(ps))
aucs.sd.p <- numeric(length(ps))
auprs.p <- numeric(length(ps))
auprs.sd.p <- numeric(length(ps))
bestvars <- list()
for (p in ps) {
  print(paste0("Fitting RF based on ", p, " best predictor(s)"))
  # K-fold CV
  set.seed(1799)
  K <- 10
  aucs.rf <- numeric(K)
  auprs.rf <- numeric(K)
  specs.rf <- numeric(K)
  sens.rf <- numeric(K)
  accs.rf <- numeric(K)
  balaccs.rf <- numeric(K)
  fold_membership <- sample(c(rep(1 : K, floor(nrow(app.data.trt) / K)),  
                              1 : (nrow(app.data.trt) %% K)), size = nrow(app.data.trt), 
                            replace = FALSE)
  cm_overall <- NA
  for (k in 1 : K) {
    # We perform separate imputation for train and test sets!
    train_flags <- fold_membership != k
    d.k <- app.data.trt[train_flags, ]
    d.k.imputed <- kNN(d.k[, -which(colnames(d.k) == "TreatmentGroupBinar")])[, 1 : (ncol(d.k) - 1)]
    d.k.imputed$TreatmentGroupBinar <- d.k$TreatmentGroupBinar
    rf.k <- randomForest(TreatmentGroupBinar ~ ., data = d.k.imputed, importance = TRUE)
    rf.vimp <- (importance(rf.k)[, 1] + importance(rf.k)[, 2]) / 2
    imp.vars <- names(tail(sort(rf.vimp), p))
    bestvars[[p]] <- imp.vars
    rf.k <- randomForest(TreatmentGroupBinar ~ ., data = d.k.imputed[, c(imp.vars, 
                                                                         "TreatmentGroupBinar")])
    d.test <- app.data.trt[!train_flags, ]
    d.test.imputed <- kNN(d.test[, -which(colnames(d.test) == "TreatmentGroupBinar")])[, 1 : (ncol(d.test) - 1)]
    d.test.imputed$TreatmentGroupBinar <- d.test$TreatmentGroupBinar
    pred <- prediction(predict(rf.k, d.test.imputed, type = "prob")[, 2], 
                       d.test.imputed$TreatmentGroupBinar)
    perf <- performance(pred, "tpr", "fpr")
    auc_test <- performance(pred, measure = "auc")@y.values[[1]]
    perf <- performance(pred, measure = "prec", x.measure = "rec")
    perf@y.values[[1]][1] <- 1.0
    aupr_test <- computeArea(perf@x.values[[1]], perf@y.values[[1]])
    aucs.rf[k] <- auc_test
    auprs.rf[k] <- aupr_test
    cm_test <- confusionMatrix(predict(rf.k, d.test.imputed, type = "response"), 
                               d.test.imputed$TreatmentGroupBinar)
    specs.rf[k] <- unname(cm_test$byClass["Specificity"])
    sens.rf[k] <- unname(cm_test$byClass["Sensitivity"])
    accs.rf[k] <- unname(cm_test$overall["Accuracy"])
    balaccs.rf[k] <- unname(cm_test$byClass["Balanced Accuracy"])
    if (is.na(cm_overall)) {
      cm_overall <- cm_test$table
    } else {
      cm_overall <- cm_overall + cm_test$table
    }
  }
  aucs.p[p] <- mean(aucs.rf)
  auprs.p[p] <- mean(auprs.rf)
  aucs.sd.p[p] <- sd(aucs.rf)
  auprs.sd.p[p] <- sd(auprs.rf)
}

# Plot performance for different numbers of predictors
jpeg("varselec_trt_auroc.jpg", width = 1800, height = 1300, pointsize = 40)
plot(ps, aucs.p, xlab = "Number of Predictors", ylab = "AUROC in 10-fold CV", pch = "", 
     ylim = c(0.850, 1), xaxt = "n", main = "Management: AUROC")
# Error bars are 95% confidence t-intervals
segments(ps, aucs.p - qt(p = 0.975, df = 9) * aucs.sd.p / sqrt(10), ps, 
         aucs.p + qt(p = 0.975, df = 9) * aucs.sd.p / sqrt(10), lwd = 3)
lines(ps, aucs.p, col = "red", lwd = 3)
points(ps, aucs.p, col = "red", cex = 1.5, lwd = 3)
axis(side = 1, at = ps[seq(1, length(vars_incl), 3)])
legend("bottomright", legend = c("Average AUROC", "95% confidence interval"), 
       col = c("red", "black"), pch = c(1, NA), lwd = c(3, 3), lty = c(NA, 1))
mtext("c", side = 3, adj = 0, line = 1.2, cex = 1.2, font = 2)
dev.off()

jpeg("varselec_trt_aupr.jpg", width = 1800, height = 1300, pointsize = 40)
plot(ps, auprs.p, xlab = "Number of Predictors", ylab = "AUPR in 10-fold CV", pch = "", 
     ylim = c(0.8, 1), xaxt = "n", main = "Management: AUPR")
# Error bars are 95% confidence t-intervals
segments(ps, auprs.p - qt(p = 0.975, df = 9) * auprs.sd.p / sqrt(10), ps, 
         auprs.p + qt(p = 0.975, df = 9) * auprs.sd.p / sqrt(10), lwd = 3)
lines(ps, auprs.p, col = "red", lwd = 3)
points(ps, auprs.p, col = "red", cex = 1.5, lwd = 3)
axis(side = 1, at = ps[seq(1, length(vars_incl), 3)])
legend("bottomright", legend = c("Average AUPR", "95% confidence interval"), 
       col = c("red", "black"), pch = c(1, NA), lwd = c(3, 3), lty = c(NA, 1))
mtext("d", side = 3, adj = 0, line = 1.2, cex = 1.2, font = 2)
dev.off()


##############################################################################
########################## Complicated Appendicitis ##########################
##############################################################################

########################## Variables to be considered in the analysis
vars_incl <- c("Age", "BMI", "Sex", "Height", "Weight", "AppendicitisComplications", 
               "AlvaradoScore", "PediatricAppendicitisScore", #"SecondaryCondition", (not pre-treatment!) 
               "AppendixOnSono", "AppendixDiameter", "MigratoryPain", "LowerAbdominalPainRight", 
               "ReboundTenderness", "CoughingPain", "PsoasSign", #"AbdominalGuarding", (kicked out!)
               "Nausea", "AppetiteLoss", "BodyTemp", "WBCCount", "NeutrophilPerc", 
               "KetonesInUrine", "ErythrocytesInUrine", "WBCInUrine", "CRPEntry",
               "Dysuria", "Stool", "Peritonitis", "FreeFluids", 
               "AppendixWallLayers", "Kokarde", "TissuePerfusion", #"AppendixPerforation", (this variable is operative!)
               "SurroundingTissueReaction", "PathLymphNodes",
               "MesentricLymphadenitis", "BowelWallThick", "Ileus", "FecalImpaction",
               "Meteorism", "Enteritis")
app.data.comp <- app.data[!is.na(app.data$DiagnosisByCriteria), vars_incl]


########################## Variable selection based on RF importance
ps <- 1 : (length(vars_incl) - 1)
aucs.p <- numeric(length(ps))
aucs.sd.p <- numeric(length(ps))
auprs.p <- numeric(length(ps))
auprs.sd.p <- numeric(length(ps))
bestvars <- list()
for (p in ps) {
  print(paste0("Fitting RF based on ", p, " best predictor(s)"))
  # K-fold CV
  set.seed(1799)
  K <- 10
  aucs.rf <- numeric(K)
  auprs.rf <- numeric(K)
  specs.rf <- numeric(K)
  sens.rf <- numeric(K)
  accs.rf <- numeric(K)
  balaccs.rf <- numeric(K)
  fold_membership <- sample(c(rep(1 : K, floor(nrow(app.data.comp) / K)),  
                              1 : (nrow(app.data.comp) %% K)), size = nrow(app.data.comp), 
                            replace = FALSE)
  cm_overall <- NA
  for (k in 1 : K) {
    # We perform separate imputation for train and test sets!
    train_flags <- fold_membership != k
    d.k <- app.data.comp[train_flags, ]
    d.k.imputed <- kNN(d.k[, -which(colnames(d.k) == "AppendicitisComplications")])[, 1 : (ncol(d.k) - 1)]
    d.k.imputed$AppendicitisComplications <- d.k$AppendicitisComplications
    rf.k <- randomForest(AppendicitisComplications ~ ., data = d.k.imputed, importance = TRUE)
    rf.vimp <- (importance(rf.k)[, 1] + importance(rf.k)[, 2]) / 2
    imp.vars <- names(tail(sort(rf.vimp), p))
    bestvars[[p]] <- imp.vars
    rf.k <- randomForest(AppendicitisComplications ~ ., 
                         data = d.k.imputed[, c(imp.vars, "AppendicitisComplications")])
    d.test <- app.data.comp[!train_flags, ]
    d.test.imputed <- kNN(d.test[, -which(colnames(d.test) == "AppendicitisComplications")])[, 1 : (ncol(d.test) - 1)]
    d.test.imputed$AppendicitisComplications <- d.test$AppendicitisComplications
    pred <- prediction(predict(rf.k, d.test.imputed, type = "prob")[, 2], 
                       d.test.imputed$AppendicitisComplications)
    perf <- performance(pred, "tpr", "fpr")
    auc_test <- performance(pred, measure = "auc")@y.values[[1]]
    perf <- performance(pred, measure = "prec", x.measure = "rec")
    perf@y.values[[1]][1] <- 1.0
    aupr_test <- computeArea(perf@x.values[[1]], perf@y.values[[1]])
    aucs.rf[k] <- auc_test
    auprs.rf[k] <- aupr_test
    cm_test <- confusionMatrix(predict(rf.k, d.test.imputed, type = "response"), 
                               d.test.imputed$AppendicitisComplications)
    specs.rf[k] <- unname(cm_test$byClass["Specificity"])
    sens.rf[k] <- unname(cm_test$byClass["Sensitivity"])
    accs.rf[k] <- unname(cm_test$overall["Accuracy"])
    balaccs.rf[k] <- unname(cm_test$byClass["Balanced Accuracy"])
    if (is.na(cm_overall)) {
      cm_overall <- cm_test$table
    } else {
      cm_overall <- cm_overall + cm_test$table
    }
  }
  aucs.p[p] <- mean(aucs.rf)
  auprs.p[p] <- mean(auprs.rf)
  aucs.sd.p[p] <- sd(aucs.rf)
  auprs.sd.p[p] <- sd(auprs.rf)
}

# Plot performance for different numbers of predictors
jpeg("varselec_comp_auroc.jpg", width = 1800, height = 1300, pointsize = 40)
plot(ps, aucs.p, xlab = "Number of Predictors", ylab = "AUROC in 10-fold CV", pch = "", 
     ylim = c(0.75, 1), xaxt = "n", main = "Severity: AUROC")
# Error bars are 95% confidence t-intervals
segments(ps, aucs.p - qt(p = 0.975, df = 9) * aucs.sd.p / sqrt(10), ps, 
         aucs.p + qt(p = 0.975, df = 9) * aucs.sd.p / sqrt(10), lwd = 3)
lines(ps, aucs.p, col = "red", lwd = 3)
points(ps, aucs.p, col = "red", cex = 1.5, lwd = 3)
axis(side = 1, at = ps[seq(1, length(vars_incl), 3)])
legend("bottomright", legend = c("Average AUROC", "95% confidence interval"), 
       col = c("red", "black"), pch = c(1, NA), lwd = c(3, 3), lty = c(NA, 1))
mtext("e", side = 3, adj = 0, line = 1.2, cex = 1.2, font = 2)
dev.off()

jpeg("varselec_comp_aupr.jpg", width = 1800, height = 1300, pointsize = 40)
plot(ps, auprs.p, xlab = "Number of Predictors", ylab = "AUPR in 10-fold CV", pch = "", 
     ylim = c(0.4, 1), xaxt = "n",  main = "Severity: AUPR")
# Error bars are 95% confidence t-intervals
segments(ps, auprs.p - qt(p = 0.975, df = 9) * auprs.sd.p / sqrt(10), ps, 
         auprs.p + qt(p = 0.975, df = 9) * auprs.sd.p / sqrt(10), lwd = 3)
lines(ps, auprs.p, col = "red", lwd = 3)
points(ps, auprs.p, col = "red", cex = 1.5, lwd = 3)
axis(side = 1, at = ps[seq(1, length(vars_incl), 3)])
legend("bottomright", legend = c("Average AUPR", "95% confidence interval"), 
       col = c("red", "black"), pch = c(1, NA), lwd = c(3, 3), lty = c(NA, 1))
mtext("f", side = 3, adj = 0, line = 1.2, cex = 1.2, font = 2)
dev.off()


########################## Inspect RF feature importance for the models trained on the full dataset

########################## Imputation

# Perform imputation using the kNN method
# NB: make sure to exclude the response variable from imputation!
set.seed(1799)
app.data.trt.imputed <- kNN(app.data.trt[, -which(colnames(app.data.trt) == "TreatmentGroupBinar")])[, 1 : (ncol(app.data.trt) - 1)]
app.data.trt.imputed$TreatmentGroupBinar <- app.data.trt$TreatmentGroupBinar
set.seed(1799)
app.data.dis.imputed <- kNN(app.data.dis[, -which(colnames(app.data.dis) == "DiagnosisByCriteria")])[, 1 : (ncol(app.data.dis) - 1)]
app.data.dis.imputed$DiagnosisByCriteria <- app.data.dis$DiagnosisByCriteria
set.seed(1799)
app.data.comp.imputed <- kNN(app.data.comp[, -which(colnames(app.data.comp) == "AppendicitisComplications")])[, 1 : (ncol(app.data.comp) - 1)]
app.data.comp.imputed$AppendicitisComplications <- app.data.comp$AppendicitisComplications

########################## Diagnosis
set.seed(1799)
# Fit the RF
rf.dis <- randomForest(DiagnosisByCriteria ~ ., data = app.data.dis.imputed, importance = TRUE)
# Importances
rf.vimp <- (importance(rf.dis)[, 1] + importance(rf.dis)[, 2]) / 2
(imp.vars <- names(tail(sort(rf.vimp), 3)))

# Perform bootstrapping, to see which variables have beeen selected most frequently
set.seed(1799)
B <- 300
freqs_b <- numeric(ncol(app.data.dis.imputed) - 1)
names(freqs_b) <- row.names(rf.dis$importance)
for (b in 1 : B) {
  idx <- 1 : nrow(app.data.dis.imputed)
  idx_b <- sample(idx, size=length(idx), replace = TRUE)
  app.data.dis.imputed_b <- app.data.dis.imputed[idx_b, ]
  rf_b <- randomForest(DiagnosisByCriteria ~ ., data = app.data.dis.imputed_b, importance = TRUE)
  imps_b <- (rf_b$importance[, 1] + rf_b$importance[, 2]) / 2
  sel_b <- which(names(freqs_b) %in% names(tail(sort(imps_b), 3)))
  freqs_b[sel_b] <- freqs_b[sel_b] + 1 
}
(freqs_b <- freqs_b / B)
sort(freqs_b[freqs_b > 0])

# Look at subsets of different sizes
set.seed(1799)
q_max <- ncol(app.data.dis.imputed) - 1
freqs <- matrix(nrow = q_max, ncol = ncol(app.data.dis.imputed))
colnames(freqs) <- colnames(app.data.dis.imputed)
for (q in 1 : q_max) {
  print(q)
  freqs[q, ] <- rfVarSelection(DiagnosisByCriteria ~ ., data = app.data.dis.imputed, q = q, B = 100)
}
heatmap.2(freqs, Rowv = FALSE, Colv = FALSE, dendrogram = "none", trace = "none", 
          sepcolor = "black", colsep = 1 : ncol(freqs), rowsep = 1 : nrow(freqs), 
          sepwidth = c(0.01, 0.01), margins = c(12, 8))

########################## Treatment
set.seed(1799)
# Fit the RF
rf.trt <- randomForest(TreatmentGroupBinar ~ ., data = app.data.trt.imputed, importance = TRUE)
# Importances
rf.vimp <- (importance(rf.trt)[, 1] + importance(rf.trt)[, 2]) / 2
(imp.vars <- names(tail(sort(rf.vimp), 14)))

# Perform bootstrapping, to see which variables have beeen selected most frequently
set.seed(1799)
B <- 300
freqs_b <- numeric(ncol(app.data.trt.imputed) - 1)
names(freqs_b) <- row.names(rf.trt$importance)
for (b in 1 : B) {
  idx <- 1 : nrow(app.data.trt.imputed)
  idx_b <- sample(idx, size=length(idx), replace = TRUE)
  app.data.trt.imputed_b <- app.data.trt.imputed[idx_b, ]
  rf_b <- randomForest(TreatmentGroupBinar ~ ., data = app.data.trt.imputed_b, importance = TRUE)
  imps_b <- (rf_b$importance[, 1] + rf_b$importance[, 2]) / 2
  sel_b <- which(names(freqs_b) %in% names(tail(sort(imps_b), 14)))
  freqs_b[sel_b] <- freqs_b[sel_b] + 1 
}
(freqs_b <- freqs_b / B)
sort(freqs_b[freqs_b > 0])

# Look at subsets of different sizes
set.seed(1799)
q_max <- ncol(app.data.trt.imputed) - 1
freqs <- matrix(nrow = q_max, ncol = ncol(app.data.trt.imputed))
colnames(freqs) <- colnames(app.data.trt.imputed)
for (q in 1 : q_max) {
  print(q)
  freqs[q, ] <- rfVarSelection(TreatmentGroupBinar ~ ., data = app.data.trt.imputed, q = q, B = 100)
}
heatmap.2(freqs, Rowv = FALSE, Colv = FALSE, dendrogram = "none", trace = "none", 
          sepcolor = "black", colsep = 1 : ncol(freqs), rowsep = 1 : nrow(freqs), 
          sepwidth = c(0.01, 0.01), margins = c(12, 8))

########################## Complicated appendicitis
set.seed(1799)
# Fit the RF
rf.comp <- randomForest(AppendicitisComplications ~ ., data = app.data.comp.imputed, importance = TRUE)
# Importances
rf.vimp <- (importance(rf.comp)[, 1] + importance(rf.comp)[, 2]) / 2
(imp.vars <- names(tail(sort(rf.vimp), 11)))

# Perform bootstrapping, to see which variables have beeen selected most frequently
set.seed(1799)
B <- 300
freqs_b <- numeric(ncol(app.data.comp.imputed) - 1)
names(freqs_b) <- row.names(rf.comp$importance)
for (b in 1 : B) {
  idx <- 1 : nrow(app.data.comp.imputed)
  idx_b <- sample(idx, size=length(idx), replace = TRUE)
  app.data.comp.imputed_b <- app.data.comp.imputed[idx_b, ]
  rf_b <- randomForest(AppendicitisComplications ~ ., data = app.data.comp.imputed_b, importance = TRUE)
  imps_b <- (rf_b$importance[, 1] + rf_b$importance[, 2]) / 2
  sel_b <- which(names(freqs_b) %in% names(tail(sort(imps_b), 11)))
  freqs_b[sel_b] <- freqs_b[sel_b] + 1 
}
(freqs_b <- freqs_b / B)
sort(freqs_b[freqs_b > 0])

# Look at subsets of different sizes
set.seed(1799)
q_max <- ncol(app.data.comp.imputed) - 1
freqs <- matrix(nrow = q_max, ncol = ncol(app.data.comp.imputed))
colnames(freqs) <- colnames(app.data.comp.imputed)
for (q in 1 : q_max) {
  print(q)
  freqs[q, ] <- rfVarSelection(AppendicitisComplications ~ ., data = app.data.comp.imputed, q = q, B = 100)
}
heatmap.2(freqs, Rowv = FALSE, Colv = FALSE, dendrogram = "none", trace = "none", 
          sepcolor = "black", colsep = 1 : ncol(freqs), rowsep = 1 : nrow(freqs), 
          sepwidth = c(0.01, 0.01), margins = c(12, 8))
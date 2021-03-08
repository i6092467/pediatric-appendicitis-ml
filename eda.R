# This sript performs some basic exploratory data analysis

########################## Load data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("app_data_clean.Rda")


########################## Summary statistics

str(app.data)
summary(app.data)


########################## Summary statistics for the paper
# Full set of variables used in analsis
vars_incl <- c("DiagnosisByCriteria", "TreatmentGroupBinar", "AppendicitisComplications",
               "Age", "BMI", "Sex", "Height", "Weight", 
               "AlvaradoScore", "PediatricAppendicitisScore",
               "AppendixOnSono", "AppendixDiameter", "MigratoryPain", "LowerAbdominalPainRight", 
               "ReboundTenderness", "CoughingPain", "PsoasSign",
               "Nausea", "AppetiteLoss", "BodyTemp", "WBCCount", "NeutrophilPerc", 
               "KetonesInUrine", "ErythrocytesInUrine", "WBCInUrine", "CRPEntry",
               "Dysuria", "Stool", "Peritonitis", "FreeFluids", 
               "AppendixWallLayers", "Kokarde", "TissuePerfusion",
               "SurroundingTissueReaction", "PathLymphNodes",
               "MesentricLymphadenitis", "BowelWallThick", "Ileus", "FecalImpaction",
               "Meteorism", "Enteritis")
app.data.full <- app.data[!is.na(app.data$DiagnosisByCriteria), vars_incl]

# Disease
app.data.dis <- app.data.full[, -which(colnames(app.data.full) == "TreatmentGroupBinar" | 
                                         colnames(app.data.full) == "AppendicitisComplications")]
summary(app.data.dis[app.data.dis$DiagnosisByCriteria == "appendicitis", ])
summary(app.data.dis[app.data.dis$DiagnosisByCriteria == "noAppendicitis", ])
s.dis <- summaryStats(data = app.data.dis[, -which(colnames(app.data.dis) == "DiagnosisByCriteria")], 
             y = app.data.dis$DiagnosisByCriteria, seed = 1799)
s.dis$pvals

# Treatment
app.data.trt <- app.data.full[, -which(colnames(app.data.full) == "DiagnosisByCriteria" | 
                                         colnames(app.data.full) == "AppendicitisComplications")]
summary(app.data.trt[app.data.trt$TreatmentGroupBinar == "surgical", ])
summary(app.data.trt[app.data.trt$TreatmentGroupBinar == "conservative", ])
s.trt <- summaryStats(data = app.data.trt[, -which(colnames(app.data.trt) == "TreatmentGroupBinar")], 
                      y = app.data.trt$TreatmentGroupBinar, seed = 1799)
s.trt$pvals

# Complications
app.data.comp <- app.data.full[, -which(colnames(app.data.full) == "DiagnosisByCriteria" | 
                                         colnames(app.data.full) == "TreatmentGroupBinar")]
summary(app.data.comp[app.data.comp$AppendicitisComplications == "yes", ])
summary(app.data.comp[app.data.comp$AppendicitisComplications == "no", ])
s.comp <- summaryStats(data = app.data.comp[, -which(colnames(app.data.comp) == "AppendicitisComplications")], 
                      y = app.data.comp$AppendicitisComplications, seed = 1799)
s.comp$pvals


########################## Percentages of missing values

(napercs <- sapply(app.data, function(x) { return(sum(is.na(x)) / length(x) * 100) }))
oldpars <- par()
par(mar = c(15, 5, 5, 5))
barplot(napercs, las=2, ylim = c(0, 100), ylab = "Percentage of Missing Values, %")
abline(a = 100, b = 0, lty = 2, col = "red")
par(mar = oldpars$mar)


########################## Response variables

# Treatment group: conservative therapy vs surgical
plot(app.data$TreatmentGroup)
# Create a binarised version of this variable (secondary surgical group is too small)
# Ask collaborators if the secondary surgical treatment can be considered surgicla?
app.data$TreatmentGroupBinar <- app.data$TreatmentGroup
app.data$TreatmentGroupBinar <- as.character(app.data$TreatmentGroupBinar)
app.data$TreatmentGroupBinar[app.data$TreatmentGroupBinar == "primarySurgical" | app.data$TreatmentGroupBinar == "secondarySurgical"] <- "surgical"
app.data$TreatmentGroupBinar <- as.factor(app.data$TreatmentGroupBinar)
plot(app.data$TreatmentGroupBinar, xlab = "Treatment")
summary(app.data$TreatmentGroupBinar)

# Appendicitis classification: with or without complications?
plot(app.data$AppendicitisComplications, xlab = "Complications?")
summary(app.data$AppendicitisComplications)

# Diagnosis: appendicitis vs. no appendicitis?
plot(factor(as.numeric(app.data$DiagnosisByCriteria), levels = c(1, 2), labels = c("appendicitis", "no appendicitis")), xlab = "Diagnosis (according to criteria)")
summary(app.data$DiagnosisByCriteria)


########################## Dimensionality reduction
# Include only relevant variables
vars_incl <- c("Age", "BMI", "Sex", "Height", "Weight", 
               "AlvaradoScore", "PediatricAppendicitisScore", 
               "AppendixOnSono", "AppendixDiameter", "MigratoryPain", "LowerAbdominalPainRight", #"SecondaryCondition", (not pre-treatment!)
               "ReboundTenderness", "CoughingPain", "PsoasSign", #"AbdominalGuarding",(kicked out!)
               "Nausea", "AppetiteLoss", "BodyTemp", "WBCCount", "NeutrophilPerc", 
               "KetonesInUrine", "ErythrocytesInUrine", "WBCInUrine", "CRPEntry",
               "Dysuria", "Stool", "Peritonitis", "FreeFluids", "FreeFluidsLoc", 
               "AppendixWallLayers", "Kokarde", "TissuePerfusion", # "AppendixPerforation", (this variable is operative!)
               "SurroundingTissueReaction", "PerityphliticAbscess", "PathLymphNodes",
               "MesentricLymphadenitis", "BowelWallThick", "Ileus", "FecalImpaction",
               "Meteorism", "Enteritis", "GynaecAbnorm")
app.data.imputed <- app.data[!is.na(app.data$DiagnosisByCriteria), vars_incl]
# Impute missing values
set.seed(1799)
app.data.imputed <- kNN(app.data.imputed)[, 1 : ncol(app.data.imputed)]
# Compute Gower's distances
d.Gower <- daisy(app.data.imputed)
# MDS
mds.fit <- cmdscale(d = d.Gower, k = 2)
# Plot by diagnosis
plot(mds.fit, col = "white", pch = 19, xlab = "Dimension 1", ylab = "Dimension 2")
points(mds.fit[as.numeric(app.data$DiagnosisByCriteria[!is.na(app.data$DiagnosisByCriteria)]) == 1,], col = "orange", pch = 19, cex = 0.75)
points(mds.fit[as.numeric(app.data$DiagnosisByCriteria[!is.na(app.data$DiagnosisByCriteria)]) == 2,], col = "blue", pch = 19, cex = 0.75)
legend(x = "bottomleft", legend = c("Appendictis", "No appendictis"), pch = 19, col = c("orange", "blue"), title = "Diagnosis according to criteria:")
# Plot by treatment among appendicitis patients
plot(mds.fit, col = "white", pch = 19, xlab = "Dimension 1", ylab = "Dimension 2")
points(mds.fit[app.data$TreatmentGroupBinar[!is.na(app.data$DiagnosisByCriteria)] == "conservative",], col = "orange", pch = 19, cex = 0.75)
points(mds.fit[app.data$TreatmentGroupBinar[!is.na(app.data$DiagnosisByCriteria)] == "surgical",], col = "blue", pch = 19, cex = 0.75)
legend(x = "bottomleft", legend = c("Conservative", "Surgical"), pch = 19, col = c("orange", "blue"), title = "Treatment:")
# Plot by complications among appendicitis patients
plot(mds.fit, col = "white", pch = 19, xlab = "Dimension 1", ylab = "Dimension 2")
points(mds.fit[app.data$AppendicitisComplications[!is.na(app.data$DiagnosisByCriteria)] == "no",], col = "orange", pch = 19, cex = 0.75)
points(mds.fit[app.data$AppendicitisComplications[!is.na(app.data$DiagnosisByCriteria)] == "yes",], col = "blue", pch = 19, cex = 0.75)
legend(x = "bottomleft", legend = c("no", "yes"), pch = 19, col = c("orange", "blue"), title = "Complicated appendicitis:")

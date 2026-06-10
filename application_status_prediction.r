
library(tidyverse)
library(pROC)
set.seed(1337)

setwd("D:/YOUR WORKING FOLDER HERE")

df = read.csv("TEC-2002-LOAN-DATA.csv", header = TRUE)
# ── DATA EXPLORATION ─────────────────────────────────────────
#Overall structure
str(df)
summary(df)

# ── ERROR CHECK START ─────────────────────────────────────────
## how many people with unknown Year of birth?
print(sum(df['yob']==99))
print(nrow(df))
## 7 out of 1225
## Any NA in the dataset?
for (i in df) {
  print(sum(is.na(i)))
}
## Any false entries?
aes_check = c('V',
              'W',
              'M',
              'P',
              'B',
              'R',
              'E',
              'T',
              'U',
              'N',
              'Z')
aes_check_count = 0
for (i in unique(df['aes'])) {
  aes_check_count = aes_check_count + i%in% aes_check
}
aes_check_count = sum(aes_check_count)
aes_check_count;nrow(unique(df['aes']))
### Match!

res_check = c('O',
              'F',
              'U',
              'P',
              'N',
              'Z')
res_check_count = 0
for (i in unique(df['res'])) {
  res_check_count = res_check_count + i%in% res_check
}
res_check_count = sum(res_check_count)
res_check_count;nrow(unique(df['res']))
### Match!

## 000001 anywhere?
sort(unique(df$dhval))
sort(unique(df$dmort))

sort(unique(df$doutm))
sort(unique(df$doutl))
sort(unique(df$douthp))
sort(unique(df$doutcc))

sum(sort(unique(df$dhval)) >0 & sort(unique(df$dhval)) <1)
sum(sort(unique(df$dmort)) >0 & sort(unique(df$dmort)) <1)
sum(sort(unique(df$doutm)) >0 & sort(unique(df$doutm)) <1)
sum(sort(unique(df$doutl)) >0 & sort(unique(df$doutl)) <1)
sum(sort(unique(df$douthp))>0 & sort(unique(df$douthp))<1)
sum(sort(unique(df$doutcc))>0 & sort(unique(df$doutcc))<1)
# No 000001 anywhere or anything similar

## No NA, errors
# ── ERROR CHECK END ─────────────────────────────────────────

#Remove 7 records with unknown year of birth (yob == 99)
df = df[-which(df$yob == 99), ]

#Overview
cat("n =", nrow(df), "| bad rate =", round(mean(df$bad), 4), "\n")
#bad=0: 898,  bad=1: 320
#heavily unbalanced (~26% default rate)

# ── DATA PREPARATION ─────────────────────────────────────────
df$yob = df$yob + 1900-2002 # turn year of birth into age
df$yob  = df$yob - mean(df$yob)   # centre age
df$phon = factor(df$phon)
df$nkid = factor(df$nkid)

# ── OUTLIER CONTROL ─────────────────────────────────────────
## Distribution of numeric vars
num_var_names = c(
  'Spouse\'s income',
  'Applicant\'s income',
  'Value of Home',
  'Mortgage balance outstanding',
  'Outgoings on mortgage or rent',
  'Outgoings on Loans',
  'Outgoings on Hire Purchase',
  'Outgoings on credit cards'
)
par(mfrow = c(3, 3))
plot(density(df$sinc), main="Distribution of \nSpouse's income (raw)") 
plot(density(df$dainc), main="Distribution of \nApplicant's income (raw)") 
plot(density(df$dhval), main="Distribution of \nValue of Home (raw)") 
plot(density(df$dmort), main="Distribution of \nMortgage balance outstanding (raw)") 
plot(density(df$doutm), main="Distribution of \nOutgoings on mortgage or rent (raw)") 
plot(density(df$doutl), main="Distribution of \nOutgoings on Loans (raw)") 
plot(density(df$douthp), main="Distribution of \nOutgoings on Hire Purchase (raw)")
plot(density(df$doutcc), main="Distribution of \nOutgoings on credit cards (raw)")
par(mfrow = c(1, 1))

df_numerics = cbind(
  df$sinc,
  df$dainc,
  df$dhval,
  df$dmort,
  df$doutm,
  df$doutl,
  df$douthp,
  df$doutcc)
colnames(df_numerics) = c(
  'sinc',
  'dainc',
  'dhval',
  'dmort',
  'doutm',
  'doutl',
  'douthp',
  'doutcc'
)
df_numerics = as.data.frame(df_numerics)
probs = c(0.25,0.5,0.75,0.99,1)

quantile_table=matrix(data = NA, nrow = length(df_numerics), ncol = length(probs))
rownames(quantile_table) = colnames(df_numerics)
colnames(quantile_table)=1:5

for (i in 1:length(probs)) {
  colnames(quantile_table)[i] = paste0(probs[i]*100,'%')
}

for (i in 1:length(df_numerics)) {
  quantile_table[i,] = quantile(df_numerics[,i], probs = probs)
}
par(mfrow = c(3, 3))
for (i in 1:length(df_numerics)) {
  boxplot(df_numerics[i], show.names=TRUE, main=num_var_names[i])
}
par(mfrow = c(1, 1))
rm(df_numerics,probs)
## Asymmetric, very long tails
## Variables dainc, dhval, dmort have no strong outliers
## dmort also acceptable

## Create a dataset that does not contain terrible outliers
num_vars_intact = c(
  'dainc',
  'dhval',
  'dmort',
  'doutm'
)

cutoff_percentile = subset(quantile_table,!rownames(quantile_table) %in% num_vars_intact)
cutoff_percentile = cutoff_percentile[,4]

df_pretrain = df

## Cut-off all the observations that do not belong to 99% percentile
for (i in labels(cutoff_percentile)) {
  cat("Items to remove at",i,": ")
  eval(parse(text=(paste0("print(sum(df_pretrain$",i,">=",as.numeric(cutoff_percentile[i]),"))",sep=""))))
  eval(parse(text=(paste0("df_pretrain = df_pretrain[df_pretrain$",i,"<",as.numeric(cutoff_percentile[i]),",]",sep=""))))
}
nrow(df)-nrow(df_pretrain)

par(mfrow = c(2, 4))
for (i in labels(cutoff_percentile)) {
  boxplot(df[i], show.names=TRUE, main=labels(cutoff_percentile)[i])
  title(main = "Before")
  boxplot(df_pretrain[i], show.names=TRUE, main=labels(cutoff_percentile)[i])
  title(main = "After")
}
par(mfrow = c(2, 4))
for (i in labels(cutoff_percentile)) {
  eval(parse(text=(paste0("plot(density(df$",i,"),main = 'Before')",sep=""))))
  eval(parse(text=(paste0("plot(density(df_pretrain$",i,"),main = 'After')",sep=""))))
}
par(mfrow = c(1, 1))

df = df_pretrain

# ── STRATIFIED TRAIN / TEST SPLIT  80 / 20 ───────────────────
# Stratify by 'bad' so both splits have the same default rate.
idx_bad1 = which(df$bad == 1)
idx_bad0 = which(df$bad == 0)

train_idx = c(
  sample(idx_bad1, size = floor(0.80 * length(idx_bad1)), replace = FALSE),
  sample(idx_bad0, size = floor(0.80 * length(idx_bad0)), replace = FALSE)
)
df_train = df[ train_idx, ]
df_test  = df[-train_idx, ]

cat(sprintf("Train n=%d | bad rate=%.4f\n", nrow(df_train), mean(df_train$bad)))
cat(sprintf("Test  n=%d | bad rate=%.4f\n", nrow(df_test),  mean(df_test$bad)))


# ============================================================
# PART 1 - full, UNBALANCED training data
# ============================================================
## Logit (all variables)
mylogit = glm(bad ~ ., 
              family = binomial, 
              data = df_train)
summary(mylogit)
### select variables with Pr(>|z|) below 0.2
mylogit_varnames_filter = summary(mylogit)$coefficients
mylogit_varnames_filter = subset(mylogit_varnames_filter, mylogit_varnames_filter[,4]<=0.2)
print(round(mylogit_varnames_filter,4))

## Logit 2 (first iteration)
mylogit2 = glm(bad ~ yob + phon + aes + dainc + res + doutm, 
               family = binomial, 
               data = df_train)
summary(mylogit2)
### select variables with Pr(>|z|) below 0.1
mylogit_varnames_filter = summary(mylogit2)$coefficients
mylogit_varnames_filter = subset(mylogit_varnames_filter, mylogit_varnames_filter[,4]<=0.1)
print(round(mylogit_varnames_filter,4))

# Variables retained from stepwise filtering (Pr < 0.1):
# yob, sinc, dainc, res, doutcc

mylogit3 = glm(bad ~ yob + dainc + res + doutm,
               family = binomial,
               data   = df_train)
summary(mylogit3)

# Predict on test set (type = "response" => probabilities)
pred_logit3 = predict(mylogit3, newdata = df_test, type = "response")


# ============================================================
# BOOTSTRAPPING - balanced logit ensemble
# ============================================================
# Strategy: for each of B iterations, draw a BALANCED bootstrap
# sample (equal n from bad=1 and bad=0 with replacement), fit
# the same logit specification, and collect the predicted probs
# on the held-out test set. Final prediction = average across B.
#
# This forces each sub-model to learn from a 50/50 dataset,
# preventing the majority-class bias that causes the plain
# logit to almost never predict a default at threshold = 0.5.
# ─────────────────────────────────────────────────────────────

B = 500   # number of bootstrap models

train_bad1 = df_train[df_train$bad == 1, ]
train_bad0 = df_train[df_train$bad == 0, ]
n_min      = nrow(train_bad1)   # minority class size in train

cat(sprintf("\nBootstrap setup: B=%d | n_min=%d | n_maj=%d\n",
            B, n_min, nrow(train_bad0)))

# Matrix to collect one column of test-set predictions per model
boot_pred_matrix = matrix(NA_real_, nrow = nrow(df_test), ncol = B)

for (b in seq_len(B)) {
  # Balanced resample: n_min rows from each class, with replacement
  idx1 = sample(nrow(train_bad1), size = n_min, replace = TRUE)
  idx0 = sample(nrow(train_bad0), size = n_min, replace = TRUE)
  df_boot = rbind(train_bad1[idx1, ], train_bad0[idx0, ])
  
  # Same formula as mylogit3
  boot_fit = glm(bad ~ yob + dainc + res + doutm,
                 family  = binomial,
                 data    = df_boot,
                 control = list(maxit = 100))
  
  boot_pred_matrix[, b] = predict(boot_fit,
                                  newdata = df_test,
                                  type    = "response")
}

# Ensemble prediction: average the 500 probability columns
pred_boot_avg = rowMeans(boot_pred_matrix, na.rm = TRUE)


# ============================================================
# MODEL COMPARISON - test-set evaluation
# ============================================================

# Define function to return confusion matrix data,
# accuracy/sensitivity/specificity, F1, Brier
eval_model = function(actual, probs, threshold, label) {
  pred_class = as.integer(probs >= threshold)
  cm   = table(factor(actual,     levels = 0:1),
               factor(pred_class, levels = 0:1))
  TN   = cm[1, 1]; FP = cm[1, 2]
  FN   = cm[2, 1]; TP = cm[2, 2]
  acc  = (TP + TN) / sum(cm)
  sens = TP / (TP + FN)
  spec = TN / (TN + FP)
  prec = ifelse((TP + FP) > 0, TP / (TP + FP), 0)
  f1   = ifelse((prec + sens) > 0, 2 * prec * sens / (prec + sens), 0)
  brier = mean((actual - probs)^2)
  cat(sprintf(
    "  %-42s  Acc=%.3f  Sens=%.3f  Spec=%.3f  F1=%.3f  Brier=%.4f\n",
    label, acc, sens, spec, f1, brier))
  cat(sprintf("    Confusion: TN=%d  FP=%d  FN=%d  TP=%d\n",
              TN, FP, FN, TP))
  invisible(list(acc=acc, sens=sens, spec=spec, f1=f1, brier=brier,
                 TN=TN, FP=FP, FN=FN, TP=TP))
}

# ── ROC / AUC ──────────────────────────────────────────────
roc_logit3 = roc(df_test$bad, pred_logit3,  quiet = TRUE)
roc_boot   = roc(df_test$bad, pred_boot_avg, quiet = TRUE)

cat(sprintf("\n%-42s  AUC = %.4f\n", "mylogit3",          auc(roc_logit3)))
cat(sprintf("%-42s  AUC = %.4f\n",  "Bootstrap ensemble", auc(roc_boot)))

# ── Youden-optimal threshold ─────────────────────────────────
# Maximizes sensitivity + specificity - 1
thresh_l3   = coords(roc_logit3, "best",
                     best.method = "youden", ret = "threshold")[[1]]
thresh_boot = coords(roc_boot,   "best",
                     best.method = "youden", ret = "threshold")[[1]]

cat(sprintf("\nYouden thresholds:  mylogit3=%.3f   bootstrap=%.3f\n",
            thresh_l3, thresh_boot))

# ── Fixed threshold = 0.5 ───────────────────────────────────
cat("\n========== Fixed threshold = 0.5 ==========\n")
r1 = eval_model(df_test$bad, pred_logit3,  0.5, "mylogit3  (t=0.50)")
r2 = eval_model(df_test$bad, pred_boot_avg, 0.5, "Bootstrap (t=0.50)")

# ── Youden-optimal thresholds ────────────────────────────────
cat(sprintf("\n========== Youden-optimal threshold (l3=%.3f, boot=%.3f) ==========\n",
            thresh_l3, thresh_boot))
r3 = eval_model(df_test$bad, pred_logit3,
                thresh_l3,   sprintf("mylogit3  (t=%.3f)", thresh_l3))
r4 = eval_model(df_test$bad, pred_boot_avg,
                thresh_boot, sprintf("Bootstrap (t=%.3f)", thresh_boot))

# ── Summary table ─────────────────────────────────────────────
cat("\n========== FINAL SUMMARY (Youden-optimal) ==========\n")
cat(sprintf("  %-10s  %10s  %10s  %10s\n",
            "Metric", "mylogit3", "Bootstrap", "Better"))
cat(strrep("-", 50), "\n")

for (k in c("acc", "sens", "spec", "f1", "brier")) {
  v3 = r3[[k]]; vb = r4[[k]]
  better = if (k == "brier") {
    if (vb < v3) "= Boot" else if (v3 < vb) "logit3 ->" else "Tie"
  } else {
    if (vb > v3) "= Boot" else if (v3 > vb) "logit3 ->" else "Tie"
  }
  cat(sprintf("  %-10s  %10.4f  %10.4f  %10s\n", k, v3, vb, better))
}
cat(sprintf("  %-10s  %10.4f  %10.4f  %10s\n",
            "AUC",
            as.numeric(auc(roc_logit3)),
            as.numeric(auc(roc_boot)),
            if (auc(roc_boot) > auc(roc_logit3)) "= Boot" else "logit3 ->"))


# ── ROC curve plot ────────────────────────────────────────────
par(mfrow = c(1, 1))
plot(roc_logit3,
     col  = "steelblue", lwd = 2,
     main = "ROC Curves: mylogit3 vs Bootstrap Ensemble",
     xlab = "1 - Specificity (FPR)", ylab = "Sensitivity (TPR)")
lines(roc_boot, col = "tomato", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "grey60")
legend("bottomright",
       legend = c(
         sprintf("mylogit3      AUC = %.4f", auc(roc_logit3)),
         sprintf("Bootstrap avg AUC = %.4f", auc(roc_boot))),
       col    = c("steelblue", "tomato"),
       lwd    = 2, bty = "n")

# ── Calibration: predicted probability histograms ─────────────
# Shows HOW the two models distribute their predicted probabilities.
# The unbalanced model clusters near ~0.27 (mean of training bad
# rate), so almost nothing exceeds 0.5 => poor sensitivity at t=0.5.
# The bootstrap ensemble spreads predictions around 0.5 correctly.
par(mfrow = c(1, 2))
hist(pred_logit3,  breaks = 30, col = "steelblue",
     main = "mylogit3: predicted probs", xlab = "P(default)")
abline(v = 0.5, col = "red", lty = 2)
hist(pred_boot_avg, breaks = 30, col = "tomato",
     main = "Bootstrap avg: predicted probs", xlab = "P(default)")
abline(v = 0.5, col = "red", lty = 2)

# ── Key takeaways ─────────────────────────────────────────────
# 0. The starting dataset was already well-prepared.
# 1. AUC: Bootstrap ensemble is marginally better (0.6328 vs 0.6285).
# 2. Brier score: mylogit3 is better (0.1872 vs 0.2295) - its probs
#    are closer on average to 0/1, but only because they are pulled
#    toward the majority-class base rate, NOT because the model is 
#    better calibrated for credit risk decisions.
# 3. At threshold 0.5 (the practical default):
#    - mylogit3 catches less than 10% of actual defaults (Sens=0.095).
#    - Bootstrap ensemble catches more than 50% (Sens=0.508).
#    This is the most important difference: without threshold tuning,
#    the unbalanced model is nearly useless for identifying defaulters.
# 4. At Youden-optimal threshold, the two models are nearly equivalent
#    in all metrics, with the bootstrap ensemble having a slight edge
#    in AUC, accuracy, specificity, and F1.
# CONCLUSION: The bootstrap ensemble is the better model for credit
# default prediction. It is more robust (no threshold tuning needed),
# achieves higher AUC, and correctly trades off sensitivity/specificity
# across all threshold choices.


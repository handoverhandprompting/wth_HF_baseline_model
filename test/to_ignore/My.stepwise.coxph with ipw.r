My.stepwise.coxph <- function (Time = NULL, T1 = NULL, T2 = NULL, Status = NULL, variable.list, 
    in.variable = "NULL", data, weights = "NULL", sle = 0.15, sls = 0.15, vif.threshold = 999,
    cluster = "NULL") 
{

    # The default of 'robust' is TRUE if there is a 'cluster' argument.

    if (!require(car)) install.packages("car", dependencies=TRUE)
    library(car)

    if (!require(survival)) install.packages("survival", dependencies=TRUE)
	library(survival)

    if (!require(lmtest)) install.packages("lmtest", dependencies=TRUE)
	library(lmtest)

    check.list.factor <- NULL
    check.list.character <- NULL
    check.list <- NULL
    for (i in (1:length(variable.list))) {
    check.list.factor[i] <- is.factor(data[, variable.list[i]])
    check.list.character[i] <- is.character(data[, variable.list[i]])
    check.list[i] <- sum(check.list.factor[i], check.list.character[i])
    }
    
    if (sum(check.list)!=0) 
    stop ("Please define dummy variables manually for your categorical variables on the variable list. \n  Then, re-run the stepwise variable selection procedure. \n  Specifically, you can define a dummy variable for each level of a categorical variable with \n  more than two levels using the 'ifelse()' function. \n  By doing so, the 'best' reference category for the categorical variable with more than two \n  levels would be identified during the stepwise variable selection procedure.")

    univar.pvalue <- NULL
    temp.model <- NULL

if (weights=="NULL"){

  if (cluster!="NULL"){

    if (is.null(T2)) {
        initial.model <- eval(bquote(
                           coxph(as.formula(paste("Surv(", Time, 
                           ", ", Status, ") ~ ", paste(in.variable, collapse = "+"), 
                           sep = "")), data = data, method = "efron", cluster = .(as.name(cluster)))
                         ))
    } else if (is.null(Time)) {
        initial.model <- eval(bquote(
                           coxph(as.formula(paste("Surv(", T1, 
                           ", ", T2, ", ", Status, ") ~ ", paste(in.variable, 
                           collapse = "+"), sep = "")), data = data, method = "efron", cluster = .(as.name(cluster)))
                         ))
    }
  } else if (cluster=="NULL"){

    if (is.null(T2)) {
        initial.model <- coxph(as.formula(paste("Surv(", Time, 
                           ", ", Status, ") ~ ", paste(in.variable, collapse = "+"), 
                           sep = "")), data = data, method = "efron")
    } else if (is.null(Time)) {
        initial.model <- coxph(as.formula(paste("Surv(", T1, 
                           ", ", T2, ", ", Status, ") ~ ", paste(in.variable, 
                           collapse = "+"), sep = "")), data = data, method = "efron")
    }
  }
} else if (weights!="NULL"){

  if (cluster!="NULL"){

    if (is.null(T2)) {
        initial.model <- eval(bquote(
                           coxph(as.formula(paste("Surv(", Time, 
                           ", ", Status, ") ~ ", paste(in.variable, collapse = "+"), 
                           sep = "")), data = data, method = "efron", weights = .(as.name(weights)),
                           cluster = .(as.name(cluster)))
                         ))
    } else if (is.null(Time)) {
        initial.model <- eval(bquote(
                           coxph(as.formula(paste("Surv(", T1, 
                           ", ", T2, ", ", Status, ") ~ ", paste(in.variable, 
                           collapse = "+"), sep = "")), data = data, method = "efron",
                           weights = .(as.name(weights)), cluster = .(as.name(cluster)))
                         ))
    }
  } else if (cluster=="NULL"){

    if (is.null(T2)) {
        initial.model <- eval(bquote(
                           coxph(as.formula(paste("Surv(", Time, 
                           ", ", Status, ") ~ ", paste(in.variable, collapse = "+"), 
                           sep = "")), data = data, method = "efron", weights = .(as.name(weights)))
                         ))
    } else if (is.null(Time)) {
        initial.model <- eval(bquote(
                           coxph(as.formula(paste("Surv(", T1, 
                           ", ", T2, ", ", Status, ") ~ ", paste(in.variable, 
                           collapse = "+"), sep = "")), data = data, method = "efron",
                           weights = .(as.name(weights)))
                         ))
    }
  }
}

    if (is.null(initial.model$coefficients)) {

if (weights=="NULL"){

  if (cluster!="NULL"){

        for (i in 1:length(variable.list)) {
            if (is.null(T2)) {
                uni.model <- eval(bquote(
                               coxph(as.formula(paste("Surv(", 
                               Time, ", ", Status, ") ~ ", variable.list[i], 
                               sep = "")), data = data, method = "efron", cluster = .(as.name(cluster)))
                             ))
            }
            if (is.null(Time)) {
                uni.model <- eval(bquote(
                               coxph(as.formula(paste("Surv(", 
                               T1, ", ", T2, ", ", Status, ") ~ ", variable.list[i], 
                               sep = "")), data = data, method = "efron", cluster = .(as.name(cluster)))
                             ))
            }
            univar.pvalue[i] <- summary(uni.model)$coefficients[5]
        }
        variable.list1 <- variable.list[univar.pvalue <= 0.9 & !is.na(univar.pvalue)]
        if (length(variable.list1)!=0) {
           univar.pvalue1 <- univar.pvalue[univar.pvalue <= 0.9 & !is.na(univar.pvalue)]
           uni.x <- variable.list1[which.min(univar.pvalue1)]
           } else {
           univar.pvalue1 <- NULL
           uni.x <- NULL
           }

        if (length(uni.x) > 0) {
            if (is.null(T2)) {
                formula <- as.formula(paste("Surv(", Time, ", ", 
                  Status, ") ~ ", uni.x, sep = ""))
                temp.model <- eval(bquote(
                                coxph(formula, data = data, method = "efron", cluster = .(as.name(cluster)))
                              ))
                if (length(temp.model$coefficients) > 1) {
                  print(vif(lm(paste(Status, paste(names(temp.model$coefficients), 
                    collapse = "+"), sep = "~"), data = data)))
                }
            }
            if (is.null(Time)) {
                formula <- as.formula(paste("Surv(", T1, ", ", 
                  T2, ", ", Status, ") ~ ", uni.x, sep = ""))
                temp.model <- eval(bquote(
                                coxph(formula, data = data, method = "efron", cluster = .(as.name(cluster)))
                              ))
            }

            cat("# --------------------------------------------------------------------------------------------------\n")
            cat("# Initial Model:\n")
            print(summary(temp.model))
        }

  } else if (cluster=="NULL"){

        for (i in 1:length(variable.list)) {
            if (is.null(T2)) {
                uni.model <- coxph(as.formula(paste("Surv(", 
                               Time, ", ", Status, ") ~ ", variable.list[i], 
                               sep = "")), data = data, method = "efron")
            }
            if (is.null(Time)) {
                uni.model <- coxph(as.formula(paste("Surv(", 
                               T1, ", ", T2, ", ", Status, ") ~ ", variable.list[i], 
                               sep = "")), data = data, method = "efron")
            }
            univar.pvalue[i] <- summary(uni.model)$coefficients[5]
        }
        variable.list1 <- variable.list[univar.pvalue <= 0.9 & !is.na(univar.pvalue)]
        if (length(variable.list1)!=0) {
           univar.pvalue1 <- univar.pvalue[univar.pvalue <= 0.9 & !is.na(univar.pvalue)]
           uni.x <- variable.list1[which.min(univar.pvalue1)]
           } else {
           univar.pvalue1 <- NULL
           uni.x <- NULL
           }

        if (length(uni.x) > 0) {
            if (is.null(T2)) {
                formula <- as.formula(paste("Surv(", Time, ", ", 
                  Status, ") ~ ", uni.x, sep = ""))
                temp.model <- coxph(formula, data = data, method = "efron")
                if (length(temp.model$coefficients) > 1) {
                  print(vif(lm(paste(Status, paste(names(temp.model$coefficients), 
                    collapse = "+"), sep = "~"), data = data)))
                }
            }
            if (is.null(Time)) {
                formula <- as.formula(paste("Surv(", T1, ", ", 
                  T2, ", ", Status, ") ~ ", uni.x, sep = ""))
                temp.model <- coxph(formula, data = data, method = "efron")
            }

            cat("# --------------------------------------------------------------------------------------------------\n")
            cat("# Initial Model:\n")
            print(summary(temp.model))
        }

  }

} else if (weights!="NULL"){

  if (cluster!="NULL"){

        for (i in 1:length(variable.list)) {
            if (is.null(T2)) {
                uni.model <- eval(bquote(
                               coxph(as.formula(paste("Surv(", 
                               Time, ", ", Status, ") ~ ", variable.list[i], 
                               sep = "")), data = data, method = "efron", weights = .(as.name(weights)),
                               cluster = .(as.name(cluster)))
                             ))
            }
            if (is.null(Time)) {
                uni.model <- eval(bquote(
                               coxph(as.formula(paste("Surv(", 
                               T1, ", ", T2, ", ", Status, ") ~ ", variable.list[i], 
                               sep = "")), data = data, method = "efron", weights = .(as.name(weights)),
                               cluster = .(as.name(cluster)))
                             ))
            }
            univar.pvalue[i] <- summary(uni.model)$coefficients[5]
        }
        variable.list1 <- variable.list[univar.pvalue <= 0.9 & !is.na(univar.pvalue)]
        if (length(variable.list1)!=0) {
           univar.pvalue1 <- univar.pvalue[univar.pvalue <= 0.9 & !is.na(univar.pvalue)]
           uni.x <- variable.list1[which.min(univar.pvalue1)]
           } else {
           univar.pvalue1 <- NULL
           uni.x <- NULL
           }

        if (length(uni.x) > 0) {
            if (is.null(T2)) {
                formula <- as.formula(paste("Surv(", Time, ", ", 
                  Status, ") ~ ", uni.x, sep = ""))
                temp.model <- eval(bquote(
                                coxph(formula, data = data, method = "efron", weights = .(as.name(weights)),
                                cluster = .(as.name(cluster)))
                              ))
                if (length(temp.model$coefficients) > 1) {
                  print(vif(lm(paste(Status, paste(names(temp.model$coefficients), 
                    collapse = "+"), sep = "~"), data = data)))
                }
            }
            if (is.null(Time)) {
                formula <- as.formula(paste("Surv(", T1, ", ", 
                  T2, ", ", Status, ") ~ ", uni.x, sep = ""))
                temp.model <- eval(bquote(
                                coxph(formula, data = data, method = "efron", weights = .(as.name(weights)),
                                cluster = .(as.name(cluster)))
                              ))
            }

            cat("# --------------------------------------------------------------------------------------------------\n")
            cat("# Initial Model:\n")
            print(summary(temp.model))
        }

  } else if (cluster=="NULL"){

        for (i in 1:length(variable.list)) {
            if (is.null(T2)) {
                uni.model <- eval(bquote(
                               coxph(as.formula(paste("Surv(", 
                               Time, ", ", Status, ") ~ ", variable.list[i], 
                               sep = "")), data = data, method = "efron", weights = .(as.name(weights)))
                             ))
            }
            if (is.null(Time)) {
                uni.model <- eval(bquote(
                               coxph(as.formula(paste("Surv(", 
                               T1, ", ", T2, ", ", Status, ") ~ ", variable.list[i], 
                               sep = "")), data = data, method = "efron", weights = .(as.name(weights)))
                               ))
            }
            univar.pvalue[i] <- summary(uni.model)$coefficients[5]
        }
        variable.list1 <- variable.list[univar.pvalue <= 0.9 & !is.na(univar.pvalue)]
        if (length(variable.list1)!=0) {
           univar.pvalue1 <- univar.pvalue[univar.pvalue <= 0.9 & !is.na(univar.pvalue)]
           uni.x <- variable.list1[which.min(univar.pvalue1)]
           } else {
           univar.pvalue1 <- NULL
           uni.x <- NULL
           }

        if (length(uni.x) > 0) {
            if (is.null(T2)) {
                formula <- as.formula(paste("Surv(", Time, ", ", 
                  Status, ") ~ ", uni.x, sep = ""))
                temp.model <- eval(bquote(
                                coxph(formula, data = data, method = "efron", weights = .(as.name(weights)))
                              ))
                if (length(temp.model$coefficients) > 1) {
                  print(vif(lm(paste(Status, paste(names(temp.model$coefficients), 
                    collapse = "+"), sep = "~"), data = data)))
                }
            }
            if (is.null(Time)) {
                formula <- as.formula(paste("Surv(", T1, ", ", 
                  T2, ", ", Status, ") ~ ", uni.x, sep = ""))
                temp.model <- eval(bquote(
                                coxph(formula, data = data, method = "efron", weights = .(as.name(weights)))
                              ))
            }

            cat("# --------------------------------------------------------------------------------------------------\n")
            cat("# Initial Model:\n")
            print(summary(temp.model))
        }

  }
}
    } else if (!is.null(initial.model$coefficients)) {
        temp.model <- initial.model
        cat("# --------------------------------------------------------------------------------------------------\n")
        cat("# Initial Model:\n")
        print(summary(temp.model))
    }
    if (length(temp.model$coefficients) > 1) {
        cat("---------------------------- Variance Inflating Factor (VIF) ----------------------------", "\n")
        cat("Multicollinearity Problem: VIF > 10 (Continuous Variable) or > 2.5 (Categorical Variable)\n")
        print(vif(lm(paste(Status, paste(names(temp.model$coefficients), 
            collapse = "+"), sep = "~"), data = data)))
    }
    i <- 0
    break.rule <- TRUE
    while (break.rule) {
        i <- i + 1
        if (i == 1) {
            variable.list2 <- setdiff(variable.list, all.vars(temp.model$formula))
        } else {
            variable.list2 <- setdiff(variable.list, c(all.vars(temp.model$formula), 
                out.x))
            out.x <- NULL
        }
        if (length(variable.list2) != 0) {
            lr.pvalue <- NULL
            mv.pvalue <- NULL
            vif.value <- NULL
            for (k in 1:length(variable.list2)) {
                model <- update(temp.model, as.formula(paste(". ~ . + ", 
                  variable.list2[k], sep = "")))
                if (length(model$coefficients) > 1) {
                  if (sum(is.na(model$coefficients)) != 0) {
                    lr.pvalue[k] <- 1
                    mv.pvalue[k] <- 1
                    vif.value[k] <- 999
                  } else {
                    lr.pvalue[k] <- lrtest(temp.model, model)["Pr(>Chisq)"][2, 
                      1]
                    mv.pvalue[k] <- summary(model)$coefficients[nrow(summary(model)$coefficients), 
                      "Pr(>|z|)"]
                    model.vif <- vif(glm(as.formula(paste(Status, 
                      paste(names(model$coefficients), collapse = "+"), 
                      sep = "~")), data = data, family = binomial(link = "logit")))
                    vif.value[k] <- model.vif[length(model.vif)]
                  }
                }
            }
            variable.list2.1 <- variable.list2[mv.pvalue <= 0.9 & !is.na(mv.pvalue) & vif.value <= vif.threshold]

            if (length(variable.list2.1)!=0) {
               lr.pvalue2 <- lr.pvalue[mv.pvalue <= 0.9 & !is.na(mv.pvalue) & vif.value <= vif.threshold]
               mv.pvalue2 <- mv.pvalue[mv.pvalue <= 0.9 & !is.na(mv.pvalue) & vif.value <= vif.threshold]
               enter.x <- variable.list2.1[lr.pvalue2 == min(lr.pvalue2, na.rm = TRUE) & lr.pvalue2 <= sle]
               wald.p <- mv.pvalue2[lr.pvalue2 == min(lr.pvalue2, na.rm = TRUE) & lr.pvalue2 <= sle]
               } else {
               lr.pvalue2 <- NULL
               mv.pvalue2 <- NULL
               enter.x <- NULL
               wald.p <- NULL
               }

            if (length(setdiff(enter.x, NA)) != 0) {
                if (length(enter.x) > 1) {
                  enter.x <- enter.x[which.min(wald.p)]
                }
                cat("# --------------------------------------------------------------------------------------------------", 
                  "\n")
                cat(paste("### iter num = ", i, ", Forward Selection by LR Test: ", 
                  "+ ", enter.x, sep = ""), "\n")
                temp.model <- update(temp.model, as.formula(paste(". ~ . + ", 
                  enter.x, sep = "")))
                print(summary(temp.model))
                cat("---------------------------- Variance Inflating Factor (VIF) ----------------------------", "\n")
                cat("Multicollinearity Problem: VIF > 10 (Continuous Variable) or > 2.5 (Categorical Variable)\n")
                if (length(temp.model$coefficients) > 1) {
                  print(vif(lm(paste(Status, paste(names(temp.model$coefficients), 
                    collapse = "+"), sep = "~"), data = data)))
                }
            }
        } else {
            enter.x <- NULL
        }
        if (i == 1 & length(enter.x) == 0) {
            cat("# ==================================================================================================", 
                "\n")
            cat(paste("*** Stepwise Final Model (sle = ", 
                sle, "; Variable selection restricted in VIF ≤ ", 
                vif.threshold, "):", sep = ""), "\n")
            print(summary(temp.model))
            break
        } else {
            variable.list3 <- setdiff(rownames(summary(temp.model)$coefficients), 
                c(enter.x, in.variable))
            if (length(variable.list3) != 0) {
                lr.pvalue <- NULL
                for (k in 1:length(variable.list3)) {
                  model <- update(temp.model, as.formula(paste(". ~ . - ", 
                    variable.list3[k], sep = "")))
                    lr.pvalue[k] <- lrtest(temp.model, model)["Pr(>Chisq)"][2, 
                      1]
                }
                out.x <- variable.list3[lr.pvalue == max(lr.pvalue, 
                  na.rm = TRUE) & lr.pvalue > sls]
                out.x <- setdiff(out.x, NA)
                if (length(out.x) != 0) {
                  if (length(out.x) > 1) {
                    out.x.1 <- out.x
                    for (j in 1:length(out.x)) {
                      out.x[j] <- out.x.1[(length(out.x) - j + 
                        1)]
                    }
                    wald.p <- rep(NA, length(out.x))
                    for (j in 1:length(out.x)) {
                      wald.p[j] <- summary(temp.model)$coefficients[, 
                        "Pr(>|z|)"][rownames(summary(temp.model)$coefficients) == 
                        out.x[j]]
                    }
                    out.x <- out.x[which.max(wald.p)]
                  }
                  cat("# --------------------------------------------------------------------------------------------------", 
                    "\n")
                  cat(paste("### iter num = ", i, ", Backward Selection by Wald's Test: ", 
                    "- ", out.x, sep = ""), "\n")
                  temp.model <- update(temp.model, as.formula(paste(". ~ . - ", 
                    out.x, sep = "")))
                  print(summary(temp.model))
                  cat("---------------------------- Variance Inflating Factor (VIF) ----------------------------", "\n")
                  cat("Multicollinearity Problem: VIF > 10 (Continuous Variable) or > 2.5 (Categorical Variable)\n")
                  if (length(temp.model$coefficients) > 1) {
                    print(vif(lm(paste(Status, paste(names(temp.model$coefficients), 
                      collapse = "+"), sep = "~"), data = data)))
                  }
                }
            } else {
                out.x <- NULL
            }
        }
        if ((length(enter.x) + length(out.x)) == 0) {
            final.model <- temp.model
            cat("# ==================================================================================================", 
                "\n")
            cat(paste("*** Stepwise Final Model (sle = ", 
                sle, "; sls = ", sls, "; Variable selection restricted in VIF ≤ ", 
                vif.threshold, "):", sep = ""), "\n")
            print(summary(final.model))
            cat("---------------------------- Variance Inflating Factor (VIF) ----------------------------", "\n")
            cat("Multicollinearity Problem: VIF > 10 (Continuous Variable) or > 2.5 (Categorical Variable)\n")
            if (length(final.model$coefficients) > 1) {
                print(vif(lm(paste(Status, paste(names(final.model$coefficients), 
                  collapse = "+"), sep = "~"), data = data)))
            }
            break.rule <- FALSE
        }
        enter.x <- NULL
    }
}
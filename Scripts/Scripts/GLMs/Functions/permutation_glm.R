#permutation models
permutation = function(dip = "shoot_type",
                       predictors = c("Length", "abs_median_distance"),
                       perm = "parent_rank_node",
                       data = met,
                       family = "binomial") {
  predictors1 = append(predictors, 1)
  #create the formula
  formula = formula(paste("data[[dip]] ~ ", paste(predictors1, collapse = " + ")))
  #null model
  null_1 = glm(formula, data = data, family = family)
  #difference: real_aic-null_aic
  dif = model$aic - null_1$aic
  #new df
  met_nul = data
  #empty df for permutations
  df = data.frame(matrix(nrow = 0, ncol = 0))
  #create the formula 2
  predictors2 = append(predictors, perm)
  formula2 = formula(paste("met_nul[[dip]] ~ ", paste(predictors2, collapse = " + ")))
  #permuting length 10.000 times
  for (i in 1:10000) {
    met_nul[[perm]] = sample(data[[perm]])
    permut = glm(formula2, data = met_nul, family = family)
    a = permut$aic - null_1$aic
    b = a < dif#se a(diff con il modello permutato)<diff(diff con modello reale) allora significa che la permutazione spiega meglio il modello
    r = cbind(i, a, b)
    df = rbind(df, r)
  }
  #variable for how many times perm_aic-null_aic was < of real_aic-null_aic
  better_perm = length(which(df$b == 1))#times better perm!!!
  assign("better_perm", better_perm, envir = .GlobalEnv)
}

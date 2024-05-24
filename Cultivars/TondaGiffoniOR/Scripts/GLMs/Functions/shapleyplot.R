#shapley plot
shapley.plot = function(x_var, y_var, data) {
  x_train <- data[, x_var]
  y_train <- data[, y_var]
  test <- shapleyvalue(y_train, x_train)
  print(test)
  value = as.matrix(test[2, ])
  barplot(value, names.arg = colnames(value), cex.names = 0.6)
  
}

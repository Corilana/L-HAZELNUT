#proportion of different thinks in PROLEPTIC vs SYLLEPTIC
prop = function(df, freq){
  rel_freq = prop.table(freq,1)
  rel_freq = as.matrix(rel_freq)
  cols=palette()[nrow(rel_freq)]
  br = barplot(height = rel_freq, ylim = c(0,1),beside = T, col = c("grey",cols),ylab = "proportion")
  for (i in 1:ncol(br)){
    #test per differenze
    freq = as.data.frame.matrix(freq)
    freq$sum = rowSums(freq)
    #are there differences between class length?
    test=prop.test(freq[1:nrow(freq),i], freq[1:nrow(freq),ncol(freq)])
    if (test$p.value<=0.05) {
      lab = "*"
      print(test)
    }
    if (test$p.value<=0.01){
      lab = "**"
      print(test)
    }
    if (test$p.value<=0.001){
      lab = "***"
      print(test)
    }
    if (test$p.value>0.05) {
      lab = NA
    }
    text(x = mean(br[,i]),y = mean(rel_freq[,i])+0.07,labels = lab, cex = 1.5)
  }
  legend("topright",legend = row.names(rel_freq), fill = c("grey",cols))
}

#count buds from fate
count_buds = function(df){
  nline = nrow(df)
  columns = levels(df$fate)
  buds = data.frame(matrix(data = NA,ncol = length(columns),nrow = nline))
  names(buds) = columns
  for (i in 1:nline) {
    fate = as.numeric(df[i,"fate"])
    buds[i, fate] = 1
  }
  df = cbind(df, buds)
  return(df)
}


#fai la somma, per ogni simulazione, del numero di gemme DATI SIMULATED
sim_bud_sum = function(df){
  buds_nb_sim = data.frame()
  new_df = data.frame()
  nb_sim = sort(unique(df$simulation_nb))
  for (sim in nb_sim) {
    dt = droplevels(all_sim[all_sim$simulation_nb==sim,c(((ncol(df)-3):ncol(df)))])
    new_df = data.frame(sim)
    sums = t(colSums(dt,na.rm = T))
    new_df = cbind(new_df, sums)
    buds_nb_sim = rbind(buds_nb_sim,new_df)
  }
  return(buds_nb_sim)
}

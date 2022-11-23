#logigram:"how many nodes"
#AIM: relationship between length and number of nodes 
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")
#data visualization
plot(node~Length, shoot)
#MODEL
model = nls(node ~ a * (Length ^ b) + 0,
               data = shoot,
               start = c(a = 1, b = 0.5))
summary(model)
# visualize
lines(seq(0,75,1), predict(model, data.frame(Length=seq(0,75,1))))
#save outputs
out=capture.output(summary(model))
cat("1_length~#nodes", out, file="Outputs/Tables/1_length~#nodes.txt", sep="\n")

#graph
png("Outputs/Plots/1_length~#nodes.png",
    width = 1200,
    height = 900,
    res = 150)
with(
  shoot,
  plot(
    shoot$node ~ shoot$Length,
    pch = 19,
    main = "length(cm) vs length(node)",
    xlab = "length(cm)",
    ylab = "length(node)"
  ),
)
lines(seq(min(shoot$Length), max(shoot$Length), 1),
      predict(model, newdata = data.frame(Length = seq(
        min(shoot$Length), max(shoot$Length), 1
      ))),
      lwd = 5)
dev.off()
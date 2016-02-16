library("devtools")

devtools::use_build_ignore("devPlaygroun.R", pkg = paste(path, "rrrsa", sep = ""))

path <- "/Users/benpeloquin/Desktop/Projects/"
devtools::document(paste(path, "rrrsa", sep = ""))

devtools::install(pkg=paste(path, "rrrsa", sep = ""))
m <- matrix(data = c(1.0, 0.0, 0.0, 0.0, 0.0,
                     0.0, 0.2, 0.3, 0.25, 0.25,
                     0.0, 0.0, 0.0, 0.3, 0.7), nrow = 5)
costs <- matrix(data = c(rep(0.1, 5),
                         rep(0.9, 5),
                         rep(0.2, 5)),
                         nrow = 5)
colnames(m) <- c("none", "some", "all")
rownames(m) <- 1:5
colnames(costs) <- c("none", "some", "all")
rownames(costs) <- 1:5
rrrsa::reason(m, costs = costs, 4)
rrrsa::reason(m, 4)

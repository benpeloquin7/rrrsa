# devPlayground.R
# testing environment for rrrsa functionality

##########
rm(list = ls())
library("devtools")
path <- "/Users/benpeloquin/Desktop/Projects/"
# devtools::use_testthat(pkg = paste(path, "rrrsa", sep = ""))
devtools::use_build_ignore("devPlayground.R", pkg = paste(path, "rrrsa", sep = ""))
devtools::load_all(pkg = paste(path, "rrrsa", sep = ""))
devtools::document(paste(path, "rrrsa", sep = ""))
devtools::install(pkg=paste(path, "rrrsa", sep = ""))

########## practice data
# matrix
# ------
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

# df :: tidied
# -------------
df <- data.frame(scales = rep("some_all", 10),
                stars = as.factor(rep(1:5, 2)),
                degrees = c(rep("strong", 5), rep("weak", 5)),
                speaker.p = c(0, 0, 0, 0.3, 0.7,
                              0, 0.1, 0.15, 0.35, 0.40),
                pragmatics = c(0, 0, 0, 0.15, 0.85,
                              0, 0.1, 0.25, 0.5, 0.15))

# df :: exp8 semantics
# --------------------
d <- practiceData
head(d)
pData <- rrrsa::processData(d,
                            group = "scale",
                            item = "degree",
                            quantity = "stars",
                            semantics = "speaker.p")
rrrsa::run_rrrsa(pData)


# demo 2.19.16
# ------------
# 1) user tidied (long) dat
d <- practiceData
head(d)
# 2) process data
pData <- rrrsa::processData(d,
                            group = "scale",
                            item = "degree",
                            quantity = "stars",
                            semantics = "speaker.p")
str(pData)
head(pData$runData)
identical(pData$originalData, d)

# 3) print semantics?
rrrsa::plotSemantics(pData)

# 4) run RSA, default hyper-params
rrrsa::run_rrrsa(pData)

# 5) run RSA, adjust alpha
rrrsa::run_rrrsa(pData, alpha = 6)

# 6) run RSA, adjust recursive depth
rrrsa::run_rrrsa(pData, depth = 3)

# 7) run RSA, adjust both
rrrsa::run_rrrsa(pData, depth = 3, alpha = 4)


plotSemantics(pData)
debug(plotSemantics)
rrrsa::plotSemantics(pData)

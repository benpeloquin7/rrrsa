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
devtools::install(pkg=paste(path, "rrrsa", sep = ""), build_vignettes = TRUE)
devtools::check(pkg = paste(path, "rrrsa", sep = ""))
devtools::build(pkg = paste(path, "rrrsa", sep = ""))


install.packages("/Users/benpeloquin/Desktop/Projects/rrrsa_0.0.0.9000.tar.gz", repos = NULL, type = "source")

remove.packages("rrrsa")
installed.packages()
library(rrrsa)

rrrsa::rsa.runDf()
plyr::ddply(.data = peloquinFrank_5Alts, .variables = c("scale"), rsa.runDf,
            quantityVarName = "stars", semanticsVarName = "speaker.p",
            itemVarName = "words")

checkWords <- c("some", "all", "good", "excellent", "liked", "loved", "memorable", "unforgettable",
                "palatable", "delicious")
results <- rsa.tuneDepthAlpha(peloquinFrank_2Alts, quantityVarName = "stars", semanticsVarName = "speaker.p",
                   itemVarName = "words", groupName = "scale", compareDataName = "e11",
                   compareItems = checkWords, alphas = seq(1, 4, by = 0.1))
best <- which.max(unlist(lapply(results, function(i) i[[1]][1])))
results[[best]]


vignette("summary", package = "rrrsa")

########## practice data
# matrix
# ------
m <- matrix(data = c(1.0, 0.0, 0.0, 0.0, 0.0,
                     0.0, 0.25, 0.25, 0.25, 0.25,
                     0.0, 0.0, 0.0, 0.0, 1.0), nrow = 5)

costs <- c("none" = 4, "some" = 4, "all" = 3)
colnames(m) <- c("none", "some", "all")
rownames(m) <- 1:5


?mtcars

### 3.1.15
### Improving rsa.fullRecursion
undebug(rsa.fullRecursion)
debug(rsa.fullRecursion)
rsa.fullRecursion(m)
all(rsa.fullRecursion(rsa.fullRecursion(m)) == rsa.reason(m, depth = 2))


runData <- df %>%
  select(stars, degrees, speaker.p) %>%
  spread(degrees, speaker.p) %>%
  select(strong, weak) %>%
  data.matrix()
priors <- c(0.1, 0.1, 0.4, 0.3, 0.2)
rrrsa::rsa.reason(runData, priors = priors)
rrrsa::rsa.reason(runData)


# df :: tidied
# -------------
df <- data.frame(scales = rep("some_all", 10),
                stars = as.factor(rep(1:5, 2)),
                degrees = c(rep("strong", 5), rep("weak", 5)),
                speaker.p = c(0, 0, 0, 0.3, 0.7,
                              0, 0.1, 0.15, 0.35, 0.40),
                pragmatics = c(0, 0, 0, 0.15, 0.85,
                              0, 0.1, 0.25, 0.5, 0.15))
df2 <- data.frame(scales = rep("some_all", 15),
                  stars = as.factor(rep(1:5, 3)),
                  words = c(rep("all", 5), rep("some", 5), rep("none", 5)),
                  speaker.p = c(rep(0.0, 4), 1.0,
                                0.0, rep(0.25, 4),
                                1.0, rep(0.0, 4)),
                  pragmatics = c(0, 0, 0, 0.15, 0.85,
                                 0, 0.1, 0.25, 0.5, 0.15,
                                 rep(0.25, 5)))
df3 <- data.frame(scales = rep("some_all", 15),
                  stars = as.factor(rep(1:5, 3)),
                  starsChar = as.factor(rep(c("uno", "dos", "tres", "quatro", "cinco"), 3)),
                  words = c(rep("all", 5), rep("some", 5), rep("nonezzz", 5)),
                  speaker.p = c(rep(0.0, 4), 1.0,
                                0.0, rep(0.25, 4),
                                1.0, rep(0.0, 4)),
                  pragmatics = c(0, 0, 0, 0.15, 0.85,
                                 0, 0.1, 0.25, 0.5, 0.15,
                                 rep(0.25, 5)))
df4 <- data.frame(scales = c(rep("some_all", 10), rep("good_excellent", 10)),
                  stars = as.factor(rep(1:5, 4)),
                  words = c(rep("all", 5), rep("some", 5), c(rep("excellent", 5), rep("good", 5))),
                  speaker.p = c(rep(0.0, 4), 1.0,
                                0.0, rep(0.25, 4),
                                rep(0.0, 4), 1.0,
                                0.0, rep(0.25, 4)),
                  pragmatics = c(0, 0, 0, 0.15, 0.85,
                                 0, 0.1, 0.25, 0.5, 0.15,
                                 0, 0, 0, 0.15, 0.85,
                                 0, 0.1, 0.25, 0.5, 0.15))

## add costs and priors
df3 <- df3 %>%
  mutate(cost = 0,
         priors = 0.20)
df4 <- df4 %>%
  mutate(cost = stringr::str_length(words),
         priors = 0.20)


################# using ddply #################
################# KEEP THIS #################
group <- "scales"
d <- plyr::ddply(df4, c(group), rsa.runDf, quantityVarName = "stars",
      semanticsVarName = "speaker.p", itemVarName = "words", costsVarName = "cost", depth = 2)
################# KEEP THIS #################
################# KEEP THIS #################
group <- "scale"
dataWithPreds <- plyr::ddply(subset(peloquinFrankData, exp == "e12"), c("scale"), rsa.runDf, quantityVarName = "stars",
            semanticsVarName = "speaker.p", itemVarName = "words", alpha = 2.3, depth = 1)
checkWords <- c("some", "all", "good", "excellent", "liked", "loved", "memorable", "unforgettable",
                "palatable", "delicious")
filteredWords <- dataWithPreds %>%
  filter(words %in% checkWords)

cor(filteredWords$e6, filteredWords$preds)



for (scale in unique(df4$scales)) {
  print(rsa.runDf(subset(df4, scales == scale), quantityVarName = "stars", semanticsVarName = "speaker.p",
            itemVarName = "words"))
}


items <- "words"
quantity <- "stars"
semantics <- "speaker.p"
costs <- "cost"
priors <- "priors"

debug(rsa.runDf)
test1 <- rsa.runDf(df3, quantityVarName = "starsChar", semanticsVarName = "speaker.p",
                  itemVarName = "words", costsVarName = "cost", priorsVarName = "priors")


n# select costs
cs <- df3 %>%
  select_(items, costs) %>%
  unique()
costsVec <- cs[ , costs]
names(costsVec) <- cs[ , items]


test2 <- data.frame(test1) %>%
  mutate(quantity = rownames(.)) %>%
  gather(item, "preds", -quantity) %>%
  rename(item = item)
rename(test2, quantity = eval(quantity))
names(test2)[which(names(test2) == "quantity")] <- quantity


debug(rsa.renameCol)
rsa.renameCol(test2, c("quantity", "item"), c(quantity, item))


d <- practiceData
d_some <- d %>% filter(scale == "some_all")

rsa.runDf(d_some, quantityVarName = "stars", semanticsVarName = "speaker.p",
          itemVarName = "degree", depth = 10, alpha = 4)
debug(rsa.runDf)

d <- plyr::ddply(d, c("scale"), rsa.runDf, quantityVarName = "stars",
                 semanticsVarName = "speaker.p", itemVarName = "degree", depth = 1, alpha = 1)

with(d, cor(speaker.p, preds))
names(practiceData)






## KEEP EVERYTHING BELOW




##################################################################
## rrrsa data preparation to include with package
##################################################################

########################
## literal listener data
l0 <- read.csv("/Users/benpeloquin/Desktop/Projects/scalar_implicature/models/model_data/RawLiteralListenerCombined.csv")

ne8 <- l0 %>%
  filter(exp == "e8") %>%
  group_by(workerid) %>%
  summarise(n = 1) %>%
  nrow()
ne10 <- l0 %>%
  filter(exp == "e10") %>%
  group_by(workerid) %>%
  summarise(n = 1) %>%
  nrow()
ne12 <- l0 %>%
  filter(exp == "e12") %>%
  group_by(workerid) %>%
  summarise(n = 1) %>%
  nrow()

literalD <- l0 %>%
  group_by(exp, scale, degree, stars) %>%
  summarise(cnt.judgment = sum(judgment))
literalD$speaker.p <- with(literalD,
                           ifelse(exp == "e8", cnt.judgment / ne8,
                                  ifelse(exp == "e10",
                                         cnt.judgment / ne10, cnt.judgment / ne12)))
literalD$words <- mapply(lookupScalar, literalD$degree, literalD$scale, literalD$exp)
drop <- c("cnt.judgment", "degree")
literalD <- literalD[, !(names(literalD) %in% drop)]

##########################
## pragmatic listener data
l1 <- read.csv("/Users/benpeloquin/Desktop/Projects/scalar_implicature/models/model_data/RawPragmaticListenerCombined.csv")

N_JUDGMENTS_e6 <- 41
N_JUDGMENTS_e11 <- 43
pragmaticD <- l1 %>%
  group_by(exp, scale, degree, stars) %>%
  summarise(cnt.judgment = n())
pragmaticD$listener.p <- with(pragmaticD,
                              ifelse(exp == "e6", cnt.judgment / N_JUDGMENTS_e6,
                                     cnt.judgment / N_JUDGMENTS_e11))
pragmaticD$words <- mapply(lookupScalar, pragmaticD$degree, pragmaticD$scale, pragmaticD$exp)
drop <- c("degree", "cnt.judgment")
pragmaticD <- pragmaticD[, !(names(pragmaticD) %in% drop)]
pragmaticD <- pragmaticD %>%
  spread(exp, listener.p)

## join into new df
peloquinFrankData <- left_join(literalD, pragmaticD) %>%
  mutate(e11 = ifelse(is.na(e11), 0, e11),
         e6 = ifelse(is.na(e6), 0, e6))
## normalize data
for (e in unique(peloquinFrankData$exp)) {
  for (word in unique(peloquinFrankData$words)) {
    if (word %in% subset(peloquinFrankData, exp == e)$words) {
      peloquinFrankData[peloquinFrankData$words == word & peloquinFrankData$exp == e, "speaker.p"] <-
        rrrsa::rsa.normVec(subset(peloquinFrankData, exp == e & words == word)$speaker.p)
    }
  }
}

## saving three different data sets for the R package
peloquinFrank_2Alts <- as.data.frame(subset(peloquinFrankData, exp == "e8"))
peloquinFrank_4Alts <- subset(peloquinFrankData, exp == "e10")
peloquinFrank_5Alts <- subset(peloquinFrankData, exp == "e12")
# devtools::use_data(peloquinFrank_2Alts, pkg = ".")
# devtools::use_data(peloquinFrank_4Alts, pkg = ".")
# devtools::use_data(peloquinFrank_5Alts, pkg = ".")


compareItems <- c("some", "all", "good", "excellent", "liked", "loved", "memorable", "unforgettable",
                  "palatable", "delicious")
debug(rsa.tuneDepthAlpha)
cors <- rsa.tuneDepthAlpha(peloquinFrank_5Alts,
                   quantityVarName = "stars",
                   semanticsVarName = "speaker.p", groupName = "scale",
                   itemVarName = "words", alphas = seq(2, 3, by=0.1), compareItems = compareItems,
                   compareDataName = "e11")
which.max(unlist(lapply(cors, function(i) i[[1]][1])))

rsa.tuneDepthAlpha(data = peloquinFrank_5Alts,
                   quantityVarName = "stars",
                   itemVarName =  "words",
                   semanticsVarName = "speaker.p",
                   compareDataName = "e11",
                   compareItems = compareItems,
                   alphas = 2,
                   groupName = "scale")


###############################################################
## LookupScalar helper
###############################################################
lookupScalar <- function(degree, scale, exp) {
  if (scale == "some_all") {
    if (exp == "e12") {
      if (degree == "hi1") return("all")
      if (degree == "hi2") return("most")
      if (degree == "mid") return("some")
      if (degree == "low1") return("little")
      if (degree == "low2") return("none")
    } else if (exp == "e10" | exp == "e11") {
      if (degree == "hi1") return("all")
      if (degree == "hi2") return("most")
      if (degree == "low1") return("some")
      if (degree == "low2") return("none")
    } else { ## must be exp 8
      if (degree == "hi") return("all")
      if (degree == "low") return("some")
    }
    # liked_loved
  } else if (scale == "liked_loved") {
    if (degree == "hi1" | degree == "hi") return("loved")
    if (degree == "hi2" | degree == "low") return("liked")
    if (degree == "mid") return("indifferent")
    if (degree == "low1") return("disliked")
    if (degree == "low2") return("hated")
    # good_excellent
  } else if (scale == "good_excellent") {
    if (degree == "hi1" | degree == "hi") return("excellent")
    if (degree == "hi2" | degree == "low") return("good")
    if (degree == "mid") return("okay")
    if (degree == "low1") return("bad")
    if (degree == "low2") return("horrible")
    # paltable_delicious
  } else if (scale == "palatable_delicious") {
    if (degree == "hi1" | degree == "hi") return("delicious")
    if (degree == "hi2" | degree == "low") return("palatable")
    if (degree == "mid") return("mediocre")
    if (degree == "low1") return("gross")
    if (degree == "low2") return("disgusting")
    # memorable_unforgettable
  } else if (scale == "memorable_unforgettable") {
    if (degree == "hi1" | degree == "hi") return("unforgettable")
    if (degree == "hi2" | degree == "low") return("memorable")
    if (degree == "mid") return("ordinary")
    if (degree == "low1") return("bland")
    if (degree == "low2") return("forgettable")
  }
  NA
}

#' d_pragmods
#'
#' Data from "Rational speech act models of pragmatic reasoning in reference games" Frank, et al. (Under Review)
#'
#' `condition` :: specific condition corresponding to `expt`.
#'
#' `expt` :: specific experimental data. See Table 1 in Frank et al. (Under Review).
#'
#' `matrix` :: type of literal semantics matrix. See fig. 9 in Frank et al. (Under Review) for an example.
#'
#' `prior` :: type of prior for current row.
#'
#' `query` :: target query corresponding to `p` and `speaker.p`
#'
#' `object` :: type of object being queried
#'
#' `count` :: number of times participants selected a queried object in this experiment
#'
#' `p` :: proportion of times participants selected a queried object in this experiment. This is the value we try to predict with `rsa`
#'
#' `n` :: sample size
#'
#' `cih` and `cil` :: high/low 95% CIs on `p`
#'
#' `priorType` :: type of prior
#'
#' `priorValue` :: proportion of times choosing `priortype`
#'
#' `grouper` :: grouping variable for compatibility with `rsa.runDf()` and `purrr::map_df()`
#'
#' `speaker.p` :: literal listener semantics
#'
#' @name d_pragmods
#' @docType data
#' @keywords data
#' @references \url{https://github.com/langcog/pragmods}
#'
NULL

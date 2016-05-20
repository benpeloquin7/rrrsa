# Really Recursive Rational speech act models (rrrsa) of pragmatic inference

`rrrsa` is an R package for running RSA, a Bayesian model of pragmatic inference. `rrrsa` was created by Ben Peloquin in collaboration with Michael C. Frank.

## What is RSA?

Rational speech act (RSA) models frame language understanding as a special case of social cognition in which `speakers` and `listeners` reason about one another recursively. A `pragmatic listener` $P_{L_n}(m|u)$, reasons about intended meaning $m$ of an utterance $u$ by a `rational speaker` $P_{s_n}(u|m)$ who chooses an utterance according to the expected utility of an utterance $U(m;u)$. $\alpha$ is a decision noise parameter.

$$P_{L_n}(m|u) \propto P_{S_n}(u|m)P(m)$$
$$P_{S_n} \propto e^{U(m;u)}$$
$$U(m;u) = -\alpha(\log(P_{L_{n-1}}(m|u)) - C(u))$$

## rrrsa includes access to all model components

`rrrsa` provides users with access to all model components. The following sections demonstrate how this functionality can be used.

### Calculating the informativity an utterance

`rsa.informativity()` takes three arguments, literal semantics $P_{L_0}$, `alpha` level (default 1), and `cost` (default 0). This function returns the surprisal of an utterance minus cost, multiplied by alpha.

### Calculating the utility of an utterance

`rsa.utility` takes an input vector of literal listener semantics and outputs a normalized vector of speaker likelihoods. If `costs` are not specified the default 0's vector is used. If `alpha` is not specified a default value of $1$ is used.

### Computing one full recursion

In the RSA framework one full recursion consists of a `pragmatic listner` $P_{L_1}$ who reasons about a `rational speaker` $P_{s_1}$ who reason about a `literal listener` $P_{L_0}$. Expected input is an $m$ matrix of  $P_{L_0}$ `literal listener` values in which columns corresond to items (words) and rows correspond to semantic quantity (stars in Peloquin & Frank, under review). Optional arguments include a `costs` vector which whould be the same length as `ncol` and an optional `priors` vector which should the same length as `nrows`. `rsa.fullRecursion` provides safety checking for these cases. Output corresponds with pragmatic listener posterior predictions.

### Running multiple recursions

`rsa.reason` is really a wrapper function for `rsa.fullRecursion` which provides an additional `depth` parameter which specifies the recursive depth during reasoning. If depth is not provided, default value is $1$.

### Running data frames
 
Run RSA on a tidied data frame and avoid running individual model components individually with `rsa.runDf`. An RSA-ready, tidied data frame must contian columns for semantic `quantity`, `item` and `semantics`, where each row corresponds with unique `item`/`quantity` combination. A user should specify their naming convention for these items in the `quantityVarName`, `itemVarName` and `semanticsVarName` arguments. The `costVarName` and `priorsVarName` args correspond with `costs` and/or `priors` data. Users can specify values for `alpha` and `depth` hyperparamenters. `runDf` will return a data frame with a new model predictions `preds` column appended.

### Tuning hyperparamaters

If a user has `pragmatic judgment` data we can use the `tuneDepthAlpha` function to tune hyperparameters. Calling this function requires the same argument list as a simple `runDf` with the addition of `alphas` and `depths` vectors to iterate over as well as the column of `pragmatic judgments` specified through the `compareDataVarName` argument, which we'll be using as the basis of comparison.

## Data from Peloquin & Frank (under review)

`rrrsa` includes empirical literal listener $P_{L_0}$ which can be used as input to `rrrsa` as well as $P_{L_1}$ pragmatic judgments for model tuning and comparison. Four data sets are included:

`peloquinFrank_2Alts`: data set with entailment alternatives

`peloquinFrank_3Alts`: data set with entailment alternatives + universal none

`peloquinFrank_4Alts`: data set with entailment alternatives + top two empirically derived alts

`peloquinFrank_5Alts`: data set with entailment alternatives + top two empirically derived alts + neutral valence alternative
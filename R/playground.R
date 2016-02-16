

# practice data
m <- matrix(data = c(0, 0.2, 0.25, 0.25, 0.3, 0, 0, 0, 0.3, 0.7), nrow = 5)
rownames(m) <- 1:5
colnames(m) <- c("item1", "item2")

# Normalize a vector
# -------------------
norm <- function(v) {
  normalizer = sum(v)
  if (normalizer == 0) rep(0, length(v))
  else (v / normalizer)
}

# Run one full recursion
# ----------------------
recurse <- function(m, f) {
  rNames <- rownames(m)
  cNames <- colnames(m)
  newM <- apply(t(apply(m, 1, norm)), 2, norm)
  rownames(newM) <- rNames
  colnames(newM) <- cNames
  newM
}

# Recurse to depth 'depth' on matrix 'm'
# --------------------------------------
reason <- function(m, depth) {
  while(depth > 0) {
    m <- recurse(m)
    depth <- depth - 1
  }
  m
}

# p_L0(m | u) = exp(-alpha * (-log(data) - cost))
# -----------------------------------------------
informativity <- function(m_u, alpha = 1, cost = 0) {
  ifelse(m_u == 0, 0, exp(-alpha * (-log(m_u) - cost)))
}

# Compute utility
# ---------------
utility <- function(items, alpha = 1, cost = 0) {
  norm(sapply(items, informativity))
}
# utility(reason(m, 3)[5,])

recurse2(m)

apply(m[2, ], FUN = function(i) utility(i, 1, 0))

literalListener <- function(m, costs, alpha) {

}

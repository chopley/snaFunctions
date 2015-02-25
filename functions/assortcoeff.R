# calculate the assortativity coefficient for a mixing matrix of a graph
# ref: MEJ Newman, 'Mixing patterns in networks', Phys Rev E 67, 026126 (2003)
# http://www.babelgraph.org/wp/?p=351
# define assortativity coefficient as
# trace (m) - sum (m^2)
# ac = -------------------------
# 1 - sum (m^2)
#
# where m is the mixing matrix of a graph

assortcoeff <- function(m) {
  tr <- 0
  for (k in 1:nrow(m)) tr <- tr + m[k,k]
  sumsq <- sum (rowSums(m)*colSums(m))
  (tr - sumsq) / (1 - sumsq)
}

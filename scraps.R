library(cna)
library(igraph)
library(Rfast)
mod <- "(A*b+C<->D)*(D*E+G<->F)"
asfs <- extract_asf(mod) 
lhs <- lapply(asfs[[1]], lhs)
rhs <- lapply(asfs[[1]], rhs)
# literals <- lapply(lhs, function(x) strsplit(gsub("\\*|\\+", "", x), ""))
# literals <- unlist(literals, recursive = FALSE)
disjuncts <- lapply(lhs, function(x) strsplit(x, "\\+"))
#disjuncts <- lapply(lhs, function(x) strsplit(gsub("\\*", "", x), "\\+"))
disjuncts <- unlist(disjuncts, recursive = FALSE)
rhsreps <- mapply(function(x, y) rep(x, length(y)), rhs, disjuncts, SIMPLIFY = FALSE)
edgelist <- data.frame(out=rev(unlist(rhsreps)), disj=rev(unlist(disjuncts)))
edgelist <- graph.edgelist(as.matrix(edgelist))
adj_matrix <- as.matrix(get.adjacency(edgelist))
adj_matrix[adj_matrix == 0] <- NA

paths <- floyd(adj_matrix)
colnames(paths) <- colnames(adj_matrix)
rownames(paths) <- rownames(adj_matrix)


dis <- test_disjuncts[1]


m2 <- "E*c*d <-> A"

tar_lhs_facs_per_disj <- lapply(target_lhss_disjuncts, function(x) gsub("\\*", "", x))
lapply(target_lhss_disjuncts, function(x) lapply(target_rhss, function(y) pmatch(x,y)))

t <- unlist(target_lhss_disjuncts)
u <- unlist(target_rhss)

et <- c(t,u)


diss <- target_lhss_disjuncts
outs <- target_rhss
pathfinder(diss, outs, c())

pathfinder <- function(diss, outs, path){
  path <- c(path, diss[[1]])
  #lapply(diss[-1], function(x) pmatch(x, outs[-1]))
  idx <- lapply(outs[1], function(x) which(!is.na(pmatch(x, outs))))
  #idx <- lapply(idx, function(x) x[!is.na(x)])
  adds <- mapply(function(x, y)  x[y], diss[-1], idx, SIMPLIFY = F)
  #adds <- unlist(diss[-1])[pmatch(outs[-1], unlist(diss[-1]), nomatch = 0)]
  adds <- unlist(adds)
  if(length(adds) == 0L){return(path)} else {
    path <- c(path, unique(adds))
    newdis <- diss[-1]
    newouts <- outs[-1]
    pathfinder(newdis, outs, path)
  }
}

chain_asfs <- lapply(chain_asfs, unique)

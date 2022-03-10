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
#disjuncts <- unlist(disjuncts, recursive = FALSE)
disjuncts <- unlist(target_lhss_disjuncts, recursive = FALSE)
rhs <- target_rhss



rhsreps <- vector("list", length(target_rhss))
for (i in seq_along(target_rhss)){
  rhsreps[[i]] <- mapply(function(x, y) rep(x, length(y)), target_rhss[i], target_lhss_facs[[i]], SIMPLIFY = FALSE)  
}



edgelist <- data.frame(disj=unlist(target_lhss_facs), out=unlist(rhsreps))
graph <- graph.edgelist(as.matrix(edgelist))

cand_facs <- unlist(strsplit(cand_disjuncts, "\\*"))
cand_fac_paths <- vector("list", length(cand_facs))
for(fac in seq_along(cand_facs)){
  temppaths <- all_simple_paths(graph, from = cand_facs[fac], to = outcome)
  cand_fac_paths[[fac]] <- lapply(temppaths, function(x) as.character(names(x)))
  if(length(cand_fac_paths[[fac]]) == 0L) {cand_fac_paths[[fac]] <- NA}
  
}
names(cand_fac_paths) <- cand_facs

viable_paths <- lapply(cand_fac_paths, function(x) lapply(x, function(y) which(target_rhss %in% y)))



adj_matrix <- as.matrix(get.adjacency(edgelist))
all_simple_paths(adj_matrix)
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
  path <- c(path, diss[[1]], outs[1])
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

m1 <- "(E*c + b*h*d + E*H*B <-> G)*(b*H + c*E*b <-> F)*(F*e + c*G*d <-> A)"
m2 <- "E*c*d + e*c*G + e*b*H <-> A"



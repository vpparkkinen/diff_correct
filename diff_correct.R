



library(cna)
library(dplyr)
#library(cnaOpt)
library(data.table)


diff_correct <- function(m1, m2){
  fulldat <- ct2df(selectCases(m1))
  
  is_outcome <- function(dd){
    is <- !unlist(lapply(names(dd), function(x)
      any(duplicated(dd[, -which(names(dd) == x)]))))
    out <- data.frame(Factor = names(dd), outcome = is)
    return(out)
  }
  
  # endogs <- findOutcomes(fulldat)
  # endogs <- as.character(endogs[endogs$outcome == TRUE,]$Factor)
  
  outcome <- gsub(" ", "", cna:::rhs(m2))
  
  #potouts <- findOutcomes(fulldat)
  potouts <- is_outcome(fulldat)
  potouts <- potouts[potouts$outcome == TRUE,]
  
  target <- cna:::noblanks(m1)
  target_asfs <- unlist(cna:::extract_asf(target))
  target_lhss <- cna:::lhs(target_asfs)
  target_rhss <- cna:::rhs(target_asfs)
  target_rhss <- gsub(" ", "", target_rhss)
  target_lhss_disjuncts <- unlist(lapply(target_lhss, strsplit, "\\+"), recursive = FALSE)
  target_lhss_ex <- lapply(target_lhss, cna:::tryparse)
  target_lhss_facs <- lapply(target_lhss_ex, all.vars)
  outcome_asf_idx <- which(target_rhss == outcome)
  
  
  #cand_lhs <- all.vars(cna:::tryparse(cna:::lhs(candidate)))
  candidate_model <- cna:::noblanks(m2)
  cand_lhs <- cna:::lhs(candidate_model)
  cand_disjuncts <- unlist(strsplit(cand_lhs, "\\+"))
  
  disjunct_check <- vector("logical", length(cand_disjuncts))
  names(disjunct_check) <- cand_disjuncts
  
  if(!outcome %in% potouts$Factor){return(list("correct" = FALSE, "difference-makers" = disjunct_check))}
  
  
  cand_facs <- lapply(cand_disjuncts, function(x) unlist(strsplit(x, "\\*")))
  #cnames <- lapply(cand_facs, toupper)
  
  # chain_asfs <- lapply(target_rhss, function(x) lapply(target_lhss_facs, function(z) x %in% z)) 
  # names(chain_asfs) <- target_rhss
  # chain_asfs <- lapply(chain_asfs, unlist)
  # 
  # chain_asfs_mt <- as.matrix(as.data.frame(chain_asfs))
  # tt <- t(chain_asfs_mt)
  # connected <- data.frame(from = row(tt)[which(tt)], to = col(tt)[which(tt)])
  # ctti_temp <- intersect(connected[,2], connected[,1]) 
  # 
  # if (identical(integer(0), ctti_temp)){
  #   chains <- NULL
  # } else {
  #   #ctti <- which(connected %in% ctti_temp, arr.ind = T)
  #   # chains_temp <- connected[ctti[,1],]
  #   # chains_temp <- chains_temp[order(as.numeric(row.names(chains_temp))),]
  #    chains <- unique(unlist(connected))
  # }
  # 
  # candf_appear <- lapply(cand_facs, function(z) 
  #   which(unlist(lapply(target_lhss_facs, function(x) z %in% x))))
  # 
  # lapply(canf)
  
  
  
  
  
  # cfac_vals <- lapply(cand_facs, function(z) unlist(lapply(z, function(x) ifelse(x == toupper(x), 1, 0))))
  # for (i in seq_along(cfac_vals)){
  #   names(cfac_vals[[i]]) <- cnames[[i]]
  # }
  # 
  # for (v in seq_along(cfac_vals)){
  #   va <- names(fulldat)[which(names(fulldat) %in% cnames[[v]])]
  #   cfac_vals[[v]] <- cfac_vals[[v]][match(va, toupper(names(cfac_vals[[v]])))]
  # }
  # 
  # cval_paste <- lapply(cfac_vals, paste0, collapse = "")
  # 
  # disstat <- vector("list", length(cfac_vals))
  # for (i in seq_along(cfac_vals)){
  #   tdat <- fulldat[,names(cfac_vals[[i]])] 
  #   pasta <- if(is.null(dim(tdat))){as.character(tdat)} else {do.call(paste0, tdat)}
  #   disstat[[i]] <- as.integer(pasta == cval_paste[[i]])
  # }
  # 
  # ddat <- as.data.frame(disstat)
  # colnames(ddat) <- cand_disjuncts
  # bydis <- lapply(ddat, function(x) ddat[x==1,])
  # 
  # bsums <- lapply(bydis, function(x) apply(t(x), 2, sum))
  # nonred_dis <- all(unlist(lapply(bsums, function(x) any(x == 1))))
  # #nonred_dis <- any(apply(ddat, 2, sum)==1)
  # if(!nonred_dis){return(list("correct" = FALSE, "difference-makers" = "redundant"))}
  
  test_disjuncts <- cand_disjuncts

  
  for (dis in test_disjuncts){
      disjunct_check[names(disjunct_check) == dis] <- diff_checker(fulldat, 
                                                                   dis, 
                                                                   outcome, 
                                                                   potouts, 
                                                                   test_disjuncts[-which(test_disjuncts == dis)])
  }
  out <- list("correct" = all(disjunct_check), "difference-makers" = disjunct_check)
  return(out)
}
    




# cofac_extract <- function(tf, ad){
#   facs <- ad[grepl(tf, ad)]
#   facs <- unlist(strsplit(facs, "\\*"))
#   out <- facs[-which(facs == tf)]
#   if(length(out)<1){out <- NULL}
#   return(out)
# }
# 
# case_flipper <- function(x){
#   out <- ifelse(x == toupper(x), tolower(x), toupper(x))
#   return(out)
# }




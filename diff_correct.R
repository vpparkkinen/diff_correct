



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
    

diff_checker <- function(fulldat, dis, outcome, potouts, resdis){
  disfacs <- unlist(strsplit(gsub("\\*", "", dis), ""))
  pre_dum_confs <- ct2df(selectCases(dis, fulldat))
  
  latent_confs <- unique(do.call('paste0', pre_dum_confs))
  allconfs_dummify <- do.call('paste0', fulldat)
  tempdat <- fulldat
  tempdat$dum <- as.integer(allconfs_dummify %in% latent_confs)
  #testdat <- unique(tempdat[,which(!names(tempdat) %in% disfacs)])
  testdat <- unique(tempdat)
  
  
  spl <- split(testdat, list(testdat$dum, testdat[,names(testdat)==toupper(outcome)]))
  
  both <- spl[names(spl) == '1.1'][[1]]
  neither <- spl[names(spl) == '0.0'][[1]]
  cause <- spl[names(spl) == '1.0'][[1]]
  effect <- spl[names(spl) == '0.1'][[1]]
  
  if (length(resdis) >= 1){
    res_neg <- paste0(resdis, collapse = "+")
    res_neg <- paste0("!(", res_neg, ")")
    both <- tt2df(selectCases(res_neg, both))
    names(both) <- names(testdat)  
  }
  
  #testdat <- as.data.table(testdat)
  # endogs <- findOutcomes(fulldat)
  # endogs <- as.character(endogs[endogs$outcome == TRUE,]$Factor)
  #intersect()
  
  if(nrow(both) < 1 | nrow(neither) < length(disfacs)){return(FALSE)} else {
    
    #neither <- as.data.table(neither)
    #cn <- rownames(neither)
    #cn <- 1:nrow(neither)
    #cs <- combn(cn, length(disfacs) + 1)
    #setkeyv(neither, c(toupper(disfacs), toupper(outcome)))
    #setkeyv(neither, c(toupper(disfacs)))
    #nsets <- apply(cs, 2, function(x) neither[x,])
    
    outv_flip <- ifelse(outcome == toupper(outcome), 0, 1)
    names(outv_flip) <- outcome
    facvals <- unlist(lapply(disfacs, function(x) ifelse(x == toupper(x), 1, 0)))
    names(facvals) <- toupper(disfacs)
    facvals_neg <- vector("list", length(facvals))
    facvals_neg <- lapply(names(facvals), 
                          function(x) 
                            c(ifelse(facvals[names(facvals) == x] == 1, 0, 1), 
                              facvals[-which(names(facvals) == x)]))
    #allneg <- c(ifelse(facvals == 1, 0, 1), outv_flip)
    #fnlist <- lapply(facvals_neg, as.list)
    
    tu <- names(neither)[which(names(neither) %in% c(toupper(disfacs)))]
    facvals_neg <- lapply(facvals_neg, function(z)
      z[match(tu, toupper(names(z)))])
    
    #ntemp <- lapply(facvals_neg, function(x) neither[as.list(x), nomatch = NULL]) 
    
    ntemp <- lapply(facvals_neg, function(x)
      if (length(x) == 1){neither[neither[which(names(neither) == names(x))] == x,]}else{
        neither[which(do.call(paste0, neither[,which(names(neither) %in% names(x))]) %in% paste(x, collapse = "")),]  
      })
      
    
    rcc <- any(unlist(lapply(ntemp, function(x) nrow(x) < 1)))
    if (rcc){return(FALSE)}
    rs <- lapply(ntemp, function(x) 1:nrow(x))
    rns <- expand.grid(rs)
    
    rns_rows <- vector("list", length(rns))
    for (i in seq_along(rns)){
      rns_rows[[i]] <- lapply(rns[,i], function(x) ntemp[[i]][x,])
    }
    
    ar <- array(unlist(rns_rows, recursive = F), dim = dim(rns))
    tar <- t(ar)
    
    minsets <- vector("list", nrow(rns))
    for (i in seq_along(1:nrow(rns))){
      minsets[[i]] <- do.call(rbind, tar[,i])
    }
    #fnt <- lapply(fnlist, as.data.table)
    
    # tpairs <- unlist(lapply(1:nrow(both), function(x) 
    #   lapply(minsets, function(z) rbind(both[x,], z))), recursive = F)
    # 
    tptemp <- unlist(lapply(1:nrow(both), function(x) 
      lapply(minsets, function(z) list(both[x,], z))), recursive = F)
    
    tpairs <- lapply(tptemp, rbindlist)
    
    c_idx <- lapply(tpairs, function(x) which(unlist(lapply(x, function(z) length(unique(z))==1))))
    # c_vals <- mapply('[', tpairs, c_idx, SIMPLIFY = F)
    # 
    
  
    #  c_vals <- lapply(c_vals, function(x) c(x[1,]))
    # c_idx_neg <- lapply(c_idx, function(x) 0L - x)
    
    # pair_variants <- vector("list", length(tpairs))
    # for (i in seq_along(tpairs)){
    #   if (length(c_vals[[i]]) < 1) {pair_variants[[i]] <- testdat} else {
    #     setkeyv(testdat, names(c_idx[[i]]))
    #     pair_variants[[i]] <- testdat[c_vals[[i]]]  
    #   }
    # }
    # 
    #cvals <- lapply(c_vals, unique)
    tpsvary <- lapply(tpairs, function(x) 
      which(unlist(lapply(x, function(z) length(unique(z))>1))))
    
    
    ##tpsvary <- lapply(tpairs, function(x) c(which(unlist(lapply(x, function(y) length(unique(y)) > 1)))))
    #names(unlist(tp1[1,-1]))
    
    tpidx <- lapply(tpsvary, function(x) 0L - x)
   # tpidx <- lapply(tpidx, function(x) {names(x)<-NULL; return(x)})
    #tpidx <- lapply(tpsvary, function(x) 0L - x)
    #lapply(tpidx, function(ix) fulldat[which(apply(fulldat[,ix], 1, function(x) all(x == tp1[1,ix]))), ]  )
    #testdat[testdat[outcome] == 1 & testdat$dum == 0,]
    tpairs <- lapply(tpairs, as.data.frame)     
    pair_variants <- vector("list", length(tpairs))
    for (i in seq_along(tpairs)){
      tpl <- length(tpidx[[i]])
      tl <- length(testdat)
      if(tpl == tl){
        pair_variants[[i]] <- testdat
      } else {
        dc <- if(tpl == tl-1){tpairs[[i]][1,tpidx[[i]]]}else{do.call(paste0, tpairs[[i]][1, tpidx[[i]]])}
        tdc <- if(tpl == tl-1){testdat[,tpidx[[i]]]} else { do.call(paste0, testdat[, tpidx[[i]]]) }
        pair_variants[[i]] <- testdat[which(tdc %in% dc),]
      }
    }
    
    
    #fulldat[which(apply(fulldat[,tpidx[[1]]], 1, function(x) all(x == tp1[1,tpidx[[1]]]))), ]
    #bc_idx <- unlist(lapply(pair_variants, function(x) any(x[, .N, keyby = dum][,N] == 1)))
    
    
    bc_idx <- unlist(lapply(pair_variants, function(x) any(table(x[,"dum"]) == 1)))
    
    bcases <- pair_variants[bc_idx]
         
    
    #bc_idx <- unlist(lapply(dumconst, function(x) any(unlist(lapply(x, function(y) nrow(y)==1)))))
    #bcases <- pair_variants[bc_idx]
    potconf <- lapply(c_idx[bc_idx], function(x)
      names(testdat)[which(!names(testdat) %in% c(names(x), toupper(outcome), toupper(disfacs), "dum"))])
    
    pair_variants <- pair_variants[!bc_idx]
    #dumconst <- dumconst[!bc_idx]
    
    dumconst <- lapply(pair_variants, function(x) split(x, x$dum))
    
    # ttt[,unique(.SD), by=dum, .SDcols = outcome][,.N, by=dum]
    # lapply(pair_variants, function(ttt) 
    #   any(ttt[,unique(.SD), by=dum, .SDcols = outcome][
    #     ,.N, by=dum][,N] > 1))
    
    
    #out <- any(outvar)
    
    #lapply(pair_variants, function(x) )
    #outvar_pos <- lapply(dumconst, function(x) lapply(x[2], function(y) var(y[,outcome]) ))
    outvar <- lapply(dumconst, function(x) lapply(x, function(y) var(y[,outcome]) ))
    outvar <- lapply(outvar, function(x) sum(x[[1]], x[[2]]) == 0L)
    out <- any(unlist(outvar))
    #legpairs <- pair_variants[unlist(outvar)]
    
    #outvar_neg <- lapply(dumconst, function(x) lapply(x[1], function(y) var(y[,outcome]) ))
    #outvar <- lapply(outvar, unlist)
    #outvar <- lapply(outvar, function(y) ifelse(is.na(y), 0L, y))
    # tpcands <- tpairs[which(unlist(lapply(outvar, function(x) sum(x) == 0L)))]
    # 
    # outvar_pos <- lapply(outvar_pos, unlist)
    # outvar_pos <- lapply(outvar_pos, function(y) ifelse(is.na(y), 0L, y))
    # #tpcands <- tpairs[which(unlist(lapply(outvar_pos, function(x) sum(x) == 0L)))]
    #outvar <- unlist(lapply(outvar, function(x) sum(unlist(x))))
    
    #out <- any(outvar == 0L)
    
    if (!out & length(potconf) >= 1){
      
      potconf <- unique(unlist(potconf))
      #lapply(potconf, function(y) !y %in% potouts$Factor)
      potconf <- potconf[!potconf %in% potouts$Factor]
      if (length(potconf) < 1){out <- TRUE}else{
        testdat <- as.data.table(testdat)
        setkeyv(testdat, toupper(disfacs))
        flt <- testdat[as.list(facvals)]
        ctest <- lapply(potconf, function(z) cov(flt[,..z], flt[,..outcome]) %in% c(0L, NA))
        #ctest <- lapply(ctest, unlist)
        #ctest <- any(unlist(lapply(ctest, all)))
        out <- any(unlist(ctest))
        #out <- ctest  
      }
      
    }
    
    
    
  }
  return(out)
}








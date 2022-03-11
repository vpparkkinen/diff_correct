library(cna)
library(igraph)

diff_correct <- function(m1, m2){
  if(!is.inus(m1)) stop("target model is not causally interpretable (not INUS)")
  if(!is.inus(m2)) stop("candidate model is not causally interpretable (not INUS)")
  fulldat <- ct2df(selectCases(m1))
  outcome <- gsub(" ", "", cna:::rhs(m2))
  target <- cna:::noblanks(m1)
  target_asfs <- unlist(cna:::extract_asf(target))
  target_lhss <- cna:::lhs(target_asfs)
  target_rhss <- cna:::rhs(target_asfs)
  target_rhss <- gsub(" ", "", target_rhss)
  target_lhss_disjuncts <- unlist(lapply(target_lhss, strsplit, "\\+"), recursive = FALSE)
  target_lhss_ex <- lapply(target_lhss, cna:::tryparse)
  target_lhss_facs <- lapply(target_lhss_ex, all.vars)
  #tar_lhs_facs_per_disj <- lapply()
  outcome_asf_idx <- which(target_rhss == outcome)
  
  #cand_lhs <- all.vars(cna:::tryparse(cna:::lhs(candidate)))
  candidate_model <- cna:::noblanks(m2)
  cand_lhs <- cna:::lhs(candidate_model)
  cand_disjuncts <- unlist(strsplit(cand_lhs, "\\+"))
  
 
  
  # cand_is_directcause <- vector("logical", length(cand_disjuncts))
  # names(cand_is_directcause) <- cand_disjuncts
  # for (i in seq_along(cand_disjuncts)){
  #   #cand_is_directcause[[i]] <- cand_disjuncts[i] %in% target_lhss_disjuncts[[outcome_asf_idx]]
  #   isd <- lapply(target_lhss_disjuncts[[outcome_asf_idx]], function(x) grepl(cand_disjuncts[i], x))
  #   #isd <- grepl(cand_disjuncts[i], target_lhss_disjuncts[[outcome_asf_idx]])
  #   #isd <- lapply(target_lhss_disjuncts[[outcome_asf_idx]], function(x) match(x, cand_disjuncts[i]))
  #   cand_is_directcause[i] <- any(unlist(isd))
  #   #cand_is_directcause[[i]] <- which(unlist(isd))
  # }
  # if (all(cand_is_directcause)){return(TRUE)}
  # 
  # 
  # if (any(cand_is_directcause)){
  #   test_disjuncts <- cand_disjuncts[!cand_is_directcause]
  # } else {
  #   test_disjuncts <- cand_disjuncts
  # }

  # disjunct_check <- vector("logical", length(test_disjuncts))
  # names(disjunct_check) <- test_disjuncts

  test_disjuncts <- cand_disjuncts
  disjunct_check <- vector("logical", length(cand_disjuncts))
  names(disjunct_check) <- cand_disjuncts
  #disjunct_check <- cand_is_directcause
  
  
  #test_factors <- all.vars(cna:::tryparse(cand_lhs))[]
  
  # test_factors <- unlist(lapply(lapply(test_disjuncts, cna:::tryparse), all.vars))
  # test_facs_check <- vector("logical", length(test_factors))
  # names(test_facs_check) <- test_factors
  
  # #<--------------THIS LOGIC MAKES NO SENSE
  # if(length(target_rhss) == 1){
  #   if(!outcome == target_rhss[outcome_asf_idx]){
  #     return(FALSE)
  #   } else{
  #     on_path_asfs_idx <- outcome_asf_idx
  #     on_path_outcomes <- target_rhss[on_path_asfs_idx]
  #     on_path_asfs_facs <- target_lhss_facs[on_path_asfs_idx]
  #   }
  # } else {
  #   chain_asfs <- lapply(target_lhss_facs, function(x) match(x, target_rhss, nomatch = 0L))
  #   #chain_asfs <- lapply(target_lhss_disjuncts, function(x) pmatch(x, target_rhss, nomatch = 0L))
  #   # chain_asfs <- lapply(target_lhss_disjuncts, 
  #   #                      function(x) sapply(target_rhss, 
  #   #                                         function(e) pmatch(x, e, nomatch = 0L)))
  #   # #which(previous_out_in_disjunct)
  # 
  #   #on_path_asfs_idx <- previous_out_in_disjunct[[which(lapply(previous_out_in_disjunct, sum) > 0)]]
  #   midpath_asfs_idx <- which(lapply(chain_asfs, sum) > 0)
  #   other_path_asfs_idx <- lapply(chain_asfs, function(y) y[y>0])
  #   
  #   n_paths <- length(other_path_asfs_idx[[outcome_asf_idx]])
  #   all_paths <- vector("list", length(chain_asfs))
  #   for(n in 1:n_paths){
  #     for(c in rev(seq_along(chain_asfs))){
  #       all_paths[[c]] <- pathfind(chain_asfs[1:c], c())
  #     }
  #   }
  #   
  #   pathfind <- function(x, path){
  #     if((length(x) == 1) | sum(x[[length(x)]]) == 0L){
  #       return(unique(path))
  #     } else {
  #       path <- c(path, length(x), x[[length((x))]])
  #       pathfind(x[-length(x)], path)
  #     }
  #   }
  #   
  #   
  #   on_path_asfs_idx <- sort(unique(c(midpath_asfs_idx, unlist(other_path_asfs_idx))))
  #   on_path_outcomes <- target_rhss[on_path_asfs_idx]
  #   if(!outcome %in% on_path_outcomes){return(FALSE)}
  #   
  #   on_path_asfs_facs <- target_lhss_facs[on_path_asfs_idx]
  # #---------THIS LOGIC MAKES NO SENSE--------->
  #}
  
  rhsreps <- vector("list", length(target_rhss))
  for (i in seq_along(target_rhss)){
    rhsreps[[i]] <- mapply(function(x, y) rep(x, length(y)), 
                           target_rhss[i], 
                           target_lhss_facs[[i]], 
                           SIMPLIFY = FALSE)  
  }
  
  
  
  edgelist <- data.frame(disj = unname(toupper(unlist(target_lhss_facs))),
                         out = unname(toupper(unlist(rhsreps))))
  graph <- graph.edgelist(as.matrix(edgelist))
  
  cand_facs <- unlist(strsplit(cand_disjuncts, "\\*"))
  cand_facs_uc <- toupper(cand_facs)
  cand_facs_uc <- unique(cand_facs_uc)
  #cand_fac_paths <- vector("list", length(cand_facs))
  cand_fac_paths <- vector("list", length(cand_facs_uc))
  
  for(fac in seq_along(cand_facs_uc)){
    temppaths <- all_simple_paths(graph, from = cand_facs_uc[fac], to = outcome)
    cand_fac_paths[[fac]] <- lapply(temppaths, function(x) as.character(names(x)))
    if(length(cand_fac_paths[[fac]]) == 0L) {cand_fac_paths[[fac]] <- NA}
    
  }
  names(cand_fac_paths) <- cand_facs_uc
  
  # first factor in cand_fac_paths is the lhs factor, must be excluded when determining
  # asf connections
  viable_paths <- lapply(cand_fac_paths, function(x) lapply(x, function(y) which(toupper(target_rhss) %in% y[-1])))
  
  
  
  #test_factors <- unlist(lapply(lapply(test_disjuncts, cna:::tryparse), all.vars))
  
  #test_facs_check <- vector("logical", length(test_factors))
  #names(test_facs_check) <- test_factors
  
  
  #test_disjuncts <- c(test_disjuncts, "A")
  
  
  for (dis in test_disjuncts){
    
    dis_facs <- unlist(strsplit(dis, "\\*"))
#_____________________________________________________________    
    pre_dum_confs <- ct2df(selectCases(dis, fulldat))
    
    latent_confs <- unique(do.call('paste0', pre_dum_confs))
    allconfs_dummify <- do.call('paste0', fulldat)
    tempdat <- fulldat
    tempdat$dum <- as.integer(allconfs_dummify %in% latent_confs)
    #testdat <- unique(tempdat[,which(!names(tempdat) %in% dis_facs)])
    testdat <- unique(tempdat)
    
    if(sum(testdat$dum) < 1){return(FALSE)}
    
    spl <- split(testdat, list(testdat$dum, testdat[,names(testdat)==toupper(outcome)]))
    
    both <- spl[names(spl) == '1.1'][[1]]
    neither <- spl[names(spl) == '0.0'][[1]]
    cause <- spl[names(spl) == '1.0'][[1]]
    effect <- spl[names(spl) == '0.1'][[1]]
    
    if(nrow(both) < 1){return(FALSE)}
      
    resdis <- test_disjuncts[-which(test_disjuncts == dis)]
    
    
    if (length(resdis) >= 1){
      res_neg <- paste0(resdis, collapse = "+")
      res_neg <- paste0("!(", res_neg, ")")
      both <- ct2df(selectCases(res_neg, both))
      names(both) <- names(testdat)  
    }
    
    if(nrow(both) < 1 | nrow(neither) < length(dis_facs)){return(FALSE)}
    
    outv_flip <- ifelse(outcome == toupper(outcome), 0, 1)
    names(outv_flip) <- outcome
    facvals <- unlist(lapply(dis_facs, function(x) ifelse(x == toupper(x), 1, 0)))
    names(facvals) <- toupper(dis_facs)
    facvals_neg <- vector("list", length(facvals))
    facvals_neg <- lapply(names(facvals), 
                          function(x) 
                            c(ifelse(facvals[names(facvals) == x] == 1, 0, 1), 
                              facvals[-which(names(facvals) == x)]))
    #allneg <- c(ifelse(facvals == 1, 0, 1), outv_flip)
    #fnlist <- lapply(facvals_neg, as.list)
    
    tu <- names(neither)[which(names(neither) %in% c(toupper(dis_facs)))]
    facvals_neg <- lapply(facvals_neg, function(z)
      z[match(tu, toupper(names(z)))])
    
    #ntemp <- lapply(facvals_neg, function(x) neither[as.list(x), nomatch = NULL]) 
    
    ntemp <- lapply(facvals_neg, function(x)
      if (length(x) == 1){neither[neither[which(names(neither) == names(x))] == x,]}else{
        neither[which(do.call(paste0, neither[,which(names(neither) %in% names(x))]) %in% paste(x, collapse = "")),]  
      })
      
    
    rcc <- any(unlist(lapply(ntemp, function(x) nrow(x) < 1)))
    if(rcc){return(FALSE)}
#-------------------------------------------------------
    
    
    
    dis_facs_check <- vector("logical", length(dis_facs))
    names(dis_facs_check) <- dis_facs
    for (id in dis_facs){
      if (!toupper(id) %in% toupper(unlist(cand_fac_paths))) {
        #dis_facs_check[names(dis_facs_check) == id] <- FALSE
        pa_check <- FALSE
      } else {
        
        #closest_path_asf_idx <- max(which(unlist(lapply(target_lhss_facs, function(x) id %in% x))))
        #paths_idx <- which(unlist(lapply(target_lhss_facs[1:outcome_asf_idx], function(x) id %in% x)))
        id_causes <- edgelist$disj[-which(edgelist$disj == toupper(id))]
        id_causes <- lapply(id_causes, function(x) all_simple_paths(graph, from = x, to = toupper(id)))
        id_causes <- names(unlist(id_causes))
        id_causes <- unique(id_causes[-which(id_causes == toupper(id))])
        
        id_effects <- all_simple_paths(graph, from = toupper(id))
        id_effects <- names(unlist(id_effects))
        id_effects <- id_effects[-which(id_effects == toupper(id))]
        
        #cofacs <- toupper(cofac_extract(id, dis))
        cofacs <- cofac_extract(id, dis)
        
        
        
        
        if(!is.null(cofacs)){
          cofac_potdep <- lapply(cofacs, function(x)
            unique(edgelist$disj[-which(edgelist$disj == toupper(x))]))
          cofac_causes <- vector("list", length(cofacs))
          for(co in seq_along(cofacs)){
            cofac_causes[[co]] <- lapply(cofac_potdep[[co]],
                                         function(x)
                                           all_simple_paths(graph,
                                                            from = x,
                                                            to = toupper(cofacs[co])))
          }
          cofac_causes <- names(unlist(cofac_causes))
          cofac_effects <- lapply(cofacs, function(x)
            all_simple_paths(graph, from = toupper(x)))
          cofac_effects <- names(unlist(cofac_effects))

        } else {
          cofac_causes <- NULL
          cofac_effects <- NULL
        }
        
        if(!is.null(id_causes)){
          # idc_potdep <- lapply(id_causes, function(x) 
          #   unique(edgelist$disj[-which(edgelist$disj == toupper(x))]))
          # # cofac_causes <- vector("list", length(cofacs))
          # for(co in seq_along(cofacs)){
          #   cofac_causes[[co]] <- lapply(cofac_potdep[[co]],
          #                                function(x)
          #                                  all_simple_paths(graph, 
          #                                                   from = x, 
          #                                                   to = toupper(cofacs[co]))) 
          # }
          #cofac_causes <- names(unlist(cofac_causes))
          idc_effects <- lapply(id_causes, function(x)
            all_simple_paths(graph, from = toupper(x)))
          idc_effects <- names(unlist(idc_effects))
          
        } else {
          idc_effects <- NULL
        }
        
        
        
        paths_idx <- viable_paths[names(viable_paths) == toupper(id)]
        paths_idx <- unlist(paths_idx, recursive = FALSE)
        pa_check <- vector("logical", length(paths_idx))
        for (pa in paths_idx){
          
          
          canvary <- unique(c(toupper(id),
                              toupper(target_rhss[pa]),
                              id_causes,
                              id_effects,
                              cofac_causes,
                              cofac_effects))
                              #toupper(cofacs)))
          canvary <- unique(canvary)
          cofacs_pres <- paste0(cofacs, collapse = "*")
          
          testdat_id <- ct2df(selectCases(cofacs_pres, testdat))
          
          # canvary <- unique(c(toupper(id),
          #                     toupper(target_rhss[pa]),
          #                     id_causes,
          #                     id_effects,
          #                     idc_effects))
          #extract co-factors for candidate factor and its effects on path to outcome
          # candidate_cofacs <- cofac_extract(id, dis)
          # if(is.null(candidate_cofacs)){
          #   candidate_cofacs_pres <- ""
          # } else {
          #   candidate_cofacs_pres <- paste0(candidate_cofacs, collapse = "*")
          # }

         
          if(all(names(fulldat) %in% canvary)){
            check_for_pairs <- list(testdat_id)
          } else {
            # check_for_pairs <- split(testdat, 
            #                        testdat[setdiff(names(testdat), canvary)], 
            #                        drop = T)
            
            check_for_pairs <- split(testdat_id, 
                                   testdat_id[setdiff(names(fulldat), canvary)], 
                                   drop = T)
          }
          
          
          
          
          check_for_pairs <- lapply(check_for_pairs,
                                    function(x) if(nrow(x) < 2){
                                      x <- NULL
                                    } else {
                                      x <- x
                                    }
                                    )
          ##\FIX THIS
          check_for_pairs[unlist(lapply(check_for_pairs, is.null))] <- NULL
          ##FIX THIS
          #var(check_for_pairs[[2]]$G) > 0L
          outvarcheck <- lapply(check_for_pairs, function(x) unique(x[outcome]))
          
          check_for_pairs <- check_for_pairs[unlist(lapply(outvarcheck, function(x) nrow(x) >1))]
          check_for_pairs <- check_for_pairs[unlist(lapply(check_for_pairs,
                                                           function(x)
                                                             length(unique(x$dum)) > 1))]
          
          idx <- which(unlist(lapply(paths_idx, function(x) identical(x, pa)))) #move this somewhere
          
          if(length(check_for_pairs) == 0L){
            pa_check[idx] <- FALSE
          } else {
            dpair_exist <- lapply(check_for_pairs, function(a)
              a[a["dum"] == 0 & a[toupper(outcome)] == 0 | a["dum"] == 1 & a[toupper(outcome)] == 1,])
            dpair_exist <- lapply(dpair_exist, function(x) nrow(x) > 1)
            pa_check[idx] <- any(unlist(dpair_exist))
            
            # checkcov <- lapply(check_for_pairs, function(x) cov(x[toupper(id)], x[outcome]))
            # checked <- lapply(checkcov, function(x) !(x %in% c(0L, NA)))
            # pa_check[idx] <- ifelse(any(unlist(checked)), TRUE, FALSE)
            } 
          
          # #  if (length(cofacs) >= 1){
          # #   ctrl_exp_cofacs_present <- ifelse(ce_temp_cofacs == "!()", ca_cofac_pres,  paste0(ce_temp_cofacs, "*" ,ca_cofac_pres))
          # #   #ctrl_exp_cofacs_supp <- ifelse(ce_temp_cofacs == "!()", sup_candidate_cofac_temp, paste0(ce_temp_cofacs, "*", sup_candidate_cofac_temp))
          # #   ctrl_exp_cofacs_supp <- if(ce_temp_cofacs == "!()"){sup_candidate_cofac_temp}else{paste0(ce_temp_cofacs, "*", sup_candidate_cofac_temp)}
          # # } else {
          # #   #ctrl_exp_cofacs_present <- ctrl_exp_cofacs_supp <- ifelse(ce_temp_cofacs == "!()", "", ce_temp_cofacs)
          # #    ctrl_exp_cofacs_present <- ifelse(ce_temp_cofacs == "!()", "", ce_temp_cofacs)
          # #    ctrl_exp_cofacs_supp <- NULL
          # # }
          # # 
          # # ctrl_exp_cofacs_present <- gsub("\\*$", "", ctrl_exp_cofacs_present)
          # # ctrl_exp_cofacs_supp <- if(is.null(sup_candidate_cofac_temp)){NULL} else {
          # #   gsub("\\*$", "", ctrl_exp_cofacs_supp)}
          # 
          # # if (length(c(candidate_cofacs, on_path_cofacs) >= 1)){
          # #   ctrl_exp_cofacs_present <- paste0(ce_temp_cofacs, ca_cofac_pres)
          # #   ctrl_exp_cofacs_supp <- paste0(ce_temp_cofacs, ca_cofac_supp)
          # # } else {
          # #   ctrl_exp_cofacs_present <- ctrl_exp_cofacs_supp <- ce_temp_cofacs
          # # }
          # 
          # 
          # 
          # 
          # #############################################33
          # 
          # #ctrl_exp_temp <- unlist(lapply(on_path_outcomes, function(x) ctrl_exp_temp[-grepl(x, ctrl_exp_temp)]))
          # 
          # ctrl_cond <- c(ctrl_exp_cofacs_present, ctrl_exp_cofacs_supp)
          # ctrl_cond <- unique(ctrl_cond)
          # ctrl_cond <- ctrl_cond[sapply(ctrl_cond, function(x) length(x) > 0)]
          # cond_check <- vector("logical", length(ctrl_cond))
          # names(cond_check) <- ctrl_cond
          # for(cond in ctrl_cond){
          # 
          # #dummy code background factor configurations
          #   full_path <- c(id, target_rhss[pa]) #needs to come earlier, take out on_path outcomes from ctrl_cond
          #   #dum_dat <- fulldat[,-which(names(fulldat) %in% full_path)]
          #   dum_dat <- fulldat
          #   pre_dum_confs <- if(cond == ""){fulldat}else{ct2df(selectCases(cond, dum_dat))}
          #   
          #   if (nrow(pre_dum_confs) == 0L){
          #     cond_check[names(cond_check) == cond] <- FALSE
          #   } else {
          #   
          #     latent_confs <- unique(do.call('paste0', pre_dum_confs))
          #     allconfs_dummify <- do.call('paste0', dum_dat)
          #     tempdat <- fulldat
          #     tempdat$dum <- as.integer(allconfs_dummify %in% latent_confs)
          #     
          #     #testdat <- tempdat[,which(names(tempdat) %in% c(toupper(full_path), "dum"))]
          #     testdat <- tempdat[,which(names(tempdat) %in% c(toupper(id), outcome, "dum"))]
          #     
          #     testdat <- unique(testdat[testdat$dum == 1,])
          #     
          #     ####################
          #     if (!id == toupper(id)){
          #       testdat[, toupper(id)] <- as.integer(!testdat[, toupper(id)])
          #     }
          #     
          #     idvals <- unique(testdat[,toupper(id)])
          #     if (length(idvals) == 1){cond_check[names(cond_check) == cond] <- FALSE} else {
          #       mdat <- testdat[testdat[,toupper(id)] %in% idvals]
          #       cond_check[names(cond_check) == cond] <- ifelse(sum(mdat[,toupper(id)] == mdat[,outcome]) < 2, FALSE, TRUE)
          #              
          #              
          #     }
          #     
          #   
          #     ########################  
          #     
          #     
          #     # testmod <- paste0(id, "<->", outcome)
          #     # 
          #     # 
          #     # datcheck <- selectCases(testmod, testdat)
          #     # if (!all(dim(datcheck) == dim(testdat)) | nrow(testdat) == 1){cond_check[names(cond_check) == cond] <- FALSE} else {
          #     #   if (all(datcheck == testdat)) {cond_check[names(cond_check) == cond] <- TRUE}
          #     # }
          #     
          #     #if (all(selectCases(testmod, testdat) == testdat)) {cond_check[names(cond_check) == cond] <- TRUE}
          #     #if (identical(selectCases(testmod, testdat), testdat[1,])) {cond_check[names(cond_check) == cond] <- TRUE}
          #     #fix the rest
          #   }
          #   
          # }
          # # if (length(cond_check) == 1){
          # #   dis_facs_check[names(dis_facs_check) == id] <- ifelse(cond_check, TRUE, FALSE)} else 
          # #   {dis_facs_check[names(dis_facs_check) == id] <- ifelse(cond_check[names(cond_check) == ctrl_exp_cofacs_present] & !cond_check[names(cond_check) == ctrl_exp_cofacs_supp], TRUE, FALSE)}
          #  
          # idx <- unlist(lapply(paths_idx, function(x) identical(x, pa)))
          # if (length(cond_check) == 1){
          #   pa_check[idx] <- ifelse(cond_check, TRUE, FALSE)} else {
          #     pa_check[idx] <- ifelse(cond_check[names(cond_check) == ctrl_exp_cofacs_present], TRUE, FALSE)
          #   }
          # 
          # 
           }
        }
        dis_facs_check[names(dis_facs_check) == id] <- ifelse(any(pa_check), TRUE, FALSE)
      }
      #disjunct_check[names(disjunct_check) == dis] <- ifelse(all(dis_facs_check), TRUE, FALSE)
      disjunct_check[names(disjunct_check) == dis] <- ifelse(all(dis_facs_check), TRUE, FALSE)
    }
  out <- list("correct" = all(disjunct_check), "difference-makers" = disjunct_check)
  return(out)
}



cofac_extract <- function(tf, ad){
  facs <- ad[grepl(tf, ad)]
  facs <- unlist(strsplit(facs, "\\*"))
  out <- facs[-which(facs == tf)]
  if(length(out)<1){out <- NULL}
  return(out)
}

case_flipper <- function(x){
  out <- ifelse(x == toupper(x), tolower(x), toupper(x))
  return(out)
}






library(cna)

diff_correct <- function(m1, m2){
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
  
  #<--------------THIS LOGIC MAKES NO SENSE
  if(length(target_rhss) == 1){
    if(!outcome == target_rhss[outcome_asf_idx]){
      return(FALSE)
    } else{
      on_path_asfs_idx <- outcome_asf_idx
      on_path_outcomes <- target_rhss[on_path_asfs_idx]
      on_path_asfs_facs <- target_lhss_facs[on_path_asfs_idx]
    }
  } else {
    chain_asfs <- lapply(target_lhss_facs, function(x) match(x, target_rhss, nomatch = 0L))
  
    #which(previous_out_in_disjunct)
  
    #on_path_asfs_idx <- previous_out_in_disjunct[[which(lapply(previous_out_in_disjunct, sum) > 0)]]
    midpath_asfs_idx <- which(lapply(chain_asfs, sum) > 0)
    other_path_asfs_idx <- lapply(chain_asfs, function(y) y[y>0])
    
    on_path_asfs_idx <- sort(unique(c(midpath_asfs_idx, unlist(other_path_asfs_idx))))
    on_path_outcomes <- target_rhss[on_path_asfs_idx]
    if(!outcome %in% on_path_outcomes){return(FALSE)}
    
    on_path_asfs_facs <- target_lhss_facs[on_path_asfs_idx]
  #---------THIS LOGIC MAKES NO SENSE--------->
  }
  
  
  
  #test_factors <- unlist(lapply(lapply(test_disjuncts, cna:::tryparse), all.vars))
  
  #test_facs_check <- vector("logical", length(test_factors))
  #names(test_facs_check) <- test_factors
  
  
  #test_disjuncts <- c(test_disjuncts, "A")
  
  
  for (dis in test_disjuncts){
    
    dis_facs <- unlist(strsplit(dis, "\\*"))
    
    dis_facs_check <- vector("logical", length(dis_facs))
    names(dis_facs_check) <- dis_facs
    for (id in dis_facs){
      if (!id %in% unlist(on_path_asfs_facs)) {
        #dis_facs_check[names(dis_facs_check) == id] <- FALSE
        pa_check <- FALSE
      } else {
        
        #closest_path_asf_idx <- max(which(unlist(lapply(target_lhss_facs, function(x) id %in% x))))
        paths_idx <- which(unlist(lapply(target_lhss_facs[1:outcome_asf_idx], function(x) id %in% x)))
  
        pa_check <- vector("logical", length(paths_idx))
        for (pa in paths_idx){
          #extract co-factors for candidate factor and its effects on path to outcome
          candidate_cofacs <- cofac_extract(id, dis)
          #candidate_cofacs_neg <- case_flipper(candidate_cofacs)
          
          #test_fac_cofacs <- cofac_extract(id, unlist(target_lhss_disjuncts[pa:outcome_asf_idx]))  
          
          test_fac_cofacs <- lapply(unlist(target_lhss_disjuncts[pa:outcome_asf_idx]),
                                    function(x) cofac_extract(id, x))
          test_fac_cofacs <- unlist(test_fac_cofacs[!unlist(lapply(test_fac_cofacs, is.null))])
          
          #test_fac_cofacs_supp_temp <- test_fac_cofacs
          
          #always_on_cofacs <- paste0(unlist(test_fac_cofacs), collapse = "*")
          
          path_outs <- on_path_outcomes[pa:outcome_asf_idx]
          #temp <- unlist(target_lhss_disjuncts[pa:(outcome_asf_idx - 1)])
          temp <- unlist(target_lhss_disjuncts[pa:(outcome_asf_idx)])
          path_outs_disjuncts <- unlist(lapply(on_path_outcomes[pa:outcome_asf_idx], function(x) temp[grepl(x, temp)]))
          on_path_cofacs <- unlist(lapply(path_outs, function(x) cofac_extract(x, path_outs_disjuncts)))
          #cofacs <- c(unlist(test_fac_cofacs), unlist(on_path_cofacs), unlist(candidate_cofacs))
          cofacs <- c(unlist(test_fac_cofacs), unlist(on_path_cofacs))
          #cofacs <- c(test_fac_cofacs, on_path_cofacs)
          #cofacs_neg <- case_flipper(cofacs) 
          
          
          ctrl_exp_temp <- unlist(target_lhss_disjuncts[pa:outcome_asf_idx])
          #ctrl_exp_temp <- ctrl_exp_temp[!grepl(id, ctrl_exp_temp)] #this would not select disjuncts that feature the cand fac
          ctrl_exp_temp <- ctrl_exp_temp[!grepl(id, ctrl_exp_temp)]
          
          fac_alt_paths <- ctrl_exp_temp[grepl(id, ctrl_exp_temp)]
          
          ctrl_exp_temp <- unique(ctrl_exp_temp[!ctrl_exp_temp %in% c(on_path_outcomes[pa:outcome_asf_idx], path_outs_disjuncts, dis, id)])
          cand_other_disjs <- cand_disjuncts[!grepl(id, cand_disjuncts)]
          
          #test_fac_cofacs[!toupper(unlist(test_fac_cofacs)) %in% toupper(c(candidate_cofacs, on_path_cofacs))]
          
          test_fac_cofacs <- lapply(test_fac_cofacs, function(x) x[!toupper(x) %in% toupper(c(candidate_cofacs, on_path_cofacs))])
          if(length(test_fac_cofacs) >= 1){test_fac_cofacs[which(unlist(lapply(test_fac_cofacs, function(x) length(x)<1)))] <- NULL}
          
          
          ca_cofac_pres <- paste0(paste0(unique(c(candidate_cofacs, on_path_cofacs, unlist(test_fac_cofacs))), collapse = "*"))
          
          #ca_cofac_pres <- paste0("*", paste0(unique(cofacs), collapse = "*"))
          #ca_cofac_pres <- paste0(paste0(unique(cofacs), collapse = "*"))
          
          
          #ca_cofac_supp <- paste0("*!(", paste0(unique(c(candidate_cofacs, on_path_cofacs)), collapse = "*"), ")")
          #ca_cofac_supp <- paste0("*!(", paste0(unique(cofacs), collapse = "*"), ")")
          # sup_cofac_temp <- lapply(test_fac_cofacs_supp_temp, function(x)
          #   paste0("!(", paste0(x, collapse = "*"), ")"))
          # 
          sup_candidate_cofac_temp <- if(length(candidate_cofacs)>=1){paste0("!(", paste0(candidate_cofacs, collapse = "*"), ")")}else{NULL}
          
          #ca_cofac_supp <- if(length(sup_cofac_temp)>=1){paste0(paste0(sup_candidate_cofac_temp, collapse = "*"), "*", sup_candidate_cofac_temp)}else{NULL}
          
          
          #ca_cofac_supp <- paste0("!(", paste0(unique(cofacs), collapse = "*"), ")")
          
          
          ce_temp_cofacs <- paste0("!(", paste0(unique(c(ctrl_exp_temp, cand_other_disjs)), collapse = "+"), ")")
          #ce_temp_cofacs_sup <- paste0(unique(c(ctrl_exp_temp, cofacs_neg, cand_other_disjs)), collapse = "+")
          #ctrl_exp_temp <- paste0(ctrl_exp_temp,  collapse = "+")
          #ctrl_exp_cofacs_present <- paste0("!(", ce_temp_cofacs, ")")
          #ctrl_exp_cofacs_supp <- paste0("!(", ce_temp_cofacs_sup, ")")
         
          ##########################################
          
           if (length(cofacs) >= 1){
            ctrl_exp_cofacs_present <- ifelse(ce_temp_cofacs == "!()", ca_cofac_pres,  paste0(ce_temp_cofacs, "*" ,ca_cofac_pres))
            #ctrl_exp_cofacs_supp <- ifelse(ce_temp_cofacs == "!()", sup_candidate_cofac_temp, paste0(ce_temp_cofacs, "*", sup_candidate_cofac_temp))
            ctrl_exp_cofacs_supp <- if(ce_temp_cofacs == "!()"){sup_candidate_cofac_temp}else{paste0(ce_temp_cofacs, "*", sup_candidate_cofac_temp)}
          } else {
            ctrl_exp_cofacs_present <- ctrl_exp_cofacs_supp <- ifelse(ce_temp_cofacs == "!()", "", ce_temp_cofacs)
          }
          
          ctrl_exp_cofacs_present <- gsub("\\*$", "", ctrl_exp_cofacs_present)
          ctrl_exp_cofacs_supp <- gsub("\\*$", "", ctrl_exp_cofacs_supp)
          
          # if (length(c(candidate_cofacs, on_path_cofacs) >= 1)){
          #   ctrl_exp_cofacs_present <- paste0(ce_temp_cofacs, ca_cofac_pres)
          #   ctrl_exp_cofacs_supp <- paste0(ce_temp_cofacs, ca_cofac_supp)
          # } else {
          #   ctrl_exp_cofacs_present <- ctrl_exp_cofacs_supp <- ce_temp_cofacs
          # }
          
          
          
          
          #############################################33
          
          #ctrl_exp_temp <- unlist(lapply(on_path_outcomes, function(x) ctrl_exp_temp[-grepl(x, ctrl_exp_temp)]))
          
          ctrl_cond <- c(ctrl_exp_cofacs_present, ctrl_exp_cofacs_supp)
          ctrl_cond <- unique(ctrl_cond)
          cond_check <- vector("logical", length(ctrl_cond))
          names(cond_check) <- ctrl_cond
          for(cond in ctrl_cond){
          
          #dummy code background factor configurations
            full_path <- c(id, on_path_outcomes[pa:outcome_asf_idx]) #needs to come earlier, take out on_path outcomes from ctrl_cond
            #dum_dat <- fulldat[,-which(names(fulldat) %in% full_path)]
            dum_dat <- fulldat
            pre_dum_confs <- if(cond == ""){fulldat}else{ct2df(selectCases(cond, dum_dat))}
            
            if (nrow(pre_dum_confs) == 0L){
              cond_check[names(cond_check) == cond] <- FALSE
            } else {
            
              latent_confs <- unique(do.call('paste0', pre_dum_confs))
              allconfs_dummify <- do.call('paste0', dum_dat)
              tempdat <- fulldat
              tempdat$dum <- as.integer(allconfs_dummify %in% latent_confs)
              
              #testdat <- tempdat[,which(names(tempdat) %in% c(toupper(full_path), "dum"))]
              testdat <- tempdat[,which(names(tempdat) %in% c(toupper(id), outcome, "dum"))]
              
              testdat <- unique(testdat[testdat$dum == 1,])
              
              ####################
              if (!id == toupper(id)){
                testdat[, toupper(id)] <- as.integer(!testdat[, toupper(id)])
              }
              
              idvals <- unique(testdat[,toupper(id)])
              if (length(idvals) == 1){cond_check[names(cond_check) == cond] <- FALSE} else {
                mdat <- testdat[testdat[,toupper(id)] %in% idvals]
                cond_check[names(cond_check) == cond] <- ifelse(sum(mdat[,toupper(id)] == mdat[,outcome]) < 2, FALSE, TRUE)
                       
                       
              }
              
            
              ########################  
              
              
              # testmod <- paste0(id, "<->", outcome)
              # 
              # 
              # datcheck <- selectCases(testmod, testdat)
              # if (!all(dim(datcheck) == dim(testdat)) | nrow(testdat) == 1){cond_check[names(cond_check) == cond] <- FALSE} else {
              #   if (all(datcheck == testdat)) {cond_check[names(cond_check) == cond] <- TRUE}
              # }
              
              #if (all(selectCases(testmod, testdat) == testdat)) {cond_check[names(cond_check) == cond] <- TRUE}
              #if (identical(selectCases(testmod, testdat), testdat[1,])) {cond_check[names(cond_check) == cond] <- TRUE}
              #fix the rest
            }
            
          }
          # if (length(cond_check) == 1){
          #   dis_facs_check[names(dis_facs_check) == id] <- ifelse(cond_check, TRUE, FALSE)} else 
          #   {dis_facs_check[names(dis_facs_check) == id] <- ifelse(cond_check[names(cond_check) == ctrl_exp_cofacs_present] & !cond_check[names(cond_check) == ctrl_exp_cofacs_supp], TRUE, FALSE)}
           
          if (length(cond_check) == 1){
            pa_check[paths_idx == pa] <- ifelse(cond_check, TRUE, FALSE)} else 
            {pa_check[paths_idx == pa] <- ifelse(cond_check[names(cond_check) == ctrl_exp_cofacs_present] & !cond_check[names(cond_check) == ctrl_exp_cofacs_supp], TRUE, FALSE)}
          
          
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



m1 <- "(E*c+b*h*d+E*H*B<->G)*(b*H+c*E*b<->F)*(F*e+c*G*d<->A)"
m2 <- "b*H*e + c*G*d <-> A"
diff_correct(m1,m2)
correct3(m2,m1)

m1 <- "(E*c+b*h*d+E*H*B<->G)*(b*H+c*E*b<->F)*(F*e+c*G*d<->A)"
m2 <- "A*H  <-> F"
diff_correct(m1,m2)
correct3(m2,m1)

m1 <- "(E*c+b*h*d+E*H*B<->G)*(b*H+c*E*b<->F)*(F*e+c*G*d<->A)"
m2 <- "b  <-> F"
diff_correct(m1,m2)
correct3(m2,m1)

  
m1 <- "(E*c+b*h*d+E*H*B<->G)*(b*H+c*E*b<->F)*(F*e+c*G*d<->A)"
m2 <- "E  <-> F"
diff_correct(m1,m2)
correct3(m2,m1)

m1 <- "(E*c+b*h*d+E*H*B<->G)*(b*H+c*E*b<->F)*(F*e+c*G*d<->A)"
m2 <- "E*H  <-> G"
diff_correct(m1,m2)
correct3(m2,m1)

m1 <- "(A + B*X  <-> C)*(C*T + B <->E)*(E+A*G+X<->H)"
m2 <- "T*A <->H"

diff_correct(m1,m2)
correct3(m2,m1)


m1 <- "(A + B*X  <-> C)*(C*T + B <->E)*(E+A+X<->H)"
m2 <- "C*T + B + X<->E"

diff_correct(m1,m2)
correct3(m2,m1)


m1 <- "(A + B*X  <-> C)*(C*T + B <->E)*(E+A+X<->H)"
m2 <- "C*T * A<->E"

diff_correct(m1,m2)

correct3(m2,m1)


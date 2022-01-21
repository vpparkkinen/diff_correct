# Correctness funcs by MB
# --------------

library(stringr)
library(cna)
library(cnaOpt)



correct3 <- function(x,y){
  y <- noblanks(y)
  x <- noblanks(x)
  asf.y <- extract_asf(y)    
  asf.x <- extract_asf(x) 
  asf.x <- unlist(asf.x)
  out.y <- rhs(unlist(asf.y))
  test.out <- rhs(unlist(asf.x))
  dat.y <- ct2df(selectCases(y))
  nam.y <- names(dat.y)
  nam.x <-toupper(unlist(str_extract_all(x, "[a-zA-Z]+"))) 
  vary <- unique(c(out.y,nam.x))
  # vary <- c("H","G")
  tr <-  split(dat.y, dat.y[,nam.y[which(!(nam.y %in% vary))]])
  tr <- tr[unlist(lapply(lapply(tr, function(w) var(w[which(names(w) %in% test.out)])>0), any))]
  if(length(tr)==0) {out <- FALSE
  } else if(any(which(sapply(tr, is.null)))) {tr <- tr[-which(sapply(tr, is.null))]
  } else {tr <- lapply(tr, function(w) w[which(names(w) %in% nam.x)])}
  for(i in 1:length(tr)){
    rownames(tr[[i]]) <- NULL 
  }
 
  tr <- unique(as.matrix(tr))
  # lapply(tr,setequal)     
    
  if(length(unlist(test.out))==1){
      u <- vector("list", length(tr))
      for(i in 1:length(tr)){          
      # cco <- suppressWarnings(suppressMessages(conCovOpt(tr[[i]], test.out)))
      # u[[i]] <-  suppressWarnings(suppressMessages(paste0(DNFbuild(selectMax(cco),test.out,reduce = "ereduce"),"<->",test.out)))
      cond <- getCond(tr[[i]], test.out)
      u[[i]] <-  paste0(suppressMessages(ereduce(cond,tr[[i]],full = FALSE)),"<->",test.out)
      }
      u <- unlist(u)
      gr <- unique(expand.grid(asf.x,u))
      ma <- mapply(function(w,v)is.submodel(w,v), w=gr$Var1, v=gr$Var2)
      out <- any(ma)
      # out <-  any(unlist(lapply(u,function(v)is.submodel(unlist(asf.x),v))))
                } else {
                u <- vector("list", length(o))
                for(i in 1:length(o)){
                  cco <- conCovOpt(w, o[i])
                 u[[i]] <-  paste0(DNFbuild(selectMax(cco),o[i],reduce = "ereduce"),"<->",o[i])
                }
                u <- unlist(u) 
                ma <- mapply(function(w,v)is.submodel(w,v), w=b, v=u)
              out <- all(apply(ma,1,any))
              }
  
      # unique(unlist(lapply(tr, function(w) 
        # asf(cna(w, ordering=list(test.out),strict=T,rm.dup.factors = F, rm.const.factors = F))$condition
        # )))
  return(out)
}

correct2b <- function(x,y){
  out <-  as.vector(is.submodel(x,y))
  if(out==FALSE) {
  m1 <- cna:::noblanks(x)
  m2 <- cna:::noblanks(y)
  dat2 <- ct2df(selectCases(m2))
  b <- cna:::extract_asf(m1)
  b <- unlist(b)
  o <-  cna:::rhs(unlist(b))
  reihe <- as.data.frame(cbind(b,o))
  reihe <- reihe[order(reihe$o),]
  reihe$b <- factor(reihe$b, levels=reihe$b)
  reihe$o <- factor(reihe$o, levels=reihe$o)
  b <- reihe$b 
  o <- reihe$o
  
  max.comp <- ncol(dat2)-1
  minsufc <- msc(cna(dat2, ordering=list(as.character(o)), strict=T,maxstep=c(max.comp,1,max.comp), suff.only = T,
                     rm.dup.factors = F, rm.const.factors = F))
  
  # minsufc$outcome<- factor(minsufc$outcome, levels=unique(minsufc$outcome))
  minsufc <- split(minsufc, minsufc$outcome)
  check2 <- vector("list", length(o))
  for(i in 1:length(o)){
  dis <- unlist(stringr:::str_split(cna:::lhs(b[i]),"[+]"))
  dis <- unlist(lapply(dis,function(x)paste0(x,"->",o[i])))
  
  subcheck <- lapply(dis, function(x) lapply(minsufc[[i]]$condition, function(y) is.submodel(x,y)))
  
  # t.recover <- lapply(subcheck, function(x) lapply(x, function(y) if(y==TRUE) attr(y,"target")))
  # t.recover <-lapply(t.recover, function(x) unlist(x))
  check2[[i]] <- all(unlist(lapply(subcheck, function(x) any(unlist(x))))  )
  } 
  out <- all(unlist(check2))
  if(out==TRUE){

  # 
  check3 <- vector("list", length(o))
  for(i in 1:length(o)){
    mins <- as.character(cna:::lhs(minsufc[[i]]$condition))
    mi.dat <- condition(mins,dat2, force.bool = T)
    mi.dat <- as.data.frame(lapply(mi.dat, function(x)x[,1]))
    mi.dat  <-  cbind(mi.dat ,dat2[,which(names(dat2)%in% o[i])])
    subst.var <-paste0("V",100:(99+length(mins)))
    key <- matrix(c(mins,subst.var),nrow = length(subst.var), ncol =2)
    names(mi.dat) <- c(key[,2],as.character(o[i]))
    models <- suppressMessages(cna(mi.dat,ordering=list(as.character(o[i])), strict=T, rm.dup.factors = F, rm.const.factors = F,
          maxstep=c(1,length(mins),length(mins))))
    
    models <-asf(models)$condition

    for(j in 1:nrow(key)){
      models <- stringr:::str_replace_all(models, key[j,2],key[j,1])
     }
   
    dis <- unlist(stringr:::str_split(cna:::lhs(b[i]),"[+]"))
    dis <- unlist(lapply(dis,function(x)paste0(x,"->",o[i])))
     
    tr <- lapply(models , function(x) lapply(dis, function(y) is.submodel(y,x)))
    tr <- lapply(tr,function(x) unlist(x))
    tr <- lapply(tr, function(x) all(x))
    tr <-  any(unlist(tr))
    check3[[i]] <- tr
   }
  out <- all(unlist(check3))  
  
  }

 
}
 
  return(out)
}

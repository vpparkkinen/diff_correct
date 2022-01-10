# Correctness 3 by MB
# --------------

library(stringr)
library(cna)




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

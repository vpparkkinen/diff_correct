correct2 <- function(x,y){
  out <-  as.vector(is.submodel(x,y))
  if(out==FALSE) {
    m1 <- cna:::noblanks(x)
    m2 <- cna:::noblanks(y)
    dat2 <- selectCases(m2)
    b <- cna:::extract_asf(m1)
    o <-  cna:::rhs(unlist(b))
    # 
    if(length(unlist(b))==1){
      cond <- cna:::getCond(dat2, o)
      u <-  paste0(suppressMessages(cnaOpt:::ereduce(cond,dat2,full = FALSE)),"<->",o)
      
      # cco <- conCovOpt(dat2, o)
      # u <-  paste0(DNFbuild(selectMax(cco),o,reduce = "ereduce"),"<->",o)
      out <-  any(unlist(lapply(u,function(v)is.submodel(b,v))))
    } else {
      u <- vector("list", length(o))
      for(i in 1:length(o)){
        #  cco <- conCovOpt(dat2, o[i])
        # u[[i]] <-  paste0(DNFbuild(selectMax(cco),o[i],reduce = "ereduce"),"<->",o[i])
        cond <- cna:::getCond(dat2, o[i])
        u[[i]] <-  paste0(suppressMessages(cnaOpt:::ereduce(cond,dat2,full = FALSE)),"<->",o[i])
      }
      u <- unlist(u) 
      ma <- mapply(function(w,v)is.submodel(w,v), w=b, v=u)
      out <- all(apply(ma,1,any))
    }
  }
  return(out)
}

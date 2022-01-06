# Correctness 2b by MB
# --------------

library(stringr)
library(cna)




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
     minsufc <- msc(cna(dat2, ordering=list(o), strict=T,maxstep=c(max.comp,1,max.comp), suff.only = T,
                        rm.dup.factors = F, rm.const.factors = F))
     minsufc <- split(minsufc, minsufc$outcome)
        check2 <- vector("list", length(o))
        for(i in 1:length(o)){
        dis <- unlist(str_split(cna:::lhs(b[i]),"[+]"))
        dis <- unlist(lapply(dis,function(x)paste0(x,"->",o[i])))
        
        subcheck <- lapply(dis, function(x) lapply(minsufc[[i]]$condition, function(y) is.submodel(x,y)))
        check2[[i]] <- all(unlist(lapply(subcheck, function(x) any(unlist(x))))  )
        } 
  out <- all(unlist(check2))
   if(out==TRUE){
    
        check3 <- vector("list", length(o))
        for(i in 1:length(o)){
          mins <- as.character(cna:::lhs(minsufc[[i]]$condition))
          mi.dat <- condition(mins,dat2, force.bool = T)
          mi.dat <- as.data.frame(lapply(mi.dat, function(x)x[,1]))
          mi.dat  <-  cbind(mi.dat ,dat2[,which(names(dat2)%in% o[i])])
          subst.var <-paste0("V",100:(99+length(mins)))
          key <- matrix(c(mins,subst.var),nrow = length(subst.var), ncol =2)
          names(mi.dat) <- c(key[,2],as.character(o[i]))
          models <- suppressMessages(cna(mi.dat,ordering=list(o[i]), strict=T, rm.dup.factors = F, rm.const.factors = F,
                maxstep=c(1,length(mins),length(mins))))
          
          models <-asf(models)$condition
      
          for(j in 1:nrow(key)){
            models <- str_replace_all(models, key[j,2],key[j,1])
           }
         
          dis <- unlist(str_split(cna:::lhs(b[i]),"[+]"))
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


# Examples

# y <- "(A + B*X  <-> C)*(C*T + G <->E)"
# x <- "C*T + A <->E"
# 
# correct2b(x,y)
# 
# 
# y <- "(E*c+b*h*d+E*H*B<->G)*(b*H*J+c*E*b*k<->F)*(F*e+c*G*d<->A)"
# x <- "b*H*J <-> A"
# 
# correct2b(x,y)
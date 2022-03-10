m1 <- "(E*c+b*h*d+E*H*B<->G)*(b*H+c*E*b<->F)*(F*e+c*G*d<->A)"
m2 <- "b*H*e + c*G*d <-> A"
diff_correct(m1,m2)
correct2b(m2,m1)

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

# <---script for testing for false negatives
set.seed(50)
targets <- replicate(10, randomCsf(7, n.asf = 3))
expansions <- lapply(targets, chain.expand2b)

candidates <- lapply(expansions, `[`, 2 )
candidates <- lapply(candidates, unlist)


#lapply(candidates[[1]], function(x) diff_correct(targets[1], x))

results <- vector("list", length(targets))
for(i in seq_along(targets)){
  results[[i]] <- lapply(candidates[[i]], function(x) diff_correct(targets[i], x))
}

uresults <- unlist(results)
wrong <- which(uresults == FALSE)
uresults[wrong]
all(unlist(results))

lapply(results, function(x) which(unlist(x) ==FALSE))


#problem case
m1 <- "(f*a*G*e+A*E*g*f+A*e*F<->C)*(f*c*g*A+G*e*c<->D)"
m2 <- "f*c*g*A+G*e*c<->D"


# script for testing for false negatives --->

dats <- vector("list", length(targets)) 

recovered <- lapply(targets, function(x) cna(ct2df(selectCases(x))))
asfs <- lapply(recovered, asf)

negres <- vector("list", length(targets))
for(n in seq_along(targets)){
  t <- lapply(candidates[[n]], function(x) is.submodel(asfs[[n]]$condition, x))  
  hits <- unique(unlist(lapply(t, which)))
  negatives <- asfs[[n]]$condition[-hits]
  negres[[n]] <- lapply(negatives, function(x) diff_correct(targets[n], x))
}



t <- lapply(b, function(x) is.submodel(a$condition,x))
hits <- unique(unlist(lapply(t, which)))
anegs <- a$condition[-hits]

lapply(anegs, function(x) diff_correct(targets[30], x))

for (i in seq_along(asfs)){
  dats[[i]] <-  
}



# mapply(function(x,y) lapply(y, function(z) diff_correct(x,z)), targets, candidates)
# lapply(candidates[[2]], function(x) diff_correct(targets[2], x))
# 
m1 <-  "(C*f*d*e+D*c*e<->A)*(F*d*e<->B)*(f*e*c+F*a+D*f*e<->G)"
m2 <-  "E*F<->G"
# 
m1 <- "(A*G+a*g*B<->E)*(E*G+d*E<->F)"
m2 <- "E*G<->F"
diff_correct(m1,m2)

###how to determine exactly what is allowed to vary given the below

m1 <- "(A*D+A*g<->B)*(a*g<->F)*(g*b*C+d*B<->E)"
m2 <-  "g*b*C<->E"

m1 <- "(E*C*F*D+c*f*d+C*f*e*D<->A)*(d*e*c+c*D*F<->B)"
m2 <-  "E*c+F*e<->B"
diff_correct(m1,m2)


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

targets <- replicate(10, randomCsf(7, n.asf = 3))
expansions <- lapply(targets, chain.expand2b)

candidates <- lapply(expansions, `[`, 2 )
candidates <- lapply(candidates, unlist)


lapply(candidates[[1]], function(x) diff_correct(targets[1], x))

results <- vector("list", length(targets))
for(i in seq_along(targets)){
  results[[i]] <- lapply(candidates[[i]], function(x) diff_correct(targets[i], x))
}


mapply(function(x,y) lapply(y, function(z) diff_correct(x,z)), targets, candidates)
lapply(candidates[[2]], function(x) diff_correct(targets[2], x))


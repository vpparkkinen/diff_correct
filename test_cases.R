condition("(F->E)->(X->E)", test2, force.bool = T) #1 
condition("(X->E)->(F->E)", test2, force.bool = T) #2
#X the confounder, F candidate
#if #1 frequency = 1, and #2 frequency < 1, then X is a confounder


#THIS GIVES FALSE POSITIVE
m1 <- "(Q+X<->F)*(A + B*G <->C)*(C*X + D <-> E)"
m2 <- "F<->E"
diff_correct(m1,m2)


#THIS DOES NOT
m1 <- "(Q+X<->F)*(C*X + D <-> E)"
m2 <- "F<->E"
diff_correct(m1,m2)



m1 <- "(E*c+b*h*d+E*H*B<->G)*(b*H+c*E*b<->F)*(F*e+c*G*d<->A)"
m2 <- "b*H*e + c*G*d <-> A"
diff_correct(m1,m2)
correct2(m2,m1)

m1 <- "(E*c+b*h*d+E*H*B<->G)*(b*H+c*E*b<->F)*(F*e+c*G*d<->A)"
m1 <- "(b*h+E*H<->G)*(b*H+c*E<->F)*(F+c*G<->A)"
is.inus(m1)
m2 <- "A*H  <-> F"
diff_correct(m1,m2)
correct2(m2,m1)

m1 <- "(E*c+b*h*d+E*H*B<->G)*(b*H+c*E*b<->F)*(F*e+c*G*d<->A)"
m2 <- "b  <-> F"
diff_correct(m1,m2)
correct2(m2,m1)


m1 <- "(E*c+b*h*d+E*H*B<->G)*(b*H+c*E*b<->F)*(F*e+c*G*d<->A)"
m2 <- "E  <-> F"
diff_correct(m1,m2)
correct2(m2,m1)

m1 <- "(E*c+b*h*d+E*H*B<->G)*(b*H+c*E*b<->F)*(F*e+c*G*d<->A)"
m2 <- "E*H  <-> G"
diff_correct(m1,m2)
correct2(m2,m1)

m1 <- "(A + B*X  <-> C)*(C*T + B <->E)*(E+A*G<->H)"
m2 <- "T*A <->H"

diff_correct(m1,m2)
correct2(m2,m1)

# 
m1 <- "(A + B*X  <-> C)*(C*T + B <->E)*(E+A+X<->H)"
m2 <- "C*T + B + A<->E"
# 
diff_correct(m1,m2)
correct2(m2,m1)
# 
# 
m1 <- "(A + B*X  <-> C)*(C*T + B <->E)*(E+A+X<->H)"
m2 <- "C*T * A<->E"
# 
diff_correct(m1,m2)


# 
# 
correct2(m2,m1)

m1 <- "(A + B*X  <-> C)*(C*T + B <->E)*(E+A+X<->H)"
m2 <- "C*T * A<->H"

diff_correct(m1,m2)
correct2(m2,m1)
# 

m1 <- "(A + B*X  <-> C)*(C*T*r + Z <->E)*(E*y+A+B*X<->H)"
#is.inus(m1)
m2 <- "C*T * A + B*X<->H"
diff_correct(m1,m2)
correct2(m2,m1)


m1 <- "(A + B*X  <-> C)*(C*T*r + Z <->E)*(E*y+A+B*X<->H)"
#is.inus(m1)
m2 <- "E + B<->H"
diff_correct(m1,m2)
correct2(m2,m1)

m1 <- "(A + B*X  <-> C)*(C*T*r + Z <->Q)*(E*y+A+B*X<->H)"
#is.inus(m1)
m2 <- "E * A + B<->H"

diff_correct(m1,m2)
correct2(m2,m1)

m1 <- "(A + B*X  <-> C)*(C*T*r + Z <->Q)*(E*y+A+B*X<->H)"
#is.inus(m1)
m2 <- "A + B<->H"

diff_correct(m1,m2)
correct2(m2,m1)

m1 <- "(f*D+c*G<->A)*(G*b+F*a<->H)"
m2 <-  " g<-> H"

m1 <- "(A + B*X  <-> C)*(C*T + G <->E)"
m2 <- "C*T + B <->E"

diff_correct(m1,m2)
correct2(m2,m1)

m1 <- "(f*D+c*G<->A)*(G*b+F*a<->H)"
m2 <-  " g + a <-> H"

diff_correct(m1,m2)
correct2(m2,m1)

m1 <- "(A + B*G <->C)*(B*F + D <-> E)"
m2 <- "C*F <-> E"

diff_correct(m1,m2)
correct2(m2,m1)

m1 <- "(A + B*G <->C)*(C*F + D <-> E)"
m2 <- "B<->E"

diff_correct(m1,m2)
correct2(m2,m1)

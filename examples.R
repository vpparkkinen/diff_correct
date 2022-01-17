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
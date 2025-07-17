
Dalv<-10
Kaq<-1e-3
Y<-2
deltaS<-3
P<-1
Salvprev<-2
E<-1
Smax<-20

# below is the solution for Qaq
Qaq<-(Dalv*Kaq*Y*(-deltaS-P-Salvprev+E))/(Dalv*Kaq*Y-Smax)
Qaq


# check for above
Kaq*(Dalv*(Salvprev+(deltaS+Qaq)+P-E)/Smax)*Y

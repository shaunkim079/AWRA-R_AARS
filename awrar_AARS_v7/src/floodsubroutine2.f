c sample subroutine to include for R with dynamic Q update
c Initial code written by Justin Hughes
c Code updated by Dushmanta Dutta on the 01/11/12
c
       SUBROUTINE flood(xx,Vfp,Qin,Qx,Qupdate,Qr,a1,paramj,floodAlpha, 
     1 			P, E, Ksat) 
       implicit none
       INTEGER xx, i
       DOUBLE PRECISION Vfp,Qin,Qx,Qupdate,Qr,a1,paramj,floodAlpha,
     1				 P,E,Ksat,Qfl
       DIMENSION Vfp(xx),Qin(xx),Qx(xx),Qupdate(xx),Qr(xx), a1(xx), 
     1 paramj(17),P(xx),E(xx),Qfl(xx)

       DO i=2,xx

       IF(Qin(i)-paramj(14)>=0)THEN
		Qx(i)=Qin(i)-paramj(14)
	   ELSE
		Qx(i)=0
	   END IF
	   a1(i) = floodAlpha*(Qx(i)+Vfp(i-1))

	   If (a1(i) > 0) THEN
	   Qfl(i) = ((E(i)-P(i))+Ksat)*a1(i)
	   ELSE
	   Qfl(i) = 0
	   ENDIF

	   If ((Vfp(i-1)+Qx(i)-Qfl(i)) > = 0) THEN
	   Vfp(i)=Vfp(i-1)+Qx(i)-Qfl(i)
	   ELSE
	   Vfp(i) = 0
	   ENDIF

	   Qr(i)=Vfp(i)*paramj(15)
	   Vfp(i) = Vfp(i) - Qr(i)

	   If ((Qin(i) - Qx(i) + Qr(i)) > = 0) THEN	! Added by Julien
	   Qupdate(i) = Qin(i) - Qx(i) + Qr(i)
	   ELSE				! Added by Julien
	   Qupdate(i) = 0	! Added by Julien
	   ENDIF 			! Added by Julien

       ENDDO
 
       END SUBROUTINE   		

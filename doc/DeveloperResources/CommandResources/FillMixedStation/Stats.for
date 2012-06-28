      PROGRAM STAT
      DIMENSION Q(2,150,13,300), TEMP1(150), TEMP2(150), EVX(2,13),
     *EMX(2,13), SDX(2,13),CVX(2,13), CSX(2,13), CKX(2,13), RX1(2,13),
     *DIFM(13),DIFSD(13),DIFCV(13),DIFSK(13),DIFR(13),NMON(2,16)
C *********************************************************************
C ***
C *** ESTIMATION OF STATISTICS FOR ACTUAL AND EXTENDED DATA SETS FOR
C *** SERIES OF EXTENDED MONTHLY STREAMFLOW DATA
C *** DATA INPUT REQUIRED:
C *** STATION NUMBER = STANO
C *** NUMBER OF YEARS OF DATA = NYRS
C *** Q = MONTHLY FLOWS ((ACTUAL,ESTIMATED),NYRS,(MONTHLY AND ANNUAL)) 
C *** TEMP1 = TEMPORARY ARRAY
C ***	TEMP2 = TEMPORARY ARRAY (LAGGED FLOWS)
C *** EVX = MEAN OF SERIES
C *** EMX = MEDIAN OF SERIES
C *** SDX = STANDARD DEVIATION OF SERIES
C *** CVX = COEFFICIENT OF VARIATION OF SERIES
C *** CSX = SKEWNESS
C *** CKX = KURTOSIS 
C *** RX1 = LAG CORRELATION
C ***
C *********************************************************************
C
C *** READ STATION NUMBER AND NUMBER OF YEARS OF DATA
C     NFILL=0 DON'T FILL MISSING DATA ON EXTENDED DATA SETS
C           1 FILL MISSING DATA WITH MONTHLY MEANS
C
      character*16 FILENAME
	CHARACTER*9 NSTA(300),STAF(300)
      INTEGER*4 YEAR(150),STA(300),YEARF(300,300),JYRS(300)
c
      write(6,'(a)') ' Enter the file name with the missing data: '
      read (5,'(a16)') FILENAME
      open(4,file=FILENAME)
      write(6,'(a)') ' Enter the file name with the filled data: '
      read (5,'(a16)') FILENAME
      open(7,file=FILENAME)
      write(6,'(a)') ' Enter the desired summary output file name: '
      read (5,'(a16)') FILENAME
      open(8,file=FILENAME)
	write(6,'(a)') ' Enter the desired filled data output file name: '
      read (5,'(a16)') FILENAME
      open(9,file=FILENAME)
C 
C *** READ IN NUMBER OF STATIONS, NUMBER OF YEARS, AVERAGE FILL
C
      READ(4,1000) NOST,NYRS,NFILL
 1000 FORMAT(3I4)
      READ(7,1000) NST,NY
      IF(NOST.NE.NST.OR.NYRS.NE.NY) GO TO 2000
C
C *** READ MONTHLY AND ANNUAL DATA, FIRST ACTUAL THEN FILLED
C
      DO 10 I=1,NYRS

	DO 10 NS=1,NOST
      READ(4,1030) YEAR(I),NSTA(NS),(Q(1,I,N,NS), N=1,12)
 1030 FORMAT(I4,A9,4X,12F8.0)
C
C *** Caculate Annual Totals for years that have 12 months of data
C
	Q(1,I,13,NS)=0.0
	DO 11 N=1,12
	IF(Q(1,I,N,NS).GE.0.0) THEN
	Q(1,I,13,NS)=Q(1,I,N,NS)+Q(1,I,13,NS)
	ELSE
	Q(1,I,13,NS)=Q(1,I,N,NS)
	GO TO 12
	ENDIF
   11 CONTINUE
   12 CONTINUE
   10 CONTINUE
	K=1
      STAF(1)='        '
	YEARF(1,1)=0
	DO 20 I=1,NYRS
	DO 20 NS=1,NOST
      READ(7,1031) (Q(2,I,N,NS), N=1,12)
 1031 FORMAT(17X,12F8.0)
	Q(2,I,13,NS)=0.0
	DO 21 N=1,12
	IF(Q(2,I,N,NS).GE.0.0)	THEN
	Q(2,I,13,NS)=Q(2,I,N,NS)+Q(2,I,13,NS)
	ELSE
	Q(2,I,13,NS)=Q(2,I,N,NS)
	GO TO 20
	ENDIF
   21 CONTINUE
   20 CONTINUE
C
C *** PUT MONTHLY DATA IN ARRAY FOR CALCULATING STATISTICS
C
      DO 25 NS=1,NOST
	DO 30 I=1,2
	DO 35 M=1,13
      DO 40 N=1,NYRS
   40 TEMP1(N)=Q(I,N,M,NS)
C
C *** CALL ROUTINE TO CALCULATE STATISTICS
C
	CALL MOMEN3(NYRS,TEMP1,-999.,EVX(I,M),EMX(I,M),SDX(I,M),CVX(I,M),
     *CSX(I,M),CKX(I,M),RX1(I,M),2,0,6,NMON(I,M))
C
C *** PUT MONTHLY VALUES IN ARRAY TO RUN LAG-1 CORRELATION
C
      IF(M.EQ.13) GO TO 30
      NBEGIN=1
      MM=M-1
      IF(MM) 50,50,60
   50 NBEGIN=2
      MM=12
   60 NP=NYRS+1-NBEGIN
	J=0
      DO 70 N=1,NP
	IF((Q(I,(N+NBEGIN-1),M,NS).LE.-999.).OR.(Q(I,N,MM,NS).LE.-999.))
     *GO TO 70
      J=J+1
	TEMP1(J)=Q(I,(N+NBEGIN-1),M,NS)
      TEMP2(J)=Q(I,N,MM,NS)
   70 CONTINUE
C
C **** CALL SUBROUTINE TO CALCULATE MONTHLY LAG CORRELATION COEF.
C
      RX1(I,M)=-999.
      IF(J.GE.6) CALL CORREL(J,TEMP1,TEMP2,RX1(I,M))
   35 CONTINUE
   30 CONTINUE
C
C **** CACULATE DIFFERENCES IN STATISTICS
C
	DO 100 J=1,13
	DIFM(J)=0.
	IF(EVX(1,J).GT.0.) DIFM(J)=100*(EVX(2,J)-EVX(1,J))/EVX(1,J)
	IF((EVX(1,J).LE.-999.).OR.(EVX(2,J).LE.-999.)) DIFM(J)=-999.
	DIFSD(J)=0.
	IF(SDX(1,J).GT.0.) DIFSD(J)=100*(SDX(2,J)-SDX(1,J))/SDX(1,J)
	IF((SDX(1,J).LE.-999.).OR.(SDX(2,J).LE.-999.)) DIFSD(J)=-999.
	DIFCV(J)=0.
	IF(CVX(1,J).GT.0.) DIFCV(J)=100*(CVX(2,J)-CVX(1,J))/CVX(1,J)
	IF((CVX(1,J).LE.-999.).OR.(CVX(2,J).LE.-999.)) DIFCV(J)=-999.
	DIFSK(J)=0.
	IF(CSX(1,J).GT.0.) DIFSK(J)=100*(CSX(2,J)-CSX(1,J))/CSX(1,J)
	IF((CSX(1,J).LE.-999.).OR.(CSX(2,J).LE.-999.)) DIFSK(J)=-999.
	DIFR(J)=0.
	IF(RX1(1,J).GT.0.) DIFR(J)=100*(RX1(2,J)-RX1(1,J))/RX1(1,J)
  100	IF((RX1(1,J).LE.-999.).OR.(RX1(2,J).LE.-999.)) DIFR(J)=-999.
C
C **** WRITE OUTPUT
C
      WRITE(8,1002) NSTA(NS),NYRS
 1002 FORMAT(/
     *,'*** Station No: ',A9,' ***',/, '*** Number of Years: ',I4,' ***'
     *,//,28X,'Oct',5X,'Nov',5X,'Dec',5X,'Jan',5X,'Feb'
	1,5X,'Mar',5X,'Apr',5X,'May',5X,'Jun',5X,'Jul',5X,'Aug',5X,'Sep'
     2,3X,'Total',/)
	WRITE(8,1010) ((NMON(I,J),J=1,13),I=1,2)
	WRITE(8,1003) ((EVX(I,J),J=1,13),I=1,2)
      WRITE(8,1011) (DIFM(J),J=1,13)
	WRITE(8,1004) ((EMX(I,J),J=1,13),I=1,2)
	WRITE(8,1005) ((SDX(I,J),J=1,13),I=1,2)
      WRITE(8,1012) (DIFSD(J),J=1,13)
      WRITE(8,1006) ((CVX(I,J),J=1,13),I=1,2)
	WRITE(8,1015) (DIFCV(J),J=1,13)
      WRITE(8,1007) ((CSX(I,J),J=1,13),I=1,2)
      WRITE(8,1013) (DIFSK(J),J=1,13)
      WRITE(8,1009) ((RX1(I,J),J=1,13),I=1,2)
      WRITE(8,1014) (DIFR(J),J=1,13)
 1010 FORMAT(1H ,'Original No.Months/Yrs',13I8,/,
     1       1H ,'Extended No.Months/Yrs',13I8,/)
 1003 FORMAT(1H ,'Original Mean          ',13F8.0,/,
     1      1H ,'Extended Mean          ',13F8.0)
 1004 FORMAT(1H ,'Orginal Median         ',13F8.0,/,
     1      1H ,'Extended Median        ',13F8.0,/)
 1005 FORMAT(1H ,'Original StD.          ',13F8.0,/,
     1      1H ,'Extended StD.          ',13F8.0)
 1006 FORMAT(1H ,'Original Coef. of Var. ',13F8.2,/,
     1      1H ,'Extended Corf. of Var. ',13F8.2,/)
 1007 FORMAT(1H ,'Original Skewness      ',13F8.2,/,
     1      1H ,'Extended Skewness      ',13F8.2)
 1009 FORMAT(1H ,'Original Lag-1 Correl. ',13F8.2,/,
     1      1H ,'Extended Lag-1 Correl. ',13F8.2)
 1011 FORMAT(1H ,'Difference - Means %   ',13(F7.1,'%'),/) 
 1012 FORMAT(1H ,'Difference - StDs. %   ',13(F7.1,'%'),/)
 1015 FORMAT(1H ,'Difference - C.V.  %   ',13(F7.1,'%'),/)
 1013 FORMAT(1H ,'Difference - Skews  %  ',13(F7.1,'%'),/) 
 1014 FORMAT(1H ,'Difference - Corrs. %  ',13(F7.1,'%'),/) 
C 
C *** IF NFILL = 1 FILL IN MISSING DATA IN FILLED FILE WITH AVERAGE MONTHLY VALUES
C
	IF (NFILL.EQ.1)THEN
	 IFLAG=0
	 J=1
	 DO 200 N=1,NYRS
	 IFILL=0
	 IPOT=0
	 DO 199 M=1,13
	 IF ((Q(2,N,M,NS).LE.-998.).AND.(M.NE.13)) THEN
	   IF(IFLAG.EQ.0) THEN
           IFLAG=1
	     WRITE(8,1100)
 1100      FORMAT(' Data was Filled with Averages for some Months in '
     *,'the Following Years:'/'     YEAR  Potential  Actual'/
     *                        '            to fill   filled')
	     ELSE
	   ENDIF
	      K=K+1
		  IF(STAF(K-1).EQ.'         ') STAF(K-1)=NSTA(NS)
		  IF(NSTA(NS).NE.STAF(K-1)) THEN
		   STAF(K)=NSTA(NS)
		   ELSE
	       K=K-1
		  ENDIF
	      J=J+1
		  IF(YEARF(K,J-1).EQ.0) YEARF(K,J-1)=YEAR(N)
		  IF(YEAR(N).GT.YEARF(K,J-1)) THEN
		  YEARF(K,J)=YEAR(N)
	      ELSE
		  J=J-1
		  ENDIF
		  IPOT=IPOT+1
	   IF(EVX(2,M).GE.-998.) THEN
	     IFILL=IFILL+1
		 Q(2,N,M,NS)=EVX(2,M)
	    ELSE
	   ENDIF
	  ELSE
	 ENDIF
  199  CONTINUE
       IF(IFLAG.GT.0) WRITE(8,1101) YEARF(K,J),IPOT,IFILL
 1101  FORMAT(5X,I4,2X,2I8)
  200  CONTINUE 
        IF(IFLAG.EQ.0) THEN
	  WRITE(8,1102)
 1102	  FORMAT(' All Data was Filled with the Mixed Station Model',
     *//)
	  ELSE
        JYRS(K)=J
	  ENDIF
	 ELSE
	ENDIF	
   25 CONTINUE
C
C *** WRITE SUMMARY OF STATIONS AND YEARS IN WHICH AVERAGES WERE USED
C
      IF(STAF(1).EQ.'         ') GO TO 1990
      WRITE(8,1980) 
      WRITE(9,1984) 
 1980 FORMAT(/,' Stations in which some Data attempted to be Filled'
     *' with Average Monthly Values Were:')
 1984 FORMAT('# Stations in which some Data attempted to be Filled'
     *' with Average Monthly Values Were:')
	DO 1981 KK=1,K
  	WRITE(8,1982) STAF(KK),(YEARF(KK,JJ),JJ=1,JYRS(KK)) 
 1981	WRITE(9,1983) STAF(KK),(YEARF(KK,JJ),JJ=1,JYRS(KK)) 
 1982 FORMAT('  Station:  ',A9,30('    YEARS:',10(I6),/,20X))
 1983 FORMAT('# Station:  ',A9,30('    YEARS:',10(I6),/,'#'21X))
C
C *** WRITE NEW FILE OF FILLED DATA
C
 1990 DO 1992 I=1,NYRS
	DO 1992 NS=1,NOST
 1992	WRITE(9,1030) YEAR(I),NSTA(NS),(Q(2,I,N,NS), N=1,13)
 2000 STOP
      END
C
C ROUTINE TO CACULATE MONTHLY AUTOCORRELATION
C


      SUBROUTINE CORREL(N,X,Y,COR)
      DIMENSION X(150),Y(150)
      B5=0.
      B6=0.
      B7=0.
      B8=0.
      B9=0.
      A=0.
      B=0.
      C=0.
      D=0.
      DO 10 I=1,N
      B5=X(I)+B5
      B6=Y(I)+B6
      B7=(X(I)*Y(I))+B7
      B8=(X(I)**2)+B8
      B9=(Y(I)**2)+B9
      A=X(I)*Y(I)+A
      B=X(I)+B
      C=Y(I)+C
      D=(X(I)**2)+D
   10 CONTINUE
      A=A*N
      D=D*N
      A6=(((B5*B6)/N)-B7)/(((B5**2.)/N)-B8)
      A8=((B8/N)-((B5/N)**2.))**0.5
      A9=((B9/N)-((B6/N)**2.))**0.5
	COR=0
C
C *** UNBIASED ESTIMATE OF CORRELATION COEF.
C
C      COR=(1+N*(A6*A8)/(A9))/(N-4)
      IF ((A9.GT.0.).AND.(A8.GT.0.)) COR=(A6*A8)/(A9)
	RETURN
      END
C
C *** STATISTICS SUBROUTINE, SALAS (1990) ***
C
      SUBROUTINE MOMEN3 (N,X,XMISS,EVX,EMX,SDX,CVX,CSX,CKX,RX1,IBIAS, 
     1IPRINT,IUNITW,L) 
C     ****************************************************************** 
C     ESTIMATION OF SAMPLE MEAN, MEDIAN, STANDARD DEVIATION, 
C     COEFFICIENT OF VARIATION,  SKEWNESS COEFFICIENT,  KURTOSIS 
C     COEFFICIENT  AND LAG-1 SERIAL CORRELATION COEFFICIENT 
C     (BIASED AND UNBIASED ESTIMATION) IN THE PRESENCE OF 
C     MISSING OBSERVATIONS 
C     X(I),I= 1,...N: SAMPLE DATA        N    =SAMPLE SIZE WITH MISSING 
C     XMISS = VALUE ASSIGNED TO MISSING OBSERVATIONS 
C     EVX   = SAMPLE MEAN                EMX   = SAMPLE MEDIAN 
C     SDX   = STANDARD DEVIATION         CVX   = COEFF. OF VARIATION 
C     CSX   = SKEWNESS COEFFICIENT       CKX   = KURTOSIS COEFFICIENT 
C     RX1   = LAG-1 SERIAL CORRELATION COEFFICIENT 
C     IBIAS = 1, BIASED ESTIMATES        IBIAS = 2, UNBIASED ESTIMATES 
C     IPRINT= 0, DO NOT WRITE RESULTS 
C     IPRINT= 1, PRINT ON SAME PAGE      IPRINT= 2, PRINT ON NEW PAGE 
C     DEVELOPED BY JOSE D SALAS AND GUILLERMO Q TABIOS III 
C     HYDROLOGY AND WATER RESOURCES PROGRAM, COLORADO STATE UNIVERSITY 
C     DATE ORIGINAL VERSION : FEB. 1987    REVISION : SEPT. 1990 
C     ****************************************************************** 
      DIMENSION             X(N),   W(1000) 
      IF (N.GT.1000) GO TO 220 
      L = 0
      SX1 = 0.0 
      SX2 = 0.0 
      SX3 = 0.0 
      SX4 = 0.0 
      SX5 = 0.0 
      XI = X(1) 
      W(1) = XI 
      IF (XI.EQ.XMISS) GO TO 100 
      L = 1 
      SX1 = XI 
      SX2 = XI * XI 
      SX3 = XI * XI * XI 
      SX4 = XI * XI * XI * XI 
  100 M = 0 
      SR1 = 0.0 
      SR2 = 0.0 
      SR3 = 0.0 
      SR4 = 0.0 
      SR5 = 0.0 
      DO 110 I = 2,N 
         XI = X(I) 
         W(I) = XI 
         IF (XI.EQ.XMISS) GO TO 110 
         L = L + 1 
         SX1 = SX1 + XI 
         SX2 = SX2 + XI * XI 
         SX3 = SX3 + XI * XI * XI 
         SX4 = SX4 + XI * XI * XI * XI 
         XI1 = X(I - 1) 
         IF (XI1.EQ.XMISS) GO TO 110 
         M = M + 1 
         SR1 = SR1 + XI 
         SR2 = SR2 + XI * XI 
         SR3 = SR3 + XI1 
         SR4 = SR4 + XI1 * XI1 
         SR5 = SR5 + XI * XI1 
  110 CONTINUE 
      IF(L.GT.0) GO TO 111
	EVX=XMISS
	SDX=XMISS
	CVX=XMISS
	CSX=XMISS
	CKX=XMISS
	RX1=XMISS
	EMX=XMISS
	RETURN
  111	AL = L 
	EVX = SX1/AL 
      SDX=0.
	IF(L.GT.1)	SDX = SQRT(SX2/AL - EVX * EVX) 
      CVX = SDX/EVX
	CSX=0.
	IF(SDX.GT.0.) CSX = (SX3/AL - 3.0 * EVX * SX2/AL + 2.0 * EVX * EVX
	1 * EVX)/(SDX * SDX * SDX) 
	IF(SDX.GT.0.) CKX = (SX4/AL - 4.0 * EVX * SX3/AL + 6.0 * EVX * EVX
     1* SX2/AL - 3.0 * EVX * EVX * EVX * EVX)/(SDX * SDX * SDX * SDX) 
      AM = M 
      AM1 = M + 1.0
	RX1=0.
	IF(SDX.GT.0) RX1 = (SR5 - SR3 * EVX - SR1 * EVX + AM * EVX * EVX)/ 
     1 (AM1 * SDX *SDX) 
      IF (IBIAS.EQ.1) GO TO 120
	AL1 = AL - 1.0
	IF (AL.GE.3.) THEN
	SDX = SDX * SQRT(AL/AL1) 
      CVX=0.
	IF (EVX.GT.0.) CVX = SDX/EVX
	ELSE
	SDX = -999.
	CVX = -999. 
      ENDIF
	IF (AL.GE.3.)THEN
	CSX = CSX * SQRT(AL * AL1)/(AL - 2.0) 
      ELSE
	CSX = -999.
	ENDIF
	IF (AL.GE.4.)THEN
	CKX = CKX * AL * AL1/((AL - 2.0) * (AL - 3.0))
      ELSE
	CKX = -999.
	ENDIF
C OVERRIDE UNBIASED ESTIMATE OF AUTOCORRELATION
C	IF (AM1.GE.5.)THEN
C	RX1 = (1.0 + AM1 * RX1)/(AM1 - 4.0) 
C	ELSE
C	RX1= -999.
C	ENDIF
	CALL SORT (N,W) 
      NN = 0 
      DO 200 I=1,N 
        IF (W(I).LT.0.0) NN = NN + 1 
        IF (W(I).GE.0.0) GO TO 210 
  200 CONTINUE 
  210 AN = N - NN 
      MAN = AN/2 
      FAN = MAN*2 
      IF (AN.EQ.FAN) THEN 
        EMX = (W(NN+MAN) + W(NN+MAN+1))/2 
      ELSE IF (AN.NE.FAN) THEN 
        EMX = W(NN+MAN+1) 
      END IF 
  120 IF (IPRINT.EQ.0) RETURN 
      IF (IPRINT.EQ.2) WRITE(IUNITW,130) 
      IF (IBIAS.EQ.1) WRITE(IUNITW,140) 
      IF (IBIAS.NE.1) WRITE(IUNITW,150) 
      WRITE(IUNITW,160) L,EVX,EMX,SDX,CVX,CSX,CKX,RX1 
      RETURN 
  220 WRITE(IUNITW,170) 
      RETURN 
  130 FORMAT (1H1) 
  140 FORMAT (/1X,'ESTIMATED SAMPLE STATISTICS (BIASED ESTIMATES)') 
  150 FORMAT (/1X,'ESTIMATED SAMPLE STATISTICS (UNBIASED ESTIMATES)') 
  160 FORMAT(/1X,'SAMPLE SIZE              = ',I11,//,1X,'SAMPLE MEAN 
     1           = ',F11.3,/,1X,'SAMPLE MEDIAN            = ',F11.3,/,1X 
     2,'STANDARD DEVIATION       = ',F11.3,/,1X,'COEFFICIENT OF VARIATIO 
     3N = ',F11.4,/,1X,'SKEWNESS COEFFICIENT     = ',F11.4,/,1X,'KURTOSI 
     4S COEFFICIENT     = ',F11.4,/,1X,'LAG-1 SERIAL CORR. COEFF.= ',F11 
     5.4) 
  170 FORMAT(//,'SAMPLE SIZE N > 1000 !',/,'USER NEEDS TO INCREASE', 
     1       ' THE DIMENSION OF W IN SUBROUTINE MOMEN3') 
      END
C
C *** SUBROUTINE SORT ADDED BY AYRES (1999) ***
C
      SUBROUTINE SORT (N,W)
      DIMENSION W(1000)
      DO 10 I=1,N
      WMIN=W(I)
      DO 20 J=I,N
      IF (WMIN.LT.W(J)) GO TO 20
      WMIN=W(J)
      NMIN=J
   20 CONTINUE
      W(NMIN)=W(I)
      W(I)=WMIN
   10 CONTINUE
      RETURN
      END
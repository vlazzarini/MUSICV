C     Music V, Max Mathews
C     
C     typed in from old XGP output
C     file last written 19Jun75 MUSIC5[M5,GM] (I believe GM = George McKee)
C     SAIL
C     
C     Bill Schottstaedt, 26-Apr-08
C     
C     This is the new version that runs (5-May-08) in gfortran and linux.
C     Most of my changes are in lower case

C     pass2 reads pass1.data and writes pass2.data.
      
C     V Lazzarini, Jan 22.
C     Bug fixed and CONVT call is now working
C     CON routine was thoroughly busted but it is now restored
C     needs more tests but score from Little Boy is running now 

C     PASS 2 MAIN PROGRAM
C     *** MUSIC V ***
C     VL Feb 22 - making this a function so it can be called externally
C     RN is expected to be the CONVT routine number      
      INTEGER FUNCTION PASS2(RN)
      INTEGER RN
      DIMENSION G(1000),I(1000),T(1000),D(10000),P(100),IP(10)
      COMMON IP,P,G,I,T,D,IXJQ,TLAST,BLAST,IROUT

      integer inputfile
      integer outputfile
      IROUT = RN
            
C     INITIALIZING PROGRAM
C     NOMINAL SAMPLING RATE, NOTE PARAMETER LENGTH, NUMBER OF CARDS
C     NO OF OP CODES, PASS 11 REPORT PRINT PARAMETER

C     NB V Lazzarini sept. 2009 G(4) seems to hold SR
C     which is set at pass1 to 44100.
C     This is required to be correct if we want to make
C     CONVT work 

      G(1)=0.
      G(2)=0.
      G(4)=44100.0
      NPAR=10000
      NCAR=1000
      NOPC=12
      IXJQ=0
      IEND=0
 

C     C*****  NREAD=2
C     C*****  NWRITE=3
c      NREAD=20
c      NWRITE=21
c      REWIND NREAD
c      REWIND NWRITE

      inputfile=1
      outputfile=2
      open(inputfile, FILE='pass1.data', STATUS='OLD')
      open(outputfile, FILE='pass2.data')

C     INITIALIZE SECTION
 150  ID=1
      IN=1
      TLAST=0.
      BLAST=0.
C     READ SECTION OF DATA
c 106  CALL READ2 (NREAD)
 106  CALL READ2 (inputfile)
      I1=IP(1)
      D(ID)=I1
      I(IN)=ID
      T(IN)=P(2)
      DO 100 I2=1,I1
         I3=ID+I2
c 100     D(13)=P(I2)
         D(I3)=P(I2)
 100     continue
      
      ID=ID+I1+1
c      IF(ID-NPAR)102,102,101
      IF((ID-NPAR).le.0) go to 102
 101  CALL ERROR(20)
C     VL FEB 22 do not stop, set error 
      PASS2 = 20
      GOTO 9999
C     STOP
 102  IN=IN+1

c      IF (IN-NCAR)103,103,101
      IF ((IN-NCAR).gt.0) go to 101
c 103  IF (P(1)-5.0)104,110,104
      IF ((P(1)-5.0).eq.0.0) go to 110
c 104  IF (P(1)-6.0)106,105,106
      IF ((P(1)-6.0).ne.0.0) go to 106
c 105  IEND=1
      IEND=1
      GO TO 110

C     SORT SECTION
C***  NOT USED ****** 110 CALL SORTFL
 110  IN=IN-1
      
c      CALL SORT(T(1),T(2),IN,I)
      CALL SORT(T(1),IN,I)

C     EXECUTE OP CODES M SECTION
c 120  DO 1 I4=1,IN
      DO 1 I4=1,IN
         I5=I(I4)
         I6=D(I5+1)
c         IF(I6)121,121,122
         IF(I6.gt.0) go to 122

 121     CALL ERROR(21)
         PASS2 = 21
         GO TO 1
c 122     IF (I6-NOPC)123,123,121
 122     IF ((I6-NOPC).gt.0) go to 121
        
c     123     GO TO (2,2,2,2,2,2,7,8,7,10,2,8),I6
          GO TO (2,2,2,2,2,2,7,8,7,10,2,8),I6
 7        CALL ERROR(22)
          PASS2 = 22
         GO TO 1
         
C     [page 3-2]

 8       I7=D(I5)
         I8=I5+4
         I9=I5+I7
         I10=IFIX(D(I5+3))-I8
         DO 124 I11=I8,I9
            I12=I10+I11
            G(I12)=D(I11)

            
C     VL:25/01/22 updated for gfortran 2018           
 124     CONTINUE
C     VL 7.02.2022 typo preventing integer setting for pass 3 fixed        
c     IF(I6-12)1,2,1
         IF((I6-12).eq.0) go to 2
         go to 1
 10      I13=D(I5+3)
         IP(2)=I5
c         IF(I13)125,125,126
         IF(I13.gt.0) go to 126
 125     CALL ERROR(23)
         PASS2 = 23
         GO TO 1
c 126     IF(I13-5)127,127,125
 126     IF((I13-5).gt.0) go to 125
c 127     GO TO (21,22,23,24,25),I13
          GO TO (21,22,23,24,25),I13
 21      CALL PLS1
         GO TO 1
 22      CALL PLS2
         GO TO 1
 23      CALL PLS3
         GO TO 1
 24      CALL PLS4
         GO TO 1
 25      CALL PLS5
         GO TO 1
C     WRITE OUT SECTION
 2       IP(1)=D(I5)
         I18=IP(1)
         DO 133 I19=1,I18
            I20=I19+I5
            P(I19)=D(I20)
C     VL:25/01/22 updated for gfortran 2018
 133     CONTINUE
c         CALL WRITE2 (NWRITE)
         CALL WRITE2 (outputfile)
 1       CONTINUE

C     END OF SECTION OR PASS
c 140  IF(IEND)141,141,143
         IF(IEND.gt.0) go to 143
c 141  PRINT 142
      PRINT 142
 142  FORMAT (' END OF SECTION PASS II')
      GO TO 150
 143  PRINT 144
 144  FORMAT (' END OF PASS II')
      PASS2 = 0
 9999 close(inputfile)
      close(outputfile)
  
C    VL feb 22 do not stop, just return      
C     STOP
      RETURN      
      END

C     READ2 PASS 2 DATA INPUT ROUTINE
C     *** MUSIC V ***
      SUBROUTINE READ2(N)
      DIMENSION IP(10),P(100)
      COMMON IP,P
c      READ(N) K,(P(J),J=1,K)
      READ(N, *) K,(P(J),J=1,K)
      IP(1)=K

      RETURN
      END

C     SORT SORTING PROGRAM
C     *** MUSIC V ***
c      SUBROUTINE SORT(A,B,N,L)
      SUBROUTINE SORT(A,N,L)
      DIMENSION A(N),L(N)
C     
C     SORT SORTS THE A ARRAY INTO ASCENDING NUMERICAL ORDER, PERFORMING
C     THE SAME OPERATIONS ON ARRAY L AS ON A
C     
      N1=N-1
      
C     [page 3-3]
      
      DO 10 I=1,N1
         IN=I+1
         DO 20 J=IN,N
            IF(A(I).LE.A(J))GO TO 20
            T=A(I)
            A(I)=A(J)
            A(J)=T
            NT=L(I)
            L(I)=L(J)
            L(J)=NT
 20      CONTINUE
 10   CONTINUE
      RETURN

C     C***********  ENTRY SORTFL
C     C***********  RETURN
      END

C     WRIT2 DATA OUTPUTING ROUTINE FOR PASS 2
C     *** MUSIC V ***
      SUBROUTINE WRITE2(N)
      COMMON IP(10),P(100),G(1000),I(1000),T(1000),D(10000),IXJQ,TLAST,
     *     BLAST, IROUT
      IF(G(2).EQ.0.)GO TO 150
      X=P(2)
      Y=P(4)
      ILOC=G(2)
      IF(P(1).NE.1.)GO TO 50
      P(4)=P(4)*60./CON(G,ILOC,P(2))
c      P(4)=P(4)*60./CON(G,P(2))
 50   P(2)=TLAST+(P(2)-BLAST)*60./CON(G,ILOC,P(2))
c 50   P(2)=TLAST+(P(2)-BLAST)*60./CON(G,P(2))
      TLAST=P(2)
      BLAST=X
 150  CALL CONVT
      K=IP(1)
      WRITE(N, *) K,(P(J),J=1,K)
C     *** PASS II REPORT IS OPTIONAL ***
      IF(G(1).NE.0.) RETURN
      IF(IXJQ.EQ.0) PRINT 100
      IXJQ=10
 100  FORMAT(15H1PASS II REPORT/11H0(WORD CNT))
      PRINT 101,K,(P(J),J=1,K)
      IF(G(2).NE.0.) PRINT 102,X,Y
 101  FORMAT(I8,10(F9.3))
 102  FORMAT(1H+,110X,2HB=,F7.4,2HD=,F7.4)
      RETURN
      END

C     VL 31/01/22 this was fully busted, now restored
C     CON2 PASS 2 FUNCTION INTERPOLATOR
C     *** MUSIC V ***
      FUNCTION CON(G,I,T)
c      FUNCTION CON(G,T)
      DIMENSION G(1000)
      DO 10 J=I,1000,2
C     VL 31/01/22 this solves the problem of P(2) beyond the
C     metro function end    
          IF(G(J) < G(J-2)) GOTO 21
C         PRINT *, G(J), J, G(J)-T
c         IF (G(J)-T) 10,20,30
         if ((G(J)-T).eq.0) go to 20
         if ((G(J)-T).lt.0) go to 10
 30      CON = G(J-1)+((T-G(J-2))/(G(J)-G(J-2)))*(G(J+1)-G(J-1))
c     CON = G(J-1)+((T-G(J-2))/(G(J)-G(J-2)))*(G(J+1)-G(J-1))
         RETURN
 10   CONTINUE
 20   CON = G(J+1)
      RETURN
 21   CON = G(J-1)
      RETURN
      END

C     CONVT FOR UNIT GENERATORS CHECK
C     
C     DUMMY NO OPERATION ACTUALLY PERFORMEDC
C******WHEN DUMMY IS REMOVED ANOTHER CONVT MUST!!!! BE LOADED!!!*****
      
C     [page 3-4]
      
C***  SUBROUTINE CONVT
C***  COMMON IP(10),P(100),G(1000)
C***  RETURN
C***  END

c added back in
      SUBROUTINE CONVT
      COMMON IP(10),P(100),G(1000),I(1000),T(1000),D(10000),IXJQ,TLAST,
     *     BLAST, IROUT
      GOTO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21),IROUT
      CALL CONVT0
      GOTO 9999
 1    CALL CONVT1
      GOTO 9999
 2    CALL CONVT2
      GOTO 9999
 3    CALL CONVT3
      GOTO 9999
 4    CALL CONVT4
      GOTO 9999
 5    CALL CONVT5
      GOTO 9999
 6    CALL CONVT6
      GOTO 9999
 7    CALL CONVT7
      GOTO 9999
 8    CALL CONVT8
      GOTO 9999
 9    CALL CONVT9
      GOTO 9999
 10   CALL CONVT10
      GOTO 9999
 11   CALL CONVT11
      GOTO 9999
 12   CALL CONVT12
      GOTO 9999
 13   CALL CONVT13
      GOTO 9999   
 14   CALL CONVT14
      GOTO 9999
 15   CALL CONVT15
      GOTO 9999
 16   CALL CONVT16
      GOTO 9999
 17   CALL CONVT17
      GOTO 9999
 18   CALL CONVT18
      GOTO 9999
 19   CALL CONVT19
      GOTO 9999
 20   CALL CONVT20
      GOTO 9999
 21   CALL CONVT21
      GOTO 9999 
 9999 RETURN
      END

C     GENERAL CONVT from Risset's Catalogue
C     Added by V Lazzarini, 2009
C     Bug in G() assignment fixed 25/01/22
      SUBROUTINE CONVT0
      COMMON IP(10),P(100),G(1000)
      IF (G(3).NE.0.0) RETURN
      IF (P(1).NE.1.0) RETURN
C     freq conversion factor tabsize/sr
      FREQ=511.0/G(4)
      I=P(3)
      NPAR=G(10*I)
      IF(NPAR.EQ.0) GOTO 1
      DO 2 J=1, NPAR
         M=10*I+J
         M=G(M)
         IF(M.GT.200) GOTO 30
         IF(M.GT.100) GOTO 30
         IF(M.LT.0) GOTO 20
         P(M)=FREQ*P(M)
         GOTO 2
 20      M=-M
         P(M)=FREQ/P(M)
         GOTO 2
 30      M=M-100
         P(M+1)=P(4)-P(M)-P(M+2)
C        IF (P(M+1)) 32,33,34
         IF (P(M+1) == 0) GOTO 33
         IF (P(M+1) > 0) GOTO 34
 32      P(M)=(P(M)*P(4))/(P(M)+P(M+2))
         P(M+2)=(P(M+2)*P(4))/(P(M)+P(M+2))
 33      P(M+1)=128
         GOTO 35
 34      P(M+1)=FREQ/(4.0*P(M+1))
 35      P(M+2)=FREQ/(4.0*P(M))
         GOTO 2
 40      M=M-200
         D=-(6.2832*P(M+1))/G(4)
         F=(6.2832*P(M))/G(4)
         P(M)=2.*EXP(D)*COS(F)
         P(M+1)=EXP(2.*D)
 2    CONTINUE
 1    CONTINUE
      RETURN
      END


C   CONVT POUR FANFARE TRIOMPHE
      SUBROUTINE CONVT1
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      IF(P(3).GE.7.)GOTO100
      F=511./G(4)
      P(6)=F*P(6)
 100  RETURN
      END
      
C   CONVT POUR FANFRE FEEDBACK
C   FOR FEEDBACK INSTS,P2 AND P4 INTEGERS IN SCORE
C   P2 AND P4 MULTIPLIED BY .1024 IN CONVT
      SUBROUTINE CONVT2
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      IF(P(1).GE.6.5)GOTO100
      F=511./G(4)
      P(6)=F*P(6)
      P(2)=.1024*P(2)
      P(4)=.1024*P(4)
 100  RETURN
      END

C    VOLCONVT CONVT POUR VOL LB
      SUBROUTINE CONVT3
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      P(6)=F*P(6)
      IF(P(3).EQ.1.)GOTO100
      P(7)=F/P(4)
      P(8)=(P(6)*P(8))/100.
      P(9)=F*P(9)
 100  RETURN
      END      

C   CONVT POUR FANFARE TRIOMPHE 
C   RVN CONVT for W20_003_2 P16 P36
      SUBROUTINE CONVT4
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      FE=F/4.
      P(6)=F*P(6)
      P(8)=P(4)-P(7)-P(9)
C      IF(P(8))2,3,4
      IF(P(8) > 0) GOTO 4
      IF(P(8) == 0) GOTO 3      
 2    P(7)=(P(7)*P(4))/(P(7)+P(9))
      P(9)=(P(9)*P(4))/(P(9)+P(7))
 3    P(8)=128.
      GOTO5
 4    P(8)=FE/P(8)
 5    P(7)=FE/P(7)
      P(9)=FE/P(9)
 100  RETURN
      END      
      
C   CONVT POUR FANFARE TRIOMPHE
C   RVN CONVT for W20_003_2 P33
      SUBROUTINE CONVT5
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      IF(P(3).GE.7.)GOTO100
      F=511./G(4)
      FE=F/4.
      P(6)=F*P(6)
      P(8)=P(4)-P(7)-P(9)
C      IF(P(8))2,3,4
      IF(P(8) > 0) GOTO 4
      IF(P(8) == 0) GOTO 3
 2    P(7)=(P(7)*P(4))/(P(7)+P(9))
      P(9)=(P(9)*P(4))/(P(9)+P(7))
 3    P(8)=128.
      GOTO5
 4    P(8)=FE/P(8)
 5    P(7)=FE/P(7)
      P(9)=FE/P(9)
 100  RETURN
      END      

C   CONVT POUR FANFARE FEEDBACK
C...FOR FEEDBACK INSTS, P2 AND P4 INTEGERS IN SCORE
C   P2 AND P4 MULTIPLIED BY .1024 IN CONVT
C
C   RVN CONVT for W20_003_2 P18 P23 P28
      SUBROUTINE CONVT6
      COMMON IP(10),P(100),G(1000)
      IF((P(1).EQ.5.).OR.(P(1).EQ.6.))GOTO50
      IF(P(1).NE.1.)GOTO100
      IF(P(3).GE.6.5)GOTO40
      F=511./G(4)
      FE=F/4.
      P(6)=F*P(6)
      P(8)=P(4)-P(7)-P(9)
C      IF(P(8))2,3,4
      IF(P(8) > 0) GOTO 4
      IF(P(8) == 0) GOTO 3      
 2    P(7)=(P(7)*P(4))/(P(7)+P(9))
      P(9)=(P(9)*P(4))/(P(9)+P(7))
 3    P(8)=128.
      GOTO5
 4    P(8)=FE/P(8)
 5    P(7)=FE/P(7)
      P(9)=FE/P(9)
 40   P(4)=.1024*P(4)
 50   P(2)=.1024*P(2)
 100  RETURN
      END

C   CONVT POUR FANFARE FEEDBACK
C...FOR FEEDBACK INSTS,P2 AND P4 INTEGERS IN SCORE
C   P2 AND P4 MULTIPLIED BY .1024 IN CONVT
C
C   RVN CONVT for W20_003_2 P26 P30
      SUBROUTINE CONVT7
      COMMON IP(10),P(100),G(1000)
      IF(G(10).GE..5)GOTO100
      IF((P(1).EQ.5.).OR.(P(1).EQ.6.))GOTO50
      IF(P(1).NE.1.)GOTO100
      IF(P(3).GE.6.5)GOTO40
      F=511./G(4)
      FE=F/4.
      P(2)=.1024*P(2)
      P(4)=.1024*(P(4)+16.)
      P(6)=F*P(6)
      P(8)=P(4)-P(7)-P(9)
C      IF(P(8))2,3,4
      IF(P(8) > 0) GOTO 4
      IF(P(8) == 0) GOTO 3      
 2    P(7)=(P(7)*P(4))/(P(7)+P(9))
      P(9)=(P(9)*P(4))/(P(9)+P(7))
 3    P(8)=128.
      GOTO5
 4    P(8)=FE/P(8)
 5    P(7)=FE/P(7)
      P(9)=FE/P(9)
      GOTO100
 40   P(4)=.1024*(P(4)+16.)
 50   P(2)=.1024*(P(2)+16.)
 100  RETURN
      END

C   RVN USED IN W20_003_2_P103 
C   CONVT FUSEES POUR LA CHUTE
C

      SUBROUTINE CONVT8
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      P(6)=F*P(6)
      P(8)=F/P(8)
      IF(P(3).EQ.2.)GOTO50
      P(7)=F/P(7)
      GOTO100
 50   P(7)=P(7)*P(6)*.01
      P(9)=F*P(9)
 100  RETURN
      END

C    RVN USED IN W20_003_2_P134
C    CIG CONVT FOR INTERPOL GLISS
C
      SUBROUTINE CONVT9
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      P(7)=F*P(7)
      P(8)=F*P(8)
      P(9)=F/P(9)
 100  RETURN
      END

C    RVN USED IN W20_003_2_P124
C    
      SUBROUTINE CONVT10
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      P(6)=F*P(6)
      P(7)=F/P(4)
 100  RETURN
      END

C    RVN USED IN W20_003_2_P113
C    CONVT POUR FANFARE TRIOMPHE
C
      SUBROUTINE CONVT11
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      IF(P(3).GE.7.)GOTO100
      F=511./G(4)
      IF(G(10).GE..5)P(7)=F*P(7)
      P(6)=F*P(6)
 100  RETURN
      END 

C    RVN CONVT USED IN W20_003_2P58
C
      SUBROUTINE CONVT12
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      P(6)=F*P(6)
      IF(P(3).EQ.1.)GOTO100
      P(7)=F/P(4)
      IF(P(3).EQ.3.)GOTO100
      P(8)=.01*P(5)
      P(9)=F*P(9)
 100  RETURN
      END 
      
C     RVN USED IN W20_003_2_P61
C    
      SUBROUTINE CONVT13
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      P(6)=F*P(6)
      IF(G(10).GE..5)GOTO50
      IF(P(3).EQ.1.)GOTO100
      P(7)=F/P(4)
      IF(P(3).EQ.3.)GOTO100
      P(8)=.01*P(5)
      P(9)=F*P(9)
      GOTO100
 50   P(7)=F/P(4)
 100  RETURN
      END

C     RVN USED IN W20_003_2_P214
C
      SUBROUTINE CONVT14
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      P(6)=F*P(6)
      IF(P(3).EQ.1.)GOTO10
      IF(P(3).EQ.7.)GOTO70
      P(7)=F/P(4)
      GOTO100
 10   P(5)=.01*P(5)*P(6)
      P(7)=F*P(7)
      GOTO100
 70   FENV=F*.25
      P(9)=P(4)-P(8)-P(10)
C     IF(P(9))2,3,4
      IF(P(9) > 0) GOTO 4
      IF(P(9) == 0) GOTO 3
 2    P(8)=(P(8)*P(4))/(P(8)+P(10))
      P(10)=(P(10)*P(4))/(P(8)+P(10))
 3    P(9)=128.
      GOTO5
 4    P(9)=FENV/P(9)
 5    P(8)=FENV/P(8)
      P(10)=FENV/P(10)
 100  RETURN
      END

C    RVN USED IN W20_003_2 P85
C 
      SUBROUTINE CONVT15
      COMMON IP(10),P(100),G(1000)
      IF(P(1).EQ.5.)P(2)=.1024*P(2)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      IF(P(3).GE.8.)GOTO50
      P(2)=.1024*P(2)
      P(4)=.1024*P(4)
      IF(P(3).GE.5.)GOTO100
      P(7)=F/P(4)
      IF(P(3).EQ.4.)P(8)=F*P(8)
      IP(1)=8
 50   P(6)=F*P(6)
 100  RETURN
      END

C     RVN USED IN W20_003_2 P87 
C
      SUBROUTINE CONVT16
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      P(6)=F*P(6)
      P(7)=F/P(4)
      IF(P(3).EQ.1.)GOTO100
      P(6)=P(6)*G(11)
 100  RETURN
      END


C     RVN USED IN W20_003_2 P152
C
      SUBROUTINE CONVT17
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      P(6)=F*P(6)
      IF(P(3).EQ.1.)GOTO100
      P(7)=F/P(4)
 100  RETURN
      END

C     RVN USED IN W_20_003 P210
C     RVN annotation by JCR *bad error in convt* for section 
C     *inharmonicity during attack only* 
C
      SUBROUTINE CONVT18
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      P(6)=F*P(6)
C     RVN next 3 lines JCR marked with *!*
      IF(P(3).EQ.1.)GOTO10
      IF(P(3).EQ.7.)GOTO70
      IF(G(10).GE..5)GOTO200
      P(7)=F/P(4)
      GOTO100
 10   P(5)=.01*P(5)*P(6)
      P(7)= F*P(7)
      GOTO100
 70   FENV=F*.25
      P(8)=.001*P(8)
      P(9)=.001*P(9)
      P(10)=.001*P(10)
      P(9)=P(4)-P(8)-P(10)
C    IF(P(9))2,3,4
      IF(P(9) > 0) GOTO 4
      IF(P(9) == 0) GOTO 3
 2    P(8)=(P(8)*P(4))/(P(8)+P(10))
      P(10)=(P(10)*P(4))/(P(8)+P(10))
 3    P(9)=128.
      GOTO5
 4    P(9)=FENV/P(9)
 5    P(8)=FENV/P(8)
      P(10)=FENV/P(10)
      GOTO100
 200  P(6)=P(6)*P(3)*10
      P(7)=F/P(4)
      FENV=F*.25
      P(8)=.001*P(8)
      P(9)=.001*P(9)
      P(10)=.001*P(10)
      P(9)=P(4)-P(8)-P(10)
C     IF(P(9))202,203,204
C     RVN line above not entirely legible, but most probably ok as here        
      IF(P(9) > 0) GOTO 204
      IF(P(9) == 0) GOTO 203
 202  P(8)=(P(8)*P(4))/(P(8)+P(10))
      P(10)=(P(10)*P(4))/(P(8)+P(10))
 203  P(9)=128.
      GOTO205
 204  P(9)=FENV/P(9)
 205  P(8)=FENV/P(8)
      P(10)=FENV/P(10)
 100  RETURN
      END


C     RVN used in W20_003_2 P219 
C     with JCR annotation *mauvaise CONVT* 
C 
      SUBROUTINE CONVT19
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      P(6)=F*P(6)
      IF(P(3).EQ.1.)GOTO10
      IF(P(3).GE.7.)GOTO70
      P(7)=F/P(4)
      GOTO100
 70   FENV=F*.25
      P(9)=P(4)-P(8)-P(10)
C      IF(P(9))2,3,4
      IF(P(9) > 0) GOTO 4
      IF(P(9) == 0) GOTO 3
 2    P(8)=(P(8)*P(4))/(P(8)+P(10))
      P(10)=(P(10)*P(4))/(P(8)+P(10))
 3    P(9)=128.
      GOTO5
 4    P(9)=FENV/P(9)
 5    P(8)=FENV/P(8)
      P(10)=FENV/P(10)
      GOTO100
 10   P(5)=.01*P(5)*P(6)
      P(7)=F*P(7)
      GOTO100
 100  RETURN
      END

C     RVN CONVT for Mathews, 1969, p.54
C
      SUBROUTINE CONVT20
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      P(6)=F*P(6)
      P(7)=F*P(7)
      P(8)=F*P(8)
      P(9)=F*P(9)
 100  RETURN
      END      


C     RVN CONVT for W20_003_2 P36 but the same as convt7
C
      SUBROUTINE CONVT21
      COMMON IP(10),P(100),G(1000)
      IF(P(1).NE.1.)GOTO100
      F=511./G(4)
      FE=F/4.
      P(6)=F*P(6)
      P(8)=P(4)-P(7)-P(9)
C     IF(P(8))2,3,4
      IF(P(8) > 0) GOTO 4
      IF(P(8) == 0) GOTO 3      
 2    P(7)=(P(7)*P(4))/(P(7)+P(9))
      P(9)=(P(9)*P(4))/(P(9)+P(7))
 3    P(8)=128.
      GOTO5
 4    P(8)=FE/P(8)
 5    P(7)=FE/P(7)
      P(9)=FE/P(9)
 100  RETURN
      END


C     ERRO1 GENERAL ERROR ROUTINE
C     *** MUSIC V ***
      SUBROUTINE ERROR2(I)
      PRINT 100,I
 100  FORMAT(' ERROR OF TYPE',I5)
      RETURN
      END
C     C***** SUBROUTINE PLS
C     C***** ENTRY PLS1
C     C***** ENTRY PLS2
C     C***** ENTRY PLS3
C     C***** ENTRY PLS4
C     C***** ENTRY PLS5
      SUBROUTINE PLS1
      RETURN
      END
      SUBROUTINE PLS2
      RETURN
      END
      SUBROUTINE PLS3
      RETURN
      END
      SUBROUTINE PLS4
      RETURN
      END
      SUBROUTINE PLS5
      RETURN
      END
C     [page 4-1]
      



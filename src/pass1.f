C     Music V, Max Mathews
C     
C     typed in from old XGP output
C     file last written 19Jun75 MUSIC5[M5,GM] (I believe GM = George McKee)
C     SAIL
C     
C     Bill Schottstaedt, 26-Apr-08
C     
C     This is the new version that runs (5-May-08) in gfortran and linux.
C     Most of my changes are in lower case.
      
C     pass1 required much more serious surgery than the other 2 passes.  I had to
C     embed the READ1 subroutine in the main program.  Apparently new Fortrans 
C     do not guarantee that subroutine locals will be saved across calls.  The 
C     other choice was an enormous COMMON block, but that became unwieldy and error-prone.
C     
C     In all passes, I replaced the arithmetic IFs, changed file IO, used characters
C     rather than ints, replaced MOVR with ichar(ch)-ichar('0'), and removed the
C     EQUIVALENCE stuff (not needed here because I'm using characters directly).

C     pass1 takes a file named 'score' and produces pass1.data

C     V Lazzarini, 04-Sept-09
C     Added IOS opcode (interpolating oscillator)
C     which I am supposing it is what Risset means in his 
C     catalogue

C     V Lazzarini, 29-01-22
C     Added PLF3 from Risset's Catalog #500
C     Since this included a call to READ1, it could not be dropped in
C     as a subroutine. I have instead incorporated in the main program
C     looping round to the reading code and back to the PLF section      
C     A bit more spaghetti, but could not be done in any other way without
C     restoring READ1
C     NB: if a PFL does not call READ1 it can be added as a subroutine.      
      
C     [page 1-1] -- these are the original XGP pages to help me find my place
C     
C     PASS1 PASS 1 MAIN PROGRAM
C     PASS1   *** MUSIC V ***   THIS VERSION RUNS ON THE PDP10, JULY 14,1971
C     VL Feb 22 - making this a function so it can be called externally from a driver program      
      INTEGER FUNCTION PASS1()
      COMMON P(100),IP(10),D(2000)
      integer ipdp
      integer pflflag

      CHARACTER*32 FLNM

      CHARACTER*1 IBCD(300)
      CHARACTER*1 CARD(129)
      DATA NOPS,NBC,NC/27,3,72/
      CHARACTER IDEC, ISTAR, IGAD
      DATA IDEC,ISTAR/'.','*'/
      CHARACTER JSEMI, JBLANK
      CHARACTER*1 IBC(4)
      DATA IBC/';',' ',',','-'/
      CHARACTER*1 IVT (4)
      CHARACTER*1 LOP (81)
      integer IDEF, I100

      DATA IVT/'P','F','B','V'/
      DATA LOP/'N','O','T','I','N','S','G','E','N','S','V','3',
     *     'S','E','C','T','E','R','S','V','1','S','V','2','P','L','F',
     *     'P','L','S','S','I','3','S','I','A','C','O','M','E','N','D',
     *     'O','U','T','O','S','C','A','D','2','R','A','N','E','N','V',
     *     'S','T','R','A','D','3','A','D','4','M','L','T','F','L','T',
     *     'R','A','H','S','E','T','I','O','S'/

      EQUIVALENCE (JSEMI,IBC(1)), (JBLANK,IBC(2))

      integer inputfile
      integer outputfile

      inputfile=1
      outputfile=2
      ipdp=0
      idef=0
      i100=1
      pflflag = 0

C     99   FORMAT(' TYPE FILE NAME')
C     999  FORMAT(A5)
C     PRINT 99
C     READ 999,FLNM

C     open(inputfile, FILE=FLNM, STATUS='OLD')
      open(inputfile, FILE='score', STATUS='OLD')
      open(outputfile, FILE='pass1.data')
C     CALL IFILE(1,FLNM)


C*****ABOVE 5 LINES FOR PDP10 ********
      
C     INITIALIZATION
C     NOMINAL SAMPLING RATE.
      D(4) = 44100.0
C     ERROR FLAG
      IP(2)=0
      P(2)=0.0
C     C   NWRITE = 2
c     NWRITE=20

C**** PDP DSK0=DEVICE 20 ******
C     C    REWIND NWRITE
C     C    CALL READ0

c     CALL READ1
      GO TO 4321
C********PDP ********
C     MAIN LOOP

c     100  CALL READ1

      
 100  GO TO 4321
 4322 I1=P(1)
      
      IF (I1.GE.1.AND.I1.LE.12) GO TO 103
      IP(2)=1
C     C    WRITE (6,200)

      PRINT 200
C********PDP ********
 200  FORMAT(' NON-EXISTENT OPCODE ON DATA STATEMENT')
      GO TO 100

c     103  GO TO (1,1,1,1,5,6,7,1,9,1,1,12),I1
c     1    CALL WRITE1 (NWRITE)
c     1    call write1(outputfile)
C     VL 29/01/22 we need to go back to PFL if we came out
C     of it to read data cards.      
C     Thi is PFL goto after reading       
 103  go to (9919,9929,9939,9949,9959),pflflag
      go to (1010,1010,1010,1010,5,6,7,1010,9,1010,1010,12),I1
 1010 call write1(outputfile)
      GO TO 100
 5    PRINT 110
C     C 5 WRITE (6, 110)
C********PDP ********
 110  FORMAT(' END OF SECTION IN PASS I')
c     GO TO 1
      go to 1010
c     6    CALL WRITE1 (NWRITE)
 6    call write1 (outputfile)

C     C	  WRITE (6, 111)
      PRINT 111
C********PDP ********	  
 111  FORMAT (' END OF PASS I')
      IF(IP(2).EQ.1) GOTO 5555
      PASS1 = 0 
      GOTO 5556
      
 5555 CALL HARVEY
      PASS1 = 1

 5556 close(inputfile)
      close(outputfile)
C     VL Feb 22 do not exit as this is not a main program      
C     CALL EXIT
      RETURN     
C     SET VARIABLES IN PASS 1
 7    I2=P(3)
      I3=I2+IP(1)-4
      DO 104 I4=I2,I3
         D(14)=P(14-I2+4)
 104  CONTINUE   
      GO TO 100
 9    I6=P(3)
      IF (I6.GE.1.AND.I6.LE.5) GO TO 107
      IP(2)=1
C     C    WRITE (6,201)
      
C     [page 1-2]

      PRINT 201
C********PDP ********
 201  FORMAT(' NON-EXISTENT PLF SUBROUTINE CALLED')
      GO TO 100
c     12   CALL WRITE1 (NWRITE)
 12   call write1(outputfile)
      GO TO 7

      
C     107  GO TO 100
c     added^
C     107   GO TO (21,22,23,24,25),I6
C     21      CALL PLF1
C     GO TO 100
C     22      CALL PLF2
C     GO TO 100
C     23      CALL PLF3
C     GO TO 100      
C     24      CALL PLF4      
C     GO TO 100
C     25      CALL PLF5
C     GO TO 100
c     END
      
C     VL: 29/01/22 set the PFL flag to PFL number to signal
C     that we are entering PFL section of the main program      
 107  pflflag = I6
C     VL: 28/01/22 restored PLF calls, labels prefixed with 99      
      GO TO (21,9922,9923,9924,9925),I6
C     VL: 28/01/2022 PLF routine from Risset's catalogue
C     BECAUSE READ1 is no more, this is embeded in the main program
 9921 CONTINUE
 9919 pflflag = 0
      GO TO 100
 9922 CONTINUE
 9929 pflflag = 0
      GO TO 100
C     PLF3 from JCR's Catalog #500      
C     SUBROUTINE PLF3
C     COMMON P(100),IP(10),D(2000)
C     VL: 29/01/22 variable names changed to avoid clashes
C     labels were also prefixed with 993  (993n)    
 9923 NNC=P(4)
      NN=P(5)
      TTS=P(6)
      TFACT=P(7)
      TDD=P(8)
      NI = 1
C     OUTER DO loop needs to be eliminated because of 9930      
C      DO 9931 NI=1, NNC
C      CALL READ1
C     There's no READ1 so we need to loop to the reading code         
 9930 GOTO 4321
C        Then loop back here from above 
 9939 CALL WRITE1(outputfile)
         TF=P(6)
         DO 9932 NJ=1,NN
            P(6)=FLOAT(NJ+1)*TF
            P(2)=P(2)+TTS
            AINST=P(3)-1
            IF(AINST == 0) GOTO 9934  
 9933       P(3)=2.
            GOTO 9935
 9934       P(2)=1.
 9935       CONTINUE
            IF(FACT.GT.0) P(5)=P(5)*TFACT 
            CALL WRITE1(outputfile)
 9932      CONTINUE
           NI = NI + 1
           IF(N1 > NNC) GOTO 9931
           GOTO 9930
 9931 CONTINUE
C     100  RETURN
C     END
C     reset PFL flag to signal end of PFL      
      pflflag = 0
      GOTO 100
 9924 CONTINUE
 9949 pflflag = 0
      GOTO 100      
 9925 CONTINUE
 9959 pflflag = 0
      GOTO 100     


C     [page 2-1]

C     READ1 INTERPRETATIVE READING ROUTINE
C**** MUSIC V ****
c     SUBROUTINE READ1
c     COMMON P(100),IP(10),D(2000),IPDP,inputfile,outputfile,END,SNA8
c     COMMON I,IDEF
C*****PDP ***** IPDP WAS ADDED TO COMMON LIST IN PLACE OF ENTRY FEATURE
c     CHARACTER*1 IBCD(300)
c     CHARACTER*1 CARD(129)
c     DIMENSION CARD(129),IBCD(300),LOP(3,26)
c     CHARACTER*1 ICAR
c     was 30
c     DIMENSION BCD(300)
c     DIMENSION IBC(4),IVT(4)
c     was 12
c     EQUIVALENCE(CARD,ICAR)
c     EQUIVALENCE(BCD,IBCD)
c     DATA NOPS,NBC,NC/26,3,72/
c     CHARACTER IDEC, ISTAR, IGAD
c     DATA IDEC,ISTAR/'.','*'/
CCC   DATA IBC(1),IBC(2),IBC(3),IBC(4)/'=',' ',',','-'/
c     DATA IBC(1),IBC(2),IBC(3),IBC(4)/';',' ',',','-'/
c     CHARACTER JSEMI, JBLANK
c     CHARACTER*1 IBC(4)
c     DATA IBC/';',' ',',','-'/
C********NO!!!!! THE CHARACTER = HAS BEEN SUBSTITUTED FOR
C     THE SEMICOLON AS THE END OF STATEMENT DELIMITER
c     CHARACTER*1 IVT (4)
c     CHARACTER*1 LOP (78)
c     
c     integer IDEF
c     
c     DATA IVT/'P','F','B','V'/
c     DATA LOP/'N','O','T','I','N','S','G','E','N','S','V','3',
c     * 'S','E','C','T','E','R','S','V','1','S','V','2','P','L','F',
c     * 'P','L','S','S','I','3','S','I','A','C','O','M','E','N','D',
c     * 'O','U','T','O','S','C','A','D','2','R','A','N','E','N','V',
c     * 'S','T','R','A','D','3','A','D','4','M','L','T','F','L','T',
c     * 'R','A','H','S','E','T'/
c     ,0,0,0,0,0,0,0,0,0,0,0,0/
C********LAST 12 LOCATIONS NOT YET USED. **** PDP ********
c     EQUIVALENCE (JSEMI,IBC(1)), (JBLANK,IBC(2))

C     TO SCAN INPUT DATA TO #, ORGANIZE FIELDS AND PRINT

 4321 CONTINUE
      IF(IPDP.EQ.0) GO TO 9999

C********PDP ********
      IF ((END+SNA8-1.).gt.0.0) go to 90
      
 10   IBK=2
      END=0.
      ERR=0.
      NUMU=0
      ISEMI=1
      L=3
      J=0
 11   I=I+1

      IF(I.GT.NC) GO TO 15
      IF(J.EQ.299) GO TO 21
      DO 13 N=1,NBC
         IF(CARD(I).NE.IBC(N)) GO TO 13
         GO TO (20,16,18),N
C     ; BLA ,
 13   CONTINUE
      J=J+1
      IBCD(J)=CARD(I)

      IBK=1
      GO TO 11
 14   IBK=N
      GO TO 11

C     C   15 READ (5,1,ERR=95,END=95) (CARD(I),I=1,NC)
C********PDP ********
c     15   READ (1,1,ERR=95,END=95) I, (CARD(I),I=1,NC)
 15   READ (inputfile,1, ERR=95,END=95) I, (CARD(I),I=1,NC)
C*****PDP ***** FIRST 'I' IS FOR PDP LINE NUMBERS!
c     1    FORMAT(I,128A1)

 1    FORMAT(128A1)
      PRINT 2121,(CARD(I),I=1,NC)
 2121 FORMAT(1H 128A1)
      I=0
      
C     [page 2-2]

      GO TO 11
 16   GO TO (17,11,11),IBK
 17   IBK=N
      J=J+1
      IBCD(J)=JBLANK
      GO TO (11,21),ISEMI
 18   GO TO (17,14,19),IBK
 19   J=J+1
      IBCD(J)=CHAR(0)
      GO TO 17
 20   ISEMI=2
      GO TO (17,21,19),IBK
 21   J=J+1
      IBCD(J)=JSEMI
C     TO SCAN FOR OP CODE

      DO 24 N=1,NOPS
         M=N
         DO 23 K=1,3
            IX2=ICHAR(IBCD(K))
            IF (IBCD(K).NE.LOP(K + ((N-1)*3))) GO TO 24
 23      CONTINUE
         GO TO 26
 24   CONTINUE
      GO TO 40
 26   NP=1
 27   L=L+1
      IF (IBCD(L).NE.JBLANK) GO TO 27

      
 29   GO TO (9100,9200,300,400,500,600,700,800,900,1000,1100,1200,1300,
     *     217,9201,202,203,204,205,206,207,208,209,210,211,212,213),M

C     OP CODE 1 TO PLAY NOTE
 9100 P(1)=1.
      GO TO 30

C     OP CODE 2 TO DEFINE INSTRUMENT
 9200 P(1)=2.
      IDEF=1
      N1=1
      GO TO 70
 2000 P(2)=XN
      N1=2
      GO TO 70
 2001 P(3)=XN
      IP(1)=3
      GO TO 50
C     OUT BOX
 9201 P(3)=101.
      NPW=2
c     IF (STER) 220,220,2011
      if (STER.LE.0) go to 220
      SNA8=1.
      STER=0
      GO TO 220
C     OSCILLATOR
 202  P(3)=102.
      NPW=5
      GO TO 220
C     ADD 2
 203  P(3)=103.
      NPW=3
      GO TO 220
C     RANDOM AND INTERPOLATE
 204  P(3)=104.
      NPW=6
      
C     [page 2-3]

      GO TO 220
C     LINEAR ENVELOPE GENERATOR
 205  P(3)=105.
      NPW=7
      GO TO 220
C     STEREO OUT BOX
 206  P(3)=106.
      NPW=3
c     IF(STER)220,2061,220
      if (STER.NE.0) go to 220
c     2061 SNA8=1.
      SNA8=1.
      STER=1.
      GO TO 220
C     THREE INPUT ADDER
 207  P(3)=107.
      NPW=4
      GO TO 220
C     FOUR INPUT ADDER
 208  P(3)=108.
      NPW=5
      GO TO 220
C     MULTIPLIER
 209  P(3)=109.
      NPW=3
      GO TO 220
C     FILTER
 210  P(3)=112.
      NPW=4
      GO TO 220
C     RANDOM AND HOLD
 211  P(3)=111.
      NPW=5
      GO TO 220
C     IOS V Lazzarini, 2009
 213  P(3)=113.
      NPW=5
      GO TO 220
C     SET NEW FUNCTION
 212  P(3)=110.
      NPW=1
      GO TO 220
C     END OF INSTRUMENT
 217  IP(1)=2
      IDEF=0
      END=1.
      GO TO 50
C     UNNAMED UNIT - NUMERICAL NAME ASSUMED
 218  N1=8
      NUMU=1
      L=0
      GO TO 70
 219  M=XN+14.
      IF(XN.LT.11.)GO TO 29
      P(3)=XN
C     TO INTERPRET VARS IN UNIT DEFS
 220  NP=3
 221  IF(IBCD(L+1).EQ.JSEMI) GO TO 240
c     222  NP=NP+1
      NP=NP+1
      L=L+1
      DO 223 N=1,4
         IF(IBCD(L).EQ.IVT(N)) GO TO 225
 223  CONTINUE
 224  L=L+1
      IF(IBCD(L).EQ.JBLANK)GO TO 46
      GO TO 224
      
C     [page 2-4]

 225  GO TO (231,232,233,234),N
C     P TYPE
 231  N1=3
      GO TO 70
 2311 P(NP)=XN
      GO TO 221
C     F TYPE
 232  N1=4
      GO TO 70
 2321 P(NP)=-(XN+100.)
      GO TO 221
C     B TYPE
 233  N1=5
      GO TO 70
 2331 P(NP)=-XN
      GO TO 221
C     V TYPE
 234  N1=6
      GO TO 70
 2341 P(NP)=XN+100.
      GO TO 221
 240  IF(NUMU.EQ.1)GO TO 242
c     241  IF(NPW+3-NP)42,242,42
      if (NPW+3-NP.NE.0) go to 42
 242  IP(1)=NP
      GO TO 50
C     OP CODE 3 - TO GENERATE FUNCTION
 300  P(1)=3.
      GO TO 30
C     OP CODE 4 -- TO SET PARAM 3RD PASS
 400  P(1)=4.
      GO TO 30
C     OP CODE 5 TO END SEC
 500  P(1)=5.
      GO TO 30
C     OP CODE 6 TO TERMINATE PIECE
 600  P(1)=6.
      GO TO 30
C     OP CODE 7 TO SET PARAM 1ST PASS
 700  P(1)=7.
      GO TO 30
C     OP CODE 8 TO SET PARAM 2ND PASS
 800  P(1)=8.
      GO TO 30
C     OP CODE 9 TO EXECUTE SUB 1ST PASS
 900  P(1)=9.
      GO TO 30
C     OP CODE 10 TO EXECUTE SUB 2ND PASS
 1000 P(1)=10.
      GO TO 30
C     OP CODE 11 TO SET INTEGER 3RD PASS
 1100 P(1)=11.
      GO TO 30
C     OP CODE 12 TO SET INTEGER ALL PASSES
 1200 P(1)=12.
      GO TO 30
C     OP CODE 13 FOR COMMENTS
 1300 IF(IBCD(L).NE.JSEMI) GO TO 1301
      L=L+1
      go to 4321
c     ? COM causes an infinite loop in the original code
 1301 L=L+1
      GO TO 1300
C     TO STORE PFIELDS
      
C     [page 2-5]

c     30   IF(IDEF)32,32,43
 30   continue
      if (IDEF.gt.0) go to 43
 32   IF(IBCD(L+1).EQ.JSEMI) GO TO 34
      NP=NP+1
      N1=7
      GO TO 70
 331  P(NP)=XN
      GO TO 32
 34   IP(1)=NP
c     IF(NP-1)47,47,50
      if (NP-1.gt.0) go to 50
      
      go to 47

C     ERRORS
c     40   IF(IDEF)41,41,218
 40   if (IDEF.gt.0) go to 218
 41   L=L+1
      IF(IBCD(L).NE.JSEMI)GO TO 41
      PRINT 1030
 1030 FORMAT(26H    OP CODE NOT UNDERSTOOD)
      GO TO 49
 42   PRINT 1040
 1040 FORMAT(44H    UNIT CONTAINS WRONG NUMBER OF PARAMETERS)
      GO TO 49
 43   PRINT 1050
 1050 FORMAT(36H    INSTRUMENT DEFINITION INCOMPLETE)
      ERR=1.
      IDEF=0
      GO TO 32
 44   PRINT 1060
 1060 FORMAT(25H    ERROR IN NUMERIC DATA)
      ERR=1.
      IF(NUMU.EQ.1)GO TO 45
      GO TO 30
 45   PRINT 1070
 1070 FORMAT(46H                          FOR UNIT DESIGNATION)
      P(3)=0.
      GO TO 220
 46   PRINT 1080
 1080 FORMAT(40H    IMPROPER VARIABLE IN UNIT DEFINITION)
      ERR=1.
      GO TO 221
 47   PRINT 1090
 1090 FORMAT(24H    STATEMENT INCOMPLETE)
 49   IP(2)=1
      GO TO 10
 50   IF(ERR.EQ.1.) GO TO 49
      goto 4322
c     RETURN

C     CONVERSION OF NUMERIC FIELD TO FLOATING POINT
 70   SGN=1.

      IF (IBCD(L+1).NE.IBC(4)) GO TO 79
      SGN=-1.
      L=L+1
 79   L1=L+1
      LD=L1
      XN=0.
 71   L=L+1
C     *** I DON'T UNDERSTAND THIS PART OF THE SCANNER!
C     C          IF(IBCD(L).EQ.JBLANK) GO TO 77
      IF (IBCD(L).EQ.JBLANK) GO TO 77
C     THIS LOOKS FOR #S, LETTERS, BLANKS, DECI.PTS, & *S. OTHERWISE=ERROR!?
C     ******** PDP ********
      IF(IBCD(L).LT.CHAR(10))GO TO 71
      IF(IBCD(L).EQ.IDEC) GO TO 71
      IF(IBCD(L).EQ.ISTAR) GO TO 71
      
C     [page 2-6]

 76   GO TO 71
C     ERROR CHECK IS REMOVED!
C**   NEXT 2 LINES BY-PASSED*** 76 L=L+1
      IF(IBCD(L).EQ.JBLANK) GO TO 44
      GO TO 76

 77   IF(IBCD(L1).NE.ISTAR) GO TO 80
      XN=P(NP)
      GO TO 89
 80   DO 81 LL=L1,L
         LD=LL
         IF (IBCD(LL).EQ.IDEC) GO TO 82
 81   CONTINUE
 82   IEX=0
      LA=L1
      LB=LD-1
c     IF(LD-L1)86,86,83
      if((LD-L1).LE.0) go to 86
      IEX=LD-LA

 84   continue
c     84   CALL MOVR (IBCD,LA,LB)
      DO 85 LL=LA,LB
         IEX=IEX-1
         IGAD=IBCD(LL)
         IX1=ICHAR(IGAD)-ICHAR('0')
         XN=XN+IX1*10.**IEX
 85   continue
c     86   IF(L-LB-2)88,88,87
 86   if(L-LB-2.le.0) go to 88
      LA=LD+1
      LB=L-1
      GO TO 84
 88   XN=XN*SGN

 89   GO TO (2000,2001,2311,2321,2331,2341,331,219),N1
C     TO WRITE S1A8 FOR MONO STEREO CONTROL
 90   P(1)=12.
      P(3)=8.
      P(4)=STER
      IP(1)=4
      END=0.
      SNA8=0.
      GO TO 50
C     FOR PREMATURE END OF FILE ON INPUT
 95   NP=2
      IP(2)=1
      L=0
      IBCD(1)=JSEMI
      GO TO 600

C     TO INITIALIZE
C     C ENTRY READ0
C     C READ (5,1,ERR=95,END=95) (CARD(I),I=1,NC)
C********PDP ********
 9999 READ (inputfile,1,ERR=95,END=95) I,(CARD(I),I=1,NC)
C*****PDP ***** FIRST 'I' IS FOR PDP LINE NUMBERS!
C     C WRITE (6,2) (CARD(I),I=1,NC)
      PRINT 2111,(CARD(I),I=1,NC)
 2111 FORMAT(1H 128A1)
C********PDP ********
      IPDP=1
      I=0
      IDEF=0
      IBK=2
      STER=0.
      END=0.
      SNA8=0.
      if (i100.eq.0) go to 4322
      i100=0
      go to 100
c     RETURN
      END


C     WRITE1 PASS 1 DATA-WRITING ROUTINE
C     *** MUSIC V ***
      SUBROUTINE WRITE1(N)
      COMMON P(100),IP(10),D(2000)
c     COMMON P(100),IP(10)
      K=IP(1)
      WRITE(N, *)K, (P(J),J=1,K)
      RETURN
      END

c     SUBROUTINE PLF
c     COMMON P(100),IP(10),D(2000)
C     C    ENTRY PLF1
C     C    ENTRY PLF2
C     C    ENTRY PLF3
C     C    ENTRY PLF4
C     C    ENTRY PLF5
c     END

C     ERRO1    GENERAL ERROR ROUTINE
C     ***MUSIC V ***
      SUBROUTINE ERROR(I)
      PRINT 8100,I
 8100 FORMAT(13HERROR OF TYPEI5)
      RETURN
      END

      SUBROUTINE HARVEY
C     C    WRITE (6,1)
      PRINT 1011
C********PDP *********
 1011 FORMAT(' WHERE IS HARVEY')
C     VL feb 22 do not exit from here, return instead      
C     CALL EXIT
      RETURN
      END

c     SUBROUTINE MOVR(IBCD,LA,LB)
c     DIMENSION IBCD(300)
C     DO 1 J=LA,LB
C     1       IBCD(J)=IBCD(J)-ICHAR('0')
C     
C     C  1 IBCD(J)=I5-(IBCD(J))/16777216
C********PDP ********
C     1       IBCD(J)=IBCD(J)/536870912-48
C     2    DUMMY=0
C     TO SET BREAKPOINT.
c     RETURN
c     END


      
      


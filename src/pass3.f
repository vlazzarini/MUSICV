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
C     pass3

C     This reads pass2.data and writes the raw (headerless) binary sound
C     file snd.raw.

C     This reads pass2.data and writes the raw (headerless) binary sound
C     file snd.raw.  This is a file of 4-byte floats at 44100Hz, mono.      
      
C     NB (VL,09): despite the above comment, SR is actually set to 44.1KHz .      

C     Victor Lazzarini, Sept 09
C     added GEN4,GEN5,GEN6 and GEN7 from Risset's Catalogue
C     added IOS as a linear-interpolating oscillator
C     which I am supposing it is what Risset means in his 
C     catalogue
C     NB: Feb 2022. Found the original code for the interpolating
C     oscillator from JCR's archive (Fonds Risset, Prism)
C     Restored it to this code, removing the 2009 implementation      

C     Victor Lazzarini, Jan 22
C     A bug in STR has been fixed where the frame counter ICT was not
C     updated
C     The possibility of stereo output has been restored by increasing
C     the buffering size from 768 to 1536 and the allocated size of the
C      IOBUF in SAMOUT has also been doubled
C
C     Output can now be mono or stereo with any sampling rate. 
C     Sampling rate and stereo/mono parameters can now be set in score.
C     A text file called "snd_params.txt" is written in pass3 containing
C     the number of channels and sampling rate used

      
C     PASS3   PASS 3 MAIN PROGRAM
C     *** MUSIC V ***
C     DATA SPECIFICATION
C     VL Feb 22 - making this a function so it can be called externally      
      INTEGER FUNCTION PASS3()      
      INTEGER PEAK
      DIMENSION T(50),TI(50),ITI(50)
      COMMON I(15000),P(100)/PARM/IP(21)/FINOUT/PEAK,NRSOR
C     C******** DATA IIIRD/Z5EECE66D/

      DATA IP/12,512,44100,14500,14400,512,13000,35,40,6657,2048,
     *     1000000,6657,512,7777777,6*0/

      DATA IIIRD/976545367/
C     SET I ARRAY =0 (7/10/69)
      DATA I/15000*0/
c      CHARACTER*1 JSTR(5)
c      CHARACTER*1 FLNM(32)

      integer inputfile
      integer outputfile

C*****************
C     INITIALIZATION OF PIECE
C     ARBITRARY STARTING NUMBER FOR SUBROUTINE RANDU


      inputfile=1
      outputfile=2
      open(inputfile, FILE='pass2.data', STATUS='OLD')
c      open(outputfile, FILE='pass3.data')

      open(outputfile, 
     * FILE='snd.raw', 
     * form='unformatted', 
     * access='direct',
     * status='replace',
     *     recl=4)
      
      I(7)=IIIRD
      IP9=IP(9)
      PEAK=0
      NRSOR=0
C********NREAD=3
C********NWRITE=2
c      NREAD=21
C     PDP DSK1=DEV.21
c      NWRITE=1
C     PDP DSK=DEV.1
c      REWIND NREAD
c      REWIND NWRITE
c      PRINT 10001
c      READ 10002,FLNM,IDSK
C     PRINT 'PASS2' OR FILENAME + ANY POS.NUMB. TO WRITE SMPLS ON DSK.
c      IF(FLNM.EQ.'     '.OR.FLNM.EQ.'PASS2')FLNM='FOR21'

c      CALL IFILE(21,FLNM)

c      IF(IDSK.LE.0) GO TO 10003
c      JSTR='MUSAA'

c      CALL PUTFILE(JSTR)

C     IF IDSK>=1, SAMPLES WILL BE WRITTEN ON DSK (MUSAA.DMD)
c      IDSK=0
      IDSK=1

c      GO TO 10004

c 10003 IDSK=-1
c 10001 FORMAT(' TYPE FILE NAME'/)
c 10002 FORMAT(A5)
C**** ABOVE FOR PDP 10 ******

c 10004 SCLFT=IP(12)
      SCLFT=IP(12)

      I(2)=IP(4)
      MS1=IP(7)
      MS3=MS1+(IP(8)*IP(9))-1
      MS2=IP(8)
      I(4)=IP(3)
      MOUT=IP(10)
C     INITIALIZATION OF SECTION
 5    T(1)=0.0
      DO 220 N1=MS1,MS3,MS2
         I(N1)=-1
 220     CONTINUE
      DO 221 N1=1,IP9
         TI(N1)=1000000.0
 221     CONTINUE

C     MAIN CARD READING LOOP
c     204  CALL DATA (NREAD)
       PCH = 0
 204   CALL DATA (inputfile)
c      IF(P(2)-T(1))200,200,244
      IF((P(2)-T(1)).gt.0.0) go to 244
 200  IOP=P(1)
c      IF(IOP)201,201,202
      IF(IOP.gt.0) go to 202
 201  CALL ERROR(1)
      PASS3 = 1
      GO TO 204
         
C     [page 4-2]
         
c 202  IF(IP(1)-IOP)201,203,203
 202  IF((IP(1)-IOP).lt.0) go to 201
c 203  GO TO (1,2,3,4,5,6,201,201,201,201,11,11),IOP
       GO TO (1,2,3,4,5,6,201,201,201,201,11,11),IOP
 11   IVAR=P(3)
      IVARE=IVAR+I(1)-4
      DO 297 N1=IVAR,IVARE
         IVARP=N1-IVAR+4
         I(N1)=P(IVARP)
C     VL:25/01/22 updated for gfortran 2018           
 297  CONTINUE
      
      GO TO 204
 3    IGEN=P(3)
      GO TO (281,282,283,284,285,286,287,288),IGEN
 281  CALL GEN1
      GO TO 204
 282  CALL GEN2
      GO TO 204
 283  CALL GEN3
      GO TO 204
 284  CALL GEN4
      GO TO 204
 285  CALL GEN5
      GO TO 204
 286  CALL GEN6
      GO TO 204
 287  CALL GEN7
      GO TO 204
 288  CALL GEN8
      GO TO 204
 4    IVAR=P(3)
      IVARE=IVAR+I(1)-4
      DO 296 N1=IVAR,IVARE
         IVARP=N1-IVAR+4
         I(N1+100)=P(IVARP)*SCLFT
C     VL:25/01/22 updated for gfortran 2018  
 296  CONTINUE
      GO TO 204

c 6    CALL FROUT3(IDSK)
 6    continue

      K=IP(10)
      L=IP(10)+IP(14)-1
      DO 1001 J=K,L
         I(J)=0
 1001 CONTINUE
      CALL SAMOUT(IDSK,IP(14), outputfile)
C     REWIND NWRITE
C     WRITE(6,10) PEAK,NRSOR
C      PRINT 10,PEAK,NRSOR
C 10   FORMAT ('0PEAK AMPLITUDE WAS',I8/'0NUMBER OF SAMPLES OUT OF RANGE WAS',I8)
C     CALL EXIT
C     IF(IDSK.LT.0) CALL EXIT
      IF(IDSK.LT.0) GOTO 9999
      J=IP(10)
      L=J+1024
      DO 2001 K=J,L
         I(K)=0
 2001 CONTINUE
C     WILL WRITE 1024 0'S ON DSK.

      CALL FASTOUT(I(J),1024, outputfile)

c      CALL FINFILE
 9999 close(inputfile)
      close(outputfile)
       
      PASS3 = 0
      RETURN  
C      CALL EXIT
c      END

c      STOP
C     ENTER NOTE TO BE PLAYED
 1    DO 230 N1=MS1,MS3,MS2
c         IF(I(N1)+1)230,231,230
         IF((I(N1)+1).eq.0) go to 231
 230  CONTINUE
      CALL ERROR(2)
      PASS3 = 2
      GO TO 204
 231  M1=N1
      M2=N1+I(1)-1
      M3=M2+1
      M4=N1+IP(8)-1
      DO 232 N1=M1,M2
         M5=N1-M1+1
         I(N1)=P(M5)*SCLFT
C     VL:25/01/22 updated for gfortran 2018           
 232  CONTINUE   
      I(M1)=P(3)
      DO 233 N1=M3,M4
         I(N1)=0
C     VL:25/01/22 updated for gfortran 2018  
 233  CONTINUE   
      DO 235 N1=1,IP9
c         IF(TI(N1)-1000000.)235,234,235
         IF(TI(N1).ne.1000000.) go to 235
c 234     TI(N1)=P(2)+P(4)
          TI(N1)=P(2)+P(4)
         ITI(N1)=M1
         GO TO 204
 235  CONTINUE
      CALL ERROR(3)
      PASS3 = 3
      GO TO 204
C     DEFINE INSTRUMENT
 2    M1=I(2)
      M2=IP(5)+IFIX(P(3))
      I(M2)=M1
c 218  CALL DATA (NREAD)
 218  CALL DATA (inputfile)
c      IF(I(1)-2)210,210,211
      IF(I(1).gt.2) go to 211
c 210  I(M1)=0
       I(M1)=0
      I(2)=M1+1
      
C     [page 4-3]
      
      GO TO 204
 211  I(M1)=P(3)
      M3=I(1)
      I(M1+1)=M1+M3-1
      M1=M1+2
      DO 217 N1=4,M3
         M5=P(N1)
c         IF(M5)212,213,213
         IF(M5.ge.0) go to 213
c 212     IF(M5+100)300,301,301
         IF((M5+100).ge.0) go to 301
c 300     I(M1)=-IP(2)+(M5+101)*IP(6)
         I(M1)=-IP(2)+(M5+101)*IP(6)
         GO TO 216
 301     I(M1)=-IP(13)+(M5+1)*IP(14)
         GO TO 216
c 213     IF(M5-100)214,214,215
 213     IF((M5-100).gt.0) go to 215
c 214     I(M1)=M5
        I(M1)=M5
         GO TO 216
 215     I(M1)=M5+262144
 216     M1=M1+1
 217  CONTINUE
      GO TO 218
C     PLAY TO ACTION TIME
 244  T(2)=P(2)
 250  TMIN=1000000.
      IREST=1
      DO 241 N1=1,IP9
c         IF(TMIN-TI(N1))241,241,240
         IF(TMIN.le.TI(N1)) go to 241
c 240     TMIN=TI(N1)
         TMIN=TI(N1)
         MNOTE=N1
 241  CONTINUE
c      IF(1000000.-TMIN)251,251,243
      IF(1000000.0.le.TMIN)go to 251

c 243  IF(TMIN-T(2))245,245,246
      IF(TMIN.gt.T(2)) go to 246
c 245  T(3)=TMIN
      T(3)=TMIN
      GO TO 260
 246  T(3)=T(2)
      GO TO 260
c 247  IF(T(1)-T(2))249,200,200
 247  IF(T(1).ge.T(2)) go to 200
c 249  TI(MNOTE)=1000000.
       TI(MNOTE)=1000000.
      M2=ITI(MNOTE)
      I(M2)=-1
      GO TO 250
C     SETUP REST
 251  T(3)=T(2)
      IREST=2
      GO TO 260
C     PLAY
 260  ISAM=(T(3)-T(1))*FLOAT(I(4))+.5
      T(1)=T(3)
c      IF(ISAM)247,247,266
      IF(ISAM.le.0) go to 247
c 266  IF(ISAM-IP(14))262,262,263
 266  IF(ISAM.gt.IP(14))go to 263
c 262  I(5)=ISAM
       I(5)=ISAM
      ISAM=0
      GO TO 264
 263  I(5)=IP(14)
      ISAM=ISAM-IP(14)

C     VL 27/01/22 printing a file with channels and sampling rate
C     used so that a driver program can convert the output correctly      
      if(PCH > 0) goto 264
      open (10, file='snd_params.txt', status='replace')
      write(10, *) I(8)+1, I(4)
      close(10)
      PCH = 1
c     264  IF(I(8))290,290,291      
 264  IF(I(8).gt.0) go to 291
c 290  M3=MOUT+I(5)-1
      M3=MOUT+I(5)-1
      MSAMP=I(5)
      GO TO 292
 291  M3=MOUT+(2*I(5))-1
      MSAMP=2*I(5)

      
C     [page 4-4]
      
 292  DO 267 N1=MOUT,M3
         I(N1)=0
C     VL:25/01/22 updated for gfortran 2018           
 267  CONTINUE     
         GO TO (268,265),IREST
 268     DO 270 NS1=MS1,MS3,MS2
c            IF(I(NS1)+1)271,270,271
            IF((I(NS1)+1).eq.0) go to 270
C     GO THROUGH UNIT GENERATORS IN INSTRUMENT
c 271        I(3)=NS1
            I(3)=NS1
            IGEN=IP(5)+I(NS1)
            IGEN=I(IGEN)
 272        I(6)=IGEN
CC***** IF((IGEN)-101)293,294,294
CC***** 293 CALL SAMGEN(I)
CC***** ABOVE FOR MACHINE LANG. UNIT GENERATORS ******
CC***** GO TO 295
c 294        CALL FORSAM
          CALL FORSAM
c 295        IGEN=I(IGEN+1)
        IGEN=I(IGEN+1)
c            IF(I(IGEN))270,270,272
            IF(I(IGEN).gt.0) go to 272
 270     CONTINUE
 265     CALL SAMOUT(IDSK,MSAMP, outputfile)
c      IF(ISAM)247,247,266
      IF(ISAM.le.0) go to 247
      go to 266
      END
      
      
C     [page 5-1]
      
C     FORS3        FORTRAN UNIT GENERATOR ROUTINE
C     *** MUSIC V ***
      SUBROUTINE FORSAM
      DIMENSION I(15000),P(100),IP(21),L(8),M(8)
      COMMON I,P/PARM/IP
      EQUIVALENCE (M1,M(1)),(M2,M(2)),(M3,M(3)),(M4,M(4)),(M5,M(5))
      EQUIVALENCE (M6,M(6)),(M7,M(7)),(M8,M(8)),(L1,L(1)),(L2,L(2))
      EQUIVALENCE (L3,L(3)),(L4,L(4)),(L5,L(5)),(L6,L(6)),(L7,L(7))
      EQUIVALENCE (L8,L(8)),(RN1,IRN1),(RN3,IRN3),(RN,IRN)
C     C***** DATA IMULT/Z5EECE66D/
      DATA IIIRD/976545367/
      SFI=1./FLOAT(IP(12))
      SFF=1./FLOAT(IP(15))
      SFID=FLOAT(IP(12))
      SFXX=FLOAT(IP(12))/FLOAT(IP(15))
      XNFUN=IP(6)-1
C     COMMON INITIALIZATION OF GENERATORS
      N1=I(6)+2
      N2=I(N1-1)-1
      DO 204 J1=N1,N2
         J2=J1-N1+1
c         IF(I(J1))200,201,201
         IF(I(J1).ge.0) go to 201
c 200     L(J2)=-I(J1)
         L(J2)=-I(J1)
         M(J2)=1
         GO TO 204
 201     M(J2)=0
c         IF(I(J1)-262144)202,202,203
         IF((I(J1)-262144).gt.0) go to 203
C*****WHAT DOES THE BIG NUMBER DO?????
c 202     L(J2)=I(J1)+I(3)-1
         L(J2)=I(J1)+I(3)-1
         GO TO 204
 203     L(J2)=I(J1)-262144
 204  CONTINUE
      NSAM=I(5)
      N3=I(N1-2)
      NGEN=N3-100
      GO TO (101,102,103,104,105,106,107,108,109,110,111,112,113),NGEN
 112  RETURN
C     UNIT GENERATORS
C     OUTPUT BOX
c 101  IF(M1)260,260,261
 101  IF(M1.gt.0) go to 261
c 260  IN1=I(L1)
       IN1=I(L1)
 261  CONTINUE
      DO 270 J3=1,NSAM
c         IF(M1)265,265,264
         IF(M1.le.0) go to 265
c 264     J4=L1+J3-1
          J4=L1+J3-1
         IN1=I(J4)
 265     J5=L2+J3-1
         I(J5)=IN1+I(J5)
 270  CONTINUE
      RETURN
C     OSCILLATOR
 102  SUM=FLOAT(I(L5))*SFI
c      IF(M1)280,280,281
      IF(M1.gt.0) go to 281
c 280  AMP=FLOAT(I(L1))*SFI
       AMP=FLOAT(I(L1))*SFI
c 281  IF(M2)282,282,283
 281  IF(M2.gt.0) go to 283
c 282  FREQ=FLOAT(I(L2))*SFI
       FREQ=FLOAT(I(L2))*SFI
 283  CONTINUE
      DO 293 J3=1,NSAM
         J4=INT(SUM)+L4
         F=FLOAT(I(J4))
         
C     [page 5-2]
         
c         IF(M2)285,285,286
         IF(M2.gt.0) go to 286
c 285     SUM=SUM+FREQ
         SUM=SUM+FREQ
         GO TO 290
 286     J4=L2+J3-1
         SUM=SUM+FLOAT(I(J4))*SFI
C     C 290 IF(SUM-XNFUN)288,287,287
 290     IF(SUM.GE.XNFUN)GO TO 287
C     C 287 SUM=SUM-XNFUN
         IF(SUM.LT.0.0)GO TO 289
 288     J5=L3+J3-1
c         IF(M1)291,291,292
         IF(M1.gt.0) go to 292
c 291     I(J5)=IFIX(AMP*F*SFXX)
          I(J5)=IFIX(AMP*F*SFXX)
         GO TO 293
C************
 287     SUM=SUM-XNFUN
         GO TO 288
 289     SUM=SUM+XNFUN
         GO TO 288
C**********ABOVE FOR FM (NEG. FREQ. TO OSCIL)
 292     J6=L1+J3-1
         I(J5)=IFIX(FLOAT(I(J6))*F*SFF)
 293  CONTINUE
C     PRINT *, L3
      I(L5)=IFIX(SUM*SFID)
      RETURN
C     ADD TWO BOX
c 103  IF(M1)250,250,251
 103  IF(M1.gt.0) go to 251
c 250  IN1=I(L1)
       IN1=I(L1)
c 251  IF(M2)252,252,253
 251  IF(M2.gt.0) go to 253
c 252  IN2=I(L2)
       IN2=I(L2)
 253  DO 258 J3=1,NSAM
c         IF(M1)255,255,254
         IF(M1.le.0) go to 255
c 254     J4=L1+J3-1
          J4=L1+J3-1
         IN1=I(J4)
c 255     IF(M2)257,257,256
 255     IF(M2.le.0) go to 257
c 256     J5=L2+J3-1
          J5=L2+J3-1
         IN2=I(J5)
 257     J6=L3+J3-1
         I(J6)=IN1+IN2
 258  CONTINUE
      RETURN
C     RANDOM INTERPOLATING GENERATOR
 104  SUM=FLOAT(I(L4))*SFI
c      IF(M1)310,310,311
      IF(M1.gt.0) go to 311
c 310  XIN1=FLOAT(I(L1))*SFI
       XIN1=FLOAT(I(L1))*SFI
c 311  IF(M2)312,312,313
 311  IF(M2.gt.0) go to 313
c 312  XIN2=FLOAT(I(L2))*SFI
       XIN2=FLOAT(I(L2))*SFI
 313  IRN1=I(L5)
      IRN3=I(L6)
      DO 340 J3=1,NSAM
c         IF(M1)316,316,315
         IF(M1.le.0) go to 316
c 315     J4=L1+J3-1
          J4=L1+J3-1
         XIN1=FLOAT(I(J4))*SFI
c 316     IF(M2)318,318,317
 316     IF(M2.le.0) go to 318
c 317     J5=L2+J3-1
         J5=L2+J3-1
         XIN2=FLOAT(I(J5))*SFI
c 318     IF(SUM-XNFUN)320,319,319
 318     IF((SUM-XNFUN).lt.0.0) go to 320
c 319     SUM=SUM-XNFUN
          SUM=SUM-XNFUN
         I(7)=IABS(I(7)*IMULT)
         RN4=(2.*FLOAT(I(7))*SFF-1.)
         RN2=RN4-RN3
         
C     [page 5-3]
         
         RN1=RN3
         RN3=RN4
         GO TO 321
 320     RN2=RN3-RN1
 321     J7=L3+J3-1
         I(J7)=XIN1*(RN1+(RN2*SUM)/XNFUN)*SFID
         SUM=SUM+XIN2
 340  CONTINUE
      I(L4)=IFIX(SUM*SFID)
      I(L5)=IRN1
      I(L6)=IRN3
      RETURN
C     ENVELOPE GENERATOR
 105  SUM=FLOAT(I(L7))*SFI
c      IF(M1)380,380,381
      IF(M1.gt.0) go to 381
c 380  XIN1=FLOAT(I(L1))*SFI
       XIN1=FLOAT(I(L1))*SFI
c 381  IF(M4)382,382,383
 381  IF(M4.gt.0) go to 383
c 382  XIN4=FLOAT(I(L4))*SFI
       XIN4=FLOAT(I(L4))*SFI
c 383  IF(M5)384,384,385
 383  IF(M5.gt.0) go to 385
c 384  XIN5=FLOAT(I(L5))*SFI
       XIN5=FLOAT(I(L5))*SFI
c 385  IF(M6)386,386,387
 385  IF(M6.gt.0) go to 387
c 386  XIN6=FLOAT(I(L6))*SFI
       XIN6=FLOAT(I(L6))*SFI
 387  X1=XNFUN/4.
      X2=2.*X1
      X3=3.*X1
      DO 403 J3=1,NSAM
         J4=INT(SUM)+L2
         F=FLOAT(I(J4))
c         IF(M1)405,405,404
         IF(M1.le.0) go to 405
c 404     J8=L1+J3-1
         J8=L1+J3-1
         XIN1=FLOAT(I(J8))*SFI
c 405     IF(SUM-XNFUN)389,388,388
 405     IF((SUM-XNFUN).lt.0.0) go to 389
c 388     SUM=SUM-XNFUN
          SUM=SUM-XNFUN
c 389     IF(SUM-X1)390,390,393
 389     IF((SUM-X1).gt.0.0) go to 393
c 390     IF(M4)392,392,391
          IF(M4.le.0) go to 392
c 391     J4=L4+J3-1
         J4=L4+J3-1
         XIN4=FLOAT(I(J4))*SFI
 392     SUM=SUM+XIN4
         GO TO 402
c 393     IF(SUM-X2)394,394,397
 393     IF((SUM-X2).gt.0.0) go to 397
c 394     IF(M5)396,396,395
         IF(M5.le.0) go to 396
c 395     J5=L5+J3-1
          J5=L5+J3-1
         XIN5=FLOAT(I(J5))*SFI
 396     SUM=SUM+XIN5
         GO TO 402
c 397     IF(M6)400,400,399
 397     IF(M6.le.0) go to 400
c 399     J6=L6+J3-1
         J6=L6+J3-1
         XIN6=FLOAT(I(J6))*SFI
 400     SUM=SUM+XIN6
 402     J7=L3+J3-1
         I(J7)=IFIX(XIN1*F*SFXX)
 403  CONTINUE
      I(L7)=IFIX(SUM*SFID)
      RETURN
C     STEREO OUTPUT BOX
c 106  IF(M1)500,500,501
 106  IF(M1.gt.0) go to 501
c 500  IN1=I(L1)
       IN1=I(L1)
c 501  IF(M2)502,502,503
 501  IF(M2.gt.0) go to 503
c 502  IN2=I(L2)
       IN2=I(L2)
 503  NSSAM=2*NSAM
C      PRINT *, L3
C     [page 5-4]
      
C     6/29/70 L.C.SMITH
      ICT=0
      DO 510 J3=1,NSSAM,2
c         IF(M1) 505,505,504
         IF(M1.le.0) go to 505
C     C*** 504 J4=L1+J3-1
c 504     J4=L1+ICT
          J4=L1+ICT
         IN1=I(J4)
 505     J5=L3+J3-1
         I(J5)=IN1+I(J5)
c         IF(M2)507,507,506
         IF(M2.le.0) go to 507
C     C*** 506 J4=L2+J3-1
c 506     J4=L2+ICT
         J4=L2+ICT
         IN2=I(J4)
 507     J5=L3+J3
         I(J5)=IN2+I(J5)
         ICT=ICT+1
 510  CONTINUE
C     PRINT *, J5, J3   
      RETURN
C     ADD 3 BOX
c 107  IF(M1)750,750,751
 107  IF(M1.gt.0) go to 751
c 750  IN1=I(L1)
       IN1=I(L1)
c 751  IF(M2)752,752,753
 751  IF(M2.gt.0) go to 753
c 752  IN2=I(L2)
       IN2=I(L2)
c 753  IF(M3)754,754,755
 753  IF(M3.gt.0) go to 755
c 754  IN3=I(L3)
       IN3=I(L3)
 755  DO 780 J3=1,NSAM
c         IF(M1)757,757,756
         IF(M1.le.0) go to 757
c 756     J4=L1+J3-1
          J4=L1+J3-1
         IN1=I(J4)
c 757     IF(M2)759,759,758
 757     IF(M2.le.0) go to 759
c 758     J5=L2+J3-1
          J5=L2+J3-1
         IN2=I(J5)
c 759     IF(M3)761,761,760
 759     IF(M3.le.0) go to 761
c 760     J6=L3+J3-1
          J6=L3+J3-1
         IN3=I(J6)
 761     J7=L4+J3-1
         I(J7)=IN1+IN2+IN3
 780  CONTINUE
      RETURN
C     ADD 4 BOX
c 108  IF(M1)850,850,851
 108  IF(M1.gt.0) go to 851
c 850  IN1=I(L1)
       IN1=I(L1)
c 851  IF(M2)852,852,853
 851  IF(M2.gt.0) go to 853
c 852  IN2=I(L2)
       IN2=I(L2)
c 853  IF(M3)854,854,855
 853  IF(M3.gt.0) go to 855
c 854  IN3=I(L3)
       IN3=I(L3)
c 855  IF(M4)856,856,857
 855  IF(M4.gt.0) go to 857
c 856  IN4=I(L4)
       IN4=I(L4)
 857  DO 880 J3=1,NSAM
c         IF(M1)859,859,858
         IF(M1.le.0) go to 859
c 858     J4=L1+J3-1
          J4=L1+J3-1
         IN1=I(J4)
c 859     IF(M2)861,861,860
 859     IF(M2.le.0) go to 861
c 860     J5=L2+J3-1
         J5=L2+J3-1
         IN2=I(J5)
c 861     IF(M3)863,863,862
 861     IF(M3.le.0) go to 863
c 862     J6=L3+J3-1
         J6=L3+J3-1
         IN3=I(J6)
c 863     IF(M4)865,865,864
 863     IF(M4.le.0) go to 865
c 864     J7=L4+J3-1
         J7=L4+J3-1
         IN4=I(J7)
         
C     [page 5-5]
         
 865     J8=L5+J3-1
         I(J8)=IN1+IN2+IN3+IN4
 880  CONTINUE
      RETURN
C     MULTIPLIER
c 109  IF(M1)900,900,901
 109  IF(M1.gt.0) go to 901
      XIN1=FLOAT(I(L1))*SFI
c 901  IF(M2)902,902,903
 901  IF(M2.gt.0) go to 903
      XIN2=FLOAT(I(L2))*SFI
 903  DO 908 J3=1,NSAM
c         IF(M1)905,905,904
         IF(M1.le.0) go to 905
         J4=L1+J3-1
         XIN1=FLOAT(I(J4))*SFI
c 905     IF(M2)907,907,906
 905     IF(M2.le.0) go to 907
         J5=L2+J3-1
         XIN2=FLOAT(I(J5))*SFI
 907     J6=L3+J3-1
         I(J6)=XIN1*XIN2*SFID
 908  CONTINUE
      RETURN
C     SET NEW FUNCTION IN OSC OR ENV
 110  ILOC=N1+6
      IF(I(N1+1).EQ.105) ILOC=N1+4
      IN1=I(3)+I(N1)-1
      IIN1=I(IN1)/IP(12)
      IF(IIN1.EQ.0) I(ILOC)=-IP(2)-(IIN1-1)*IP(6)
c 960  RETURN
      RETURN

C     RANDOM AND HOLD GENERATOR
 111  SUM=FLOAT(I(L4))*SFI
c      IF(M1)910,910,911
      IF(M1.gt.0) go to 911
c 910  XIN1=FLOAT(I(L1))*SFI
      XIN1=FLOAT(I(L1))*SFI
c 911  IF(M2)912,912,913
 911  IF(M2.gt.0) go to 913
c 912  XIN2=FLOAT(I(L2))*SFI
      XIN2=FLOAT(I(L2))*SFI
 913  IRN=I(L5)
      DO 940 J3=1,NSAM
c         IF(M1)916,916,915
         IF(M1.le.0) go to 916
c 915     J4=L1+J3-1
          J4=L1+J3-1
         XIN1=FLOAT(I(J4))*SFI
c 916     IF(M2)918,918,917
 916     IF(M2.le.0) go to 918
c 917     J5=L2+J3-1
         J5=L2+J3-1
         XIN2=FLOAT(I(J5))*SFI
c 918     IF(SUM-XNFUN)920,919,919
 918     IF((SUM-XNFUN).lt.0.0) go to 920
c 919     SUM=SUM-XNFUN
         SUM=SUM-XNFUN
         I(7)=IABS(I(7)*IMULT)
         RN=(2.*FLOAT(I(7))*SFF-1.)
 920     J7=L3+J3-1
         I(J7)=XIN1*RN*SFID
         SUM=SUM+XIN2
 940  CONTINUE
      I(L4)=IFIX(SUM*SFID)
      I(L5)=IRN
      RETURN
C     VL 7/02/022 restored ugen from JCR's archive material      
C     INTERPOLATING OSCILLATOR (FROM 1968/9 LISTINGS)
 113  SUM=FLOAT(I(L5))*SFI
      IF(M1.gt.0) go to 1081
 1080 AMP=FLOAT(I(L1))*SFI
 1081 IF(M2.gt.0) go to 1083
 1082 FREQ=FLOAT(I(L2))*SFI
 1083 XNFUN = IP(6) - 1
      DO 1093 J3=1,NSAM
         J4=INT(SUM)+L4
         FRAC=SUM-AINT(SUM)
 1086    F1 = FLOAT(I(J4))
         F2 = FLOAT(I(J4+1))
 1087    F3 = F1 + (F2-F1)*FRAC
         IF(M2.gt.0) GOTO 1089
 1088    SUM = SUM+FREQ
         GOTO 1090
 1089    J4=L2+J3-1
         SUM=SUM+FLOAT(I(J4))*SFI
 1090    IF((SUM-XNFUN).le.0) GOTO 1085
 1084    SUM=SUM-XNFUN
 1085    J5=L3+J3-1
         IF(M1.gt.0) GOTO 1092
 1091    I(J5) = IFIX(AMP*F3*SFXX)
         GOTO 1093
 1092    J6=L1+J3-1
         I(J5) = IFIX(FLOAT(I(J6))*F3*SFF)
 1093    CONTINUE
      I(L5)=IFIX(SUM*SFID)
      RETURN
      END
      
C     [page 6-1]
      
C     GEN1 FUNCTION GENERATOR 1
C     *** MUSIC V ***
      SUBROUTINE GEN1
      DIMENSION I(15000),P(100),IP(21)
      COMMON I,P/PARM/IP
      N1=IP(2)+(IFIX(P(4))-1)*IP(6)
      M1=7
      SCLFT=IP(15)
c 102  IF(P(M1+1))103,103,100
 102  IF(P(M1+1).le.0.0) go to 103
c 100  V1=P(M1-2)*SCLFT
      V1=P(M1-2)*SCLFT
      V2=(P(M1)-P(M1-2))/(P(M1+1)-P(M1-1))*SCLFT
      MA=N1+IFIX(P(M1-1))
      MB=N1+IFIX(P(M1+1))-1
      DO 101 J=MA,MB
         XJ=J-MA
         I(J)=V1+V2*XJ
C     VL:25/01/22 updated for gfortran 2018           
 101  CONTINUE   
      IF(IFIX(P(M1+1)).EQ.(IP(6)-1))GO TO 103
      M1=M1+2
      GO TO 102
 103  I(MB+1)=P(M1)*SCLFT
      RETURN
      END
C     GEN2 FUNCTION GENERATOR 2
c     *** MUSIC V ***
      SUBROUTINE GEN2
      DIMENSION I(15000),P(100),IP(21),A(7000)
      COMMON I,P/PARM/IP
      EQUIVALENCE(I,A)
      SCLFT=IP(15)
      N1=IP(2)+(IFIX(P(4))-1)*IP(6)
      N2=N1+IP(6)-1
      DO 101 K1=N1,N2
         A(K1)=0.0
C     VL:25/01/22 updated for gfortran 2018           
 101  CONTINUE   
      FAC=6.283185/(FLOAT(IP(6))-1.0)
      NMAX=I(1)
      N3=5+INT(ABS(P(NMAX)))-1
c      IF(N3-5)104,100,100
      IF((N3-5).lt.0) go to 104
c 100  DO 103 J=5,N3
      DO 103 J=5,N3
         FACK=FAC*FLOAT(J-4)
         DO 102 K=N1,N2
            A(K)=A(K)+SIN(FACK*FLOAT(K-N1))*P(J)
C     VL:25/01/22 updated for gfortran 2018  
 102     CONTINUE   
 103     CONTINUE
 104  N4=N3+1
      N5=I(1)-1
c      IF(N5-N4)114,105,105
      IF((N5-N4).le.0) go to 114
c 105  DO 107 J1=N4,N5
      DO 107 J1=N4,N5
         FACK=FAC*FLOAT(J1-N4)
         DO 106 K1=N1,N2
            A(K1)=A(K1)+COS(FACK*FLOAT(K1-N1))*P(J1)
C     VL:25/01/22 updated for gfortran 2018              
 106     CONTINUE   
 107     CONTINUE
 114  CONTINUE
c      IF(P(NMAX))112,112,108
      IF(P(NMAX).le.0.0) go to 112
c 108  FMAX=0.0
      FMAX=0.0
      DO 110 K2=N1,N2
c         IF(ABS(A(K2))-FMAX)110,110,109
         IF((ABS(A(K2))-FMAX).le.0.0) go to 110
c 109     FMAX=ABS(A(K2))
         FMAX=ABS(A(K2))
 110  CONTINUE
 113  DO 111 K3=N1,N2
         I(K3)=(A(K3)*SCLFT*.99999)/FMAX
C     VL:25/01/22 updated for gfortran 2018           
 111  CONTINUE   
      RETURN
                  
C     [page 6-2]
                  
 112  FMAX=.99999
      GO TO 113
      END
C     GEN3 FUNCTION GENERATOR 3
C     *** MUSIC V ***
C     ASSUMPTIONS--P(4) = THE NUMBER OF THE FUNCTION TO BE GENERATED,
C     I(1) = WORD COUNT FOR CURRENT DATA RECORD
C     P(5) = THE BEGINNING THE THE LIST OF DESCRIPTION NUMBERS
C     IP(2) = THE BEGINNING SUBSCRIPT FOR FUNCTIONS IN THE I ARRAY,
C     IP(6) = THE LENGTH OF THE FUNCTIONS
C     IP(15) = SCALE FACTOR FOR STORED FUNCTIONS
C     
      SUBROUTINE GEN3
      COMMON I(15000),P(100) /PARM/ IP(21)
      N=I(1)-5
      NL=5
      SCLFT=IP(15)
      LL=IP(6)
      RMIN=0
      RMAX=0
      NR=NL+N
      DO 10 J=NL,NR
         IF(P(J).GT.RMAX) RMAX=P(J)
         IF(P(J).LT.RMIN) RMIN=P(J)
C     VL:25/01/22 updated for gfortran 2018            
 10   CONTINUE   
      DIV=AMAX1(ABS(RMIN),ABS(RMAX))
      N1 = IP(2) + (IFIX(P(4))-1)*IP(6)
      I(N1)=(P(NL)/DIV)*SCLFT
      LAST=N1
      DO 100 J=1,N
         LL = LL-LL/(N-J+1)
         IX = N1+IP(6)-LL-1
         IX2 = NL+J
         I(IX)=(P(IX2)/DIV)*SCLFT
         DELTA=FLOAT(I(IX))-FLOAT(I(LAST))
         NR = IX-LAST-1
         SEG = NR+1
         HNCR=DELTA/SEG
         DO 50 K=1,NR
            IX2 = LAST+K
            I(IX2)=FLOAT(I(IX2-1))+HNCR
C     VL:25/01/22 updated for gfortran 2018              
 50      CONTINUE
         LAST=IX
C     VL:25/01/22 updated for gfortran 2018          
 100  CONTINUE   
      RETURN
      END

C     DATA3 PASS 3 DATA INPUTING ROUTINE
C     *** MUSIC V ***
      SUBROUTINE DATA(N)
      COMMON I(15000),P(100)
      READ(N, *) K,(P(J),J=1,K)
      I(1)=K
      RETURN
      END

C     PARM CONTROL DATA SPECIFICATION FOR PASS 3
C     *** MUSIC V ***
C     
C     IP(1) = NUMBER OF OP CODES
C     IP(2) = BEGINNING SUBSCRIPT OF FIRST FUNCTION
C     IP(3) = STANDARD SAMPLING RATE
C     IP(4) = BEGINNING SUBSCRIPT OF INSTRUMENT DEFINITIONS
C     IP(5) = BEGINNING OF LOCATION TABLE FOR INSTRUMENT DEFINITIONS
C     IP(6) = LENGTH OF FUNCTIONS
      
C     [page 6-3]
      
C     IP(7) = BEGINNING OF NOTE CARD PARAMETERS
C     IP(8) = LENGTH OF NOTE CARD PARAMETER BLOCKS
C     IP(9) = NUMBER OF NOTE CARD PARAMETER BLOCKS
C     IP(10) = BEGINNING OF OUTPUT DATA BLOCK
C     IP(11) = SOUND ZERO (SILENCE VALUE)
C     IP(12) = SCALE FACTOR FOR NOTE CARD PARAMETERS
C     IP(13) = BEGINNING OF GENERATOR INPUT-OUTPUT BLOCKS
C     IP(14) = LENGTH OF GENERATOR INPUT-OUTPUT BLOCKS
C     IP(15) = SCALE FACTOR FOR FUNCTIONS
C     
c      BLOCK DATA
c      COMMON /PARM/IP(21)
c      DATA IP/12,512,20000,14500,14400,512,13000,35,40,6657,2048,
c     1     1000000,6657,512,7777777,5*0/
C**** BIG NUMB. IS IBM360'S BIGGEST. 1  65536,6657,512,Z7FFFFFFF/
c     END
C**** SUBROUTINE DUM
C**** ENTRY SAMGEN
C**** ENTRY GEN4
C**** ENTRY GEN5
C**** END
      SUBROUTINE SAMGEN
      RETURN
      END

C     GENS 4,6,7,8 from Risset's catalogue
C     V Lazzarini, 2009
      SUBROUTINE GEN4
      DIMENSION I(15000),P(100),IP(21),A(7000)
      COMMON I,P/PARM/IP
      EQUIVALENCE(I,A)
      SCLFT=IP(15)
      N1=IP(2)+(IFIX(P(4))-1)*IP(6)
      N2=N1+IP(6)-1
      DO 100 K=N1,N2
      A(K1)=0.0   
 100  CONTINUE
      FAC=6.283185/(FLOAT(IP(6))-1.0)
      NMAX=I(1)-4
      DO 103 L=5, NMAX,5
         P2=P(L)
         P3=P(L+1)
         P4=P(L+2)
         JP5=P(L+3)
         IP5=JP5+N1-1
         IP6=IFIX(P(L+4))+N1-1
         DO 105 J=IP5,IP6
            XJ=J-JP5-N1+1
            ARG=XJ*P3+P4
            A(J)=A(J)+P2*SIN(FAC*ARG)
 105     CONTINUE   
 103     CONTINUE
      XMAX=0.0000001
      DO 115 J=N1,N2
C     IF(XMAX-ABS(A(J))) 116,115,115
       IF(XMAX-ABS(A(J)) >= 0) GOTO 115
 116   XMAX=ABS(A(J))
 115  CONTINUE
      DO 120 L=N1,N2
       I(L) = (A(L)*SCLFT*.99999)/XMAX
 120  CONTINUE   
 113  RETURN
      END

      SUBROUTINE GEN5
      END

      SUBROUTINE GEN6
      DIMENSION I(15000),P(100),IP(21),A(512)
      COMMON I,P/PARM/IP
      SCLFT=IP(15)
      N1=IP(2)+(IFIX(P(4))-1)*IP(6)
 6    N11=N1-1
      N6=IP(6)
      N2=N6/4
      XNQ=N2-1
      N3=N2+N2
      N4=N3+N2
      ARG1=P(5)
      ARG2=P(6)
      ARG3=P(7)
      ARG4=P(8)
C     IF(ARG1) 610,610,611
      IF(ARG1 > 0) GOTO 611
 610  Y1=11.*ALOG(2.)/XNQ
 611  Y1=ARG1*ALOG(2.)/XNQ
 612  CONTINUE
C     IF(ARG2) 614,614,615
      IF(ARG2 > 0 ) GOTO 615
 614  Y2=.99999
      GOTO 616
 615  Y2=ARG2
 616  CONTINUE
 618  Y3=.99999
      GOTO 620
 619  Y3=ARG3
 620  CONTINUE
C     IF(ARG4) 622,622,623
      IF(ARG4 > 0) GOTO 623
 622  Y4=11.*ALOG(2.)/XNQ
      GOTO 624
 623  Y4=ARG4*ALOG(2.)/XNQ
 624  CONTINUE
      DO 630 J=1,N2
         XJ=J-N2
         YJ=Y1*XJ
         A(J)=.99999*EXP(YJ)*Y2
         JJ=J+N11
         I(JJ)=A(J)*SCLFT
 630  CONTINUE   
      FACT=(Y3-Y2)/XNQ
      NN2=N2+1
      DO 640 J=NN2,N3
         AJ=J-N2
         A(J)=J-N2
         A(J)=.99999*(Y2+FACT*AJ)
         JJ=J+N11
         I(JJ)=A(J)*SCLFT
 640  CONTINUE   
      NN3=N3+1
      DO 650 J=NN3,N4
         XJ=NN3-J
         YJ=Y4*XJ
         A(J)=.99999*EXP(YJ)*Y3
         JJ=J+N11
         I(JJ)=A(J)*SCLFT
 650     CONTINUE
      NN4=N1+N4
      NN6=N11+N6
      DO 660 J=NN4,NN6
         I(JJ)=0
 660  CONTINUE
      RETURN
      END

      SUBROUTINE GEN7
      DIMENSION I(15000),P(100),IP(21),A(7000)
      COMMON I,P/PARM/IP
      EQUIVALENCE(I,A)
      SCLFT=IP(15)
      N1=IP(2)+(IFIX(P(4))-1)*IP(6)
      N2=N1+IP(6)-1
      DO 100 K=N1,N2
         A(K)=0.0 
 100  CONTINUE
C     IF(P(5)) 200,300,250
      IF(P(5) == 0) GOTO 300
      IF(P(5) > 0) GOTO  250
 200  XN=P(5)*ALOG(2.)/511.
      DO 205 J=N1,N2
      XJ=J-N1
      YJ=XN*XJ
      A(J)=EXP(YJ)*.9999
      I(J)=A(J)*SCLFT
 205  CONTINUE
      GOTO 500
 250  XN=P(5)*ALOG(2.)/511.
      DO 255 J=N1,N2
      XJ=J-N1-511
      YJ=XN*XJ
      A(J)=EXP(YJ)*.999999
      I(J)=EXP(YJ)*.999999
 255  CONTINUE
      GOTO 500
 300  CONTINUE
      DO 325 J=N1,N2
      XJ=J-N1+1
      YJ=(6.2832*(XJ-256.5))/511.
      ZJ=ALOG(.008)*(1.-COS(YJ))
      A(J)=EXP(ZJ)*.99999
      I(J)=A(J)*SCLFT
 325  CONTINUE
 500  RETURN
      END

      SUBROUTINE GEN8
      DIMENSION I(15000),P(100),IP(21),A(7000)
      COMMON I,P/PARM/IP
      EQUIVALENCE(I,A)
      SCLFT=IP(15)
      N1=IP(2)+(IFIX(P(4))-1)*IP(6)
      N2=N1+IP(6)-1
      DO 100 K=N1,N2
        A(K)=0.0 
 100  CONTINUE
C     IF(P(5)) 200,250,300
      IF(P(5) == 0) GOTO 250
      IF(P(5) > 0) GOTO 300
 200  CONTINUE
      XM=-P(5)
      DO 225 J=N1,N2
         XJ=J-N1+1
         YJ=(6.2832*(XJ-44.))/170.
         ZJ = ALOG(.008)*(1.-SIN(YJ))*.5*XM
         A(J)=EXP(ZJ)
         I(J)=A(J)*SCLFT
 225     CONTINUE
      GOTO 500
 250  CONTINUE
      XM=P(6)
      IF(XM.EQ.0)XM=1
      DO 275 J=N1,N2
         XJ=J-N1+1
         YJ=(6.2832*(XJ-256.5))/511.
         ZJ=ALOG(.008)*(1.-COS(YJ))*.5*XM
         A(J)=EXP(ZJ)
         I(J)=A(J)*SCLFT
 275     CONTINUE
      GOTO 500
 300  CONTINUE
      XM=P(5)
      DO 325 J=N1,N2
         XJ=J-N1+1
         YJ=(6.2832*(XJ-65.))/256.
         ZJ=ALOG(.008)*(1.-SIN(YJ))*.5*XM
         A(J)=EXP(ZJ)
         I(J)=A(J)*SCLFT
 325     CONTINUE
 500     RETURN
         END
C     **** DUMMY SUBROUTINES ****
      
      
c      SUBROUTINE FROUT3(IDSK)
C     TERMINATE OUTPUT
c      INTEGER PEAK
c      COMMON I(15000),P(100)/PARM/IP(21)/FINOUT/PEAK,NRSOR
c      K=IP(10)
c      L=IP(10)+IP(14)-1
c      DO 1 J=K,L
c         I(J)=0
c 1       CONTINUE
c      CALL SAMOUT(IDSK,IP(14))
C     REWIND NWRITE
C     WRITE(6,10) PEAK,NRSOR
C      PRINT 10,PEAK,NRSOR
C 10   FORMAT ('0PEAK AMPLITUDE WAS',I8/'0NUMBER OF SAMPLES OUT OF RANGE WAS',I8)
C     CALL EXIT
c      IF(IDSK.LT.0)CALL EXIT
c      J=IP(10)
c      L=J+1024
c      DO 2 K=J,L
c         I(K)=0
c 2       CONTINUE
cC     WILL WRITE 1024 0'S ON DSK.
c
c       CALL FASTOUT(I(J),1024)
cc      CALL FINFILE
c       
c       close(inputfile)
c       close(outputfile)
c
c      CALL EXIT
c      END
      
      
C     DSMOUT DEBUG SAMOUT
C     *** MUSIC V ***
      
C     [page 6-4]
      
C     DEBUG SAMOUT
      SUBROUTINE SAMOUT(IDSK,N, nwrite)
c      DIMENSION IDBUF(2000),MS(3)
      DIMENSION IDBUF(4000)
C***  IDSK IS FLAG TO WRITE SAMPLES ON DSK -- PDP ****
C***  IDBUF WILL STORE PACKED SAMPLES. ****
      DIMENSION I(15000),T(10),P(100),IP(21)
      COMMON I,P/PARM/IP/FINOUT/PEAK,NRSOR
      INTEGER PEAK
C     VL 27/01/22: IOBUFSIZE constant (UG bufsize*3 to accommodate stereo)
      IOBUFSIZE = 1536
C      PRINT *, IDSK
      IF(IDSK.GE.0) GO TO 99
      N1=N
      PRINT 100,N1
 100  FORMAT(7H OUTPUTI6,8H SAMPLES)
      N2=IP(10)-1
      N3=10
      GO TO 104
 106  DO 101 L=1,10
         J=N2+L
         T(L)=FLOAT(I(J))/FLOAT(IP(12))
C     VL:25/01/22 updated for gfortran 2018           
 101  CONTINUE   
      PRINT 102, (T(K),K=1,N3)
 102  FORMAT(1H 10F11.4)
      N2=N2+10
      N1=N1-10
c      IF(N1)103,103,104
      IF(N1.gt.0) go to 104
c 103  RETURN
      RETURN
c 104  IF(N1-10)105,106,106
 104  IF(N1.ge.10) go to 106
c 105  N3=N1
      N3=N1
      GO TO 106
      
 99   J=IDSK+1
      M1=IP(10)
      M2=0
      ISC=IP(12)
      IDSK=IDSK+N
c      PRINT *, M1, J
C     COUNTS SAMPLES TO DATE
      DO 1 K=J,IDSK
         N1=I(M1+M2)/ISC
         IF(N1.GT.PEAK)PEAK=N1
         IDBUF(K)=N1
         M2=M2+1   
C     VL:25/01/22 updated for gfortran 2018           
 1    CONTINUE
C     PRINT *, M2, M1+M2
      IF(IDSK.LT.IOBUFSIZE)RETURN
         
      CALL FASTOUT(IDBUF(1), IOBUFSIZE, nwrite)

c      KL=0
c      DO 2 K=1,IOBUFSIZE,3
c         KL=KL+1
c         KJ=K-1
c         MS(1)=IDBUF(K)
c         IF(MS(1).EQ.2048) MS(1)=2047
cC     A 2048 IN THE 12 LEFT HAND BITS CREATES PROBLEMS
c         DO 3 L=2,3
c            MS(L)=IDBUF(KJ+L)
c 3          IF(MS(L).LT.0) MS(L)=4096+MS(L)
c 2       IDBUF(KL)=MS(3)+MS(2)*4096+MS(1)*16777216
cC     PACKS 3 SMPLS TO A 36-BIT WORD. 4096=2**12, 16---=2**24.
cC     MS(1) HAS LEFT HAND 12 BITS; MS(2), MIDDLE 12 BITS; MS(3), RIGHT 12.
cC     NEGATIVE NUMBERS RUN FROM 4096(I.E. -1) TO 2049(I.E. -2048).
c
c      CALL FASTOUT(IDBUF(1),256, nwrite)
c
      J=IDSK-IOBUFSIZE
      IF(J.LT.1) GO TO 4
      DO 5 K=1,J
         IDBUF(K)=IDBUF(IOBUFSIZE+K)
C     VL:25/01/22 updated for gfortran 2018           
 5    CONTINUE
                  
C     [page 6-5]
                  
 4    IDSK=J
      RETURN
      END

      SUBROUTINE FASTOUT(IARR, N, nwrite)

      DIMENSION I(15000),P(100),IP(21)
      COMMON I,P/PARM/IP/FINOUT/PEAK,NRSOR

      DIMENSION IARR(N)

      ICTR=IP(21)
c      print *, 'output ', N, ICTR

      do 55 K=1,N
         SAMPLE=IARR(k)*0.000488
c 1/2048
         ICTR = ICTR+1
         WRITE(nwrite, rec=ICTR) SAMPLE
C     VL:25/01/22 updated for gfortran 2018  
 55   CONTINUE   

         IP(21)=ICTR

c      WRITE(nwrite) xarr
c (XARR(J), J=1,N)
      end
      
C     ERROR1 GENERAL ERROR ROUTINE
C     *** MUSIC V ***
      SUBROUTINE ERROR3(I)
      PRINT 100,I
 100  FORMAT(' ERROR OF TYPE',I5)
      RETURN
      END
      
      
      
      
      

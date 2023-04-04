       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRDCGC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/04/88.
       SECURITY.      *************************************************
                      *                                               *
                      *   Calculo de validade de C.G.C.               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 SALVA-CGC                PIC  9(014) VALUE ZERO.
           05 TESTE-77                 PIC  9(001) VALUE ZERO.
           05 LIXO                     PIC  9(006) VALUE ZERO.
           05 DV                       PIC  9(006) VALUE ZERO.
           05 RESTO                    PIC  9(002) VALUE ZERO.
           05 FILLER REDEFINES RESTO.
              10 R-1                   PIC  9(001).
              10 R-2                   PIC  9(001).

       LINKAGE SECTION.

       01  PARAMETROS-GRDCGC.
           05 CGC                         PIC  9(014).
              88 EXCESSAO VALUE 62580000000084
                                78408606000151
                                92875673000163.
           05 FILLER REDEFINES CGC.
              10 CGC-01                   PIC  9(001).
              10 CGC-02                   PIC  9(001).
              10 CGC-03                   PIC  9(001).
              10 CGC-04                   PIC  9(001).
              10 CGC-05                   PIC  9(001).
              10 CGC-06                   PIC  9(001).
              10 CGC-07                   PIC  9(001).
              10 CGC-08                   PIC  9(001).
              10 CGC-09                   PIC  9(001).
              10 CGC-10                   PIC  9(001).
              10 CGC-11                   PIC  9(001).
              10 CGC-12                   PIC  9(001).
              10 CGC-13                   PIC  9(002).
              10 FILLER REDEFINES CGC-13.
                 15 CGC-14                PIC  9(001).
                 15 CGC-15                PIC  9(001).
           05 GRDCGC-RETORNO.
              10 RETORNO-1                PIC  X(001).
              10 RETORNO-2                PIC  X(001).

       PROCEDURE DIVISION USING PARAMETROS-GRDCGC.

       010-PROCESSAMENTO.

           IF   GRDCGC-RETORNO EQUAL "77"
                MOVE CGC  TO SALVA-CGC
                MOVE 1    TO TESTE-77
                EXIT PROGRAM
           ELSE
                IF  TESTE-77 EQUAL 1
                    MOVE SALVA-CGC TO CGC
                    MOVE 0         TO TESTE-77
                    MOVE "78"      TO GRDCGC-RETORNO
                    EXIT PROGRAM.

           MOVE "11" TO GRDCGC-RETORNO

           COMPUTE DV  = CGC-01 *  5
                       + CGC-02 *  4
                       + CGC-03 *  3
                       + CGC-04 *  2
                       + CGC-05 *  9
                       + CGC-06 *  8
                       + CGC-07 *  7
                       + CGC-08 *  6
                       + CGC-09 *  5
                       + CGC-10 *  4
                       + CGC-11 *  3
                       + CGC-12 *  2

           DIVIDE 11 INTO DV GIVING LIXO REMAINDER RESTO

           IF   RESTO EQUAL 0 OR 1
                MOVE 0 TO RESTO
           ELSE
                COMPUTE RESTO = RESTO - 11.

           IF   CGC-14 EQUAL RESTO
                MOVE "0" TO RETORNO-1
                COMPUTE DV  = CGC-01 *  6
                            + CGC-02 *  5
                            + CGC-03 *  4
                            + CGC-04 *  3
                            + CGC-05 *  2
                            + CGC-06 *  9
                            + CGC-07 *  8
                            + CGC-08 *  7
                            + CGC-09 *  6
                            + CGC-10 *  5
                            + CGC-11 *  4
                            + CGC-12 *  3
                            + CGC-14 *  2
                DIVIDE 11 INTO DV GIVING LIXO REMAINDER RESTO
                IF   RESTO EQUAL 0 OR 1
                     MOVE 0 TO RESTO
                     IF   CGC-15 EQUAL RESTO
                          MOVE "0" TO RETORNO-2
                     ELSE
                          NEXT SENTENCE
                ELSE
                     COMPUTE RESTO = RESTO - 11
                     IF   CGC-15 EQUAL RESTO
                          MOVE "0" TO RETORNO-2.

           IF   CGC EQUAL ZERO
           OR   CGC EQUAL 99999999999
                MOVE "99" TO GRDCGC-RETORNO.

           IF   EXCESSAO
                MOVE "00" TO GRDCGC-RETORNO.

       010-99-FIM. EXIT PROGRAM.

       END PROGRAM GRDCGC.

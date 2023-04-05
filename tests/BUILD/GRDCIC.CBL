       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRDCIC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/04/88.
       SECURITY.      *************************************************
                      *                                               *
                      *   Calculo de validade de C.I.C.               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 LIXO                     PIC  9(006) VALUE ZERO.
           05 DV                       PIC  9(006) VALUE ZERO.
           05 RESTO                    PIC  9(002) VALUE ZERO.

       LINKAGE SECTION.

       01  PARAMETROS-GRDCIC.
           05 CIC                         PIC  9(011).
              88 CIC-INVALIDO             VALUE 11111111111
                                                22222222222
                                                33333333333
                                                44444444444
                                                55555555555
                                                66666666666
                                                77777777777
                                                88888888888
                                                99999999999
                                                00000000000.
           05 FILLER REDEFINES CIC.
              10 CIC-01                   PIC  9(001).
              10 CIC-02                   PIC  9(001).
              10 CIC-03                   PIC  9(001).
              10 CIC-04                   PIC  9(001).
              10 CIC-05                   PIC  9(001).
              10 CIC-06                   PIC  9(001).
              10 CIC-07                   PIC  9(001).
              10 CIC-08                   PIC  9(001).
              10 CIC-09                   PIC  9(001).
              10 CIC-10                   PIC  9(002).
              10 FILLER REDEFINES CIC-10.
                 15 CIC-11                PIC  9(001).
                 15 CIC-12                PIC  9(001).
           05 GRDCIC-RETORNO.
              10 RETORNO-1                PIC  X(001).
              10 RETORNO-2                PIC  X(001).

       PROCEDURE DIVISION USING PARAMETROS-GRDCIC.

       010-PROCESSAMENTO.

           IF   CIC-INVALIDO
                MOVE "99" TO GRDCIC-RETORNO
                EXIT PROGRAM.

           MOVE "11" TO GRDCIC-RETORNO

           COMPUTE DV  = CIC-01 * 10
                       + CIC-02 *  9
                       + CIC-03 *  8
                       + CIC-04 *  7
                       + CIC-05 *  6
                       + CIC-06 *  5
                       + CIC-07 *  4
                       + CIC-08 *  3
                       + CIC-09 *  2

           DIVIDE 11 INTO DV GIVING LIXO REMAINDER RESTO

           IF   RESTO EQUAL 0 OR 1
                MOVE 0 TO RESTO
           ELSE
                COMPUTE RESTO = RESTO - 11.

           IF   CIC-11 EQUAL RESTO
                MOVE "0" TO RETORNO-1
                COMPUTE DV  = CIC-01 * 11
                            + CIC-02 * 10
                            + CIC-03 *  9
                            + CIC-04 *  8
                            + CIC-05 *  7
                            + CIC-06 *  6
                            + CIC-07 *  5
                            + CIC-08 *  4
                            + CIC-09 *  3
                            + CIC-11 *  2
                DIVIDE 11 INTO DV GIVING LIXO REMAINDER RESTO
                IF   RESTO EQUAL 0 OR 1
                     MOVE 0 TO RESTO
                     IF   CIC-12 EQUAL RESTO
                          MOVE "0" TO RETORNO-2
                     ELSE
                          NEXT SENTENCE
                ELSE
                     COMPUTE RESTO = RESTO - 11
                     IF   CIC-12 EQUAL RESTO
                          MOVE "0" TO RETORNO-2.

       010-99-FIM. EXIT PROGRAM.

       END PROGRAM GRDCIC.

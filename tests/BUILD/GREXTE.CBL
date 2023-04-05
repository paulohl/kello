      $ Set OldIndex
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
         PROGRAM-ID.           GREXTE.
         AUTHOR.               COBOLware Services Ltda.
         DATE-WRITTEN.         11/09/87.
         SECURITY.             ESCREVE O VALOR RECEBIDO POR EXTENSO.

      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
        CONFIGURATION SECTION.
         SPECIAL-NAMES.        DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*

       WORKING-STORAGE SECTION.

       01  W-INDICADORES.
           03 W-IND-2                  PIC 9(02)    VALUE ZEROS.
           03 W-IND-4                  PIC 9(03)    VALUE ZEROS.
           03 W-IND                    PIC 9(03)    VALUE ZEROS.
           03 IND-2                    PIC 9(02)    VALUE ZEROS.
           03 IND-5                    PIC 9(03)    VALUE ZEROS.
           03 W-CONT-DIG               PIC 9(02)    VALUE ZEROS.
           03 W-TAM-AUX                PIC 9(03)    VALUE ZEROS.
           03 W-TAM-SAI                PIC 9(03)    VALUE 46.
           03 W-FLAG                   PIC 9(02)    VALUE ZEROS.
              88 CENTENA                            VALUE 01 04 07 10.
              88 DEZENA                             VALUE 02 05 08 11.
              88 UNIDADE                            VALUE 03 06 09 12.

       01  W-AREAS-AUXILIARES.
           03 W-NOME-AUX.
              05 W-LET-AUX             PIC X(01)    OCCURS 27 TIMES
                                                    INDEXED BY IND-3.

           03 W-NOME-SAI.
              05 W-LET-SAI             PIC X(01)    OCCURS 230 TIMES
                                                    INDEXED BY IND-4.
           03 W-LINHA-FIM.
              05 W-LET-FIM             PIC X(01)    OCCURS  46 TIMES
                                                    INDEXED BY IND-6.
           03 W-TAB-TAM.
              05 W-TAM-TB              PIC 9(03)    OCCURS  20 TIMES
                                                    INDEXED BY IND-TB.
       01  W-NUMERO-TOTAL.
           03  W-NUMERO.
               05 W-DIG                PIC 9(01)    OCCURS  12 TIMES
                                                    INDEXED BY IND-1.
           03 W-CENT                   PIC 9(01)    OCCURS  02 TIMES
                                                    INDEXED BY IND-C.

      *-----------------------  TABELAS DE VALORES  --------------------

       01  W-TAB-NOME-UNI.
           03 FILLER                   PIC X(06) VALUE "um".
           03 FILLER                   PIC X(06) VALUE "dois".
           03 FILLER                   PIC X(06) VALUE "tres".
           03 FILLER                   PIC X(06) VALUE "quatro".
           03 FILLER                   PIC X(06) VALUE "cinco".
           03 FILLER                   PIC X(06) VALUE "seis".
           03 FILLER                   PIC X(06) VALUE "sete".
           03 FILLER                   PIC X(06) VALUE "oito".
           03 FILLER                   PIC X(06) VALUE "nove".

       01  W-TABELA-UNIDADE            REDEFINES W-TAB-NOME-UNI.
           03 W-NOME-UNI               PIC X(06) OCCURS 09 TIMES.

       01  W-TAB-NOME-DEZ.
           03 FILLER                   PIC X(11) VALUE "dez".
           03 FILLER                   PIC X(11) VALUE "vinte e".
           03 FILLER                   PIC X(11) VALUE "trinta e".
           03 FILLER                   PIC X(11) VALUE "quarenta e".
           03 FILLER                   PIC X(11) VALUE "cinquenta e".
           03 FILLER                   PIC X(11) VALUE "sessenta e".
           03 FILLER                   PIC X(11) VALUE "setenta e".
           03 FILLER                   PIC X(11) VALUE "oitenta e".
           03 FILLER                   PIC X(11) VALUE "noventa e".

       01  W-TABELA-DEZENA             REDEFINES W-TAB-NOME-DEZ.
           03 W-NOME-DEZ               PIC X(11) OCCURS 09 TIMES.
      *
       01  W-TAB-NOME-CEN.
           03 FILLER                   PIC X(14) VALUE "cento e".
           03 FILLER                   PIC X(14) VALUE "duzentos e".
           03 FILLER                   PIC X(14) VALUE "trezentos e".
           03 FILLER                   PIC X(14) VALUE "quatrocentos e".
           03 FILLER                   PIC X(14) VALUE "quinhentos e".
           03 FILLER                   PIC X(14) VALUE "seiscentos e".
           03 FILLER                   PIC X(14) VALUE "setecentos e".
           03 FILLER                   PIC X(14) VALUE "oitocentos e".
           03 FILLER                   PIC X(14) VALUE "novecentos e".

       01  W-TABELA-CENTENA            REDEFINES W-TAB-NOME-CEN.
           03 W-NOME-CEN               PIC X(14) OCCURS 09 TIMES.

       01  W-TAB-NOME-11-A-19.
           03 FILLER                   PIC X(09) VALUE "onze".
           03 FILLER                   PIC X(09) VALUE "doze".
           03 FILLER                   PIC X(09) VALUE "treze".
           03 FILLER                   PIC X(09) VALUE "quatorze".
           03 FILLER                   PIC X(09) VALUE "quinze".
           03 FILLER                   PIC X(09) VALUE "dezesseis".
           03 FILLER                   PIC X(09) VALUE "dezesete".
           03 FILLER                   PIC X(09) VALUE "dezoito".
           03 FILLER                   PIC X(09) VALUE "dezenove".

       01 W-TABELA-11-A-19             REDEFINES W-TAB-NOME-11-A-19.
          03 W-NOME-11-A-19            PIC X(09) OCCURS 09 TIMES.

       01 W-AREA-AUX.
          03 W-H                       PIC X(01)    VALUE "H".
          03 W-NOME                    PIC X(230)   VALUE SPACES.

       LINKAGE SECTION.
       01  L-PARAMETRO.
           03 L-VALOR                  PIC 9(12)V99.
           03 VALOR-ALFA REDEFINES L-VALOR.
              04 FILLER                PIC X(12).
              04 CTS                   PIC X(02).
           03 L-EXTENSO.
              04 B-EXTENSO OCCURS 231  PIC 9(02) COMP-X.

      *---------------------------------------------------------------*
       PROCEDURE DIVISION USING L-PARAMETRO.
      *---------------------------------------------------------------*

      *                 PRIMEIRO NIVEL DO PROGRAMA                    *


       A01-PROCURA-DIGITO.
           MOVE VALOR-ALFA TO W-NUMERO-TOTAL.
      *    SEARCH ALL W-DIG AT END SET IND-1 TO 13
      *                       WHEN W-DIG(IND-1) NOT = 0 NEXT SENTENCE.

       A02-INICIALIZAR.
           SET IND-3, IND-4, IND-5 TO 0.
           SET IND-1, IND-C        TO 1.
           MOVE SPACES TO W-NOME-AUX, W-NOME-SAI, W-LINHA-FIM.
           PERFORM B01-PROCESSAMENTO
                   UNTIL W-NUMERO = ZEROS OR IND-1      >  12.
           IF W-CENT(IND-C)     NOT = 0 OR
              W-CENT(IND-C + 1) NOT = 0
               PERFORM B05-CENTAVOS
               PERFORM C06-NOME-CENTAVO.

       A03-PREPARA-SAIDA.
           MOVE SPACES TO L-EXTENSO.
             IF W-LET-SAI(1) = "U" OR "u"
                MOVE W-NOME-SAI    TO W-NOME
                MOVE W-AREA-AUX    TO L-EXTENSO
              ELSE
                MOVE W-NOME-SAI    TO L-EXTENSO.

       A04-FINALIZAR.
           IF   B-EXTENSO (1) GREATER 96 AND LESS 123
                SUBTRACT 32 FROM B-EXTENSO (1).

           EXIT PROGRAM.
      *                    SEGUNDO NIVEL DO PROGRAMA                   /

       B01-PROCESSAMENTO.
           MOVE W-DIG(IND-1) TO IND-2.
           SET  W-FLAG       TO IND-1.
           IF CENTENA AND IND-2 > 0 PERFORM C01-CENTENA           ELSE
           IF DEZENA  AND IND-2 > 0 PERFORM C02-DEZENA            ELSE
           IF IND-2 > ZERO        MOVE W-NOME-UNI(IND-2) TO W-NOME-AUX.
           SET IND-3 TO 1.
           PERFORM D01-MOVER-LETRA
                   UNTIL W-LET-AUX(IND-3)     = SPACES
                     AND W-LET-AUX(IND-3 + 1) = SPACES
                      OR W-DIG(IND-1)         = ZERO.
           IF W-DIG(IND-1) > ZERO SET IND-4 UP BY 1.
           SET W-FLAG TO IND-1.
           IF CENTENA
                IF W-DIG(IND-1)     > 1 AND W-DIG(IND-1 + 1) = 0 AND
                   W-DIG(IND-1 + 2) = 0
                     SET IND-4 DOWN BY 2.
           IF DEZENA
                IF W-DIG(IND-1) > 1 AND W-DIG(IND-1 + 1) = 0
                     SET IND-4 DOWN BY 2.
           IF UNIDADE
                PERFORM C03-NOME-UNIDADE.
           SET IND-1 UP BY 1.

       B05-CENTAVOS.
      *    TRATAMENTO DA PARTE DE CENTAVOS A SER ESCRITA POR EXTENSO
           IF W-CENT(IND-C) = 0
                MOVE W-CENT(IND-C + 1) TO IND-5
                MOVE W-NOME-UNI(IND-5) TO W-NOME-AUX
              ELSE
                IF W-CENT(IND-C) = 1
                     IF W-CENT(IND-C + 1) =  0
                          MOVE 1 TO IND-5
                          MOVE W-NOME-DEZ(IND-5) TO W-NOME-AUX
                        ELSE
                          MOVE W-CENT(IND-C + 1) TO IND-5
                          MOVE W-NOME-11-A-19(IND-5) TO W-NOME-AUX
                   ELSE
                     MOVE W-CENT(IND-C) TO IND-5
                     MOVE W-NOME-DEZ(IND-5) TO W-NOME-AUX
                     SET IND-3 TO 1
                     PERFORM D01-MOVER-LETRA UNTIL
                             W-LET-AUX(IND-3)     = SPACES AND
                             W-LET-AUX(IND-3 + 1) = SPACES.
           IF W-CENT(IND-C) > 1
                IF W-CENT(IND-C + 1) = 0
                     SET IND-4 DOWN BY 2
                   ELSE
                     MOVE W-CENT(IND-C + 1) TO IND-5
                     MOVE W-NOME-UNI(IND-5) TO W-NOME-AUX.
           IF W-CENT(IND-C) > 1
                IF W-CENT(IND-C + 1) = 0
                    NEXT SENTENCE
                   ELSE
                     SET IND-3 TO 1
                     SET IND-4 UP BY 1
                     PERFORM D01-MOVER-LETRA UNTIL
                             W-LET-AUX(IND-3)     = SPACES AND
                             W-LET-AUX(IND-3 + 1) = SPACES
              ELSE
                SET IND-3 TO 1
                PERFORM D01-MOVER-LETRA UNTIL
                        W-LET-AUX(IND-3)     = SPACES AND
                        W-LET-AUX(IND-3 + 1) = SPACES.

      *----------- TERCEIRO NIVEL DO PROGRAMA --------------------------

       C01-CENTENA.
      *    PROCURA NA TABELA CENTENA O VALOR POR EXTENSO CORRESPONDENTE

           IF W-DIG(IND-1) = 1 AND W-DIG(IND-1 + 1) = 0
                               AND W-DIG(IND-1 + 2) = 0
                MOVE "cem"                  TO W-NOME-AUX
              ELSE
                MOVE W-NOME-CEN(IND-2)      TO W-NOME-AUX.

       C02-DEZENA.
      *    PROCURA NA TABELA DEZENA O VALOR POR EXTENSO CORRESPONDENTE

           IF W-DIG(IND-1) = 1 AND W-DIG(IND-1 + 1) NOT = ZERO
                SET IND-1 UP BY 1
                MOVE W-DIG(IND-1) TO IND-2
                SET IND-5 TO IND-2
                MOVE W-NOME-11-A-19(IND-5)  TO W-NOME-AUX
              ELSE
                MOVE W-NOME-DEZ(IND-2)      TO W-NOME-AUX.

       C03-NOME-UNIDADE.
           SET W-IND     TO IND-1.
           SET IND-1  UP BY 1.
           MOVE ZEROS TO W-CONT-DIG.
           PERFORM D02-CONT-DIG UNTIL IND-1 > 12.
           SET IND-1 TO W-IND.
           IF IND-1 = 3
                IF IND-2 = 1 AND W-DIG(IND-1 - 1) = 0
                             AND W-DIG(IND-1 - 2) = 0
                     IF W-CONT-DIG = ZERO
                          MOVE "bilhao de"  TO W-NOME-AUX
                        ELSE
                          MOVE "bilhao,"    TO W-NOME-AUX
                   ELSE
                    IF W-DIG(IND-1) = 0 AND W-DIG(IND-1 - 1) = 0
                                    AND W-DIG(IND-1 - 2) = 0
                     MOVE SPACES            TO W-NOME-AUX
                   ELSE
                     IF W-CONT-DIG = ZERO
                          MOVE "bilhoes de" TO W-NOME-AUX
                        ELSE
                     IF W-CONT-DIG = 1
                          MOVE "bilhoes e"  TO W-NOME-AUX
                        ELSE
                          MOVE "bilhoes,"   TO W-NOME-AUX.

           IF IND-1 = 6
                IF IND-2 = 1 AND W-DIG(IND-1 - 1) = ZERO
                             AND W-DIG(IND-1 - 2) = ZERO
                     IF W-CONT-DIG = ZERO
                          MOVE "milhao de"  TO W-NOME-AUX
                        ELSE
                     IF W-CONT-DIG = 1
                          MOVE "milhao e"   TO W-NOME-AUX
                        ELSE
                          MOVE "milhao,"    TO W-NOME-AUX
                   ELSE
                IF W-DIG(IND-1) = 0 AND W-DIG(IND-1 - 1) = 0
                                    AND W-DIG(IND-1 - 2) = 0
                     MOVE SPACES            TO W-NOME-AUX
                   ELSE
                     IF W-CONT-DIG = ZERO
                          MOVE "milhoes de" TO W-NOME-AUX
                        ELSE
                     IF W-CONT-DIG = 1
                          MOVE "milhoes e"  TO W-NOME-AUX
                        ELSE
                          MOVE "milhoes,"   TO W-NOME-AUX.
           IF IND-1 = 9
                IF W-DIG(IND-1) = 0 AND W-DIG(IND-1 - 1) = 0
                                    AND W-DIG(IND-1 - 2) = 0
                     MOVE SPACES            TO W-NOME-AUX
                   ELSE
                     IF W-CONT-DIG = ZERO
                          MOVE "mil"        TO W-NOME-AUX
                        ELSE
                     IF W-CONT-DIG = 1
                          MOVE "mil e"      TO W-NOME-AUX
                        ELSE
                          MOVE "mil,"       TO W-NOME-AUX.
           IF IND-1 = 12
           IF W-CENT(IND-C) = 0 AND W-CENT(IND-C + 1) = 0
                IF IND-2 = 1 AND IND-4 < 5
                     MOVE "Real"    TO W-NOME-AUX
                   ELSE
                     MOVE "Reais"  TO W-NOME-AUX
              ELSE
                IF IND-2 = 1 AND IND-4 < 5
                     MOVE "Real e"       TO W-NOME-AUX
                   ELSE
                     MOVE "Reais e"     TO W-NOME-AUX.
           SET IND-3 TO 1.
           PERFORM D01-MOVER-LETRA
                   UNTIL W-LET-AUX(IND-3)     = SPACES
                     AND W-LET-AUX(IND-3 + 1) = SPACES.
           IF W-NOME-AUX NOT = SPACES  SET IND-4 UP BY 1.


       C06-NOME-CENTAVO.
           IF W-CENT(IND-C) = 0 AND W-CENT(IND-C + 1) = 1
                MOVE " centavo" TO W-NOME-AUX
              ELSE
                MOVE " centavos" TO W-NOME-AUX.
           SET IND-3 TO 1
           PERFORM D01-MOVER-LETRA UNTIL
                   W-LET-AUX(IND-3)     = SPACES AND
                   W-LET-AUX(IND-3 + 1) = SPACES.
      *------------- QUARTO NIVEL DO PROGRAMA --------------------------

       D01-MOVER-LETRA.
           SET  IND-4 UP BY 1.
           MOVE W-LET-AUX(IND-3) TO W-LET-SAI(IND-4).
           SET  IND-3 UP BY 1.

       D02-CONT-DIG.
           IF W-DIG(IND-1) > ZERO
                ADD 1 TO W-CONT-DIG
                IF W-IND NOT = 9
                     IF IND-1 < 7
                          SET IND-1 TO 6
                        ELSE
                     IF IND-1 < 10
                          SET IND-1 TO 9
                        ELSE
                          SET IND-1 TO 12
                   ELSE
                     IF IND-1 = 11 AND W-DIG(IND-1 - 1) = ZERO
                          SET IND-1 TO 12.
           SET IND-1 UP BY 1.

       END PROGRAM GREXTE.


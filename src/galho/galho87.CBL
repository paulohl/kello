       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO84.
      *AUTORA: ALFREDO SAVIOLLI NETO
      *DATA: 23-05-2007
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY CHPX010.

           SELECT CHD910 ASSIGN TO PATH-CHD910
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CH91 = DATA-MOVTO-CH91 SEQ-CH91
                  ALTERNATE RECORD KEY ALT1-CH91 =
                     NOME-CH91, PORTADOR-CH91 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT2-CH91 = SITUACAO-CH91
                     DATA-VENCTO-CH91 PORTADOR-CH91 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT3-CH91 = DATA-MOVTO-CH91
                     VENDEDOR-CH91 SEQ-CH91 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT4-CH91 = COD-COMPL-CH91
                     DATA-VENCTO-CH91 WITH DUPLICATES
                  STATUS IS ST-CHD910.

           COPY CHPX099.

           SELECT CHD999 ASSIGN TO PATH-CHD999
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CH999 = DATA-MOVTO-CH999 SEQ-CH999
                  ALTERNATE RECORD KEY ALT1-CH999 =
                     NOME-CH999, PORTADOR-CH999 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT2-CH999 = SITUACAO-CH999
                     DATA-VENCTO-CH999 PORTADOR-CH999 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT3-CH999 = DATA-MOVTO-CH999
                     VENDEDOR-CH999 SEQ-CH999 WITH DUPLICATES
                  STATUS IS ST-CHD999.

           COPY CGPX004.

           SELECT CGD904 ASSIGN TO PATH-CGD904
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CODIGO-CG94
                  ALTERNATE RECORD KEY IS COD-VENDEDOR-CG94
                            WITH DUPLICATES
                  STATUS IS ST-CGD904.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CHPW010.

       FD  CHD910.
       01  REG-CHD910.
           05  DATA-MOVTO-CH91          PIC 9(8).
           05  SEQ-CH91                 PIC 9(4).
           05  COD-COMPL-CH91.
               10  CLASS-CLIENTE-CH91   PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CH91         PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  NOME-CH91                PIC X(30).
           05  CIDADE-CH91              PIC X(19).
           05  LOTE-CH91                PIC X(01).
           05  CONTA-CORR-CH91          PIC 9(8)   COMP-3.
           05  DV-CONTA-CH91            PIC X.
           05  BANCO-CH91               PIC 9(4)   COMP-3.
           05  AGENCIA-CH91             PIC 9(5)   COMP-3.
           05  DV-AGENCIA-CH91          PIC X.
           05  COMPENSACAO-CH91         PIC 9(3).
           05  CPF-CH91                 PIC 9(11)  COMP-3.
           05  NR-CHEQUE-CH91           PIC X(7).
           05  OUTRO-DOCTO-CH91         PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  DATA-VENCTO-CH91         PIC 9(8).
           05  PORTADOR-CH91            PIC 9999.
           05  CARTEIRA-CH91            PIC 9.
      *    CARTEIRA-CH10  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CH91        PIC 99.
           05  SITUACAO-CH91            PIC 9.
      *    SITUACAO = 0-OK  2-RECEBIDO  3-ESTONADO  4-CANCELADO
      *               5-DEVOLVIDO   6-PROBLEMATICO
           05  CODREDUZ-APUR-CH91       PIC 9(3).
           05  VALOR-CH91               PIC 9(8)V99.
           05  VENDEDOR-CH91            PIC 9(6)   COMP-3.
           05  DIGITADOR-CH91           PIC X(5).
           05  SEQ-CAIXA-CH91           PIC 9(3).
           05  NR-NOTA-FISCAL-CH91      PIC X(10).
           05  DATA-NTA-FISCAL-CH91     PIC 9(8)   COMP-3.
           05  ORIGEM-CH91              PIC 99.

           COPY CHPW099.

       FD  CHD999.
       01  REG-CHD999.
           05  DATA-MOVTO-CH999          PIC 9(8).
           05  SEQ-CH999                 PIC 9(4).
           05  COD-COMPL-CH999.
               10  CLASS-CLIENTE-CH999   PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CH999         PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  NOME-CH999                PIC X(30).
           05  CIDADE-CH999              PIC X(20).
           05  CONTA-CORR-CH999          PIC 9(8)   COMP-3.
           05  DV-CONTA-CH999            PIC X.
           05  BANCO-CH999               PIC 9(4)   COMP-3.
           05  AGENCIA-CH999             PIC 9(5)   COMP-3.
           05  DV-AGENCIA-CH999          PIC X.
           05  COMPENSACAO-CH999         PIC 9(3).
           05  CPF-CH999                 PIC 9(11)  COMP-3.
           05  NR-CHEQUE-CH999           PIC X(7).
           05  OUTRO-DOCTO-CH999         PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  DATA-VENCTO-CH999         PIC 9(8).
           05  PORTADOR-CH999            PIC 9999.
           05  CARTEIRA-CH999            PIC 9.
      *    CARTEIRA-CH99  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CH999        PIC 99.
           05  SITUACAO-CH999            PIC 9.
      *    SITUACAO = 0-OK  2-RECEBIDO  3-ESTONADO  4-CANCELADO
      *               5-DEVOLVIDO   6-PROBLEMATICO
           05  CODREDUZ-APUR-CH999       PIC 9(3).
           05  VALOR-CH999               PIC 9(8)V99.
           05  VENDEDOR-CH999            PIC 9(6)   COMP-3.
           05  DIGITADOR-CH999           PIC X(5).
           05  SEQ-CAIXA-CH999           PIC 9(3).
           05  NR-NOTA-FISCAL-CH999      PIC X(10).
           05  DATA-NTA-FISCAL-CH999     PIC 9(8)   COMP-3.
           05  ORIGEM-CH999              PIC 99.
           05  USUARIO-EXCLUSAO-CH999    PIC X(5).
           05  DATA-EXCLUSAO-CH999       PIC 9(08).
           05  HORA-EXCLUSAO-CH999       PIC 9(06).
           05  FILLER                    PIC X(30).

           COPY CGPW004.

       FD  CGD904.
       01  REG-CGD904.
           05  CODIGO-CG94             PIC 9(6).
           05  DEPTO-CG94              PIC 9(2).
           05  CONTA-APUR-RED-CG94     PIC 9(3).
           05  BANCO-CG94              PIC 9(3).
           05  NOME-BANCO-CG94         PIC X(15).
           05  AGENCIA-CG94            PIC 9(4).
           05  DIGITO-AGENCIA-CG94     PIC X.
           05  NR-CONTA-CG94           PIC 9(10).
           05  DIGITO-CONTA-CG94       PIC X.
           05  COD-VENDEDOR-CG94       PIC 9(5).

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-CHD910             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD999             PIC XX       VALUE SPACES.
           05  ST-CHD099             PIC XX       VALUE SPACES.
           05  ST-CGD004             PIC XX       VALUE SPACES.
           05  ST-CGD904             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W - flag que controla se houve erro abertura nos arquivos
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  I                     PIC 9        VALUE ZEROS.
           05  LETRA                 PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           COPY "PARAMETR".


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC

           MOVE "CHD010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010
           MOVE "CHD910"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD910
           MOVE "CHD099"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD099
           MOVE "CHD999"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD999
           MOVE "CGD004"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD004
           MOVE "CGD904"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD904


           MOVE ZEROS     TO ERRO-W.
           OPEN OUTPUT CHD010 CHD099 CGD004
           CLOSE       CHD010 CHD099 CGD004
           OPEN I-O    CHD010 CHD099 CGD004
           OPEN INPUT  CHD910 CHD999 CGD904

           CLOSE CONTROLE.


           IF ST-CHD010 = "35"
              CLOSE CHD010      OPEN OUTPUT CHD010
              CLOSE CHD010      OPEN I-O    CHD010
           END-IF.
           IF ST-CHD910 = "35"
              CLOSE CHD910      OPEN OUTPUT CHD910
              CLOSE CHD910      OPEN I-O    CHD910
           END-IF.

           IF ST-CHD099 = "35"
              CLOSE CHD099      OPEN OUTPUT CHD099
              CLOSE CHD099      OPEN I-O    CHD099
           END-IF.
           IF ST-CHD999 = "35"
              CLOSE CHD999      OPEN OUTPUT CHD999
              CLOSE CHD999      OPEN I-O    CHD999
           END-IF.

           IF ST-CGD004 = "35"
              CLOSE CGD004      OPEN OUTPUT CGD004
              CLOSE CGD004      OPEN I-O    CGD004
           END-IF.
           IF ST-CGD904 = "35"
              CLOSE CGD904      OPEN OUTPUT CGD904
              CLOSE CGD904      OPEN I-O    CGD904
           END-IF.



          DISPLAY "VOU COMECAR A CONVERSAO DOS ARQUIVOS CXD020 CBD004 CX
      -   "D900" STOP " ".

           INITIALIZE REG-CHD910
           START CHD910 KEY IS NOT LESS CHAVE-CH91 INVALID KEY
               MOVE "10" TO ST-CHD910.

           PERFORM UNTIL ST-CHD910 = "10"
               READ CHD910 NEXT AT END
                   MOVE "10" TO ST-CHD910
               NOT AT END
                   MOVE DATA-MOVTO-CH91      TO DATA-MOVTO-CH10
                   MOVE SEQ-CH91             TO SEQ-CH10
                   MOVE CLASS-CLIENTE-CH91   TO CLASS-CLIENTE-CH10
                   MOVE CLIENTE-CH91         TO CLIENTE-CH10
                   MOVE NOME-CH91            TO NOME-CH10
                   MOVE CIDADE-CH91          TO CIDADE-CH10
                   MOVE LOTE-CH91            TO LOTE-CH10
                   MOVE CONTA-CORR-CH91      TO CONTA-CORR-CH10
                   MOVE DV-CONTA-CH91        TO DV-CONTA-CH10
                   MOVE BANCO-CH91           TO BANCO-CH10
                   MOVE AGENCIA-CH91         TO AGENCIA-CH10
                   MOVE DV-AGENCIA-CH91      TO DV-AGENCIA-CH10
                   MOVE COMPENSACAO-CH91     TO COMPENSACAO-CH10
                   MOVE CPF-CH91             TO CPF-CH10
                   MOVE NR-CHEQUE-CH91       TO NR-CHEQUE-CH10
                   MOVE OUTRO-DOCTO-CH91     TO OUTRO-DOCTO-CH10
                   MOVE DATA-VENCTO-CH91     TO DATA-VENCTO-CH10
                   MOVE PORTADOR-CH91        TO PORTADOR-CH10
                   MOVE CARTEIRA-CH91        TO CARTEIRA-CH10
                   MOVE SITUACAO-TIT-CH91    TO SITUACAO-TIT-CH10
                   MOVE SITUACAO-CH91        TO SITUACAO-CH10
                   MOVE CODREDUZ-APUR-CH91   TO CODREDUZ-APUR-CH10
                   MOVE VALOR-CH91           TO VALOR-CH10
                   MOVE VENDEDOR-CH91        TO VENDEDOR-CH10
                   MOVE DIGITADOR-CH91       TO DIGITADOR-CH10
                   MOVE SEQ-CAIXA-CH91       TO SEQ-CAIXA-CH10
                   MOVE NR-NOTA-FISCAL-CH91  TO NR-NOTA-FISCAL-CH10
                   MOVE DATA-NTA-FISCAL-CH91 TO DATA-NTA-FISCAL-CH10
                   MOVE ORIGEM-CH91          TO ORIGEM-CH10

                   display "CHD010 => " reg-chd010
                   WRITE REG-CHD010 INVALID KEY
                      DISPLAY "Erro de Gravacao...CHD010" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-CHD999
           START CHD999 KEY IS NOT LESS CHAVE-CH999 INVALID KEY
               MOVE "10" TO ST-CHD999.

           PERFORM UNTIL ST-CHD999 = "10"
               READ CHD999 NEXT AT END
                   MOVE "10" TO ST-CHD999
               NOT AT END
                   MOVE DATA-MOVTO-CH999       TO DATA-MOVTO-CH99
                   MOVE SEQ-CH999              TO SEQ-CH99
                   MOVE CLASS-CLIENTE-CH999    TO CLASS-CLIENTE-CH99
                   MOVE CLIENTE-CH999          TO CLIENTE-CH99
                   MOVE NOME-CH999             TO NOME-CH99
                   MOVE CIDADE-CH999           TO CIDADE-CH99
                   MOVE CONTA-CORR-CH999       TO CONTA-CORR-CH99
                   MOVE DV-CONTA-CH999         TO DV-CONTA-CH99
                   MOVE BANCO-CH999            TO BANCO-CH99
                   MOVE AGENCIA-CH999          TO AGENCIA-CH99
                   MOVE DV-AGENCIA-CH999       TO DV-AGENCIA-CH99
                   MOVE COMPENSACAO-CH999      TO COMPENSACAO-CH99
                   MOVE CPF-CH999              TO CPF-CH99
                   MOVE NR-CHEQUE-CH999        TO NR-CHEQUE-CH99
                   MOVE OUTRO-DOCTO-CH999      TO OUTRO-DOCTO-CH99
                   MOVE DATA-VENCTO-CH999      TO DATA-VENCTO-CH99
                   MOVE PORTADOR-CH999         TO PORTADOR-CH99
                   MOVE CARTEIRA-CH999         TO CARTEIRA-CH99
                   MOVE SITUACAO-TIT-CH999     TO SITUACAO-TIT-CH99
                   MOVE SITUACAO-CH999         TO SITUACAO-CH99
                   MOVE CODREDUZ-APUR-CH999    TO CODREDUZ-APUR-CH99
                   MOVE VALOR-CH999            TO VALOR-CH99
                   MOVE VENDEDOR-CH999         TO VENDEDOR-CH99
                   MOVE DIGITADOR-CH999        TO DIGITADOR-CH99
                   MOVE SEQ-CAIXA-CH999        TO SEQ-CAIXA-CH99
                   MOVE NR-NOTA-FISCAL-CH999   TO NR-NOTA-FISCAL-CH99
                   MOVE DATA-NTA-FISCAL-CH999  TO DATA-NTA-FISCAL-CH99
                   MOVE ORIGEM-CH999           TO ORIGEM-CH99
                   MOVE USUARIO-EXCLUSAO-CH999 TO USUARIO-EXCLUSAO-CH99
                   MOVE DATA-EXCLUSAO-CH999    TO DATA-EXCLUSAO-CH99
                   MOVE HORA-EXCLUSAO-CH999    TO HORA-EXCLUSAO-CH99

                   display "CHD099 => " reg-chd099
                   WRITE REG-CHD099 INVALID KEY
                      DISPLAY "Erro de Gravacao...CHD099" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-CGD904
           START CGD904 KEY IS NOT LESS CODIGO-CG94 INVALID KEY
               MOVE "10" TO ST-CGD904.

           PERFORM UNTIL ST-CGD904 = "10"
               READ CGD904 NEXT AT END
                   MOVE "10" TO ST-CGD904
               NOT AT END
                   MOVE CODIGO-CG94         TO CODIGO-CG04
                   MOVE DEPTO-CG94          TO DEPTO-CG04
                   MOVE CONTA-APUR-RED-CG94 TO CONTA-APUR-RED-CG04
                   MOVE BANCO-CG94          TO BANCO-CG04
                   MOVE NOME-BANCO-CG94     TO NOME-BANCO-CG04
                   MOVE AGENCIA-CG94        TO AGENCIA-CG04
                   MOVE DIGITO-AGENCIA-CG94 TO DIGITO-AGENCIA-CG04
                   MOVE NR-CONTA-CG94       TO NR-CONTA-CG04
                   MOVE DIGITO-CONTA-CG94   TO DIGITO-CONTA-CG04
                   MOVE COD-VENDEDOR-CG94   TO COD-VENDEDOR-CG04

                   display "CGD004 => " reg-cgd004
                   WRITE REG-CGD004 INVALID KEY
                      DISPLAY "Erro de Gravacao...CGD004" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           CLOSE CHD010 CHD910 CHD099 CHD999 CGD004 CGD904
           STOP RUN.

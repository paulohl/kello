       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO66.
      *DATA: 12-09-2006
      *AUTOR: ALFREDO SAVIOLLI NETO
      *PROGRAMA: INICIALIZA TAXAS ADMINISTRATIVAS CRD020 RCD101 CGD020
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CRPX020.
           COPY RCPX101.
           COPY CGPX020.
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


       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY CRPW020.
           COPY RCPW101.
           COPY CGPW020.
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
           05  PORTADOR-CH999            PIC 99.
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



       WORKING-STORAGE SECTION.
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-CGD020             PIC XX       VALUE SPACES.
           05  ST-CHD099             PIC XX       VALUE SPACES.
           05  ST-CHD999             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  MESANOW.
               10  MES-WW            PIC 99.
               10  ANO-WW            PIC 9999.
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-WI            PIC 9999.
               10  MES-WI            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  CARACTERISTICA-W      PIC 9        VALUE ZEROS.
      *    P/ SABER QUAL O TIPO-LCTO SELECIONADO
           05  IMPRIME-W             PIC 9        VALUE ZEROS.
      *    FLAG P/ IDENTIFICAR QUAIS NOMES FAZEM PARTE DA CARACTERISTICA
      *    SELECIONADA
           05  GRAVA1-REGRAVA2       PIC 9        VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  MESANO-E              PIC 99/9999  BLANK WHEN ZEROS.
           05  TIPO-LCTO-W           PIC 9        VALUE ZEROS.
           05  VALOR-E               PIC ZZZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           COPY "PARAMETR".
       01  CAB01.
           05  EMPRESA-REL         PIC X(40)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(10)   VALUE "  :  ".
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(37)   VALUE
           "RELACAO DE CREDITOS DE REPORTAGEM".
           05  FILLER              PIC X(09)   VALUE "MES/ANO: ".
           05  MESANO-REL          PIC 99/9999.
           05  FILLER              PIC X(09)   VALUE SPACES.
           05  FILLER              PIC X(08)   VALUE 'VENCTO: '.
           05  VENCTO-REL          PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "FUNCIONARIO                     TOTAL-CREDITOS".
       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "RCD101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD101.
           MOVE "CGD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD020.
           MOVE "CHD099" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD099.
           MOVE "CHD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD999.
           CLOSE CONTROLE.

           OPEN I-O CRD020
           IF ST-CRD020 = "35"
              CLOSE CRD020      OPEN OUTPUT CRD020
              CLOSE CRD020      OPEN I-O CRD020
           END-IF.

           OPEN I-O RCD101
           IF ST-RCD101 = "35"
              CLOSE RCD101      OPEN OUTPUT RCD101
              CLOSE RCD101      OPEN I-O RCD101
           END-IF.

           OPEN I-O CGD020
           IF ST-CGD020 = "35"
              CLOSE CGD020      OPEN OUTPUT CGD020
              CLOSE CGD020      OPEN I-O CGD020
           END-IF.

           OPEN I-O CHD099
           IF ST-CHD099 = "35"
              CLOSE CHD099      OPEN OUTPUT CHD099
              CLOSE CHD099      OPEN I-O CHD099
           END-IF.

           OPEN INPUT CHD999
           IF ST-CHD999 = "35"
              CLOSE CHD999      OPEN OUTPUT CHD999
              CLOSE CHD999      OPEN I-O CHD999
           END-IF.



           DISPLAY "VOU COMECAR A EXECUTAR O GALHO66" STOP " ".
           DISPLAY "VOU COMECAR A EXECUTAR O GALHO66" STOP " ".

           INITIALIZE REG-CHD999
           START CHD999 KEY IS NOT LESS CHAVE-CH999 INVALID KEY
               MOVE "10" TO ST-CHD999.
           PERFORM UNTIL ST-CHD999 = "10"
               READ CHD999 NEXT AT END
                   MOVE "10" TO ST-CHD999
               NOT AT END
                   INITIALIZE REG-CHD099
                   MOVE REG-CHD999 TO REG-CHD099
                   DISPLAY REG-CHD099
                   WRITE REG-CHD099
                   END-WRITE
               END-READ
           END-PERFORM.


           INITIALIZE REG-CRD020
           START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
               MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
               READ CRD020 NEXT AT END
                   MOVE "10" TO ST-CRD020
               NOT AT END
                   MOVE TAXA-ADMINIST-CREDITO-CR20 TO
                        TAXA-ADMINIST-PARCELA-CR20
                   IF FORMA-PAGTO-CR20 = SPACES and
                      SITUACAO-CR20 = 2
                      MOVE "4-Receb.Bco." TO FORMA-PAGTO-CR20
                   END-IF
                   DISPLAY REG-CRD020
                   REWRITE REG-CRD020
                   END-REWRITE
               END-READ
           END-PERFORM.

           INITIALIZE REG-RCD101
           START RCD101 KEY IS NOT LESS CHAVE-REC1 INVALID KEY
               MOVE "10" TO ST-RCD101.
           PERFORM UNTIL ST-RCD101 = "10"
               READ RCD101 NEXT AT END
                   MOVE "10" TO ST-RCD101
               NOT AT END
                   MOVE TAXA-ADMINIST-CREDITO-REC1 TO
                        TAXA-ADMINIST-PARCELA-REC1

                   DISPLAY REG-RCD101
                   REWRITE REG-RCD101
                   END-REWRITE
               END-READ
           END-PERFORM.

           INITIALIZE REG-CGD020
           START CGD020 KEY IS NOT LESS CODIGO-CG20 INVALID KEY
               MOVE "10" TO ST-CGD020.
           PERFORM UNTIL ST-CGD020 = "10"
               READ CGD020 NEXT AT END
                   MOVE "10" TO ST-CGD020
               NOT AT END
                   MOVE TAXA-CREDITO-CG20 TO TAXA-PARCELA-CG20
                   DISPLAY REG-CGD020
                   REWRITE REG-CGD020
                   END-REWRITE
               END-READ
           END-PERFORM.


           DISPLAY "ACABOU DE EXECUTAR O GALHO66" STOP " ".

           CLOSE CRD020 RCD101 CGD020 CHD099 CHD999
           EXIT PROGRAM
           STOP RUN.


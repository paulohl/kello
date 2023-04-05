       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO26.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 31/01/2004.
      *FUNÇÃO: ATUALIZA A DATA DE MOVIMENTO DO CHD010 DOS CHEQUES Q
      *ESTÃO SEM DATA DE MOVIMENTO E VENCTO ACIMA DE 01/01/2005 PORTADOR
      *= 1 COLOCAR DATA MOVIMENTO = 17/01/2005
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CHPX010.
           SELECT CHD999 ASSIGN TO PATH-CHD999
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CH99 = DATA-MOVTO-CH99 SEQ-CH99
                  ALTERNATE RECORD KEY ALT-CH91 =
                     NOME-CH99, PORTADOR-CH99 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH92 = SITUACAO-CH99
                     DATA-VENCTO-CH99 PORTADOR-CH99 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH93 = DATA-MOVTO-CH99
                     VENDEDOR-CH99 SEQ-CH99 WITH DUPLICATES
                  ALTERNATE RECORD KEY ALT-CH94 = COD-COMPL-CH99
                     DATA-VENCTO-CH99 WITH DUPLICATES
                  STATUS IS ST-CHD999.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CHPW010.

      * ARQUIVO DO MOVIMENTO DE CHEQUES
       FD  CHD999.
       01  REG-CHD999.
           05  DATA-MOVTO-CH99          PIC 9(8).
           05  SEQ-CH99                 PIC 9(4).
           05  COD-COMPL-CH99.
               10  CLASS-CLIENTE-CH99   PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CH99         PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  NOME-CH99                PIC X(30).
           05  CIDADE-CH99              PIC X(19).
           05  LOTE-CH99                PIC X(01).
           05  CONTA-CORR-CH99          PIC 9(8)   COMP-3.
           05  DV-CONTA-CH99            PIC X.
           05  BANCO-CH99               PIC 9(4)   COMP-3.
           05  AGENCIA-CH99             PIC 9(5)   COMP-3.
           05  DV-AGENCIA-CH99          PIC X.
           05  COMPENSACAO-CH99         PIC 9(3).
           05  CPF-CH99                 PIC 9(11)  COMP-3.
           05  NR-CHEQUE-CH99           PIC X(7).
           05  OUTRO-DOCTO-CH99         PIC X(15).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  DATA-VENCTO-CH99         PIC 9(8).
           05  PORTADOR-CH99            PIC 99.
           05  CARTEIRA-CH99            PIC 9.
      *    CARTEIRA-CH10  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CH99        PIC 99.
           05  SITUACAO-CH99            PIC 9.
      *    SITUACAO = 0-OK  2-RECEBIDO  3-ESTONADO  4-CANCELADO
      *               5-DEVOLVIDO   6-PROBLEMATICO
           05  CODREDUZ-APUR-CH99       PIC 9(3).
           05  VALOR-CH99               PIC 9(8)V99.
           05  VENDEDOR-CH99            PIC 9(6)   COMP-3.
           05  DIGITADOR-CH99           PIC X(5).
           05  SEQ-CAIXA-CH99           PIC 9(3).
           05  NR-NOTA-FISCAL-CH99      PIC X(10).
           05  DATA-NTA-FISCAL-CH99     PIC 9(8)   COMP-3.
           05  ORIGEM-CH99              PIC 99.


       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD999             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.

           05  CUSTO-PREVISTO-W      PIC 9(8)V99  VALUE ZEROS.
           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           05  VALOR-E               PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  I                     PIC 99       VALUE ZEROS.
           05  L                     PIC 99       VALUE ZEROS.
           05  K                     PIC 99       VALUE ZEROS.
           05  J                     PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  REG-AUX               pic x(1000).

           COPY "PARAMETR".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.

       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONTRATO    ITEM    CURSO    TURMA".


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "CHD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CHD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD999.

           OPEN I-O CHD999

           OPEN I-O CHD010.

           CLOSE CONTROLE.
           IF ST-CHD010 = "35"
              CLOSE CHD010      OPEN OUTPUT CHD010
              CLOSE CHD010      OPEN I-O CHD010
           END-IF.

           IF ST-CHD999 = "35"
              CLOSE CHD999      OPEN OUTPUT CHD999
              CLOSE CHD999      OPEN I-O CHD999
           END-IF.

      *    INITIALIZE REG-CHD010
      *
      *    START CHD010 KEY IS NOT LESS CHAVE-CH10 INVALID KEY
      *          MOVE "10" TO ST-CHD010.
      *
      *
      *    PERFORM UNTIL ST-CHD010 = "10"
      *      READ CHD010 NEXT RECORD AT END MOVE "10" TO ST-CHD010
      *       NOT AT END
      *            display reg-chd010
      *            IF DATA-MOVTO-CH10 IS NOT NUMERIC OR
      *               DATA-MOVTO-CH10 = 0
      *                     display reg-chd010 stop " "
      *                     MOVE REG-CHD010 TO REG-CHD999
      *                     WRITE REG-CHD999
      *                     END-WRITE
      *            ELSE
      *               MOVE "10" TO ST-CHD010
      *            END-IF
      *       END-READ
      *    END-PERFORM.



           INITIALIZE REG-CHD999

           START CHD999 KEY IS NOT LESS CHAVE-CH99 INVALID KEY
                 MOVE "10" TO ST-CHD999.


           PERFORM UNTIL ST-CHD999 = "10"
             READ CHD999 NEXT RECORD AT END MOVE "10" TO ST-CHD999
              NOT AT END
                   IF DATA-MOVTO-CH99 = 0
                      IF DATA-VENCTO-CH99 > 20041231
                         IF PORTADOR-CH99 = 1
                            DISPLAY REG-CHD999 STOP " "
                            MOVE REG-CHD999 TO REG-CHD010
                            READ CHD010 NOT INVALID KEY
                               DELETE CHD010
                               END-DELETE
                            END-READ
                            MOVE REG-CHD999 TO REG-CHD010
                            MOVE 20050117   TO DATA-MOVTO-CH10
                            WRITE REG-CHD010
                            END-WRITE
                         END-IF
                      END-IF
                   END-IF
              END-READ
           END-PERFORM.

           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE CHD010
                 CHD999
           EXIT PROGRAM.

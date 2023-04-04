       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO15.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 10/11/2004.
      *FUNÇÃO: GERA UM NOVO ARQUIVO CRD998 E CRD999
      * CRD998 => IGUAL O CRD200 APARTIR DO DIA 8/11/2004
      * CRD999 => IGUAL 0 CRD201 APARTIR DO DIA 8/11/2004

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CRPX200.

          SELECT CRD999 ASSIGN TO PATH-CRD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD999
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR999 = COD-COMPL-CR999 SEQ-CR999
                  ALTERNATE RECORD KEY IS ALT1-CR998 = COD-COMPL-CR999
                         DATA-MOVTO-CR999 HORA-MOVTO-CR999
                         WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CR999 =
                      DATA-RETORNO-CR999 USUARIO-CR999 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CR999 = DATA-MOVTO-CR999
                      HORA-MOVTO-CR999 USUARIO-CR999 WITH DUPLICATES.


       DATA DIVISION.
       FILE SECTION.
           COPY CAPW001.
           COPY CRPW200.

      *CADASTRO DE ANOTAÇÕES DO CONTAS A RECEBER E CHEQUES
      * ARQUIVO NOVO
       FD  CRD999.
       01  REG-CRD999.
           05  COD-COMPL-CR999     PIC 9(9).
           05  SEQ-CR999           PIC 9(3).
           05  DATA-MOVTO-CR999    PIC 9(8).
           05  HORA-MOVTO-CR999    PIC 9(4).
           05  DATA-RETORNO-CR999  PIC 9(8).
           05  USUARIO-CR999       PIC X(5).
           05  SITUACAO-ANOTACAO-CR999 PIC 9.
      *    SITUACAO-ANOTACAO-CR998 = 0-PENDENTE  1-CHECADO

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD999             PIC XX       VALUE SPACES.
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

           COPY "PARAMETR".


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD999.
           OPEN I-O CRD200 CRD999
           CLOSE CONTROLE.
           IF ST-CRD200 = "35"
              CLOSE CRD200      OPEN OUTPUT CRD200
              CLOSE CRD200      OPEN I-O CRD200
           END-IF.
           IF ST-CRD999 = "35"
              CLOSE CRD999      OPEN OUTPUT CRD999
              CLOSE CRD999      OPEN I-O CRD999
           END-IF.
           DISPLAY "VOU COMEÇAR O GALHO16" STOP " ".
           DISPLAY "VOU COMEÇAR O GALHO16" STOP " ".

           INITIALIZE REG-CRD999

           START CRD999 KEY IS NOT LESS CHAVE-CR999 INVALID KEY
                 MOVE "10" TO ST-CRD999.

           PERFORM UNTIL ST-CRD999 = "10"
             READ CRD999 NEXT RECORD AT END
                  MOVE "10" TO ST-CRD999
             NOT AT END
                  display reg-crd999 "CRD999"
                  MOVE REG-CRD999 TO REG-CRD200
                  WRITE REG-CRD200
                  END-WRITE
             END-READ
           END-PERFORM.


           DISPLAY "ACABOU DE EXECUTAR O GALHO16" STOP " ".

           CLOSE CRD200 CRD999
           EXIT PROGRAM
           STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO51.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 04/07/2005.
      *FUNÇÃO: ALTERA LAYOUT DO COP003

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY COPX003.

           COPY COPX060.

           COPY CRPX020.

           SELECT  COD999 ASSIGN TO PATH-COD999
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   STATUS IS ST-COD999
                   RECORD KEY IS CODIGO-CO99
                   ALTERNATE RECORD KEY IS NOME-CO99 WITH DUPLICATES.

           SELECT  COD998 ASSIGN TO PATH-COD998
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   STATUS IS ST-COD998
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   RECORD KEY IS CHAVE-CO98 = NR-CONTRATO-CO98 ITEM-CO98
                   ALTERNATE RECORD KEY IS DATAREALIZA-CO98
                             WITH DUPLICATES
                   ALTERNATE RECORD KEY IS ALT1-CO98 = NR-CONTRATO-CO98
                             DATAREALIZA-CO98 ITEM-CO98.

          SELECT CRD999 ASSIGN TO PATH-CRD999
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD999
                  LOCK MODE IS MANUAL WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR99 = COD-COMPL-CR99 SEQ-CR99
                  ALTERNATE RECORD KEY IS DATA-MOVTO-CR99
                            WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-CR99 = COD-COMPL-CR99
                        DATA-VENCTO-CR99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CR99 = SITUACAO-CR99
                        DATA-VENCTO-CR99 COD-COMPL-CR99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CR99 = SITUACAO-CR99
                        DATA-MOVTO-CR99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-CR99 = SITUACAO-CR99
                        CLIENTE-CR99 DATA-VENCTO-CR99 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT6-CR99 = DATA-RCTO-CR99
                       SEQ-CAIXA-CR99 WITH DUPLICATES.


       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY COPW003.

           COPY COPW060.

           COPY CRPW020.

       FD  COD999.
       01  REG-COD999.
           05  CODIGO-CO99       PIC 999.
           05  NOME-CO99         PIC X(20).
           05  EVENTO-PRE-CO99   PIC 9.

       FD  COD998.
       01  REG-COD998.
           05  NR-CONTRATO-CO98  PIC 9(04).
           05  ITEM-CO98         PIC 999.
           05  CODEVENTO-CO98    PIC 999.
           05  DATA-SOLIC-CO98   PIC 9(08)  COMP-3.
           05  DATAREALIZA-CO98  PIC 9(08).
      *    DATAREALIZA-CO60 = AAAA/MM/DD.
           05  HORARIO-CO98      PIC X(10).
           05  LOCAL-CO98        PIC X(25).
           05  REFERENCIA-CO98   PIC X(30).
           05  ENDERECO-CO98     PIC X(40).
           05  PESSOA-CONTATO-CO98 PIC X(30).
           05  UNIFORME-CO98     PIC X(20).
      *    LOCAL, HORARIO, REFERENCIA, ENDERECO, PESSOA-CONTATO SÃO
      *    INFORMACOES REFERENTES A LOCALIZACAO DO EVENTO
           05  COD-COMISSAO-CO98 PIC 9(4).
      *    COD-COMISSAO = NR-CONTRATO+COD-COMISSAO
           05  QT-PARTICIPANTE-CO98  PIC 9(4).
           05  QT-TELAO-CO98     PIC 9.
           05  FOTO-CO98         PIC 9.
           05  VIDEO-CO98        PIC 9.
           05  BECA-CO98         PIC 9.
           05  CLIP-CO98         PIC 9.
           05  FAX-CO98          PIC 9.
           05  APROVACAO-CO98    PIC 9.
           05  ORGANIZADOR-CO98  PIC X(15).
           05  OBSERVACAO-CO98   PIC X(80).
           05  NR-REL-REPOR-CO98 PIC 9(6)   COMP-3.
           05  NR-PLANEJ-CO98    PIC 9(8)   COMP-3.
           05  DATA-CANCELAM-CO98 PIC 9(8)  COMP-3.
           05  HORA-CANCELAM-CO98 PIC X(5).

       FD  CRD999.
       01  REG-CRD999.
           05  DATA-MOVTO-CR99          PIC 9(8).
           05  COD-COMPL-CR99.
               10  CLASS-CLIENTE-CR99   PIC 9.
      *    classificação cliente =  0-contrato  1-comum
               10  CLIENTE-CR99         PIC 9(8).
      *    quando a classificação for = 0 - o código do cliente será
      *    o nr-contrato+album e = 1(comum) será uma sequência de código
           05  SEQ-CR99                 PIC 9(5).
           05  PORTADOR-CR99            PIC 99.
           05  CARTEIRA-CR99            PIC 9.
      *    CARTEIRA-CR20  1-SIMPLES  2-CAUÇÃO  3-DESCONTO
           05  SITUACAO-TIT-CR99        PIC 99.
           05  NR-DOCTO-CR99            PIC X(10).
           05  OUTRO-DOCTO-CR99         PIC X(25).
      *    O NR DA REMESSA, E NO RETORNO O NR-TÍTULO-NO-BANCO(NOSSO-NR)
           05  TIPO-DOCTO-CR99          PIC 9.
      *    TIPO-DOCTO = 0-DUPLICATA  1-NTA-PROMISSÓRIA  2-ORG.EVENTO
      *                 3-DEBITO AUTOMATICO 4-CARTAO CREDITO
           05  DATA-EMISSAO-CR99        PIC 9(8).
           05  DATA-VENCTO-CR99         PIC 9(8).
      *    DATA-VENCTO-CR20 - AAAAMMDD
           05  DESCRICAO-CR99           PIC X(30).
           05  SITUACAO-CR99            PIC 9.
      *    SITUACAO = 0-OK  2-PAGA  3-ESTONADA  4-CANCELADA
           05  TIPO-MOEDA-CR99          PIC 9.
      *    TIPO-MOEDA = 0-REAL  1-DOLAR
           05  NR-PARCELA-CR99.
               10  NR-PARC-CR99         PIC 99.
               10  TOT-PARC-CR99        PIC 99.
           05  CODREDUZ-APUR-CR99       PIC 9(3).
           05  VALOR-TOT-CR99           PIC 9(8)V99.
           05  JURO-RCTO-CR99           PIC 9(6)V99.
           05  MULTA-RCTO-CR99          PIC 9(6)V99.
           05  DESCONTO-CR99            PIC 9(6)V99.
           05  DATA-RCTO-CR99           PIC 9(8).
      *    DATA-RCTO-CR20 = AAAA/MM/DD
           05  VALOR-LIQ-CR99           PIC 9(8)V99.
           05  CONTABILIZADO-CR99       PIC 9.
           05  VENDEDOR-CR99            PIC 9(6).
           05  RESPONSAVEL-CR99         PIC X(5).
           05  DIGITADOR-CR99           PIC X(5).
           05  SEQ-CAIXA-CR99           PIC 9(3).
           05  NR-NOTA-FISCAL-CR99      PIC X(10).
           05  DATA-NTA-FISCAL-CR99     PIC 9(8).
           05  FORMA-PAGTO-CR99         PIC X(10).
           05  DCR-MEM-CR99             PIC X(15).
           05  CARTAO-CRED-CR99         PIC 9(02).
           05  TAXA-ADMINISTRATIVA-CR99 PIC 9(03)V99.
           05  FILLER                   PIC X(23).

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-COD999             PIC XX       VALUE SPACES.
           05  ST-COD998             PIC XX       VALUE SPACES.
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
           05  AUX-ALBUM             PIC 9(08)    VALUE ZEROS.

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
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003
           MOVE "COD060" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD060
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020
           MOVE "COD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD999
           MOVE "COD998" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD998
           MOVE "CRD999" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD999
           OPEN I-O   COD003 COD060 CRD020
           CLOSE      COD003 COD060 CRD020
           OPEN I-O   COD003 COD060 CRD020
           OPEN INPUT COD999 COD998 CRD999

           CLOSE CONTROLE.
           IF ST-COD003 = "35"
              CLOSE COD003      OPEN OUTPUT COD003
              CLOSE COD003      OPEN I-O COD003
           END-IF.

           IF ST-COD060 = "35"
              CLOSE COD060      OPEN OUTPUT COD060
              CLOSE COD060      OPEN I-O COD060
           END-IF.

           IF ST-CRD020 = "35"
              CLOSE CRD020      OPEN OUTPUT CRD020
              CLOSE CRD020      OPEN I-O CRD020
           END-IF.


           display "Vou comecar a atualizar o COD003, COD060 e CRD020"
           stop " ".

           IF ST-COD003 <> "00" OR ST-COD999 <> "00"
              CLOSE COD003
                    COD999
              stop run.

           IF ST-COD060 <> "00" OR ST-COD998 <> "00"
              CLOSE COD060
                    COD998
              stop run.

           IF ST-CRD020 <> "00" OR ST-CRD999 <> "00"
              CLOSE CRD020
                    CRD999
              stop run.

           INITIALIZE REG-COD999

           START COD999 KEY IS NOT LESS CODIGO-CO99 INVALID KEY
                 MOVE "10" TO ST-COD999.


           PERFORM UNTIL ST-COD999 = "10"
             READ COD999 NEXT RECORD AT END
                  MOVE "10" TO ST-COD999
             NOT AT END
                  MOVE CODIGO-CO99        TO CODIGO-CO03
                  MOVE NOME-CO99          TO NOME-CO03
                  MOVE EVENTO-PRE-CO99    TO EVENTO-PRE-CO03
                  MOVE 0                  TO RELAT-ESTAT-CO03
                  DISPLAY REG-COD003
                  WRITE REG-COD003
              END-READ
           END-PERFORM.

           INITIALIZE REG-COD998

           START COD998 KEY IS NOT LESS CHAVE-CO98 INVALID KEY
                 MOVE "10" TO ST-COD998.


           PERFORM UNTIL ST-COD998 = "10"
             READ COD998 NEXT RECORD AT END
                  MOVE "10" TO ST-COD998
             NOT AT END
                  MOVE NR-CONTRATO-CO98     TO NR-CONTRATO-CO60
                  MOVE ITEM-CO98            TO ITEM-CO60
                  MOVE CODEVENTO-CO98       TO CODEVENTO-CO60
                  MOVE DATA-SOLIC-CO98      TO DATA-SOLIC-CO60
                  MOVE DATAREALIZA-CO98     TO DATAREALIZA-CO60
                  MOVE HORARIO-CO98         TO HORARIO-CO60
                  MOVE LOCAL-CO98           TO LOCAL-CO60
                  MOVE REFERENCIA-CO98      TO REFERENCIA-CO60
                  MOVE ENDERECO-CO98        TO ENDERECO-CO60
                  MOVE PESSOA-CONTATO-CO98  TO PESSOA-CONTATO-CO60
                  MOVE UNIFORME-CO98        TO UNIFORME-CO60
                  MOVE COD-COMISSAO-CO98    TO COD-COMISSAO-CO60
                  MOVE QT-PARTICIPANTE-CO98 TO QT-PARTICIPANTE-CO60
                  MOVE QT-TELAO-CO98        TO QT-TELAO-CO60
                  MOVE FOTO-CO98            TO FOTO-CO60
                  MOVE VIDEO-CO98           TO VIDEO-CO60
                  MOVE BECA-CO98            TO BECA-CO60
                  MOVE CLIP-CO98            TO CLIP-CO60
                  MOVE FAX-CO98             TO FAX-CO60
                  MOVE APROVACAO-CO98       TO APROVACAO-CO60
                  MOVE ORGANIZADOR-CO98     TO ORGANIZADOR-CO60
                  MOVE OBSERVACAO-CO98      TO OBSERVACAO-CO60
                  MOVE NR-REL-REPOR-CO98    TO NR-REL-REPOR-CO60
                  MOVE NR-PLANEJ-CO98       TO NR-PLANEJ-CO60
                  MOVE DATA-CANCELAM-CO98   TO DATA-CANCELAM-CO60
                  MOVE HORA-CANCELAM-CO98   TO HORA-CANCELAM-CO60
                  MOVE 2                    TO DT-PREV-REAL-CO60
                  DISPLAY REG-COD060
                  WRITE REG-COD060
              END-READ
           END-PERFORM.

           INITIALIZE REG-CRD999

           START CRD999 KEY IS NOT LESS CHAVE-CR99 INVALID KEY
                 MOVE "10" TO ST-CRD999.


           PERFORM UNTIL ST-CRD999 = "10"
             READ CRD999 NEXT RECORD AT END
                  MOVE "10" TO ST-CRD999
             NOT AT END
                  MOVE REG-CRD999 TO REG-CRD020
                  DISPLAY REG-CRD020
                  WRITE REG-CRD020
              END-READ
           END-PERFORM.


           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".
           DISPLAY "ACABOU" STOP "  ".

           CLOSE COD999 COD998 CRD999
           CLOSE COD003 COD060 CRD020
           EXIT PROGRAM
           STOP RUN.

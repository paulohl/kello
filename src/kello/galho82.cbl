       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO82.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 30/07/1998
      *exclui conta fixa 3325 e 3326
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY CRPX200.

          SELECT CRD990 ASSIGN TO PATH-CRD990
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD990
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR990 = COD-COMPL-CR990 SEQ-CR990
                  ALTERNATE RECORD KEY IS ALT1-CR990 = COD-COMPL-CR990
                         DATA-MOVTO-CR990 HORA-MOVTO-CR990
                         WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-CR990 =
                      DATA-RETORNO-CR990 USUARIO-CR990 COD-COMPL-CR990
                      SEQ-CR990 WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CR990 = DATA-MOVTO-CR990
                      HORA-MOVTO-CR990 USUARIO-CR990 WITH DUPLICATES.



           COPY CRPX201.

          SELECT CRD991 ASSIGN TO PATH-CRD991
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CRD991
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CR991 = COD-COMPL-CR991 SEQ-CR991
                                              SUBSEQ-CR991.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY CRPW200.

      *CADASTRO DE ANOTAÇÕES DO CONTAS A RECEBER E CHEQUES
       FD  CRD990.
       01  REG-CRD990.
           05  COD-COMPL-CR990         PIC 9(9).
           05  SEQ-CR990               PIC 9(3).
           05  DATA-MOVTO-CR990        PIC 9(8).
           05  HORA-MOVTO-CR990        PIC 9(4).
           05  DATA-RETORNO-CR990      PIC 9(8).
           05  USUARIO-CR990           PIC X(5).
           05  SITUACAO-ANOTACAO-CR990 PIC 9.
      *    SITUACAO-ANOTACAO-CR200 = 0-PENDENTE  1-CHECADO



           COPY CRPW201.

      *COMPLEMENTO DE ANOTAÇÕES DO CONTAS A RECEBER E CHEQUE-CONT CRD200
       FD  CRD991.
       01  REG-CRD991.
           05  COD-COMPL-CR991     PIC 9(9).
           05  SEQ-CR991           PIC 9(3).
           05  SUBSEQ-CR991        PIC 9(2).
           05  ANOTACAO-CR991      PIC X(80).

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-CRD990             PIC XX       VALUE SPACES.
           05  ST-CRD991             PIC XX       VALUE SPACES.
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

           MOVE "CRD990" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD990.
           MOVE "CRD991" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD991.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE ZEROS    TO ERRO-W.
           OPEN I-O CRD200
                    CRD201
                    CRD990
                    CRD991
           CLOSE    CRD200
                    CRD201
                    CRD990
                    CRD991
           OPEN I-O CRD200
                    CRD201
                    CRD990
                    CRD991
           CLOSE CONTROLE.

           IF ST-CRD200 = "35"
              CLOSE CRD200      OPEN OUTPUT CRD200
              CLOSE CRD200      OPEN I-O    CRD200
           END-IF.
           IF ST-CRD201 = "35"
              CLOSE CRD201      OPEN OUTPUT CRD201
              CLOSE CRD201      OPEN I-O    CRD201
           END-IF.

           IF ST-CRD990 = "35"
              CLOSE CRD990      OPEN OUTPUT CRD990
              CLOSE CRD990      OPEN I-O    CRD990
           END-IF.
           IF ST-CRD991 = "35"
              CLOSE CRD991      OPEN OUTPUT CRD991
              CLOSE CRD991      OPEN I-O    CRD991
           END-IF.

          DISPLAY "VOU COMECAR A CONVERSAO DOS ARQUIVOS CRD200 E CRD201"
            STOP " ".

           INITIALIZE REG-CRD990
           START CRD990 KEY IS NOT LESS CHAVE-CR990 INVALID KEY
               MOVE "10" TO ST-CRD990.

           PERFORM UNTIL ST-CRD990 = "10"
               READ CRD990 NEXT AT END
                   MOVE "10" TO ST-CRD990
               NOT AT END
                   MOVE COD-COMPL-CR990         TO
                        COD-COMPL-CR200
                   MOVE SEQ-CR990               TO
                        SEQ-CR200
                   MOVE DATA-MOVTO-CR990        TO
                        DATA-MOVTO-CR200
                   MOVE HORA-MOVTO-CR990        TO
                        HORA-MOVTO-CR200
                   MOVE DATA-RETORNO-CR990      TO
                        DATA-RETORNO-CR200
                   MOVE USUARIO-CR990           TO
                        USUARIO-CR200
                   MOVE SITUACAO-ANOTACAO-CR990 TO
                        SITUACAO-ANOTACAO-CR200

                   DISPLAY "REG-CRD200 => " REG-CRD200
                   WRITE REG-CRD200 INVALID KEY
                       DISPLAY "Erro de Gravacao...CRD200" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM

           INITIALIZE REG-CRD991
           START CRD991 KEY IS NOT LESS CHAVE-CR991 INVALID KEY
               MOVE "10" TO ST-CRD991.

           PERFORM UNTIL ST-CRD991 = "10"
               READ CRD991 NEXT AT END
                   MOVE "10" TO ST-CRD991
               NOT AT END
                   MOVE COD-COMPL-CR991  TO COD-COMPL-CR201
                   MOVE SEQ-CR991        TO SEQ-CR201
                   MOVE SUBSEQ-CR991     TO SUBSEQ-CR201
                   MOVE ANOTACAO-CR991   TO ANOTACAO-CR201
                   DISPLAY "REG-CRD201 => " REG-CRD201
                   WRITE REG-CRD201 INVALID KEY
                       DISPLAY "Erro de Gravacao...CRD201" STOP " "
                   END-WRITE
               END-READ
           END-PERFORM


           CLOSE CRD200 CRD201 CRD991 CRD990
           STOP RUN.

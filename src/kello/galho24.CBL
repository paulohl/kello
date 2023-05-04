       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO24.
      *AUTORA: ALFREDO SAVIOLLI NETO
      *DATA: 10/01/2005
      *DESCRIÇÃO: PROGRAMA Q ALTERA CONTRATOS ERRADOS EM CONTRATOS
      *           CORRETOS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.
           COPY CAPX010.
           COPY MTPX019.
           COPY CGPX010.
           COPY CGPX011.
           COPY CGPX012.
           COPY CRPX020.
           COPY CRPX200.
           COPY CRPX201.
           COPY CHPX010.
           COPY CHPX012.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY CAPW010.
           COPY MTPW019.
           COPY CGPW010.
           COPY CGPW011.
           COPY CGPW012.
           COPY CRPW020.
           COPY CRPW200.
           COPY CRPW201.
           COPY CHPW010.
           COPY CHPW012.

       WORKING-STORAGE SECTION.
           COPY "GALHO24.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".

       77  LIN                       PIC 9(02).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-ACD010             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CGD012             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD012             PIC XX       VALUE SPACES.
           05  CODIGO-E              PIC Z(3).
           05  HORA-E                PIC X(5)     VALUE SPACES.
           05  ULT-CODIGO            PIC 9(3)     VALUE ZEROS.
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
           05  TIPO-W                PIC X(13)    VALUE SPACES.
           05  TIPO1-W               PIC 9(01)    VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  AUX-ASSUNTO           PIC 9(01).
           05  DATA-E                PIC 99/99/9999.
           05  VALOR-E               PIC Z.ZZ9,99.
           05  REG-AUX               PIC X(700).
       77  COMPARA                   PIC 9(09).

       01 AUX-ERRADO.
          05 AUX-E                   PIC 9(01).
          05 AUX-CLIENTE             PIC 9(08).

           COPY "PARAMETR".

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).

       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011
           MOVE "CGD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD012
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201
           MOVE "CHD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010
           MOVE "CHD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD012

           OPEN I-O CGD010
                    CGD011
                    CGD012
                    CRD020
                    CRD200
                    CRD201
                    CHD010

           OPEN INPUT MTD019 CHD012

           CLOSE CONTROLE.
           MOVE 1 TO GRAVA-W.
           IF ST-MTD019 = "35"
              CLOSE MTD019      OPEN OUTPUT MTD019
              CLOSE MTD019      OPEN I-O MTD019
           END-IF.
           IF ST-CGD010 = "35"
              CLOSE CGD010      OPEN OUTPUT CGD010
              CLOSE CGD010      OPEN I-O CGD010
           END-IF.
           IF ST-CGD011 = "35"
              CLOSE CGD011      OPEN OUTPUT CGD011
              CLOSE CGD011      OPEN I-O CGD011
           END-IF.
           IF ST-CGD012 = "35"
              CLOSE CGD012      OPEN OUTPUT CGD012
              CLOSE CGD012      OPEN I-O CGD012
           END-IF.
           IF ST-CRD200 = "35"
              CLOSE CRD200      OPEN OUTPUT CRD200
              CLOSE CRD200      OPEN I-O CRD200
           END-IF.
           IF ST-CRD201 = "35"
              CLOSE CRD201      OPEN OUTPUT CRD201
              CLOSE CRD201      OPEN I-O CRD201
           END-IF.
           IF ST-CRD020 = "35"
              CLOSE CRD020      OPEN OUTPUT CRD020
              CLOSE CRD020      OPEN I-O CRD020
           END-IF.
           IF ST-CHD010 = "35"
              CLOSE CHD010      OPEN OUTPUT CHD010
              CLOSE CHD010      OPEN I-O CHD010
           END-IF.
           IF ST-CHD012 = "35"
              CLOSE CHD012      OPEN OUTPUT CHD012
              CLOSE CHD012      OPEN I-O CHD012
           END-IF.


           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD012 <> "00"
              MOVE "ERRO ABERTURA CGD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD012 <> "00"
              MOVE "ERRO ABERTURA CHD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ERRO-W = ZEROS
               PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-LE-MTD019-TRUE
                    PERFORM LER-MTD019
               WHEN GS-LE-CGD010-TRUE
                    PERFORM LER-CGD010
               WHEN GS-LE-TROCA-TRUE
                    PERFORM LER-TROCA
               WHEN GS-POP-UP-CGD010-TRUE
                    PERFORM POP-UP-CGD010
               WHEN GS-POP-UP-MTD019-TRUE
                    PERFORM POP-UP-MTD019
               WHEN GS-POP-UP-TROCA-TRUE
                    PERFORM POP-UP-TROCA
               WHEN GS-LE-DADOS-TRUE
                    PERFORM LE-DADOS
               WHEN GS-CHAMA-ALTERACAO-TRUE
                    PERFORM CHAMAR-ALTERACAO
               WHEN GS-ALTERAR-LB-TRUE
                    PERFORM ALTERAR-LB
               WHEN GS-LIMPAR-LB-TRUE
                    PERFORM LIMPAR-LB
               WHEN GS-CONFIRMA-ALTERACAO-TRUE
                    PERFORM CONFIRMA-ALTERACAO
               WHEN GS-SELECIONA-CHD010-TRUE
                    PERFORM SELECIONA-CHD010
               WHEN GS-SELECIONA-CRD020-TRUE
                    PERFORM SELECIONA-CRD020
               WHEN GS-SELECIONA-CRD200-TRUE
                    PERFORM SELECIONA-CRD200
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CONFIRMA-ALTERACAO SECTION.
           MOVE "N" TO GS-ALTERAR
           MOVE "Os dados Informados estão Corretos ?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           IF RESP-MSG EQUAL "S"
              MOVE "S" TO GS-ALTERAR.

       SELECIONA-CHD010 SECTION.
           MOVE SPACES TO GS-LINDET
           MOVE 1      TO GS-POSICAO
           MOVE "LER-LB1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINDET = SPACES
               MOVE "X"                   TO GS-LINDET(2:1)
               MOVE GS-CLASSIF-TROCA      TO GS-LINDET(119:1)
               MOVE GS-CONTRATO-TROCA     TO GS-LINDET(120:9)
               MOVE "ALTERAR-LB-CHD010" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
               MOVE SPACES TO GS-LINDET
               ADD 1 TO GS-POSICAO
               MOVE "LER-LB1" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       SELECIONA-CRD020 SECTION.
           MOVE SPACES TO GS-LINDET
           MOVE 1      TO GS-POSICAO
           MOVE "LER-LB2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINDET = SPACES
               MOVE "X"                   TO GS-LINDET(2:1)
               MOVE GS-CLASSIF-TROCA      TO GS-LINDET(119:1)
               MOVE GS-CONTRATO-TROCA     TO GS-LINDET(120:9)
               MOVE "ALTERAR-LB-CRD020" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
               MOVE SPACES TO GS-LINDET
               ADD 1 TO GS-POSICAO
               MOVE "LER-LB2" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       SELECIONA-CRD200 SECTION.
           MOVE SPACES TO GS-LINDET
           MOVE 1      TO GS-POSICAO
           MOVE "LER-LB3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINDET = SPACES
               IF GS-LINDET(8:1) = "/"
                  MOVE "X"                   TO GS-LINDET(2:1)
                  MOVE GS-CLASSIF-TROCA      TO GS-LINDET(119:1)
                  MOVE GS-CONTRATO-TROCA     TO GS-LINDET(120:9)
                  MOVE "ALTERAR-LB-CRD200" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
               END-IF
               MOVE SPACES TO GS-LINDET
               ADD 1 TO GS-POSICAO
               MOVE "LER-LB3" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.


       LER-MTD019 SECTION.
           MOVE GS-CONTRATO-MTD019 TO ALBUMMT19
           READ MTD019 INVALID KEY
               MOVE 0         TO GS-CONTRATO-MTD019
               MOVE SPACES    TO NOME-FORM-MT19.

           MOVE NOME-FORM-MT19 TO GS-DESC-CONTRATO-MTD019.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       LER-CGD010 SECTION.
           MOVE GS-CONTRATO-CGD010      TO COD-COMPL-CG10
           READ CGD010 INVALID KEY
               MOVE 0                   TO GS-CONTRATO-CGD010
               MOVE SPACES              TO COMPRADOR-CG10.

           MOVE COMPRADOR-CG10 TO GS-DESC-CONTRATO-CGD010.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       POP-UP-CGD010 SECTION.
           CALL "CGP010T" USING PASSAR-PARAMETROS
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(1:30)  TO GS-DESC-CONTRATO-CGD010
           STRING PASSAR-STRING-1(42: 1)
                  PASSAR-STRING-1(33:8) INTO GS-CONTRATO-CGD010
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       LER-TROCA SECTION.
           MOVE GS-CONTRATO-TROCA   TO COD-COMPL-CG10
           READ CGD010 INVALID KEY
               MOVE "TRANSFERENCIA" TO COMPRADOR-CG10.

           MOVE COMPRADOR-CG10 TO GS-DESC-TROCA.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       POP-UP-MTD019 SECTION.
           CALL "MTP019T2" USING PASSAR-PARAMETROS
           CANCEL "MTP019T2".
           MOVE PASSAR-STRING-1(1:30) TO GS-DESC-CONTRATO-MTD019
           STRING PASSAR-STRING-1(40:4)
                  PASSAR-STRING-1(45:4) INTO GS-CONTRATO-MTD019
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       POP-UP-TROCA SECTION.
           CALL "CGP010T" USING PASSAR-PARAMETROS
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(1:30)     TO GS-DESC-TROCA
           STRING PASSAR-STRING-1(42: 1)
                  PASSAR-STRING-1(33:8) INTO GS-CONTRATO-TROCA

           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       ALTERAR-LB SECTION.
           IF GS-LINDET(8:1) = "/"
           MOVE "X"                   TO GS-LINDET(2:1)
           MOVE GS-CONTRATO-TROCA     TO GS-LINDET(119:9).

       LIMPAR-LB SECTION.
           MOVE SPACES                TO GS-LINDET(2:1)
           MOVE SPACES                TO GS-LINDET(119:9).

       LE-DADOS SECTION.
           MOVE "LIMPAR-LB"           TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-CRD020
           MOVE GS-CONTRATO-CGD010       TO COD-COMPL-CR20
           MOVE ZEROS                    TO SEQ-CR20
           START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
               GO TO START-CHD010.

           IF ST-CRD020 <> "00" AND "02"
              GO TO LE-DADOS.

       READ-CRD020.

           READ CRD020 NEXT RECORD AT END
               GO TO START-CHD010.

           IF ST-CRD020 <> "00" AND "02"
              GO TO READ-CRD020.

           IF GS-CONTRATO-CGD010 <> COD-COMPL-CR20
              GO TO START-CHD010.

           MOVE SPACES           TO GS-LINDET

           MOVE DATA-VENCTO-CR20 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(6: 11)

           MOVE NR-DOCTO-CR20     TO GS-LINDET(18:11)

           MOVE VALOR-TOT-CR20    TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(32:08)

           MOVE COD-COMPL-CR20    TO GS-LINDET(129:9)
           MOVE SEQ-CR20          TO GS-LINDET(138:5)

           MOVE "INSERIR-LB2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           GO TO READ-CRD020.

       START-CHD010.
           INITIALIZE REG-CHD010
           MOVE GS-CONTRATO-CGD010        TO COD-COMPL-CH10
           MOVE ZEROS                     TO DATA-VENCTO-CH10

           START CHD010 KEY IS NOT LESS ALT-CH4 INVALID KEY
               GO TO START-CRD200.

           IF ST-CHD010 <> "00" AND "02"
              GO TO START-CHD010.

       READ-CHD010.
           READ CHD010 NEXT RECORD AT END
               GO TO START-CRD200.

           IF ST-CHD010 <> "00" AND "02"
              GO TO READ-CHD010.

           IF COD-COMPL-CH10 <> GS-CONTRATO-CGD010
               GO TO START-CRD200.

           MOVE SPACES            TO GS-LINDET

           MOVE DATA-VENCTO-CH10  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV          TO DATA-E
           MOVE DATA-E            TO GS-LINDET(6: 11)

           MOVE NR-CHEQUE-CH10    TO GS-LINDET(18:11)

           MOVE VALOR-CH10        TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(32:08)

           MOVE ORIGEM-CH10       TO CODIGO-CH12
           READ CHD012 INVALID KEY
               MOVE SPACES            TO GS-LINDET(43:53)
           NOT INVALID KEY
               MOVE DESCR-ORIGEM-CH12 TO GS-LINDET(43:53)
           END-READ

           MOVE NOME-CH10         TO GS-LINDET(55:30)

           MOVE CIDADE-CH10       TO GS-LINDET(87:30)

           MOVE COD-COMPL-CH10   TO GS-LINDET(129:9)
           MOVE DATA-MOVTO-CH10  TO GS-LINDET(142:8)
           MOVE SEQ-CH10         TO GS-LINDET(138:4)

           MOVE "INSERIR-LB1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           GO TO READ-CHD010.

       START-CRD200.
           INITIALIZE REG-CRD200
           MOVE GS-CONTRATO-CGD010       TO COD-COMPL-CR200
           MOVE COD-COMPL-CR200          TO COMPARA
           MOVE ZEROS                    TO SEQ-CR200

           START CRD200 KEY IS NOT LESS CHAVE-CR200 INVALID KEY
               GO TO SAIR.

           IF ST-CRD200 <> "00" AND "02"
              GO TO START-CRD200.

       READ-CRD200.
           READ CRD200 NEXT RECORD AT END
               GO TO SAIR.

           IF ST-CRD200 <> "00" AND "02"
              GO TO READ-CRD200.

           IF COD-COMPL-CR200 <> COMPARA
               GO TO SAIR.

           MOVE SPACES TO GS-LINDET

           MOVE DATA-MOVTO-CR200 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV         TO DATA-E
           MOVE DATA-E           TO GS-LINDET(6:11)
           MOVE HORA-MOVTO-CR200(1: 2) TO HORA-E(1: 2)
           MOVE ":"                    TO HORA-E(3: 1)
           MOVE HORA-MOVTO-CR200(3: 2) TO HORA-E(4: 2)
           MOVE HORA-E                 TO GS-LINDET(18: 10)
           MOVE USUARIO-CR200          TO GS-LINDET(28: 11)
           MOVE SEQ-CR200              TO GS-LINDET(38: 10)
           MOVE "DATA AGENDADA: "      TO GS-LINDET(52: 15)
           MOVE DATA-RETORNO-CR200     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV               TO DATA-E
           MOVE DATA-E                 TO GS-LINDET(67: 10)
           MOVE "INSERIR-LB3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM CARREGAR-CRD201



           GO TO READ-CRD200.
       SAIR.
           EXIT.

       CARREGAR-CRD201 SECTION.
           MOVE COD-COMPL-CG10   TO COD-COMPL-CR201.
           MOVE SEQ-CR200        TO SEQ-CR201.
           MOVE ZEROS            TO SUBSEQ-CR201.
           START CRD201 KEY IS NOT < CHAVE-CR201 INVALID KEY
                 MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
              READ CRD201 NEXT RECORD AT END MOVE "10" TO ST-CRD201
                   NOT AT END
                     IF COD-COMPL-CR201 <> COD-COMPL-CG10 OR
                        SEQ-CR201 <> SEQ-CR200
                          MOVE "10" TO ST-CRD201
                     ELSE
                        MOVE SPACES TO GS-LINDET
                        MOVE ANOTACAO-CR201 TO GS-LINDET(16: 80)
                        MOVE "INSERIR-LB3" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                     END-IF
              END-READ
           END-PERFORM.

       CHAMAR-ALTERACAO SECTION.
      * LE OS CHEQUES
           MOVE SPACES           TO GS-LINDET
           MOVE 1                TO GS-POSICAO
           MOVE "LER-LB1"        TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINDET = SPACES
               IF GS-LINDET(2:1) = "X"
                  PERFORM ALTERAR-CH010
               END-IF
               ADD 1 TO GS-POSICAO
               MOVE SPACES           TO GS-LINDET
               MOVE "LER-LB1"        TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

      * LE AS DUPLICATAS
           MOVE SPACES           TO GS-LINDET
           MOVE 1                TO GS-POSICAO
           MOVE "LER-LB2"        TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINDET = SPACES
               IF GS-LINDET(2:1) = "X"
                  PERFORM ALTERAR-CRD020
               END-IF
               ADD 1 TO GS-POSICAO
               MOVE SPACES           TO GS-LINDET
               MOVE "LER-LB2"        TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

      * LE AS ANOTAÇÕES
           MOVE SPACES           TO GS-LINDET
           MOVE 1                TO GS-POSICAO
           MOVE "LER-LB3"        TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINDET = SPACES
               IF GS-LINDET(2:1) = "X"
                  PERFORM ALTERAR-CRD200
                  PERFORM ALTERAR-CRD201
               END-IF
               ADD 1 TO GS-POSICAO
               MOVE SPACES           TO GS-LINDET
               MOVE "LER-LB3"        TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

           PERFORM TRANSFERIR-CGD010

           PERFORM APAGAR-CRD020
           PERFORM APAGAR-CRD200

           PERFORM APAGAR-CGD010.

       ALTERAR-CH010 SECTION.
           MOVE GS-LINDET(142:8) TO  DATA-MOVTO-CH10
           MOVE GS-LINDET(138:4) TO  SEQ-CH10
           READ CHD010 INVALID KEY
               MOVE "NÃO ACHEI O CHEQUE" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE GS-LINDET(119:9) TO COD-COMPL-CH10 COD-COMPL-CG10
               READ CGD010 NOT INVALID KEY
                   MOVE COMPRADOR-CG10 TO NOME-CH10
                   MOVE SPACES         TO CIDADE-CH10
                   MOVE CLASSIF-CG10   TO CLASSIF-CG11
                   MOVE CODIGO-CG10    TO CODIGO-CG11
                   READ CGD011 NOT INVALID KEY
                       MOVE CIDADE1-CG11 TO CIDADE
                       READ CAD010 INVALID KEY
                           MOVE SPACES TO CIDADE-CH10
                       NOT INVALID KEY
                           MOVE NOME-CID TO CIDADE-CH10
                       END-READ
                   END-READ
               END-READ
               MOVE REG-CHD010 TO GS-STATUS
               MOVE "ALTERAR-STATUS" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
               REWRITE REG-CHD010 INVALID KEY
                   MOVE "Erro de Regravação...CHD010" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               END-REWRITE.

       ALTERAR-CRD020 SECTION.
           MOVE GS-LINDET(129:9) TO COD-COMPL-CR20
           MOVE GS-LINDET(138:5) TO SEQ-CR20
           READ CRD020 INVALID KEY
               MOVE "Não Encontrei o CRD020" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               STRING GS-LINDET(119:1) GS-LINDET(120:8)
                 INTO COD-COMPL-CR20
               MOVE REG-CRD020 TO GS-STATUS
               MOVE "ALTERAR-STATUS" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
               WRITE REG-CRD020.

       ALTERAR-CRD200 SECTION.
           MOVE GS-CONTRATO-CGD010 TO COD-COMPL-CR200
           MOVE GS-LINDET(38: 5) TO SEQ-CR200
           READ CRD200 INVALID KEY
               MOVE "Não Encontrei o CRD200" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               STRING GS-LINDET(119:1) GS-LINDET(120:8)
                      INTO COD-COMPL-CR200
               MOVE REG-CRD200 TO GS-STATUS
               MOVE "ALTERAR-STATUS" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
               WRITE REG-CRD200.

       ALTERAR-CRD201 SECTION.
           INITIALIZE REG-CRD201
           MOVE GS-CONTRATO-CGD010 TO COD-COMPL-CR201
           MOVE COD-COMPL-CR201          TO COMPARA
           MOVE GS-LINDET(38: 5) TO SEQ-CR201
           START CRD201 KEY IS NOT LESS CHAVE-CR201 INVALID KEY
               MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
               READ CRD201 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD201
               NOT AT END
               IF COMPARA   <> COD-COMPL-CR201 OR
                  SEQ-CR201 <> GS-LINDET(38:5)
                  MOVE "10" TO ST-CRD201
               ELSE
                  MOVE GS-LINDET(119:9) TO COD-COMPL-CR201
                  MOVE REG-CRD201 TO GS-STATUS
                  MOVE "ALTERAR-STATUS" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
                  WRITE REG-CRD201
                  MOVE COMPARA TO COD-COMPL-CR201.

       TRANSFERIR-CGD010 SECTION.
           MOVE GS-CONTRATO-CGD010      TO COD-COMPL-CG10
           READ CGD010 NOT INVALID KEY
               MOVE REG-CGD010           TO REG-AUX
               MOVE GS-CONTRATO-TROCA    TO COD-COMPL-CG10
               READ CGD010 INVALID KEY
                   MOVE REG-AUX              TO REG-CGD010
                   MOVE GS-CONTRATO-TROCA    TO COD-COMPL-CG10
                   MOVE REG-CGD010 TO GS-STATUS
                   MOVE "ALTERAR-STATUS" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   WRITE REG-CGD010
                   END-WRITE
               NOT INVALID KEY
                   MOVE REG-AUX              TO REG-CGD010
                   MOVE GS-CONTRATO-TROCA    TO COD-COMPL-CG10
                   MOVE REG-CGD010 TO GS-STATUS
                   MOVE "ALTERAR-STATUS" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   REWRITE REG-CGD010
                   END-REWRITE.

           MOVE GS-CONTRATO-CGD010      TO COD-COMPL-CG11
           READ CGD011 NOT INVALID KEY
               MOVE REG-CGD011           TO REG-AUX
               MOVE GS-CONTRATO-TROCA    TO COD-COMPL-CG11
               READ CGD011 INVALID KEY
                   MOVE REG-AUX              TO REG-CGD011
                   MOVE GS-CONTRATO-TROCA    TO COD-COMPL-CG11
                   MOVE REG-CGD011 TO GS-STATUS
                   MOVE "ALTERAR-STATUS" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   WRITE REG-CGD011
                   END-WRITE
               NOT INVALID KEY
                   MOVE REG-AUX              TO REG-CGD011
                   MOVE GS-CONTRATO-TROCA    TO COD-COMPL-CG11
                   MOVE REG-CGD011 TO GS-STATUS
                   MOVE "ALTERAR-STATUS" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   REWRITE REG-CGD011
                   END-REWRITE.

           MOVE GS-CONTRATO-CGD010(2:8)        TO CODIGO-CG12
           READ CGD012 NOT INVALID KEY
               MOVE REG-CGD012                 TO REG-AUX
               MOVE GS-CONTRATO-TROCA(2:8)     TO CODIGO-CG12
               READ CGD010 INVALID KEY
                   MOVE REG-AUX                TO REG-CGD012
                   MOVE GS-CONTRATO-TROCA(2:8) TO CODIGO-CG12
                   MOVE REG-CGD012             TO GS-STATUS
                   MOVE "ALTERAR-STATUS"       TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   WRITE REG-CGD012
                   END-WRITE
               NOT INVALID KEY
                   MOVE REG-AUX                TO REG-CGD012
                   MOVE GS-CONTRATO-TROCA(2:8) TO CODIGO-CG12
                   MOVE REG-CGD012             TO GS-STATUS
                   MOVE "ALTERAR-STATUS"       TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   REWRITE REG-CGD012
                   END-REWRITE.

       APAGAR-CGD010 SECTION.
           MOVE GS-CONTRATO-CGD010      TO COD-COMPL-CG10
           READ CGD010 NOT INVALID KEY
               MOVE REG-CGD010 TO GS-STATUS
               MOVE "ALTERAR-STATUS" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
               DELETE CGD010.

       APAGAR-CRD020 SECTION.
           MOVE SPACES           TO GS-LINDET
           MOVE 1                TO GS-POSICAO
           MOVE "LER-LB2"        TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINDET = SPACES
               IF GS-LINDET(2:1) = "X"
                  PERFORM APAGA-CRD020
               END-IF
               ADD 1 TO GS-POSICAO
               MOVE SPACES           TO GS-LINDET
               MOVE "LER-LB2"        TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       APAGAR-CRD200 SECTION.
           MOVE SPACES           TO GS-LINDET
           MOVE 1                TO GS-POSICAO
           MOVE "LER-LB3"        TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL GS-LINDET = SPACES
               IF GS-LINDET(2:1) = "X"
                  PERFORM APAGA-CRD200
               END-IF
               ADD 1 TO GS-POSICAO
               MOVE SPACES           TO GS-LINDET
               MOVE "LER-LB3"        TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.



       APAGA-CRD020 SECTION.
           MOVE GS-LINDET(129:9) TO COD-COMPL-CR20
           MOVE GS-LINDET(138:5) TO SEQ-CR20
           READ CRD020 INVALID KEY
               MOVE "Não Encontrei o CRD020 para Apagar" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               MOVE REG-CRD020 TO GS-STATUS
               MOVE "ALTERAR-STATUS" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
               DELETE CRD020.

       APAGA-CRD200 SECTION.
           INITIALIZE REG-CRD201
           MOVE GS-CONTRATO-CGD010       TO COD-COMPL-CR201
           MOVE COD-COMPL-CR201          TO COMPARA
           MOVE GS-LINDET(38: 5) TO SEQ-CR201
           START CRD201 KEY IS NOT LESS CHAVE-CR201 INVALID KEY
               MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
               READ CRD201 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD201
               NOT AT END
                   IF COD-COMPL-CR201 <> COMPARA OR
                      GS-LINDET(38:5) <> SEQ-CR201
                      MOVE "10" TO ST-CRD201
                   ELSE
                      MOVE REG-CRD201 TO GS-STATUS
                      MOVE "ALTERAR-STATUS" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      DELETE CRD201.

           INITIALIZE REG-CRD200
           MOVE GS-CONTRATO-CGD010  TO COD-COMPL-CR200
           MOVE GS-LINDET(38: 5)    TO SEQ-CR200
           READ CRD200 INVALID KEY
               MOVE "Não Encontrei o CRD200" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
               DELETE CRD200.

       APAGA-CRD201 SECTION.
           INITIALIZE REG-CRD201
           MOVE GS-LINDET(129:9) TO COD-COMPL-CR201 COMPARA
           START CRD201 KEY IS NOT LESS CHAVE-CR201 INVALID KEY
               MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
               READ CRD201 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD201
               NOT AT END
                   IF COD-COMPL-CR201 <> COMPARA
                      MOVE "10" TO ST-CRD201
                   ELSE
                      MOVE REG-CRD201 TO GS-STATUS
                      MOVE "ALTERAR-STATUS" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      DELETE CRD201.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.


       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "GALHO24" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.

       FINALIZAR-PROGRAMA SECTION.
           CLOSE MTD019 CGD010 CRD020 CRD200 CRD201 CHD010 CHD012
                 CGD011 CGD012
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM
           STOP RUN.



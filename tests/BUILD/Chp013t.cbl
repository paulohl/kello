       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHP013T.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 13/03/2004.
      *FUNÇÃO: Movimento de CHEQUES DEVOLVIDO

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX002.
           COPY CAPX004.
           COPY CAPX018.
           COPY CRPX001.
           COPY CHPX010.
           COPY CHPX013.
           COPY CRPX200.
           COPY CRPX201.
           COPY RCPX100.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW002.
       COPY CAPW004.
       COPY CAPW018.
       COPY CRPW001.
       COPY CHPW010.
       COPY CHPW013.
       COPY CRPW200.
       COPY CRPW201.
       COPY RCPW100.
       WORKING-STORAGE SECTION.
           COPY "CHP013T.CPB".
           COPY "CHP013T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CRD001             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-CHD013             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  WS-DATA-SISTEMA       PIC X(21)    VALUE SPACES.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  DATAWI.
               10  ANO-WI            PIC 9(4).
               10  MES-WI            PIC 99.
               10  DIA-WI            PIC 99.
           05  DATA-WI REDEFINES DATAWI PIC 9(8).
           05  DATAWII.
               10  ANO-WII           PIC 9(4).
               10  MES-WII           PIC 99.
               10  DIA-WII           PIC 99.
           05  DATA-WII REDEFINES DATAWII PIC 9(8).
      * DATA-WII - Encontrar proxima data caso a data de vencto da conta
      * permanente seja invalida, por exemplo 30/02/1998
           05  DATA-E                PIC 99/99/9999.
           05  CODIGO-E              PIC Z.ZZ.ZZ.ZZ.


           05  VALOR-E               PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(5)     VALUE ZEROS.
           05  FLAG-ACESSO           PIC 9(01)    VALUE 3.
               88  ACESSO-MOVTO                   VALUE 1.
               88  ACESSO-DEVOLV                  VALUE 2.
               88  ACESSO-NEGAD                   VALUE 3.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       LINKAGE SECTION.
           COPY "PARAMETR".
       01  STRING-1               PIC X(65) VALUE SPACES.

       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
      *    ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           MOVE STRING-1(20: 5) TO USUARIO-W.
           MOVE STRING-1(26: 3) TO COD-USUARIO-W
           ACCEPT DATA6-W FROM DATE.
           ACCEPT HORA-BRA FROM TIME.
           MOVE DATA6-W TO DATA-INV(3: 6).
           IF DATA6-W < 20
              MOVE 20 TO DATA-INV (1: 2)
           ELSE MOVE 19 TO DATA-INV (1: 2).
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I DATA-MOVTO-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD001.
           MOVE "CHD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "CHD013" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD013.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "RCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100.

           OPEN I-O CHD010 CHD013.
           OPEN INPUT CAD018 CAD002 CRD001 RCD100 CAD004
           IF ST-CHD010 = "35"
              CLOSE CHD010      OPEN OUTPUT CHD010
              CLOSE CHD010      OPEN I-O CHD010
           END-IF.

           IF ST-CHD013 = "35"
              CLOSE CHD013      OPEN OUTPUT CHD013
              CLOSE CHD013      OPEN I-O CHD013
           END-IF.
           IF ST-CAD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD001 <> "00"
              MOVE "ERRO ABERTURA CRD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD010 <> "00"
              MOVE "ERRO ABERTURA CHD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CHD013 <> "00"
              MOVE "ERRO ABERTURA CHD013: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CHD013 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           PERFORM VALIDAR-ACESSO.


           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       VALIDAR-ACESSO SECTION.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004.
           MOVE "SENHA12"      TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
               MOVE "S"                TO GS-DESABILITA
           NOT INVALID KEY
               MOVE "N"                TO GS-DESABILITA.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-DEVOLVIDO-FLG-TRUE
                    MOVE USUARIO-W TO DIGITADOR-CH10

                    PERFORM GRAVAR-DADOS-DEVOLVIDO
                    PERFORM CANCELA-DEVOLVIDO
               WHEN GS-EXCLUI-DEVOLVIDO-TRUE
                    PERFORM EXCLUI-DEVOLVIDO
               WHEN GS-CARREGAR-DADOS-DEV-TRUE
                    PERFORM CARREGAR-DADOS-DEVOLVIDO
               WHEN GS-BAIXAR-DEVOLVIDO-TRUE
                    PERFORM BAIXAR-CHEQUE-DEVOLVIDO
               WHEN GS-LE-PORTADOR-TRUE
                   PERFORM LE-PORTADOR
               WHEN GS-LE-SITUACAO-TIT-TRUE
                   PERFORM LE-SITUACAO-TIT
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle win1 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 3 CALL "CAP018T" USING PASSAR-PARAMETROS
                    CANCEL "CAP018T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-DESCR-PORTADOR
                    MOVE PASSAR-STRING-1(33: 2) TO GS-PORTADOR
             WHEN 5 CALL "CRP001T" USING PASSAR-PARAMETROS
                    CANCEL "CRP001T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-DESCR-SITUACAO-TIT
                    MOVE PASSAR-STRING-1(33: 2) TO GS-SITUACAO-TIT
           END-EVALUATE.

       EXCLUI-DEVOLVIDO SECTION.
           MOVE DATA-MOVTO-CH10         TO DATA-MOVTO-CH13
           MOVE SEQ-CH10                TO SEQ-CH13

           READ CHD013 INVALID KEY
                CONTINUE

              NOT INVALID KEY
                  DELETE CHD013
           END-READ.
           MOVE 0                       TO SITUACAO-CH10
           REWRITE REG-CHD010
           END-REWRITE.

       BAIXAR-CHEQUE-DEVOLVIDO SECTION.
           MOVE 2                       TO SITUACAO-CH10

           PERFORM GRAVAR-DADOS-DEVOLVIDO.
       CARREGAR-DADOS-DEVOLVIDO SECTION.
           MOVE STRING-1(1: 8)          TO DATA-MOVTO-CH10
                                           DATA-MOVTO-CH13
           MOVE STRING-1(10: 4)         TO SEQ-CH10
                                           SEQ-CH13
           READ CHD010 INVALID KEY
              MOVE "NÃO ENCONTRADO CH10:  "  TO GS-MENSAGEM-ERRO
              MOVE DATA-MOVTO-CH10           TO GS-MENSAGEM-ERRO(22: 8)
              MOVE SEQ-CH10                  TO GS-MENSAGEM-ERRO(31: 4)
              PERFORM CARREGA-MENSAGEM-ERRO
             NOT INVALID KEY
              MOVE PORTADOR-CH10             TO GS-PORTADOR
                                                PORTADOR
              READ CAD018 INVALID KEY MOVE "******" TO GS-DESCR-PORTADOR
                NOT INVALID KEY
                    MOVE NOME-PORT           TO GS-DESCR-PORTADOR
              END-READ
              MOVE SITUACAO-TIT-CH10         TO GS-SITUACAO-TIT
                                                CODIGO-CR01
              READ CRD001 INVALID KEY
                   MOVE "****"               TO GS-DESCR-SITUACAO-TIT
                 NOT INVALID KEY
                   MOVE SITUACAO-TIT-CR01    TO GS-DESCR-SITUACAO-TIT
              END-READ
              MOVE VALOR-CH10                TO GS-VLR-PRINCIPAL
              MOVE SITUACAO-CH10             TO GS-SITUACAO
              EVALUATE SITUACAO-CH10
                  WHEN 0
                       MOVE "OK           "  TO GS-STATUS-ATUAL
                  WHEN 2
                       MOVE "RECEBIDO     "  TO GS-STATUS-ATUAL
                  WHEN 3
                       MOVE "ESTORNADO    "  TO GS-STATUS-ATUAL
                  WHEN 4
                       MOVE "CANCELADO    "  TO GS-STATUS-ATUAL
                  WHEN 5
                       MOVE "DEVOLVIDO    "  TO GS-STATUS-ATUAL
                  WHEN 6
                       MOVE "PROBLEMATICO "  TO GS-STATUS-ATUAL
                  WHEN OTHER
                       MOVE "*********    "  TO GS-STATUS-ATUAL
              END-EVALUATE
           END-READ

           READ CHD013 INVALID KEY
               INITIALIZE GS-ALINEA-D
                          GS-DATA-COMPRA-D
                          GS-DATA-ENTRADA-D
                          GS-DATA-APRES-D
                          GS-DATA-REPRES-D
                          GS-DATA-RECTO-D
                          GS-VLR-JUROS-D
                          GS-VLR-MULTA-D
                          GS-VLR-DESCONTO-D
                          GS-FORMA-PAGTO-D
                          GS-DCR-MEM-D
               PERFORM CARREGAR-DATAS-DEVOLVIDO
           NOT INVALID KEY
               MOVE ALINEA-CH13          TO GS-ALINEA-D

               MOVE DATA-COMPRA-CH13     TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV             TO GS-DATA-COMPRA-D

               MOVE DATA-ENTRADA-CH13    TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV             TO GS-DATA-ENTRADA-D

               MOVE DATA-APRES-CH13      TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV             TO GS-DATA-APRES-D

               MOVE DATA-REAPRES-CH13    TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV             TO GS-DATA-REPRES-D

               MOVE DATA-RECTO-CH13      TO DATA-INV
               CALL "GRIDAT1" USING DATA-INV
               MOVE DATA-INV             TO GS-DATA-RECTO-D

               MOVE VLR-JUROS-CH13       TO GS-VLR-JUROS-D
               MOVE VLR-MULTA-CH13       TO GS-VLR-MULTA-D
               MOVE VLR-DESCONTO-CH13    TO GS-VLR-DESCONTO-D
               MOVE FORMA-PAGTO-CH13     TO GS-FORMA-PAGTO-D
               MOVE DCR-MEM-CH13         TO GS-DCR-MEM-D
           END-READ.
       CARREGAR-DATAS-DEVOLVIDO SECTION.
           move function current-date to ws-data-sistema
           MOVE WS-DATA-SISTEMA(1: 8)         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV                      TO GS-DATA-ENTRADA-D
           MOVE CLIENTE-CH10                  TO ALBUM-REC
           READ RCD100 INVALID KEY MOVE ZEROS TO GS-DATA-COMPRA-D
                NOT INVALID KEY
                    MOVE DATAVEN-REC          TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV             TO GS-DATA-COMPRA-D
           END-READ.

       CANCELA-DEVOLVIDO SECTION.
           IF SITUACAO-CH10 = 5
              CONTINUE
           ELSE
              MOVE 5                    TO SITUACAO-CH10
              PERFORM ANOTACAO-DEVOLVIDO
           END-IF
           REWRITE REG-CHD010.
       ANOTACAO-DEVOLVIDO SECTION.
           OPEN I-O CRD200 CRD201.
           IF ST-CRD200 = "35"  CLOSE CRD200  OPEN OUTPUT CRD200
                                CLOSE CRD200  OPEN I-O CRD200.
           IF ST-CRD201 = "35"  CLOSE CRD201  OPEN OUTPUT CRD201
                                CLOSE CRD201  OPEN I-O CRD201.
           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           MOVE COD-COMPL-CH10 TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
             READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
               NOT AT END
                 IF COD-COMPL-CR200 <> COD-COMPL-CH10
                              MOVE "10" TO ST-CRD200
                 ELSE MOVE SEQ-CR200 TO ULT-SEQ
                      CONTINUE
             END-READ
           END-PERFORM.
           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ      TO SEQ-CR200
           MOVE COD-COMPL-CH10 TO COD-COMPL-CR200.
           MOVE ZEROS        TO DATA-RETORNO-CR200
           MOVE USUARIO-W    TO USUARIO-CR200
           MOVE DATA-DIA-I   TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                 ADD 1 TO SEQ-CR200
                 CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200       TO SEQ-CR201.
           MOVE COD-COMPL-CR200 TO COD-COMPL-CR201.
           MOVE 1               TO SUBSEQ-CR201.
           MOVE "A T E N C A O - CHEQUE DEVOLVIDO NR: "
                  TO ANOTACAO-CR201(1: 37)
           MOVE NR-CHEQUE-CH10  TO ANOTACAO-CR201(38: 08)
           MOVE NOME-CH10       TO ANOTACAO-CR201(50: 30)
           MOVE ZEROS TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
                   ADD 1 TO SUBSEQ-CR201
                   CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.
           ADD 1 TO SUBSEQ-CR201
           MOVE SPACES           TO ANOTACAO-CR201
           MOVE "MOVTO: "        TO ANOTACAO-CR201(1: 07)
           MOVE DATA-MOVTO-CH10  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV         TO DATA-E
           MOVE DATA-E           TO ANOTACAO-CR201(08: 11)
           MOVE "VENCTO:"        TO ANOTACAO-CR201(23: 08)
           MOVE DATA-VENCTO-CH10 TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV         TO DATA-E
           MOVE DATA-E           TO ANOTACAO-CR201(31: 11)
           MOVE "VALOR: "        TO ANOTACAO-CR201(42: 7)
           MOVE VALOR-CH10       TO VALOR-E
           MOVE VALOR-E          TO ANOTACAO-CR201(49: 13)
           MOVE ZEROS TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
                   ADD 1 TO SUBSEQ-CR201
                   CONTINUE
               NOT INVALID KEY MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.
           CLOSE CRD200 CRD201.

       GRAVAR-DADOS-DEVOLVIDO SECTION.
           MOVE DATA-MOVTO-CH10         TO DATA-MOVTO-CH13
           MOVE SEQ-CH10                TO SEQ-CH13

           READ CHD013 INVALID KEY
                PERFORM MOVER-DADOS-DEVOLVIDO
                WRITE REG-CHD013
                END-WRITE
              NOT INVALID KEY
                  PERFORM MOVER-DADOS-DEVOLVIDO
                  REWRITE REG-CHD013
                  END-REWRITE
           END-READ

           REWRITE REG-CHD010.
       MOVER-DADOS-DEVOLVIDO SECTION.
           MOVE GS-ALINEA-D             TO ALINEA-CH13
           MOVE GS-DATA-COMPRA-D        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                TO DATA-COMPRA-CH13

           MOVE GS-DATA-ENTRADA-D       TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                TO DATA-ENTRADA-CH13

           MOVE GS-DATA-APRES-D         TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                TO DATA-APRES-CH13

           MOVE GS-DATA-REPRES-D        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                TO DATA-REAPRES-CH13

           MOVE GS-DATA-RECTO-D         TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV                TO DATA-RECTO-CH13

           MOVE GS-VLR-JUROS-D          TO VLR-JUROS-CH13
           MOVE GS-VLR-MULTA-D          TO VLR-MULTA-CH13
           MOVE GS-VLR-DESCONTO-D       TO VLR-DESCONTO-CH13
           MOVE GS-FORMA-PAGTO-D        TO FORMA-PAGTO-CH13
           MOVE GS-DCR-MEM-D            TO DCR-MEM-CH13

      *    ATUALIZA DADOS DO MOVIMENTO CHD010
           MOVE GS-PORTADOR             TO PORTADOR-CH10
           MOVE GS-SITUACAO-TIT         TO SITUACAO-TIT-CH10.


       LE-PORTADOR SECTION.
           MOVE GS-PORTADOR    TO PORTADOR
           READ CAD018 INVALID KEY MOVE "******" TO NOME-PORT
           END-READ
           MOVE NOME-PORT          TO GS-DESCR-PORTADOR.
       LE-SITUACAO-TIT SECTION.
           MOVE GS-SITUACAO-TIT TO CODIGO-CR01
           READ CRD001 INVALID KEY MOVE "*********" TO
                                       SITUACAO-TIT-CR01
           END-READ
           MOVE SITUACAO-TIT-CR01  TO GS-DESCR-SITUACAO-TIT.


       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CHD010
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.


       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CHP013T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.


       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD018 CAD004 CHD010 CAD002 CHD013 CRD001 RCD100.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

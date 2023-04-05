       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAR001.
       AUTHOR.  ALFREDO SAVIOLLI NETO.
       SECURITY. PARAMETRIZAÇÃO DE CODIGOS REDUZIDOS.
       DATE-WRITTEN. 17-07-2009.
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".

       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CXPX020.

           COPY COPX001.

           COPY COPX003.

           COPY CRPX001.

           COPY PARX001.

           COPY CPARX001.

           COPY CGPX001.

       DATA DIVISION.
       FILE SECTION.

           COPY CXPW020.

           COPY COPW001.

           COPY COPW003.

           COPY CRPW001.

           COPY PARW001.

           COPY CPARW001.

           COPY CGPW001.

       WORKING-STORAGE SECTION.
           COPY "PAR001.CPB".
           COPY "PAR001.CPY".
           COPY "DS-CNTRL.MF".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1   PIC X(55).
       01  PASSAR-STRING.
           05  PASSAR-STRING1        PIC X(60).
       01  VARIAVEIS.
           05  ST-CXD020             PIC X(02).
           05  ST-COD001             PIC X(02).
           05  ST-COD003             PIC X(02).
           05  ST-CRD001             PIC X(02).
           05  ST-PAR001             PIC X(02).
           05  ST-CPAR001            PIC X(02).
           05  ST-CGD001             PIC X(02).
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           COPY "PARAMETR".

       01 DET-01.
          05 DET-CODIGO              PIC 9(05).
          05 FILLER                  PIC X(01).
          05 DET-NOME                PIC X(50).

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 achei               pic x(01) value spaces.
       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.

           INITIALIZE GS-DATA-BLOCK DS-CONTROL-BLOCK
           MOVE ZEROS TO ERRO-W.
           MOVE GS-DATA-BLOCK-VERSION-NO   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO              TO DS-VERSION-NO

           MOVE EMPRESA-W                  TO EMP-REC

           MOVE "CXD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "COD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "CRD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD001.
           MOVE "PAR001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-PAR001.
           MOVE "CPAR001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CPAR001.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.

           OPEN I-O   CXD020 COD001 COD003 CRD001 PAR001 CGD001 CPAR001
           CLOSE      CXD020 COD001 COD003 CRD001 PAR001 CGD001 CPAR001
           OPEN INPUT CXD020 COD001 COD003 CRD001 CGD001

           OPEN I-O   PAR001 CPAR001

           IF ST-CXD020 = "35"
              CLOSE CXD020      OPEN OUTPUT CXD020
              CLOSE CXD020      OPEN I-O    CXD020
           END-IF.
           IF ST-COD001 = "35"
              CLOSE COD001      OPEN OUTPUT COD001
              CLOSE COD001      OPEN I-O    COD001
           END-IF.
           IF ST-COD003 = "35"
              CLOSE COD003      OPEN OUTPUT COD003
              CLOSE COD003      OPEN I-O    COD003
           END-IF.
           IF ST-CRD001 = "35"
              CLOSE CRD001      OPEN OUTPUT CRD001
              CLOSE CRD001      OPEN I-O    CRD001
           END-IF.
           IF ST-PAR001 = "35"
              CLOSE PAR001      OPEN OUTPUT PAR001
              CLOSE PAR001      OPEN I-O    PAR001
           END-IF.
           IF ST-CPAR001 = "35"
              CLOSE CPAR001     OPEN OUTPUT CPAR001
              CLOSE CPAR001     OPEN I-O    CPAR001
           END-IF.
           IF ST-CGD001 = "35"
              CLOSE CGD001      OPEN OUTPUT CGD001
              CLOSE CGD001      OPEN I-O    CGD001
           END-IF.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD001 <> "00"
              MOVE "ERRO ABERTURA CRD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PAR001 <> "00"
              MOVE "ERRO ABERTURA PAR001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-PAR001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPAR001 <> "00"
              MOVE "ERRO ABERTURA CPAR001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CPAR001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           PERFORM LOAD-SCREENSET

           MOVE 1                         TO CHAVE-PAR001
           READ PAR001 INVALID KEY
                INITIALIZE GS-BRINDES
                           GS-CUSTEIOS
                           GS-DESC-BRINDES
                           GS-DESC-CUSTEIOS
                           GS-COLACAO-GRAU
                           GS-DESC-COLACAO-GRAU
                           GS-COD-REVENDIDO
                           GS-DESC-REVENDIDO
                           GS-COD-ANALISE
                           GS-DESC-ANALISE
                           GS-COD-FATURAMENTO
                           GS-DESC-FATURAMENTO
                           GS-STATUS-FATURAMENTO
                           GS-DESC-STATUS-FAT
                           GS-ACP-CAMINHO
                           GS-FORN-CHEQUE
                           GS-DESC-FORN-CHEQUE
                           GS-FORN-RECEBER
                           GS-DESC-FORN-RECEBER
                           GS-CODRED-CHEQUE
                           GS-DESC-CODRED-CHEQUE
                           GS-CODRED-RECEBER
                           GS-DESC-CODRED-RECEBER
                           GS-ACP-CAMINHO-CX

           NOT INVALID KEY
      *>-
                MOVE CODRED-BRINDE-PAR001    TO GS-BRINDES
                                                CODIGO-REDUZ-CX20
                READ CXD020 INVALID KEY
                     MOVE SPACES             TO DESCRICAO-CX20
                END-READ
                MOVE DESCRICAO-CX20          TO GS-DESC-BRINDES
      *>-
                MOVE CODRED-CUSTEIO-PAR001   TO GS-CUSTEIOS
                                                CODIGO-REDUZ-CX20
                READ CXD020 INVALID KEY
                     MOVE SPACES             TO DESCRICAO-CX20
                END-READ
                MOVE DESCRICAO-CX20          TO GS-DESC-CUSTEIOS
      *>-
                MOVE COLACAO-GRAU-PAR001     TO GS-COLACAO-GRAU
                                                CODIGO-CO03
                READ COD003 INVALID KEY
                     MOVE SPACES             TO NOME-CO03
                END-READ
                MOVE NOME-CO03               TO GS-DESC-COLACAO-GRAU

      *>-
                MOVE STATUS-ANALISE-PAR001   TO GS-COD-ANALISE
                                                CODIGO-CO01
                READ COD001 INVALID KEY
                     MOVE SPACES             TO STATUS-CO01
                END-READ
                MOVE STATUS-CO01             TO GS-DESC-ANALISE

      *>-
                MOVE STATUS-REVENDIDO-PAR001 TO GS-COD-REVENDIDO
                                                CODIGO-CO01
                READ COD001 INVALID KEY
                     MOVE SPACES             TO STATUS-CO01
                END-READ
                MOVE STATUS-CO01             TO GS-DESC-REVENDIDO

      *>-
                MOVE CODRED-FATURAMENTO-PAR001 TO GS-COD-FATURAMENTO
                                                  CODIGO-REDUZ-CX20
                READ CXD020 INVALID KEY
                     MOVE SPACES               TO DESCRICAO-CX20
                END-READ
                MOVE DESCRICAO-CX20            TO GS-DESC-FATURAMENTO

      *>-
                MOVE STATUS-TIT-FATURAMENTO-PAR001
                                               TO GS-STATUS-FATURAMENTO
                                                  CODIGO-CR01
                READ CRD001 INVALID KEY
                     MOVE SPACES             TO DESCRICAO-CR01
                END-READ
                MOVE DESCRICAO-CR01          TO GS-DESC-STATUS-FAT

      *>-
                MOVE CAMINHO-IMAGEM-PAR001   TO GS-ACP-CAMINHO

      *>-
                MOVE FORN-CHEQUE-PAR001      TO GS-FORN-CHEQUE
                                                CODIGO-CG01
                READ CGD001 INVALID KEY
                     MOVE "***************"  TO NOME-CG01
                END-READ
                MOVE NOME-CG01               TO GS-DESC-FORN-CHEQUE


      *>-
                MOVE CODRED-CHEQUE-PAR001    TO GS-CODRED-CHEQUE
                                                CODIGO-REDUZ-CX20
                READ CXD020 INVALID KEY
                     MOVE SPACES             TO DESCRICAO-CX20
                END-READ
                MOVE DESCRICAO-CX20          TO GS-DESC-CODRED-CHEQUE


      *>-
                MOVE FORN-RECEBER-PAR001     TO GS-FORN-RECEBER
                                                CODIGO-CG01
                READ CGD001 INVALID KEY
                     MOVE "***************"  TO NOME-CG01
                END-READ
                MOVE NOME-CG01               TO GS-DESC-FORN-RECEBER


      *>-
                MOVE CODRED-RECEBER-PAR001   TO GS-CODRED-RECEBER
                                                CODIGO-REDUZ-CX20
                READ CXD020 INVALID KEY
                     MOVE SPACES             TO DESCRICAO-CX20
                END-READ
                MOVE DESCRICAO-CX20          TO GS-DESC-CODRED-RECEBER

           END-READ
           MOVE 1                            TO CHAVE-CPAR001
           READ CPAR001 INVALID KEY
                INITIALIZE GS-ACP-CAMINHO-CX

                MOVE "APAGAR-LB" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                MOVE CAMINHO-IMAGEM-CX-CPAR001 TO GS-ACP-CAMINHO-CX
                PERFORM CARREGAR-COLACOES
           END-READ.

       CARREGAR-COLACOES SECTION.
           MOVE "APAGAR-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE 0 TO GS-LINHA
           PERFORM 30 TIMES
               ADD 1 TO GS-LINHA
               IF TAB-COLACAO(GS-LINHA) IS NOT NUMERIC
                  MOVE 0 TO TAB-COLACAO(GS-LINHA)
               END-IF

               IF TAB-COLACAO(GS-LINHA) > 0
                  MOVE TAB-COLACAO(GS-LINHA) TO CODIGO-CO03
                  READ COD003 INVALID KEY
                       MOVE SPACES           TO NOME-CO03
                  END-READ
                  IF NOME-CO03 <> SPACES
                     PERFORM INSERIR-COLACAO
                  END-IF
               ELSE
                  EXIT PERFORM
               END-IF
           END-PERFORM.


       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-LER-TRUE
                    PERFORM LER
               WHEN GS-POPUP-TRUE
                    PERFORM POPUP
               WHEN GS-SALVAR-TRUE
                    PERFORM SALVAR-PARAMETRIZACAO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER SECTION.
           EVALUATE GS-CAMPO
               WHEN "1"  PERFORM LER-CAMPO1
               WHEN "2"  PERFORM LER-CAMPO2
               WHEN "3"  PERFORM LER-CAMPO3
               WHEN "4"  PERFORM LER-CAMPO4
               WHEN "5"  PERFORM LER-CAMPO5
               WHEN "6"  PERFORM LER-CAMPO6
               WHEN "8"  PERFORM LER-CAMPO8
               WHEN "9"  PERFORM LER-CAMPO9
               WHEN "10" PERFORM LER-CAMPO10
               WHEN "11" PERFORM LER-CAMPO11
               WHEN "12" PERFORM LER-CAMPO12
           END-EVALUATE.

       LER-CAMPO1 SECTION.
           MOVE GS-BRINDES           TO CODIGO-REDUZ-CX20
           READ CXD020 INVALID KEY
                MOVE SPACES          TO DESCRICAO-CX20
           END-READ
           MOVE DESCRICAO-CX20       TO GS-DESC-BRINDES
           REFRESH-OBJECT PRINCIPAL.

       LER-CAMPO2 SECTION.
           MOVE GS-COLACAO-GRAU      TO CODIGO-CO03
           READ COD003 INVALID KEY
                MOVE SPACES          TO NOME-CO03
           END-READ
           MOVE NOME-CO03            TO GS-DESC-COLACAO-GRAU
           REFRESH-OBJECT PRINCIPAL

           IF NOME-CO03 = SPACES
              MOVE "Colação não encontrada" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              PERFORM VERIFICAR-IGUAL

              IF ACHEI = "N"
                 MOVE SPACES TO MENSAGEM
                 STRING "Inserir a Colação" x"0a"
                        nome-co03 into mensagem
                 MOVE "Q" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM

                 IF RESP-MSG = "S"
                    PERFORM INSERIR-COLACAO.

       LER-CAMPO3 SECTION.
           MOVE GS-COD-ANALISE       TO CODIGO-CO01
           READ COD001 INVALID KEY
                MOVE SPACES          TO STATUS-CO01
           END-READ
           MOVE STATUS-CO01          TO GS-DESC-ANALISE.
           REFRESH-OBJECT PRINCIPAL.

       LER-CAMPO4 SECTION.
           MOVE GS-COD-REVENDIDO     TO CODIGO-CO01
           READ COD001 INVALID KEY
                MOVE SPACES          TO STATUS-CO01
           END-READ
           MOVE STATUS-CO01          TO GS-DESC-REVENDIDO.
           REFRESH-OBJECT PRINCIPAL.

       LER-CAMPO5 SECTION.
           MOVE GS-COD-FATURAMENTO   TO CODIGO-REDUZ-CX20
           READ CXD020 INVALID KEY
                MOVE SPACES          TO DESCRICAO-CX20
           END-READ
           MOVE DESCRICAO-CX20       TO GS-DESC-FATURAMENTO.
           REFRESH-OBJECT PRINCIPAL.

       LER-CAMPO6 SECTION.
           MOVE GS-STATUS-FATURAMENTO TO CODIGO-CR01
           READ CRD001 INVALID KEY
                MOVE SPACES           TO DESCRICAO-CR01
           END-READ
           MOVE DESCRICAO-CR01        TO GS-DESC-STATUS-FAT.
           REFRESH-OBJECT PRINCIPAL.

       LER-CAMPO8 SECTION.
           MOVE GS-FORN-CHEQUE TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES    TO NOME-CG01
           END-READ
           MOVE NOME-CG01      TO GS-DESC-FORN-CHEQUE
           REFRESH-OBJECT PRINCIPAL.

       LER-CAMPO9 SECTION.
           MOVE GS-CODRED-CHEQUE     TO CODIGO-REDUZ-CX20
           READ CXD020 INVALID KEY
                MOVE SPACES          TO DESCRICAO-CX20
           END-READ
           MOVE DESCRICAO-CX20       TO GS-DESC-CODRED-CHEQUE.
           REFRESH-OBJECT PRINCIPAL.

       LER-CAMPO10 SECTION.
           MOVE GS-FORN-RECEBER TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES    TO NOME-CG01
           END-READ
           MOVE NOME-CG01      TO GS-DESC-FORN-RECEBER
           REFRESH-OBJECT PRINCIPAL.

       LER-CAMPO11 SECTION.
           MOVE GS-CODRED-RECEBER    TO CODIGO-REDUZ-CX20
           READ CXD020 INVALID KEY
                MOVE SPACES          TO DESCRICAO-CX20
           END-READ
           MOVE DESCRICAO-CX20       TO GS-DESC-CODRED-RECEBER.
           REFRESH-OBJECT PRINCIPAL.

       LER-CAMPO12 SECTION.
           MOVE GS-CUSTEIOS          TO CODIGO-REDUZ-CX20
           READ CXD020 INVALID KEY
                MOVE SPACES          TO DESCRICAO-CX20
           END-READ
           MOVE DESCRICAO-CX20       TO GS-DESC-CUSTEIOS
           REFRESH-OBJECT PRINCIPAL.


       POPUP SECTION.
           EVALUATE GS-CAMPO
               WHEN "1"  PERFORM POPUP-CAMPO1
               WHEN "2"  PERFORM POPUP-CAMPO2
               WHEN "3"  PERFORM POPUP-CAMPO3
               WHEN "4"  PERFORM POPUP-CAMPO4
               WHEN "5"  PERFORM POPUP-CAMPO5
               WHEN "6"  PERFORM POPUP-CAMPO6
               WHEN "8"  PERFORM POPUP-CAMPO8
               WHEN "9"  PERFORM POPUP-CAMPO9
               WHEN "10" PERFORM POPUP-CAMPO10
               WHEN "11" PERFORM POPUP-CAMPO11
               WHEN "12" PERFORM POPUP-CAMPO12
           END-EVALUATE.

       POPUP-CAMPO1 SECTION.
           CALL "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           MOVE PASSAR-PARAMETROS(52: 5) TO GS-BRINDES
           CANCEL "CXP020T"
           PERFORM LER-CAMPO1.

       POPUP-CAMPO2 SECTION.
           CALL "COP003T" USING PARAMETROS-W PASSAR-PARAMETROS
           MOVE PASSAR-PARAMETROS(33: 5) TO GS-COLACAO-GRAU
           CANCEL "COP003T"
           PERFORM LER-CAMPO2.

       POPUP-CAMPO3 SECTION.
           CALL   "COP001T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "COP001T"
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-ANALISE
           MOVE PASSAR-STRING-1(33: 2) TO GS-COD-ANALISE.


       POPUP-CAMPO4 SECTION.
           CALL   "COP001T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "COP001T"
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-REVENDIDO
           MOVE PASSAR-STRING-1(33: 2) TO GS-COD-REVENDIDO.

       POPUP-CAMPO5 SECTION.
           CALL "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           MOVE PASSAR-PARAMETROS(52: 5) TO GS-COD-FATURAMENTO
           CANCEL "CXP020T"
           PERFORM LER-CAMPO5.

       POPUP-CAMPO6 SECTION.
           CALL   "CRP001T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CRP001T"
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-STATUS-FAT
           MOVE PASSAR-STRING-1(33: 2) TO GS-STATUS-FATURAMENTO.

       POPUP-CAMPO8 SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING
           CANCEL "CGP001T"
           MOVE PASSAR-STRING(33: 6) TO GS-FORN-CHEQUE
           PERFORM LER-CAMPO8.

       POPUP-CAMPO9 SECTION.
           CALL "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           MOVE PASSAR-PARAMETROS(52: 5) TO GS-CODRED-CHEQUE
           CANCEL "CXP020T"
           PERFORM LER-CAMPO9.

       POPUP-CAMPO10 SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING
           CANCEL "CGP001T"
           MOVE PASSAR-STRING(33: 6) TO GS-FORN-RECEBER
           PERFORM LER-CAMPO10.

       POPUP-CAMPO11 SECTION.
           CALL "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           MOVE PASSAR-PARAMETROS(52: 5) TO GS-CODRED-RECEBER
           CANCEL "CXP020T"
           PERFORM LER-CAMPO11.

       POPUP-CAMPO12 SECTION.
           CALL "CXP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           MOVE PASSAR-PARAMETROS(52: 5) TO GS-CUSTEIOS
           CANCEL "CXP020T"
           PERFORM LER-CAMPO12.

       VERIFICAR-IGUAL SECTION.
           MOVE "N"          TO ACHEI

           MOVE 1            TO GS-LINHA
           MOVE SPACES       TO GS-LINHA-DETALHE
           MOVE "LER-LB"     TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-DETALHE = SPACES
               MOVE GS-LINHA-DETALHE TO DET-01

               IF DET-CODIGO = GS-COLACAO-GRAU
                  MOVE "Código já informado" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM
                  MOVE "S" TO ACHEI
                  EXIT PERFORM
               ELSE
                  ADD 1             TO GS-LINHA
                  MOVE SPACES       TO GS-LINHA-DETALHE
                  MOVE "LER-LB"     TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
               END-IF
           END-PERFORM

           IF GS-LINHA > 30
              MOVE "Quantidade máxima alcançada avisar" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
              MOVE "S" TO ACHEI.


       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       SALVAR-PARAMETRIZACAO SECTION.
           MOVE 1                    TO CHAVE-PAR001
           READ PAR001 INVALID KEY
                MOVE GS-BRINDES            TO CODRED-BRINDE-PAR001
                MOVE GS-CUSTEIOS           TO CODRED-CUSTEIO-PAR001
                MOVE GS-COLACAO-GRAU       TO COLACAO-GRAU-PAR001
                MOVE GS-COD-ANALISE        TO STATUS-ANALISE-PAR001
                MOVE GS-COD-REVENDIDO      TO STATUS-REVENDIDO-PAR001
                MOVE GS-COD-FATURAMENTO    TO CODRED-FATURAMENTO-PAR001
                MOVE GS-STATUS-FATURAMENTO TO
                     STATUS-TIT-FATURAMENTO-PAR001
                MOVE GS-ACP-CAMINHO        TO CAMINHO-IMAGEM-PAR001
                MOVE GS-FORN-CHEQUE        TO FORN-CHEQUE-PAR001
                MOVE GS-CODRED-CHEQUE      TO CODRED-CHEQUE-PAR001
                MOVE GS-FORN-RECEBER       TO FORN-RECEBER-PAR001
                MOVE GS-CODRED-RECEBER     TO CODRED-RECEBER-PAR001
                WRITE REG-PAR001
                END-WRITE
           NOT INVALID KEY
                MOVE GS-BRINDES            TO CODRED-BRINDE-PAR001
                MOVE GS-CUSTEIOS           TO CODRED-CUSTEIO-PAR001
                MOVE GS-COLACAO-GRAU       TO COLACAO-GRAU-PAR001
                MOVE GS-COD-ANALISE        TO STATUS-ANALISE-PAR001
                MOVE GS-COD-REVENDIDO      TO STATUS-REVENDIDO-PAR001
                MOVE GS-COD-FATURAMENTO    TO CODRED-FATURAMENTO-PAR001
                MOVE GS-STATUS-FATURAMENTO TO
                     STATUS-TIT-FATURAMENTO-PAR001
                MOVE GS-ACP-CAMINHO        TO CAMINHO-IMAGEM-PAR001
                MOVE GS-FORN-CHEQUE        TO FORN-CHEQUE-PAR001
                MOVE GS-CODRED-CHEQUE      TO CODRED-CHEQUE-PAR001
                MOVE GS-FORN-RECEBER       TO FORN-RECEBER-PAR001
                MOVE GS-CODRED-RECEBER     TO CODRED-RECEBER-PAR001
                REWRITE REG-PAR001
                END-REWRITE.

           MOVE 1                          TO CHAVE-CPAR001
           READ CPAR001 INVALID KEY
                MOVE GS-ACP-CAMINHO-CX     TO CAMINHO-IMAGEM-CX-CPAR001
                PERFORM MOVER-COLACAO
                WRITE REG-CPAR001
                END-WRITE
           NOT INVALID KEY
                MOVE GS-ACP-CAMINHO-CX     TO CAMINHO-IMAGEM-CX-CPAR001
                PERFORM MOVER-COLACAO
                REWRITE REG-CPAR001
                END-REWRITE.

       MOVER-COLACAO SECTION.
           MOVE 0 TO GS-LINHA
           PERFORM 30 TIMES
               ADD  1        TO GS-LINHA
               MOVE SPACES   TO GS-LINHA-DETALHE
               MOVE "LER-LB" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM

               IF GS-LINHA-DETALHE = SPACES
                  MOVE ZEROS            TO DET-CODIGO
               ELSE
                  MOVE GS-LINHA-DETALHE TO DET-01
               END-IF

               MOVE DET-CODIGO TO TAB-COLACAO(GS-LINHA)
           END-PERFORM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       INSERIR-COLACAO SECTION.
           MOVE CODIGO-CO03        TO DET-CODIGO
           MOVE NOME-CO03          TO DET-NOME
           MOVE DET-01             TO GS-LINHA-DETALHE
           MOVE "INSERIR-LB"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
      *  INICIALIZA UMA NOVA PILHA
           MOVE DS-NEW-SET TO DS-CONTROL
           MOVE "PAR001" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
               IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CXD020 PAR001 COD001 COD003 CRD001 CGD001 CPAR001
           STOP RUN.

         IDENTIFICATION DIVISION.
       PROGRAM-ID. CGP001.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 30/07/1998
      *exclui conta fixa 3325 e 3326
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CGPX001.
           COPY CGPX002.
           COPY CGPX003.
           COPY CGPX004.
           COPY CGPX005.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CGPW001.
       COPY CGPW002.
       COPY CGPW003.
       COPY CGPW004.
       COPY CGPW005.
       WORKING-STORAGE SECTION.
           COPY "GALHO10.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD002             PIC XX       VALUE SPACES.
           05  ST-CGD003             PIC XX       VALUE SPACES.
           05  ST-CGD004             PIC XX       VALUE SPACES.
           05  ST-CGD005             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD014             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-RED002             PIC XX       VALUE SPACES.
           05  ST-RED006             PIC XX       VALUE SPACES.
           05  CARACTERISTICA-W      PIC 9        VALUE ZEROS.
      *   tipo de ordem de impressão
           05  IMPRIME-W             PIC 9        VALUE ZEROS.
      *  IMPRIME-W = 0 (o registro nao faz parte da opcao) e = 1 Sim
           05  ULT-CODIGO            PIC 9(6)     VALUE ZEROS.
      *    Ult-codigo - será utilizado p/ encontrar o último código
      *    de CADASTRO utilizado
           05  GRAVA-W               PIC 9        VALUE ZEROS.
      *    Grava = 0 (regravar)   grava = 1 (gravar)
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
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


       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
      *    PERFORM INICIALIZA-PROGRAMA.
      *    PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
      *    GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
      *    COPY "CBDATA1.CPY".
      *    MOVE ZEROS TO PAG-W ERRO-W.
      *    INITIALIZE CGP001-DATA-BLOCK
      *    INITIALIZE DS-CONTROL-BLOCK
      *    MOVE CGP001-DATA-BLOCK-VERSION-NO
      *                            TO DS-DATA-BLOCK-VERSION-NO
      *    MOVE CGP001-VERSION-NO  TO DS-VERSION-NO

           OPEN INPUT CONTROLE
      *    READ CONTROLE
      *    MOVE NOME-EMP           TO EMPRESA-REL
      *    MOVE EMPRESA            TO EMP-REC

           MOVE 999                TO EMP-REC
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD002.
           MOVE "CGD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD003.
           MOVE "CGD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD004.
           MOVE "CGD005" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD005.
           MOVE ZEROS TO ERRO-W.
           OPEN I-O CGD001 CGD002 CGD003 CGD004 CGD005.
           CLOSE CONTROLE.
      *    IF ST-CGD001 = "35"
      *       CLOSE CGD001      OPEN OUTPUT CGD001
      *       CLOSE CGD001      OPEN I-O CGD001
      *    END-IF.
      *    IF ST-CGD002 = "35"
      *       CLOSE CGD002      OPEN OUTPUT CGD002
      *       CLOSE CGD002      OPEN I-O CGD002
      *    END-IF.
      *    IF ST-CGD003 = "35"
      *       CLOSE CGD003      OPEN OUTPUT CGD003
      *       CLOSE CGD003      OPEN I-O CGD003
      *    END-IF.
      *    IF ST-CGD004 = "35"
      *       CLOSE CGD004      OPEN OUTPUT CGD004
      *       CLOSE CGD004      OPEN I-O CGD004.
      *    IF ST-CGD005 = "35"
      *       CLOSE CGD005      OPEN OUTPUT CGD005
      *       CLOSE CGD005      OPEN I-O CGD005.
      *    IF ST-CGD001 <> "00"
      *       MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
      *       MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
      *    IF ST-CGD002 <> "00"
      *       MOVE "ERRO ABERTURA CGD002: "  TO GS-MENSAGEM-ERRO
      *       MOVE ST-CGD002 TO GS-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
      *    IF ST-CGD003 <> "00"
      *       MOVE "ERRO ABERTURA CGD003: "  TO GS-MENSAGEM-ERRO
      *       MOVE ST-CGD003 TO GS-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
      *    IF ST-CGD004 <> "00"
      *       MOVE "ERRO ABERTURA CGD004: "  TO GS-MENSAGEM-ERRO
      *       MOVE ST-CGD004 TO GS-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
      *    IF ST-CGD005 <> "00"
      *       MOVE "ERRO ABERTURA CGD005: "  TO GS-MENSAGEM-ERRO
      *       MOVE ST-CGD005 TO GS-MENSAGEM-ERRO(23: 02)
      *       PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
      *    IF COD-USUARIO-W NOT NUMERIC
      *       MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
      *       PERFORM CARREGA-MENSAGEM-ERRO.
      *
      **    IF ERRO-W = ZEROS
      *       PERFORM LOAD-SCREENSET.


      *CORPO-PROGRAMA SECTION.
      *    EVALUATE TRUE
      *        WHEN GS-APAGA-DADOS-TRUE
      *             PERFORM APAGA-DADOS
      *    END-EVALUATE
      *
      *    SET GS-EXIT-FLG   TO TRUE
      *    PERFORM CALL-DIALOG-SYSTEM.

      *LOAD-SCREENSET SECTION.
      *    MOVE DS-NEW-SET TO DS-CONTROL
      *    MOVE "GALHO10" TO DS-SET-NAME
      *    PERFORM CALL-DIALOG-SYSTEM.

      *CARREGA-MENSAGEM-ERRO SECTION.
      *    PERFORM LOAD-SCREENSET.
      *    MOVE "EXIBE-ERRO" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *    MOVE 1 TO ERRO-W.

       APAGA-DADOS SECTION.
           MOVE 3325           TO CODIGO-CG01
           READ CGD001 INVALID KEY
                               CONTINUE
                   NOT INVALID KEY
                        DELETE CGD001
           END-READ


           MOVE 3326           TO CODIGO-CG01
           READ CGD001 INVALID KEY
                               CONTINUE
                   NOT INVALID KEY
                        DELETE CGD001
           END-READ

           MOVE 3325           TO CODIGO-CG02
           READ CGD002 INVALID KEY
                               CONTINUE
                   NOT INVALID KEY
                        DELETE CGD002
           END-READ

           MOVE 3326           TO CODIGO-CG02
           READ CGD002 INVALID KEY
                               CONTINUE
                   NOT INVALID KEY
                        DELETE CGD002
           END-READ

           MOVE 3325           TO CODIGO-CG03
           READ CGD003 INVALID KEY
                               CONTINUE
                   NOT INVALID KEY
                        DELETE CGD003
           END-READ

           MOVE 3326           TO CODIGO-CG03
           READ CGD003 INVALID KEY
                               CONTINUE
                   NOT INVALID KEY
                        DELETE CGD003
           END-READ


           MOVE 3325           TO CODIGO-CG04
           READ CGD004 INVALID KEY
                               CONTINUE
                   NOT INVALID KEY
                        DELETE CGD004
           END-READ

           MOVE 3326           TO CODIGO-CG04
           READ CGD004 INVALID KEY
                               CONTINUE
                   NOT INVALID KEY
                        DELETE CGD004
           END-READ


           MOVE 3325           TO CODIGO-CG05
           READ CGD005 INVALID KEY
                               CONTINUE
                   NOT INVALID KEY
                        DELETE CGD005
           END-READ

           MOVE 3326           TO CODIGO-CG05
           READ CGD005 INVALID KEY
                               CONTINUE
                   NOT INVALID KEY
                        DELETE CGD005
           END-READ.


      *    MOVE 3325           TO CODIGO-CG01
      *    WRITE REG-CGD001 INVALID KEY
      *          CONTINUE
      *    END-WRITE.
      *CALL-DIALOG-SYSTEM SECTION.
      *    CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
      *    IF NOT DS-NO-ERROR
      *       MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
      *       DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
      *      GO FINALIZAR-PROGRAMA
      *    END-IF.


       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 CGD002 CGD003 CGD004 CGD005.
           EXIT PROGRAM.

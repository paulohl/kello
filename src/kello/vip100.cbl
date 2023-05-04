       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIP100.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 27/07/2000.
      *FUNÇÃO: Movimento de RECEBIMENTO DE FITAS BRUTAS

       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY COPX003.
           COPY COPX040.
           COPY CGPX001.
           COPY VIPX020.
           COPY VIPX100.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
           COPY CAPW004.
           COPY COPW003.
           COPY COPW040.
           COPY CGPW001.
           COPY VIPW020.
           COPY VIPW100.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "VIP100.CPB".
           COPY "VIP100.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-VID020             PIC XX       VALUE SPACES.
           05  ST-VID100             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  ULT-NR-FITA           PIC 9(5)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 mensagem                 pic x(200).
       01 tipo-msg                 pic x(01).
       01 resp-msg                 pic x(01).


       01  CAB01.
           05  EMPRESA-REL         PIC X(64)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(43)   VALUE
           "CONFERENCIA-MOVTO RECTO DE FITAS".
           05  FILLER              PIC X(07)   VALUE "MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(07)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(86)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(86)   VALUE
           "SEQ FITA      CONT CURSO      EVENT-DESCRICAO  DTA-EVENTO LO
      -    "CAL CINEGRAFISTA FILMADORA".

       01  LINDET.
           05  LINDET-REL          PIC X(84)   VALUE SPACES.

           copy impressora.

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "VID020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID020.
           MOVE "VID100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID100.
           OPEN I-O   VID100
           CLOSE      VID100
           OPEN INPUT VID100

           OPEN INPUT CGD001 COD003 COD040 VID020.
           IF ST-VID100 = "35"
              CLOSE VID100      OPEN OUTPUT VID100
              CLOSE VID100      OPEN I-O VID100
           END-IF.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID020 <> "00"
              MOVE "ERRO ABERTURA VID020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID100 <> "00"
              MOVE "ERRO ABERTURA VID100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0
              PERFORM LOAD-SCREENSET.


       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   IF GS-TIPO-GRAVACAO = 1
                      PERFORM REGRAVA-DADOS
                   ELSE
                      PERFORM GRAVA-DADOS
                   END-IF
                   PERFORM LIMPAR-DADOS
                   PERFORM CARREGA-ULTIMOS
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
                   PERFORM CARREGA-ULTIMOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM CARREGA-ULTIMOS
               WHEN GS-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE DATA-MOVTO-I     TO DATA-MOVTO-V100
                   MOVE GS-LINDET(1: 3)  TO SEQ-V100
                   IF GS-LINDET(90:1) = "X"
                      MOVE SPACES TO GS-LINDET(90:1)
                   ELSE
                      MOVE "X"    TO GS-LINDET(90:1)
                   END-IF
                   MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-CONTRATO-TRUE
                   PERFORM LE-CONTRATO
               WHEN GS-LE-CONTRATO-NOV-TRUE
                   PERFORM LE-CONTRATO2
               WHEN GS-LE-EVENTO-TRUE
                   PERFORM LE-EVENTO
               WHEN GS-LE-CINEGRAFIST-TRUE
                   PERFORM LE-CINEGRAFISTA
               WHEN GS-LE-FILMADORA-TRUE
                   PERFORM LE-FILMADORA
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
               WHEN GS-LE-CONTRATO-IMP-TRUE
                   PERFORM LE-CONTRATO-IMP
               WHEN GS-VALIDA-NR-FITA-TRUE
                   PERFORM VALIDAR-NR-FITA
               WHEN GS-TRANSF-CONTRATO-TRUE
                   PERFORM TRANSFERENCIA-CONTRATO
               WHEN GS-SELECAO-TRUE
                   PERFORM VERIFICAR-SELECAO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          OPEN INPUT CAD004
          MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
          MOVE "SENHA49"          TO PROGRAMA-CA004
          READ CAD004 INVALID KEY
              DISABLE-OBJECT PB9
              DISABLE-OBJECT CB1
          NOT INVALID KEY
              ENABLE-OBJECT PB9
              ENABLE-OBJECT CB1.

          CLOSE CAD004.


       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(22: 10) TO GS-DESC-CONTRATO
                    MOVE "-"         TO GS-DESC-CONTRATO(11: 1)
                    MOVE PASSAR-STRING-1(33: 10)
                         TO GS-DESC-CONTRATO(12: 10)
                    MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO
                    PERFORM VERIFICA-ULT-NR-FITA
             WHEN 3 CALL   "COP003T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP003T"
                    MOVE PASSAR-STRING-1(1: 20) TO GS-NOME-EVENTO
                    MOVE PASSAR-STRING-1(33: 5) TO GS-EVENTO
             WHEN 4 CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CGP001T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-CINEGRAFISTA
                    MOVE PASSAR-STRING-1(33: 6) TO GS-CINEGRAFISTA
             WHEN 5 CALL   "VIP020T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "VIP020T"
                    MOVE PASSAR-STRING-1(1: 20) TO GS-NOME-FILMADORA
                    MOVE PASSAR-STRING-1(33: 2) TO GS-FILMADORA
             WHEN 6 CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(22: 10) TO GS-DESC-CONTRATO-IMP
                    MOVE "-"         TO GS-DESC-CONTRATO-IMP(11: 1)
                    MOVE PASSAR-STRING-1(33: 10)
                         TO GS-DESC-CONTRATO-IMP(12: 10)
                    MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO-IMP
                    PERFORM VERIFICA-ULT-NR-FITA
             WHEN 7 CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(22: 10) TO GS-DESC-CONTRATO2
                    MOVE "-"         TO GS-DESC-CONTRATO2(11: 1)
                    MOVE PASSAR-STRING-1(33: 10)
                         TO GS-DESC-CONTRATO2(12: 10)
                    MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO-NOVO
             WHEN OTHER CONTINUE
           END-EVALUATE.
      *----------------------------------------------------------------
       LE-CINEGRAFISTA SECTION.
           MOVE GS-CINEGRAFISTA    TO CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE "********"    TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-CINEGRAFISTA.
       LE-EVENTO SECTION.
           MOVE GS-EVENTO          TO CODIGO-CO03.
           READ COD003 INVALID KEY
                MOVE "********"    TO NOME-CO03.
           MOVE NOME-CO03          TO GS-NOME-EVENTO.
       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY
                MOVE "******"      TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO
           IF GS-TIPO-GRAVACAO = 0
              PERFORM VERIFICA-ULT-NR-FITA.

       LE-CONTRATO2 SECTION.
           MOVE GS-CONTRATO-NOVO   TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY
                MOVE "******"      TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO2.

       LE-FILMADORA SECTION.
           MOVE GS-FILMADORA       TO CODIGO-V20.
           READ VID020 INVALID KEY
                MOVE SPACES        TO DESCRICAO-V20.
           MOVE DESCRICAO-V20      TO GS-NOME-FILMADORA.
       LE-CONTRATO-IMP SECTION.
           MOVE GS-CONTRATO-IMP    TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY
                MOVE "******"      TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-DESC-CONTRATO-IMP.

      *--------------------------------------------------------------
       CARREGAR-DADOS SECTION.
           START VID100 KEY IS = CHAVE-V100 INVALID KEY
                CONTINUE.
           READ VID100 INVALID KEY
                INITIALIZE REG-VID100.

           MOVE DATA-MOVTO-W         TO GS-DATA-MOVTO
           MOVE CONTRATO-V100        TO GS-CONTRATO
                                        NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE SPACES          TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40   TO GS-DESC-CONTRATO.
           MOVE SEQ-V100             TO GS-SEQ
           MOVE EVENTO-V100          TO GS-EVENTO
                                        CODIGO-CO03
           READ COD003 INVALID KEY MOVE
                SPACES               TO NOME-CO03.
           MOVE NOME-CO03            TO GS-NOME-EVENTO
           MOVE CURSO-V100           TO GS-NOME-CURSO
           MOVE CINEGRAFISTA-V100    TO GS-CINEGRAFISTA
                                        CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES          TO NOME-CG01.
           MOVE NOME-CG01            TO GS-NOME-CINEGRAFISTA.
           MOVE DATA-EVENTO-V100     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO GS-DATA-EVENTO
           MOVE IDENTIFICADOR-V100   TO GS-IDENTIFICADOR
           MOVE LOCALIZACAO-V100     TO GS-LOCALIZACAO
           MOVE NR-FITA-V100         TO GS-NR-FITA.
           IF QTDE-ARQUIVOS-V100 IS NOT NUMERIC
              MOVE 0 TO QTDE-ARQUIVOS-V100
           END-IF
           MOVE QTDE-ARQUIVOS-V100   TO GS-QTDE-ARQ
           MOVE FILMADORA-V100       TO GS-FILMADORA
                                        CODIGO-V20
           READ VID020 INVALID KEY
                MOVE SPACES          TO DESCRICAO-V20.
           MOVE DESCRICAO-V20        TO GS-NOME-FILMADORA.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-VID100
           INITIALIZE GS-DATA-BLOCK
           MOVE ULT-SEQ TO GS-SEQ
           MOVE ULT-NR-FITA TO GS-NR-FITA
           MOVE DATA-MOVTO-W TO GS-DATA-MOVTO.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

       EXCLUI SECTION.
           CLOSE      VID100
           OPEN I-O   VID100

           DELETE VID100.

           CLOSE      VID100
           OPEN INPUT VID100
           PERFORM LIMPAR-DADOS.
      *    PERFORM CARREGA-ULTIMOS.

       VALIDAR-NR-FITA SECTION.
           IF GS-IDENTIFICADOR = 0
              PERFORM VERIFICA-ULT-NR-FITA
           END-IF.

       SALVAR-DADOS SECTION.
           IF GS-TIPO-GRAVACAO = 0
              PERFORM VERIFICA-ULT-NR-FITA
           END-IF
           MOVE DATA-MOVTO-I          TO DATA-MOVTO-V100
           MOVE GS-SEQ                TO SEQ-V100
           MOVE GS-CONTRATO           TO CONTRATO-V100
           MOVE GS-EVENTO             TO EVENTO-V100
           MOVE GS-NOME-CURSO         TO CURSO-V100
           MOVE GS-CINEGRAFISTA       TO CINEGRAFISTA-V100
           MOVE GS-FILMADORA          TO FILMADORA-V100.
           MOVE GS-LOCALIZACAO        TO LOCALIZACAO-V100
           MOVE GS-DATA-EVENTO        TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO DATA-EVENTO-V100
           MOVE GS-NR-FITA            TO NR-FITA-V100
           MOVE GS-QTDE-ARQ           TO QTDE-ARQUIVOS-V100
           MOVE USUARIO-W             TO DIGITADOR-V100
           MOVE GS-IDENTIFICADOR      TO IDENTIFICADOR-V100.
       GRAVA-DADOS SECTION.
           CLOSE      VID100
           OPEN I-O   VID100
           MOVE ZEROS TO ST-VID100.
           PERFORM UNTIL ST-VID100 = "10"
                WRITE REG-VID100 INVALID KEY
                    ADD 1 TO SEQ-V100
                NOT INVALID KEY
                    MOVE "10" TO ST-VID100
                END-WRITE
           END-PERFORM
           CLOSE      VID100
           OPEN INPUT VID100
           PERFORM MOVER-DADOS-LISTA.
           PERFORM VERIFICA-ULT-NR-FITA *> TEM QUE VERIFICAR POIS
      *      NEM SEMPRE SERÁ ACRESCENTADO 1 AUTOMÁTICO P/ NR-FITA
      *      POIS PODE HAVER MAIS DE 1 CONTRATO P/ O MESMO NR-FITA

      *    ADD 1 TO ULT-SEQ ULT-NR-FITA.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           CLOSE    VID100
           OPEN I-O VID100
           REWRITE REG-VID100 INVALID KEY
                 MOVE "Erro Regravacao VID100" TO GS-MENSAGEM-ERRO
                 MOVE ST-VID100 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           END-REWRITE
           CLOSE      VID100
           OPEN INPUT VID100
           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           INITIALIZE REG-VID100
           MOVE GS-DATA-MOVTO   TO DATA-MOVTO-W DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV        TO DATA-MOVTO-I DATA-MOVTO-V100.
           MOVE ZEROS           TO SEQ-V100
                                   GS-SEQ
           START VID100 KEY IS NOT < CHAVE-V100
                    INVALID KEY MOVE "10" TO ST-VID100.

           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-VID100 = "10"
                 READ VID100 NEXT RECORD AT END
                      MOVE "10" TO ST-VID100
                 NOT AT END
                      IF DATA-MOVTO-V100 <> DATA-MOVTO-I
                         MOVE "10" TO ST-VID100
                      ELSE
                         PERFORM MOVER-DADOS-LISTA
                         MOVE SEQ-V100      TO GS-SEQ
                         MOVE "INSERE-LIST" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                 END-READ
           END-PERFORM
           ADD 1 TO GS-SEQ.
      *    PERFORM VERIFICA-ULT-NR-FITA.
       VERIFICA-ULT-NR-FITA SECTION.
      *    PROCURA ULTIMO NR DE FITA
           MOVE GS-CONTRATO TO CONTRATO-V100
           MOVE 99999       TO NR-FITA-V100
           MOVE ZEROS       TO DATA-EVENTO-V100
                               GS-NR-FITA.
           START VID100 KEY IS LESS THAN ALT-V100 INVALID KEY
                 MOVE "10" TO ST-VID100.
           PERFORM UNTIL ST-VID100 = "10"
                 READ VID100 PREVIOUS AT END
                      MOVE "10" TO ST-VID100
                 NOT AT END
                      IF GS-CONTRATO <> CONTRATO-V100
                         MOVE "10" TO ST-VID100
                      ELSE
                         MOVE NR-FITA-V100    TO GS-NR-FITA
                         MOVE "10"            TO ST-VID100
                      END-IF
                 END-READ
           END-PERFORM.

           ADD 1           TO GS-NR-FITA

           INITIALIZE REG-VID100
           MOVE GS-DATA-MOVTO   TO DATA-MOVTO-W DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV        TO DATA-MOVTO-I DATA-MOVTO-V100.
           MOVE ZEROS           TO SEQ-V100
                                   GS-SEQ
           START VID100 KEY IS NOT < CHAVE-V100
                    INVALID KEY MOVE "10" TO ST-VID100.

           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-VID100 = "10"
                 READ VID100 NEXT RECORD AT END
                      MOVE "10" TO ST-VID100
                 NOT AT END
                      IF DATA-MOVTO-V100 <> DATA-MOVTO-I
                         MOVE "10" TO ST-VID100
                      ELSE
                         MOVE SEQ-V100      TO GS-SEQ
                      END-IF
                 END-READ
           END-PERFORM.
           ADD 1 TO GS-SEQ.

           IF GS-IDENTIFICADOR = 0
              STRING GS-CONTRATO GS-NR-FITA INTO GS-IDENTIFICADOR.

       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE SEQ-V100           TO GS-LINDET(1:04)
           MOVE IDENTIFICADOR-V100 TO GS-LINDET(5:10)
           MOVE CONTRATO-V100      TO GS-LINDET(15: 5)
           MOVE CURSO-V100         TO GS-LINDET(21: 10)
           MOVE EVENTO-V100        TO GS-LINDET(31: 6)
                                      CODIGO-CO03
           READ COD003 INVALID KEY
                MOVE SPACES        TO NOME-CO03.
           MOVE NOME-CO03          TO GS-LINDET(37: 10)
           MOVE DATA-EVENTO-V100   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET(48: 11)
           MOVE LOCALIZACAO-V100   TO GS-LINDET(59: 6)
           MOVE CINEGRAFISTA-V100  TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES        TO NOME-CG01.
           MOVE NOME-CG01          TO GS-LINDET(65: 12)
           MOVE FILMADORA-V100     TO CODIGO-V20.
           READ VID020 INVALID KEY
                MOVE SPACES        TO DESCRICAO-V20.
           MOVE DESCRICAO-V20      TO GS-LINDET(78: 10).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "VIP100" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.

           MOVE ZEROS TO PAG-W.

           copy condensa.

           MOVE DATA-MOVTO-I   TO DATA-MOVTO-V100.
           MOVE ZEROS          TO SEQ-V100.
           START VID100 KEY IS NOT < CHAVE-V100 INVALID KEY
                 MOVE "10" TO ST-VID100.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-VID100 = "10"
             READ VID100 NEXT RECORD AT END MOVE "10" TO ST-VID100
              NOT AT END
                IF DATA-MOVTO-V100 <> DATA-MOVTO-I
                         MOVE "10" TO ST-VID100
                ELSE
                  IF GS-CONTRATO-IMP <> ZEROS
                     IF CONTRATO-V100 <> GS-CONTRATO-IMP
                        CONTINUE
                     ELSE
                      PERFORM MOVER-DADOS-REL
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56 PERFORM CABECALHO
                      END-IF
                     END-IF
                  ELSE
                      PERFORM MOVER-DADOS-REL
                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56 PERFORM CABECALHO
                      END-IF
                END-IF
             END-READ
           END-PERFORM.

           copy descondensa.


       MOVER-DADOS-REL SECTION.
           MOVE SPACES             TO LINDET-REL
           MOVE SEQ-V100           TO LINDET-REL(1:04)
           MOVE NR-FITA-V100       TO LINDET-REL(5:10)
           MOVE CONTRATO-V100      TO LINDET-REL(15: 5)
           MOVE CURSO-V100         TO LINDET-REL(19: 10)
           MOVE EVENTO-V100        TO LINDET-REL(31: 6)
                                      CODIGO-CO03
           READ COD003 INVALID KEY
                MOVE SPACES        TO NOME-CO03.
           MOVE NOME-CO03          TO LINDET-REL(37: 10)
           MOVE DATA-EVENTO-V100   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO LINDET-REL(48: 11)
           MOVE LOCALIZACAO-V100   TO LINDET-REL(59: 6)
           MOVE CINEGRAFISTA-V100  TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES        TO NOME-CG01.
           MOVE NOME-CG01          TO LINDET-REL(65: 12)
           MOVE FILMADORA-V100     TO CODIGO-V20.
           READ VID020 INVALID KEY
                MOVE SPACES        TO DESCRICAO-V20.
           MOVE DESCRICAO-V20      TO LINDET-REL(78: 10).


       CABECALHO SECTION.
           MOVE DATA-MOVTO-W    TO DATA-MOVTO-REL.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 5 TO LIN.

       TRANSFERENCIA-CONTRATO SECTION.
           CLOSE      VID100
           OPEN I-O   VID100

           MOVE 1         TO GS-CONT
           MOVE SPACES    TO GS-LINDET
           MOVE "LE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINDET = SPACES
               IF GS-LINDET(90:1) = "X"
                  STRING GS-DATA-MOVTO(5:4)
                         GS-DATA-MOVTO(3:2)
                         GS-DATA-MOVTO(1:2)
                    INTO DATA-MOVTO-V100
                  MOVE FUNCTION NUMVAL(GS-LINDET(1:3)) TO SEQ-V100
                  READ VID100 INVALID KEY
                      MOVE "Não Encontrei o Lançamento" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                  NOT INVALID KEY
                      MOVE GS-CONTRATO-NOVO TO CONTRATO-V100
                      REWRITE REG-VID100 INVALID KEY
                          MOVE "Erro de Regravação...VID100" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                      END-REWRITE
                  END-READ
               END-IF
               ADD 1          TO GS-CONT
               MOVE SPACES    TO GS-LINDET
               MOVE "LE-LIST" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      VID100
           OPEN INPUT VID100.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       VERIFICAR-SELECAO SECTION.
           MOVE 1         TO GS-CONT
           MOVE SPACES    TO GS-LINDET
           MOVE "LE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINDET = SPACES
               IF GS-MARCAR = 0
                  MOVE SPACES      TO GS-LINDET(90:1)
               ELSE
                  MOVE "X"         TO GS-LINDET(90:1)
               END-IF

               MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM

               ADD 1          TO GS-CONT
               MOVE SPACES    TO GS-LINDET
               MOVE "LE-LIST" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD003 COD040 VID100 VID020 CGD001.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

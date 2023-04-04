       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP230.
      *DATA: 20/06/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RELATORIO DE AVALIAÇÃO DE FOTOS NA MONTAGEM
      *FUNÇÃO: Listar todos os trabalhos que estiverem dentro do
      *        intervalo de data solicitado. Poderá ser geral ou indiv.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY COPX040.
           COPY LBPX030.
           COPY LBPX021.
           COPY LBPX100.
           COPY LBPX106.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS CONTRATO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS FOTOGRAFO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TIPO-PROB-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY COPW040.
       COPY LBPW030.
       COPY LBPW021.
       COPY LBPW100.
       COPY LBPW106.

       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(4).
           05  FOTOGRAFO-WK        PIC X(20).
           05  COD-FOTOGRAFO-WK    PIC 9(6).
           05  CONTRATO-WK         PIC 9(4).
           05  QT-FILME-WK         PIC 9(4).
           05  QT-FOTO-WK          PIC 9(6).
           05  FOTO-PROB-WK        PIC 9(4).
           05  TIPO-PROB-WK        PIC X(30).
           05  PERC-WK             PIC 99V99.
           05  ALBUM-WK            PIC X(5).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LBP230.CPB".
           COPY "LBP230.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-LBD030             PIC XX       VALUE SPACES.
           05  ST-LBD021             PIC XX       VALUE SPACES.
           05  ST-LBD100             PIC XX       VALUE SPACES.
           05  ST-LBD106             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  DATA-INI-ANT          PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM-ANT          PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  PERC-E                PIC ZZ,ZZ    BLANK WHEN ZEROS.
      *    OPCAO INDIVIDUAL
           05  GRAVAR-OPCAO          PIC 9        VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  FOTOGRAFO-W           PIC 9(6)     VALUE ZEROS.
           05  TIPO-PROB-W           PIC 9(3)     VALUE ZEROS.

      *    CONTROLE DE QUEBRA
           05  FOTOGRAFO-ANT         PIC X(20)    VALUE SPACES.
           05  TIPO-PROB-ANT         PIC X(30)    VALUE SPACES.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
      *    TOTALIZA VARIAVEIS
           05  QT-FOTOS-W            PIC 9(6)     VALUE ZEROS.
           05  QT-FILME-W            PIC 9(6)     VALUE ZEROS.
           05  TOTAL-FILME           PIC 9(7)     VALUE ZEROS.
           05  TOTAL-FOTO            PIC 9(7)     VALUE ZEROS.
           05  TOTAL-FOTO-PROB       PIC 9(8)     VALUE ZEROS.
           05  TOTAL-FILME-G         PIC 9(7)     VALUE ZEROS.
           05  TOTAL-FOTO-G          PIC 9(8)     VALUE ZEROS.
           05  TOTAL-FOTO-PROB-G     PIC 9(7)     VALUE ZEROS.
           05  QTDE-E                PIC ZZZZ.ZZZ.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(70)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(44)   VALUE
           "AVALIACAO DE FOTOS MONTAGEM-PROBLEMA-ORDEM:".
           05  ORDEM-REL           PIC X(17)   VALUE SPACES.
           05  FILLER              PIC X(10)    VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "INT.DATA MOVTO: ".
           05  DATA-INI-REL        PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  DATA-FIM-REL        PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110) VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "CODIGO-FOTOGRAFO            CONT TIP-PROBLEMA
      -    "        QT-FILM QT-FOT-PROB  PERC(%)   ALBUM".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "LBD030"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD030.
           MOVE "LBD021"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD021.
           MOVE "LBD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD100.
           MOVE "LBD106"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD106.
           OPEN INPUT CGD001 COD040 LBD030 LBD021 LBD100 LBD106.
           IF ST-LBD030 <> "00"
              MOVE "ERRO ABERTURA LBD030: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD030 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD021 <> "00"
              MOVE "ERRO ABERTURA LBD021: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD106 <> "00"
              MOVE "ERRO ABERTURA LBD106: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD106 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD100 <> "00"
              MOVE "ERRO ABERTURA LBD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-LER-INDIVIDUAL-TRUE
                    PERFORM LER-CODIGOS
               WHEN GS-OPCAO-POP-UP-TRUE
                    PERFORM LER-POPUP
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LER-CODIGOS SECTION.
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 1 MOVE GS-CODIGO TO CODIGO-CG01
                 READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                 END-READ
                 MOVE NOME-CG01    TO GS-DESCRICAO
             WHEN 2 MOVE GS-CODIGO TO CODIGO-LB30
                 READ LBD030 INVALID KEY MOVE SPACES TO DESCRICAO-LB30
                 END-READ
                 MOVE DESCRICAO-LB30 TO GS-DESCRICAO
             WHEN 3 MOVE GS-CODIGO TO NR-CONTRATO-CO40
                 READ COD040 INVALID KEY
                      MOVE SPACES TO IDENTIFICACAO-CO40
                 END-READ
                 MOVE IDENTIFICACAO-CO40 TO GS-DESCRICAO
           END-EVALUATE.

       LER-POPUP SECTION.
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 1 CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CGP001T"
                    MOVE PASSAR-STRING-1(1: 30)  TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(33: 6)  TO GS-CODIGO
             WHEN 2 CALL   "LBP030T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "LBP030T"
                    MOVE PASSAR-STRING-1(1: 30)  TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(33: 3)  TO GS-CODIGO
             WHEN 3 CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(22: 11)  TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(52: 4)  TO GS-CODIGO
           END-EVALUATE.

       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-DATA-INI    TO DATA-INV DATA-INI-ANT DATA-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-INI.
           MOVE GS-DATA-FIM    TO DATA-INV DATA-FIM-ANT DATA-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-FIM.
      *    VERIFICA OPCAO DO RELATORIO - INDIV OU GERAL
           MOVE DATA-INI       TO DATA-MOVTO-L106
           MOVE GS-CODIGO      TO CONTRATO-W FOTOGRAFO-W TIPO-PROB-W
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 0 MOVE ZEROS TO SEQ-L106
                    START LBD106 KEY IS NOT < CHAVE-L106 INVALID KEY
                          MOVE "10" TO ST-LBD106
             WHEN 1 MOVE FOTOGRAFO-W TO FOTOGRAFO-L106
                    START LBD106 KEY IS NOT < ALT2-L106 INVALID KEY
                          MOVE "10" TO ST-LBD106
             WHEN 2 MOVE TIPO-PROB-W TO TIPO-PROBLEMA-L106
                    START LBD106 KEY IS NOT < ALT3-L106 INVALID KEY
                          MOVE "10" TO ST-LBD106
             WHEN 3 MOVE CONTRATO-W TO CONTRATO-L106
                    START LBD106 KEY IS NOT < ALT1-L106 INVALID KEY
                          MOVE "10" TO ST-LBD106
           END-EVALUATE.

           MOVE ZEROS          TO SEQ-WK.
           PERFORM UNTIL ST-LBD106 = "10"
             READ LBD106 NEXT RECORD AT END MOVE "10" TO ST-LBD106
              NOT AT END
               IF DATA-MOVTO-L106 > DATA-FIM
                  MOVE "10" TO ST-LBD106
               ELSE
                PERFORM VERIFICA-OPCAO
                IF GRAVAR-OPCAO = ZEROS MOVE "10" TO ST-LBD106
                                       CONTINUE
                ELSE
                 ADD 1                   TO SEQ-WK
                 MOVE DATA-MOVTO-L106    TO GS-EXIBE-VENCTO
                 MOVE "TELA-AGUARDA1"    TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 MOVE FOTOGRAFO-L106   TO CODIGO-CG01 COD-FOTOGRAFO-WK
                 READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                 END-READ
                 MOVE NOME-CG01          TO FOTOGRAFO-WK
                 MOVE CONTRATO-L106      TO CONTRATO-WK
                 MOVE QT-FOTOG-PROB-L106 TO FOTO-PROB-WK

                 MOVE TIPO-PROBLEMA-L106 TO CODIGO-LB30
                 READ LBD030 INVALID KEY MOVE SPACES TO DESCRICAO-LB30
                 END-READ
                 MOVE DESCRICAO-LB30     TO TIPO-PROB-WK
                 PERFORM VERIFICA-QTDE-FILME
                 MOVE QT-FILME-W         TO QT-FILME-WK
                 MOVE QT-FOTOS-W         TO QT-FOTO-WK
                 COMPUTE PERC-WK = (FOTO-PROB-WK * 100) / QT-FOTO-WK

                 EVALUATE ALBUM-L106
                   WHEN 0 MOVE "0-NÃO"   TO ALBUM-WK
                   WHEN 1 MOVE "1-SIM"   TO ALBUM-WK
                 END-EVALUATE
                 WRITE REG-WORK
                END-IF
               END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       VERIFICA-QTDE-FILME SECTION.
           MOVE ZEROS    TO QT-FILME-W QT-FOTOS-W
           MOVE DATA-INI TO DATA-MOVTO-L100.
           MOVE CONTRATO-L106  TO CONTRATO-L100.
           MOVE FOTOGRAFO-L106 TO FOTOGRAFO-L100.
           START LBD100 KEY IS NOT < ALT1-L100 INVALID KEY
                 MOVE "10" TO ST-LBD100.
           PERFORM UNTIL ST-LBD100 = "10"
             READ LBD100 NEXT RECORD AT END MOVE "10" TO ST-LBD100
               NOT AT END
                 IF CONTRATO-L100 <> CONTRATO-L106 OR
                    FOTOGRAFO-L100 <> FOTOGRAFO-L106
                      MOVE "10" TO ST-LBD100
                 ELSE
                   ADD QTDE-FILMES-L100  TO QT-FILME-W
                   MOVE TIPO-FILME-L100 TO CODIGO-LB21
                   READ LBD021 INVALID KEY MOVE ZEROS TO QT-FOTOS-LB21
                   END-READ
                   COMPUTE QT-FOTOS-W = (QTDE-FILMES-L100 *
                      QT-FOTOS-LB21) + QT-FOTOS-W
                 END-IF
             END-READ
           END-PERFORM.
       VERIFICA-OPCAO SECTION.
           MOVE GS-CODIGO TO CONTRATO-W FOTOGRAFO-W TIPO-PROB-W.
           MOVE 0 TO GRAVAR-OPCAO
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 0 MOVE 1 TO GRAVAR-OPCAO
             WHEN 1 IF FOTOGRAFO-W = FOTOGRAFO-L106
                       MOVE 1 TO GRAVAR-OPCAO
             WHEN 2 IF TIPO-PROB-W = TIPO-PROBLEMA-L106
                       MOVE 1 TO GRAVAR-OPCAO
             WHEN 3 IF CONTRATO-W = CONTRATO-L106
                       MOVE 1 TO GRAVAR-OPCAO
           END-EVALUATE.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           PERFORM ZERA-VARIAVEIS.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                  PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA
           MOVE "TOTAL GERAL: " TO GS-LINDET(1: 30)
           MOVE TOTAL-FILME-G          TO QTDE-E
           MOVE QTDE-E                 TO GS-LINDET(68: 8)
           MOVE TOTAL-FOTO-PROB-G      TO QTDE-E
           MOVE QTDE-E                 TO GS-LINDET(80: 12)
           COMPUTE PERC-E = (TOTAL-FOTO-PROB-G * 100) / TOTAL-FOTO-G
           MOVE PERC-E                 TO GS-LINDET(92: 10).

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       ORDEM SECTION.
           INITIALIZE REG-WORK.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "FOTOGRAFO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < FOTOGRAFO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "CONTRATO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "TIPO-PROBLEMA " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < TIPO-PROB-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF FOTOGRAFO-ANT  NOT = SPACES
                 IF FOTOGRAFO-ANT NOT = FOTOGRAFO-WK
                    PERFORM TOTALIZA
             WHEN 2
              IF CONTRATO-ANT NOT = SPACES
                 IF CONTRATO-ANT NOT = CONTRATO-WK
                    PERFORM TOTALIZA
             WHEN 3
              IF TIPO-PROB-ANT NOT = SPACES
                 IF TIPO-PROB-ANT NOT = TIPO-PROB-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE COD-FOTOGRAFO-WK       TO GS-LINDET(1: 7)
           MOVE FOTOGRAFO-WK           TO GS-LINDET(8: 21)
           MOVE CONTRATO-WK            TO GS-LINDET(29: 5)
           MOVE TIPO-PROB-WK           TO GS-LINDET(34: 31)
           MOVE QT-FILME-WK            TO QTDE-E
           ADD QT-FILME-WK             TO TOTAL-FILME
           ADD QT-FOTO-WK              TO TOTAL-FOTO
           MOVE QTDE-E                 TO GS-LINDET(68: 8)
           MOVE FOTO-PROB-WK           TO QTDE-E
           MOVE QTDE-E                 TO GS-LINDET(80: 12)
           ADD FOTO-PROB-WK            TO TOTAL-FOTO-PROB
           MOVE PERC-WK                TO PERC-E
           MOVE PERC-E                 TO GS-LINDET(92: 8).
           MOVE ALBUM-WK               TO GS-LINDET(100: 5).

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO CONTRATO-ANT.
           MOVE SPACES TO FOTOGRAFO-ANT TIPO-PROB-ANT.
           MOVE ZEROS TO TOTAL-FOTO-G TOTAL-FOTO-PROB-G TOTAL-FILME-G.
           PERFORM ZERA-SUBTOTAL.

       ZERA-SUBTOTAL SECTION.
           MOVE ZEROS TO TOTAL-FOTO TOTAL-FOTO-PROB TOTAL-FILME.

       MOVER-CHAVE-ANT SECTION.
           MOVE FOTOGRAFO-WK         TO FOTOGRAFO-ANT.
           MOVE TIPO-PROB-WK         TO TIPO-PROB-ANT
           MOVE CONTRATO-WK          TO CONTRATO-ANT.
       TOTALIZA SECTION.
           MOVE SPACES                 TO GS-LINDET
           MOVE TOTAL-FILME            TO QTDE-E
           MOVE QTDE-E                 TO GS-LINDET(68: 8)
           MOVE TOTAL-FOTO-PROB        TO QTDE-E
           MOVE QTDE-E                 TO GS-LINDET(80: 12)
           COMPUTE PERC-E = (TOTAL-FOTO-PROB * 100) / TOTAL-FOTO
           MOVE PERC-E                 TO GS-LINDET(92: 8)
           ADD TOTAL-FOTO              TO TOTAL-FOTO-G
           ADD TOTAL-FOTO-PROB         TO TOTAL-FOTO-PROB-G
           ADD TOTAL-FILME             TO TOTAL-FILME-G.

           PERFORM ZERA-SUBTOTAL.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LBP230" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.

           PERFORM ORDEM.
           PERFORM ZERA-VARIAVEIS.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                      PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL

           MOVE SPACES          TO LINDET-REL

           MOVE "TOTAL GERAL: "        TO LINDET-REL(1: 30)
           MOVE TOTAL-FILME-G          TO QTDE-E
           MOVE QTDE-E                 TO LINDET-REL(68: 8)
           MOVE TOTAL-FOTO-PROB-G      TO QTDE-E
           MOVE QTDE-E                 TO LINDET-REL(80: 12)
           COMPUTE PERC-E = (TOTAL-FOTO-PROB-G * 100) / TOTAL-FOTO-G
           MOVE PERC-E                 TO LINDET-REL(92: 10)

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN.

           MOVE SPACES TO REG-RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.
       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF FOTOGRAFO-ANT  NOT = SPACES
                 IF FOTOGRAFO-ANT NOT = FOTOGRAFO-WK
                    PERFORM TOTALIZA-REL
             WHEN 2
              IF CONTRATO-ANT  NOT = SPACES
                 IF CONTRATO-ANT NOT = CONTRATO-WK
                    PERFORM TOTALIZA-REL
             WHEN 3
              IF TIPO-PROB-ANT NOT = SPACES
                 IF TIPO-PROB-ANT NOT = TIPO-PROB-WK
                    PERFORM TOTALIZA-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS
           MOVE GS-LINDET TO LINDET-REL

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE SPACES                 TO LINDET-REL
           MOVE TOTAL-FILME            TO QTDE-E
           MOVE QTDE-E                 TO LINDET-REL(68: 8)
           MOVE TOTAL-FOTO-PROB        TO QTDE-E
           MOVE QTDE-E                 TO LINDET-REL(80: 12)
           COMPUTE PERC-E = (TOTAL-FOTO-PROB * 100) / TOTAL-FOTO
           MOVE PERC-E                 TO LINDET-REL(92: 10)
           ADD TOTAL-FOTO              TO TOTAL-FOTO-G
           ADD TOTAL-FOTO-PROB         TO TOTAL-FOTO-PROB-G
           ADD TOTAL-FILME             TO TOTAL-FILME-G.

           PERFORM ZERA-SUBTOTAL.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.

           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE LBD030 LBD021 LBD100 LBD106 CGD001 COD040.
           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP207.
      *DATA: 06/06/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RESUMO DE FILMES RECEBIDOS
      *FUNÇÃO: Listar todos as análises que estiverem dentro de interva-
      *        lo de data solicitado
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY LBPX021.
           COPY LBPX100.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS FOTOGRAFO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TIPO-FILME-WK DUPLICATES
                  ALTERNATE RECORD KEY IS QT-FILME-WK DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY LBPW021.
       COPY LBPW100.
       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(4).
           05  FOTOGRAFO-WK        PIC X(30).
           05  TIPO-FILME-WK       PIC X(30).
           05  QT-FILME-WK         PIC 9(4).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LBP207.CPB".
           COPY "LBP207.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-LBD021             PIC XX       VALUE SPACES.
           05  ST-LBD100             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
      *    CONTROLE DE QUEBRA
           05  DATA-MOVTO-ANT        PIC 9(8)     VALUE ZEROS.
           05  FOTOGRAFO-ANT         PIC X(30)    VALUE SPACES.
           05  TIPO-FILME-ANT        PIC X(30)    VALUE SPACES.
      *    TOTALIZA VARIAVEIS
           05  TOTAL-FILME           PIC 9(8)     VALUE ZEROS.
           05  QTDE-E                PIC ZZZZ.ZZZ.
           05  TOTAL-FILME-G         PIC 9(8)     VALUE ZEROS.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
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



       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(35)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(33)   VALUE
           "RESUMO MIDIAS RECEBIDAS - ORDEM: ".
           05  ORDEM-REL           PIC X(14)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE "DTA MVTO: ".
           05  DATA-INI-REL        PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  DATA-FIM-REL        PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(80) VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(80)  VALUE
           "DESCRICAO                     QTDE-MIDIAS".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(80)  VALUE SPACES.

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
           MOVE "LBD021"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD021.
           MOVE "LBD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD100.
           OPEN INPUT CGD001 LBD021 LBD100.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD021 <> "00"
              MOVE "ERRO ABERTURA LBD021: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD100 <> "00"
              MOVE "ERRO ABERTURA LBD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD100 TO GS-MENSAGEM-ERRO(23: 02)
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
      *        WHEN GS-CARREGA-LISTA-FLG-TRUE
      *             PERFORM ZERA-VARIAVEIS
      *             PERFORM CARREGA-LISTA
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
       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-DATA-INI    TO DATA-INV DATA-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-INI.
           MOVE GS-DATA-FIM    TO DATA-INV DATA-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV       TO DATA-FIM.
           MOVE ZEROS          TO SEQ-L100.
           MOVE DATA-INI       TO DATA-MOVTO-L100.
           MOVE ZEROS          TO SEQ-WK.
           START LBD100 KEY IS NOT < CHAVE-L100 INVALID KEY
                  MOVE "10" TO ST-LBD100.
           PERFORM UNTIL ST-LBD100 = "10"
             READ LBD100 NEXT RECORD AT END MOVE "10" TO ST-LBD100
              NOT AT END
               IF DATA-MOVTO-L100 > DATA-FIM
                  MOVE "10" TO ST-LBD100
               ELSE
                ADD 1                   TO SEQ-WK
                MOVE DATA-MOVTO-L100    TO GS-EXIBE-VENCTO
                MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                MOVE FOTOGRAFO-L100     TO CODIGO-CG01
                READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                END-READ
                MOVE NOME-CG01          TO FOTOGRAFO-WK
                MOVE QTDE-FILMES-L100   TO QT-FILME-WK
                MOVE TIPO-FILME-L100    TO CODIGO-LB21
                READ LBD021 INVALID KEY MOVE SPACES TO DESCRICAO-LB21
                END-READ
                MOVE DESCRICAO-LB21     TO TIPO-FILME-WK

                WRITE REG-WORK

             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE ZEROS TO TOTAL-FILME-G TOTAL-FILME.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                  PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           PERFORM MOVER-DADOS

           MOVE SPACES          TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "TOTAL GERAL: " TO GS-LINDET(1: 30)
           MOVE TOTAL-FILME-G   TO QTDE-E
           MOVE QTDE-E          TO GS-LINDET(54: 10)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *    MOVE "REFRESH-DATA" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           INITIALIZE REG-WORK
           EVALUATE GS-TIPO-ORDEM
             WHEN 1 MOVE "FOTOGRAFO" TO GS-DESCR-ORDEM
               START WORK KEY IS NOT < FOTOGRAFO-WK INVALID KEY
                 MOVE "10" TO ST-WORK
             WHEN 2 MOVE "TIPO-FILME" TO GS-DESCR-ORDEM
               START WORK KEY IS NOT < TIPO-FILME-WK INVALID KEY
                 MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-TIPO-ORDEM
             WHEN 1
              IF FOTOGRAFO-ANT  NOT = SPACES
                 IF FOTOGRAFO-ANT NOT = FOTOGRAFO-WK
                    PERFORM MOVER-DADOS
             WHEN 2
              IF TIPO-FILME-ANT NOT = SPACES
                 IF TIPO-FILME-ANT NOT = TIPO-FILME-WK
                    PERFORM MOVER-DADOS
           END-EVALUATE.
           ADD QTDE-FILMES-L100            TO TOTAL-FILME
           PERFORM MOVER-CHAVE-ANT.
       MOVER-DADOS SECTION.
           IF GS-TIPO-ORDEM = 1
              MOVE FOTOGRAFO-ANT     TO GS-LINDET(1: 50)
           ELSE MOVE TIPO-FILME-ANT  TO GS-LINDET(1: 50).

           MOVE TOTAL-FILME          TO QTDE-E
           ADD TOTAL-FILME           TO TOTAL-FILME-G
           MOVE ZEROS                TO TOTAL-FILME.
           MOVE QTDE-E               TO GS-LINDET(54: 10).
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       ZERA-VARIAVEIS SECTION.
           MOVE SPACES TO FOTOGRAFO-ANT TIPO-FILME-ANT.
       MOVER-CHAVE-ANT SECTION.
           MOVE FOTOGRAFO-WK      TO FOTOGRAFO-ANT.
           MOVE TIPO-FILME-WK     TO TIPO-FILME-ANT.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LBP207" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.
      *    COPY "COND-IMP".
           OPEN OUTPUT RELAT.
           PERFORM ORDEM.
           MOVE ZEROS TO TOTAL-FILME TOTAL-FILME-G.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                      PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM MOVER-DADOS-REL
           MOVE SPACES          TO LINDET-REL
           MOVE "TOTAL GERAL: " TO LINDET-REL(1: 30)
           MOVE TOTAL-FILME-G   TO QTDE-E
           MOVE QTDE-E          TO LINDET-REL(54: 10)

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.
      *    COPY "DESC-IMP".
       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-TIPO-ORDEM
             WHEN 1
              IF FOTOGRAFO-ANT  NOT = SPACES
                 IF FOTOGRAFO-ANT NOT = FOTOGRAFO-WK
                    PERFORM MOVER-DADOS-REL
             WHEN 2
              IF TIPO-FILME-ANT NOT = SPACES
                 IF TIPO-FILME-ANT NOT = TIPO-FILME-WK
                    PERFORM MOVER-DADOS-REL
           END-EVALUATE.
           ADD QTDE-FILMES-L100            TO TOTAL-FILME
           PERFORM MOVER-CHAVE-ANT.
       MOVER-DADOS-REL SECTION.
           IF GS-TIPO-ORDEM = 1
              MOVE FOTOGRAFO-ANT     TO LINDET-REL(1: 50)
           ELSE MOVE TIPO-FILME-ANT  TO LINDET-REL(1: 50).

           MOVE TOTAL-FILME          TO QTDE-E
           ADD TOTAL-FILME           TO TOTAL-FILME-G
           MOVE ZEROS                TO TOTAL-FILME.
           MOVE QTDE-E               TO LINDET-REL(54: 10).

           WRITE REG-RELAT FROM LINDET.
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
           CLOSE LBD021 LBD100 CGD001.
           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

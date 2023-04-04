       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBP208.
      *DATA: 07/06/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: ANÁLISE DE FILMES NA PRODUÇÃO
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
           COPY LBPX021.
           COPY LBPX100.
           COPY LBPX101.
           COPY LBPX102.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS COD-TIPO-FILME-WK
                  ALTERNATE RECORD KEY IS TIPO-FILME-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY LBPW021.
       COPY LBPW100.
       COPY LBPW101.
       COPY LBPW102.
       FD  WORK.
       01  REG-WORK.
           05  COD-TIPO-FILME-WK   PIC 9(2).
           05  TIPO-FILME-WK       PIC X(20).
           05  RECEBIDOS-WK        PIC 9(8).
           05  REVELADOS-WK        PIC 9(8).
           05  AVALIADOS-WK        PIC 9(8).
           05  PROBLEMAS-WK        PIC 9(8).
           05  PERC-WK             PIC 9(2)V99.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "LBP208.CPB".
           COPY "LBP208.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-LBD021             PIC XX       VALUE SPACES.
           05  ST-LBD100             PIC XX       VALUE SPACES.
           05  ST-LBD101             PIC XX       VALUE SPACES.
           05  ST-LBD102             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
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
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
      *    TOTALIZA VARIAVEIS
           05  TOTAL-RECEBIDO        PIC 9(8)     VALUE ZEROS.
           05  TOTAL-RECEBIDO-G      PIC 9(8)     VALUE ZEROS.
           05  TOTAL-REVELADO        PIC 9(8)     VALUE ZEROS.
           05  TOTAL-REVELADO-G      PIC 9(8)     VALUE ZEROS.
           05  TOTAL-AVALIADO        PIC 9(8)     VALUE ZEROS.
           05  TOTAL-AVALIADO-G      PIC 9(8)     VALUE ZEROS.
           05  TOTAL-PROBLEMA        PIC 9(8)     VALUE ZEROS.
           05  TOTAL-PROBLEMA-G      PIC 9(8)     VALUE ZEROS.
           05  QTDE-E                PIC ZZ.ZZZ.ZZZ.
           05  PERC-E                PIC ZZZ,ZZ.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(55)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(41)   VALUE
           "ANALISE FILMES NA PRODUCAO ".
           05  FILLER              PIC X(16)   VALUE "INT.DATA MOVTO: ".
           05  DATA-INI-REL        PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  DATA-FIM-REL        PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(80)   VALUE
           "TIPO DE MIDIA          RECEBIDOS   REVELADOS   AVALIADOS   P
      -    "ROBLEMAS   PERC-(%)".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

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
           MOVE "LBD021"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD021.
           MOVE "LBD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD100.
           MOVE "LBD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD101.
           MOVE "LBD102"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD102.
           OPEN INPUT LBD021 LBD100 LBD101 LBD102.
           IF ST-LBD021 <> "00"
              MOVE "ERRO ABERTURA LBD021: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD100 <> "00"
              MOVE "ERRO ABERTURA LBD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD101 <> "00"
              MOVE "ERRO ABERTURA LBD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD102 <> "00"
              MOVE "ERRO ABERTURA LBD102: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD102 TO GS-MENSAGEM-ERRO(23: 02)
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
                    PERFORM CARREGA-LISTA
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
           PERFORM GRAVA-RECEBIDOS
           PERFORM GRAVA-REVELADOS
           PERFORM GRAVA-AVALIADOS.

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       GRAVA-RECEBIDOS SECTION.
           MOVE ZEROS          TO SEQ-L100.
           MOVE DATA-INI       TO DATA-MOVTO-L100.
           START LBD100 KEY IS NOT < CHAVE-L100 INVALID KEY
                  MOVE "10" TO ST-LBD100.
           PERFORM UNTIL ST-LBD100 = "10"
             READ LBD100 NEXT RECORD AT END MOVE "10" TO ST-LBD100
              NOT AT END
               IF DATA-MOVTO-L100 > DATA-FIM
                  MOVE "10" TO ST-LBD100
               ELSE
                MOVE DATA-MOVTO-L100    TO GS-EXIBE-VENCTO
                MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                MOVE TIPO-FILME-L100    TO COD-TIPO-FILME-WK
                READ WORK INVALID KEY
                     INITIALIZE REG-WORK
                     MOVE TIPO-FILME-L100  TO COD-TIPO-FILME-WK
                                              CODIGO-LB21
                     PERFORM LER-TIPO-FILME
                     MOVE DESCRICAO-LB21   TO TIPO-FILME-WK
                     MOVE QTDE-FILMES-L100 TO RECEBIDOS-WK
                     WRITE REG-WORK
                     END-WRITE
                   NOT INVALID KEY
                     ADD QTDE-FILMES-L100  TO RECEBIDOS-WK
                     REWRITE REG-WORK
                     END-REWRITE
                END-READ
             END-READ
           END-PERFORM.
       GRAVA-REVELADOS SECTION.
           MOVE ZEROS          TO SEQ-L101.
           MOVE DATA-INI       TO DATA-MOVTO-L101.
           START LBD101 KEY IS NOT < CHAVE-L101 INVALID KEY
                  MOVE "10" TO ST-LBD101.
           PERFORM UNTIL ST-LBD101 = "10"
             READ LBD101 NEXT RECORD AT END MOVE "10" TO ST-LBD101
              NOT AT END
               IF DATA-MOVTO-L101 > DATA-FIM
                  MOVE "10" TO ST-LBD101
               ELSE
                MOVE DATA-MOVTO-L101    TO GS-EXIBE-VENCTO
                MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                MOVE TIPO-FILME-L101    TO COD-TIPO-FILME-WK
                READ WORK INVALID KEY
                     INITIALIZE REG-WORK
                     MOVE TIPO-FILME-L101  TO COD-TIPO-FILME-WK
                                              CODIGO-LB21
                     PERFORM LER-TIPO-FILME
                     MOVE DESCRICAO-LB21   TO TIPO-FILME-WK
                     MOVE QTDE-FILMES-L101 TO REVELADOS-WK
                     WRITE REG-WORK
                     END-WRITE
                   NOT INVALID KEY
                     ADD QTDE-FILMES-L101  TO REVELADOS-WK
                     REWRITE REG-WORK
                     END-REWRITE
                END-READ
             END-READ
           END-PERFORM.
       GRAVA-AVALIADOS SECTION.
           MOVE ZEROS          TO SEQ-L102.
           MOVE DATA-INI       TO DATA-MOVTO-L102.
           START LBD102 KEY IS NOT < CHAVE-L102 INVALID KEY
                  MOVE "10" TO ST-LBD102.
           PERFORM UNTIL ST-LBD102 = "10"
             READ LBD102 NEXT RECORD AT END MOVE "10" TO ST-LBD102
              NOT AT END
               IF DATA-MOVTO-L102 > DATA-FIM
                  MOVE "10" TO ST-LBD102
               ELSE
                MOVE DATA-MOVTO-L102    TO GS-EXIBE-VENCTO
                MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                MOVE TIPO-FILME-L102    TO COD-TIPO-FILME-WK
                READ WORK INVALID KEY
                     INITIALIZE REG-WORK
                     MOVE TIPO-FILME-L102  TO COD-TIPO-FILME-WK
                                              CODIGO-LB21
                     PERFORM LER-TIPO-FILME
                     MOVE DESCRICAO-LB21   TO TIPO-FILME-WK
                     MOVE QT-FILME-PROB-L102 TO AVALIADOS-WK
                     MOVE QT-FOTOG-PROB-L102 TO PROBLEMAS-WK
                     WRITE REG-WORK
                     END-WRITE
                   NOT INVALID KEY
                     ADD QT-FILME-PROB-L102  TO AVALIADOS-WK
                     ADD QT-FOTOG-PROB-L102  TO PROBLEMAS-WK
                     REWRITE REG-WORK
                     END-REWRITE
                END-READ
             END-READ
           END-PERFORM.
       LER-TIPO-FILME SECTION.
           READ LBD021 INVALID KEY MOVE SPACES TO DESCRICAO-LB21.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO TOTAL-RECEBIDO TOTAL-REVELADO TOTAL-AVALIADO
                         TOTAL-PROBLEMA.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE SPACES TO TIPO-FILME-WK.
           START WORK KEY IS NOT < TIPO-FILME-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                  PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           MOVE SPACES            TO GS-LINDET
           MOVE "INSERE-LIST"     TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "TOTAL GERAL: "   TO GS-LINDET(1: 30)
           MOVE TOTAL-RECEBIDO    TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(22: 12)
           MOVE TOTAL-REVELADO    TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(33: 12)
           MOVE TOTAL-AVALIADO    TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(45: 12)
           MOVE TOTAL-PROBLEMA    TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(57: 12)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-LINDET SECTION.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE TIPO-FILME-WK     TO GS-LINDET(1: 21)
           MOVE RECEBIDOS-WK      TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(22: 12)
           MOVE REVELADOS-WK      TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(33: 12)
           MOVE AVALIADOS-WK      TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(45: 12)
           MOVE PROBLEMAS-WK      TO QTDE-E
           MOVE QTDE-E            TO GS-LINDET(57: 12)
           ADD RECEBIDOS-WK       TO TOTAL-RECEBIDO
           ADD REVELADOS-WK       TO TOTAL-REVELADO
           ADD AVALIADOS-WK       TO TOTAL-AVALIADO
           ADD PROBLEMAS-WK       TO TOTAL-PROBLEMA.
           MOVE COD-TIPO-FILME-WK TO CODIGO-LB21.
           READ LBD021 INVALID KEY MOVE ZEROS TO QT-FOTOS-LB21.
           COMPUTE PERC-E = ((PROBLEMAS-WK / QT-FOTOS-LB21) /
                             AVALIADOS-WK) * 100.
           MOVE PERC-E            TO GS-LINDET(71: 6).

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "LBP208" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO TOTAL-RECEBIDO TOTAL-REVELADO TOTAL-AVALIADO
                         TOTAL-PROBLEMA.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE SPACES TO TIPO-FILME-WK
           START WORK KEY IS NOT < TIPO-FILME-WK INVALID KEY
                 MOVE "10"  TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                      PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           MOVE SPACES            TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           MOVE "TOTAL GERAL: "   TO LINDET-REL(1: 30)
           MOVE TOTAL-RECEBIDO    TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(22: 12)
           MOVE TOTAL-REVELADO    TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(33: 12)
           MOVE TOTAL-AVALIADO    TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(45: 12)
           MOVE TOTAL-PROBLEMA    TO QTDE-E
           MOVE QTDE-E            TO LINDET-REL(57: 12)
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN.

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           PERFORM MOVER-DADOS
           MOVE GS-LINDET TO LINDET-REL

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       CABECALHO SECTION.
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
           CLOSE LBD021 LBD100 LBD101 LBD102.
           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

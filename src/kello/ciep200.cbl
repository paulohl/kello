       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIEP200.
      *DATA: 02/09/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *RELATÓRIO: Controle de mensagens internas automáticas/descritivas
      *FUNÇÃO:
      *
      *
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CIEPX001.
           COPY CIEPX002.
           COPY CIEPX010.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS COD-MENS-PADRAO-WK
                  STATUS IS ST-WORK.

           SELECT WORK1 ASSIGN TO VARIA-W1
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CHAVE-WK1 = COD-MENS-PADRAO-WK1 SEQ-WK1
                  STATUS IS ST-WORK1.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CIEPW001.
       COPY CIEPW002.
       COPY CIEPW010.
       FD  WORK.
       01  REG-WORK.
           05  COD-MENS-PADRAO-WK  PIC 9(2).
           05  FUNCAO-WK           PIC 99.
           05  ELIMINAR-WK         PIC 9.
      *    ELIMINAR-WK = 0(NAO)  1(SIM)
       FD  WORK1.
       01  REG-WORK1.
           05  COD-MENS-PADRAO-WK1 PIC 9(2).
           05  SEQ-WK1             PIC 9(2).
           05  DESCRICAO-WK1       PIC X(60).
           05  DATA-WK1            PIC 9(8).
           05  HORA-WK1            PIC 9(4).
           05  ORIGEM-WK1          PIC X(5).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CIEP200.CPB".
           COPY "CIEP200.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CIED001            PIC XX       VALUE SPACES.
           05  ST-CIED002            PIC XX       VALUE SPACES.
           05  ST-CIED010            PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK1              PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VARIA-W1              PIC 9(8)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  DATA-MOVTO-W          PIC 9(8)    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 VALUE ZEROS.
           05  HORA-E                PIC X(5)    VALUE SPACES.
           05  USUARIO-FUNCAO        PIC 9(3)    VALUE ZEROS.
           05  SEQ-W                 PIC 9(2)    VALUE ZEROS.
           05  LIN                   PIC 9(02)   VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(125)  VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(46)   VALUE
           "RELATORIO DE MENSAGENS             ".
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(04)   VALUE "HR: ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(7)    VALUE SPACES.
           05  FILLER              PIC X(14)   VALUE "DATA EMISSAO: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.

       01  CAB03.
           05  FILLER              PIC X(100)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(10)   VALUE "ASSUNTO: ".
           05  ASSUNTO-REL         PIC X(60)   VALUE SPACES.

       01  CAB05.
           05  FILLER              PIC X(100)  VALUE
           "DATA-ENVIO HORA  USUARIO DESCRICAO".

       01  LINDET.
           05  LINDET-REL          PIC X(100)  VALUE SPACES.
       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CIEP200-EXIT-FLG-TRUE.
           PERFORM EXCLUIR-MENSAGENS.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CIEP200-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CIEP200-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CIEP200-VERSION-NO TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           OPEN INPUT CIED001 CIED002.
           OPEN I-O CIED010.
           IF ST-CIED001 <> "00"
              MOVE "ERRO ABERTURA CIED001: "  TO CIEP200-MENSAGEM-ERRO
              MOVE ST-CIED001 TO CIEP200-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CIED002 <> "00"
              MOVE "ERRO ABERTURA CIED002: "  TO CIEP200-MENSAGEM-ERRO
              MOVE ST-CIED002 TO CIEP200-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CIED010<> "00"
              MOVE "ERRO ABERTURA CIED010: "  TO CIEP200-MENSAGEM-ERRO
              MOVE ST-CIED010 TO CIEP200-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 6 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CIEP200-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CIEP200-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CIEP200-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CIEP200-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN CIEP200-ITEM-SELECIONADO-TRUE
                    PERFORM CARREGA-LISTA1
               WHEN CIEP200-GRAVA-WORK-TRUE
                    PERFORM INICIO-WORK
               WHEN CIEP200-ELIMINAR-ITEM-TRUE
                    PERFORM ELIMINAR-ITEM
           END-EVALUATE.
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
       ELIMINAR-ITEM SECTION.
           MOVE CIEP200-LINDET(1: 2) TO COD-MENS-PADRAO-WK.
           READ WORK INVALID KEY CONTINUE
            NOT INVALID KEY
             IF ELIMINAR-WK = 0
                MOVE 1       TO ELIMINAR-WK
                MOVE "SIM"   TO CIEP200-LINDET(83: 03)
                REWRITE REG-WORK
             ELSE MOVE 0          TO ELIMINAR-WK
                MOVE "NÃO"        TO CIEP200-LINDET(83: 03)
                REWRITE REG-WORK.
       LIMPAR-DADOS SECTION.
           INITIALIZE CIEP200-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       INICIO-WORK SECTION.
           MOVE "EXIBE-TELA-AGUARDE" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           OPEN INPUT WORK.
           IF ST-WORK = "35"
              ACCEPT VARIA-W FROM TIME
              MOVE VARIA-W TO VARIA-W1
              ADD 1 TO VARIA-W1
              CLOSE WORK WORK1  OPEN OUTPUT WORK WORK1
              CLOSE WORK WORK1  OPEN I-O WORK WORK1
           ELSE CLOSE WORK WORK1   OPEN OUTPUT WORK WORK1
                CLOSE WORK WORK1   OPEN I-O WORK WORK1.
           IF COD-USUARIO-W = 002
      *       (SUPERVISOR P/ LER TODAS AS MENSAGENS - CODIGO DO ADALTON)
              PERFORM LER-TODAS-MENSAGENS
              MOVE "10" TO ST-CIED002
           ELSE
              MOVE COD-USUARIO-W TO SUPERIOR-CI02
              START CIED002 KEY IS NOT < SUPERIOR-CI02
                 INVALID KEY MOVE "10" TO ST-CIED002.
           PERFORM UNTIL ST-CIED002 = "10"
             READ CIED002 NEXT RECORD AT END MOVE "10" TO ST-CIED002
               NOT AT END
                 IF SUPERIOR-CI02 NOT = COD-USUARIO-W
                           MOVE "10" TO ST-CIED002
                 ELSE
                  MOVE CODIGO-CI02 TO FUNCAO-DESTINO-CI10
                  MOVE ZEROS       TO COD-MENS-PADRAO-CI10
                  MOVE ZEROS       TO SEQ-W
                  START CIED010 KEY IS NOT < ALT1-CI10
                        INVALID KEY MOVE "10" TO ST-CIED010
                  END-START
                  PERFORM UNTIL ST-CIED010 = "10"
                    READ CIED010 NEXT RECORD AT END
                         MOVE "10" TO ST-CIED010
                      NOT AT END
                         ADD 1 TO SEQ-W
                         IF FUNCAO-DESTINO-CI10 NOT = CODIGO-CI02
                             MOVE "10" TO ST-CIED010
                         ELSE
                           MOVE COD-MENS-PADRAO-CI10 TO
                               COD-MENS-PADRAO-WK COD-MENS-PADRAO-WK1
                           MOVE FUNCAO-DESTINO-CI10 TO FUNCAO-WK
                           MOVE ZEROS TO ELIMINAR-WK
                           WRITE REG-WORK
                           END-WRITE
                           MOVE DATA-CI10   TO DATA-WK1
                           MOVE HORA-CI10   TO HORA-WK1
                           MOVE ORIGEM-CI10 TO ORIGEM-WK1
                           MOVE DESCRICAO-MENS-CI10 TO DESCRICAO-WK1
                           MOVE SEQ-W       TO SEQ-WK1
                           WRITE REG-WORK1
                           END-WRITE
                         END-IF
                    END-READ
                  END-PERFORM
             END-READ
           END-PERFORM.
           MOVE "APAGA-TELA-AGUARDE" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LER-TODAS-MENSAGENS SECTION.
      * FUNCAO P/ O SUPERVISOR
           MOVE ZEROS       TO FUNCAO-DESTINO-CI10
           MOVE ZEROS       TO COD-MENS-PADRAO-CI10
           MOVE ZEROS       TO SEQ-W
           START CIED010 KEY IS NOT < ALT1-CI10
                 INVALID KEY MOVE "10" TO ST-CIED010
           END-START
           PERFORM UNTIL ST-CIED010 = "10"
             READ CIED010 NEXT RECORD AT END MOVE "10" TO ST-CIED010
               NOT AT END
                  ADD 1 TO SEQ-W
                    MOVE COD-MENS-PADRAO-CI10 TO
                        COD-MENS-PADRAO-WK COD-MENS-PADRAO-WK1
                    MOVE FUNCAO-DESTINO-CI10 TO FUNCAO-WK
                    MOVE ZEROS TO ELIMINAR-WK
                    WRITE REG-WORK
                    END-WRITE
                    MOVE DATA-CI10   TO DATA-WK1
                    MOVE HORA-CI10   TO HORA-WK1
                    MOVE ORIGEM-CI10 TO ORIGEM-WK1
                    MOVE DESCRICAO-MENS-CI10 TO DESCRICAO-WK1
                    MOVE SEQ-W       TO SEQ-WK1
                    WRITE REG-WORK1
                    END-WRITE
             END-READ
           END-PERFORM.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO COD-MENS-PADRAO-WK.
           START WORK KEY IS NOT < COD-MENS-PADRAO-WK
                 INVALID KEY MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                    MOVE SPACES            TO CIEP200-LINDET
                    MOVE COD-MENS-PADRAO-WK TO CODIGO-CI01
                    READ CIED001 INVALID KEY
                         MOVE SPACES TO MENSAGEM-CI01
                    END-READ
                    MOVE COD-MENS-PADRAO-WK TO CIEP200-LINDET(1: 2)
                    MOVE "-"                TO CIEP200-LINDET(3: 1)
                    MOVE MENSAGEM-CI01      TO CIEP200-LINDET(4: 60)
                    MOVE "NÃO"              TO CIEP200-LINDET(83: 3)
                    MOVE "INSERE-LIST" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       CARREGA-LISTA1 SECTION.
           MOVE "CLEAR-LIST-BOX1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE CIEP200-LINDET(1: 2) TO COD-MENS-PADRAO-WK1 CODIGO-CI01.
           MOVE ZEROS                TO SEQ-WK1.
           START WORK1 KEY IS NOT < CHAVE-WK1
                 INVALID KEY MOVE "10" TO ST-WORK1.
           READ CIED001 INVALID KEY MOVE SPACES TO MENSAGEM-CI01.
           MOVE MENSAGEM-CI01 TO CIEP200-ASSUNTO.
           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END MOVE "10" TO ST-WORK1
              NOT AT END
                    IF COD-MENS-PADRAO-WK1 NOT = CODIGO-CI01
                          MOVE "10" TO ST-WORK1
                    ELSE
                    MOVE SPACES             TO CIEP200-LINDET1
                    MOVE DATA-WK1           TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV           TO DATA-E
                    MOVE DATA-E             TO CIEP200-LINDET1(1: 11)
                    MOVE HORA-WK1(1: 2)     TO HORA-E(1: 2)
                    MOVE ":"                TO HORA-E(3: 1)
                    MOVE HORA-WK1(3: 2)     TO HORA-E(4: 2)
                    MOVE HORA-E             TO CIEP200-LINDET1(12: 6)
                    MOVE ORIGEM-WK1         TO CIEP200-LINDET1(18: 6)
                    MOVE DESCRICAO-WK1      TO CIEP200-LINDET1(26: 60)
                    MOVE "INSERE-LIST1" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       CLEAR-FLAGS SECTION.
           INITIALIZE CIEP200-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-NEW-SET TO DS-CONTROL
           MOVE "CIEP200"   TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       EXCLUIR-MENSAGENS SECTION.
           MOVE ZEROS TO COD-MENS-PADRAO-WK.
           START WORK KEY IS NOT < COD-MENS-PADRAO-WK INVALID KEY
                  MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
                NOT AT END
                 IF ELIMINAR-WK = 0 CONTINUE
                 ELSE
                  MOVE ZEROS TO ST-CIED010
                  PERFORM UNTIL ST-CIED010 = "10"
                   MOVE COD-MENS-PADRAO-WK TO COD-MENS-PADRAO-CI10
                   MOVE FUNCAO-WK          TO FUNCAO-DESTINO-CI10
                   START CIED010 KEY IS = ALT1-CI10 INVALID KEY
                             MOVE "10" TO ST-CIED010
                      NOT INVALID KEY
                         READ CIED010 NEXT RECORD
                         END-READ
                         DELETE CIED010
                         END-DELETE
                   END-START
                  END-PERFORM
                 END-IF
             END-READ
           END-PERFORM.
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE CIEP200-LINDET(1: 2) TO COD-MENS-PADRAO-WK1 CODIGO-CI01
                                        COD-MENS-PADRAO-WK.
      *    MOVE 1                    TO ELIMINAR-WK.
      *    MOVE "SIM"                TO CIEP200-LINDET(83: 3).
      *    READ WORK INVALID KEY CONTINUE
      *              NOT INVALID KEY REWRITE REG-WORK.
           MOVE ZEROS                TO SEQ-WK1.
           START WORK1 KEY IS NOT < CHAVE-WK1
                 INVALID KEY MOVE "10" TO ST-WORK1.
           READ CIED001 INVALID KEY MOVE SPACES TO MENSAGEM-CI01.
           MOVE MENSAGEM-CI01 TO ASSUNTO-REL.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK1 = "10"
              READ WORK1 NEXT RECORD AT END MOVE "10" TO ST-WORK1
              NOT AT END
                    MOVE SPACES             TO LINDET-REL
                    MOVE DATA-WK1           TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV           TO DATA-E
                    MOVE DATA-E             TO LINDET-REL(1: 11)
                    MOVE HORA-WK1(1: 2)     TO HORA-E(1: 2)
                    MOVE ":"                TO HORA-E(3: 1)
                    MOVE HORA-WK1(3: 2)     TO HORA-E(4: 2)
                    MOVE HORA-E             TO LINDET-REL(12: 6)
                    MOVE ORIGEM-WK1         TO LINDET-REL(18: 6)
                    MOVE DESCRICAO-WK1      TO LINDET-REL(26: 60)
                    WRITE REG-RELAT FROM LINDET
                    END-WRITE
                    ADD 1 TO LIN
                    IF LIN > 56 PERFORM CABECALHO
                    END-IF
              END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       LINHA-BRANCO-IMP SECTION.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           MOVE 7 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CIEP200-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CIED001 CIED002 CIED010 WORK WORK1.
           DELETE FILE WORK WORK1.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

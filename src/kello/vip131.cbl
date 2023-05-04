       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIP131.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 10/08/2000
      *DESCRIÇÃO: MANUTENÇÃO P/ O CAMPO "LOCALIZACAO DE FITAS" E
      *                                      IMPRESSAO DO PROTOCOLO
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX003.
           COPY VIPX100.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY COPW003.
       COPY VIPW100.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(100).
       WORKING-STORAGE SECTION.
           COPY "VIP131.CPB".
           COPY "VIP131.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD999             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-VID100             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  DATA-W                PIC 9(8)     VALUE ZEROS.
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
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(58)   VALUE
           "PROTOCOLO DE ENTREGA DE FITA     ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(75)   VALUE ALL "=".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(75)   VALUE SPACES.

           copy impressora.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           accept parametros-w from command-line.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO.
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "VID100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID100.
           OPEN I-O VID100.
           OPEN INPUT COD003.
           IF ST-VID100 = "35"
              CLOSE VID100      OPEN OUTPUT VID100
              CLOSE VID100      OPEN I-O VID100
           END-IF.
           IF ST-VID100 <> "00"
              MOVE "ERRO ABERTURA VID100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              MOVE 1 TO GS-ORDER
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-PROTOCOLO
                    end-if
               WHEN GS-TRANSF-LOCALIZ-TRUE
                    PERFORM TRANSFERE-LOCALIZACAO
               WHEN GS-LER-EVENTO-TRUE
                    PERFORM LER-EVENTO
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


       LER-EVENTO SECTION.
           INITIALIZE REG-VID100
           MOVE GS-IDENTIFICADOR TO IDENTIFICADOR-V100
           START VID100 KEY IS NOT < ALT4-V100 INVALID KEY
                 MOVE "NAO CADASTRADA" TO GS-EVENTO
           NOT INVALID KEY
                 READ VID100 NEXT RECORD
                 END-READ
                 IF GS-IDENTIFICADOR <> IDENTIFICADOR-V100
                    MOVE "NAO CADASTRADA"  TO GS-EVENTO
                 ELSE
                    MOVE NR-FITAS-V100     TO GS-NR-FITA
                    MOVE EVENTO-V100       TO CODIGO-CO03
                    READ COD003 INVALID KEY
                         MOVE SPACES       TO NOME-CO03
                    END-READ
                    MOVE NOME-CO03         TO GS-EVENTO
                    MOVE LOCALIZACAO-V100  TO GS-LOCAL-ATUAL
                    MOVE "OUTR"            TO GS-LOCAL-TRANSF
                 END-IF
           END-START.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       TRANSFERE-LOCALIZACAO SECTION.
      *    TODOS OS CONTRATOS QUE CONTEREM O MESMO NR DE FITA DEVERÁ
      *    SER TRANSFERIDO P/ O LOCAL DESEJADO
           INITIALIZE REG-VID100
           MOVE GS-IDENTIFICADOR      TO IDENTIFICADOR-V100
           START VID100 KEY IS NOT < ALT4-V100 INVALID KEY
              MOVE "10" TO ST-VID100.
           PERFORM UNTIL ST-VID100 = "10"
             READ VID100 NEXT RECORD AT END
                  MOVE "10" TO ST-VID100
             NOT AT END
                 IF GS-IDENTIFICADOR <> IDENTIFICADOR-V100
                    MOVE "10" TO ST-VID100
                 ELSE
                   MOVE GS-LOCAL-TRANSF   TO LOCALIZACAO-V100
                   REWRITE REG-VID100
                   END-REWRITE
                 END-IF
             END-READ
           END-PERFORM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE ds-push-set TO DS-CONTROL
           MOVE "VIP131" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-PROTOCOLO SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO PAG-W.
           PERFORM CABECALHO.

           MOVE SPACES TO LINDET-REL
           MOVE "NR-FITA............: "          TO LINDET-REL(1: 21)
           MOVE GS-IDENTIFICADOR                 TO LINDET-REL(22: 30)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE "EVENTO.............: "          TO LINDET-REL(1: 21)
           MOVE GS-EVENTO                        TO LINDET-REL(22: 30)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE SPACES                           TO LINDET-REL
           MOVE "QUEM ESTA RETIRANDO: "          TO LINDET-REL(1: 21)
           MOVE "______________________________" TO LINDET-REL(22: 30)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE "OBJETIVO...........: "          TO LINDET-REL(1: 21)
           MOVE "_________________________________________________"
                TO LINDET-REL(22: 30)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE "_________________________________________________"
               TO LINDET-REL(22: 30)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE "_________________________________________________"
               TO LINDET-REL(22: 30)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE "PRAZO DE ENTREGA...: "          TO LINDET-REL(1: 21)
           MOVE "______________________________" TO LINDET-REL(22: 30)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE "______________________________" TO LINDET-REL(22: 30)
           MOVE "AUTORIZADO POR.....: "          TO LINDET-REL(1: 21)
           MOVE "______________________________" TO LINDET-REL(22: 30)
           WRITE REG-RELAT FROM LINDET AFTER 2
           MOVE "______________________________" TO LINDET-REL(1: 40)
           MOVE "______________________________" TO LINDET-REL(41: 39)
           WRITE REG-RELAT FROM LINDET AFTER 5
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           WRITE REG-RELAT FROM CAB01
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD003 VID100.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.

       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP121.
      *DATA: 03/03/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: salários de reportagem
      *FUNÇÃO: Conferência de creditos de VENDEDORES, e atualização
      *        no contas correntes
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CCPX120.
           COPY CCPX100.
           COPY CCPX101.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CODIGO-WK
                  ALTERNATE RECORD KEY IS NOME-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CCPW120.
       COPY CCPW100.
       COPY CCPW101.
       FD  WORK.
       01  REG-WORK.
           05  CODIGO-WK           PIC 9(6).
           05  NOME-WK             PIC X(30).
           05  VALOR-WK            PIC 9(8)V99.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CCP121.CPB".
           COPY "CCP121.CPY".
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
           05  ST-CCD100             PIC XX       VALUE SPACES.
           05  ST-CCD101             PIC XX       VALUE SPACES.
           05  ST-CCD120             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MESANOW.
               10  MES-WW            PIC 99.
               10  ANO-WW            PIC 9999.
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-WI            PIC 9999.
               10  MES-WI            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  CARACTERISTICA-W      PIC 9        VALUE ZEROS.
      *    P/ SABER QUAL O TIPO-LCTO SELECIONADO
           05  IMPRIME-W             PIC 9        VALUE ZEROS.
      *    FLAG P/ IDENTIFICAR QUAIS NOMES FAZEM PARTE DA CARACTERISTICA
      *    SELECIONADA
           05  GRAVA1-REGRAVA2       PIC 9        VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  MESANO-E              PIC 99/9999  BLANK WHEN ZEROS.
           05  TIPO-LCTO-W           PIC 9        VALUE ZEROS.
           05  VALOR-E               PIC ZZZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(40)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(10)   VALUE "  :  ".
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(37)   VALUE
           "RELACAO DE CREDITOS DE REPORTAGEM".
           05  FILLER              PIC X(09)   VALUE "MES/ANO: ".
           05  MESANO-REL          PIC 99/9999.
           05  FILLER              PIC X(09)   VALUE SPACES.
           05  FILLER              PIC X(08)   VALUE 'VENCTO: '.
           05  VENCTO-REL          PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "CODIGO FUNCIONARIO                     TOTAL-CREDITOS".
       01  LINDET.
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
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD100.
           MOVE "CCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD101.
           MOVE "CCD120"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CCD120.
           OPEN INPUT CGD001.
           OPEN I-O   CCD120
           CLOSE      CCD120
           OPEN INPUT CCD120

           IF ST-CCD120 = "35"
              CLOSE CCD120    OPEN OUTPUT CCD120  CLOSE CCD120
              OPEN I-O CCD120.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           OPEN INPUT CCD100.
           IF ST-CCD100 <> "00"
              MOVE "ERRO ABERTURA CCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           CLOSE CCD100.  OPEN INPUT CCD101.
           IF ST-CCD101 <> "00"
              MOVE "ERRO ABERTURA CCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           CLOSE CCD101.
           IF ST-CCD120 <> "00"
              MOVE "ERRO ABERTURA CCD120: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD120 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
      *    MOVE 1 TO COD-USUARIO-W.
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
               WHEN GS-ATUALIZA-CTACORR-TRUE
                    PERFORM ATUALIZA-CTACORRENTE
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
           MOVE GS-MESANO-BASE(1: 2) TO MESANO-BASE-CC120(5: 2).
           MOVE GS-MESANO-BASE(3: 4) TO MESANO-BASE-CC120(1: 4).
           MOVE MESANO-BASE-CC120 TO MESANO-I.
           MOVE ZEROS TO CODIGO-CC120.
           START CCD120 KEY IS NOT < CHAVE-CC120 INVALID KEY
                 MOVE "10" TO ST-CCD120.
           PERFORM UNTIL ST-CCD120 = "10"
                 READ CCD120 NEXT RECORD AT END
                      MOVE "10" TO ST-CCD120
                 NOT AT END
                      MOVE CODIGO-CC120    TO GS-EXIBE-CODIGO
                      MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      IF MESANO-BASE-CC120 NOT = MESANO-I
                         MOVE "10" TO ST-CCD120
                      ELSE
                         IF ATUALIZADO-CC-CC120 <> 1
                            MOVE CODIGO-CC120   TO CODIGO-WK
                                                   CODIGO-CG01
                            READ CGD001 INVALID KEY
                                 MOVE SPACES    TO NOME-CG01
                            END-READ
                            MOVE NOME-CG01      TO NOME-WK
                            MOVE VALOR-CREDITO-CC120 TO VALOR-WK
                            WRITE REG-WORK
                            END-WRITE
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO GS-TOTAL-CREDITO
                         GS-TOTAL-PESSOAS
           MOVE GS-MESANO-BASE TO MESANO-W.
           MOVE ANO-WW  TO ANO-WI
           MOVE MES-WW  TO MES-WI
           MOVE SPACES TO NOME-WK
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   ADD  1                 TO GS-TOTAL-PESSOAS
                   MOVE SPACES            TO GS-LINDET
                   MOVE CODIGO-WK         TO GS-LINDET(01:06)
                   MOVE NOME-WK           TO GS-LINDET(08:31)
                   MOVE CODIGO-WK         TO CODIGO-CC120
                   MOVE VALOR-WK          TO VALOR-E
                   ADD VALOR-WK           TO GS-TOTAL-CREDITO
                   MOVE VALOR-E           TO GS-LINDET(40: 13)
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ATUALIZA-CTACORRENTE SECTION.
           CLOSE    CCD120
           OPEN I-O CCD100 CCD101 CCD120
           MOVE GS-MESANO-BASE(1: 2) TO MESANO-I(5: 2)
           MOVE GS-MESANO-BASE(3: 4) TO MESANO-I(1: 4)
           MOVE MESANO-I TO MESANO-BASE-CC120
           MOVE ZEROS                TO CODIGO-CC120.
           START CCD120 KEY IS NOT < CHAVE-CC120 INVALID KEY
                 MOVE "10" TO ST-CCD120.
           PERFORM UNTIL ST-CCD120 = "10"
             READ CCD120 NEXT RECORD AT END
                  MOVE "10" TO ST-CCD120
             NOT AT END
                 IF MESANO-BASE-CC120 NOT = MESANO-I
                    MOVE "10" TO ST-CCD120
                 ELSE
                    IF ATUALIZADO-CC-CC120 <> 1
                      INITIALIZE REG-CCD100
                      MOVE VALOR-CREDITO-CC120 TO VALOR-CC100
                      MOVE 0                  TO CRED-DEB-CC100
                      MOVE DATA-DIA-I         TO DATA-MOVTO-CC100
                                                 DATA-EMISSAO-CC100
                      MOVE 2                  TO TIPO-LCTO-CC100
                      MOVE ZEROS              TO TIPO-FORN-CC100
                      MOVE DOCTO-CC120        TO NR-DOCTO-CC100
                      MOVE GS-VENCTO          TO DATA-INV
                      CALL "GRIDAT2" USING DATA-INV
                      MOVE DATA-INV           TO DATA-VENCTO-CC100
                      MOVE "VENDAS - MES: " TO DESCRICAO-CC100(1: 15)
                      MOVE GS-MESANO-BASE     TO MESANO-E
                      MOVE MESANO-E           TO DESCRICAO-CC100(16: 7)
                      MOVE 0101               TO NR-PARCELA-CC100
                      MOVE ZEROS TO SITUACAO-CC100 LIBERADO-CC100
                        JUROS-PAGO-CC100 TIPO-MOEDA-CC100
                        MULTA-PAGA-CC100 DESCONTO-CC100 VALOR-PAGO-CC100
                        SEQ-CAIXA-CC100 DATA-PGTO-CC100
                      MOVE ZEROS              TO CODREDUZ-APUR-CC100
                      MOVE USUARIO-W          TO RESPONSAVEL-CC100
                                                 DIGITADOR-CC100
                      MOVE CODIGO-CC120 TO FORNEC-CC100 FORNEC-CC101
                      PERFORM ATUALIZA-SEQ-CCD101
                      MOVE SEQ-CC101          TO SEQ-CC100
                      MOVE ZEROS TO ST-CCD100
                      PERFORM UNTIL ST-CCD100 = "10"
                        WRITE REG-CCD100 INVALID KEY
                            PERFORM ATUALIZA-SEQ-CCD101
                            MOVE SEQ-CC101 TO SEQ-CC100
                        NOT INVALID KEY
                            MOVE 1         TO ATUALIZADO-CC-CC120
                            REWRITE REG-CCD120
                            END-REWRITE
                            MOVE "10" TO ST-CCD100
                        END-WRITE
                      END-PERFORM
                    END-IF
                 END-IF
             END-READ
           END-PERFORM.
           CLOSE CCD100 CCD101 CCD120
           OPEN INPUT CCD120.
       ATUALIZA-SEQ-CCD101 SECTION.
           READ CCD101 INVALID KEY
                MOVE 1 TO SEQ-CC101
                WRITE REG-CCD101 INVALID KEY
                        MOVE "CCD101"    TO GS-MENSAGEM-ERRO(15: 7)
                        MOVE ST-CCD101 TO GS-MENSAGEM-ERRO(23: 2)
                        PERFORM ERRO-GRAVACAO
                END-WRITE
              NOT INVALID KEY
                  ADD 1 TO SEQ-CC101
                  REWRITE REG-CCD101 INVALID KEY
                        MOVE "CCD101"    TO GS-MENSAGEM-ERRO(15: 7)
                        MOVE "ST-CCD101" TO GS-MENSAGEM-ERRO(23: 2)
                        PERFORM ERRO-GRAVACAO
                  END-REWRITE
           END-READ.

       LIMPAR-ENTRADA SECTION.
           MOVE "LIMPAR-ENTR-DADOS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO(1: 14)
           PERFORM LOAD-SCREENSET
           PERFORM MENSAGEM-ERRO-GRAVACAO.
       MENSAGEM-ERRO-GRAVACAO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO-GRAVACAO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CCP121" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO NOME-WK
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   MOVE SPACES            TO LINDET-REL

                   MOVE SPACES            TO LINDET-REL
                   MOVE CODIGO-WK         TO LINDET-REL(01:06)
                                             CODIGO-CC120
                   MOVE NOME-WK           TO LINDET-REL(08:31)
                   MOVE MESANO-I          TO MESANO-BASE-CC120
                   MOVE VALOR-WK          TO VALOR-E
                   MOVE VALOR-E           TO LINDET-REL(40:13)
                   WRITE REG-RELAT FROM LINDET
                   END-WRITE
                   ADD 1 TO LIN
                   IF LIN > 56 PERFORM CABECALHO
                   END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.
       CABECALHO SECTION.
           MOVE GS-VENCTO TO VENCTO-REL.
           MOVE GS-MESANO-BASE TO MESANO-REL
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
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
           CLOSE CCD120 CGD001.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

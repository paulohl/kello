       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP119.
      *DATA: 07-08-2007
      *AUTOR: ALFREDO SAVIOLLI NETO
      *PROGRAMA: EXPORTAÇÃO EVENTOS WEB

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY COPX003.
           COPY COPX040.
           COPY COPX041.
           COPY COPX060.
           COPY CAPX010.
           COPY IEPX011.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS LINHA-WK
                  ALTERNATE RECORD KEY IS ALT1-CHAVE =
                                             CONTRATO-WK
                                             DATA-REALIZ-WK
                                             UF-WK
                                             CIDADE-WK
                                             DESCR-EVENTO-WK
                                             CURSO-WK
                                             ITEM-WK
                  ALTERNATE RECORD KEY IS ALT2-CHAVE =
                                             CIDADE-WK
                                             CONTRATO-WK
                                             UF-WK
                                             DATA-REALIZ-WK
                                             DESCR-EVENTO-WK
                                             CURSO-WK
                                             ITEM-WK
                  WITH             DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-CHAVE =
                                             UF-WK
                                             CONTRATO-WK
                                             CIDADE-WK
                                             DATA-REALIZ-WK
                                             DESCR-EVENTO-WK
                                             CURSO-WK
                                             ITEM-WK
                  WITH             DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-CHAVE =
                                             DATA-REALIZ-WK
                                             CONTRATO-WK
                                             UF-WK
                                             CIDADE-WK
                                             DESCR-EVENTO-WK
                                             CURSO-WK
                                             ITEM-WK
                  WITH             DUPLICATES
                  ALTERNATE RECORD KEY IS ALT5-CHAVE =
                                             DESCR-EVENTO-WK
                                             DATA-REALIZ-WK
                                             CONTRATO-WK
                                             CIDADE-WK
                                             UF-WK
                                             CURSO-WK
                                             ITEM-WK
                  WITH             DUPLICATES
                  ALTERNATE RECORD KEY IS ALT6-CHAVE =
                                             CURSO-WK
                                             UF-WK
                                             CONTRATO-WK
                                             CIDADE-WK
                                             DATA-REALIZ-WK
                                             DESCR-EVENTO-WK
                                             ITEM-WK
                  WITH             DUPLICATES.


           SELECT EXCEL ASSIGN TO ARQUIVO-EXCEL
                        ORGANIZATION IS LINE SEQUENTIAL
                        ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           COPY COPW003.
           COPY COPW040.
           COPY COPW041.
           COPY COPW060.
           COPY CAPW010.
           COPY IEPW011.

      * EXPORTA-WK = 1 (EXPORTAR DADOS) 0 (NÃO EXPORTAR)
       FD  WORK.
       01  REG-WORK.
           05  LINHA-WK            PIC 9(05).
           05  CONTRATO-WK         PIC 9(4).
           05  CURSO-WK            PIC X(20).
           05  CIDADE-WK           PIC X(30).
           05  UF-WK               PIC X(02).
           05  DATA-REALIZ-WK      PIC 9(08).
           05  DESCR-EVENTO-WK     PIC X(20).
           05  COD-CIDADE-WK       PIC 9(04).
           05  COD-EVENTO-WK       PIC 9(05).
           05  ITEM-WK             PIC 9(03).
           05  EXPORTA-WK          PIC 9(01).

       FD  EXCEL
           LABEL RECORD IS OMITTED.
       01  REG-EXCEL               PIC X(150).

       WORKING-STORAGE SECTION.
           COPY "COP119.CPB".
           COPY "COP119.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD041             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
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
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           05  NUMERO-CURSOS         PIC 9(02)    VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 DATA-INI                   PIC 9(08).
       01 DATA-INI-R REDEFINES DATA-INI.
          05 ANO-INI                 PIC 9(04).
          05 MES-INI                 PIC 9(02).
          05 DIA-INI                 PIC 9(02).

       01 DATA-FIM                   PIC 9(08).
       01 DATA-FIM-R REDEFINES DATA-FIM.
          05 ANO-FIM                 PIC 9(04).
          05 MES-FIM                 PIC 9(02).
          05 DIA-FIM                 PIC 9(02).

       01 MASC-CONTRATO              PIC ZZZ9.
       01 MASC-CIDADE                PIC ZZZ9.
       01 MASC-EVENTO                PIC ZZZZ9.
       01 MASC-ITEM                  PIC ZZ9.

       01 MASC-CONTRATO2             PIC X(04).
       01 MASC-CIDADE2               PIC X(04).
       01 MASC-EVENTO2               PIC X(05).
       01 MASC-ITEM2                 PIC X(03).

       01 AJUDA1                     PIC X(20).
       01 AJUDA2                     PIC X(20).
       01 AJUDA3                     PIC X(20).

       01 WS-DATA-SYS.
          05 WS-ANO-CPU              PIC 9(04).
          05 WS-MES-CPU              PIC 9(02).
          05 WS-DIA-CPU              PIC 9(02).
          05 FILLER                  PIC X(13).

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD041"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD041.
           MOVE "COD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           OPEN INPUT CAD010 COD003 COD040 COD041 COD060 IED011
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD041 <> "00"
              MOVE "ERRO ABERTURA COD041: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD041 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-LE-CIDADE-TRUE
                    PERFORM LER-CIDADE
               WHEN GS-LE-EVENTO-TRUE
                    PERFORM LER-EVENTO
               WHEN GS-POP-CIDADE-TRUE
                    PERFORM POP-CIDADE
               WHEN GS-POP-EVENTO-TRUE
                    PERFORM POP-EVENTO
               WHEN GS-EXCEL-FLG-TRUE
                    PERFORM GERAR-EXCEL
               WHEN GS-SELECIONAR-TUDO-TRUE
                    PERFORM SELECIONAR-TUDO
                    PERFORM CARREGA-LISTA
               WHEN GS-MARCAR-DESMARCAR-TRUE
                    PERFORM MARCAR-DESMARCAR
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.
       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       SELECIONAR-TUDO SECTION.
           INITIALIZE REG-WORK
           START WORK KEY IS NOT LESS LINHA-WK INVALID KEY
               MOVE "10" TO ST-WORK.

           PERFORM UNTIL ST-WORK = "10"
               READ WORK NEXT AT END
                   MOVE "10" TO ST-WORK
               NOT AT END
                   EVALUATE EXPORTA-WK
                       WHEN 0 MOVE 1 TO EXPORTA-WK
                       WHEN 1 MOVE 0 TO EXPORTA-WK
                   END-EVALUATE
                   REWRITE REG-WORK
               END-READ
           END-PERFORM.

       MARCAR-DESMARCAR SECTION.
           MOVE GS-LINDET(97:5)       TO LINHA-WK
           READ WORK NOT INVALID KEY
               EVALUATE EXPORTA-WK
                   WHEN 0 MOVE 1 TO EXPORTA-WK
                   WHEN 1 MOVE 0 TO EXPORTA-WK
               END-EVALUATE
               REWRITE REG-WORK
               END-REWRITE
           END-READ

           EVALUATE EXPORTA-WK
               WHEN 0 MOVE SPACES     TO GS-LINDET(95:1)
               WHEN 1 MOVE "X"        TO GS-LINDET(95:1)
           END-EVALUATE.

       GERAR-EXCEL SECTION.
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS

           MOVE SPACES TO ARQUIVO-EXCEL

           STRING "\ARQUIVOS\WB"
                            WS-ANO-CPU(3:2) WS-MES-CPU WS-DIA-CPU ".TXT"
           INTO ARQUIVO-EXCEL

           OPEN OUTPUT EXCEL

           PERFORM ORDEM.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   IF EXPORTA-WK = 1
                      MOVE CONTRATO-WK            TO MASC-CONTRATO
                      MOVE ITEM-WK                TO MASC-ITEM
                      MOVE COD-CIDADE-WK          TO MASC-CIDADE
                      MOVE COD-EVENTO-WK          TO MASC-EVENTO

                      MOVE MASC-CONTRATO          TO MASC-CONTRATO2
                      IF MASC-CONTRATO2(1:1) = SPACES
                         MOVE MASC-CONTRATO2(2:3) TO MASC-CONTRATO2
                      END-IF
                      IF MASC-CONTRATO2(1:1) = SPACES
                         MOVE MASC-CONTRATO2(3:2) TO MASC-CONTRATO2
                      END-IF
                      IF MASC-CONTRATO2(1:1) = SPACES
                         MOVE MASC-CONTRATO2(4:1) TO MASC-CONTRATO2
                      END-IF

                      MOVE MASC-ITEM              TO MASC-ITEM2
                      IF MASC-ITEM2(1:1) = SPACES
                         MOVE MASC-ITEM2(2:2)     TO MASC-ITEM2
                      END-IF
                      IF MASC-ITEM2(1:1) = SPACES
                         MOVE MASC-ITEM2(2:1)     TO MASC-ITEM2
                      END-IF

                      MOVE MASC-CIDADE            TO MASC-CIDADE2
                      IF MASC-CIDADE2(1:1) = SPACES
                         MOVE MASC-CIDADE2(2:3) TO MASC-CIDADE2
                      END-IF
                      IF MASC-CIDADE2(1:1) = SPACES
                         MOVE MASC-CIDADE2(3:2) TO MASC-CIDADE2
                      END-IF
                      IF MASC-CIDADE2(1:1) = SPACES
                         MOVE MASC-CIDADE2(4:1) TO MASC-CIDADE2
                      END-IF

                      MOVE MASC-EVENTO            TO MASC-EVENTO2
                      IF MASC-EVENTO2(1:1) = SPACES
                         MOVE MASC-EVENTO2(2:2)     TO MASC-EVENTO2
                      END-IF
                      IF MASC-EVENTO2(1:1) = SPACES
                         MOVE MASC-EVENTO2(3:1)     TO MASC-EVENTO2
                      END-IF



                      MOVE DATA-REALIZ-WK(1:4)    TO ANO-INI
                      MOVE DATA-REALIZ-WK(5:2)    TO MES-INI
                      MOVE DATA-REALIZ-WK(7:2)    TO DIA-INI

                      MOVE SPACES                 TO REG-EXCEL
                                                     AJUDA1
                                                     AJUDA2
                                                     AJUDA3
                      STRING MASC-CONTRATO2 ";" MASC-ITEM2
                      DELIMITED BY " "
                      INTO AJUDA1

                      STRING MASC-CIDADE2 DELIMITED BY " "
                      INTO AJUDA2

                      STRING MASC-EVENTO2 DELIMITED BY " "
                      INTO AJUDA3

                      STRING AJUDA1 ";" UF-WK ";"
                             AJUDA2 ";" CIDADE-WK ";" AJUDA3
                             ";" DESCR-EVENTO-WK ";" DIA-INI "/" MES-INI
                             "/" ANO-INI ";" CURSO-WK DELIMITED BY "  "
                             INTO REG-EXCEL
                      WRITE REG-EXCEL
                   END-IF
              END-READ
           END-PERFORM

           CLOSE EXCEL.

       LER-CIDADE SECTION.
           MOVE GS-CIDADE  TO CIDADE
           READ CAD010 INVALID KEY
               INITIALIZE REG-CAD010.

           MOVE CIDADE     TO GS-CIDADE
           MOVE NOME-CID   TO GS-DESC-CIDADE
           MOVE UF-CID     TO GS-UF.


       LER-EVENTO SECTION.
           MOVE GS-EVENTO     TO CODIGO-CO03
           READ COD003 INVALID KEY
               INITIALIZE REG-COD003.

           MOVE CODIGO-CO03   TO GS-EVENTO
           MOVE NOME-CO03     TO GS-DESC-EVENTO.

       POP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP010T".
           MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE
           PERFORM LER-CIDADE.

       POP-EVENTO SECTION.
           CALL   "COP003T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "COP003T".
           MOVE PASSAR-STRING-1(33: 05) TO GS-EVENTO
           PERFORM LER-EVENTO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE GS-DATA-INICIAL(1:2) TO DIA-INI
           MOVE GS-DATA-INICIAL(3:2) TO MES-INI
           MOVE GS-DATA-INICIAL(5:4) TO ANO-INI

           MOVE GS-DATA-FINAL(1:2)   TO DIA-FIM
           MOVE GS-DATA-FINAL(3:2)   TO MES-FIM
           MOVE GS-DATA-FINAL(5:4)   TO ANO-FIM


           INITIALIZE REG-COD060
                      LINHA-WK

           MOVE DATA-INI             TO DATAREALIZA-CO60
           START COD060 KEY IS NOT LESS DATAREALIZA-CO60 INVALID KEY
               MOVE "10" TO ST-COD060.

           PERFORM UNTIL ST-COD060 = "10"
               READ COD060 NEXT AT END
                   MOVE "10" TO ST-COD060
               NOT AT END
                   IF DATA-FIM > 0 AND DATAREALIZA-CO60 > DATA-FIM
                      MOVE "10" TO ST-COD060
                   ELSE
                      IF GS-EVENTO = 0 OR CODEVENTO-CO60
                         MOVE NR-CONTRATO-CO60 TO NR-CONTRATO-CO40
                         READ COD040 INVALID KEY
                              INITIALIZE REG-COD040
                         END-READ
                         IF GS-CIDADE = 0 OR CIDADE-CO40
                            MOVE CIDADE-CO40 TO CIDADE
                            READ CAD010 INVALID KEY
                                 INITIALIZE REG-CAD010
                            END-READ
                            IF GS-UF = SPACES OR UF-CID
                               MOVE NR-CONTRATO-CO40 TO
                                    CONTRATO-WK
                               MOVE NOME-COMPL-CID   TO
                                    CIDADE-WK
                               MOVE UF-CID           TO
                                    UF-WK
                               MOVE DATAREALIZA-CO60 TO
                                    DATA-REALIZ-WK
                               MOVE CODEVENTO-CO60   TO
                                    CODIGO-CO03
                                    COD-EVENTO-WK

                               MOVE ITEM-CO60        TO
                                    ITEM-WK

                               MOVE CIDADE           TO
                                    COD-CIDADE-WK

                               READ COD003 INVALID KEY
                                    INITIALIZE REG-COD003
                               END-READ
                               MOVE NOME-CO03        TO
                                    DESCR-EVENTO-WK

                               PERFORM VERIFICAR-CURSO

                               MOVE DATAREALIZA-CO60 TO
                                    GS-EXIBE-VENCTO
                               MOVE "TELA-AGUARDA1"  TO
                                    DS-PROCEDURE
                               PERFORM CALL-DIALOG-SYSTEM
                               MOVE ZEROS TO EXPORTA-WK
                               ADD 1      TO LINHA-WK
                               WRITE REG-WORK
                            END-IF
                         END-IF
                      END-IF
                   END-IF
               END-READ
           END-PERFORM
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       VERIFICAR-CURSO SECTION.
           INITIALIZE REG-COD041
                      REG-IED011
                      NUMERO-CURSOS
           MOVE NR-CONTRATO-CO40       TO NR-CONTRATO-CO41
           START COD041 KEY IS NOT LESS CHAVE-CO41 INVALID KEY
               MOVE "10" TO ST-COD041.

           PERFORM UNTIL ST-COD041 = "10"
               READ COD041 NEXT AT END
                   MOVE "10" TO ST-COD041
               NOT AT END
                   IF NR-CONTRATO-CO40 <> NR-CONTRATO-CO41 OR
                      NUMERO-CURSOS > 1
                      MOVE "10" TO ST-COD041
                   ELSE
                      ADD 1 TO NUMERO-CURSOS
                      MOVE CURSO-CO41          TO CODIGO-IE11
                      READ IED011 INVALID KEY
                           INITIALIZE REG-IED011
                      END-READ
                   END-IF
               END-READ
           END-PERFORM

           EVALUATE NUMERO-CURSOS
               WHEN 0     MOVE "NENHUM"            TO CURSO-WK
               WHEN 1     MOVE NOME-REDUZ-IE11     TO CURSO-WK
               WHEN OTHER MOVE "VÁRIOS"            TO CURSO-WK
           END-EVALUATE.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.

       ORDEM SECTION.
           INITIALIZE REG-WORK
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "CONTRATO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO CONTRATO-WK
                START WORK KEY IS NOT < ALT1-CHAVE INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "CIDADE" TO GS-DESCR-ORDEM
                MOVE SPACES TO CURSO-WK
                START WORK KEY IS NOT < ALT2-CHAVE INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "UF"          TO GS-DESCR-ORDEM
                MOVE SPACES TO UF-WK
                START WORK KEY IS NOT < ALT3-CHAVE INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "DATA REALIZ." TO GS-DESCR-ORDEM
                MOVE ZEROS          TO DATA-REALIZ-WK
                START WORK KEY IS NOT < ALT4-CHAVE INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "DESCR. EVENTO" TO GS-DESCR-ORDEM
                MOVE SPACES TO DESCR-EVENTO-WK
                START WORK KEY IS NOT < ALT5-CHAVE INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "CURSO"         TO GS-DESCR-ORDEM
                MOVE SPACES TO CURSO-WK
                START WORK KEY IS NOT < ALT6-CHAVE INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.

       MOVER-DADOS-LINDET SECTION.
           MOVE SPACES                TO GS-LINDET

           MOVE CONTRATO-WK           TO GS-LINDET(1: 5)
           MOVE CIDADE-WK             TO GS-LINDET(6:30)
           MOVE UF-WK                 TO GS-LINDET(36:2)
           MOVE DATA-REALIZ-WK(1:4)   TO ANO-INI
           MOVE DATA-REALIZ-WK(5:2)   TO MES-INI
           MOVE DATA-REALIZ-WK(7:2)   TO DIA-INI

           STRING DIA-INI "/" MES-INI "/" ANO-INI INTO GS-LINDET(40:10)

           MOVE DESCR-EVENTO-WK       TO GS-LINDET(51:20)
           MOVE CURSO-WK              TO GS-LINDET(73:20)

           EVALUATE EXPORTA-WK
               WHEN 0 MOVE SPACES     TO GS-LINDET(95:1)
               WHEN 1 MOVE "X"        TO GS-LINDET(95:1)
           END-EVALUATE

           MOVE LINHA-WK              TO GS-LINDET(97:5)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP119" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD003 COD040 COD041 COD060 IED011 CAD010

           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

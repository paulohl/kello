       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCP207.
      *DATA: 20/09/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Resumo de Vendas a vista/prazo/duplicata/antecipado
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".

       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CGPX010.
           COPY RCPX100.
           COPY RCPX101.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CODIGO-WK.

           SELECT WORK1 ASSIGN TO VARIA-W1
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK1
                  RECORD KEY IS CHAVE-WK1.

           SELECT WORK2 ASSIGN TO VARIA-W2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK2
                  RECORD KEY IS CHAVE-WK2.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CGPW010.
       COPY RCPW100.
       COPY RCPW101.

       FD  WORK.
       01  REG-WORK.
           05  CODIGO-WK           PIC 9(6).
           05  VLR-VISTA-WK        PIC 9(8)V99.
           05  VLR-ANTECIPADO-WK   PIC 9(8)V99.
           05  VLR-PRAZO-WK        PIC 9(8)V99.
           05  VLR-DUPLICATA-WK    PIC 9(8)V99.
           05  VLR-DEBITO-WK       PIC 9(8)V99.
           05  VLR-CARTAO-WK       PIC 9(8)V99.
           05  VLR-DESCCOM-WK      PIC 9(8)V99.

       FD  WORK1.
       01  REG-WORK1.
           05 CHAVE-WK1.
              10 COND-WK1          PIC X(10).
              10 CODIGO-WK1        PIC 9(06).
              10 TIPO-REC-WK1      PIC 9(01).
           05  VALOR-WK1           PIC 9(08)V99.

       FD  WORK2.
       01  REG-WORK2.
           05 CHAVE-WK2.
              10 COND-WK2          PIC X(10).
              10 CODIGO-WK2        PIC 9(06).
              10 TIPO-REC-WK2      PIC 9(01).
              10 ALBUM-REC-WK2     PIC 9(08).
              10 DATA-MOV-WK2      PIC 9(08).
              10 DATA-REC-WK2      PIC 9(08).
              10 VALOR-REC-WK2     PIC 9(08)V99.
              10 DOCTO-WK2         PIC X(10).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "RCP207.CPB".
           COPY "RCP207.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK1              PIC XX       VALUE SPACES.
           05  ST-WORK2              PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
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
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  DATA-W                PIC 9(8)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  TOT-VENDA-BRUTA       PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-VISTA         PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-PRAZO         PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-DUPLICATA     PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-ANTECIPADO    PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-DEBITO        PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-CARTAO        PIC 9(8)V99  VALUE ZEROS.
           05  TOT-GER-DESSCOM       PIC 9(8)V99  VALUE ZEROS.
           05  PASSAR-STRING-1       PIC X(65).
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  VARIA-W1              PIC 9(08)    VALUE ZEROS.
           05  VARIA-W2              PIC 9(08)    VALUE ZEROS.
           05  AUX-COND              PIC X(10)    VALUE ZEROS.
           05  AUX-CODIGO            PIC 9(06)    VALUE ZEROS.
           05  AUX-TIPO-REC          PIC 9(01)    VALUE ZEROS.
           05  MASC-DATA             PIC 99/99/9999 VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(35)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(51)   VALUE
           "RESUMO VENDAS A VISTA/PRAZO/DUPL/ANTECIP/DEB/CARTAO".
           05  FILLER              PIC X(05)   VALUE SPACES.
           05  DET-TIPO-REL        PIC X(11)   VALUE SPACES.
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(111)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(108)  VALUE
           "CODIGO NOME-VENDEDOR     VALOR-A-VISTA VALOR-A-PRAZO VALOR-A
      -    "NTECIP VLR-BOLETO/DP VLR-DEBT-AUT.    VLR-CARTAO".

       01  LINDET.
           05  LINDET-REL          PIC X(125)  VALUE SPACES.
       01  CAB05.
           05  FILLER              PIC X(124)  VALUE
                "   VENDA-BRUTA VALOR-A-VISTA    VALOR-A-PRAZO    VALOR-
      -    "ANTECIP  VALOR-BOLETO/DP  VLR-DEBITO-AUT.      VLR-CARTAO VL
      -    "R-DESCCOM".


       01  LINTOT.
           05  LINTOT-REL          PIC X(125)  VALUE SPACES.

       01  CAB-03.
           05 FILLER               PIC X(09)
              VALUE "ALBUM".
           05 FILLER               PIC X(11)
              VALUE " DT.MOVTO".
           05 FILLER               PIC X(11)
              VALUE " DT.VECTO".
           05 FILLER               PIC X(17)
              VALUE "           VALOR".
           05 FILLER               PIC X(30)
              VALUE "COMPRADOR".

       01 CAB-04.
          05 FILLER                PIC X(78)
             VALUE ALL "=".


       01 DET-01.
          05 FILLER                PIC X(07)
             VALUE "PRAZO: ".
          05 DET-COND              PIC X(20).
          05 FILLER                PIC X(07)
             VALUE "TIPO.: ".
          05 DET-TIPO-REC          PIC X(30).

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO      TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "RCD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "RCD101"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RCD101.
           ACCEPT VARIA-W  FROM TIME.

           ACCEPT VARIA-W1 FROM TIME.
           ADD 100 TO VARIA-W1

           ACCEPT VARIA-W2 FROM TIME.
           ADD 200 TO VARIA-W2

           OPEN OUTPUT WORK   CLOSE WORK   OPEN I-O WORK
           OPEN OUTPUT WORK1  CLOSE WORK1  OPEN I-O WORK1
           OPEN OUTPUT WORK2  CLOSE WORK2  OPEN I-O WORK2

           OPEN INPUT CGD001 CGD010 RCD100 RCD101.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD101 <> "00"
              MOVE "ERRO ABERTURA RCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD101 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-POPUP-VENDEDOR-TRUE
                    PERFORM CHAMAR-POPUP-VENDEDOR
               WHEN GS-LE-VENDEDOR-TRUE
                    PERFORM LE-VENDEDOR
               WHEN GS-DETALHAR-FLG-TRUE
                    PERFORM DETALHAR-DADOS
               WHEN GS-DETALHAR2-FLG-TRUE
                    PERFORM DETALHAR2-DADOS
               WHEN GS-DETALHAR3-FLG-TRUE
                    PERFORM DETALHAR3-DADOS
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


       DETALHAR-DADOS SECTION.
           MOVE "CLEAR-LB2"        TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "CLEAR-LB3"        TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "DESMARCAR"        TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-LINDET(1:6)     TO AUX-CODIGO.


       DETALHAR2-DADOS SECTION.
           MOVE GS-LINDET(30:1)    TO AUX-TIPO-REC

           MOVE "CLEAR-LB3"        TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE CAB-03             TO GS-LINDET
           MOVE "INSERIR-LB3"  TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE CAB-04             TO GS-LINDET
           MOVE "INSERIR-LB3"  TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-WORK2
           MOVE AUX-COND           TO COND-WK2
           MOVE AUX-CODIGO         TO CODIGO-WK2
           MOVE AUX-TIPO-REC       TO TIPO-REC-WK2
           START WORK2 KEY IS NOT LESS CHAVE-WK2 INVALID KEY
                MOVE "10" TO ST-WORK2.

           PERFORM UNTIL ST-WORK2 = "10"
                READ WORK2 NEXT AT END
                     MOVE "10" TO ST-WORK2
                NOT AT END
                     IF AUX-COND     <> COND-WK2     OR
                        AUX-CODIGO   <> CODIGO-WK2   OR
                        AUX-TIPO-REC <> TIPO-REC-WK2
                        MOVE "10" TO ST-WORK2
                     ELSE
                        MOVE SPACES         TO GS-LINDET

                        MOVE ALBUM-REC-WK2  TO GS-LINDET(1:8)
                                               CODIGO-CG10
                        MOVE 0              TO CLASSIF-CG10
                        READ CGD010 INVALID KEY
                             MOVE "*****"   TO COMPRADOR-CG10
                        END-READ

                        MOVE DATA-MOV-WK2   TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV       TO MASC-DATA
                        MOVE MASC-DATA      TO GS-LINDET(10:10)

                        MOVE DATA-REC-WK2   TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV       TO MASC-DATA
                        MOVE MASC-DATA      TO GS-LINDET(21:10)

                        MOVE VALOR-REC-WK2  TO VALOR-E
                        MOVE VALOR-E        TO GS-LINDET(35:13)

                        MOVE COMPRADOR-CG10 TO GS-LINDET(49:30)

                        MOVE "INSERIR-LB3"  TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                     END-IF
                END-READ
           END-PERFORM.

       DETALHAR3-DADOS SECTION.
           MOVE "CLEAR-LB2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "CLEAR-LB3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-WORK1

           MOVE GS-LINDET          TO AUX-COND
                                      COND-WK1

           MOVE AUX-CODIGO         TO CODIGO-WK1

           START WORK1 KEY IS NOT LESS CHAVE-WK1 INVALID KEY
                MOVE "10" TO ST-WORK1.

           PERFORM UNTIL ST-WORK1 = "10"
                READ WORK1 NEXT AT END
                     MOVE "10" TO ST-WORK1
                NOT AT END
                     IF AUX-COND   <> COND-WK1    OR
                        AUX-CODIGO <> CODIGO-WK1
                        MOVE "10" TO ST-WORK1
                     ELSE
                        MOVE SPACES                 TO GS-LINDET
                        EVALUATE TIPO-REC-WK1
                           WHEN 1 MOVE "Cheque"     TO GS-LINDET(1:10)
                           WHEN 2 MOVE "Moeda"      TO GS-LINDET(1:10)
                           WHEN 3 MOVE "Antecipado" TO GS-LINDET(1:10)
                           WHEN 4 MOVE "Dup/Promis" TO GS-LINDET(1:10)
                           WHEN 5 MOVE "Deb.Autom"  TO GS-LINDET(1:10)
                           WHEN 6 MOVE "Cart Cred"  TO GS-LINDET(1:10)
                           WHEN 9 MOVE "Desc.Comissão" TO
                                       GS-LINDET(1:10)
                        END-EVALUATE
                        MOVE VALOR-WK1              TO VALOR-E
                        MOVE VALOR-E                TO GS-LINDET(13:13)

                        MOVE TIPO-REC-WK1           TO GS-LINDET(30:1)

                        MOVE "INSERIR-LB2" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                     END-IF
                END-READ
           END-PERFORM.

       CHAMAR-POPUP-VENDEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-VENDEDOR.

       LE-VENDEDOR SECTION.
           MOVE GS-VENDEDOR        TO CODIGO-CG01.
           READ CGD001 INVALID KEY
                MOVE "****"        TO NOME-CG01.

           MOVE NOME-CG01          TO GS-NOME-VENDEDOR.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

      *--------------------------------------------------------------
       GRAVA-WORK SECTION.
           CLOSE WORK   OPEN OUTPUT WORK   CLOSE WORK   OPEN I-O WORK
           CLOSE WORK1  OPEN OUTPUT WORK1  CLOSE WORK1  OPEN I-O WORK1
           CLOSE WORK2  OPEN OUTPUT WORK2  CLOSE WORK2  OPEN I-O WORK2

           MOVE ZEROS TO TOT-GER-VISTA TOT-GER-PRAZO TOT-GER-ANTECIPADO
                         TOT-GER-DUPLICATA TOT-GER-DEBITO
                         TOT-GER-CARTAO TOT-VENDA-BRUTA TOT-GER-DESSCOM


           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.


           EVALUATE GS-TIPO-REL
               WHEN 1 PERFORM POR-MOVTO
               WHEN 2 PERFORM POR-VENDA
           END-EVALUATE

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       POR-MOVTO SECTION.
           MOVE GS-VECTO-INI TO DATA-INV VECTO-INI-REL
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO VECTO-INI.
           MOVE GS-VECTO-FIM TO DATA-INV VECTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO VECTO-FIM.

           INITIALIZE REG-RCD100
           MOVE VECTO-INI    TO DATA-MOVTO-REC.
           COMPUTE DATA-MOVTO-REC = DATA-MOVTO-REC - 1
           MOVE ALL "9" TO ALBUM-REC
           START RCD100 KEY IS GREATER THAN ALT-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
            READ RCD100 NEXT RECORD AT END
                 MOVE "10" TO ST-RCD100
            NOT AT END
                 IF DATA-MOVTO-REC > VECTO-FIM
                    MOVE "10" TO ST-RCD100
                 ELSE
                    IF GS-VENDEDOR = 0 OR VENDEDOR-REC
                       MOVE DATA-MOVTO-REC TO GS-EXIBE-MOVTO
                       MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                       PERFORM VERIFICA-RCD101
                    END-IF
                 END-IF
             END-READ
           END-PERFORM.

       POR-VENDA SECTION.
           MOVE GS-VECTO-INI TO DATA-INV VECTO-INI-REL
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO VECTO-INI.
           MOVE GS-VECTO-FIM TO DATA-INV VECTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV     TO VECTO-FIM.

           INITIALIZE REG-RCD100
           MOVE VECTO-INI    TO DATAVEN-REC.
           COMPUTE DATAVEN-REC = DATAVEN-REC - 1
           START RCD100 KEY IS GREATER THAN DATAVEN-REC INVALID KEY
                 MOVE "10" TO ST-RCD100.
           PERFORM UNTIL ST-RCD100 = "10"
            READ RCD100 NEXT RECORD AT END
                 MOVE "10" TO ST-RCD100
            NOT AT END
                 IF DATAVEN-REC > VECTO-FIM
                    MOVE "10" TO ST-RCD100
                 ELSE
                    IF GS-VENDEDOR = 0 OR VENDEDOR-REC
                       MOVE DATAVEN-REC TO GS-EXIBE-MOVTO
                       MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                       PERFORM VERIFICA-RCD101
                    END-IF
                 END-IF
             END-READ
           END-PERFORM.


       VERIFICA-RCD101 SECTION.
           MOVE ZEROS TO REG-RCD101

           MOVE ALBUM-REC     TO ALBUM-REC1
           COMPUTE ALBUM-REC1 = ALBUM-REC1 - 1

           MOVE ALL "9"       TO VENCTO-REC1 BANCO-REC1 NUMERO-REC1.
      *    MOVE ZEROS         TO VENCTO-REC1 BANCO-REC1 NUMERO-REC1.

           START RCD101 KEY IS GREATER THAN CHAVE-REC1 INVALID KEY
                 MOVE "10" TO ST-RCD101
           END-START
           PERFORM UNTIL ST-RCD101 = "10"
               READ RCD101 NEXT RECORD AT END
                    MOVE "10" TO ST-RCD101
               NOT AT END
                    IF ALBUM-REC1 <> ALBUM-REC
                       MOVE "10" TO ST-RCD101
                    ELSE
                       ADD  VALOR-REC1      TO TOT-VENDA-BRUTA
                       MOVE VENDEDOR-REC    TO CODIGO-WK
                       READ WORK INVALID KEY
                            PERFORM GRAVA-DADOS-WORK
                       NOT INVALID KEY
                            PERFORM REGRAVA-DADOS-WORK
                       END-READ

                       MOVE CODIGO-WK          TO CODIGO-WK1
                       MOVE TIPO-REC1          TO TIPO-REC-WK1
                       READ WORK1 INVALID KEY
                            MOVE VALOR-REC1    TO VALOR-WK1
                            WRITE REG-WORK1
                            END-WRITE
                       NOT INVALID KEY
                            ADD  VALOR-REC1    TO VALOR-WK1
                            REWRITE REG-WORK1
                            END-REWRITE
                       END-READ

                       MOVE COND-WK1           TO COND-WK2
                       MOVE CODIGO-WK          TO CODIGO-WK2
                       MOVE TIPO-REC1          TO TIPO-REC-WK2
                       MOVE ALBUM-REC1         TO ALBUM-REC-WK2
                       MOVE VENCTO-REC1        TO DATA-REC-WK2
                       MOVE VALOR-REC1         TO VALOR-REC-WK2
                       MOVE DATA-MOVTO-REC     TO DATA-MOV-WK2
                       MOVE NUMERO-REC1        TO DOCTO-WK2
                       WRITE REG-WORK2

                    END-IF
               END-READ
           END-PERFORM.
       GRAVA-DADOS-WORK SECTION.
           MOVE SPACES TO COND-WK1 COND-WK2
           MOVE ZEROS TO VLR-VISTA-WK VLR-PRAZO-WK VLR-ANTECIPADO-WK
                         VLR-DUPLICATA-WK VLR-DEBITO-WK VLR-CARTAO-WK
                         VLR-DESCCOM-WK.
           EVALUATE TIPO-REC1
             WHEN 1
                IF VENCTO-REC1 NOT > DATA-MOVTO-REC
                   MOVE VALOR-REC1     TO VLR-VISTA-WK
                ELSE
                   MOVE VALOR-REC1     TO VLR-PRAZO-WK
                END-IF
             WHEN 3 MOVE VALOR-REC1    TO VLR-ANTECIPADO-WK
             WHEN 2 MOVE VALOR-REC1    TO VLR-VISTA-WK
             WHEN 4 MOVE VALOR-REC1    TO VLR-DUPLICATA-WK
             WHEN 5 MOVE VALOR-REC1    TO VLR-DEBITO-WK
             WHEN 6 MOVE VALOR-REC1    TO VLR-CARTAO-WK
             WHEN 9 MOVE VALOR-REC1    TO VLR-DESCCOM-WK
           END-EVALUATE

           IF TIPO-REC1 = 2
              MOVE "A VISTA" TO COND-WK1
           ELSE
              IF VENCTO-REC1 NOT > DATA-MOVTO-REC
                 MOVE "A VISTA"           TO COND-WK1
              ELSE
                 MOVE "A PRAZO"           TO COND-WK1
              END-IF
           END-IF
           WRITE REG-WORK.

      *    1 CHEQUE - 2 MOEDA - 3 ANTECIPADO - 4 DUPLI/NT.PROM
      *    5 - DEBITO AUTOMATICO 6 - CARTÃO DE CRÉDITO

       REGRAVA-DADOS-WORK SECTION.
           MOVE SPACES TO COND-WK1 COND-WK2
           EVALUATE TIPO-REC1
             WHEN 1
                IF VENCTO-REC1 NOT > DATA-MOVTO-REC
                   ADD VALOR-REC1     TO VLR-VISTA-WK
                ELSE ADD VALOR-REC1   TO VLR-PRAZO-WK
             WHEN 3 ADD VALOR-REC1    TO VLR-ANTECIPADO-WK
             WHEN 2 ADD VALOR-REC1    TO VLR-VISTA-WK
             WHEN 4 ADD VALOR-REC1    TO VLR-DUPLICATA-WK
             WHEN 5 ADD VALOR-REC1    TO VLR-DEBITO-WK
             WHEN 6 ADD VALOR-REC1    TO VLR-CARTAO-WK
             WHEN 9 ADD VALOR-REC1    TO VLR-DESCCOM-WK
           END-EVALUATE.

           IF TIPO-REC1 = 2
              MOVE "A VISTA" TO COND-WK1
           ELSE
              IF VENCTO-REC1 NOT > DATA-MOVTO-REC
                 MOVE "A VISTA"           TO COND-WK1
              ELSE
                 MOVE "A PRAZO"           TO COND-WK1
              END-IF
           END-IF
           REWRITE REG-WORK.
      *    1 CHEQUE - 2 MOEDA  - 3 ANTECIPADO - 4 DUPLI/NT.PROM
      *    5 - DEBITO AUTOMATICO 6 - CARTÃO DE CRÉDITO


      *-------------------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.

           MOVE ZEROS TO CODIGO-WK.
           START WORK KEY IS NOT < CODIGO-WK INVALID KEY
              MOVE "10" TO ST-WORK.

           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-LINDET
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA.

       MOVER-DADOS-LINDET SECTION.
           MOVE SPACES            TO GS-LINDET.
           MOVE CODIGO-WK         TO GS-LINDET(1: 7) CODIGO-CG01
           READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01(1: 17)  TO GS-LINDET(8: 18)
           MOVE VLR-VISTA-WK      TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(26: 14)
           MOVE VLR-PRAZO-WK      TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(40: 14)
           MOVE VLR-ANTECIPADO-WK TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(54: 14)
           MOVE VLR-DUPLICATA-WK  TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(68: 14)
           MOVE VLR-DEBITO-WK     TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(82: 14)
           MOVE VLR-CARTAO-WK     TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(96: 13)
           MOVE VLR-DESCCOM-WK    TO VALOR-E
           MOVE VALOR-E           TO GS-LINDET(110:13)

           ADD VLR-VISTA-WK       TO TOT-GER-VISTA.
           ADD VLR-PRAZO-WK       TO TOT-GER-PRAZO.
           ADD VLR-ANTECIPADO-WK  TO TOT-GER-ANTECIPADO.
           ADD VLR-DUPLICATA-WK   TO TOT-GER-DUPLICATA
           ADD VLR-DEBITO-WK      TO TOT-GER-DEBITO
           ADD VLR-CARTAO-WK      TO TOT-GER-CARTAO
           ADD VLR-DESCCOM-WK     TO TOT-GER-DESSCOM.

       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE TOT-VENDA-BRUTA    TO VALOR-E
           MOVE VALOR-E            TO GS-LINTOT(02:14)
           MOVE TOT-GER-VISTA      TO VALOR-E
           MOVE VALOR-E            TO GS-LINTOT(16: 17)
           MOVE TOT-GER-PRAZO      TO VALOR-E
           MOVE VALOR-E            TO GS-LINTOT(33: 17)
           MOVE TOT-GER-ANTECIPADO TO VALOR-E
           MOVE VALOR-E            TO GS-LINTOT(50: 17)
           MOVE TOT-GER-DUPLICATA  TO VALOR-E
           MOVE VALOR-E            TO GS-LINTOT(67: 17)
           MOVE TOT-GER-DEBITO     TO VALOR-E
           MOVE VALOR-E            TO GS-LINTOT(84: 16)
           MOVE TOT-GER-CARTAO     TO VALOR-E
           MOVE VALOR-E            TO GS-LINTOT(100: 13)
           MOVE TOT-GER-DESSCOM    TO VALOR-E
           MOVE VALOR-E            TO GS-LINTOT(114: 13)


           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "RCP207" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *----------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           EVALUATE GS-TIPO-REL
               WHEN 1 MOVE "INT.MOVTO: " TO DET-TIPO-REL
               WHEN 2 MOVE "INT.VECTO: " TO DET-TIPO-REL
           END-EVALUATE

           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.

           IF GS-IMPRESSAO = 1
              MOVE AUX-CODIGO    TO CODIGO-WK
              READ WORK NOT INVALID KEY
                   PERFORM MOVER-DADOS-RELATORIO
                   MOVE AUX-COND        TO DET-COND
                   EVALUATE AUX-TIPO-REC
                       WHEN 1 MOVE "Cheque"         TO DET-TIPO-REC
                       WHEN 2 MOVE "Moeda"          TO DET-TIPO-REC
                       WHEN 3 MOVE "Antecipado"     TO DET-TIPO-REC
                       WHEN 4 MOVE "Dupl/Promis"    TO DET-TIPO-REC
                       WHEN 5 MOVE "Deb.Automatico" TO DET-TIPO-REC
                       WHEN 6 MOVE "Cartao Credito" TO DET-TIPO-REC
                       WHEN 9 MOVE "Desc.Comissão"  TO DET-TIPO-REC
                   END-EVALUATE

                   WRITE REG-RELAT FROM CAB03
                   ADD 1 TO LIN

                   MOVE SPACES          TO LINDET-REL
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN

                   MOVE SPACES          TO LINDET-REL
                   MOVE DET-01          TO LINDET-REL
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN

                   MOVE SPACES          TO LINDET-REL
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN

                   MOVE 1               TO GS-LINHA
                   MOVE SPACES          TO GS-LINDET
                   MOVE "LER-LB3"       TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM

                   PERFORM UNTIL GS-LINDET = SPACES

                       MOVE SPACES      TO LINDET-REL
                       MOVE GS-LINDET   TO LINDET-REL
                       WRITE REG-RELAT FROM LINDET
                       ADD 1 TO LIN
                       IF LIN > 56
                          PERFORM CABECALHO
                       END-IF

                       ADD 1 TO GS-LINHA
                       MOVE SPACES TO GS-LINDET
                       MOVE "LER-LB3"       TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                   END-PERFORM

                   MOVE SPACES  TO LINDET-REL
                   MOVE ALL "=" TO LINDET-REL(35:13)
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN
                   IF LIN > 56
                      PERFORM CABECALHO
                   END-IF

                   MOVE AUX-COND       TO COND-WK1
                   MOVE AUX-CODIGO     TO CODIGO-WK1
                   MOVE AUX-TIPO-REC   TO TIPO-REC-WK1
                   READ WORK1 INVALID KEY
                        MOVE ZEROS     TO VALOR-WK1
                   END-READ
                   MOVE VALOR-WK1      TO VALOR-E

                   MOVE SPACES         TO LINDET-REL
                   MOVE VALOR-E        TO LINDET-REL(35:13)
                   WRITE REG-RELAT FROM LINDET
                   ADD 1 TO LIN
                   IF LIN > 56
                      PERFORM CABECALHO
                   END-IF
              END-READ
           ELSE
              MOVE ZEROS TO CODIGO-WK
              START WORK KEY IS NOT < CODIGO-WK INVALID KEY
                  MOVE "10" TO ST-WORK
              END-START

              PERFORM UNTIL ST-WORK = "10"
                  READ WORK NEXT RECORD AT END
                       MOVE "10" TO ST-WORK
                  NOT AT END
                       PERFORM MOVER-DADOS-RELATORIO
                  END-READ
              END-PERFORM
              PERFORM TOTALIZA-REL
           END-IF

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE CODIGO-WK         TO LINDET-REL(1: 7) CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE SPACES TO NOME-CG01.
           MOVE NOME-CG01(1: 17)  TO LINDET-REL(8: 18)
           MOVE VLR-VISTA-WK      TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(26: 14)
           MOVE VLR-PRAZO-WK      TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(40: 14)
           MOVE VLR-ANTECIPADO-WK TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(54: 14)
           MOVE VLR-DUPLICATA-WK  TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(68: 14)
           MOVE VLR-DEBITO-WK     TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(82: 14)
           MOVE VLR-CARTAO-WK     TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(96: 13)
           MOVE VLR-DESCCOM-WK    TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(110: 13)

           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.


       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINTOT-REL.
           MOVE GS-LINTOT TO LINTOT-REL.

           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM LINTOT.
       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.

           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 5 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 CGD010 RCD100 RCD101 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

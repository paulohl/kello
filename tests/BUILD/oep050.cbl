       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OEP050.
      *DATA: 17/01/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RESUMO DE ARRECADAÇÃO DE ORGANIZAÇÃO DE EVENTO
      *FUNÇÃO: Listar todos os títulos que pertencerem ao contrato sele-
      *        cionado, e tipo-docto-cr20 = 2(org.evento).
      *        As ordens serão: Código, nome.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX010.
           COPY CGPX020.
           COPY CRPX020.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CLIENTE-WK
                  ALTERNATE RECORD KEY IS NOME-CLIEN-WK WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW010.
       COPY CGPW020.
       COPY CRPW020.
       FD  WORK.
       01  REG-WORK.
           05  CLASSIF-WK          PIC 9.
           05  CLIENTE-WK          PIC 9(8).
           05  NOME-CLIEN-WK       PIC X(30).
           05  VLR-TOTAL-WK        PIC 9(8)V99.
           05  PARC-TOTAL-WK       PIC 9(2).
           05  VLR-PAGO-WK         PIC 9(8)V99.
           05  PARC-PAGO-WK        PIC 9(2).
           05  VLR-ATRASADO-WK     PIC 9(8)V99.
           05  PARC-ATRASADO-WK    PIC 9(2).
           05  VLR-RESTANTE-WK     PIC 9(8)V99.
           05  PARC-RESTANTE-WK    PIC 9(2).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "OEP050.CPB".
           COPY "OEP050.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CGD020             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.

           05  ST-WORK               PIC XX       VALUE SPACES.
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
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VLR-GERAL-TOT         PIC 9(8)V99  VALUE ZEROS.
           05  LIN                   PIC 9(02).
           05  PARC-GERAL-TOT        PIC 9(4)     VALUE ZEROS.
           05  VLR-PAGO-TOT          PIC 9(8)V99  VALUE ZEROS.
           05  PARC-PAGO-TOT         PIC 9(4)     VALUE ZEROS.
           05  VLR-ATRASADO-TOT      PIC 9(8)V99  VALUE ZEROS.
           05  PARC-ATRASADO-TOT     PIC 9(4)     VALUE ZEROS.
           05  VLR-RESTANTE-TOT      PIC 9(8)V99  VALUE ZEROS.
           05  PARC-RESTANTE-TOT     PIC 9(4)     VALUE ZEROS.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  CLIENTE-W             PIC 9(8)     VALUE ZEROS.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  SEQ-W                 PIC 9(5)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(30)    VALUE SPACES.
           05  AUX-TIPO              PIC 9(01)    VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(60)   VALUE
           "RESUMO DE ARRECADACAO DE ORGANIZACAO DE EVENTOS - ORDEM: ".
           05  ORDEM-REL           PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(6)    VALUE "TIPO: ".
           05  TIPO-REL            PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE "CONTRATO: ".
           05  CONTRATO-REL        PIC 9(4)    VALUE ZEROS.
       01  CAB03.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "COD. NOME                                   TOTAL
      -    "   PAGO          ATRASADO          RESTANTE ".
       01  LINDET.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  LINTOT.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "Total Geral...:".
           05  TOTAL-GERAL-REL     PIC x(20)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "Total Atrasado:".
           05  TOTAL-ATRASADO-REL  PIC X(20)   VALUE SPACES.
       01  LINTOT1.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "Total Recebido:".
           05  TOTAL-PAGO-REL      PIC x(20)   VALUE SPACES.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "Total Restante:".
           05  TOTAL-RESTANTE-REL  PIC X(20)   VALUE SPACES.

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
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD020.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           OPEN INPUT CGD010 CRD020 CGD020.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD020 <> "00"
              MOVE "ERRO ABERTURA CGD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

      *    MOVE 1 TO COD-USUARIO-W
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
      *             PERFORM VERIFICA-VENCTO-ANT
                    PERFORM CRIA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-LE-CARTAO-TRUE
                    PERFORM LER-CARTAO
               WHEN GS-POPUP-CARTAO-TRUE
                    PERFORM POPUP-CARTAO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       LER-CARTAO SECTION.
           MOVE GS-ACP-CARTAO TO CODIGO-CG20
           READ CGD020 INVALID KEY
                MOVE SPACES TO NOME-CG20.

           MOVE NOME-CG20 TO GS-DESC-CARTAO.

       POPUP-CARTAO SECTION.
           CALL   "CGP020T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CGP020T"
           MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-CARTAO
           MOVE PASSAR-STRING-1(33: 2) TO GS-ACP-CARTAO.

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
       CRIA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           ACCEPT VARIA-W FROM TIME
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE FUNCTION NUMVAL(GS-TIPO(1:1))   TO AUX-TIPO

           INITIALIZE REG-CRD020
           MOVE 0             TO COD-COMPL-CR20(1: 1)
           MOVE GS-CONTRATO   TO COD-COMPL-CR20(2: 4)
           MOVE ZEROS         TO COD-COMPL-CR20(6: 4)
                                 SEQ-CR20

           START CRD020 KEY IS NOT < CHAVE-CR20 INVALID KEY
                  MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
             READ CRD020 NEXT RECORD AT END
                  MOVE "10" TO ST-CRD020
             NOT AT END
                  MOVE COD-COMPL-CR20(2: 4) TO CONTRATO-W
                  IF GS-CONTRATO <> CONTRATO-W
                     MOVE "10" TO ST-CRD020
                  ELSE
                    IF SITUACAO-CR20 > 2
                       CONTINUE
                    ELSE
                       IF AUX-TIPO = 9 OR TIPO-DOCTO-CR20
                          IF GS-ACP-CARTAO = 0 OR
                             CARTAO-CRED-CR20
                             PERFORM CONT-GRAVA-WORK
                          END-IF
                       END-IF
                    END-IF
                  END-IF
             END-READ
           END-PERFORM.

           PERFORM GRAVA-CLIENTE-SFINANC

           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CONT-GRAVA-WORK SECTION.
      *    IF TIPO-DOCTO-CR20 <> 2
      *       CONTINUE
      *    ELSE
              MOVE CLIENTE-CR20 TO CLIENTE-WK GS-EXIBE-VENCTO
              READ WORK INVALID KEY
                   INITIALIZE REG-WORK
                   PERFORM GRAVA-WORK
              NOT INVALID KEY
                   PERFORM REGRAVA-WORK
              END-READ
              MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.
      *    END-IF.

       GRAVA-WORK SECTION.
           MOVE CLASS-CLIENTE-CR20   TO CLASSIF-WK
                                        CLASSIF-CG10
           MOVE CLIENTE-CR20         TO CLIENTE-WK
                                        CODIGO-CG10

           READ CGD010 INVALID KEY
                MOVE "*******"       TO COMPRADOR-CG10.

           MOVE COMPRADOR-CG10       TO NOME-CLIEN-WK
           MOVE VALOR-TOT-CR20       TO VLR-TOTAL-WK
           MOVE 1                    TO PARC-TOTAL-WK
           IF DATA-RCTO-CR20 <> ZEROS
              MOVE VALOR-LIQ-CR20    TO VLR-PAGO-WK
              MOVE 1                 TO PARC-PAGO-WK
              MOVE ZEROS             TO VLR-ATRASADO-WK
                                        PARC-ATRASADO-WK
                                        VLR-RESTANTE-WK
                                        PARC-RESTANTE-WK
           ELSE
              IF DATA-VENCTO-CR20 < DATA-DIA-I
                 MOVE VALOR-TOT-CR20    TO VLR-ATRASADO-WK
                 MOVE 1                 TO PARC-ATRASADO-WK
                 MOVE ZEROS             TO VLR-PAGO-WK
                                           PARC-PAGO-WK
                                           VLR-RESTANTE-WK
                                           PARC-RESTANTE-WK
              ELSE
                 MOVE VALOR-TOT-CR20    TO VLR-RESTANTE-WK
                 MOVE 1                 TO PARC-RESTANTE-WK
                 MOVE ZEROS             TO VLR-PAGO-WK
                                           PARC-PAGO-WK
                                           VLR-ATRASADO-WK
                                           PARC-ATRASADO-WK
              END-IF
           END-IF.
           WRITE REG-WORK.
       REGRAVA-WORK SECTION.
           ADD VALOR-TOT-CR20       TO VLR-TOTAL-WK.
           ADD 1                    TO PARC-TOTAL-WK.
           IF DATA-RCTO-CR20 <> ZEROS
              ADD VALOR-LIQ-CR20    TO VLR-PAGO-WK
              ADD 1                 TO PARC-PAGO-WK
           ELSE
             IF DATA-VENCTO-CR20 < DATA-DIA-I
              ADD VALOR-TOT-CR20    TO VLR-ATRASADO-WK
              ADD 1                 TO PARC-ATRASADO-WK
             ELSE
              ADD VALOR-TOT-CR20    TO VLR-RESTANTE-WK
              ADD 1                 TO PARC-RESTANTE-WK
             END-IF
           END-IF.
           REWRITE REG-WORK.
       GRAVA-CLIENTE-SFINANC SECTION.
      *CASO CLIENTE ESTEJA CADASTRADO NO CONTRATO MAIS NÁO TEM NENHUM
      *LANCAMENTO NO FINANCEIRO, TAMBEM DEVE LISTAR.
           MOVE GS-CONTRATO   TO CODIGO-CG10(1: 4)
           MOVE ZEROS         TO CODIGO-CG10(5: 4)
           MOVE 0             TO CLASSIF-CG10
           START CGD010 KEY IS NOT < COD-COMPL-CG10 INVALID KEY
                  MOVE "10" TO ST-CGD010.
           PERFORM UNTIL ST-CGD010 = "10"
             READ CGD010 NEXT RECORD AT END MOVE "10" TO ST-CGD010
              NOT AT END
               MOVE CODIGO-CG10   TO CONTRATO-W
               IF  GS-CONTRATO <> CONTRATO-W   OR
                   CLASSIF-CG10 <> 0
                         MOVE "10" TO ST-CGD010
               ELSE
                 MOVE CODIGO-CG10         TO CLIENTE-WK
                 READ WORK INVALID KEY
                      MOVE 0              TO CLASSIF-WK
                      MOVE COMPRADOR-CG10 TO NOME-CLIEN-WK
                      MOVE ZEROS          TO VLR-TOTAL-WK
                                             PARC-TOTAL-WK
                                             VLR-PAGO-WK
                                             PARC-PAGO-WK
                                             VLR-ATRASADO-WK
                                             PARC-ATRASADO-WK
                                             VLR-RESTANTE-WK
                                             PARC-RESTANTE-WK

                       WRITE REG-WORK
                       END-WRITE
                     NOT INVALID KEY
                         CONTINUE
                 END-READ
              END-IF
             END-READ
           END-PERFORM.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO VLR-GERAL-TOT PARC-GERAL-TOT
                         VLR-PAGO-TOT PARC-PAGO-TOT
                         VLR-ATRASADO-TOT PARC-ATRASADO-TOT
                         VLR-RESTANTE-TOT PARC-RESTANTE-TOT.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                 IF GS-ORDEM-RADIO = 2 AND VLR-ATRASADO-WK = ZEROS
                    CONTINUE
                 ELSE
                   PERFORM MOVER-DADOS-LINDET
                 END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "CÓDIGO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO CLIENTE-WK
                START WORK KEY IS NOT < CLIENTE-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "CLIENTE" TO GS-DESCR-ORDEM
                MOVE SPACES TO NOME-CLIEN-WK
                START WORK KEY IS NOT < NOME-CLIEN-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           MOVE SPACES            TO GS-LINDET
           MOVE CLIENTE-WK(5: 4)  TO GS-LINDET(1: 5)
           MOVE NOME-CLIEN-WK     TO GS-LINDET(06: 31)
           MOVE VLR-TOTAL-WK      TO VALOR-E
      *    ADD VLR-TOTAL-WK       TO VLR-GERAL-TOT
           MOVE VALOR-E           TO GS-LINDET(37: 13)
           MOVE "("               TO GS-LINDET(50: 1)
           MOVE PARC-TOTAL-WK     TO GS-LINDET(51: 2)
           ADD PARC-TOTAL-WK      TO PARC-GERAL-TOT
           MOVE ")"               TO GS-LINDET(53: 2)
           MOVE VLR-PAGO-WK       TO VALOR-E
           ADD VLR-PAGO-WK        TO VLR-PAGO-TOT
           MOVE VALOR-E           TO GS-LINDET(55: 13)
           MOVE "("               TO GS-LINDET(68: 1)
           MOVE PARC-PAGO-WK      TO GS-LINDET(69: 2)
           ADD PARC-PAGO-WK       TO PARC-PAGO-TOT
           MOVE ")"               TO GS-LINDET(71: 2)
           MOVE VLR-ATRASADO-WK   TO VALOR-E
           ADD VLR-ATRASADO-WK    TO VLR-ATRASADO-TOT
           MOVE VALOR-E           TO GS-LINDET(73: 13)
           MOVE "("               TO GS-LINDET(86: 1)
           MOVE PARC-ATRASADO-WK  TO GS-LINDET(87: 2)
           ADD PARC-ATRASADO-WK   TO PARC-ATRASADO-TOT
           MOVE ")"               TO GS-LINDET(89: 2)
           MOVE VLR-RESTANTE-WK   TO VALOR-E
           ADD VLR-RESTANTE-WK    TO VLR-RESTANTE-TOT
           MOVE VALOR-E           TO GS-LINDET(91: 13)
           MOVE "("               TO GS-LINDET(104: 1)
           MOVE PARC-RESTANTE-WK  TO GS-LINDET(105: 2)
           ADD PARC-RESTANTE-WK   TO PARC-RESTANTE-TOT
           MOVE ")"               TO GS-LINDET(107: 2).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       TOTALIZA SECTION.
           COMPUTE VLR-GERAL-TOT = VLR-PAGO-TOT + VLR-ATRASADO-TOT +
                                   VLR-RESTANTE-TOT.
           MOVE VLR-GERAL-TOT     TO VALOR-E
           MOVE VALOR-E           TO GS-TOTAL-GERAL(1: 13)
           MOVE "("               TO GS-TOTAL-GERAL(14: 1)
           MOVE PARC-GERAL-TOT    TO GS-TOTAL-GERAL(15: 4)
           MOVE ")"               TO GS-TOTAL-GERAL(19: 1)
           MOVE VLR-PAGO-TOT      TO VALOR-E
           MOVE VALOR-E           TO GS-TOTAL-PAGO(1: 13)
           MOVE "("               TO GS-TOTAL-PAGO(14: 1)
           MOVE PARC-PAGO-TOT     TO GS-TOTAL-PAGO(15: 4)
           MOVE ")"               TO GS-TOTAL-PAGO(19: 1)
           MOVE VLR-ATRASADO-TOT  TO VALOR-E
           MOVE VALOR-E           TO GS-TOTAL-ATRASADO(1: 13)
           MOVE "("               TO GS-TOTAL-ATRASADO(14: 1)
           MOVE PARC-ATRASADO-TOT TO GS-TOTAL-ATRASADO(15: 4)
           MOVE ")"               TO GS-TOTAL-ATRASADO(19: 1)
           MOVE VLR-RESTANTE-TOT  TO VALOR-E
           MOVE VALOR-E           TO GS-TOTAL-RESTANTE(1: 13)
           MOVE "("               TO GS-TOTAL-RESTANTE(14: 1)
           MOVE PARC-RESTANTE-TOT TO GS-TOTAL-RESTANTE(15: 4)
           MOVE ")"               TO GS-TOTAL-RESTANTE(19: 1).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "OEP050" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   IF GS-ORDEM-RADIO = 2 AND VLR-ATRASADO-WK = ZEROS
                      CONTINUE
                   ELSE
                     PERFORM MOVER-DADOS-RELATORIO
                   END-IF
              END-READ
           END-PERFORM.
           MOVE GS-TOTAL-GERAL    TO TOTAL-GERAL-REL.
           MOVE GS-TOTAL-PAGO     TO TOTAL-PAGO-REL.
           MOVE GS-TOTAL-ATRASADO TO TOTAL-ATRASADO-REL.
           MOVE GS-TOTAL-RESTANTE TO TOTAL-RESTANTE-REL.
           WRITE REG-RELAT FROM LINTOT AFTER 2.
           WRITE REG-RELAT FROM LINTOT1.

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL.
           MOVE CLIENTE-WK(5: 4)  TO LINDET-REL(1: 5)
           MOVE NOME-CLIEN-WK     TO LINDET-REL(06: 31)
           MOVE VLR-TOTAL-WK      TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(37: 13)
           MOVE "("               TO LINDET-REL(50: 1)
           MOVE PARC-TOTAL-WK     TO LINDET-REL(51: 2)
           MOVE ")"               TO LINDET-REL(53: 2)
           MOVE VLR-PAGO-WK       TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(55: 13)
           MOVE "("               TO LINDET-REL(68: 1)
           MOVE PARC-PAGO-WK      TO LINDET-REL(69: 2)
           MOVE ")"               TO LINDET-REL(71: 2)
           MOVE VLR-ATRASADO-WK   TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(73: 13)
           MOVE "("               TO LINDET-REL(86: 1)
           MOVE PARC-ATRASADO-WK  TO LINDET-REL(87: 2)
           MOVE ")"               TO LINDET-REL(89: 2)
           MOVE VLR-RESTANTE-WK   TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(91: 13)
           MOVE "("               TO LINDET-REL(104: 1)
           MOVE PARC-RESTANTE-WK  TO LINDET-REL(105: 2)
           MOVE ")"               TO LINDET-REL(107: 2).

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE ZEROS TO TOTAL-W.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           MOVE GS-CONTRATO TO CONTRATO-REL.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           IF GS-ORDEM-RADIO = 1 MOVE "GERAL" TO TIPO-REL
           ELSE MOVE "ATRASADO"  TO TIPO-REL.
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
           CLOSE CRD020 CGD010 WORK CGD020.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

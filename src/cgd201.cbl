       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CGD201.
      *DATA: 28/10/2009
      *AUTOR: ALFREDO SAVIOLLI NETO
      *PROGRAMA: EXTRATO FINANCEIRO CONSOLIDADO
      *FUNÇÃO: Listar todos os lançamentos dentro do intervalo de
      *        vencto selecionado e do fornecedor solicitado,
      *        informando saldo anterior, vencidos e a vencer.

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CCPX001.
           COPY CPPX020.
           COPY CRPX020.
           COPY CAPX004.
           COPY LOGCCD.SEL.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS ALT-WK = TIPO-WK VENCTO-WK
                                WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CCPW001.
       COPY CPPW020.
       COPY CRPW020.
       COPY CAPW004.
       COPY LOGCCD.FD.

       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK              PIC 9(6).
           05  TIPO-WK             PIC 9.
      *    TIPO = 1 - VENCIDOS     2- A VENCER
           05  DATA-MOVTO-WK       PIC 9(8).
           05  RESPONSAVEL-WK      PIC X(5).
           05  HISTORICO-WK        PIC X(30).
           05  VENCTO-WK           PIC 9(8).
           05  VLR-CRED-INI-WK     PIC 9(8)V99.
           05  VLR-DEB-INI-WK      PIC S9(8)V99.
           05  DIAMES-LIQ-WK       PIC 9(4).
           05  VLR-CRED-TX-WK      PIC 9(08)V99.
           05  VLR-DEB-TX-WK       PIC S9(8)V99.
           05  FORNEC-WK           PIC 9(09).
           05  SEQUEN-WK           PIC 9(05).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(155).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CGD201.CPB".
           COPY "CGD201.CPY".
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
           05  ST-CCD001             PIC XX       VALUE SPACES.
           05  ST-CPD020             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-LOGCCD             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
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
               10  ANO-II            PIC 9999.
               10  MES-II            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  MESANOINI.
               10 ANOINI             PIC 9(04).
               10 MESINI             PIC 9(02).
           05  MESANOFIN.
               10 ANOFIN             PIC 9(04).
               10 MESFIN             PIC 9(02).
           05  DIAINI                PIC 9(02).
           05  DIAFIN                PIC 9(02).
           05  NUM-DIAS              PIC 9(02).
           05  QTDE-TAXA             PIC 9(04).
           05  IND                   PIC 9(04).
           05  TOTAL-TAXA            PIC 9(04)V999.
           05  AUX-VALOR             PIC S9(09)V99.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  GRAVA1-REGRAVA2       PIC 9        VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VENCTO-INI            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-FIM            PIC 9(8)     VALUE ZEROS.
           05  DATA-BASE             PIC 9(8)     VALUE ZEROS.
           05  SEQ-W                 PIC 9(6)     VALUE ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ.
           05  VALOR1                PIC 9(08)V99.
           05  VALOR2                PIC s9(08)V99.
           05  VALOR-E1              PIC ZZZZZ.ZZZ,ZZ-.
           05  DIA-MES-E             PIC 99/99 BLANK WHEN ZEROS.
           05  SALDO-WI              PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-WT              PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-GI              PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-GT              PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-ANT             PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-ANT-T           PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-E               PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  TIPO-ANT              PIC 9        VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           05  VALOR-AUX             PIC 9(09)V99 VALUE ZEROS.
           05  DS-DIAS               pic 9(05)    value zeros.
           05  CONTADOR              pic 9(05)    value zeros.
           05  VALORE-W              PIC 9(8)V99  VALUE ZEROS.
      *   VALORE-W - VARIAVEL P/IDENTIFICAR VALOR DE ENTRADA
           05  VALORS-W              PIC 9(8)V99  VALUE ZEROS.
           05  AUX-SITUACAO          PIC 9(01)    VALUE ZEROS.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(55)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(57)   VALUE
           "EXTRATO DE CONTAS CORRENTES ".
           05  FILLER              PIC X(28)   VALUE SPACES.
      *    05  FILLER              PIC X(06)   VALUE "TIPO: ".
      *    05  TIPO-REL            PIC X(19)   VALUE SPACES.
       01  CAB02A.
           05  FILLER              PIC X(12)    VALUE "FORNECEDOR: ".
           05  NOME-REL            PIC X(50)   VALUE SPACES.
           05  FILLER              PIC X(01).
           05  FILLER              PIC X(11)   VALUE "INT.VECTO: ".
           05  VENCTO-INI-REL      PIC 99/99/9999.
           05  FILLER              PIC X(05) VALUE " ATE ".
           05  VENCTO-FIM-REL      PIC 99/99/9999.
       01  CAB02B.
           05  FILLER              PIC X(6)    VALUE "TAXA: ".
           05  TAXA-REL            PIC Z9,99.
           05  FILLER              PIC X(03)   VALUE "%".
           05  FILLER              PIC X(11)   VALUE "DATA BASE: ".
           05  BASE-REL            PIC 99/99/9999.

       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  CAB031.
           05  FILLER              PIC X(137)  VALUE ALL "=".
       01  CAB03A.
           05  FILLER              PIC X(110)  VALUE ALL "-".
       01  CAB03A1.
           05  FILLER              PIC X(137)  VALUE ALL "-".
       01  CAB04.
           05  FILLER              PIC X(110)  VALUE
           "DATA-MOVTO RESP. HISTORICO         VENCIMENTO VALOR-CREDITO
      -    " VALOR-DEBITO   SALDO-ATUAL  LIQUI".

       01  CAB041.
           05  FILLER              PIC X(146)  VALUE
           "DATA-MOVTO RESP. HISTORICO         VENCIMENTO VALOR-CREDITO
      -    " VALOR-DEBITO   SALDO-ATUAL  VALOR-CREDITO  VALOR-DEBITO   S
      -    "ALDO-ATUAL  LIQUI".

       01  DET-LOG.
           05 DET-USUARIO          PIC X(05).
           05 FILLER               PIC X(01).
           05 DET-DIA              PIC 99/.
           05 DET-MES              PIC 99/.
           05 DET-ANO              PIC 9999.
           05 FILLER               PIC X(01).
           05 DET-HORA             PIC 99.
           05 FILLER               PIC X(01) VALUE ":".
           05 DET-MINU             PIC 99.
           05 FILLER               PIC X(01).
           05 DET-PROGRAMA         PIC X(07).

       01  LINDET.
           05  LINDET-REL          PIC X(150)  VALUE SPACES.

           copy   "ldifdias".

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
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CPD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPD020.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "LOGCCD"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LOGCCD.

           OPEN I-O   LOGCCD
           CLOSE      LOGCCD
           OPEN INPUT LOGCCD

           OPEN INPUT CGD001 CCD001 CAD004 CPD020 CRD020

           ACCEPT VARIA-W FROM TIME

           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CCD001 <> "00"
              MOVE "ERRO ABERTURA CCD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPD020 <> "00"
              MOVE "ERRO ABERTURA CPD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CPD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LOGCCD <> "00"
              MOVE "ERRO ABERTURA LOGCCD: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOGCCD TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           MOVE "SENHA32"     TO PROGRAMA-CA004

           READ CAD004 INVALID KEY
                MOVE "N" TO GS-LIBERA
           NOT INVALID KEY
                MOVE "S" TO GS-LIBERA
           END-READ

           CLOSE CAD004

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
               WHEN GS-GERAR-RELATORIO-TRUE
                    PERFORM GERAR-RELATORIO
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-LER-FORNEC-TRUE
                    PERFORM LER-FORNECEDOR
               WHEN GS-POPUP-FORNEC-TRUE
                    PERFORM POPUP-FORNECEDOR
               WHEN GS-VERIFICA-ACESSO-TRUE
                    PERFORM VERIFICA-ACESSO-USUARIO
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
       VERIFICA-ACESSO-USUARIO SECTION.
           MOVE 0              TO GS-LIBERA-ACESSO
           MOVE 0              TO TIPO-ACESSO-CC01.
           MOVE GS-CODIGO      TO CODIGO-CG01.
           READ CGD001 INVALID KEY
                INITIALIZE REG-CGD001.

           IF T-PESFIS-CG01 = 1
              MOVE 1 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-PESJUR-CG01 = 1
              MOVE 2 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-FUNC-CG01 = 1
              MOVE 3 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-REPRES-CG01 = 1
              MOVE 4 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-FOTOG-CG01 = 1
              MOVE 5 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-CINEG-CG01 = 1
              MOVE 6 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-VEND-CG01 = 1
              MOVE 7 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-IMPOSTO-CG01 = 1
              MOVE 8 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-INVESTIDOR-CG01 = 1
              MOVE 9 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF OUTRO3-CG01= 1
              MOVE 0 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-TERCEIRIZADO-CG01 = 1
              MOVE 10 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

           IF T-FRANQUIA-CG01 = 1
              MOVE 11 TO TIPO-ACESSO-CC01
              PERFORM LER-USUARIO.

       LER-USUARIO SECTION.
           MOVE USUARIO-W          TO NOME-REDUZ-CC01.
           IF GS-LIBERA-ACESSO = 0
              READ CCD001 INVALID KEY
                   MOVE 0  TO GS-LIBERA-ACESSO
              NOT INVALID KEY
                   MOVE 1 TO GS-LIBERA-ACESSO.

       POPUP-FORNECEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CGP001T".
           MOVE PASSAR-STRING-1(33: 6) TO GS-CODIGO.
           MOVE PASSAR-STRING-1(1: 30) TO GS-NOME.
       LER-FORNECEDOR SECTION.
           MOVE GS-CODIGO        TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE "*********" TO NOME-CG01.

           MOVE NOME-CG01        TO GS-NOME.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       GERAR-RELATORIO SECTION.
           MOVE FUNCTION NUMVAL(GS-ACP-SITUACAO(1:1)) TO AUX-SITUACAO

           MOVE GS-DATA-BASE TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV     TO DATA-BASE

           CLOSE       WORK
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-VENCTO-INI TO DATA-INV
           CALL "GRIDAT2"  USING DATA-INV
           MOVE DATA-INV      TO VENCTO-INI

           MOVE GS-VENCTO-FIM TO DATA-INV
           CALL "GRIDAT2"  USING DATA-INV
           MOVE DATA-INV      TO VENCTO-FIM
           MOVE ZEROS         TO SEQ-W

           PERFORM LER-SALDO-ANT

           INITIALIZE REG-CRD020
           MOVE 9                           TO CLASS-CLIENTE-CR20
           MOVE GS-CODIGO                   TO CLIENTE-CR20
           MOVE VENCTO-INI                  TO DATA-VENCTO-CR20
           START CRD020 KEY IS NOT LESS ALT1-CR20 INVALID KEY
                MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
                READ CRD020 NEXT AT END
                     MOVE "10" TO ST-CRD020
                NOT AT END
                     IF CLASS-CLIENTE-CR20 <> 9 OR
                        CLIENTE-CR20       <> GS-CODIGO
                        MOVE "10" TO ST-CRD020
                     ELSE
                        IF DATA-VENCTO-CR20 > VENCTO-FIM
                           MOVE "10" TO ST-CRD020
                        ELSE
                        IF (AUX-SITUACAO = 0 and
                            SITUACAO-CR20 = 0 OR 1) OR
                            AUX-SITUACAO = SITUACAO-CR20
                           ADD 1                TO SEQ-W
                           MOVE SEQ-W           TO GS-EXIBE-CODIGO
                                                   SEQ-WK
                           MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                           IF DATA-VENCTO-CR20 < DATA-DIA-I
                              MOVE 1 TO TIPO-WK
                           ELSE
                              MOVE 2 TO TIPO-WK
                           END-IF

                           MOVE DATA-MOVTO-CR20      TO DATA-INV
                           CALL "GRIDAT1" USING DATA-INV
                           MOVE DATA-INV             TO DATA-MOVTO-WK
                           MOVE RESPONSAVEL-CR20     TO RESPONSAVEL-WK
                           MOVE DESCRICAO-CR20       TO HISTORICO-WK
                           MOVE DATA-VENCTO-CR20     TO VENCTO-WK

                           MOVE COD-COMPL-CR20       TO FORNEC-WK
                           MOVE SEQ-CR20             TO SEQUEN-WK

                           MOVE VALOR-SALDO-CR20     TO AUX-VALOR

                           IF DATA-BASE > 0
                              IF DATA-VENCTO-CR20 = DATA-BASE
                                 MOVE VALOR-SALDO-CR20 TO VLR-CRED-TX-WK
                                 MOVE ZEROS            TO VLR-DEB-TX-WK
                              ELSE
                                 IF DATA-VENCTO-CR20 > DATA-BASE
                                    CALL   "UTIDIAS" USING DATA-BASE
                                                        DATA-VENCTO-CR20
                                                        DS-DIAS
                                    CANCEL "UTIDIAS"

                                    MOVE VALOR-SALDO-CR20 TO AUX-VALOR

                                    PERFORM UNTIL DS-DIAS < 30
                                         COMPUTE DS-DIAS = DS-DIAS - 30
                                         COMPUTE AUX-VALOR = AUX-VALOR +
                                             (AUX-VALOR * GS-TAXA / 100)
                                    END-PERFORM

                                    COMPUTE VALOR-AUX = AUX-VALOR *
                                                        GS-TAXA / 100

                                    COMPUTE VALOR-AUX = VALOR-AUX / 30

                                    COMPUTE VALOR-AUX = VALOR-AUX *
                                                        DS-DIAS

                                    ADD VALOR-AUX      TO AUX-VALOR

                                    MOVE AUX-VALOR     TO VLR-CRED-TX-WK
                                    MOVE ZEROS         TO VLR-DEB-TX-WK

                                 ELSE
                                    CALL   "UTIDIAS" USING
                                                        DATA-VENCTO-CR20
                                                        DATA-BASE
                                                        DS-DIAS
                                    CANCEL "UTIDIAS"

                                    MOVE VALOR-SALDO-CR20 TO AUX-VALOR

                                    PERFORM UNTIL DS-DIAS < 30
                                         COMPUTE DS-DIAS = DS-DIAS - 30
                                         COMPUTE AUX-VALOR = AUX-VALOR +
                                             (AUX-VALOR * GS-TAXA / 100)
                                    END-PERFORM

                                    COMPUTE VALOR-AUX = AUX-VALOR *
                                                        GS-TAXA / 100

                                    COMPUTE VALOR-AUX = VALOR-AUX / 30

                                    COMPUTE VALOR-AUX = VALOR-AUX *
                                                        DS-DIAS

                                    ADD VALOR-AUX     TO AUX-VALOR

                                    MOVE AUX-VALOR    TO VLR-CRED-TX-WK
                                    MOVE ZEROS        TO VLR-DEB-TX-WK
                                 END-IF
                              END-IF
                           ELSE
                             MOVE ZEROS             TO VLR-CRED-TX-WK
                                                       VLR-DEB-TX-WK
                           END-IF

                           MOVE VALOR-SALDO-CR20     TO VLR-CRED-INI-WK
                           MOVE ZEROS                TO VLR-DEB-INI-WK

                           MOVE DATA-RCTO-CR20(5: 2) TO
                                DIAMES-LIQ-WK(3:2)
                           MOVE DATA-RCTO-CR20(7: 2) TO
                                DIAMES-LIQ-WK(1:2)

                           WRITE REG-WORK
                           END-WRITE
                        END-IF
                        END-IF
                     END-IF
                END-READ
           END-PERFORM


           INITIALIZE REG-CRD020
           MOVE 0                           TO CLASS-CLIENTE-CR20
           MOVE GS-CODIGO                   TO CLIENTE-CR20
           MOVE VENCTO-INI                  TO DATA-VENCTO-CR20
           START CRD020 KEY IS NOT LESS ALT1-CR20 INVALID KEY
                MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
                READ CRD020 NEXT AT END
                     MOVE "10" TO ST-CRD020
                NOT AT END
                     IF CLASS-CLIENTE-CR20 <> 0 OR
                        CLIENTE-CR20       <> GS-CODIGO
                        MOVE "10" TO ST-CRD020
                     ELSE
                        IF DATA-VENCTO-CR20 > VENCTO-FIM
                           MOVE "10" TO ST-CRD020
                        ELSE
                        IF (AUX-SITUACAO = 0 and
                            SITUACAO-CR20 = 0 OR 1) OR
                            AUX-SITUACAO = SITUACAO-CR20
                           ADD 1                TO SEQ-W
                           MOVE SEQ-W           TO GS-EXIBE-CODIGO
                                                   SEQ-WK
                           MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                           IF DATA-VENCTO-CR20 < DATA-DIA-I
                              MOVE 1 TO TIPO-WK
                           ELSE
                              MOVE 2 TO TIPO-WK
                           END-IF

                           MOVE DATA-MOVTO-CR20      TO DATA-INV
                           CALL "GRIDAT1" USING DATA-INV
                           MOVE DATA-INV             TO DATA-MOVTO-WK
                           MOVE RESPONSAVEL-CR20     TO RESPONSAVEL-WK
                           MOVE DESCRICAO-CR20       TO HISTORICO-WK
                           MOVE DATA-VENCTO-CR20     TO VENCTO-WK

                           MOVE COD-COMPL-CR20       TO FORNEC-WK
                           MOVE SEQ-CR20             TO SEQUEN-WK

                           MOVE VALOR-SALDO-CR20     TO AUX-VALOR

                           IF DATA-BASE > 0
                              IF DATA-VENCTO-CR20 = DATA-BASE
                                 MOVE VALOR-SALDO-CR20 TO VLR-CRED-TX-WK
                                 MOVE ZEROS            TO VLR-DEB-TX-WK
                              ELSE
                                 IF DATA-VENCTO-CR20 > DATA-BASE
                                    CALL   "UTIDIAS" USING DATA-BASE
                                                        DATA-VENCTO-CR20
                                                        DS-DIAS
                                    CANCEL "UTIDIAS"

                                    MOVE VALOR-SALDO-CR20 TO AUX-VALOR

                                    PERFORM UNTIL DS-DIAS < 30
                                         COMPUTE DS-DIAS = DS-DIAS - 30
                                         COMPUTE AUX-VALOR = AUX-VALOR +
                                             (AUX-VALOR * GS-TAXA / 100)
                                    END-PERFORM

                                    COMPUTE VALOR-AUX = AUX-VALOR *
                                                        GS-TAXA / 100

                                    COMPUTE VALOR-AUX = VALOR-AUX / 30

                                    COMPUTE VALOR-AUX = VALOR-AUX *
                                                        DS-DIAS

                                    ADD VALOR-AUX      TO AUX-VALOR

                                    MOVE AUX-VALOR     TO VLR-CRED-TX-WK
                                    MOVE ZEROS         TO VLR-DEB-TX-WK

                                 ELSE
                                    CALL   "UTIDIAS" USING
                                                        DATA-VENCTO-CR20
                                                        DATA-BASE
                                                        DS-DIAS
                                    CANCEL "UTIDIAS"

                                    MOVE VALOR-SALDO-CR20 TO AUX-VALOR

                                    PERFORM UNTIL DS-DIAS < 30
                                         COMPUTE DS-DIAS = DS-DIAS - 30
                                         COMPUTE AUX-VALOR = AUX-VALOR +
                                             (AUX-VALOR * GS-TAXA / 100)
                                    END-PERFORM

                                    COMPUTE VALOR-AUX = AUX-VALOR *
                                                        GS-TAXA / 100

                                    COMPUTE VALOR-AUX = VALOR-AUX / 30

                                    COMPUTE VALOR-AUX = VALOR-AUX *
                                                        DS-DIAS

                                    ADD VALOR-AUX     TO AUX-VALOR

                                    MOVE AUX-VALOR    TO VLR-CRED-TX-WK
                                    MOVE ZEROS        TO VLR-DEB-TX-WK
                                 END-IF
                              END-IF
                           ELSE
                             MOVE ZEROS             TO VLR-CRED-TX-WK
                                                       VLR-DEB-TX-WK
                           END-IF

                           MOVE VALOR-SALDO-CR20     TO VLR-CRED-INI-WK
                           MOVE ZEROS                TO VLR-DEB-INI-WK

                           MOVE DATA-RCTO-CR20(5: 2) TO
                                DIAMES-LIQ-WK(3:2)
                           MOVE DATA-RCTO-CR20(7: 2) TO
                                DIAMES-LIQ-WK(1:2)

                           WRITE REG-WORK
                           END-WRITE
                        END-IF
                        END-IF
                     END-IF
                END-READ
           END-PERFORM

           INITIALIZE REG-CPD020
           MOVE GS-CODIGO                   TO FORNEC-CP20
           MOVE VENCTO-INI                  TO DATA-VENCTO-CP20
           START CPD020 KEY IS NOT LESS ALT1-CP20 INVALID KEY
                MOVE "10" TO ST-CPD020.

           PERFORM UNTIL ST-CPD020 = "10"
                READ CPD020 NEXT AT END
                     MOVE "10" TO ST-CPD020
                NOT AT END
                     IF GS-CODIGO <> FORNEC-CP20
                        MOVE "10" TO ST-CPD020
                     ELSE
                        IF DATA-VENCTO-CP20 > VENCTO-FIM
                           MOVE "10" TO ST-CPD020
                        ELSE
                        IF SITUACAO-CP20 = AUX-SITUACAO
                           ADD 1                TO SEQ-W
                           MOVE SEQ-W           TO GS-EXIBE-CODIGO
                                                   SEQ-WK
                           MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                           IF DATA-VENCTO-CP20 < DATA-DIA-I
                              MOVE 1 TO TIPO-WK
                           ELSE
                              MOVE 2 TO TIPO-WK
                           END-IF

                           MOVE DATA-MOVTO-CP20      TO DATA-INV
                           CALL "GRIDAT1" USING DATA-INV
                           MOVE DATA-INV             TO DATA-MOVTO-WK
                           MOVE RESPONSAVEL-CP20     TO RESPONSAVEL-WK
                           MOVE DESCRICAO-CP20       TO HISTORICO-WK
                           MOVE DATA-VENCTO-CP20     TO VENCTO-WK

                           MOVE FORNEC-CP20          TO FORNEC-WK
                           MOVE SEQ-CP20             TO SEQUEN-WK

                           MOVE VALOR-TOT-CP20       TO AUX-VALOR

                           IF DATA-BASE > 0
                              IF DATA-VENCTO-CP20 = DATA-BASE
                                 COMPUTE VLR-DEB-TX-WK = VALOR-TOT-CP20
                                 * -1
                                 MOVE ZEROS          TO VLR-CRED-TX-WK
                              ELSE
                                 IF DATA-VENCTO-CP20 > DATA-BASE
                                    CALL   "UTIDIAS" USING DATA-BASE
                                                        DATA-VENCTO-CP20
                                                        DS-DIAS
                                    CANCEL "UTIDIAS"

                                    MOVE VALOR-TOT-CP20 TO AUX-VALOR

                                    PERFORM UNTIL DS-DIAS < 30
                                         COMPUTE DS-DIAS = DS-DIAS - 30
                                         COMPUTE AUX-VALOR = AUX-VALOR +
                                             (AUX-VALOR * GS-TAXA / 100)
                                    END-PERFORM

                                    COMPUTE VALOR-AUX = AUX-VALOR *
                                                        GS-TAXA / 100

                                    COMPUTE VALOR-AUX = VALOR-AUX / 30

                                    COMPUTE VALOR-AUX = VALOR-AUX *
                                                        DS-DIAS

                                    ADD VALOR-AUX    TO AUX-VALOR

                                    COMPUTE VLR-DEB-TX-WK = AUX-VALOR
                                                            * -1
                                    MOVE ZEROS       TO VLR-CRED-TX-WK

                                 ELSE
                                    CALL   "UTIDIAS" USING
                                                        DATA-VENCTO-CP20
                                                        DATA-BASE
                                                        DS-DIAS
                                    CANCEL "UTIDIAS"

                                    MOVE VALOR-TOT-CP20 TO AUX-VALOR

                                    PERFORM UNTIL DS-DIAS < 30
                                         COMPUTE DS-DIAS = DS-DIAS - 30
                                         COMPUTE AUX-VALOR = AUX-VALOR +
                                             (AUX-VALOR * GS-TAXA / 100)
                                    END-PERFORM

                                    COMPUTE VALOR-AUX = AUX-VALOR *
                                                        GS-TAXA / 100

                                    COMPUTE VALOR-AUX = VALOR-AUX / 30

                                    COMPUTE VALOR-AUX = VALOR-AUX *
                                                        DS-DIAS

                                    ADD VALOR-AUX     TO AUX-VALOR

                                    COMPUTE VLR-DEB-TX-WK = AUX-VALOR
                                                            * -1
                                    MOVE ZEROS        TO VLR-CRED-TX-WK
                                 END-IF
                              END-IF
                           ELSE
                              MOVE ZEROS             TO VLR-CRED-TX-WK
                                                        VLR-DEB-TX-WK
                           END-IF

                           COMPUTE VLR-DEB-INI-WK = VALOR-TOT-CP20 * -1
                           MOVE ZEROS                TO VLR-CRED-INI-WK

                           MOVE DATA-PGTO-CP20(5: 2) TO
                                DIAMES-LIQ-WK(3:2)
                           MOVE DATA-PGTO-CP20(7: 2) TO
                                DIAMES-LIQ-WK(1:2)

                           WRITE REG-WORK
                           END-WRITE
                        END-IF
                     END-IF
                     END-IF
                END-READ
           END-PERFORM


           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.


       LER-SALDO-ANT SECTION.
           MOVE VENCTO-INI(1: 6) TO MESANO-I.
           IF MES-II = 01
              MOVE 12 TO MES-II
              SUBTRACT 1 FROM ANO-II
           ELSE
              SUBTRACT 1 FROM MES-II.


           INITIALIZE REG-CRD020
                      SALDO-ANT
                      SALDO-ANT-T
           MOVE 9                           TO CLASS-CLIENTE-CR20
           MOVE GS-CODIGO                   TO CLIENTE-CR20
           START CRD020 KEY IS NOT LESS ALT1-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      IF CLASS-CLIENTE-CR20   <> 9             OR
                         GS-CODIGO            <> CLIENTE-CR20  OR
                         DATA-VENCTO-CR20 NOT < VENCTO-INI
                         MOVE "10" TO ST-CRD020
                      ELSE
                         ADD 1                TO SEQ-W
                         MOVE SEQ-W           TO GS-EXIBE-CODIGO
                         MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                         IF SITUACAO-CP20 = 0 OR 1
                            ADD VALOR-SALDO-CR20 TO SALDO-ANT
                            IF DATA-BASE > 0
                               CALL "UTIDIAS" USING DATA-VENCTO-CR20
                                                    DATA-BASE DS-DIAS
                               CANCEL "UTIDIAS"

                               MOVE VALOR-SALDO-CR20 TO AUX-VALOR

                               PERFORM UNTIL DS-DIAS < 30
                                    COMPUTE DS-DIAS = DS-DIAS - 30
                                    COMPUTE AUX-VALOR = AUX-VALOR +
                                           (AUX-VALOR * GS-TAXA / 100)
                               END-PERFORM

                               COMPUTE VALOR-AUX = AUX-VALOR *
                                                   GS-TAXA / 100

                               COMPUTE VALOR-AUX = VALOR-AUX / 30

                               COMPUTE VALOR-AUX = VALOR-AUX * DS-DIAS

                               ADD VALOR-AUX     TO AUX-VALOR

                               ADD AUX-VALOR     TO SALDO-ANT-T

                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM

           INITIALIZE REG-CPD020
           MOVE GS-CODIGO TO FORNEC-CP20
           START CPD020 KEY IS NOT LESS ALT1-CP20 INVALID KEY
                 MOVE "10" TO ST-CPD020.

           PERFORM UNTIL ST-CPD020 = "10"
                 READ CPD020 NEXT AT END
                      MOVE "10" TO ST-CPD020
                 NOT AT END
                      IF GS-CODIGO <> FORNEC-CP20 OR
                         DATA-VENCTO-CP20 NOT < VENCTO-INI
                         MOVE "10" TO ST-CPD020
                      ELSE
                         ADD 1                TO SEQ-W
                         MOVE SEQ-W           TO GS-EXIBE-CODIGO
                         MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                         IF SITUACAO-CP20 = 0
                            COMPUTE SALDO-ANT = SALDO-ANT -
                                                VALOR-TOT-CP20
                            IF DATA-BASE > 0
                               CALL "UTIDIAS" USING DATA-VENCTO-CP20
                                                    DATA-BASE DS-DIAS
                               CANCEL "UTIDIAS"

                               MOVE VALOR-TOT-CP20 TO AUX-VALOR

                               PERFORM UNTIL DS-DIAS < 30
                                    COMPUTE DS-DIAS = DS-DIAS - 30
                                    COMPUTE AUX-VALOR = AUX-VALOR +
                                           (AUX-VALOR * GS-TAXA / 100)
                               END-PERFORM

                               COMPUTE VALOR-AUX = AUX-VALOR *
                                                   GS-TAXA / 100

                               COMPUTE VALOR-AUX = VALOR-AUX / 30

                               COMPUTE VALOR-AUX = VALOR-AUX * DS-DIAS

                               ADD VALOR-AUX     TO AUX-VALOR

                               COMPUTE SALDO-ANT-T = SALDO-ANT-T -
                                                     AUX-VALOR
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.

       CARREGA-LISTA SECTION.
           MOVE ZEROS TO SALDO-WT SALDO-GT
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ZEROS TO TIPO-WK VENCTO-WK VALOR1 VALOR2.
           START WORK KEY IS NOT < ALT-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           MOVE 3 TO TIPO-ANT

           MOVE SALDO-ANT     TO SALDO-GI SALDO-E
           MOVE "SALDO ANTERIOR..." TO GS-LINDET(1: 25)
           MOVE SALDO-E       TO GS-LINDET(85: 14)
           MOVE SALDO-ANT-T   TO SALDO-GT SALDO-E
           MOVE SALDO-E       TO GS-LINDET(130: 14)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           move zeros to saldo-wi saldo-wt

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   IF TIPO-WK NOT = TIPO-ANT
                      PERFORM LINHA-BRANCO
                      EVALUATE TIPO-WK
      *                  WHEN 0 MOVE "LIQUIDADOS"  TO GS-LINDET
                         WHEN 1 MOVE "VENCIDOS  "  TO GS-LINDET
                         WHEN 2 MOVE "À VENCER  "  TO GS-LINDET
                      END-EVALUATE
                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      PERFORM LINHA-BRANCO
                      MOVE TIPO-WK     TO TIPO-ANT
                      IF SALDO-WI = 0
                         MOVE SALDO-ANT   TO SALDO-WI
                      END-IF
                      IF SALDO-WT = 0
                         MOVE SALDO-ANT-T TO SALDO-WT
                      END-IF

                      IF DATA-BASE > 0
                         MOVE ALL "-" TO GS-LINDET(1:150)
                      ELSE
                         MOVE ALL "-" TO GS-LINDET(1:110)
                      END-IF

                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      MOVE SPACES TO GS-LINDET


                      MOVE "DATA-MOVTO RESP. HISTORICO" TO GS-LINDET
                      MOVE "VENCIMENTO  VLR-CREDITO   VLR-DEBITO   SALDO
      -               " ATUAL" TO GS-LINDET(49:50)
                      IF DATA-BASE > 0
                         MOVE "VLR-CREDITO   VLR-DEBITO   SALDO ATUAL"
                         TO GS-LINDET(105:38)
                         MOVE "LIQUID" TO GS-LINDET(145:5)
                      ELSE
                         MOVE "LIQUID" TO GS-LINDET(105:5)
                      END-IF

                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      IF DATA-BASE > 0
                         MOVE ALL "-" TO GS-LINDET(1:150)
                      ELSE
                         MOVE ALL "-" TO GS-LINDET(1:110)
                      END-IF
                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      MOVE SPACES TO GS-LINDET
                   END-IF
                   MOVE SPACES            TO GS-LINDET
                   MOVE DATA-MOVTO-WK     TO DATA-E
                   MOVE DATA-E            TO GS-LINDET(1: 11)
                   MOVE RESPONSAVEL-WK    TO GS-LINDET(12: 6)
                   MOVE HISTORICO-WK      TO GS-LINDET(18: 31)
                   MOVE VENCTO-WK         TO DATA-INV
                   CALL "GRIDAT1" USING DATA-INV
                   MOVE DATA-INV          TO DATA-E
                   MOVE DATA-E            TO GS-LINDET(49: 11)
                   MOVE VLR-CRED-INI-WK   TO VALOR-E
                   ADD  VLR-CRED-INI-WK   TO VALOR1
                   MOVE VALOR-E           TO GS-LINDET(59: 14)
                   MOVE VLR-DEB-INI-WK    TO VALOR-E1
      *            COMPUTE VALOR2 = VALOR2 - VLR-DEB-INI-WK
                   ADD  VLR-DEB-INI-WK    TO VALOR2
                   MOVE VALOR-E1          TO GS-LINDET(72: 14)
                   IF DIAMES-LIQ-WK = ZEROS
                      ADD VLR-CRED-INI-WK TO SALDO-WI SALDO-GI
                      ADD VLR-DEB-INI-WK  TO SALDO-WI SALDO-GI
                   END-IF
                   MOVE SALDO-WI          TO SALDO-E
                   MOVE SALDO-E           TO GS-LINDET(85: 14)

                   MOVE VLR-CRED-TX-WK    TO VALOR-E
                   MOVE VALOR-E           TO GS-LINDET(102:14)
                   MOVE VLR-DEB-TX-WK     TO VALOR-E1
                   MOVE VALOR-E1          TO GS-LINDET(116:14)
                   IF DIAMES-LIQ-WK = ZEROS
                      ADD VLR-CRED-TX-WK  TO SALDO-WT SALDO-GT
                      ADD VLR-DEB-TX-WK   TO SALDO-WT SALDO-GT
                   END-IF
                   MOVE SALDO-WT           TO SALDO-E
                   MOVE SALDO-E            TO GS-LINDET(130: 14)

                   MOVE SEQUEN-WK         TO GS-LINDET(160:5)

                   IF VLR-CRED-TX-WK <> 0 OR VLR-DEB-TX-WK <> 0
                      MOVE DIAMES-LIQ-WK     TO DIA-MES-E
                      MOVE DIA-MES-E         TO GS-LINDET(145: 5)
                   ELSE
                      MOVE DIAMES-LIQ-WK     TO DIA-MES-E
                      MOVE DIA-MES-E         TO GS-LINDET(105: 5)
                   END-IF


                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
           PERFORM LINHA-BRANCO
           MOVE "SOMA...." TO GS-LINDET(1:20)
           MOVE VALOR1     TO VALOR-E
           MOVE VALOR-E    TO GS-LINDET(59:14)
           MOVE VALOR2     TO VALOR-E1
           MOVE VALOR-E1   TO GS-LINDET(72:14)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM LINHA-BRANCO

           MOVE "SALDO..." TO GS-LINDET(1: 20)
           MOVE SALDO-GI   TO SALDO-E
           MOVE SALDO-E    TO GS-LINDET(85: 14)
           MOVE SALDO-GT   TO SALDO-E
           MOVE SALDO-E    TO GS-LINDET(130: 14)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       LINHA-BRANCO SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CGD201" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO SALDO-WT SALDO-GT
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE ZEROS TO VENCTO-WK TIPO-WK VALOR1 VALOR2
                         SALDO-WI SALDO-WT
           MOVE SALDO-ANT TO SALDO-GI SALDO-E
           MOVE SPACES TO LINDET-REL.
           MOVE "SALDO ANTERIOR..." TO LINDET-REL(1: 20)
           MOVE SALDO-E    TO LINDET-REL(75: 14)

           IF GS-TAXA = 0
              MOVE ZEROS      TO SALDO-GT SALDO-E
              MOVE SALDO-E    TO LINDET-REL(118: 14)
           ELSE
              MOVE SALDO-ANT-T  TO SALDO-GT SALDO-E
              MOVE SALDO-E      TO LINDET-REL(118: 14).

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.



           MOVE 3     TO TIPO-ANT
           START WORK KEY IS NOT < ALT-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF TIPO-WK NOT = TIPO-ANT
                   MOVE SPACES TO GS-LINDET
                   EVALUATE TIPO-WK
      *              WHEN 0 MOVE "LIQUIDADOS"  TO LINDET-REL
                     WHEN 1 MOVE "VENCIDOS  "  TO LINDET-REL
                     WHEN 2 MOVE "À VENCER  "  TO LINDET-REL
                   END-EVALUATE
                   WRITE REG-RELAT FROM LINDET AFTER 2
                   END-WRITE
                   ADD 2 TO LIN
                   IF LIN > 56 PERFORM CABECALHO
                   END-IF
                   MOVE TIPO-WK TO TIPO-ANT

                   IF SALDO-WI = 0
                      MOVE SALDO-ANT   TO SALDO-WI
                   END-IF
                   IF SALDO-WT = 0
                      MOVE SALDO-ANT-T TO SALDO-WT
                   END-IF

                END-IF
                MOVE SPACES            TO LINDET-REL
                MOVE DATA-MOVTO-WK     TO DATA-E
                MOVE DATA-E            TO LINDET-REL(1: 11)
                MOVE RESPONSAVEL-WK    TO LINDET-REL(12: 5)
                MOVE HISTORICO-WK      TO LINDET-REL(18: 18)
                MOVE VENCTO-WK         TO DATA-INV
                CALL "GRIDAT1" USING DATA-INV
                MOVE DATA-INV          TO DATA-E
                MOVE DATA-E            TO LINDET-REL(37: 11)
                MOVE VLR-CRED-INI-WK   TO VALOR-E
                ADD  VLR-CRED-INI-WK   TO VALOR1
                MOVE VALOR-E           TO LINDET-REL(48: 14)
                MOVE VLR-DEB-INI-WK    TO VALOR-E1
                ADD  VLR-DEB-INI-WK    TO VALOR2
                MOVE VALOR-E1          TO LINDET-REL(62: 14)
                IF DIAMES-LIQ-WK = ZEROS
                   ADD VLR-CRED-INI-WK TO SALDO-WI SALDO-GI
                   ADD VLR-DEB-INI-WK  TO SALDO-WI SALDO-GI
                END-IF
                MOVE SALDO-WI          TO SALDO-E
                MOVE SALDO-E           TO LINDET-REL(75: 14)

                IF DATA-BASE > 0
                   MOVE VLR-CRED-TX-WK    TO VALOR-E
                   MOVE VALOR-E           TO LINDET-REL(90:14)
                   MOVE VLR-DEB-TX-WK     TO VALOR-E1
                   MOVE VALOR-E1          TO LINDET-REL(104:14)
                   IF DIAMES-LIQ-WK = ZEROS
                      ADD VLR-CRED-TX-WK  TO SALDO-WT SALDO-GT
                      ADD VLR-DEB-TX-WK   TO SALDO-WT SALDO-GT
                   END-IF
                   MOVE SALDO-WT           TO SALDO-E
                   MOVE SALDO-E            TO LINDET-REL(118: 14)

                   MOVE DIAMES-LIQ-WK     TO DIA-MES-E
                   MOVE DIA-MES-E         TO LINDET-REL(132: 5)
                ELSE
                   MOVE DIAMES-LIQ-WK     TO DIA-MES-E
                   MOVE DIA-MES-E         TO LINDET-REL(91: 5)
                END-IF

                WRITE REG-RELAT FROM LINDET-REL
                END-WRITE

                ADD 1 TO LIN
                IF LIN > 56
                   PERFORM CABECALHO
                END-IF

                MOVE FORNEC-WK             TO LOGCCD-FORNEC
                MOVE SEQUEN-WK             TO LOGCCD-SEQ
                READ LOGCCD NOT INVALID KEY
                     MOVE LOGCCD-USUARIO   TO DET-USUARIO
                     MOVE LOGCCD-DIA       TO DET-DIA
                     MOVE LOGCCD-MES       TO DET-MES
                     MOVE LOGCCD-ANO       TO DET-ANO
                     MOVE LOGCCD-HORA(1:2) TO DET-HORA
                     MOVE LOGCCD-HORA(3:2) TO DET-MINU
                     MOVE LOGCCD-PROGRAMA  TO DET-PROGRAMA
                     MOVE DET-LOG          TO LINDET-REL
                     WRITE REG-RELAT FROM LINDET-REL
                     ADD 1 TO LIN
                     IF LIN > 56
                        PERFORM CABECALHO
                     END-IF
                END-READ
              END-READ
           END-PERFORM.

           MOVE SPACES TO LINDET-REL
           WRITE REG-RELAT FROM LINDET-REL

           MOVE "SOMA...." TO LINDET-REL(1:20)
           MOVE VALOR1     TO VALOR-E
           MOVE VALOR-E    TO LINDET-REL(48:14)
           MOVE VALOR2     TO VALOR-E1
           MOVE VALOR-E1   TO LINDET-REL(62:14)

           WRITE REG-RELAT FROM LINDET-REL



           IF DATA-BASE > 0
              WRITE REG-RELAT FROM CAB03A1
              MOVE SPACES TO LINDET-REL
              MOVE "SALDO..." TO LINDET-REL(1: 20)
              MOVE SALDO-GI   TO SALDO-E
              MOVE SALDO-E    TO LINDET-REL(75: 14)
              MOVE SALDO-GT   TO SALDO-E
              MOVE SALDO-E    TO LINDET-REL(118: 14)
              WRITE REG-RELAT FROM LINDET
           ELSE
              WRITE REG-RELAT FROM CAB03A
              MOVE SPACES TO LINDET-REL
              MOVE "SALDO..." TO LINDET-REL(1: 20)
              MOVE SALDO-GI   TO SALDO-E
              MOVE SALDO-E    TO LINDET-REL(75: 14)
              MOVE ZEROS      TO SALDO-E
              MOVE SALDO-E    TO LINDET-REL(118: 14)
              WRITE REG-RELAT FROM LINDET.


           COPY DESCONDENSA.

       CABECALHO SECTION.
      *    MOVE GS-TIPO-LCTO  TO TIPO-REL.
           MOVE SPACES TO NOME-REL
           STRING GS-CODIGO "-" GS-NOME INTO NOME-REL
           MOVE GS-VENCTO-INI TO VENCTO-INI-REL
           MOVE GS-VENCTO-FIM TO VENCTO-FIM-REL
           MOVE GS-TAXA       TO TAXA-REL
           MOVE GS-DATA-BASE  TO BASE-REL
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB02A.
           WRITE REG-RELAT FROM CAB02B.
           IF DATA-BASE > 0
              WRITE REG-RELAT FROM CAB031
              WRITE REG-RELAT FROM CAB041
              WRITE REG-RELAT FROM CAB031
           ELSE
              WRITE REG-RELAT FROM CAB03
              WRITE REG-RELAT FROM CAB04
              WRITE REG-RELAT FROM CAB03
           END-IF
           MOVE 8 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CCD001 CGD001 LOGCCD CRD020 CPD020 WORK
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP051.
      *DATA: 06/07/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: FECHAMENTO FORMATURAS - PREVISÃO DE PRODUÇÃO
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CAPX004.
           COPY CAPX012.
           COPY COPX001.
           COPY COPX002.
           COPY COPX005.
           COPY COPX040.
           COPY COPX050.
           COPY COPX060.
           COPY IEPX010.
           COPY LBPX027.
      *    COPY MTPX021.
           COPY MTPX021D.

           SELECT  MTD021 ASSIGN TO PATH-MTD021
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   LOCK MODE IS MANUAL WITH LOCK ON RECORD
                   STATUS IS ST-MTD021
                   RECORD KEY IS CONTRATO-MT21
                   ALTERNATE RECORD KEY IS CHAVE-PRI-MT21 =
                                           CAMPANHA-MT21
                                           PRIORIDADE-MT21
                                           CONTRATO-MT21.



           COPY MTPX051.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CONTRATO-WK
                  ALTERNATE RECORD KEY IS ALT1-WK = IDENTIFICACAO-WK
                       ULT-DATA-REAL-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-WK = STATUS-WK
                       ULT-DATA-REAL-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-WK = CIDADE-WK
                       ULT-DATA-REAL-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-WK = ULT-DATA-REAL-WK
                       PRIORIDADE-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT5-WK = PRIORIDADE-WK
                       ULT-DATA-REAL-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW004.
       COPY CAPW012.
       COPY COPW001.
       COPY COPW002.
       COPY COPW005.
       COPY COPW040.
       COPY COPW050.
       COPY COPW060.
       COPY IEPW010.
       COPY LBPW027.
      *COPY MTPW021.
      *COPY MTPW021D.

      *ARQUIVO DE PRIORIODADE P/ PRODUÇÃO
       FD  MTD021.
       01  REG-MTD021.
           05  CONTRATO-MT21      PIC 9(4).
           05  PRIORIDADE-MT21    PIC 999.
           05  PRODUTO-MT21       PIC 9(2).
           05  OBS-MT21           PIC X(40).
           05  ORDEM-MT21         PIC 9(4).
           05  CAMPANHA-MT21      PIC 9(2).

      *COPY MTPW021D.

      *ARQUIVO DE PRIORIODADE P/ PRODUÇÃO (ANOTAÇÕES)
       FD  MTD021D.
       01  REG-MTD021D.
           05 CHAVE-MT21D.
              10 CONTRATO-MT21D      PIC 9(04).
              10 DATA-MT21D          PIC 9(08).
              10 SEQUENCIA-MT21D     PIC 9(03).
           05 ANOTACOES-MT21D        PIC X(791).
           05 USUARIO-MT21D          PIC X(05).
           05 HORA-MT21D             PIC 9(04).

       COPY MTPW051.

       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(4).
           05  IDENTIFICACAO-WK    PIC X(15).
           05  STATUS-WK           PIC X(20).
           05  CIDADE-WK           PIC X(13).
           05  MESANO-WK           PIC 9(6).
           05  PADRAO-WK           PIC X.
           05  QT-FORM-WK          PIC 9(4).
           05  QT-FOTOS-WK         PIC 9(6).
           05  VIDEO-WK            PIC X.
           05  ULT-DATA-REAL-WK    PIC 9(8).
           05  PRIORIDADE-WK       PIC 9(4).
           05  TAMANHO-WK          PIC X(20).
           05  OBS-WK              PIC X(30).
           05  CLIP-WK OCCURS 4 TIMES PIC X(14).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP051.CPB".
           COPY "MTP051.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-COD001             PIC XX       VALUE SPACES.
           05  ST-COD002             PIC XX       VALUE SPACES.
           05  ST-COD005             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD050             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-MTD021             PIC XX       VALUE SPACES.
           05  ST-MTD021D            PIC XX       VALUE SPACES.
           05  ST-MTD051             PIC XX       VALUE SPACES.
           05  ST-LBD027             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  GS-LINDET2            PIC X(180)   VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-E              PIC 99/9999  BLANK WHEN ZEROS.
           05  QTDE-E                PIC ZZZ.ZZZ BLANK WHEN ZEROS.
           05  QTDE-E1               PIC ZZZ.ZZZ.ZZZ BLANK WHEN ZEROS.
           05  CLIP-ANT OCCURS 4 TIMES PIC 9(3).
           05  I                     PIC 9        VALUE ZEROS.
           05  MESANO-INI            PIC 9(6)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(6)     VALUE ZEROS.
           05  PRIORIDADE-E          PIC ZZZZ.
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  ULT-DATA-EVENTO       PIC 9(8)     VALUE ZEROS.
      *    VARIAVEIS P/ TOTALIZAÇÃO
           05  TOT-CONTRATO          PIC 9(9)     VALUE ZEROS.
           05  TOT-FOTO              PIC 9(9)     VALUE ZEROS.
           05  TOT-FORMANDO          PIC 9(9)     VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  ACHEI                 PIC X(01).
           05  PRIORIDADE            PIC 9(03)    VALUE ZEROS.
           05  POSICAO               PIC 9(02)    VALUE ZEROS.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).

       01  CAB01.
           05  EMPRESA-REL         PIC X(85)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(47)   VALUE
           "RELAT.PRIORIDADE NA PRODUCAO DE ALBUNS- ORDEM: ".
           05  ORDEM-REL           PIC X(21)   VALUE SPACES.
           05  FILLER              PIC X(9)    VALUE "MES/ANO: ".
           05  MESANO-INI-REL      PIC 99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X(3)    VALUE " a".
           05  MESANO-FIM-REL      PIC 99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(8)    VALUE "REALIZ: ".
           05  DATA-INI-REL        PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X(3)    VALUE " a".
           05  DATA-FIM-REL        PIC 99/99/9999 BLANK WHEN ZEROS.

       01  CAB03.
           05  FILLER              PIC X(131)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(131)  VALUE
           "|CONT|IDENTIFIC.|STATUS         |CIDADE       |FORM P V QT.F 60
      -    "OTOS DATA-ENCER|PRI |PRODUTO          |OBSERVACAO
      -    "          |".

       01  LINDET.
           05  LINDET-REL         PIC X(131)   VALUE
           "|    |          |               |             |
      -    "               |    |                 |
      -    "          |".
       01  LINDET1.
           05  LINDET-REL1        PIC X(131)   VALUE
           "|    |          |               |             |
      -    "               |    |                 |
      -    "          |".
       01  TRACO-SEPARADOR.
           05 FILLER                         PIC  X(133) VALUE
           "|____|__________|_______________|_____________|_____________
      -    "_______________|____|_________________|_____________________
      -    "__________|".

       01  CAB05.
           05  FILLER              PIC X(20)   VALUE SPACES.
           05  FILLER              PIC X(90)   VALUE
           "TOTAL-CONTRATO      TOTAL-FORMANDOS     TOTAL-FOTOS".
       01  LINTOT.
           05  FILLER              PIC X(20)   VALUE SPACES.
           05  LINTOT-REL          PIC X(90)   VALUE SPACES.


       01 DET-ANOTACOES.
          05 DET-DIA                   PIC 99/.
          05 DET-MES                   PIC 99/.
          05 DET-ANO                   PIC 9999.
          05 FILLER                    PIC X(01).
          05 DET-SEQ                   PIC ZZ9.
          05 FILLER                    PIC X(01).
          05 DET-HORA                  PIC 9(02).
          05 FILLER                    PIC X(01) VALUE ":".
          05 DET-MINU                  PIC 9(02).
          05 FILLER                    PIC X(02).
          05 DET-USUARIO               PIC X(05).
          05 FILLER                    PIC X(01).
          05 DET-ANOTACAO              PIC X(76).

       01 DET-ANOTACOES2.
          05 FILLER                    PIC X(028).
          05 DET-ANOTACAO2             PIC X(76).

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
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "COD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "COD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD002.
           MOVE "COD005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD005.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD050"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD050.
           MOVE "COD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "IED010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "MTD021"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD021.
           MOVE "MTD021D" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD021D.
           MOVE "MTD051"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD051.
           MOVE "LBD027"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD027.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN I-O   MTD051 MTD021D
           CLOSE      MTD051 MTD021D
           OPEN INPUT MTD051 MTD021D

           OPEN INPUT CAD010 IED010 MTD021 COD005 COD040 COD060 LBD027
                      COD002 COD050 COD001 CAD012.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD002 <> "00"
              MOVE "ERRO ABERTURA COD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD005 <> "00"
              MOVE "ERRO ABERTURA COD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD050 <> "00"
              MOVE "ERRO ABERTURA COD050: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD050 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED010 <> "00"
              MOVE "ERRO ABERTURA IED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD021 <> "00"
              MOVE "ERRO ABERTURA MTD021: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD021D <> "00"
              MOVE "ERRO ABERTURA MTD021D: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD021D TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD027 <> "00"
              MOVE "ERRO ABERTURA LBD027: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD027 TO GS-MENSAGEM-ERRO(23: 02)
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
                   PERFORM VERIFICAR-SENHA-STATUS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
      *             PERFORM VERIFICA-DATA-MOVTO-ANT
                    PERFORM GRAVA-WORK
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-LE-STATUS-TRUE
                    PERFORM LE-STATUS
               WHEN GS-CHAMAR-POP-STATUS-TRUE
                    PERFORM CHAMAR-POPUP-STATUS
               WHEN GS-INCLUIR-TRUE
                    PERFORM INCLUIR
               WHEN GS-CARREGAR-STATUS-TRUE
                    PERFORM CARREGAR-STATUS
               WHEN GS-GRAVA-STATUS-TRUE
                    PERFORM GRAVA-STATUS
               WHEN GS-LE-CIDADE-TRUE
                    PERFORM LE-CIDADE
               WHEN GS-LE-REGIAO-TRUE
                    PERFORM LE-REGIAO
               WHEN GS-POPUP-CIDADE-TRUE
                    PERFORM POPUP-CIDADE
               WHEN GS-POPUP-REGIAO-TRUE
                    PERFORM POPUP-REGIAO
               WHEN GS-LE-PRODUTO-TRUE
                   PERFORM LER-PRODUTO
               WHEN GS-POPUP-PRODUTO-TRUE
                   PERFORM POPUP-PRODUTO
               WHEN GS-LER-TRUE
                   PERFORM LER-LISTA

               WHEN GS-IMPRIMIR-INDIV-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIMIR-INDIVIDUAL
                    END-IF
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIMIR-INDIVIDUAL SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.

           MOVE FUNCTION NUMVAL(GS-LINDET2(01: 5))  TO CONTRATO-WK
           READ WORK INVALID KEY
                MOVE "Dados para Impressão Não Encontrado" to mensagem
                move "C" to tipo-msg
                perform exibir-mensagem
           NOT INVALID KEY
                PERFORM MOVER-DADOS-RELATORIO
           END-READ

           COPY DESCONDENSA.


       LER-LISTA SECTION.
           MOVE "N" TO ACHEI

           MOVE GS-LINDET TO GS-LINDET2

           CLEAR-OBJECT LB2

           INITIALIZE REG-MTD021D
                      GS-ANOTACAO
                      GS-CONT
           MOVE FUNCTION NUMVAL(GS-LINDET(01: 5))  TO CONTRATO-MT21D
                                                      CONTRATO-WK
           START MTD021D KEY IS NOT LESS CHAVE-MT21D INVALID KEY
                MOVE "10" TO ST-MTD021D.

           PERFORM UNTIL ST-MTD021D = "10"
                READ MTD021D NEXT AT END
                     MOVE "10" TO ST-MTD021D
                NOT AT END
                     IF CONTRATO-WK <> CONTRATO-MT21D
                        MOVE "10" TO ST-MTD021D
                     ELSE
                        ADD 1                       TO GS-CONT

                        MOVE DATA-MT21D(1:4)        TO DET-ANO
                        MOVE DATA-MT21D(5:2)        TO DET-MES
                        MOVE DATA-MT21D(7:2)        TO DET-DIA

                        MOVE SEQUENCIA-MT21D        TO DET-SEQ

                        MOVE HORA-MT21D(1:2)        TO DET-HORA
                        MOVE HORA-MT21D(3:2)        TO DET-MINU

                        MOVE USUARIO-MT21D          TO DET-USUARIO


                        MOVE ANOTACOES-MT21D(1:76)  TO DET-ANOTACAO
                        MOVE DET-ANOTACOES          TO GS-LINDET
                        MOVE "INSERIR-ANOT"         TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM

                        IF ANOTACOES-MT21D(77:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(77:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(153:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(153:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(229:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(229:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(305:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(305:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(381:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(381:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(457:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(457:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(533:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(533:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF
                        IF ANOTACOES-MT21D(609:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(609:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF
                        IF ANOTACOES-MT21D(685:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(685:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF
                        IF ANOTACOES-MT21D(761:30) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(761:30)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF
                        MOVE "S" TO ACHEI
                     END-IF
                END-READ
           END-PERFORM.

           IF ACHEI = "N"
              UNSHOW-WINDOW WIN3 PRINCIPAL
           ELSE
              REFRESH-OBJECT WIN3
              SHOW-WINDOW WIN3
              SET-FOCUS WIN3.

       VERIFICAR-SENHA-STATUS SECTION.
           OPEN INPUT CAD004
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA48"          TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
               DISABLE-OBJECT PB13
           NOT INVALID KEY
               ENABLE-OBJECT PB13.

           CLOSE CAD004.

       LER-PRODUTO SECTION.
           MOVE GS-PRODUTO              TO CODIGO-LB27
           READ LBD027 INVALID KEY
                MOVE "******"           TO DESCRICAO-LB27.

           MOVE DESCRICAO-LB27          TO GS-TAMANHO.
       POPUP-PRODUTO SECTION.
           CALL   "LBP027T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "LBP027T"
           MOVE PASSAR-STRING-1(33: 2) TO GS-PRODUTO
           MOVE PASSAR-STRING-1(1: 20) TO GS-TAMANHO.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop"

          move-object-handle win1 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win2 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


          move-object-handle win3 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LE-CIDADE SECTION.
           MOVE GS-ACP-CIDADE  TO CIDADE
           READ CAD010 INVALID KEY
                MOVE SPACES TO NOME-CID UF-CID NOME-COMPL-CID.
           MOVE NOME-CID TO GS-DESC-CIDADE.

       LE-REGIAO SECTION.
           MOVE GS-ACP-REGIAO       TO CODIGO-REG.
           READ CAD012 INVALID KEY MOVE SPACES TO NOME-REG.
           MOVE NOME-REG        TO GS-DESC-REGIAO.

       POPUP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP010T".
           MOVE PASSAR-STRING-1(35: 4) TO GS-ACP-CIDADE.
           PERFORM LE-CIDADE.

       POPUP-REGIAO SECTION.
           CALL   "CAP012T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CAP012T".
           MOVE PASSAR-STRING-1(33: 2)  TO GS-ACP-REGIAO.
           MOVE PASSAR-STRING-1(1: 30)  TO GS-DESC-REGIAO.

       GRAVA-STATUS SECTION.
           CLOSE    MTD051
           OPEN I-O MTD051

           INITIALIZE REG-MTD051
           START MTD051 KEY IS NOT LESS CODIGO-MTD051 INVALID KEY
                MOVE "10" TO ST-MTD051.
           PERFORM UNTIL ST-MTD051 = "10"
                READ MTD051 NEXT AT END
                     MOVE "10" TO ST-MTD051
                NOT AT END
                     DELETE MTD051 INVALID KEY
                         MOVE "Erro de Exclusão...MTD051" TO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                     END-DELETE
                END-READ
           END-PERFORM

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               MOVE GS-LINHA-STATUS(1:2)   TO CODIGO-MTD051
               WRITE REG-MTD051
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      MTD051
           OPEN INPUT MTD051.

       CARREGAR-STATUS SECTION.
           MOVE "LIMPAR-STATUS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "N" TO ACHEI

           INITIALIZE REG-MTD051
           START MTD051 KEY IS NOT LESS CODIGO-MTD051 INVALID KEY
               MOVE "10" TO ST-MTD051.

           PERFORM UNTIL ST-MTD051 = "10"
               READ MTD051 NEXT AT END
                    MOVE "10" TO ST-MTD051
               NOT AT END
                    MOVE CODIGO-MTD051 TO CODIGO-CO01
                    READ COD001 NOT INVALID KEY
                         MOVE "S"              TO ACHEI
                         MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                         MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                         MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                    END-READ
               END-READ
           END-PERFORM

           IF ACHEI = "N"
              INITIALIZE REG-COD001
              MOVE 50        TO CODIGO-CO01
              START COD001 KEY IS NOT LESS CODIGO-CO01 INVALID KEY
                   MOVE "10" TO ST-COD001
              END-START

              PERFORM UNTIL ST-COD001 = "10"
                   READ COD001 NEXT AT END
                        MOVE "10" TO ST-COD001
                   NOT AT END
                        MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                        MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                        MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
              END-PERFORM.

       INCLUIR SECTION.
           MOVE "Você Deseja Incluir o Status?" TO MENSAGEM
           MOVE "Q" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           IF RESP-MSG = "S"
              MOVE GS-STATUS        TO GS-LINHA-STATUS(1:2)
              MOVE GS-DESC-STATUS   TO GS-LINHA-STATUS(4:30)
              MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

       LE-STATUS SECTION.
           MOVE GS-STATUS              TO CODIGO-CO01
           READ COD001 INVALID KEY
                MOVE SPACES            TO STATUS-CO01
           END-READ
           MOVE STATUS-CO01            TO GS-DESC-STATUS.

       CHAMAR-POPUP-STATUS SECTION.
           CALL   "COP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP001T".
           MOVE PASSAR-STRING-1(33: 2) TO GS-STATUS
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-STATUS.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
      *------------------------------------------------------------
       GRAVA-WORK SECTION.
           CLOSE       WORK
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-MESANO-INI(1: 2)  TO MESANO-INI(5: 2)
           MOVE GS-MESANO-INI(3: 4)  TO MESANO-INI(1: 4)
           MOVE GS-MESANO-FIM(1: 2)  TO MESANO-FIM(5: 2)
           MOVE GS-MESANO-FIM(3: 4)  TO MESANO-FIM(1: 4)
           IF MESANO-FIM < MESANO-INI
              MOVE MESANO-FIM        TO MESANO-INI.


           MOVE GS-DATA-INI          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV             TO DATA-INI


           MOVE GS-DATA-FIM          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV             TO DATA-FIM

           IF MESANO-INI > 0
              PERFORM GRAVAR-POR-MESANO
           ELSE
              PERFORM GRAVAR-POR-INTERVALO.


           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       GRAVAR-POR-INTERVALO SECTION.
           IF DATA-FIM > 0
              ADD 1               TO DATA-FIM.
           INITIALIZE REG-COD040
           START COD040 KEY IS NOT < NR-CONTRATO-CO40 INVALID KEY
                 MOVE "10" TO ST-COD040.

           PERFORM UNTIL ST-COD040 = "10"
                 READ COD040 NEXT RECORD AT END
                      MOVE "10" TO ST-COD040
                 NOT AT END
                      INITIALIZE REG-COD060
                      MOVE NR-CONTRATO-CO40 TO NR-CONTRATO-CO60
                      IF DATA-FIM > 0
                         MOVE DATA-FIM       TO DATAREALIZA-CO60
                      ELSE
                         MOVE ALL "9"        TO DATAREALIZA-CO60
                      END-IF
                      START COD060 KEY IS LESS THAN ALT1-CO60
                                                             INVALID KEY
                            MOVE "10" TO ST-COD060
                      END-START
                      PERFORM UNTIL ST-COD060 = "10"
                            READ COD060 PREVIOUS RECORD AT END
                                 MOVE "10" TO ST-COD060
                            NOT AT END
                                 IF NR-CONTRATO-CO40 <> NR-CONTRATO-CO60
                                    MOVE "10" TO ST-COD060
                                 ELSE
                                    IF DATAREALIZA-CO60 NOT <
                                       DATA-INI
                                       MOVE NR-CONTRATO-CO60 TO
                                            CONTRATO-WK
                                       READ WORK INVALID KEY
                                            PERFORM TESTAR-DADOS
                                            MOVE "10" TO ST-COD060
                                       END-READ
                                    ELSE
                                       MOVE "10" TO ST-COD060
                                    END-IF
                                 END-IF
                            END-READ
                      END-PERFORM
                 END-READ
           END-PERFORM.

       TESTAR-DADOS SECTION.
           MOVE NR-CONTRATO-CO40 TO CONTRATO-MT21
           READ MTD021 INVALID KEY
                INITIALIZE REG-MTD021
                MOVE "N" TO ACHEI
           NOT INVALID KEY
                MOVE "S" TO ACHEI
           END-READ
           IF (GS-PRIORIDADE-DEFINIDA = 1 AND ACHEI = "S") OR
               GS-PRIORIDADE-DEFINIDA = 0
               IF GS-ACP-CIDADE = 0 OR  CIDADE-CO40
                  IF GS-PRODUTO = 0 OR  PRODUTO-MT21 OR
                    (GS-PRODUTO = 0 AND PRODUTO-MT21 IS NOT NUMERIC)
                     MOVE FUNCTION NUMVAL (GS-PRIORIDADE(1:1))
                       TO PRIORIDADE
                     IF (PRIORIDADE > 0 AND PRIORIDADE =PRIORIDADE-MT21)
                         OR (GS-PRIORIDADE = SPACES)
                         MOVE CIDADE-CO40 TO CIDADE
                         READ CAD010 INVALID KEY
                              INITIALIZE REG-CAD010
                         END-READ
                         IF GS-ACP-REGIAO = 0 OR REGIAO-CID
                              IF GS-ACP-UF = SPACES OR UF-CID
                                 PERFORM PESQUISAR-STATUS
                                 IF ACHEI = "S"
                                    MOVE NR-CONTRATO-CO40
                                      TO GS-EXIBE-MOVTO
                                    MOVE "TELA-AGUARDA1"
                                      TO DS-PROCEDURE
                                    PERFORM
                                        CALL-DIALOG-SYSTEM
                                    MOVE DATAREALIZA-CO60
                                      TO ULT-DATA-EVENTO
                                    PERFORM ACHA-CLIP
                                    PERFORM
                                          MOVER-DADOS-WORK
                                    WRITE REG-WORK.


       GRAVAR-POR-MESANO SECTION.
           IF DATA-FIM > 0
              ADD 1 TO DATA-FIM.

           INITIALIZE REG-COD040
           MOVE MESANO-INI           TO MESANO-PREV-CO40.
           MOVE ZEROS                TO NR-CONTRATO-CO40.
           START COD040 KEY IS NOT < ALT1-CO40 INVALID KEY
                 MOVE "10" TO ST-COD040.

           PERFORM UNTIL ST-COD040 = "10"
             READ COD040 NEXT RECORD AT END
                  MOVE "10" TO ST-COD040
             NOT AT END
                  IF MESANO-PREV-CO40 > MESANO-FIM
                     MOVE "10" TO ST-COD040
                  ELSE
                     MOVE NR-CONTRATO-CO40        TO CONTRATO-MT21
                     READ MTD021 INVALID KEY
                          INITIALIZE REG-MTD021
                          MOVE "N" TO ACHEI
                     NOT INVALID KEY
                          MOVE "S" TO ACHEI
                     END-READ

                     IF (GS-PRIORIDADE-DEFINIDA = 1 AND ACHEI = "S") OR
                         GS-PRIORIDADE-DEFINIDA = 0
                         IF GS-ACP-CIDADE = 0 OR CIDADE-CO40
                            IF GS-PRODUTO = 0 OR PRODUTO-MT21 OR
                              (GS-PRODUTO = 0 AND PRODUTO-MT21 IS NOT
                                NUMERIC)
                               MOVE FUNCTION
                                    NUMVAL(GS-PRIORIDADE(1:1)) TO
                                    PRIORIDADE
                               IF (PRIORIDADE > 0 AND PRIORIDADE =
                                   PRIORIDADE-MT21) OR
                                  (GS-PRIORIDADE = SPACES)
                                  MOVE CIDADE-CO40    TO CIDADE
                                  READ CAD010 INVALID KEY
                                       INITIALIZE REG-CAD010
                                  END-READ
                                  IF GS-ACP-REGIAO = 0 OR REGIAO-CID
                                     IF GS-ACP-UF = SPACES OR UF-CID
                                        PERFORM PESQUISAR-STATUS
                                        IF ACHEI = "S"
                                           MOVE NR-CONTRATO-CO40
                                             TO GS-EXIBE-MOVTO
                                           MOVE "TELA-AGUARDA1"
                                             TO DS-PROCEDURE
                                           PERFORM CALL-DIALOG-SYSTEM
                                           PERFORM
                                                  VERIFICA-ULTIMO-EVENTO
                                           IF (ULT-DATA-EVENTO NOT <
                                               DATA-INI AND
                                               ULT-DATA-EVENTO NOT >
                                               DATA-FIM) OR
                                              (DATA-INI = 0 AND
                                               DATA-FIM = 0)
                                               PERFORM ACHA-CLIP
                                               PERFORM MOVER-DADOS-WORK
                                               WRITE REG-WORK
                                           ELSE
                                               CONTINUE
                                           END-IF
                                        END-IF
                                     END-IF
                                  END-IF
                               END-IF
                            END-IF
                         END-IF
                     END-IF
             END-READ
           END-PERFORM.

       PESQUISAR-STATUS SECTION.
           MOVE "N" TO ACHEI

           MOVE 1           TO GS-CONT
           MOVE SPACES      TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES OR ACHEI = "S"
               IF GS-LINHA-STATUS(1:2) = STATUS-CO40
                  MOVE "S" TO ACHEI
               END-IF
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       VERIFICA-ULTIMO-EVENTO SECTION.
      *    MOVE NR-CONTRATO-CO40  TO NR-CONTRATO-CO60
      *    MOVE DATA-INI          TO DATAREALIZA-CO60
      *    START COD060 KEY IS NOT < ALT1-CO60 INVALID KEY
      *          MOVE "10" TO ST-COD060.
      *    PERFORM UNTIL ST-COD060 = "10"
      *      READ COD060 NEXT RECORD AT END MOVE "10" TO ST-COD060
      *        NOT AT END
      *         IF NR-CONTRATO-CO60 <> NR-CONTRATO-CO40
      *             MOVE "10" TO ST-COD060
      *         ELSE
      *             MOVE DATAREALIZA-CO60 TO ULT-DATA-EVENTO
      *         END-IF
      *      END-READ
      *    END-PERFORM.
           INITIALIZE REG-COD060
           MOVE NR-CONTRATO-CO40  TO NR-CONTRATO-CO60
           IF DATA-FIM > 0
              MOVE DATA-FIM       TO DATAREALIZA-CO60
           ELSE
              MOVE ALL "9"        TO DATAREALIZA-CO60
           END-IF
           START COD060 KEY IS LESS THAN ALT1-CO60 INVALID KEY
                 MOVE "10" TO ST-COD060.
           PERFORM UNTIL ST-COD060 = "10"
             READ COD060 NEXT RECORD AT END
                  MOVE "10" TO ST-COD060
             NOT AT END
                  IF NR-CONTRATO-CO60 <> NR-CONTRATO-CO40
                     MOVE "10" TO ST-COD060
                  ELSE
                     MOVE DATAREALIZA-CO60 TO ULT-DATA-EVENTO
                     MOVE "10" TO ST-COD060
                  END-IF
             END-READ
           END-PERFORM.
       MOVER-DADOS-WORK SECTION.
           INITIALIZE REG-WORK.
           MOVE NR-CONTRATO-CO40         TO CONTRATO-WK
           MOVE IDENTIFICACAO-CO40       TO IDENTIFICACAO-WK
      *    MOVE INSTITUICAO-CO40         TO CODIGO-IE10
      *    READ IED010 INVALID KEY
      *         MOVE SPACES              TO NOME-IE10.
      *    MOVE NOME-IE10                TO INSTITUICAO-WK

           MOVE MESANO-PREV-CO40         TO MESANO-WK
           MOVE CIDADE-CO40              TO CIDADE
           READ CAD010 INVALID KEY
                MOVE SPACES              TO NOME-CID.
           MOVE NOME-CID                 TO CIDADE-WK
           MOVE QTDE-FORM-CO40           TO QT-FORM-WK
           MOVE PADRAO-CO40              TO PADRAO-WK PADRAO-CO05
           READ COD005 INVALID KEY
                MOVE ZEROS               TO PREV-FOTOS-CO05.

           COMPUTE QT-FOTOS-WK = PREV-FOTOS-CO05 * QTDE-FORM-CO40.
           IF COBERTURA-CO40 = 1 OR 2 OR 4 OR 6
              MOVE "V"                   TO VIDEO-WK
           ELSE
              MOVE SPACES                TO VIDEO-WK
           END-IF

           MOVE ULT-DATA-EVENTO         TO ULT-DATA-REAL-WK

           MOVE NR-CONTRATO-CO40        TO CONTRATO-MT21
           READ MTD021 INVALID KEY
                MOVE TIPO-FOTOG-CO40    TO CODIGO-LB27
                READ LBD027 INVALID KEY
                     MOVE SPACES        TO DESCRICAO-LB27
                END-READ
                MOVE SPACES             TO OBS-WK
                MOVE 9999               TO PRIORIDADE-WK
                MOVE DESCRICAO-LB27     TO TAMANHO-WK
                MOVE "****************" TO STATUS-WK
           NOT INVALID KEY
                MOVE ORDEM-MT21         TO PRIORIDADE-WK
                MOVE PRODUTO-MT21       TO CODIGO-LB27
                READ LBD027 INVALID KEY
                     MOVE "********"    TO DESCRICAO-LB27
                END-READ
                MOVE DESCRICAO-LB27     TO TAMANHO-WK
                EVALUATE PRIORIDADE-MT21
                    WHEN 0 MOVE "0-Nao Recebido"   TO STATUS-WK
                    WHEN 1 MOVE "1-Distribuicao"   TO STATUS-WK
                    WHEN 2 MOVE "2-Photoshop"      TO STATUS-WK
                    WHEN 3 MOVE "3-Correcao"       TO STATUS-WK
                    WHEN 4 MOVE "4-Impressao"      TO STATUS-WK
                    WHEN 5 MOVE "5-Serv. Externo"  TO STATUS-WK
                    WHEN 6 MOVE "6-Montagem"       TO STATUS-WK
                    WHEN 7 MOVE "7-Finalizado"     TO STATUS-WK
                END-EVALUATE
                MOVE OBS-MT21           TO OBS-WK
           END-READ.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
             MOVE CLIP-ANT(I)           TO CODIGO-CO02
             READ COD002 INVALID KEY
                  MOVE SPACES           TO NOME-CO02
             END-READ
             MOVE NOME-CO02             TO CLIP-WK(I)
           END-PERFORM.
       ACHA-CLIP SECTION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
             MOVE ZEROS TO CLIP-ANT(I)
           END-PERFORM.
           MOVE ZEROS                 TO I.
           MOVE NR-CONTRATO-CO40     TO NR-CONTRATO-CO50.
           MOVE ZEROS                TO ITEM-CO50.
           START COD050 KEY IS NOT < CHAVE-CO50 INVALID KEY
                 MOVE "10" TO ST-COD050.
           READ COD050 NEXT RECORD AT END
                 MOVE "10" TO ST-COD050
           NOT AT END
                 IF CODBRINDE-CO50 = 115 OR = 35 OR = 161 OR = 162
                                         OR = 163 OR = 164
                    IF CODBRINDE-CO50 <> CLIP-ANT(1)     AND
                       <> CLIP-ANT(2) AND <> CLIP-ANT(3) AND
                       <> CLIP-ANT(4)
                       ADD 1 TO I
                       IF I NOT > 4
                          MOVE CODBRINDE-CO50 TO CLIP-ANT(I)
                       END-IF
                    END-IF
                 END-IF
           END-READ.
      *---------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE ZEROS TO TOT-CONTRATO TOT-FOTO TOT-FORMANDO.

           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
      *    MOVE 5 TO GS-ORDEM
           PERFORM ORDEM.
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
           ADD 1                   TO TOT-CONTRATO
           MOVE CONTRATO-WK        TO GS-LINDET(01: 5)
           MOVE IDENTIFICACAO-WK   TO GS-LINDET(06: 11)
           MOVE STATUS-WK          TO GS-LINDET(18: 14)
           MOVE CIDADE-WK          TO GS-LINDET(34: 14)
           MOVE MESANO-WK(1: 4)    TO MESANO-W(3: 4)
           MOVE MESANO-WK(5: 2)    TO MESANO-W(1: 2)
           MOVE MESANO-W           TO MESANO-E
           MOVE MESANO-E           TO GS-LINDET(48: 8)
           MOVE QT-FORM-WK         TO GS-LINDET(56: 5)
           ADD QT-FORM-WK          TO TOT-FORMANDO
           MOVE PADRAO-WK          TO GS-LINDET(61: 2)
           MOVE VIDEO-WK           TO GS-LINDET(63: 2)
           MOVE QT-FOTOS-WK        TO QTDE-E
           MOVE QTDE-E             TO GS-LINDET(65: 9)
           ADD QT-FOTOS-WK         TO TOT-FOTO
           MOVE ULT-DATA-REAL-WK   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO GS-LINDET(74: 11)
           IF PRIORIDADE-WK = 9999
              MOVE ZEROS           TO PRIORIDADE-E
           ELSE
              MOVE PRIORIDADE-WK   TO PRIORIDADE-E
           END-IF
           MOVE PRIORIDADE-E       TO GS-LINDET(85: 4)
           MOVE TAMANHO-WK         TO GS-LINDET(90: 16)

           MOVE "N"                TO ACHEI

           INITIALIZE REG-MTD021D
           MOVE CONTRATO-WK        TO CONTRATO-MT21D
           START MTD021D KEY IS NOT LESS CHAVE-MT21D INVALID KEY
                MOVE "10"          TO ST-MTD021D.

           PERFORM UNTIL ST-MTD021D = "10"
                READ MTD021D NEXT AT END
                     MOVE "10"     TO ST-MTD021D
                NOT AT END
                     IF CONTRATO-WK <> CONTRATO-MT21D
                        MOVE "10"  TO ST-MTD021D
                     ELSE
                        MOVE "S"   TO ACHEI
                     END-IF
                END-READ
           END-PERFORM

           IF ACHEI = "S"
              MOVE "SIM"           TO GS-LINDET(107:04)
           ELSE
              MOVE SPACES          TO GS-LINDET(107:04)
           END-IF

           MOVE OBS-WK             TO GS-LINDET(111:30).
       ORDEM SECTION.
           INITIALIZE REG-WORK.
           EVALUATE GS-ORDEM
             WHEN 0
                MOVE "CONTRATO  " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 1
                MOVE "IDENTIF/DATA " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT1-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "STATUS/DATA "  TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT2-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "CIDADE/DATA " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT3-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "DATA/PRIORIDADE" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT4-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "PRIORIDADE/DATA  " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT5-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       TOTALIZA SECTION.
           MOVE SPACES TO GS-LINTOT.
           MOVE TOT-CONTRATO      TO QTDE-E1
           MOVE QTDE-E1           TO GS-LINTOT(1: 20)
           MOVE TOT-FORMANDO      TO QTDE-E1
           MOVE QTDE-E1           TO GS-LINTOT(21: 20)
           MOVE TOT-FOTO          TO QTDE-E1
           MOVE QTDE-E1           TO GS-LINTOT(41: 20)
           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP051" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                   PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           MOVE CONTRATO-WK        TO LINDET-REL(02: 4)
           MOVE IDENTIFICACAO-WK   TO LINDET-REL(07: 11)
           MOVE STATUS-WK          TO LINDET-REL(18: 14)
           MOVE CIDADE-WK          TO LINDET-REL(34: 14)
           MOVE QT-FORM-WK         TO LINDET-REL(48: 5)
           MOVE PADRAO-WK          TO LINDET-REL(53: 2)
           MOVE VIDEO-WK           TO LINDET-REL(55: 2)
           MOVE QT-FOTOS-WK        TO QTDE-E
           MOVE QTDE-E             TO LINDET-REL(57: 9)
           MOVE ULT-DATA-REAL-WK   TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO DATA-E
           MOVE DATA-E             TO LINDET-REL(66: 10)

           IF PRIORIDADE-WK = 9999
              MOVE SPACES          TO LINDET-REL(77:4)
           ELSE
              MOVE PRIORIDADE-WK   TO LINDET-REL(77: 4)
           END-IF
           MOVE TAMANHO-WK         TO LINDET-REL(82: 16).
           MOVE OBS-WK             TO LINDET-REL(100: 30)
           WRITE REG-RELAT FROM LINDET

           WRITE REG-RELAT FROM TRACO-SEPARADOR.
           ADD 2 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

           INITIALIZE REG-MTD021D
           MOVE CONTRATO-WK          TO CONTRATO-MT21D
           START MTD021D KEY IS NOT LESS CHAVE-MT21D INVALID KEY
                MOVE "10" TO ST-MTD021D.

           PERFORM UNTIL ST-MTD021D = "10"
                READ MTD021D NEXT AT END
                     MOVE "10" TO ST-MTD021D
                NOT AT END
                     IF CONTRATO-WK <> CONTRATO-MT21D
                        MOVE "10" TO ST-MTD021D
                     ELSE
                        MOVE ZEROS           TO POSICAO
                        PERFORM 11 TIMES
                            MOVE SPACES      TO REG-RELAT

                            ADD 1            TO POSICAO
                            MOVE SPACES      TO REG-RELAT

                            EVALUATE POSICAO
                                WHEN 1  MOVE DATA-MT21D(1:4)
                                          TO DET-ANO
                                        MOVE DATA-MT21D(5:2)
                                          TO DET-MES
                                        MOVE DATA-MT21D(7:2)
                                          TO DET-DIA
                                        MOVE SEQUENCIA-MT21D
                                          TO DET-SEQ
                                        MOVE HORA-MT21D(1:2)
                                          TO DET-HORA
                                        MOVE HORA-MT21D(3:2)
                                          TO DET-MINU
                                        MOVE ANOTACOES-MT21D(1:76)
                                          TO DET-ANOTACAO
                                        MOVE DET-ANOTACOES TO
                                             REG-RELAT(5:100)
                                WHEN 2  MOVE ANOTACOES-MT21D(77:76)
                                          TO DET-ANOTACAO2
                                        MOVE DET-ANOTACOES2 TO
                                             REG-RELAT(5:100)
                                WHEN 3  MOVE ANOTACOES-MT21D(153:76)
                                          TO DET-ANOTACAO2
                                        MOVE DET-ANOTACOES2 TO
                                             REG-RELAT(5:100)
                                WHEN 4  MOVE ANOTACOES-MT21D(229:76)
                                          TO DET-ANOTACAO2
                                        MOVE DET-ANOTACOES2 TO
                                             REG-RELAT(5:100)
                                WHEN 5  MOVE ANOTACOES-MT21D(305:76)
                                          TO DET-ANOTACAO2
                                        MOVE DET-ANOTACOES2 TO
                                             REG-RELAT(5:100)
                                WHEN 6  MOVE ANOTACOES-MT21D(381:76)
                                          TO DET-ANOTACAO2
                                        MOVE DET-ANOTACOES2 TO
                                             REG-RELAT(5:100)
                                WHEN 7  MOVE ANOTACOES-MT21D(457:76)
                                          TO DET-ANOTACAO2
                                        MOVE DET-ANOTACOES2 TO
                                             REG-RELAT(5:100)
                                WHEN 8  MOVE ANOTACOES-MT21D(533:76)
                                          TO DET-ANOTACAO2
                                        MOVE DET-ANOTACOES2 TO
                                             REG-RELAT(5:100)
                                WHEN 9  MOVE ANOTACOES-MT21D(609:76)
                                          TO DET-ANOTACAO2
                                        MOVE DET-ANOTACOES2 TO
                                             REG-RELAT(5:100)
                                WHEN 10 MOVE ANOTACOES-MT21D(685:76)
                                          TO DET-ANOTACAO2
                                        MOVE DET-ANOTACOES2 TO
                                             REG-RELAT(5:100)
                                WHEN 11 MOVE ANOTACOES-MT21D(761:30)
                                          TO DET-ANOTACAO2
                                        MOVE DET-ANOTACOES2 TO
                                             REG-RELAT(5:100)
                            END-EVALUATE
                            IF REG-RELAT <> SPACES
                               WRITE REG-RELAT
                               ADD 1 TO LIN
                               IF LIN > 56
                                  PERFORM CABECALHO
                               END-IF
                            END-IF
                        END-PERFORM
                     END-IF
                END-READ
           END-PERFORM.

      *    PERFORM VARYING I FROM 2 BY 1 UNTIL I > 4
      *      IF CLIP-WK(I) = SPACES
      *         MOVE 4 TO I
      *      ELSE
      *         MOVE CLIP-WK(I)     TO LINDET-REL1(48: 14)
      *         WRITE REG-RELAT FROM LINDET1
      *         ADD 1 TO LIN
      *         IF LIN > 56 PERFORM CABECALHO
      *         END-IF
      *      END-IF
      *    END-PERFORM.

      *    WRITE REG-RELAT FROM TRACO-SEPARADOR.
      *    ADD 2 TO LIN
      *    IF LIN > 56 PERFORM CABECALHO.

       TOTALIZA-REL SECTION.
           MOVE SPACES TO LINTOT-REL.
           MOVE TOT-CONTRATO      TO QTDE-E1
           MOVE QTDE-E1           TO LINTOT-REL(1: 20)
           MOVE TOT-FORMANDO      TO QTDE-E1
           MOVE QTDE-E1           TO LINTOT-REL(21: 20)
           MOVE TOT-FOTO          TO QTDE-E1
           MOVE QTDE-E1           TO LINTOT-REL(41: 20)
           WRITE REG-RELAT FROM CAB05 AFTER 2.
           WRITE REG-RELAT FROM LINTOT.
       CABECALHO SECTION.
           MOVE GS-DATA-INI      TO DATA-INI-REL.
           MOVE GS-DATA-FIM      TO DATA-FIM-REL
           MOVE GS-MESANO-INI    TO MESANO-INI-REL
           MOVE GS-MESANO-FIM    TO MESANO-FIM-REL
           MOVE GS-DESCR-ORDEM   TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 7 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD002 COD040 COD060 IED010 MTD021 LBD027 WORK
                 COD002 COD050 COD001 CAD012 MTD021D.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

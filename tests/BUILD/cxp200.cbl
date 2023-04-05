       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP200.
      *DATA: 16/03/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Relatório de anotações automáticas feita pelo caixa
      *          POR DATA DE OCORRENCIA
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CXPX200.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CXPW200.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CXP200.CPB".
           COPY "CXP200.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CXD200             PIC XX       VALUE SPACES.
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
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  VECTO-INI             PIC 9(8)     VALUE ZEROS.
           05  VECTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  TAXA-E                PIC ZZ,Z     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  HORA-E                PIC X(5)     VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  LISTAR-W              PIC 9        VALUE ZEROS.
           05  ANOTACAO-W            PIC X(16)    VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(59)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(57)   VALUE
           "RELACAO DE ANOTACOES POR DATA DE OCORRENCIA-CAIXA ".
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(17) VALUE "DATA OCORRENCIA: ".
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

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
           MOVE DATA-INV TO DATA-MOVTO-I.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CXD200"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD200.

           OPEN INPUT CXD200.
           IF ST-CXD200 <> "00"
              MOVE "ERRO ABERTURA CXD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CXD200 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-LISTA-ANOTACAO-TRUE
                    PERFORM CARREGA-LISTA-ANOTACAO
               WHEN GS-VERIFICA-DATA-TRUE
                    PERFORM VERIFICA-DATA
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
       VERIFICA-DATA SECTION.
      *    funcao p/ verificar o dia da semana. Caso for segunda deixar
      *    o intervalo de sabado a segunda, caso contrario o intervalo
      *    é a data do dia
           MOVE DATA-MOVTO-W TO GRTIME-DATE.
           MOVE 1            TO GRTIME-TYPE.
           MOVE 8            TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
      *    SE FOR = A SEGUNDA SUBTRAIR 2 DIAS P/ QUE FIQUE NO SABADO
           IF GRTIME-WEEK-NUM = 2
              MOVE DATA-MOVTO-W TO GRTIME-DATE
              MOVE 1            TO GRTIME-TYPE
              MOVE 5            TO GRTIME-FUNCTION
              MOVE 2            TO GRTIME-DAYS
              CALL "GRTIME" USING PARAMETROS-GRTIME
              MOVE GRTIME-DATE-FINAL TO GS-VECTO-INI
              MOVE DATA-MOVTO-W      TO GS-VECTO-FIM
           ELSE MOVE DATA-MOVTO-W TO GS-VECTO-INI GS-VECTO-FIM.
       CARREGA-LISTA-ANOTACAO SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-VECTO-INI TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV      TO VECTO-INI.
           MOVE GS-VECTO-FIM TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV TO VECTO-FIM.
           MOVE VECTO-INI TO DATA-OCORRENCIA-CX200.
           MOVE ZEROS     TO SEQ-CX200.
           START CXD200 KEY IS NOT < CHAVE-CX200 INVALID KEY
                 MOVE "10" TO ST-CXD200.
           PERFORM UNTIL ST-CXD200 = "10"
              READ CXD200 NEXT RECORD AT END MOVE "10" TO ST-CXD200
                NOT AT END
                  MOVE SPACES TO GS-LINDET
                  IF DATA-OCORRENCIA-CX200 > VECTO-FIM
                             MOVE "10" TO ST-CXD200
                  ELSE
                    MOVE DATA-OCORRENCIA-CX200 TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV              TO DATA-E
                    MOVE DATA-E                TO GS-LINDET(01: 14)
                    MOVE SEQ-CX200             TO GS-LINDET(15: 8)
                    MOVE HORA-OCORRENCIA-CX200 TO GS-LINDET(23: 8)
                    MOVE USUARIO-CX200         TO GS-LINDET(31: 10)
                    EVALUATE SITUACAO-ANOTACAO-CX200
                      WHEN 0 MOVE "PENDENTE"   TO GS-LINDET(41: 15)
                      WHEN 1 MOVE "CHECADA "   TO GS-LINDET(41: 15)
                    END-EVALUATE
                    MOVE "INSERE-LIST" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM

                    MOVE SPACES                TO GS-LINDET
                    MOVE DESCRICAO-CX200       TO GS-LINDET
                    MOVE "INSERE-LIST" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                  END-IF
              END-READ
           END-PERFORM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CXP200" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE VECTO-INI TO DATA-OCORRENCIA-CX200.
           MOVE ZEROS     TO SEQ-CX200.
           START CXD200 KEY IS NOT < CHAVE-CX200 INVALID KEY
                 MOVE "10" TO ST-CXD200.
           PERFORM UNTIL ST-CXD200 = "10"
              READ CXD200 NEXT RECORD AT END MOVE "10" TO ST-CXD200
                NOT AT END
                  MOVE SPACES TO GS-LINDET
                  IF DATA-OCORRENCIA-CX200 > VECTO-FIM
                          MOVE "10" TO ST-CXD200
                  ELSE
                    MOVE DATA-OCORRENCIA-CX200 TO DATA-INV
                    CALL "GRIDAT1" USING DATA-INV
                    MOVE DATA-INV              TO DATA-E
                    MOVE DATA-E                TO LINDET-REL(01: 14)
                    MOVE SEQ-CX200             TO LINDET-REL(15: 8)
                    MOVE HORA-OCORRENCIA-CX200 TO LINDET-REL(23: 8)
                    MOVE USUARIO-CX200         TO LINDET-REL(31: 10)
                    EVALUATE SITUACAO-ANOTACAO-CX200
                      WHEN 0 MOVE "PENDENTE"   TO LINDET-REL(41: 15)
                      WHEN 1 MOVE "CHECADA "   TO LINDET-REL(41: 15)
                    END-EVALUATE
                    WRITE REG-RELAT FROM LINDET
                    ADD 1 TO LIN
                    IF LIN > 56 PERFORM CABECALHO
                    END-IF

                    MOVE SPACES                TO LINDET-REL
                    MOVE DESCRICAO-CX200       TO LINDET-REL
                    WRITE REG-RELAT FROM LINDET
                    ADD 1 TO LIN
                    IF LIN > 56 PERFORM CABECALHO
                    END-IF
              END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB03.
           MOVE 3 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CXD200.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

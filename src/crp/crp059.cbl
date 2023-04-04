       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP059.
      *DATA: 26/05/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: Relatório de anotações POR DATA DE OCORRENCIA
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX010.
           COPY CRPX200.
           COPY CRPX201.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW010.
       COPY CRPW200.
       COPY CRPW201.
       COPY LOGACESS.FD.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP059.CPB".
           COPY "CRP059.CPY".
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
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
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
           05  LIN                   PIC 9(02)    VALUE ZEROS.
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
           "RELACAO DE ANOTACOES POR DATA DE OCORRENCIA     ".
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(17) VALUE "DATA OCORRENCIA: ".
           05  VECTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  VECTO-FIM-REL       PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(110)  VALUE ALL "=".
       01  LINDET.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.

       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU         PIC 9(04).
             10 WS-MES-CPU         PIC 9(02).
             10 WS-DIA-CPU         PIC 9(02).
          05 FILLER                PIC X(13).

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

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
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CRD200"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN INPUT CRD200 CRD201 CGD010.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP059"            to logacess-programa
           move "ABERTO"            to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess


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
                    MOVE "AGUARDE" TO GS-STATUS1
                    REFRESH-OBJECT PRINCIPAL
                    PERFORM CARREGA-LISTA-ANOTACAO
                    MOVE SPACES    TO GS-STATUS1
                    MOVE SPACES    TO GS-STATUS2
                    REFRESH-OBJECT PRINCIPAL
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
           MOVE "CLEAR-LIST-BOX"  TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-VECTO-INI      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO VECTO-INI.
           MOVE GS-VECTO-FIM      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO VECTO-FIM.

           INITIALIZE REG-CRD200
           MOVE VECTO-INI         TO DATA-MOVTO-CR200.
           MOVE ZEROS             TO HORA-MOVTO-CR200.
           MOVE SPACES            TO USUARIO-CR200.
           START CRD200 KEY IS NOT < ALT3-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.

           PERFORM UNTIL ST-CRD200 = "10"
              READ CRD200 NEXT RECORD AT END
                   MOVE "10" TO ST-CRD200
              NOT AT END
                  MOVE SPACES TO GS-LINDET
                  IF DATA-MOVTO-CR200 > VECTO-FIM
                     MOVE "10" TO ST-CRD200
                  ELSE
                     IF GS-ACP-USUARIO = SPACES OR USUARIO-CR200
                        MOVE REG-CRD200             TO GS-STATUS2
                        REFRESH-OBJECT PRINCIPAL
                        MOVE DATA-MOVTO-CR200       TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV               TO DATA-E
                        MOVE DATA-E                 TO GS-LINDET(01: 14)

                        MOVE HORA-MOVTO-CR200(1: 2) TO HORA-E(1: 2)
                        MOVE ":"                    TO HORA-E(3: 1)
                        MOVE HORA-MOVTO-CR200(3: 2) TO HORA-E(4: 2)
                        MOVE HORA-E                 TO GS-LINDET(15: 08)
                        MOVE COD-COMPL-CR200        TO GS-LINDET(23: 11)
                                                       COD-COMPL-CG10
                        READ CGD010 INVALID KEY
                             MOVE SPACES            TO COMPRADOR-CG10
                        END-READ
                        MOVE COMPRADOR-CG10         TO GS-LINDET(34: 20)
                        MOVE DATA-RETORNO-CR200     TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV               TO DATA-E
                        MOVE DATA-E                 TO GS-LINDET(56: 15)
                        MOVE USUARIO-CR200          TO GS-LINDET(80: 08)
                        MOVE SEQ-CR200              TO GS-LINDET(88: 05)
                        PERFORM VERIFICA-TIPO-MENSAGEM
                        IF LISTAR-W = 1
                           MOVE "INSERE-LIST" TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                           PERFORM CARREGA-CRD201
                        ELSE
                           CONTINUE
                        END-IF
                     END-IF
              END-READ
           END-PERFORM.
       VERIFICA-TIPO-MENSAGEM SECTION.
      *    listar apenas se o tipo de mensagem for diferente de
      *    alteração ou de troca de portador ou ch.devolvido
           MOVE COD-COMPL-CR200  TO COD-COMPL-CR201
           MOVE SEQ-CR200        TO SEQ-CR201
           MOVE ZEROS            TO SUBSEQ-CR201 LISTAR-W.
      *    LISTAR-W = 0(NÃO) 1(SIM)
           START CRD201 KEY IS NOT < CHAVE-CR201 INVALID KEY
                 MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
              READ CRD201 NEXT RECORD AT END MOVE "10" TO ST-CRD201
                   NOT AT END
                     IF COD-COMPL-CR201 <> COD-COMPL-CR200 OR
                        SEQ-CR201 <> SEQ-CR200
                          MOVE "10" TO ST-CRD201
                     ELSE
                        MOVE ANOTACAO-CR201(1: 28) TO ANOTACAO-W
                        IF ANOTACAO-W = "ALTERACAO EFETUA" OR
                           ANOTACAO-W = "TRANSF.PORTADOR-" OR
                           ANOTACAO-W = "A T E N C A O - "
                           CONTINUE
                        ELSE MOVE 1 TO LISTAR-W
                        END-IF
                     END-IF
                     MOVE "10" TO ST-CRD201
              END-READ
           END-PERFORM.
       CARREGA-CRD201 SECTION.
           MOVE COD-COMPL-CR200  TO COD-COMPL-CR201.
           MOVE SEQ-CR200        TO SEQ-CR201.
           MOVE ZEROS            TO SUBSEQ-CR201.
           START CRD201 KEY IS NOT < CHAVE-CR201 INVALID KEY
                 MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
              READ CRD201 NEXT RECORD AT END MOVE "10" TO ST-CRD201
                   NOT AT END
                     IF COD-COMPL-CR201 <> COD-COMPL-CR200 OR
                        SEQ-CR201 <> SEQ-CR200
                          MOVE "10" TO ST-CRD201
                     ELSE
                        MOVE SPACES TO GS-LINDET
                        MOVE ANOTACAO-CR201 TO GS-LINDET(15: 80)
                        MOVE "INSERE-LIST" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                     END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP059" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE VECTO-INI TO DATA-MOVTO-CR200.
           MOVE ZEROS     TO HORA-MOVTO-CR200.
           MOVE SPACES    TO USUARIO-CR200.
           START CRD200 KEY IS NOT < ALT3-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
                NOT AT END
                  MOVE SPACES TO GS-LINDET
                  IF DATA-MOVTO-CR200 > VECTO-FIM
                          MOVE "10" TO ST-CRD200
                  ELSE
                     IF GS-ACP-USUARIO = SPACES OR USUARIO-CR200
                        MOVE DATA-MOVTO-CR200 TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV         TO DATA-E
                        MOVE DATA-E           TO GS-LINDET(01: 14)
                        MOVE HORA-MOVTO-CR200(1: 2) TO HORA-E(1: 2)
                        MOVE ":"                    TO HORA-E(3: 1)
                        MOVE HORA-MOVTO-CR200(3: 2) TO HORA-E(4: 2)
                        MOVE HORA-E           TO LINDET-REL(15: 8)
                        MOVE COD-COMPL-CR200  TO LINDET-REL(23: 11)
                                                 COD-COMPL-CG10
                        READ CGD010 INVALID KEY
                                      MOVE SPACES TO COMPRADOR-CG10
                        END-READ
                        MOVE COMPRADOR-CG10        TO LINDET-REL(34: 20)
                        MOVE DATA-RETORNO-CR200 TO DATA-INV
                        CALL "GRIDAT1" USING DATA-INV
                        MOVE DATA-INV TO DATA-E
                        MOVE DATA-E           TO LINDET-REL(56: 15)
                        MOVE USUARIO-CR200    TO LINDET-REL(80: 8)
                        MOVE SEQ-CR200        TO LINDET-REL(88: 5)
                        PERFORM VERIFICA-TIPO-MENSAGEM
                        IF LISTAR-W = 1
                           WRITE REG-RELAT FROM LINDET
                           ADD 1 TO LIN
                           IF LIN > 56 PERFORM CABECALHO
                           END-IF
                           PERFORM CARREGA-CRD201-IMPR
                        ELSE
                           CONTINUE
                        END-IF
                     END-IF
                  END-IF
              END-READ
           END-PERFORM.

           COPY DESCONDENSA.
       CARREGA-CRD201-IMPR SECTION.
           MOVE COD-COMPL-CR200  TO COD-COMPL-CR201.
           MOVE SEQ-CR200        TO SEQ-CR201.
           MOVE ZEROS            TO SUBSEQ-CR201.
           START CRD201 KEY IS NOT < CHAVE-CR201 INVALID KEY
                 MOVE "10" TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
              READ CRD201 NEXT RECORD AT END MOVE "10" TO ST-CRD201
                   NOT AT END
                     IF COD-COMPL-CR201 <> COD-COMPL-CR200 OR
                        SEQ-CR201 <> SEQ-CR200
                          MOVE "10" TO ST-CRD201
                     ELSE
                        MOVE SPACES TO LINDET-REL
                        MOVE ANOTACAO-CR201 TO LINDET-REL(15: 80)
                        WRITE REG-RELAT FROM LINDET
                        ADD 1 TO LIN
                        IF LIN > 56 PERFORM CABECALHO
                        END-IF
                     END-IF
              END-READ
           END-PERFORM.
           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
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
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP059"            to logacess-programa
           move "FECHADO"           to logacess-status
           move "10" to fs-logacess
           perform until fs-logacess = "00"
                write reg-logacess invalid key
                    add 1 to logacess-sequencia
                not invalid key
                    move "00" to fs-logacess
                end-write
           end-perform

           close logacess

           CLOSE CGD010 CRD200 CRD201.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

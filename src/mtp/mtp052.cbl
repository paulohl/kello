       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP052.
      *DATA: 19/07/2001
      *AUTORA: LUCIA
      *FUNÇÃO: RESUMO DE REMESSA DE MONTAGEM
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
           COPY COPX040.
           COPY MTPX052.
           COPY MTPX020.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CONTRATO-WK.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW004.
       COPY CAPW012.
       COPY COPW001.
       COPY COPW040.
       COPY MTPW052.
       COPY MTPW020.
       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(4).
           05  ESTOJO-WK           PIC 9(4).
           05  ENCADERNACAO-WK     PIC 9(4).
           05  FOTOS-WK            PIC 9(8).
           05  FITAS-WK            PIC 9(6).
           05  DVD-WK              PIC 9(6).
           05  FOTO-CD-WK          PIC 9(6).
           05  POSTER-WK           PIC 9(4).
           05  DATA-MOV-WK         PIC 9(8).
           05  BOOK-WK             PIC 9(4).
           05  NR-FORMANDOS-WK     PIC 9(5).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP052.CPB".
           COPY "MTP052.CPY".
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
           05  ST-MTD052             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
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
           05  QTDE-E                PIC ZZZ.ZZ9.
      *    BLANK WHEN ZEROS.
           05  QTDE-E1               PIC ZZ.ZZZ.ZZ9.
      *    BLANK WHEN ZEROS.
           05  DATA-INI              PIC 9(8)     VALUE ZEROS.
           05  DATA-FIM              PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  ULT-DATA-EVENTO       PIC 9(8)     VALUE ZEROS.
           05  MEDIA-E               PIC ZZZ,ZZ.
           05  PASSAR-STRING-1       PIC X(40).
           05  ACHEI                 PIC X(01)    VALUE SPACES.

           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(47)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(52)   VALUE
           "RESUMO DE REMESSA DE ALBUM/FOTOS/FITAS- ".
           05  FILLER              PIC X(09)   VALUE SPACES.
           05  FILLER              PIC X(8)    VALUE "MOVTO.: ".
           05  DATA-INI-REL        PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X(3)    VALUE " a".
           05  DATA-FIM-REL        PIC 99/99/9999 BLANK WHEN ZEROS.

       01  CAB03.
           05  FILLER              PIC X(107)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(107)  VALUE
           "CONT QCLI ESTOJO ENCADERN QT.FOTOS QT.FITAS QT.POSTER QT.DVD
      -    " QT.FTCD QT.BK MEDIA F/A MEDIA F/CLI DATA-MONT".

       01  DET-01.
           05 DET-CONTRATO         PIC 9(04).
           05 FILLER               PIC X(01).
           05 DET-NRCLIENTE        PIC ZZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTESTOJO         PIC ZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTENCARD         PIC ZZZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTFOTOS          PIC ZZZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTFITAS          PIC ZZZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTPOSTER         PIC Z.ZZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTDVD            PIC ZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTFTCD           PIC ZZZ.ZZ9.
           05 FILLER               PIC X(01).
           05 DET-QTBK             PIC Z.ZZ9.
           05 FILLER               PIC X(02).
           05 DET-MEDIA-ALB        PIC Z.ZZZ,99.
           05 FILLER               PIC X(04).
           05 DET-MEDIA-CLI        PIC Z.ZZZ,99.
           05 FILLER               PIC X(01).
           05 DET-DATA-MONT        PIC 99/99/9999.

       01  LINDET.
           05  LINDET-REL         PIC X(107)   VALUE SPACES.

       01  CAB05a.
           05  FILLER             PIC X(13)
               VALUE " T. ESTOJOS".
           05  FILLER             PIC X(13)
               VALUE "  T. ENCARD".
           05  FILLER             PIC X(13)
               VALUE "   T. FOTOS".
           05  FILLER             PIC X(13)
               VALUE "   T. FITAS".
           05  FILLER             PIC X(13)
               VALUE "  T. POSTER".

       01  CAB05b.
           05  FILLER             PIC X(13)
               VALUE "     T. DVD".
           05  FILLER             PIC X(13)
               VALUE " T. FOTO CD".
           05  FILLER             PIC X(12)
               VALUE "   T. BOOKS".
           05  FILLER             PIC X(12)
               VALUE " T.FORMANDOS".
           05  FILLER             PIC X(13)
               VALUE "    MEDIA F/A".
           05  FILLER             PIC X(13)
               VALUE " MEDIA CLI".

       01  LINTOTa.
           05  LINTOT-ESTOJO       PIC ZZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-ENCARD       PIC ZZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-FOTOS        PIC ZZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-FITAS        PIC ZZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-POSTER       PIC ZZZ.ZZZ.ZZ9.

       01  LINTOTb.
           05 LINTOT-DVD           PIC ZZZ.ZZZ.ZZ9.
           05 FILLER               PIC X(02).
           05 LINTOT-FOTOCD        PIC ZZZ.ZZZ.ZZ9.
           05 FILLER               PIC X(02).
           05 LINTOT-BOOKS         PIC ZZZ.ZZZ.ZZ9.
           05 FILLER               PIC X(05).
           05 LINTOT-FORMANDOS     PIC ZZZ.ZZ9.
           05 FILLER               PIC X(02).
           05 LINTOT-MEDIA         PIC ZZZ.ZZ9,99.
           05 FILLER               PIC X(02).
           05 LINTOT-MEDIA-CLI     PIC ZZZ.ZZ9,99.

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
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "COD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD052"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD052.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN I-O   MTD052
           CLOSE      MTD052
           OPEN INPUT MTD052

           OPEN INPUT MTD020 COD001 COD040 CAD010 CAD012.
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
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD052 <> "00"
              MOVE "ERRO ABERTURA MTD052: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD052 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-CHAMAR-POP-CIDADE-TRUE
                    PERFORM CHAMAR-POPUP-CIDADE
               WHEN GS-LE-CIDADE-TRUE
                    PERFORM LER-CIDADE
               WHEN GS-CHAMAR-POP-REGIAO-TRUE
                    PERFORM CHAMAR-POPUP-REGIAO
               WHEN GS-LE-REGIAO-TRUE
                    PERFORM LER-REGIAO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       VERIFICAR-SENHA-STATUS SECTION.
           OPEN INPUT CAD004
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA48"          TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
               DISABLE-OBJECT PB12
           NOT INVALID KEY
               ENABLE-OBJECT PB12.

           CLOSE CAD004.

       CHAMAR-POPUP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CAP010T".
           MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE
           MOVE PASSAR-STRING-1(1:30)  TO GS-DESC-CIDADE.

       LER-CIDADE SECTION.
           MOVE GS-CIDADE              TO CIDADE
           READ CAD010 INVALID KEY
                INITIALIZE REG-CAD010.
           MOVE NOME-CID               TO GS-DESC-CIDADE.

       CHAMAR-POPUP-REGIAO SECTION.
           CALL   "CAP012T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CAP012T"
           MOVE PASSAR-STRING-1(33: 2)  TO GS-REGIAO
           MOVE PASSAR-STRING-1(1: 30)  TO GS-DESC-REGIAO.

       LER-REGIAO SECTION.
           MOVE GS-REGIAO              TO CODIGO-REG
           READ CAD012 INVALID KEY
                INITIALIZE REG-CAD012.
           MOVE NOME-REG               TO GS-DESC-REGIAO.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GRAVA-STATUS SECTION.
           CLOSE    MTD052
           OPEN I-O MTD052

           INITIALIZE REG-MTD052
           START MTD052 KEY IS NOT LESS CODIGO-MTD052 INVALID KEY
                MOVE "10" TO ST-MTD052.
           PERFORM UNTIL ST-MTD052 = "10"
                READ MTD052 NEXT AT END
                     MOVE "10" TO ST-MTD052
                NOT AT END
                     DELETE MTD052 INVALID KEY
                         MOVE "Erro de Exclusão...MTD052" TO MENSAGEM
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
               MOVE GS-LINHA-STATUS(1:2)   TO CODIGO-MTD052
               WRITE REG-MTD052
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      MTD052
           OPEN INPUT MTD052.

       CARREGAR-STATUS SECTION.
           MOVE "LIMPAR-STATUS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "N" TO ACHEI

           INITIALIZE REG-MTD052
           START MTD052 KEY IS NOT LESS CODIGO-MTD052 INVALID KEY
               MOVE "10" TO ST-MTD052.

           PERFORM UNTIL ST-MTD052 = "10"
               READ MTD052 NEXT AT END
                    MOVE "10" TO ST-MTD052
               NOT AT END
                    MOVE CODIGO-MTD052 TO CODIGO-CO01
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
              CLOSE      MTD052
              OPEN I-O   MTD052
              INITIALIZE REG-COD001
              MOVE 50        TO CODIGO-CO01
              START COD001 KEY IS NOT LESS CODIGO-CO01 INVALID KEY
                   MOVE "10" TO ST-COD001
              END-START

              PERFORM UNTIL ST-COD001 = "10"
                   READ COD001 NEXT AT END
                        MOVE "10" TO ST-COD001
                   NOT AT END
                        MOVE CODIGO-CO01      TO CODIGO-MTD052
                        WRITE REG-MTD052

                        MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                        MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                        MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
              END-PERFORM
              CLOSE      MTD052
              OPEN INPUT MTD052.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem
           move 1 to gs-flag-critica.

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
           MOVE STATUS-CO01            TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       CHAMAR-POPUP-STATUS SECTION.
           CALL   "COP001T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP001T".
           MOVE PASSAR-STRING-1(33: 2) TO GS-STATUS
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-STATUS
           PERFORM VERIFICAR-IGUAL.

       VERIFICAR-IGUAL SECTION.
           MOVE 0   TO GS-FLAG-CRITICA
           MOVE "N" TO ACHEI
           MOVE 1   TO GS-CONT
           MOVE SPACES TO GS-LINHA-STATUS
           MOVE "LER-LINHA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-STATUS = SPACES
               IF GS-LINHA-STATUS(1:2) = GS-STATUS
                  MOVE "S" TO ACHEI
                  EXIT PERFORM
               ELSE
                  ADD 1 TO GS-CONT
                  MOVE SPACES TO GS-LINHA-STATUS
                  MOVE "LER-LINHA" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
               END-IF
           END-PERFORM

           IF ACHEI = "S"
              MOVE "Status já Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

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
           CLOSE WORK  OPEN OUTPUT WORK CLOSE WORK  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-DATA-INI          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV             TO DATA-INI
           MOVE GS-DATA-FIM          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV             TO DATA-FIM.
           MOVE DATA-INI    TO DATAMOV-MTG.
           MOVE ZEROS       TO ALBUM-MTG.
           START MTD020 KEY IS NOT < CHAVE-MTG INVALID KEY
                  MOVE "10" TO ST-MTD020.

           PERFORM UNTIL ST-MTD020 = "10"
             READ MTD020 NEXT RECORD AT END
                  MOVE "10" TO ST-MTD020
             NOT AT END
                  IF DATAMOV-MTG > DATA-FIM
                     MOVE "10" TO ST-MTD020
                  ELSE
                     MOVE CONTRATO-MTG    TO NR-CONTRATO-CO40
                     READ COD040 INVALID KEY
                          INITIALIZE REG-COD040
                     END-READ

                     PERFORM PESQUISAR-STATUS
                     IF ACHEI = "S"
                        IF GS-CIDADE = 0 OR CIDADE-CO40
                           MOVE CIDADE-CO40     TO CIDADE
                           READ CAD010 INVALID KEY
                                INITIALIZE REG-CAD010
                           END-READ
                           IF GS-UF = SPACES OR UF-CID
                              IF GS-REGIAO = 0 OR REGIAO-CID
                                 MOVE ALBUM-MTG       TO GS-EXIBE-MOVTO
                                 MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
                                 PERFORM CALL-DIALOG-SYSTEM
                                 PERFORM GRAVAR-WORK
                              END-IF
                           END-IF
                        END-IF
                     END-IF
                  END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

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

       GRAVAR-WORK  SECTION.
           INITIALIZE REG-WORK.
           MOVE CONTRATO-MTG      TO CONTRATO-WK
           READ WORK INVALID KEY
                PERFORM MOVER-DADOS-WORK
           NOT INVALID KEY
               PERFORM SOMAR-DADOS-WORK.
       MOVER-DADOS-WORK SECTION.
           MOVE 1                    TO NR-FORMANDOS-WK
           MOVE QT-ESTOJO-MTG        TO ESTOJO-WK
           MOVE QT-ENCADER-MTG       TO ENCADERNACAO-WK
           MOVE QT-FOTOS-MTG         TO FOTOS-WK
           MOVE QT-FITAS-MTG         TO FITAS-WK
           MOVE QT-DVD-MTG           TO DVD-WK
           MOVE QT-BOOK-MTG          TO BOOK-WK
           MOVE QT-FOTO-CD-MTG       TO FOTO-CD-WK
           MOVE QT-POSTER-MTG        TO POSTER-WK
           MOVE DATAMOV-MTG          TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO DATA-MOV-WK.
           WRITE REG-WORK.

       SOMAR-DADOS-WORK SECTION.
           ADD  1                    TO NR-FORMANDOS-WK
           ADD  QT-ESTOJO-MTG        TO ESTOJO-WK
           ADD  QT-ENCADER-MTG       TO ENCADERNACAO-WK
           ADD  QT-FOTOS-MTG         TO FOTOS-WK
           ADD  QT-FITAS-MTG         TO FITAS-WK
           ADD  QT-DVD-MTG           TO DVD-WK
           ADD  QT-POSTER-MTG        TO POSTER-WK
           ADD  QT-FOTO-CD-MTG       TO FOTO-CD-WK
           ADD  QT-BOOK-MTG          TO BOOK-WK
           REWRITE REG-WORK.

      *---------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE ZEROS TO GS-TOT-ESTOJO GS-TOT-ENCADERNACAO GS-TOT-FOTOS
                         GS-TOT-FITAS GS-TOT-POSTER GS-TOT-DVD
                         GS-TOT-FOTO-CD GS-TOT-BOOK GS-TOT-FORMANDOS

           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.

           MOVE CAB04  TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE CAB03  TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-WORK
           MOVE ZEROS TO CONTRATO-WK
           START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                      PERFORM MOVER-DADOS-LINDET
                      ADD NR-FORMANDOS-WK     TO GS-TOT-FORMANDOS
                      ADD ESTOJO-WK           TO GS-TOT-ESTOJO
                      ADD ENCADERNACAO-WK     TO GS-TOT-ENCADERNACAO
                      ADD FOTOS-WK            TO GS-TOT-FOTOS
                      ADD FITAS-WK            TO GS-TOT-FITAS
                      ADD DVD-WK              TO GS-TOT-DVD
                      ADD FOTO-CD-WK          TO GS-TOT-FOTO-CD
                      ADD POSTER-WK           TO GS-TOT-POSTER
                      ADD BOOK-WK             TO GS-TOT-BOOK
                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                 END-READ
           END-PERFORM.
           PERFORM TOTALIZA.
           MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-DADOS-LINDET SECTION.
           MOVE CONTRATO-WK        TO DET-CONTRATO
           MOVE NR-FORMANDOS-WK    TO DET-NRCLIENTE
           MOVE ESTOJO-WK          TO DET-QTESTOJO
           MOVE ENCADERNACAO-WK    TO DET-QTENCARD
           MOVE FOTOS-WK           TO DET-QTFOTOS
           MOVE FITAS-WK           TO DET-QTFITAS
           MOVE POSTER-WK          TO DET-QTPOSTER
           MOVE DVD-WK             TO DET-QTDVD
           MOVE FOTO-CD-WK         TO DET-QTFTCD
           MOVE BOOK-WK            TO DET-QTBK
           COMPUTE DET-MEDIA-CLI = FOTOS-WK / NR-FORMANDOS-WK
           COMPUTE DET-MEDIA-ALB = FOTOS-WK / ENCADERNACAO-WK
           MOVE DATA-MOV-WK        TO DET-DATA-MONT

           MOVE DET-01             TO GS-LINDET.

       TOTALIZA SECTION.
           COMPUTE  GS-MEDIA-W-CLI = GS-TOT-FOTOS / GS-TOT-FORMANDOS.
           COMPUTE  GS-MEDIA-W-FA  = GS-TOT-FOTOS / GS-TOT-ENCADERNACAO.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP052" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.

           INITIALIZE REG-WORK
           MOVE ZEROS TO CONTRATO-WK.
           START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
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
           PERFORM MOVER-DADOS-LINDET.
           MOVE GS-LINDET          TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO.

       TOTALIZA-REL SECTION.

           WRITE REG-RELAT FROM CAB05a AFTER 2.

           MOVE GS-TOT-ESTOJO       TO LINTOT-ESTOJO
           MOVE GS-TOT-ENCADERNACAO TO LINTOT-ENCARD
           MOVE GS-TOT-FOTOS        TO LINTOT-FOTOS
           MOVE GS-TOT-FITAS        TO LINTOT-FITAS
           MOVE GS-TOT-POSTER       TO LINTOT-POSTER
           MOVE GS-TOT-DVD          TO LINTOT-DVD
           MOVE GS-TOT-FOTO-CD      TO LINTOT-FOTOCD
           MOVE GS-TOT-BOOK         TO LINTOT-BOOKS
           MOVE GS-TOT-FORMANDOS    TO LINTOT-FORMANDOS
           MOVE GS-MEDIA-W-FA       TO LINTOT-MEDIA
           MOVE GS-MEDIA-W-CLI      TO LINTOT-MEDIA-CLI

           WRITE REG-RELAT FROM LINTOTa.

           WRITE REG-RELAT FROM CAB05b
           WRITE REG-RELAT FROM LINTOTb.
       CABECALHO SECTION.
           MOVE GS-DATA-INI      TO DATA-INI-REL.
           MOVE GS-DATA-FIM      TO DATA-FIM-REL
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
           CLOSE MTD020 COD001 COD040 MTD052 WORK CAD010 CAD012.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

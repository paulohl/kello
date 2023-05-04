       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAP004.
      *DATA: 13/11/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: Libera acesso a programas pelo menu
      *FUNÇÃO:
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CAPX002.
           COPY CAPX002D.
           COPY CAPX002e.
           COPY CAPX003.
           COPY CAPX004.

           SELECT CAD004b ASSIGN TO PATH-CAD004b
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-CAD004b
                  RECORD KEY IS CHAVE-CA004b = COD-USUARIO-CA004b
                                               PROGRAMA-CA004b.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
           COPY CAPW001.
           COPY CAPW002.
           COPY CAPW002D.
           COPY CAPW002e.
           COPY CAPW003.
           COPY CAPW004.

       FD  CAD004b.
       01  REG-CAD004b.
           05  COD-USUARIO-CA004b  PIC 9(3).
           05  PROGRAMA-CA004b     PIC X(8).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "CAP004.CPB".
           COPY "CAP004.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CPTIME.CPY".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS     PIC X(65)    VALUE SPACES.
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-CAD002D            PIC XX       VALUE SPACES.
           05  ST-CAD002e            PIC XX       VALUE SPACES.
           05  ST-CAD003             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD004b            PIC XX       VALUE SPACES.
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
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-INV          PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  SENHA-WW              PIC 9(4)     COMP-3.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 mensagem                 pic x(200).
       01 tipo-msg                 pic x(01).
       01 resp-msg                 pic x(01).

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
           05  FILLER              PIC X(80)   VALUE
           "RELACAO DE PROGRAMAS LIBERADOS ".
       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)  VALUE
           "PROGRAMA     DESCRICAO                            LIBERADO".
       01  LINDET.
           05  LINDET-REL          PIC X(80)  VALUE SPACES.

           COPY IMPRESSORA.

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
           MOVE DATA-INV TO DATA-DIA-INV.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD003.
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           OPEN INPUT CAD002 CAD003 CAD002E CAD001 CAD002D
           OPEN I-O CAD004
           IF ST-CAD004 = "35"
              CLOSE CAD004   OPEN OUTPUT CAD004   CLOSE CAD004
              OPEN I-O CAD004.
           IF ST-CAD001 <> "00"
              MOVE "ERRO ABERTURA CAD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD002D <> "00"
              MOVE "ERRO ABERTURA CAD002D: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD002D TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD003 <> "00"
              MOVE "ERRO ABERTURA CAD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-LE-USUARIO-TRUE
                    PERFORM LE-USUARIO
               WHEN GS-POPUP-USUARIO-TRUE
                    PERFORM CHAMA-POPUP-USUARIO
               WHEN GS-LE-USUARIO2-TRUE
                    PERFORM LE-USUARIO2
               WHEN GS-POPUP-USUARIO2-TRUE
                    PERFORM CHAMA-POPUP-USUARIO2
               WHEN GS-LE-PROGRAMA-TRUE
                    PERFORM LER-PROGRAMA
               WHEN GS-POPUP-PROGRAMA-TRUE
                    PERFORM POPUP-PROGRAMA
               WHEN GS-LISTAR-USUARIOS-TRUE
                    PERFORM LISTAR-USUARIOS
               WHEN GS-IMPRIMIR-PROGRAMAS-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIMIR-PROGRAMAS
                    END-IF
               WHEN GS-COPIAR-USUARIO-TRUE
                    PERFORM COPIAR-USUARIO
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-LE-EMPRESA-TRUE
                    PERFORM LER-EMPRESA
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       LER-EMPRESA SECTION.
           MOVE GS-EMPRESA TO CODIGO-CA001
           READ CAD001 INVALID KEY
                MOVE "Empresa Inválida" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE NOME-EMP-CA001 TO GS-DESC-EMPRESA
                REFRESH-OBJECT WIN1.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       LER-PROGRAMA SECTION.
           MOVE GS-PROGRAMA TO PROGRAMA-CA003
           READ CAD003 INVALID KEY
                MOVE SPACES TO DESCRICAO-CA003.
           MOVE DESCRICAO-CA003 TO GS-NOME-PROGRAMA.

       POPUP-PROGRAMA SECTION.
           CALL "CAP003T" USING PASSAR-PARAMETROS.
           MOVE PASSAR-PARAMETROS(32: 8) TO GS-PROGRAMA.
           PERFORM LER-PROGRAMA.

       LISTAR-USUARIOS SECTION.
           MOVE "APAGAR-USUARIOS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           IF GS-PROGRAMA = "ELIMINAR"
              MOVE SPACES TO MENSAGEM
              STRING "Deseja eliminar os usuário INATIVOS?" into
                          mensagem
              move "Q" to tipo-msg
              perform exibir-mensagem
              if resp-msg = "S"
                 perform eliminar-cad002E
              end-if
           ELSE
           INITIALIZE REG-CAD002
           START CAD002 KEY IS NOT LESS CODIGO-CA002 INVALID KEY
                 MOVE "10" TO ST-CAD002
           END-START

           PERFORM UNTIL ST-CAD002 = "10"
                 READ CAD002 NEXT AT END
                      MOVE "10" TO ST-CAD002
                 NOT AT END
                      MOVE CODIGO-CA002       TO CODIGO-CA002D
                      READ CAD002D INVALID KEY
                           INITIALIZE REG-CAD002D
                      END-READ
                      IF STATUS-CAD002D <> "Inativo"
                      MOVE CODIGO-CA002       TO COD-USUARIO-CA004
                      MOVE GS-PROGRAMA        TO PROGRAMA-CA004
                      READ CAD004 NOT INVALID KEY
                           MOVE EMPRESA-W     TO EMPRESA-CA002E
                           MOVE CODIGO-CA002  TO COD-USUARIO-CA002E
                           READ CAD002E NOT INVALID KEY
                                MOVE SPACES        TO GS-LINDET

                                MOVE CODIGO-CA002  TO GS-LINDET(3:3)
                                MOVE NOME-CA002    TO GS-LINDET(12:30)

                                MOVE "INSERE-USUARIOS" TO DS-PROCEDURE
                                PERFORM CALL-DIALOG-SYSTEM
                           END-READ
                      END-READ
                      END-IF
                 END-READ
           END-PERFORM.

       eliminar-cad002E section.
           close      cad002e
           open i-o   cad002e
           initialize reg-cad002e
           start cad002e key is not less chave-usu-ca002e invalid key
                 move "10" to st-cad002e.

           perform until st-cad002e = "10"
                 read cad002e next at end
                      move "10" to st-cad002e
                 not at end
                      move cod-usuario-ca002e to codigo-ca002
                      read cad002 invalid key
                           delete cad002e
                      end-read
                 end-read
           end-perform
           close      cad002e
           open input cad002e

           initialize reg-cad004
           start cad004 key is not less chave-ca004 invalid key
                 move "10" to st-cad004
           end-start
           perform until st-cad004 = "10"
                 read cad004 next at end
                      move "10" to st-cad004
                 not at end
                      move cod-usuario-ca004 to codigo-ca002
                      read cad002 invalid key
                           delete cad004
                      end-read
                 end-read
           end-perform.

       eliminar-cad002E-fim.
           exit.

       IMPRIMIR-PROGRAMAS SECTION.
           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.


           MOVE SPACES     TO GS-LINDET
           MOVE "CÓDIGO"   TO GS-LINDET
           MOVE "USUÁRIO"  TO GS-LINDET(12:30)

           MOVE ALL "="    TO GS-LINDET(1:42)

           MOVE SPACES     TO GS-LINDET
           MOVE 1          TO GS-LINHA
           MOVE "LER-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINDET = SPACES
               MOVE GS-LINDET    TO REG-RELAT

               WRITE REG-RELAT

               MOVE SPACES       TO GS-LINDET
               ADD 1             TO GS-LINHA
               MOVE "LER-LIST"   TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           MOVE SPACES TO REG-RELAT.
           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.

       LE-USUARIO SECTION.
           MOVE GS-CODIGO TO CODIGO-CA002.
           READ CAD002 INVALID KEY MOVE SPACES TO NOME-CA002.
           MOVE NOME-CA002 TO GS-USUARIO.

       LE-USUARIO2 SECTION.
           MOVE GS-CODIGO-NOVO TO CODIGO-CA002.
           READ CAD002 INVALID KEY
                MOVE SPACES TO NOME-CA002.
           MOVE NOME-CA002 TO GS-USUARIO-NOVO.

       CHAMA-POPUP-USUARIO SECTION.
           CALL "CAP002T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP002T"
           MOVE PASSAR-PARAMETROS(38: 3) TO GS-CODIGO.
           PERFORM LE-USUARIO.

       CHAMA-POPUP-USUARIO2 SECTION.
           CALL "CAP002T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP002T"
           MOVE PASSAR-PARAMETROS(38: 3) TO GS-CODIGO-NOVO.
           PERFORM LE-USUARIO2.

       COPIAR-USUARIO SECTION.
           IF GS-EMPRESA <> EMPRESA-W
              MOVE GS-EMPRESA         TO EMP-REC
              MOVE "CAD004"           TO ARQ-REC
              MOVE EMPRESA-REF        TO PATH-CAD004B
              OPEN I-O CAD004B

              INITIALIZE REG-CAD004B
              MOVE GS-CODIGO-NOVO     TO COD-USUARIO-CA004B
              START CAD004B KEY IS NOT LESS CHAVE-CA004B INVALID KEY
                   MOVE "10" TO ST-CAD004B
              END-START

              PERFORM UNTIL ST-CAD004B = "10"
                   READ CAD004B NEXT AT END
                        MOVE "10" TO ST-CAD004B
                   NOT AT END
                        IF GS-CODIGO-NOVO <> COD-USUARIO-CA004B
                           MOVE "10" TO ST-CAD004B
                        ELSE
                           DELETE CAD004B
                           END-DELETE
                        END-IF
                   END-READ
              END-PERFORM

              MOVE SPACES     TO GS-LINDET
              MOVE 1          TO GS-LINHA
              MOVE "LER-LIST" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM

              PERFORM UNTIL GS-LINDET = SPACES
                  IF GS-LINDET(57: 3) = "SIM"
                     MOVE GS-CODIGO-NOVO      TO COD-USUARIO-CA004B
                     MOVE GS-LINDET(1: 8)     TO PROGRAMA-CA004B
                     WRITE REG-CAD004B
                  END-IF
                  MOVE SPACES TO GS-LINDET
                  ADD 1       TO GS-LINHA
                  MOVE "LER-LIST" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-PERFORM

              CLOSE    CAD004B
           ELSE
              INITIALIZE REG-CAD004
              MOVE GS-CODIGO-NOVO     TO COD-USUARIO-CA004
              START CAD004 KEY IS NOT LESS CHAVE-CA004 INVALID KEY
                   MOVE "10" TO ST-CAD004
              END-START

              PERFORM UNTIL ST-CAD004 = "10"
                   READ CAD004 NEXT AT END
                        MOVE "10" TO ST-CAD004
                   NOT AT END
                        IF GS-CODIGO-NOVO <> COD-USUARIO-CA004
                           MOVE "10" TO ST-CAD004
                        ELSE
                           DELETE CAD004
                           END-DELETE
                        END-IF
                   END-READ
              END-PERFORM

              MOVE SPACES     TO GS-LINDET
              MOVE 1          TO GS-LINHA
              MOVE "LER-LIST" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM

              PERFORM UNTIL GS-LINDET = SPACES
                  IF GS-LINDET(57: 3) = "SIM"
                     MOVE GS-CODIGO-NOVO      TO COD-USUARIO-CA004
                     MOVE GS-LINDET(1: 8)     TO PROGRAMA-CA004
                     WRITE REG-CAD004
                  END-IF
                  MOVE SPACES TO GS-LINDET
                  ADD 1       TO GS-LINHA
                  MOVE "LER-LIST" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-PERFORM.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       ITEM-SELECIONADO SECTION.
           MOVE GS-CODIGO         TO COD-USUARIO-CA004.
           MOVE GS-LINDET(1: 8)   TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                WRITE REG-CAD004
                MOVE "SIM" TO GS-LINDET(57: 3)
            NOT INVALID KEY
                MOVE "NÃO" TO GS-LINDET(57: 3)
                DELETE CAD004
           END-READ.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO PROGRAMA-CA003.
           START CAD003 KEY IS NOT < PROGRAMA-CA003 INVALID KEY
                 MOVE "10" TO ST-CAD004.
           PERFORM UNTIL ST-CAD003 = "10"
              READ CAD003 NEXT RECORD AT END MOVE "10" TO ST-CAD003
              NOT AT END
                MOVE PROGRAMA-CA003  TO GS-LINDET(1: 8)
                MOVE DESCRICAO-CA003 TO GS-LINDET(14: 31)
                MOVE GS-CODIGO       TO COD-USUARIO-CA004
                MOVE PROGRAMA-CA003  TO PROGRAMA-CA004
                READ CAD004 INVALID KEY MOVE "NÃO" TO GS-LINDET(57: 3)
                     NOT INVALID KEY MOVE "SIM" TO GS-LINDET(57: 3)
                END-READ
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CAP004" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE ZEROS TO PROGRAMA-CA003.
           START CAD003 KEY IS NOT < PROGRAMA-CA003 INVALID KEY
                 MOVE "10" TO ST-CAD004.
           PERFORM UNTIL ST-CAD003 = "10"
              READ CAD003 NEXT RECORD AT END
                   MOVE "10" TO ST-CAD003
              NOT AT END
                MOVE PROGRAMA-CA003  TO LINDET-REL(1: 8)
                MOVE DESCRICAO-CA003 TO LINDET-REL(14: 31)
                MOVE GS-CODIGO       TO COD-USUARIO-CA004
                MOVE PROGRAMA-CA003  TO PROGRAMA-CA004
                READ CAD004 INVALID KEY MOVE "NAO" TO LINDET-REL(57: 3)
                     NOT INVALID KEY MOVE "SIM" TO LINDET-REL(57: 3)
                END-READ
                WRITE REG-RELAT FROM LINDET
                END-WRITE
                ADD 1 TO LIN
                IF LIN > 56 PERFORM CABECALHO
                END-IF
              END-READ
           END-PERFORM.

           copy descondensa.



       TOTALIZA-REL SECTION.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT.
           ADD 1 TO LIN.
       CABECALHO SECTION.
           ADD 1 TO PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
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
           CLOSE CAD002 CAD003 CAD004 CAD002E CAD001 CAD002D.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

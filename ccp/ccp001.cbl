       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCP001.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 31/03/1999
      *DESCRIÇÃO: Cadastro de pessoas que poderão ter acesso aos
      *           relatórios de contas correntes - por tipo-lcto
      *           Dependendo o tipo da conta não poderá ter acesso que
      *           são: 1-funcionário   2-Vendedor    3-Representante
      *                4-reportagem    5-PES-FISICA/JUR
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX002.
           COPY CCPX001.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW002.
       COPY CCPW001.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CCP001.CPB".
           COPY "CCP001.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETRO          PIC X(40)    VALUE SPACES.
       01  VARIAVEIS.
           05  ST-CAD002             PIC XX       VALUE SPACES.
           05  ST-CCD001             PIC XX       VALUE SPACES.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  MENSAGEM              PIC X(200)   VALUE SPACES.
           05  TIPO-MSG              PIC X(01)    VALUE SPACES.
           05  RESP-MSG              PIC X(01)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(63)   VALUE
           "RELACAO DE PESSOAS C/ PERMISSÃO P/ ACESSO CTA-CORRENTE".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "NOME-REDUZ   TIPO-ACESSO".

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       LINKAGE SECTION.
       77  POP-UP                  PIC X(30).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           OPEN I-O   CCD001
           CLOSE      CCD001
           OPEN INPUT CCD001
           OPEN INPUT CAD002
           IF ST-CCD001 = "35"
              CLOSE CCD001      OPEN OUTPUT CCD001
              CLOSE CCD001      OPEN I-O CCD001
           END-IF.
           IF ST-CCD001 <> "00"
              MOVE "ERRO ABERTURA CCD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CCD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD002 <> "00"
              MOVE "ERRO ABERTURA CAD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                    PERFORM SALVAR-DADOS
                    PERFORM LIMPAR-DADOS
                    PERFORM CARREGA-ULTIMOS
                    MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                    PERFORM EXCLUI-RECORD
                    PERFORM LIMPAR-DADOS
                    PERFORM CARREGA-ULTIMOS
                    MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
               WHEN GS-CLR-FLG-TRUE
                    PERFORM LIMPAR-DADOS
               WHEN GS-LE-USUARIO-TRUE
                    PERFORM LER-USUARIO
               WHEN GS-POP-UP-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CARREGA-LIST-BOX-TRUE
                    PERFORM CARREGAR-LIST-BOX
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR" TO DS-PROCEDURE
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-CCD001
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CARREGAR-LIST-BOX SECTION.
           MOVE GS-LINDET(1: 10) TO GS-NOME-REDUZ NOME-REDUZ-CA002
           READ CAD002 KEY IS NOME-REDUZ-CA002 INVALID KEY
               MOVE 0 TO GS-CODIGO
           NOT INVALID KEY
               MOVE CODIGO-CA002 TO GS-CODIGO
           END-READ
           MOVE GS-LINDET(11:20) TO GS-TIPO-ACESSO.

       EXCLUI-RECORD SECTION.
           CLOSE    CCD001
           OPEN I-O CCD001
           MOVE GS-NOME-REDUZ        TO NOME-REDUZ-CC01.
           MOVE FUNCTION NUMVAL(GS-TIPO-ACESSO(1: 2))
                                     TO TIPO-ACESSO-CC01.
           READ CCD001 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE CCD001
                END-DELETE
                PERFORM LIMPAR-DADOS.
           CLOSE      CCD001
           OPEN INPUT CCD001.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".

       SALVAR-DADOS SECTION.
           CLOSE    CCD001
           OPEN I-O CCD001

           MOVE GS-NOME-REDUZ        TO NOME-REDUZ-CC01.
           MOVE GS-TIPO-ACESSO(1: 2) TO TIPO-ACESSO-CC01.
           WRITE REG-CCD001 INVALID KEY CONTINUE.

           CLOSE      CCD001
           OPEN INPUT CCD001.

       LER-USUARIO SECTION.
           MOVE GS-CODIGO        TO CODIGO-CA002.
           READ CAD002 INVALID KEY
                MOVE SPACES      TO NOME-REDUZ-CA002.
           MOVE NOME-REDUZ-CA002 TO GS-NOME-REDUZ.
       CHAMAR-POPUP SECTION.
           CALL   "CAP002T" USING PARAMETROS-W PASSAR-PARAMETRO.
           CANCEL "CAP002T".
           MOVE PASSAR-PARAMETRO(38: 3)TO GS-CODIGO.
           PERFORM LER-USUARIO.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-CCD001       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           INITIALIZE REG-CCD001

           START CCD001 KEY IS NOT < CHAVE-CC01 INVALID KEY
                 MOVE "10" TO ST-CCD001.

           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS  TO GS-CONT.
           PERFORM UNTIL ST-CCD001 = "10"
              READ CCD001 NEXT RECORD AT END
                   MOVE "10" TO ST-CCD001
              NOT AT END
                   ADD 1 TO GS-CONT
                   MOVE SPACES TO GS-LINDET
                   MOVE NOME-REDUZ-CC01   TO GS-LINDET(1: 10)
                   EVALUATE TIPO-ACESSO-CC01
                     WHEN 1
                          MOVE "01-PESSOA FÍSICA"   TO GS-LINDET(11: 20)
                     WHEN 2
                          MOVE "02-PESSOA JURÍDICA" TO GS-LINDET(11: 20)
                     WHEN 3
                          MOVE "03-FUNCIONÁRIO"     TO GS-LINDET(11: 20)
                     WHEN 4
                          MOVE "04-REPRESENTANTE"   TO GS-LINDET(11: 20)
                     WHEN 5
                          MOVE "05-FOTÓGRAFO"       TO GS-LINDET(11: 20)
                     WHEN 6
                          MOVE "06-CINEGRAFISTA"    TO GS-LINDET(11: 20)
                     WHEN 7
                          MOVE "07-VENDEDOR"        TO GS-LINDET(11: 20)
                     WHEN 8
                          MOVE "08-IMPOSTO"         TO GS-LINDET(11: 20)
                     WHEN 9
                          MOVE "09-PENDENCIA"       TO GS-LINDET(11: 20)
                     WHEN 0
                          MOVE "00-OUTROS"          TO GS-LINDET(11: 20)
                     WHEN 10
                          MOVE "10-TERCEIRIZADO"    TO GS-LINDET(11: 20)
                     WHEN 11
                          MOVE "11-FRANQUIA"        TO GS-LINDET(11: 20)
                   END-EVALUATE
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
           MOVE "CCP001" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.

           INITIALIZE REG-CCD001
           START CCD001 KEY IS NOT < CHAVE-CC01 INVALID KEY
                 MOVE "10" TO ST-CCD001.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           PERFORM UNTIL ST-CCD001 = "10"
               READ CCD001 NEXT RECORD AT END
                    MOVE "10" TO ST-CCD001
               NOT AT END
                    MOVE SPACES TO LINDET-REL
                    MOVE NOME-REDUZ-CC01       TO LINDET-REL(01: 13)
                    EVALUATE TIPO-ACESSO-CC01
                       WHEN 1
                         MOVE "01-PESSOA FÍSICA"   TO LINDET-REL(11: 20)
                       WHEN 2
                         MOVE "02-PESSOA JURÍDICA" TO LINDET-REL(11: 20)
                       WHEN 3
                         MOVE "03-FUNCIONÁRIO"     TO LINDET-REL(11: 20)
                       WHEN 4
                         MOVE "04-REPRESENTANTE"   TO LINDET-REL(11: 20)
                       WHEN 5
                         MOVE "05-FOTÓGRAFO"       TO LINDET-REL(11: 20)
                       WHEN 6
                         MOVE "06-CINEGRAFISTA"    TO LINDET-REL(11: 20)
                       WHEN 7
                         MOVE "07-VENDEDOR"        TO LINDET-REL(11: 20)
                       WHEN 8
                         MOVE "08-IMPOSTO"         TO LINDET-REL(11: 20)
                       WHEN 9
                         MOVE "09-PENDENCIA"       TO LINDET-REL(11: 20)
                       WHEN 0
                         MOVE "00-OUTROS"          TO LINDET-REL(11: 20)
                       WHEN 10
                         MOVE "10-TERCEIRIZADO"    TO LINDET-REL(11: 20)
                       WHEN 11
                         MOVE "11-FRANQUIA"        TO LINDET-REL(11: 20)
                    END-EVALUATE
                    WRITE REG-RELAT FROM LINDET
                    ADD 1 TO LIN
                    IF LIN > 56
                       PERFORM CABECALHO
                    END-IF
               END-READ
           END-PERFORM.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
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
           CLOSE CCD001 CAD002.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

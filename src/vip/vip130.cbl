       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIP130.
      *AUTORA: MARELI AMANCIO VOLPATO
      *DATA: 08/08/2000
      *DESCRIÇÃO: REMESSA DE FITAS BRUTAS - DO SETOR QUALIDADE
      *                                     P/ SETOR VIDEO PRODUÇÃO
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY COPX003.
           COPY COPX060.
           COPY VIPX100.
           COPY VIPX130.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY COPW003.
       COPY COPW060.
       COPY VIPW100.
       COPY VIPW130.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).
       WORKING-STORAGE SECTION.
           COPY "VIP130.CPB".
           COPY "VIP130.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-VID100             PIC XX       VALUE SPACES.
           05  ST-VID130             PIC XX       VALUE SPACES.
           05  ULT-CODIGO            PIC 9(2)     VALUE ZEROS.
      *    Ult-codigo - será utilizado p/ encontrar o último código
      *    de CODIGO-V20 utilizado
           05  GRAVA-W               PIC 9        VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  ORDEM-W               PIC 9        VALUE ZEROS.
      *    ordem-w - flag que controla a ordem do relatorio - numérico
      *    ou alfabético
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  ULT-CONTRATO          PIC 9(4)     VALUE ZEROS.
           05  ULT-DATAREALIZA       PIC 9(8)     VALUE ZEROS.
           05  DATA-ANTERIOR         PIC 9(8)     VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  DATA-W                PIC 9(8)     VALUE ZEROS.
           05  LOTE-W                PIC 9(2)     VALUE ZEROS.
           05  IDENTIFICADOR-WK      PIC 9(9)     VALUE ZEROS.
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
           "RELACAO DE REMESSA DE FITAS BRUTAS ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "NR.FITA CURSO      EVENTO               CINEGRAFISTA    LOCA
      -    "LIZ".
       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

           copy impressora.
       LINKAGE SECTION.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           accept parametros-w from command-line.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO.
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD060" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "VID100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID100.
           MOVE "VID130" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID130.
           OPEN I-O VID130.
           OPEN INPUT CGD001 COD003 COD060 VID100.
           IF ST-VID130 = "35"
              CLOSE VID130      OPEN OUTPUT VID130
              CLOSE VID130      OPEN I-O VID130
           END-IF.
           IF ST-VID100 <> "00"
              MOVE "ERRO ABERTURA VID100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID130 <> "00"
              MOVE "ERRO ABERTURA VID130: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID130 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              MOVE 1 TO GS-ORDER
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM CARREGA-ULTIMOS
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI-RECORD
                   PERFORM CARREGA-ULTIMOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(01:09) TO GS-IDENTIFICADOR
                   MOVE GS-LINDET(25:21) TO GS-EVENTO
               WHEN GS-TRANSF-LOCALIZ-TRUE
                    PERFORM TRANSFERE-LOCALIZACAO
                    PERFORM CARREGA-ULTIMOS
               WHEN GS-LER-EVENTO-TRUE
                    PERFORM LER-EVENTO
               WHEN GS-LER-LOCALIZACAO-TRUE
                    PERFORM LER-LOCALIZACAO
               WHEN GS-LER-LOTE-TRUE
                    PERFORM LER-LOTE
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


       LER-LOTE SECTION.
           IF GS-LOTE = 0
              INITIALIZE REG-VID130
              MOVE GS-DATA          TO DATA-INV
              CALL "GRIDAT2" USING DATA-INV
              MOVE DATA-INV         TO DATA-V130
              MOVE ALL "9"          TO LOTE-V130
              START VID130 KEY IS LESS THAN CHAVE-V130 NOT INVALID KEY
                   READ VID130 NEXT NOT AT END
                        IF DATA-INV = DATA-V130
                           MOVE LOTE-V130 TO GS-LOTE
                        END-IF
                   END-READ
              END-START
              ADD 1 TO GS-LOTE.

      *CARREGAR-DADOS SECTION.
      *    MOVE GS-DATA   TO DATA-V130.
      *    MOVE GS-NR-FITA      TO NR-FITA-V130.

       LER-LOCALIZACAO SECTION.
           INITIALIZE REG-VID130
           MOVE GS-DATA          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV         TO DATA-V130
           MOVE GS-LOTE          TO LOTE-V130
           MOVE ZEROS            TO NR-FITAS-V130
           MOVE ZEROS            TO GS-CONT

           START VID130 KEY IS NOT < CHAVE-V130 INVALID KEY
              MOVE "10" TO ST-VID130.

           PERFORM UNTIL ST-VID130 = "10"
              READ VID130 NEXT RECORD AT END
                   MOVE "10" TO ST-VID130
              NOT AT END
                   IF DATA-V130 <> DATA-INV  OR
                      LOTE-V130 <> GS-LOTE
                      MOVE "10" TO ST-VID130
                   ELSE
                      INITIALIZE REG-VID100
                      MOVE NR-FITAS-V130       TO NR-FITAS-V100
                      MOVE ZEROS               TO DATA-EVENTO-V100
                      START VID100 KEY IS NOT < ALT-V100 INVALID KEY
                            CONTINUE
                      NOT INVALID KEY
                            READ VID100 NEXT RECORD
                            END-READ
                            IF NR-FITAS-V130 <> NR-FITAS-V100
                               MOVE "AVAL" TO GS-LOCAL-ATUAL
                            ELSE
                               MOVE LOCALIZACAO-V100 TO GS-LOCAL-ATUAL
                            END-IF
                      END-START
                   END-IF
              END-READ
           END-PERFORM.


       LER-EVENTO SECTION.
           INITIALIZE REG-VID100
           MOVE GS-IDENTIFICADOR  TO IDENTIFICADOR-V100
           START VID100 KEY IS NOT < ALT4-V100 INVALID KEY
                 MOVE "NAO CADASTRADA1" TO GS-EVENTO
           NOT INVALID KEY
                 READ VID100 NEXT AT END
                      MOVE "NAO CADASTRADA2"  TO GS-EVENTO
                 NOT AT END
                      IF IDENTIFICADOR-V100  <> GS-IDENTIFICADOR
                         MOVE "NAO CADASTRADA3" TO GS-EVENTO
                      ELSE
                         MOVE NR-FITAS-V100     TO GS-NR-FITA
                         MOVE EVENTO-V100       TO CODIGO-CO03
                         READ COD003 INVALID KEY
                              MOVE SPACES       TO NOME-CO03
                         END-READ
                         MOVE NOME-CO03         TO GS-EVENTO
                         MOVE LOCALIZACAO-V100  TO GS-LOCAL-ATUAL.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-VID130
           MOVE GS-DATA TO DATA-W
           MOVE GS-LOTE TO LOTE-W
           INITIALIZE GS-DATA-BLOCK
           MOVE DATA-W TO GS-DATA
           MOVE LOTE-W TO GS-LOTE
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       EXCLUI-RECORD SECTION.
           MOVE GS-DATA     TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV    TO DATA-V130.

           MOVE GS-LOTE     TO LOTE-V130

           INITIALIZE REG-VID100
                      GS-NR-FITA
           MOVE GS-IDENTIFICADOR  TO IDENTIFICADOR-V100
           START VID100 KEY IS NOT < ALT4-V100 INVALID KEY
                 MOVE "NAO CADASTRADA1" TO GS-EVENTO
           NOT INVALID KEY
                 READ VID100 NEXT AT END
                      MOVE "NAO CADASTRADA2"  TO GS-EVENTO
                 NOT AT END
                      IF IDENTIFICADOR-V100  <> GS-IDENTIFICADOR
                         MOVE "NAO CADASTRADA3" TO GS-EVENTO
                      ELSE
                         MOVE NR-FITAS-V100     TO GS-NR-FITA.

           MOVE GS-NR-FITA  TO NR-FITAS-V130.
           READ VID130 INVALID KEY
                MOVE SPACES TO MENSAGEM
                STRING "Registro Não Encontrado" X"0DA0"
                       "DATA-V130 = " DATA-V130 X"0DA0"
                       "NR-FITAS-V130 = " NR-FITAS-V130
                  INTO MENSAGEM
                MOVE "C"                       TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                DELETE VID130.
           PERFORM LIMPAR-DADOS.
           MOVE 1 TO GRAVA-W.
       SALVAR-DADOS SECTION.
           MOVE GS-DATA         TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV        TO DATA-V130
           MOVE GS-LOTE         TO LOTE-V130
           MOVE GS-NR-FITA      TO NR-FITAS-V130.
           WRITE REG-VID130 INVALID KEY
                 CONTINUE.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           MOVE ST-VID130       TO GS-MENSAGEM-ERRO(23: 2)
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-DATA          TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV         TO DATA-V130
           MOVE GS-LOTE          TO LOTE-V130
           MOVE ZEROS            TO NR-FITAS-V130
           MOVE ZEROS            TO GS-CONT

           START VID130 KEY IS NOT < CHAVE-V130 INVALID KEY
              MOVE "10" TO ST-VID130.
           PERFORM UNTIL ST-VID130 = "10"
              READ VID130 NEXT RECORD AT END
                   MOVE "10" TO ST-VID130
              NOT AT END
                   IF DATA-V130 <> DATA-INV OR
                      LOTE-V130 <> GS-LOTE
                      MOVE "10" TO ST-VID130
                   ELSE
                      ADD 1                    TO GS-CONT
                      MOVE SPACES              TO GS-LINDET
                      MOVE NR-FITAS-V130       TO NR-FITAS-V100
                      MOVE ZEROS               TO DATA-EVENTO-V100
                      START VID100 KEY IS NOT < ALT-V100 INVALID KEY
                            CONTINUE
                      NOT INVALID KEY
                            READ VID100 NEXT RECORD
                            END-READ
                            IF NR-FITAS-V130 <> NR-FITAS-V100
                               MOVE "AVAL" TO GS-LINDET(61: 5)
                            ELSE
                               MOVE IDENTIFICADOR-V100 TO
                                                       GS-LINDET(01:09)
                               MOVE CURSO-V100      TO GS-LINDET(14:11)
                               MOVE EVENTO-V100     TO CODIGO-CO03
                               READ COD003 INVALID KEY
                                    MOVE SPACES     TO NOME-CO03
                               END-READ
                               MOVE NOME-CO03       TO GS-LINDET(25: 21)
                               MOVE CINEGRAFISTA-V100 TO CODIGO-CG01
                               READ CGD001 INVALID KEY
                                    MOVE SPACES       TO NOME-CG01
                               END-READ
                               MOVE NOME-CG01       TO GS-LINDET(46: 15)
                               MOVE LOCALIZACAO-V100 TO GS-LINDET(67: 5)
                            END-IF
                      END-START
                      MOVE "INSERE-LIST" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
              END-READ
           END-PERFORM.

       TRANSFERE-LOCALIZACAO SECTION.
      *    um nr-fita poderá conter mais de um contrato, sendo assim
      *    dentro destes contratos deverá ser considerado p/ transfere-
      *    rencia aquele que tiver a maior data-realizacao no evento

           CLOSE    VID100
           OPEN I-O VID100

           MOVE GS-DATA               TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV              TO DATA-V130
           MOVE GS-LOTE               TO LOTE-V130

           MOVE ZEROS                 TO CONTRATO-V130
           MOVE ZEROS                 TO NR-FITA-V130
           MOVE ZEROS                 TO ULT-CONTRATO
                                         ULT-DATAREALIZA
                                         DATA-ANTERIOR

           START VID130 KEY IS NOT < CHAVE-V130 INVALID KEY
                 MOVE "10" TO ST-VID130.
           PERFORM UNTIL ST-VID130 = "10"
                 READ VID130 NEXT RECORD AT END
                      MOVE "10" TO ST-VID130
                 NOT AT END
                      IF DATA-V130 <> DATA-INV OR
                         LOTE-V130 <> GS-LOTE
                         MOVE "10" TO ST-VID130
                      ELSE
                         INITIALIZE REG-VID100
                         MOVE NR-FITAS-V130  TO NR-FITAS-V100
                         MOVE ZEROS          TO DATA-EVENTO-V100
                         START VID100 KEY IS NOT < ALT-V100 INVALID KEY
                              MOVE "10" TO ST-VID100
                         END-START
                         READ VID100 NEXT RECORD
                         END-READ
                         IF NR-FITAS-V130 = NR-FITAS-V100
                            MOVE IDENTIFICADOR-V100 TO IDENTIFICADOR-WK
                         ELSE
                            MOVE 0                  TO IDENTIFICADOR-WK
                         END-IF
                         INITIALIZE REG-VID100
                         MOVE IDENTIFICADOR-WK TO IDENTIFICADOR-V100
                         MOVE ZEROS            TO DATA-EVENTO-V100
                         START VID100 KEY IS NOT < ALT4-V100 INVALID KEY
                              MOVE "10" TO ST-VID100
                         END-START
      *                  le a partir do vid100 p/ verificar a maior data
      *                  realiz dentro dos contratos c/ mesmo nr de fita
                         MOVE ZEROS TO ULT-CONTRATO DATA-ANTERIOR
                         PERFORM UNTIL ST-VID100 = "10"
                            READ VID100 NEXT RECORD AT END
                                 MOVE "10" TO ST-VID100
                             NOT AT END
                                 IF IDENTIFICADOR-V100 <>
                                    IDENTIFICADOR-WK
                                    MOVE "10" TO ST-VID100
                                 ELSE
                                    MOVE GS-LOCAL-TRANSF
                                                     TO LOCALIZACAO-V100
                                    REWRITE REG-VID100
                                    END-REWRITE
                                 END-IF
                            END-READ
                         END-PERFORM
                      END-IF
                 END-READ
           END-PERFORM

           CLOSE      VID100
           OPEN INPUT VID100.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE ds-push-set TO DS-CONTROL
           MOVE "VIP130" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO PAG-W.

           MOVE GS-DATA         TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV        TO DATA-V130
           MOVE GS-LOTE         TO LOTE-V130
           MOVE ZEROS           TO NR-FITAS-V130
           START VID130 KEY IS NOT < CHAVE-V130 INVALID KEY
                 MOVE "10" TO ST-VID130.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           PERFORM UNTIL ST-VID130 = "10"
               READ VID130 NEXT RECORD AT END
                    MOVE "10" TO ST-VID130
               NOT AT END
                    IF DATA-INV <> DATA-V130 OR
                       GS-LOTE  <> LOTE-V130
                       MOVE "10" TO ST-VID130
                    ELSE
                       MOVE SPACES TO LINDET-REL
                       MOVE NR-FITAS-V130       TO NR-FITAS-V100
                       MOVE ZEROS               TO DATA-EVENTO-V100
                       START VID100 KEY IS NOT < ALT-V100 INVALID KEY
                             CONTINUE
                       NOT INVALID KEY
                             READ VID100 NEXT RECORD

                             END-READ
                             IF NR-FITAS-V100 <> NR-FITAS-V130
                                CONTINUE
                             ELSE
                               MOVE IDENTIFICADOR-V100
                                 TO LINDET-REL(1:9)
                               MOVE CURSO-V100
                                 TO LINDET-REL(14: 11)
                               MOVE EVENTO-V100
                                 TO CODIGO-CO03
                               READ COD003 INVALID KEY
                                    MOVE SPACES TO NOME-CO03
                               END-READ
                               MOVE NOME-CO03
                                 TO LINDET-REL(25: 21)
                               MOVE CINEGRAFISTA-V100
                                 TO CODIGO-CG01
                               READ CGD001 INVALID KEY
                                    MOVE SPACES TO NOME-CG01
                               END-READ
                               MOVE NOME-CG01
                                 TO LINDET-REL(46:15)
                               MOVE LOCALIZACAO-V100
                                 TO LINDET-REL(61: 5)
                             END-IF
                       END-START
                       WRITE REG-RELAT FROM LINDET
                       ADD 1 TO LIN
                       IF LIN > 56
                          PERFORM CABECALHO
                       END-IF
                    END-IF
               END-READ
           END-PERFORM.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
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
           CLOSE COD003 COD060 CGD001 VID130 VID100.
           move ds-quit-set to ds-control
           perform call-dialog-system.
           EXIT PROGRAM.

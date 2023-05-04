       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIP118.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 20/08/2000.
      *FUN플O: CADASTRO DE OBS - P/ DEPTO VENDAS E QUALIDADE GERAL

       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY COPX040.
           COPY VIPX118.
           COPY VIPX119.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY COPW040.
       COPY VIPW118.
       COPY VIPW119.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "VIP118.CPB".
           COPY "VIP118.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-VID118             PIC XX       VALUE SPACES.
           05  ST-VID119             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  AVALIACAO-W           PIC X        VALUE SPACES.
           05  OBS-W                 PIC X(100)   VALUE SPACES.
      *    CONTADORES P/ OBS
           05  I                     PIC 9(3)     VALUE ZEROS.
           05  J                     PIC 9(3)     VALUE ZEROS.
           05  K                     PIC 9(3)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
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
           "PLANEJAMENTO E RELATORIO DE VIDEO EDICAO".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       02  LINHA-01                          PIC  X(080) VALUE ALL "_".
       02  LINHA-02.
           05 FILLER                         PIC  X(014) VALUE
              "|Nr.Contrato: ".
           05 NR-CONTRATO-REL                PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(009) VALUE
              "   Item: ".
           05 ITEM-REL                       PIC  Z(002) VALUE ZEROS.
           05 FILLER                         PIC  X(004) VALUE SPACES.
           05 CURSO-REL                      PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 CIDADE-REL                     PIC  X(014) VALUE SPACES.
           05 FILLER                         PIC  X(010) VALUE
              "         |".
       02  LINHA-03.
           05 FILLER                         PIC  X(055) VALUE
              "|______________________________________________________".
           05 FILLER                         PIC  X(025) VALUE
              "________________________|".
       02  LINHA-04.
           05 FILLER                         PIC  X(055) VALUE
              "|              OBSERVACOES PARA DEPARTAMENTO DE VENDAS ".
           05 FILLER                         PIC  X(025) VALUE
              "                        |".
       02  LINHA-05.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 OBS-VENDAS-REL                 PIC  X(078) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-06.
           05 FILLER                         PIC  X(055) VALUE
              "|                OBSERVACOES DO REVISOR SOBRE QUALIDADE".
           05 FILLER                         PIC  X(025) VALUE
              " GERAL                  |".
       02  LINHA-07.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 OBS-QG-REL                     PIC  X(078) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".

           copy impressora.

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "VID118" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID118.
           MOVE "VID119" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID119.
           OPEN I-O VID118 VID119.
           OPEN INPUT CAD010 COD040.
           IF ST-VID118 = "35"
              CLOSE VID118      OPEN OUTPUT VID118
              CLOSE VID118      OPEN I-O VID118
           END-IF.
           IF ST-VID119 = "35"
              CLOSE VID119      OPEN OUTPUT VID119
              CLOSE VID119      OPEN I-O VID119
           END-IF.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID118 <> "00"
              MOVE "ERRO ABERTURA VID118: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID118 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID119 <> "00"
              MOVE "ERRO ABERTURA VID119: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID119 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   PERFORM LIMPAR-DADOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
               WHEN GS-VERIFICA-CONTRATO-TRUE
                   PERFORM VERIFICA-CONTRATO
               WHEN GS-LE-CONTRATO-TRUE
                   PERFORM LE-CONTRATO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

      *----------------------------------------------------------------
       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY INITIALIZE REG-COD040.
           MOVE IDENTIFICACAO-CO40 TO GS-CURSO.
           MOVE CIDADE-CO40        TO CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID           TO GS-CIDADE.
      *------------------------------------------------------
       VERIFICA-CONTRATO SECTION.
           PERFORM CARREGAR-OBS.

       CARREGAR-OBS SECTION.
           MOVE GS-CONTRATO           TO CONTRATO-V118
           MOVE GS-ITEM               TO ITEM-V118
           MOVE ZEROS                 TO TIPO-V118
           START VID118 KEY IS NOT < CHAVE-V118 INVALID KEY
              MOVE "10" TO ST-VID118.
           PERFORM UNTIL ST-VID118 = "10"
             READ VID118 NEXT RECORD AT END MOVE "10" TO ST-VID118
              NOT AT END
               IF CONTRATO-V118 <> GS-CONTRATO OR
                  ITEM-V118 <> GS-ITEM MOVE "10" TO ST-VID118
               ELSE
                 MOVE CONTRATO-V118  TO CONTRATO-V119
                 MOVE ITEM-V118      TO ITEM-V119
                 MOVE TIPO-V118      TO TIPO-V119
                 MOVE ZEROS          TO SEQ-V119
                 START VID119 KEY IS NOT < CHAVE-V119 INVALID KEY
                       MOVE "10" TO ST-VID119
                 END-START
                 MOVE 1 TO I J K
                 PERFORM UNTIL ST-VID119 = "10"
                   READ VID119 NEXT RECORD AT END MOVE "10" TO ST-VID119
                     NOT AT END
                       IF CONTRATO-V119 <> GS-CONTRATO OR
                          ITEM-V119 <> GS-ITEM MOVE "10" TO ST-VID119
                       ELSE
                        EVALUATE TIPO-V119
                          WHEN 1 MOVE OBS-V119 TO GS-OBS-VENDAS(J: 100)
                                 ADD 100 TO J
                          WHEN 2 MOVE OBS-V119 TO GS-OBS-QG(K: 100)
                                 ADD 100 TO K
                        END-EVALUATE
                       END-IF
                   END-READ
                 END-PERFORM
               END-IF
             END-READ
           END-PERFORM.
      *----------------------------------------------------------
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-VID118 REG-VID119.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
      *SALVAR, EXCLUIR CONTRATO

       SALVAR-DADOS SECTION.
           PERFORM EXCLUIR-DADOS-CONTRATO.


      *   GRAVAR OBS DE VENDAS -CABECALHO
           MOVE GS-CONTRATO        TO CONTRATO-V118
           MOVE GS-ITEM            TO ITEM-V118
           MOVE 1                  TO TIPO-V118
           MOVE DATA-MOVTO-W         TO DATA-V118
           ACCEPT HORARIO-V118 FROM TIME.
           MOVE USUARIO-W          TO USUARIO-V118
           WRITE REG-VID118.
      *   GRAVAR OBS E INSTRU합ES -EDICAO

           MOVE GS-CONTRATO        TO CONTRATO-V119
           MOVE GS-ITEM            TO ITEM-V119
           MOVE 1                  TO TIPO-V119
           MOVE ZEROS TO SEQ-V119
           PERFORM VARYING I FROM 1 BY 100 UNTIL I > 500
             MOVE GS-OBS-VENDAS(I: 100) TO OBS-W
             IF OBS-W = SPACES MOVE 500 TO I
             ELSE MOVE OBS-W       TO OBS-V119
                  ADD 1            TO SEQ-V119
                  WRITE REG-VID119
                  END-WRITE
             END-IF
           END-PERFORM.
      *   GRAVAR OBS QUALIDADE GERAL- CABECALHO
           MOVE GS-CONTRATO        TO CONTRATO-V118
           MOVE GS-ITEM            TO ITEM-V118
           MOVE 2                  TO TIPO-V118
           MOVE DATA-MOVTO-W         TO DATA-V118
           ACCEPT HORARIO-V118 FROM TIME.
           MOVE USUARIO-W          TO USUARIO-V118
           WRITE REG-VID118.
      *   GRAVAR OBS E INSTRU합ES -COPIA

           MOVE GS-CONTRATO        TO CONTRATO-V119
           MOVE GS-ITEM            TO ITEM-V119
           MOVE 2                  TO TIPO-V119
           MOVE ZEROS TO SEQ-V119
           PERFORM VARYING J FROM J BY 100 UNTIL J > 500
             MOVE GS-OBS-QG(J: 100) TO OBS-W
             IF OBS-W = SPACES MOVE 500 TO J
             ELSE MOVE OBS-W       TO OBS-V119
                  ADD 1            TO SEQ-V119
                  WRITE REG-VID119
                  END-WRITE
             END-IF
           END-PERFORM.


       EXCLUIR-DADOS-CONTRATO SECTION.
           MOVE GS-CONTRATO   TO CONTRATO-V118
           MOVE GS-ITEM       TO ITEM-V118
           MOVE ZEROS         TO TIPO-V118.
           START VID118 KEY IS NOT < CHAVE-V118 INVALID KEY
              MOVE "10" TO ST-VID118.
           PERFORM UNTIL ST-VID118 = "10"
             READ VID118 NEXT RECORD AT END MOVE "10" TO ST-VID118
               NOT AT END
                 IF CONTRATO-V118 <> GS-CONTRATO OR
                    ITEM-V118 <> GS-ITEM MOVE "10" TO ST-VID118
                 ELSE
                   DELETE VID118
                 END-IF
             END-READ
           END-PERFORM.

           MOVE GS-CONTRATO   TO CONTRATO-V119
           MOVE GS-ITEM       TO ITEM-V119
           MOVE ZEROS         TO TIPO-V119 SEQ-V119.
           START VID119 KEY IS NOT < CHAVE-V119 INVALID KEY
              MOVE "10" TO ST-VID119.
           PERFORM UNTIL ST-VID119 = "10"
             READ VID119 NEXT RECORD AT END MOVE "10" TO ST-VID119
               NOT AT END
                 IF CONTRATO-V119 <> GS-CONTRATO OR
                    ITEM-V119 <> GS-ITEM MOVE "10" TO ST-VID119
                 ELSE
                   DELETE VID119
                 END-IF
             END-READ
           END-PERFORM.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVA플O" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

      *------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "VIP118" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.
           OPEN OUTPUT RELAT.
           PERFORM CABECALHO.
           PERFORM MOVER-DADOS-REL.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.

       MOVER-DADOS-REL SECTION.
           MOVE GS-CONTRATO        TO NR-CONTRATO-REL
           MOVE GS-ITEM            TO ITEM-REL
           MOVE GS-CURSO           TO CURSO-REL
           MOVE GS-CIDADE          TO CIDADE-REL
           WRITE REG-RELAT FROM LINHA-02
           WRITE REG-RELAT FROM LINHA-03.
           WRITE REG-RELAT FROM LINHA-04.
           ADD 3 TO LIN.

      *    OBSERVACOES P/ DEPTO DE VENDAS
           IF LIN > 56 PERFORM CABECALHO.
           MOVE SPACES TO OBS-W.
           PERFORM VARYING I FROM 1 BY 78 UNTIL I > 500
             MOVE GS-OBS-VENDAS(I: 78)      TO OBS-VENDAS-REL OBS-W
             IF OBS-W = SPACES MOVE 500 TO I
             ELSE WRITE REG-RELAT FROM LINHA-05
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
             END-IF
           END-PERFORM.
           WRITE REG-RELAT FROM LINHA-03
           ADD 1 TO LIN.

      *    OBSERVACOES P/ QUALIDADE GERAL
           IF LIN > 58 PERFORM CABECALHO.
           WRITE REG-RELAT FROM LINHA-06.
           WRITE REG-RELAT FROM LINHA-03.
           ADD 2 TO LIN.
           MOVE SPACES TO OBS-W.
           PERFORM VARYING I FROM 1 BY 78 UNTIL I > 500
             MOVE GS-OBS-QG(I: 78)      TO OBS-QG-REL OBS-W
             IF OBS-W = SPACES MOVE 500 TO I
             ELSE WRITE REG-RELAT FROM LINHA-07
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
             END-IF
           END-PERFORM.
           WRITE REG-RELAT FROM LINHA-03
           ADD 1 TO LIN.
       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM LINHA-01.
           MOVE 4 TO LIN.
      *-------------------------------------------------------------
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD040 VID118 VID119.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

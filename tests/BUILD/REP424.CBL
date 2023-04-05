       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP424.
      *DATA: 23/02/1999
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: Emite recibo de reportagem
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY REPX020.
           COPY REPX030.
           COPY REPX090.
           COPY REPX300.
           COPY REPX301.
           COPY REPX302.
           COPY REPX303.
           COPY CAPX010.
           COPY COPX040.
           COPY COPX003.
           SELECT WORK1 ASSIGN TO VARIA-W1
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK1
                  RECORD KEY IS CODIGO-WK1
                  ALTERNATE RECORD KEY IS NOME-WK1 WITH DUPLICATES.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY REPW020.
       COPY REPW030.
       COPY REPW090.
       COPY REPW300.
       COPY REPW301.
       COPY REPW302.
       COPY REPW303.
       COPY CAPW010.
       COPY COPW040.
       COPY COPW003.
       FD  WORK1.
       01  REG-WORK1.
           05  CODIGO-WK1           PIC 9(3).
           05  NOME-WK1             PIC X(30).
           05  VALOR-TOTAL-WK1      PIC 9(8)V99.
           05  VALOR-REP-WK1        PIC 9(8)V99.
           05  VALOR-LOC-WK1        PIC 9(8)V99.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPEXTE.CPY".
           COPY "REP424.CPB".
           COPY "REP424.CPY".
           COPY "DS-CNTRL.MF".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).

       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-RED020             PIC XX       VALUE SPACES.
           05  ST-RED030             PIC XX       VALUE SPACES.
           05  ST-RED090             PIC XX       VALUE SPACES.
           05  ST-RED300             PIC XX       VALUE SPACES.
           05  ST-RED301             PIC XX       VALUE SPACES.
           05  ST-RED302             PIC XX       VALUE SPACES.
           05  ST-RED303             PIC XX       VALUE SPACES.
           05  ST-WORK1              PIC XX       VALUE SPACES.
           05  VARIA-W1              PIC 9(8)     VALUE ZEROS.
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
           05  DATAI.
               10  ANO-I             PIC 9(4).
               10  MES-I             PIC 9(2).
               10  DIA-I             PIC 9(2).
           05  DATA-I REDEFINES DATAI PIC 9(8).
           05  DIAMES-W              PIC 9(4)     VALUE ZEROS.
           05  FLAG                  PIC 9        VALUE ZEROS.
           05  QT-EVENTO-W           PIC 9        VALUE ZEROS.
           05  QT-CONTRATO-W         PIC 9        VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PASSAR-PARAMETRO1.
               10  MESANO-REC        PIC 9(6)     VALUE ZEROS.
               10  DADOS-REC         PIC X(34)    VALUE SPACES.
           05  PASSAR-PARAMETRO REDEFINES PASSAR-PARAMETRO1 PIC X(40).

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

      *---------------------------------------------------------------
       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01                          PIC  X(080) VALUE ALL "Ä".
       02  LINHA-02.
           05 FILLER                         PIC  X(032) VALUE SPACES.
           05 FILLER                         PIC  X(018) VALUE
              "Kello Foto & Video".
       02  LINHA-03.
           05 FILLER                         PIC  X(027) VALUE SPACES.
           05 FILLER                         PIC  X(027) VALUE
              "3F EMPRESA FOTOGRAFICA LTDA".
       02  LINHA-04.
           05 FILLER                         PIC  X(017) VALUE SPACES.
           05 FILLER                         PIC  X(050) VALUE
              "Av.Getulio Vargas, 719 - 86770-000 - SANTA FE - PR".
       02  LINHA-05.
           05 FILLER                         PIC  X(023) VALUE SPACES.
           05 FILLER                         PIC  X(036) VALUE
              "79981940/0001-69    -    63300072-71".
       02  LINHA-06.
           05 FILLER                         PIC  X(049) VALUE SPACES.
           05 FILLER                         PIC  X(018) VALUE
              "VALOR TOTAL...R$: ".
           05 VALOR-TOTAL-REL                PIC  ZZ.ZZZ.ZZZ,ZZ.
       02  LINHA-07.
           05 FILLER                         PIC  X(013) VALUE SPACES.
           05 FILLER                         PIC  X(064) VALUE
              "DECLARO QUE RECEBI DA KELLO FOTO & VIDEO, O VALOR TOTAL A
      -        "CIMA DE".
           05 FILLER                         PIC  X(003) VALUE " DE".
       02  LINHA-08.
           05 FILLER                         PIC  X(001) VALUE "(".
           05 DESC-VALOR-REL                 PIC  X(078) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE ")".
       02  LINHA-09.
           05 FILLER                         PIC  X(004) VALUE "CORR".
           05 FILLER                         PIC  X(055) VALUE
              "ESPONDENTE A SERVICOS PROFISSIONAIS DE REPORTAGENS VIDE".
           05 FILLER                         PIC  X(021) VALUE
      -        "O FOTOGRAFICAS, DE CA".
       02  LINHA-10.
           05 FILLER                         PIC  X(027) VALUE
              "RACTER EVENTUAL, NO MES DE ".
           05 MESANO-REL                     PIC  99/9999.
           05 FILLER                         PIC  X      VALUE ".".
       02  LINHA-11.
           05 FILLER                         PIC  X(021) VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "PARA MAIOR CLAREZA FIRMO O PRESENTE RECIBO NA FORMA DA ".
           05 FILLER                         PIC  X(004) VALUE "LEI.".
       02  LINHA-12.
           05 FILLER                         PIC  X(043) VALUE SPACES.
           05 FILLER                         PIC  X(037) VALUE
              "SANTA FE(PR),___DE_____________DE___.".
       02  LINHA-13.
           05 FILLER                         PIC  X(043) VALUE SPACES.
           05 FILLER                         PIC  X(037) VALUE ALL "_".
       02  LINHA-14.
           05 FILLER                         PIC  X(047) VALUE SPACES.
           05 NOME-REL                       PIC  X(030) VALUE SPACES.
       02  LINHA-15.
           05 FILLER                         PIC  X(047) VALUE SPACES.
           05 FILLER                         PIC  X(007) VALUE "RG.NR.".
           05 RG-REL                         PIC  X(015) VALUE SPACES.
       02  LINHA-20.
           05 FILLER                         PIC  X(029) VALUE
              "RELACAO DE SERVICOS QUITADOS:".
       02  LINHA-21                          PIC  X(079) VALUE ALL "Ä".
       02  LINHA-22.
           05 FILLER                         PIC  X(001) VALUE "N".
           05 FILLER                         PIC  X(001) VALUE "R".
           05 FILLER                         PIC  X(055) VALUE
              ".REL DT.RP CONT CURSO      CIDADE     EVENTO      PART ".
           05 FILLER                         PIC  X(022) VALUE
              "F/F REP FUN      VALOR".
       02  LINHA-23.
           05 NR-RELAT-REL                   PIC  Z(006) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 DATA-REP-REL                   PIC  99/99.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CONTRATO-REL                   PIC  9(004) VALUE ZEROS.
           05 QT-CONT-REL                    PIC  X(001) VALUE SPACE.
           05 CURSO-REL                      PIC  X(010) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CIDADE-REL                     PIC  X(010) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 EVENTO-REL                     PIC  X(011) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 PARTICIPANTE-REL               PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FIL-FIT-REL                    PIC  ZZ9.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 QT-REPORT-REL                  PIC  Z(003) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FUNCAO-REL                     PIC  X(003) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 VALOR-REP-REL                  PIC  ZZZ.ZZZ,ZZ.
      *------------------------------------------------------------

           copy impressora.

       LINKAGE SECTION.
           COPY "PARAMETR".

       01  STRING-1               PIC X(65) VALUE SPACES.
       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT DATA6-W FROM DATE.
           ACCEPT HORA-BRA FROM TIME.
           MOVE DATA6-W TO DATA-INV(3: 6).
           MOVE DATA6-W(1: 2) TO ANO-V.
           IF ANO-V > 80 MOVE "19" TO DATA-INV(1: 2)
           ELSE MOVE "20" TO DATA-INV(1: 2).
           CALL "GRIDAT1" USING DATA-INV.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO      TO DS-VERSION-NO
           MOVE ZEROS              TO PAG-W ERRO-W.
           MOVE EMPRESA-W          TO EMP-REC.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003.
           MOVE 111 TO EMP-REC.
           MOVE "RED020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED020.
           MOVE "RED030" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED030.
           MOVE "RED090" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED090.
           MOVE "RED300" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED300.
           MOVE "RED301" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED301.
           MOVE "RED302" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED302.
           MOVE "RED303" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED303.
           OPEN INPUT CAD010 COD040 COD003 RED020 RED030 RED090
                      RED300 RED301 RED302 RED303.

           MOVE ZEROS TO ERRO-W.
           IF ST-CAD010 <> "00"  OR  ST-COD040 <> "00" OR
              ST-COD003 <> "00"  OR  ST-RED020 <> "00" OR
              ST-RED030 <> "00"  OR  ST-RED090 <> "00" OR
              ST-RED300 <> "00"  OR  ST-RED301 <> "00" OR
              ST-RED302 <> "00"  OR  ST-RED303 <> "00"
              MOVE 1 TO ERRO-W
              PERFORM FINALIZAR-PROGRAMA.
           IF ERRO-W = ZEROS
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-GERAR-TRUE
                    PERFORM DESENVOLVE
           END-EVALUATE.
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       DESENVOLVE SECTION.
           MOVE STRING-1          TO PASSAR-PARAMETRO.
           ACCEPT VARIA-W1 FROM TIME.
           OPEN OUTPUT WORK1.   CLOSE WORK1.   OPEN I-O WORK1.
           PERFORM LEITURA-RED300.
           copy impressora.chama.
           if lnk-mapeamento <> spaces
              PERFORM EMISSAO-PGTO.

       LEITURA-RED300 SECTION.
           MOVE MESANO-REC(1: 4) TO GS-MESANO(3: 4)
           MOVE MESANO-REC(5: 2) TO GS-MESANO(1: 2).
           MOVE MESANO-REC TO ANOMES-R300.
           START RED300 KEY IS NOT < ANOMES-R300 INVALID KEY
                 MOVE "10" TO ST-RED300.
           PERFORM UNTIL ST-RED300 = "10"
             READ RED300 NEXT RECORD AT END MOVE "10" TO ST-RED300
              NOT AT END
               IF ANOMES-R300 NOT = MESANO-REC MOVE "10" TO ST-RED300
               ELSE
                PERFORM LEITURA-RED303
               END-IF
             END-READ
           END-PERFORM.
       LEITURA-RED303 SECTION.
           MOVE "GRAVANDO..." TO GS-DESCRICAO(1: 12)
           MOVE DOCTO-R300 TO DOCTO-R303 GS-DESCRICAO(13: 6).
           MOVE "REFRESH-TELA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE ZEROS TO REPRES-R303 FUNCAO-R303.
           START RED303 KEY IS NOT < CHAVE-R303 INVALID KEY
                 MOVE "10" TO ST-RED303.
           PERFORM UNTIL ST-RED303 = "10"
              READ RED303 NEXT RECORD AT END MOVE "10" TO ST-RED303
                NOT AT END
                 IF DOCTO-R303 NOT = DOCTO-R300 MOVE "10" TO ST-RED303
                 ELSE
                  MOVE REPRES-R303      TO CODIGO-R030 CODIGO-WK1
                  READ RED030 INVALID KEY MOVE SPACES TO NOME-R030
                                          MOVE ZEROS  TO EMPRESA-R030
                  END-READ
                  IF EMPRESA-R030 NOT = ZEROS
                     MOVE EMPRESA-R030 TO CODIGO-R090
                     READ RED090 INVALID KEY CONTINUE
                      NOT INVALID KEY
                       MOVE CODIGO-REP-R090 TO CODIGO-WK1
                       MOVE NOME-R090       TO NOME-R030
                     END-READ
                  END-IF
                  MOVE NOME-R030        TO NOME-WK1
                  READ WORK1 INVALID KEY PERFORM GRAVA-WORK1
                   NOT INVALID KEY
                    ADD VLR-REPORT-R303   TO VALOR-TOTAL-WK1
                    IF FUNCAO-R303 = 12 OR 15
                       ADD VLR-REPORT-R303 TO VALOR-LOC-WK1
                    ELSE ADD VLR-REPORT-R303 TO VALOR-REP-WK1
                    END-IF
                    REWRITE REG-WORK1
                    END-REWRITE
                  END-READ
                 END-IF
              END-READ
           END-PERFORM.
       GRAVA-WORK1 SECTION.
           IF FUNCAO-R303 = 12 OR 15
              MOVE VLR-REPORT-R303 TO VALOR-LOC-WK1
              MOVE ZEROS TO VALOR-REP-WK1
           ELSE MOVE VLR-REPORT-R303 TO VALOR-REP-WK1
                MOVE ZEROS TO VALOR-LOC-WK1.
           MOVE VLR-REPORT-R303  TO VALOR-TOTAL-WK1.
           WRITE REG-WORK1.

       EMISSAO-PGTO SECTION.
           MOVE ZEROS TO FLAG.
           OPEN OUTPUT RELAT.
           MOVE SPACES TO NOME-WK1.
           START WORK1 KEY IS NOT < NOME-WK1 INVALID KEY
                 MOVE "10" TO ST-WORK1.
           PERFORM UNTIL ST-WORK1 = "10"
             READ WORK1 NEXT RECORD AT END MOVE "10" TO ST-WORK1
              NOT AT END
               MOVE "Imprimindo." TO GS-DESCRICAO(1: 12)
               MOVE CODIGO-WK1 TO DOCTO-R303 GS-DESCRICAO(13: 6)
               MOVE "REFRESH-TELA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM

               MOVE VALOR-TOTAL-WK1 TO VALOR-TOTAL-REL
                                   GREXTE-VALOR-BASE
               CALL "GREXTE" USING PARAMETROS-GREXTE
               MOVE GREXTE-VALOR-EXTENSO TO DESC-VALOR-REL
               MOVE NOME-WK1      TO NOME-REL
               MOVE CODIGO-WK1 TO CODIGO-R030
               READ RED030 INVALID KEY MOVE ZEROS TO RGINSC-R030
               END-READ
               MOVE RGINSC-R030   TO RG-REL
               PERFORM IMPRIME-RECIBO
               PERFORM IMPRIME-EXTRATO
             END-READ
           END-PERFORM.
           PERFORM FINALIZA-IMPRESSAO.
           MOVE 1 TO GS-EXIT-FLG.

       IMPRIME-RECIBO SECTION.
           IF FLAG = ZEROS
               WRITE REG-RELAT FROM LINHA-02
           ELSE WRITE REG-RELAT FROM LINHA-02 AFTER PAGE.
           WRITE REG-RELAT FROM LINHA-03.
           WRITE REG-RELAT FROM LINHA-04.
           WRITE REG-RELAT FROM LINHA-05.
           WRITE REG-RELAT FROM LINHA-01.
           WRITE REG-RELAT FROM LINHA-06 AFTER 2.
           WRITE REG-RELAT FROM LINHA-07 AFTER 2.
           WRITE REG-RELAT FROM LINHA-08.
           WRITE REG-RELAT FROM LINHA-09.
           WRITE REG-RELAT FROM LINHA-10.
           WRITE REG-RELAT FROM LINHA-11 AFTER 2.
           WRITE REG-RELAT FROM LINHA-12 AFTER 2.
           WRITE REG-RELAT FROM LINHA-13 AFTER 3.
           WRITE REG-RELAT FROM LINHA-14.
           WRITE REG-RELAT FROM LINHA-15.
           MOVE 1 TO FLAG.
       IMPRIME-EXTRATO SECTION.
           WRITE REG-RELAT FROM LINHA-20 AFTER 3.
           WRITE REG-RELAT FROM LINHA-21.
           WRITE REG-RELAT FROM LINHA-22.
           WRITE REG-RELAT FROM LINHA-21.

           MOVE MESANO-REC TO ANOMES-R300.
           START RED300 KEY IS NOT < ANOMES-R300 INVALID KEY
                        MOVE "10" TO ST-RED300.
           PERFORM UNTIL ST-RED300 = "10"
              READ RED300 NEXT RECORD AT END MOVE "10" TO ST-RED300
               NOT AT END
                IF ANOMES-R300 NOT = MESANO-REC MOVE "10" TO ST-RED300
                ELSE
                 MOVE DOCTO-R300 TO DOCTO-R303
                 MOVE CODIGO-WK1 TO REPRES-R303
                 MOVE ZEROS      TO FUNCAO-R303
                 START RED303 KEY IS NOT < CHAVE-R303 INVALID KEY
                       MOVE "10" TO ST-RED303
                 END-START
                 PERFORM UNTIL ST-RED303 = "10"
                  READ RED303 NEXT RECORD AT END MOVE "10" TO ST-RED303
                   NOT AT END
                    IF DOCTO-R303 NOT = DOCTO-R300 OR
                       REPRES-R303 NOT = CODIGO-WK1
                         MOVE "10" TO ST-RED303
                    ELSE
                      MOVE DOCTO-R303         TO NR-RELAT-REL
                      MOVE DATA-MOV-R300 TO DATA-I
                      MOVE MES-I              TO DIAMES-W (03: 02)
                      MOVE DIA-I              TO DIAMES-W (01: 02)
                      MOVE DIAMES-W           TO DATA-REP-REL
                      MOVE QTDE-FORM-R300     TO PARTICIPANTE-REL
                      MOVE QT-FILMES-R303     TO FIL-FIT-REL
                      MOVE QT-REPORT-R303     TO QT-REPORT-REL
                      MOVE FUNCAO-R303        TO CODIGO-R020
                      READ RED020 INVALID KEY MOVE SPACES TO NOME-R020
                      END-READ
                      MOVE NOME-R020          TO FUNCAO-REL
                      MOVE VLR-REPORT-R303    TO VALOR-REP-REL
                      MOVE DOCTO-R303 TO DOCTO-R301 DOCTO-R302
                      MOVE ZEROS      TO CONTRATO-R301
                      MOVE ZEROS      TO QT-CONTRATO-W
                      START RED301 KEY IS NOT < CHAVE-R301 INVALID KEY
                            MOVE "10" TO ST-RED301
                            MOVE ZEROS TO CONTRATO-REL NR-CONTRATO-CO40
                      END-START
                      PERFORM UNTIL ST-RED301 = "10"
                       READ RED301 NEXT RECORD AT END
                                  MOVE "10" TO ST-RED301
                        NOT AT END
                         IF DOCTO-R301 NOT = DOCTO-R300
                                  MOVE "10" TO ST-RED301
                         ELSE
                            IF QT-CONTRATO-W = 1 MOVE "10" TO ST-RED301
                               MOVE 2 TO QT-CONTRATO-W
                            ELSE
                             MOVE CONTRATO-R301 TO
                               CONTRATO-REL NR-CONTRATO-CO40
                             MOVE 1 TO QT-CONTRATO-W
                            END-IF
                         END-IF
                       END-READ
                      END-PERFORM
                      PERFORM IMPRIME-LINHA
                  END-READ
                 END-PERFORM
              END-READ
           END-PERFORM.
       IMPRIME-LINHA SECTION.
           IF QT-CONTRATO-W = 2 MOVE "+" TO QT-CONT-REL
           ELSE MOVE SPACES TO QT-CONT-REL.
           READ COD040 INVALID KEY
                MOVE ZEROS TO CIDADE-CO40
                MOVE SPACES TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40   TO CURSO-REL.
           MOVE CIDADE-CO40 TO CIDADE.
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID    TO CIDADE-REL.
           MOVE ZEROS TO EVENTO-R302.
           MOVE ZEROS      TO QT-EVENTO-W.
           START RED302 KEY IS NOT < CHAVE-R302 INVALID KEY
                 MOVE "10" TO ST-RED302
                 MOVE SPACES TO EVENTO-REL
                 MOVE ZEROS TO CODIGO-CO03.
           PERFORM UNTIL ST-RED302 = "10"
             READ RED302 NEXT RECORD AT END MOVE "10" TO ST-RED302
               NOT AT END
                 IF DOCTO-R302 NOT = DOCTO-R300 MOVE "10" TO ST-RED302
                 ELSE
                   IF QT-EVENTO-W = 1 MOVE "10" TO ST-RED302
                      MOVE 2 TO QT-EVENTO-W
                   ELSE
                     MOVE EVENTO-R302   TO CODIGO-CO03
                     MOVE 1 TO QT-EVENTO-W
                   END-IF
                 END-IF
              END-READ
           END-PERFORM.
           IF QT-EVENTO-W = 2 MOVE "VARIOS" TO EVENTO-REL
           ELSE READ COD003 INVALID KEY MOVE SPACES TO NOME-CO03
                                    MOVE NOME-CO03 TO EVENTO-REL
                            NOT INVALID KEY
                                    MOVE NOME-CO03 TO EVENTO-REL.
           WRITE REG-RELAT FROM LINHA-23.

       FINALIZA-IMPRESSAO SECTION.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT WORK1.
           DELETE FILE WORK1.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP424" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.

       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD040 COD003 RED020
                 RED030 RED090 RED300 RED301 RED302 RED303.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

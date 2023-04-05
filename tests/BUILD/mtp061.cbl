       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP061.
      *DATA: 12/01/2001
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: RELAÇÃO DE ESTOQUE NO FOGO
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY MTPX019.
           COPY MTPX020.
           COPY CAPX010.
           COPY CAPX012.
           COPY COPX040.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS NR-ALBUM-WK
                  ALTERNATE RECORD KEY IS ALT1-WK = CLIENTE-WK
                          WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-WK = CIDADE-WK
                     VISITA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-WK = UF-WK
                     VISITA-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-WK = MESANO-WK
                     CIDADE-WK NR-ALBUM-WK
                  ALTERNATE RECORD KEY IS ALT5-WK = VISITA-WK
                     CIDADE-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY MTPW019.
       COPY MTPW020.
       COPY CAPW010.
       COPY CAPW012.
       COPY COPW040.
       FD  WORK.
       01  REG-WORK.
           05  NR-ALBUM-WK         PIC 9(8).
           05  ESTOJO-WK           PIC 9(2).
           05  ENCADERN-WK         PIC 9(2).
           05  FOLHA-WK            PIC 9(4).
           05  FOTO-WK             PIC 9(4).
           05  POSTER-WK           PIC 9(2).
           05  FITA-WK             PIC 9(2).
           05  DVD-WK              PIC 9(2).
           05  PFITA-WK            PIC 9(2).
           05  PDVD-WK             PIC 9(2).
           05  FOTO-CD-WK          PIC 9(2).
           05  MOLDURA-WK          PIC 9(2).
           05  CLIENTE-WK          PIC X(30).
           05  CIDADE-WK           PIC X(13).
           05  FONE-WK             PIC 9(8).
           05  UF-WK               PIC XX.
           05  MESANO-WK           PIC 9(6).
           05  VISITA-WK           PIC 9.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP061.CPB".
           COPY "MTP061.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(6)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(6)     VALUE ZEROS.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  QTDE-E6               PIC ZZ.ZZ9.
           05  QTDE-E9               PIC Z.ZZZ.ZZ9.
           05  QTDE-E10              PIC ZZ.ZZZ.ZZ9.

           05  MESANOW.
               10  MES-WW            PIC 99.
               10  ANO-WW            PIC 9(4).
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-II            PIC 9(4).
               10  MES-II            PIC 9(2).
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  PERC-E                PIC ZZ,ZZ    BLANK WHEN ZEROS.
      *    OPCAO INDIVIDUAL
           05  GRAVAR-OPCAO          PIC 9        VALUE ZEROS.
           05  CODIGO-W              PIC 9(2)     VALUE ZEROS.
           05  UF-W                  PIC XX       VALUE SPACES.
      *    TOTALIZA VARIAVEIS
           05  TOTAL-ESTOJO          PIC 9(7)     VALUE ZEROS.
           05  TOTAL-ENCADERNACAO    PIC 9(8)     VALUE ZEROS.
           05  TOTAL-FOLHAS          PIC 9(7)     VALUE ZEROS.
           05  TOTAL-FOTOS           PIC 9(8)     VALUE ZEROS.
           05  TOTAL-POSTER          PIC 9(8)     VALUE ZEROS.
           05  TOTAL-FITAS           PIC 9(7)     VALUE ZEROS.
           05  TOTAL-DVD             PIC 9(7)     VALUE ZEROS.
           05  TOTAL-PFITAS          PIC 9(7)     VALUE ZEROS.
           05  TOTAL-PDVD            PIC 9(7)     VALUE ZEROS.
           05  TOTAL-FOTO-CD         PIC 9(7)     VALUE ZEROS.
           05  TOTAL-MOLDURA         PIC 9(7)     VALUE ZEROS.
           05  QTDE-E                PIC Z.ZZZ.ZZ9.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(70)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(43)   VALUE
           "RELACAO DE ESTOQUES NO FOGO        -ORDEM: ".
           05  ORDEM-REL           PIC X(24)   VALUE SPACES.
           05  FILLER              PIC X(10)    VALUE SPACES.
           05  FILLER              PIC X(16)   VALUE "INT.MES/ANO...: ".
           05  MESANO-INI-REL        PIC 99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  MESANO-FIM-REL        PIC 99/9999.
       01  CAB03.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110) VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(110)  VALUE
           "NR-ALBUM ES EN FOLH FOTO PO FI PF DV PD FC MD CLIENTE
      -    "     CIDADE        FONE     UF MES/ANO VISITA".
       01  CAB05.
           05  FILLER              PIC X(05)   VALUE SPACES.
           05  FILLER              PIC X(101)  VALUE
           "ESTOJO ENCADERN. TOT-FOLHAS TOT-FOTOS POSTER TOT-FITAS PORTA
      -    "-FITA  TOTAL-DVD TOT-PDV  TOT-FC TOT-MOLD".
       01  LINDET.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINDET-REL          PIC X(110)  VALUE SPACES.
       01  LINTOT.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LINTOT-REL          PIC X(100)  VALUE SPACES.

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
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           OPEN INPUT CAD010 CAD012 COD040 MTD019 MTD020.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
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
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-LER-INDIVIDUAL-TRUE
                    PERFORM LER-CODIGOS
               WHEN GS-OPCAO-POP-UP-TRUE
                    PERFORM LER-POPUP
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
       LER-CODIGOS SECTION.
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 0 MOVE GS-CODIGO TO CIDADE
                  READ CAD010 INVALID KEY MOVE SPACES TO NOME-COMPL-CID
                  END-READ
                  MOVE NOME-COMPL-CID TO GS-DESCRICAO
             WHEN 1 MOVE GS-CODIGO TO CODIGO-REG
                  READ CAD012 INVALID KEY MOVE SPACES TO NOME-REG
                  END-READ
                  MOVE NOME-REG     TO GS-DESCRICAO
             WHEN 2 CONTINUE
             WHEN 3 MOVE GS-CODIGO  TO NR-CONTRATO-CO40
                  READ COD040 INVALID KEY
                       MOVE SPACES TO IDENTIFICACAO-CO40
                  END-READ
                  MOVE IDENTIFICACAO-CO40 TO GS-DESCRICAO
           END-EVALUATE.

       LER-POPUP SECTION.
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 0 CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CAP010T"
                    MOVE PASSAR-STRING-1(1: 30)   TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(35: 4)   TO GS-CODIGO
             WHEN 1 CALL   "CAP012T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CAP012T"
                    MOVE PASSAR-STRING-1(1: 30)   TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(33: 2)   TO GS-CODIGO
             WHEN 2 CONTINUE
             WHEN 3 CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(22: 11)  TO GS-DESCRICAO
                    MOVE PASSAR-STRING-1(52: 4)   TO GS-CODIGO
           END-EVALUATE.

       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35" CLOSE WORK   DELETE FILE WORK.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.  CLOSE WORK.  OPEN I-O WORK.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-MESANO-INI    TO MESANO-INI-REL MESANO-W
           MOVE MES-WW         TO MES-II
           MOVE ANO-WW         TO ANO-II
           MOVE MESANO-I       TO MESANO-INI
           MOVE GS-MESANO-FIM    TO MESANO-W MESANO-FIM-REL.
           MOVE MES-WW         TO MES-II
           MOVE ANO-WW         TO ANO-II
           MOVE MESANO-I       TO MESANO-FIM

      *    VERIFICA OPCAO DO RELATORIO - INDIV OU GERAL
           IF GS-OPCAO-INDIVIDUAL < 3
              MOVE MESANO-INI      TO ANOMES-VISITA-MTG
              START MTD020 KEY IS NOT < ANOMES-VISITA-MTG
                    INVALID KEY MOVE "10" TO ST-MTD020
              END-START
           ELSE MOVE GS-CODIGO   TO ALBUM-MTG(1: 4)
                MOVE ZEROS       TO ALBUM-MTG(5: 4)
                START MTD020 KEY IS NOT < ALBUM-MTG INVALID KEY
                      MOVE "10"  TO ST-MTD020
                END-START
           END-IF.
           PERFORM UNTIL ST-MTD020 = "10"
             READ MTD020 NEXT RECORD AT END MOVE "10" TO ST-MTD020
              NOT AT END
               IF GS-OPCAO-INDIVIDUAL < 3
                  IF ANOMES-VISITA-MTG > MESANO-FIM
                     MOVE "10" TO ST-MTD020
                  ELSE PERFORM MOVER-DADOS-WORK
               ELSE
                IF CONTRATO-MTG <> GS-CODIGO
                       MOVE "10" TO ST-MTD020
                ELSE PERFORM MOVER-DADOS-WORK
                END-IF
               END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS-WORK SECTION.
           MOVE ALBUM-MTG          TO ALBUM-MT19 GS-EXIBE-VENCTO.
           MOVE "TELA-AGUARDA1"    TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           READ MTD019 INVALID KEY INITIALIZE REG-MTD019.
           PERFORM VERIFICA-OPCAO
           IF GRAVAR-OPCAO = ZEROS OR FOGO-MTG <> 8 AND FOGO-MTG <> 9
                                      CONTINUE
           ELSE
            MOVE ALBUM-MTG         TO NR-ALBUM-WK
            MOVE QT-ESTOJO-MTG     TO ESTOJO-WK
            MOVE QT-ENCADER-MTG    TO ENCADERN-WK
            MOVE QT-FOLHAS-MTG     TO FOLHA-WK
            MOVE QT-FOTOS-MTG      TO FOTO-WK
            MOVE QT-POSTER-MTG     TO POSTER-WK
            MOVE QT-FITAS-MTG      TO FITA-WK
            MOVE QT-DVD-MTG        TO DVD-WK
            MOVE QT-PORTA-FITA-MTG TO PFITA-WK
            MOVE QT-PORTA-DVD-MTG  TO PDVD-WK
            MOVE QT-FOTO-CD-MTG    TO FOTO-CD-WK
            MOVE QT-MOLDURA-MTG    TO MOLDURA-WK
            MOVE NOME-FORM-MT19    TO CLIENTE-WK
            MOVE CIDADE-MT19       TO CIDADE
            READ CAD010 INVALID KEY MOVE SPACES TO NOME-COMPL-CID
            END-READ
            MOVE NOME-CID          TO CIDADE-WK
            MOVE FONE-MT19         TO FONE-WK
            MOVE UF-MT19           TO UF-WK
            MOVE ANOMES-VISITA-MTG TO MESANO-WK
            MOVE VISITA-MTG        TO VISITA-WK
            WRITE REG-WORK
            END-WRITE
           END-IF.
       VERIFICA-OPCAO SECTION.
           MOVE GS-CODIGO TO CODIGO-W
           MOVE 0 TO GRAVAR-OPCAO
           EVALUATE GS-OPCAO-INDIVIDUAL
             WHEN 0 IF GS-CODIGO = CIDADE-MT19  MOVE 1 TO GRAVAR-OPCAO
             WHEN 1 MOVE GS-DESCRICAO TO UF-W
                    IF UF-W = UF-MT19   MOVE 1 TO GRAVAR-OPCAO
             WHEN 2 MOVE 1 TO GRAVAR-OPCAO
             WHEN 3 IF GS-CODIGO = CONTRATO-MTG  MOVE 1 TO GRAVAR-OPCAO
           END-EVALUATE.
      *-----------------------------------------------------
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           PERFORM ZERA-VARIAVEIS.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                  PERFORM MOVER-DADOS-LINDET
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA.

       ORDEM SECTION.
           INITIALIZE REG-WORK.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "ALBUM         " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < NR-ALBUM-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "CLIENTE       " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT1-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "CIDADE/VISITA " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT2-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "ESTADO/VISITA " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT3-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "MESANO/CIDADE " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT4-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "VISITA/CIDADE " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT5-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE NR-ALBUM-WK            TO GS-LINDET(1: 9)
           MOVE ESTOJO-WK              TO GS-LINDET(10: 3)
           ADD ESTOJO-WK               TO TOTAL-ESTOJO
           MOVE ENCADERN-WK            TO GS-LINDET(13: 3)
           ADD ENCADERN-WK             TO TOTAL-ENCADERNACAO
           MOVE FOLHA-WK               TO GS-LINDET(16: 5)
           ADD FOLHA-WK                TO TOTAL-FOLHAS
           MOVE FOTO-WK                TO GS-LINDET(21: 5)
           ADD FOTO-WK                 TO TOTAL-FOTOS
           MOVE POSTER-WK              TO GS-LINDET(26: 3)
           ADD POSTER-WK               TO TOTAL-POSTER
           MOVE FITA-WK                TO GS-LINDET(29: 3)
           ADD FITA-WK                 TO TOTAL-FITAS
           MOVE PFITA-WK               TO GS-LINDET(32: 3)
           ADD PFITA-WK                TO TOTAL-PFITAS
           MOVE DVD-WK                 TO GS-LINDET(35: 3)
           ADD DVD-WK                  TO TOTAL-DVD
           MOVE PDVD-WK                TO GS-LINDET(38:3)
           ADD  PDVD-WK                TO TOTAL-PDVD
           MOVE FOTO-CD-WK             TO GS-LINDET(41:3)
           ADD  FOTO-CD-WK             TO TOTAL-FOTO-CD
           MOVE MOLDURA-WK             TO GS-LINDET(44:3)
           ADD  MOLDURA-WK             TO TOTAL-MOLDURA
           MOVE CLIENTE-WK             TO GS-LINDET(47: 21)
           MOVE CIDADE-WK              TO GS-LINDET(69: 14)
           MOVE FONE-WK                TO GS-LINDET(83: 9)
           MOVE UF-WK                  TO GS-LINDET(92: 3)
           MOVE MESANO-WK              TO MESANO-I
           MOVE ANO-II                 TO ANO-WW
           MOVE MES-II                 TO MES-WW
           MOVE MESANO-W               TO MESANO-E
           MOVE MESANO-E               TO GS-LINDET(100: 8)
           MOVE VISITA-WK              TO GS-LINDET(108: 2).
       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO TOTAL-ESTOJO TOTAL-ENCADERNACAO TOTAL-FOTOS
                         TOTAL-FITAS TOTAL-POSTER TOTAL-PFITAS
                         TOTAL-FOLHAS TOTAL-DVD TOTAL-PDVD
                         TOTAL-FOTO-CD TOTAL-MOLDURA.
       TOTALIZA SECTION.
           MOVE SPACES                 TO GS-LINTOT
           MOVE TOTAL-ESTOJO           TO QTDE-E6
           MOVE QTDE-E6                TO GS-LINTOT(1: 7)
           MOVE TOTAL-ENCADERNACAO     TO QTDE-E9
           MOVE QTDE-E9                TO GS-LINTOT(8: 10)
           MOVE TOTAL-FOLHAS           TO QTDE-E10
           MOVE QTDE-E10               TO GS-LINTOT(18: 11)
           MOVE TOTAL-FOTOS            TO QTDE-E9
           MOVE QTDE-E9                TO GS-LINTOT(29: 10)
           MOVE TOTAL-POSTER           TO QTDE-E6
           MOVE QTDE-E6                TO GS-LINTOT(39: 7)
           MOVE TOTAL-FITAS            TO QTDE-E9
           MOVE QTDE-E9                TO GS-LINTOT(46: 10)
           MOVE TOTAL-PFITAS           TO QTDE-E10
           MOVE QTDE-E10               TO GS-LINTOT(56: 10).
           MOVE TOTAL-DVD              TO QTDE-E6
           MOVE QTDE-E6                TO GS-LINTOT(70: 07)
           MOVE TOTAL-PDVD             TO QTDE-E6
           MOVE QTDE-E6                TO GS-LINTOT(79:07)
           MOVE TOTAL-FOTO-CD          TO QTDE-E6
           MOVE QTDE-E6                TO GS-LINTOT(86:07)
           MOVE TOTAL-MOLDURA          TO QTDE-E6
           MOVE QTDE-E6                TO GS-LINTOT(96:07)
           MOVE "INSERE-LINTOT" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP061" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           PERFORM ZERA-VARIAVEIS.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                      PERFORM MOVER-DADOS-RELATORIO
              END-READ
           END-PERFORM.
           WRITE REG-RELAT FROM CAB05 AFTER 2
           MOVE SPACES TO REG-RELAT
           WRITE REG-RELAT

           PERFORM TOTALIZA-REL

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           PERFORM MOVER-DADOS
           MOVE GS-LINDET TO LINDET-REL

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE SPACES                 TO LINTOT-REL
           MOVE TOTAL-ESTOJO           TO QTDE-E6
           MOVE QTDE-E6                TO LINTOT-REL(1: 7)
           MOVE TOTAL-ENCADERNACAO     TO QTDE-E9
           MOVE QTDE-E9                TO LINTOT-REL(8: 10)
           MOVE TOTAL-FOLHAS           TO QTDE-E10
           MOVE QTDE-E10               TO LINTOT-REL(18: 11)
           MOVE TOTAL-FOTOS            TO QTDE-E9
           MOVE QTDE-E9                TO LINTOT-REL(29: 10)
           MOVE TOTAL-POSTER           TO QTDE-E6
           MOVE QTDE-E6                TO LINTOT-REL(39: 7)
           MOVE TOTAL-FITAS            TO QTDE-E9
           MOVE QTDE-E9                TO LINTOT-REL(46: 10)
           MOVE TOTAL-PFITAS           TO QTDE-E10
           MOVE QTDE-E10               TO LINTOT-REL(56: 10).
           MOVE TOTAL-DVD              TO QTDE-E6
           MOVE QTDE-E6                TO LINTOT-REL(76: 07)
           MOVE TOTAL-PDVD             TO QTDE-E6
           MOVE QTDE-E6                TO LINTOT-REL(83:07)
           MOVE TOTAL-FOTO-CD          TO QTDE-E10
           MOVE QTDE-E6                TO LINTOT-REL(91:07)
           MOVE TOTAL-MOLDURA          TO QTDE-E10
           MOVE QTDE-E6                TO LINTOT-REL(100:07)

           WRITE REG-RELAT FROM LINTOT-REL.
           ADD 1 TO LIN.

           MOVE SPACES TO LINTOT-REL.
           WRITE REG-RELAT FROM LINTOT-REL.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       CABECALHO SECTION.
           MOVE GS-DESCR-ORDEM TO ORDEM-REL.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
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
           CLOSE CAD010 CAD012 COD040 MTD019 MTD020.
           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

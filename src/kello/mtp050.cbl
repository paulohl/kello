       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP050.
      *DATA: 05/07/2000
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *FUNÇÃO: RELACAO DE ALBUM NA MONTAGEM
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY IEPX011.
           COPY COPX040.
           COPY CEAPX010.
           COPY MTPX001.
           COPY MTPX019.
           COPY MTPX020.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS ALBUM-WK
                  ALTERNATE RECORD KEY IS NOME-FORMANDO-WK
                        WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT1-WK =
                       CIDADE-WK ALBUM-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT2-WK =
                       CURSO-WK ALBUM-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT3-WK = FOGO-WK
                       ALBUM-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT4-WK = VISITA-WK
                       ALBUM-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           SELECT FILIAL ASSIGN TO ARQUIVO-FILIAL
                         ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CEAPW010.
       COPY IEPW011.
       COPY COPW040.
       COPY MTPW001.
       COPY MTPW019.
       COPY MTPW020.
       FD  WORK.
       01  REG-WORK.
           05  ALBUM-WK            PIC 9(4).
           05  ESTOJO-WK           PIC 9.
           05  ENCADERNACAO-WK     PIC 9.
           05  FOLHA-WK            PIC 9(4).
           05  FOTO-WK             PIC 9(4).
           05  POSTER-WK           PIC 9.
           05  FITA-WK             PIC 9.
           05  DVD-WK              PIC 9.
           05  PORTA-FITA-WK       PIC 9.
           05  PORTA-DVD-WK        PIC 9.
           05  FOTO-CD-WK          PIC 9.
           05  MOLDURA-WK          PIC 9.
           05  BOOK-WK             PIC 9.
           05  NOME-FORMANDO-WK    PIC X(30).
           05  CIDADE-WK           PIC X(13).
           05  CURSO-WK            PIC X(15).
           05  FONE-WK             PIC 9(8).
           05  FOGO-WK             PIC 9.
           05  VISITA-WK           PIC 9.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       FD  FILIAL.
       01  REG-FILIAL          PIC X(140).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP050.CPB".
           COPY "MTP050.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
           COPY "CPDIAS1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CEAD010            PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-MTD001             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  SEQUENCIA             PIC 9(04).
           05  MASC-SEQ              PIC ZZZ9.
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
           05  FONE-E                PIC ZZZZ.ZZZZ.
           05  QTDE-E                PIC ZZZ.ZZZ.ZZ9 BLANK WHEN ZEROS.
           05  QTDE1-E               PIC ZZZ.ZZZ.ZZ9 BLANK WHEN ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  PRIMEIRA-VEZ          PIC 9        VALUE ZEROS.
      *Total-acum - valor acumulado p/ calcula o prazo médio de atraso
           05  PASSAR-STRING-1       PIC X(65).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

       01  CAB01.
           05  EMPRESA-REL         PIC X(65)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(94)   VALUE
           "RELACAO DE ALBUNS NA MONTAGEM- ORDEM: ".
           05  ORDEM-REL           PIC X(16)   VALUE SPACES.
       01  CAB02A.
           05  FILLER              PIC X(12)   VALUE "CONTRATO:   ".
           05  CONTRATO-REL        PIC ZZZ.ZZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  INSTITUICAO-REL     PIC X(30)   VALUE ZEROS.
       01  CAB03.
           05  FILLER              PIC X(111)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(112)  VALUE
           "ALB. ES EN FOLH FOTO P F PF DV PD FC MD BK NOME-CLIENTE
      -    " CIDADE        CURSO                FONE F V    ORD".
       01  LINDET.
           05  LINDET-REL          PIC X(111)  VALUE SPACES.
       01  CAB05a.
           05 FILLER               PIC X(12)
              VALUE "T. CLIENTE".
           05 FILLER               PIC X(12)
              VALUE "T. ESTOJOS".
           05 FILLER               PIC X(12)
              VALUE "T. ENCADER".
           05 FILLER               PIC X(12)
              VALUE " T. FOLHAS".
           05 FILLER               PIC X(12)
              VALUE "  T. FOTOS".
           05 FILLER               PIC X(12)
              VALUE " T. POSTER".
           05 FILLER               PIC X(12)
              VALUE "  T. FITAS".
       01  CAB05b.
           05 FILLER               PIC X(12)
              VALUE "T. PT FITA".
           05 FILLER               PIC X(12)
              VALUE " T. PT DVD".
           05 FILLER               PIC X(12)
              VALUE "    T. DVD".
           05 FILLER               PIC X(12)
              VALUE "T. FOTO CD".
           05 FILLER               PIC X(12)
              VALUE "T. MOLDURA".
           05 FILLER               PIC X(12)
              VALUE "  T. BOOKS".
       01  CAB06.
           05  FILLER              PIC X(111)  VALUE ALL "=".

       01  LINTOT1.
           05  LINTOT-ALBUM        PIC ZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-ESTOJO       PIC ZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-ENCADERNACAO PIC ZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-FOLHAS       PIC ZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-FOTOS        PIC ZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-POSTER       PIC ZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-FITAS        PIC ZZ.ZZZ.ZZ9.

       01  LINTOT2.
           05  LINTOT-PORTA-FITA   PIC ZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-PORTA-DVD    PIC ZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-DVD          PIC ZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-FOTO-CD      PIC ZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-MOLDURAS     PIC ZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(02).
           05  LINTOT-BOOK         PIC ZZ.ZZZ.ZZ9.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE .
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
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "MTD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD001.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.

           OPEN INPUT CEAD010 CAD010 IED011 MTD001 MTD019 MTD020 COD040.
           IF ST-CEAD010 <> "00"
              MOVE "ERRO ABERTURA CEAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CEAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD001 <> "00"
              MOVE "ERRO ABERTURA MTD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD001 TO GS-MENSAGEM-ERRO(23: 02)
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
                    IF GS-IMPRIMIR = "N"
                       PERFORM IMPRIME-RELATORIO
                       move spaces to mensagem
                       string "Arquivo gerado em . . . " x"0a"
                              ARQUIVO-FILIAL into mensagem
                         move "C" to tipo-msg
                       perform 140-exibir-mensagem
                    ELSE
                       COPY IMPRESSORA.CHAMA.
                       IF LNK-MAPEAMENTO <> SPACES
                          PERFORM IMPRIME-RELATORIO
                       END-IF
                    END-IF
               WHEN GS-GRAVA-WORK-FLG-TRUE
      *             PERFORM VERIFICA-DATA-MOVTO-ANT
                    PERFORM GRAVA-WORK
                    PERFORM CABECALHO-LISTA
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-POPUP-CONTRATO-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN GS-LE-CONTRATO-TRUE
                   PERFORM LE-CONTRATO
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-POPUP SECTION.
           CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "COP040T".
           MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO.
           PERFORM LE-CONTRATO.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO  TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE SPACES TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-IDENTIFICACAO.
      *------------------------------------------------------------
       GRAVA-WORK SECTION.
           CLOSE WORK  OPEN OUTPUT WORK CLOSE WORK  OPEN I-O WORK.
           MOVE ZEROS TO PRIMEIRA-VEZ GS-TOT-ALBUM.
           MOVE "TELA-AGUARDA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-CONTRATO   TO CONTRATO-MTG.
           MOVE ZEROS         TO NRALBUM-MTG.
           START MTD020 KEY IS NOT < ALBUM-MTG INVALID KEY
                  MOVE "10" TO ST-MTD020.

           PERFORM UNTIL ST-MTD020 = "10"
             READ MTD020 NEXT RECORD AT END MOVE "10" TO ST-MTD020
              NOT AT END
              IF CONTRATO-MTG > GS-CONTRATO
                 MOVE "10" TO ST-MTD020
              ELSE
                IF PRIMEIRA-VEZ = 0
                   MOVE DATAMOV-MTG     TO DATA-INV
                   CALL "GRIDAT1" USING DATA-INV
                   CANCEL "GRIDAT1"
                   MOVE DATA-INV         TO GS-DATA-MONT
                   MOVE 1 TO PRIMEIRA-VEZ
                END-IF
                MOVE GS-CONTRATO         TO CONTRATO-MT19
                                            GS-EXIBE-MOVTO
                MOVE "TELA-AGUARDA1"     TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                MOVE NRALBUM-MTG            TO ALBUM-WK SEQ-MT19
                READ MTD019 INVALID KEY
                   MOVE ZEROS  TO CIDADE-MT19
                   MOVE SPACES TO NOME-FORM-MT19
                END-READ
                IF NRALBUM-MTG > 0 AND NAO-GEROU-ALBUM-MTG <> 1
                   ADD 1 TO GS-TOT-ALBUM
                END-IF
                MOVE NOME-FORM-MT19      TO NOME-FORMANDO-WK
                IF CIDADE-MT19 NOT NUMERIC CONTINUE
                ELSE
                  MOVE CIDADE-MT19         TO CIDADE
                  READ CAD010 INVALID KEY
                       MOVE SPACES TO NOME-CID
                  END-READ
                  MOVE NOME-CID            TO CIDADE-WK
                  MOVE CURSO-MT19          TO CODIGO-IE11
                  READ IED011 INVALID KEY MOVE SPACES TO NOME-IE11
                  END-READ
                  MOVE NOME-IE11          TO CURSO-WK
                  MOVE FONE-MT19          TO FONE-WK
                  MOVE QT-ESTOJO-MTG      TO ESTOJO-WK
                  MOVE QT-ENCADER-MTG     TO ENCADERNACAO-WK
                  MOVE QT-FOLHAS-MTG      TO FOLHA-WK
                  MOVE QT-FOTOS-MTG       TO FOTO-WK
                  MOVE QT-FITAS-MTG       TO FITA-WK
                  MOVE QT-DVD-MTG         TO DVD-WK
                  MOVE QT-PORTA-FITA-MTG  TO PORTA-FITA-WK
                  MOVE QT-POSTER-MTG      TO POSTER-WK
                  MOVE QT-PORTA-DVD-MTG   TO PORTA-DVD-WK
                  MOVE QT-FOTO-CD-MTG     TO FOTO-CD-WK
                  MOVE QT-MOLDURA-MTG     TO MOLDURA-WK
                  MOVE QT-BOOK-MTG        TO BOOK-WK
                  MOVE VISITA-MTG         TO VISITA-WK
                  MOVE FOGO-MTG           TO FOGO-WK

                  WRITE REG-WORK
               END-IF
             END-READ
           END-PERFORM.
           MOVE "TELA-AGUARDA2" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
      *---------------------------------------------------
       CABECALHO-LISTA SECTION.
           MOVE GS-CONTRATO   TO CONTRATO-MT01.
           READ MTD001 INVALID KEY INITIALIZE REG-MTD001.
           MOVE GUIA-MT01         TO GS-NR-GUIA
           MOVE PRODUZIDA-MT01    TO GS-FOTO-PRODUZIDA
           MOVE MONTADA-MT01      TO GS-FOTO-MONTADA
           MOVE PERDIDA-MT01      TO GS-FOTO-PERDIDA
           MOVE AVULSA-MT01       TO GS-FOTO-AVULSA
           MOVE CLIEN-ALBUM-MT01  TO GS-CLIENTE-ALBUM
           MOVE CODALBUM-MT01     TO GS-COD-ALBUM PRODUTO
           READ CEAD010 INVALID KEY MOVE SPACES TO DESC-PROD.
           MOVE DESC-PROD         TO GS-DESC-ALBUM
           MOVE CODFOLHA-MT01     TO GS-COD-FOLHA PRODUTO
           READ CEAD010 INVALID KEY MOVE SPACES TO DESC-PROD.
           MOVE DESC-PROD         TO GS-DESC-FOLHA
           MOVE CODSEDA-MT01      TO GS-COD-SEDA
           READ CEAD010 INVALID KEY MOVE SPACES TO DESC-PROD.
           MOVE DESC-PROD         TO GS-DESC-SEDA
           MOVE CODFOTO-MT01      TO GS-COD-FOTO PRODUTO
           READ CEAD010 INVALID KEY MOVE SPACES TO DESC-PROD.
           MOVE DESC-PROD         TO GS-DESC-FOTO.
       CARREGA-LISTA SECTION.
           MOVE ZEROS TO GS-TOT-ESTOJO GS-TOT-ENCADERNACAO
                         GS-TOT-FITA   GS-TOT-FOTO GS-TOT-FOLHA
                         GS-TOT-POSTER
                         GS-TOT-PORTA-FITA SEQUENCIA GS-TOT-DVD
                         GS-TOT-PORTA-DVD  GS-TOT-FOTO-CD GS-TOT-MOLDURA
                         GS-TOT-BOOK.

           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                 IF FOTO-WK > 0
                    PERFORM MOVER-DADOS-LINDET
                    MOVE "INSERE-LIST" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                 END-IF
              END-READ
           END-PERFORM.
      *    PERFORM TOTALIZA.

       MOVER-DADOS-LINDET SECTION.
           MOVE ALBUM-WK          TO GS-LINDET(01: 5)
           MOVE ESTOJO-WK         TO GS-LINDET(06: 3)
           ADD ESTOJO-WK          TO GS-TOT-ESTOJO
           MOVE ENCADERNACAO-WK   TO GS-LINDET(9: 3)
           ADD ENCADERNACAO-WK    TO GS-TOT-ENCADERNACAO
           MOVE FOLHA-WK          TO GS-LINDET(12: 5)
           ADD FOLHA-WK           TO GS-TOT-FOLHA
           MOVE FOTO-WK           TO GS-LINDET(17: 5)
           ADD FOTO-WK            TO GS-TOT-FOTO
           MOVE POSTER-WK         TO GS-LINDET(22: 2)
           ADD POSTER-WK          TO GS-TOT-POSTER
           MOVE FITA-WK           TO GS-LINDET(24: 2)
           ADD FITA-WK            TO GS-TOT-FITA
           MOVE PORTA-FITA-WK     TO GS-LINDET(26: 3)
           ADD PORTA-FITA-WK      TO GS-TOT-PORTA-FITA
           MOVE DVD-WK            TO GS-LINDET(30: 2)
           ADD DVD-WK             TO GS-TOT-DVD
           MOVE PORTA-DVD-WK      TO GS-LINDET(33: 2)
           ADD PORTA-DVD-WK       TO GS-TOT-PORTA-DVD
           MOVE FOTO-CD-WK        TO GS-LINDET(36: 2)
           ADD FOTO-CD-WK         TO GS-TOT-FOTO-CD
           MOVE MOLDURA-WK        TO GS-LINDET(39: 2)
           ADD MOLDURA-WK         TO GS-TOT-MOLDURA
           MOVE BOOK-WK           TO GS-LINDET(41: 2)
           ADD BOOK-WK            TO GS-TOT-BOOK

           MOVE NOME-FORMANDO-WK  TO GS-LINDET(43: 18)
           MOVE CIDADE-WK         TO GS-LINDET(62: 14)
           MOVE CURSO-WK          TO GS-LINDET(76: 16)
           MOVE FONE-WK           TO FONE-E
           MOVE FONE-E            TO GS-LINDET(93: 10)
           MOVE FOGO-WK           TO GS-LINDET(103: 2)
           MOVE VISITA-WK         TO GS-LINDET(105: 2)

           IF ALBUM-WK = 0
              MOVE 0              TO MASC-SEQ
              MOVE MASC-SEQ       TO GS-LINDET(106:4)
           ELSE
              ADD  1              TO SEQUENCIA
              MOVE SEQUENCIA      TO MASC-SEQ
              MOVE MASC-SEQ       TO GS-LINDET(106:4).

       ORDEM SECTION.
           INITIALIZE REG-WORK.
           EVALUATE GS-ORDEM
             WHEN 0
                MOVE "ALBUM        " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALBUM-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 1
                MOVE "NOME-FORMANDO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < NOME-FORMANDO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "CIDADE" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT1-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "CURSO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT2-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "FOGO" TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT3-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "VISITA    " TO GS-DESCR-ORDEM
                START WORK KEY IS NOT < ALT4-WK INVALID KEY
                      MOVE "10" TO ST-WORK
           END-EVALUATE.
      *TOTALIZA SECTION.
      *    MOVE SPACES TO GS-LINTOT.
      *    MOVE TOT-ALBUM         TO QTDE-E
      *    MOVE QTDE-E            TO GS-LINTOT(1: 10)
      *    MOVE TOT-ESTOJO        TO QTDE-E
      *    MOVE QTDE-E            TO GS-LINTOT(11: 10)
      *    MOVE TOT-ENCADERNACAO  TO QTDE-E
      *    MOVE QTDE-E            TO GS-LINTOT(21: 10)
      *    MOVE TOT-FOLHA         TO QTDE-E
      *    MOVE QTDE-E            TO GS-LINTOT(31: 10)
      *    MOVE TOT-FOTO          TO QTDE-E
      *    MOVE QTDE-E            TO GS-LINTOT(41: 10)
      *    MOVE TOT-POSTER        TO QTDE-E
      *    MOVE QTDE-E            TO GS-LINTOT(51: 10)
      *    MOVE TOT-FITA          TO QTDE-E
      *    MOVE QTDE-E            TO GS-LINTOT(61: 10)
      *    MOVE TOT-PORTA-FITA    TO QTDE-E
      *    MOVE QTDE-E            TO GS-LINTOT(71: 10).
      *    MOVE TOT-DVD           TO QTDE2-E
      *    MOVE QTDE2-E           TO GS-LINTOT(81: 08)
      *    MOVE TOT-PORTA-DVD     TO QTDE2-E
      *    MOVE QTDE2-E           TO GS-LINTOT(90: 08)
      *    MOVE TOT-FOTO-CD       TO QTDE2-E
      *    MOVE QTDE2-E           TO GS-LINTOT(98: 08)
      *    MOVE TOT-MOLDURA       TO QTDE2-E
      *    MOVE QTDE2-E           TO GS-LINTOT(105:07)
      *
      *    MOVE "INSERE-LINTOT"   TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP050" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE SPACES TO ARQUIVO-FILIAL

           STRING "\ARQUIVOS\MTD050-" GS-CONTRATO INTO ARQUIVO-FILIAL

           IF GS-IMPRIMIR = "S"
              OPEN OUTPUT RELAT
              IF LNK-TIPO = 1
                 WRITE REG-RELAT FROM COND-HP BEFORE 0
              ELSE
                 WRITE REG-RELAT FROM COND-EP BEFORE 0
              END-IF
           ELSE
              OPEN OUTPUT FILIAL.

           MOVE ZEROS TO PAG-W SEQUENCIA.
           PERFORM ORDEM.

           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                   IF FOTO-WK > 0
                      PERFORM MOVER-DADOS-RELATORIO
                   END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL

           IF GS-IMPRIMIR = "S"
              MOVE SPACES TO REG-RELAT
              IF LNK-TIPO = 1
                 WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
              ELSE
                 WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE
              END-IF
              CLOSE RELAT
           ELSE
              MOVE "SALTAR PAGINA" TO REG-FILIAL
              WRITE REG-FILIAL
              CLOSE FILIAL.



      *    MOVE ZEROS TO PAG-W SEQUENCIA.
      *    COPY "COND-IMP".
      *    OPEN OUTPUT RELAT.
      *    PERFORM ORDEM.
      *    MOVE ZEROS TO LIN. PERFORM CABECALHO.
      *    PERFORM UNTIL ST-WORK = "10"
      *       READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
      *       NOT AT END
      *            IF FOTO-WK > 0
      *               PERFORM MOVER-DADOS-RELATORIO
      *            END-IF
      *       END-READ
      *    END-PERFORM.
      *    PERFORM TOTALIZA-REL
      *    MOVE SPACES TO REG-RELAT.
      *    WRITE REG-RELAT AFTER PAGE.
      *    CLOSE RELAT.
      *    COPY "DESC-IMP".
       CABECALHO-LISTA-REL SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE "DATA MONTAGEM: " TO LINDET-REL(6: 15)
           MOVE GS-DATA-MONT      TO DATA-E
           MOVE DATA-E            TO LINDET-REL(21: 20)
           MOVE "NR.GUIA......: " TO LINDET-REL(41: 15)
           MOVE GS-NR-GUIA        TO QTDE1-E
           MOVE QTDE1-E           TO LINDET-REL(57: 20)
           MOVE "FOTO PRODUZID: " TO LINDET-REL(77: 15)
           MOVE GS-FOTO-PRODUZIDA TO QTDE1-E
           MOVE QTDE1-E           TO LINDET-REL(92: 20)

           IF GS-IMPRIMIR = "S"
              WRITE REG-RELAT  FROM LINDET-REL AFTER 2
           ELSE
              MOVE SPACES TO REG-FILIAL
              WRITE REG-FILIAL
              WRITE REG-FILIAL FROM LINDET-REL
           END-IF

           MOVE "FOTO MONTADA.: " TO LINDET-REL(6: 15)
           MOVE GS-FOTO-MONTADA   TO QTDE1-E
           MOVE QTDE1-E           TO LINDET-REL(21: 20)
           MOVE "FOTO PERDIDA.: " TO LINDET-REL(41: 15)
           MOVE GS-FOTO-PERDIDA   TO QTDE1-E
           MOVE QTDE1-E           TO LINDET-REL(57: 20)
           MOVE "FOTO AVULSA..: " TO LINDET-REL(77: 15)
           MOVE GS-FOTO-AVULSA    TO QTDE1-E
           MOVE QTDE1-E           TO LINDET-REL(92: 20)
      *    WRITE REG-RELAT FROM LINDET-REL

           IF GS-IMPRIMIR = "S"
              WRITE REG-RELAT  FROM LINDET-REL
           ELSE
              WRITE REG-FILIAL FROM LINDET-REL
           END-IF

           MOVE SPACES            TO LINDET-REL
           MOVE "CLIENTE/ALBUM: " TO LINDET-REL(6: 15)
           MOVE GS-CLIENTE-ALBUM  TO QTDE1-E
           MOVE QTDE1-E           TO LINDET-REL(21: 20)
      *    WRITE REG-RELAT FROM LINDET-REL

           IF GS-IMPRIMIR = "S"
              WRITE REG-RELAT  FROM LINDET-REL
           ELSE
              WRITE REG-FILIAL FROM LINDET-REL
           END-IF

           MOVE SPACES            TO LINDET-REL
           MOVE "TIPO FOTO....: " TO LINDET-REL(6: 15)
           MOVE GS-COD-FOTO       TO QTDE1-E
           MOVE QTDE1-E           TO LINDET-REL(21:11)
           MOVE GS-DESC-FOTO      TO LINDET-REL(33: 45)
      *    WRITE REG-RELAT FROM LINDET-REL

           IF GS-IMPRIMIR = "S"
              WRITE REG-RELAT  FROM LINDET-REL
           ELSE
              WRITE REG-FILIAL FROM LINDET-REL
           END-IF


           MOVE "TIPO ALBUM...: " TO LINDET-REL(6: 15)
           MOVE GS-COD-ALBUM      TO QTDE1-E
           MOVE QTDE1-E           TO LINDET-REL(21:11)
           MOVE GS-DESC-ALBUM     TO LINDET-REL(33: 45)
      *    WRITE REG-RELAT FROM LINDET-REL

           IF GS-IMPRIMIR = "S"
              WRITE REG-RELAT  FROM LINDET-REL
           ELSE
              WRITE REG-FILIAL FROM LINDET-REL
           END-IF

           MOVE "TIPO FOLHA...: " TO LINDET-REL(6: 15)
           MOVE GS-COD-FOLHA      TO QTDE1-E
           MOVE QTDE1-E           TO LINDET-REL(21:11)
           MOVE GS-DESC-FOLHA     TO LINDET-REL(33: 45)
      *    WRITE REG-RELAT FROM LINDET-REL

           IF GS-IMPRIMIR = "S"
              WRITE REG-RELAT  FROM LINDET-REL
           ELSE
              WRITE REG-FILIAL FROM LINDET-REL
           END-IF

           MOVE "TIPO SEDA....: " TO LINDET-REL(6: 15)
           MOVE GS-COD-SEDA       TO QTDE1-E
           MOVE QTDE1-E           TO LINDET-REL(21:11)
           MOVE GS-DESC-SEDA      TO LINDET-REL(33: 45)
      *    WRITE REG-RELAT FROM LINDET-REL

           IF GS-IMPRIMIR = "S"
              WRITE REG-RELAT  FROM LINDET-REL
           ELSE
              WRITE REG-FILIAL FROM LINDET-REL
           END-IF

           ADD 8 TO LIN.


       MOVER-DADOS-RELATORIO SECTION.
           MOVE SPACES            TO LINDET-REL
           MOVE ALBUM-WK          TO LINDET-REL(01: 5)
           MOVE ESTOJO-WK         TO LINDET-REL(06: 3)
           MOVE ENCADERNACAO-WK   TO LINDET-REL(9: 3)
           MOVE FOLHA-WK          TO LINDET-REL(12: 5)
           MOVE FOTO-WK           TO LINDET-REL(17: 5)
           MOVE POSTER-WK         TO LINDET-REL(22: 2)
           MOVE FITA-WK           TO LINDET-REL(24: 2)
           MOVE PORTA-FITA-WK     TO LINDET-REL(26: 3)
           MOVE DVD-WK            TO LINDET-REL(30: 2)
           MOVE PORTA-DVD-WK      TO LINDET-REL(33: 2)
           MOVE FOTO-CD-WK        TO LINDET-REL(36: 2)
           MOVE MOLDURA-WK        TO LINDET-REL(39: 2)
           MOVE BOOK-WK           TO LINDET-REL(41: 2)
           MOVE NOME-FORMANDO-WK  TO LINDET-REL(43: 17)
           MOVE CIDADE-WK         TO LINDET-REL(62: 14)
           MOVE CURSO-WK          TO LINDET-REL(76: 16)
           MOVE FONE-WK           TO FONE-E
           MOVE FONE-E            TO LINDET-REL(92: 9)
           MOVE FOGO-WK           TO LINDET-REL(102: 2)
           MOVE VISITA-WK         TO LINDET-REL(104: 2)

           IF ALBUM-WK = 0
              MOVE 0              TO MASC-SEQ
              MOVE MASC-SEQ       TO LINDET-REL(108:4)
           ELSE
              ADD  1              TO SEQUENCIA
              MOVE SEQUENCIA      TO MASC-SEQ
              MOVE MASC-SEQ       TO LINDET-REL(108:4).

           IF GS-IMPRIMIR = "S"
              WRITE REG-RELAT FROM LINDET
           ELSE
              WRITE REG-FILIAL FROM LINDET
           END-IF

           ADD 1 TO LIN
           IF LIN > 56 PERFORM CABECALHO.

       TOTALIZA-REL SECTION.
           INITIALIZE LINTOT1
                      LINTOT2

           MOVE GS-TOT-ALBUM        TO LINTOT-ALBUM
           MOVE GS-TOT-ESTOJO       TO LINTOT-ESTOJO
           MOVE GS-TOT-ENCADERNACAO TO LINTOT-ENCADERNACAO
           MOVE GS-TOT-FOLHA        TO LINTOT-FOLHAS
           MOVE GS-TOT-FOTO         TO LINTOT-FOTOS
           MOVE GS-TOT-POSTER       TO LINTOT-POSTER
           MOVE GS-TOT-FITA         TO LINTOT-FITAS
           MOVE GS-TOT-PORTA-FITA   TO LINTOT-PORTA-FITA
           MOVE GS-TOT-DVD          TO LINTOT-DVD
           MOVE GS-TOT-PORTA-DVD    TO LINTOT-PORTA-DVD
           MOVE GS-TOT-FOTO-CD      TO LINTOT-FOTO-CD
           MOVE GS-TOT-MOLDURA      TO LINTOT-MOLDURAS
           MOVE GS-TOT-BOOK         TO LINTOT-BOOK

           IF GS-IMPRIMIR = "S"
              WRITE REG-RELAT  FROM CAB05a AFTER 2
              WRITE REG-RELAT  FROM CAB06
              WRITE REG-RELAT  FROM LINTOT1
              WRITE REG-RELAT  FROM CAB05b AFTER 2
              WRITE REG-RELAT  FROM CAB06
              WRITE REG-RELAT  FROM LINTOT2
           ELSE
              MOVE SPACES TO REG-FILIAL
              WRITE REG-FILIAL
              WRITE REG-FILIAL FROM CAB05a
              WRITE REG-FILIAL FROM CAB06
              WRITE REG-FILIAL FROM LINTOT1
              WRITE REG-FILIAL FROM CAB05b
              WRITE REG-FILIAL FROM CAB06
              WRITE REG-FILIAL  FROM LINTOT2
           END-IF.

      *    WRITE REG-RELAT FROM CAB05 AFTER 2.
      *    WRITE REG-RELAT FROM CAB06.
      *    WRITE REG-RELAT FROM LINTOT.
       CABECALHO SECTION.
           MOVE GS-CONTRATO      TO CONTRATO-REL.
           MOVE GS-IDENTIFICACAO TO INSTITUICAO-REL
           MOVE GS-DESCR-ORDEM   TO ORDEM-REL.
           ADD 1 TO PAG-W.
           MOVE PAG-W TO PG-REL.

           IF PAG-W = 1
              IF GS-IMPRIMIR = "S"
                 WRITE REG-RELAT FROM CAB01 AFTER 0
              ELSE
                 MOVE "INICIO" TO REG-FILIAL
                 WRITE REG-FILIAL
                 WRITE REG-FILIAL FROM CAB01
              END-IF
           ELSE
              IF GS-IMPRIMIR = "S"
                 WRITE REG-RELAT FROM CAB01 AFTER PAGE
              ELSE
                 MOVE "SALTAR PAGINA" TO REG-FILIAL
                 WRITE REG-FILIAL
                 MOVE "INICIO" TO REG-FILIAL
                 WRITE REG-FILIAL
                 WRITE REG-FILIAL FROM CAB01.

      *    IF PAG-W = 1
      *       WRITE REG-RELAT FROM CAB01 AFTER 0
      *    ELSE
      *       WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           IF GS-IMPRIMIR = "S"
              WRITE REG-RELAT FROM CAB02
              WRITE REG-RELAT FROM CAB02A
           ELSE
              WRITE REG-FILIAL FROM CAB02
              WRITE REG-FILIAL FROM CAB02A.


      *    WRITE REG-RELAT FROM CAB02.
      *    WRITE REG-RELAT FROM CAB02A AFTER 2.
           MOVE 3 TO LIN.

           IF PAG-W = 1
              PERFORM CABECALHO-LISTA-REL.

           IF GS-IMPRIMIR = "S"
              WRITE REG-RELAT FROM CAB03
              WRITE REG-RELAT FROM CAB04
              WRITE REG-RELAT FROM CAB03
           ELSE
              WRITE REG-FILIAL FROM CAB03
              WRITE REG-FILIAL FROM CAB04
              WRITE REG-FILIAL FROM CAB03.

      *    WRITE REG-RELAT FROM CAB03.
      *    WRITE REG-RELAT FROM CAB04.
      *    WRITE REG-RELAT FROM CAB03.
           ADD 4 TO LIN.

       140-exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD040 IED011 MTD001 MTD019 MTD020 CEAD010 WORK.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

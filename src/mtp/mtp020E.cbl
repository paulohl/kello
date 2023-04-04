       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP020E.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 25/08/2016.
      *FUNÇÃO: ESCALA DE VENDAS DOS PRODUTOS

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window              is class "wclass"
           AListview           is class "alistview".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX004.
           COPY COPX040.
           COPY MTPX020E.
           COPY LOGX005.
           COPY LOGACESS.SEL.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW004.
           COPY COPW040.
           COPY MTPW020E.
           COPY LOGW005.
           COPY LOGACESS.FD.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "MTP020E.CPB".
           COPY "MTP020E.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-MTD020E            PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-LOG005             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATAMOV-W             PIC 9(8)     VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  IDENTIFICACAO-W       PIC X(30)    VALUE SPACES.
           05  QT-FITA-W             PIC 9        VALUE ZEROS.
           05  QT-DVD-W              PIC 9        VALUE ZEROS.
           05  QT-PFITA-W            PIC 9        VALUE ZEROS.
           05  VISITA-W              PIC 9(3)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  TOTAL-AVULSA          PIC 9(09).
           05  TOTAL-FOTOS           PIC 9(09).
           05  TOTAL-CLIENTES        PIC 9(09).
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  LIBERADO              PIC 9(01)    VALUE ZEROS.

           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
      *    COPY "PARAMETR".
           COPY IMPRESSORA.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(48)   VALUE
           "MOVIMENTO PLANILHA MONTAGEM ALBUM".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(5)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)  VALUE
           "ALB. ES EN FOLH FOTO PO FI PF DVD FC MO PD BK NOME-CLIENTE
      -    "        MES/ANO V".

       01  LINDET.
           05  LINDET-REL          PIC X(90)  VALUE SPACES.

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

       01 masc-qtde                    PIC Z.ZZZ.ZZ9 BLANK WHEN ZEROS.

       01 lnktabela.
          02 lnkobjetoscol object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas
                           pic 9(09) comp-5 value zeros occurs 99 times.

       01 indice           pic 9(02).

       01 wssize           pic 9(09) comp-5 value zeros.
       01 wsIndice         pic 9(09) comp-5 value zeros.

       77 wsTexto          pic x(255) value spaces.
       77 wsItem           pic 9(009) comp-5 value zeros.
       77 UMITEM           object reference.
       77 UMOBJETO         object reference.

       01 lnkusu.
          copy usuario.cpy.

       LINKAGE SECTION.
           COPY "PARAMETR".

       01 LNK-CONTRATO                 PIC 9(04).

       PROCEDURE DIVISION USING PARAMETROS-W LNK-CONTRATO.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
      *    ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-CAD004.
           MOVE "COD040"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-COD040.
           MOVE "MTD020E"          TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-MTD020E.
           MOVE "LOG005"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-LOG005.
           MOVE "LOGACESS"         TO ARQ-REC.
           MOVE EMPRESA-REF        TO ARQUIVO-LOGACESS

           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario

           OPEN I-O   LOG005 MTD020E
           CLOSE      MTD020E
           OPEN I-O   MTD020E

           OPEN INPUT COD040 CAD004.

           IF ST-MTD020E = "35"
              CLOSE MTD020E      OPEN OUTPUT MTD020E
              CLOSE MTD020E      OPEN I-O MTD020E
           END-IF.
           IF ST-LOG005 = "35"
              CLOSE LOG005      OPEN OUTPUT LOG005
              CLOSE LOG005      OPEN I-O LOG005
           END-IF.
           IF ST-LOG005 <> "00"
              MOVE "ERRO ABERTURA LOG005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020E <> "00"
              MOVE "ERRO ABERTURA MTD020E: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020E TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           CLOSE      LOG005 MTD020E
           OPEN INPUT MTD020E

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP020E"           to logacess-programa
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

           move lnk-contrato        to gs-contrato
                                       nr-contrato-co40
           read cod040 invalid key
                initialize reg-cod040.

           move identificacao-co40  to gs-identificacao

           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   IF GS-TIPO-GRAVACAO = 1
                      PERFORM REGRAVA-DADOS
                   ELSE
                      PERFORM GRAVA-DADOS
                   END-IF
                   PERFORM LIMPAR-DADOS
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   SET-FOCUS EF-PRECO-ESTOJO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop"

          move function current-date to ws-data-sys

          MOVE "SENHA69"         TO PROGRAMA-CA004
          MOVE COD-USUARIO-W     TO COD-USUARIO-CA004
          READ CAD004 INVALID KEY
               DISABLE-OBJECT PB1
               DISABLE-OBJECT PB3
          NOT INVALID KEY
               ENABLE-OBJECT PB1
               ENABLE-OBJECT PB3
          END-READ.

      *----------------------------------------------------------------
       EXCLUI SECTION.
           CLOSE    MTD020E
           OPEN I-O LOG005 MTD020E

           MOVE GS-CONTRATO   TO CONTRATO-MTG-E
           READ MTD020E INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE MTD020E NOT INVALID KEY
                       MOVE USUARIO-W     TO LOG5-USUARIO
                       MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                       MOVE WS-DATA-CPU   TO LOG5-DATA
                       ACCEPT WS-HORA-SYS FROM TIME
                       MOVE WS-HORA-SYS   TO LOG5-HORAS
                       MOVE "E"           TO LOG5-OPERACAO
                       MOVE "MTD020E"     TO LOG5-ARQUIVO
                       MOVE "MTP020E"     TO LOG5-PROGRAMA
                       MOVE REG-MTD020E   TO LOG5-REGISTRO
                       WRITE REG-LOG005
                       END-WRITE
                END-DELETE
           END-READ

           CLOSE      LOG005 MTD020E
           OPEN INPUT MTD020E

           PERFORM LIMPAR-DADOS.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.

       LIMPAR-DADOS SECTION.
           MOVE GS-CONTRATO      TO CONTRATO-W
           INITIALIZE REG-MTD020E
           INITIALIZE GS-DATA-BLOCK
           MOVE CONTRATO-W       TO GS-CONTRATO
           MOVE IDENTIFICACAO-W  TO GS-IDENTIFICACAO
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO               TO CONTRATO-MTG-E
           MOVE GS-PRECO-ESTOJO           TO PRECO-ESTOJO-MTG-E
           MOVE GS-QTDE-ACIMA-DE-ESTOJO   TO QTDE-ACIMA-DE-ESTOJO
           MOVE GS-PERC-DESC-FORM-ESTOJO  TO PERC-DESC-FORM-ESTOJO
           MOVE GS-PERC-DESC-COM-ESTOJO   TO PERC-DESC-COM-ESTOJO

           MOVE GS-PRECO-ENCADER          TO PRECO-ENCADER-MTG-E
           MOVE GS-QTDE-ACIMA-DE-ENCADER  TO QTDE-ACIMA-DE-ENCADER
           MOVE GS-PERC-DESC-FORM-ENCADER TO PERC-DESC-FORM-ENCADER
           MOVE GS-PERC-DESC-COM-ENCADER  TO PERC-DESC-COM-ENCADER

           MOVE GS-PRECO-FOLHA            TO PRECO-FOLHA-MTG-E
           MOVE GS-QTDE-ACIMA-DE-FOLHA    TO QTDE-ACIMA-DE-FOLHA
           MOVE GS-PERC-DESC-FORM-FOLHA   TO PERC-DESC-FORM-FOLHA
           MOVE GS-PERC-DESC-COM-FOLHA    TO PERC-DESC-COM-FOLHA

           MOVE GS-PRECO-FOTO             TO PRECO-FOTO-MTG-E
           MOVE GS-QTDE-ACIMA-DE-FOTO     TO QTDE-ACIMA-DE-FOTO
           MOVE GS-PERC-DESC-FORM-FOTO    TO PERC-DESC-FORM-FOTO
           MOVE GS-PERC-DESC-COM-FOTO     TO PERC-DESC-COM-FOTO

           MOVE GS-PRECO-FITA             TO PRECO-FITA-MTG-E
           MOVE GS-QTDE-ACIMA-DE-FITA     TO QTDE-ACIMA-DE-FITA
           MOVE GS-PERC-DESC-FORM-FITA    TO PERC-DESC-FORM-FITA
           MOVE GS-PERC-DESC-COM-FITA     TO PERC-DESC-COM-FITA

           MOVE GS-PRECO-DVD              TO PRECO-DVD-MTG-E
           MOVE GS-QTDE-ACIMA-DE-DVD      TO QTDE-ACIMA-DE-DVD
           MOVE GS-PERC-DESC-FORM-DVD     TO PERC-DESC-FORM-DVD
           MOVE GS-PERC-DESC-COM-DVD      TO PERC-DESC-COM-DVD

           MOVE GS-PRECO-POSTER           TO PRECO-POSTER-MTG-E
           MOVE GS-QTDE-ACIMA-DE-POSTER   TO QTDE-ACIMA-DE-POSTER
           MOVE GS-PERC-DESC-FORM-POSTER  TO PERC-DESC-FORM-POSTER
           MOVE GS-PERC-DESC-COM-POSTER   TO PERC-DESC-COM-POSTER

           MOVE GS-PRECO-PORTA-FITA       TO PRECO-PORTA-FITA-MTG-E
           MOVE GS-QTDE-ACIMA-DE-PT-FITA  TO QTDE-ACIMA-DE-PT-FITA
           MOVE GS-PERC-DESC-FORM-PT-FITA TO PERC-DESC-FORM-PT-FITA
           MOVE GS-PERC-DESC-COM-PT-FITA  TO PERC-DESC-COM-PT-FITA

           MOVE GS-PRECO-FOTO-CD          TO PRECO-FOTO-CD-MTG-E
           MOVE GS-QTDE-ACIMA-DE-FT-CD    TO QTDE-ACIMA-DE-FT-CD
           MOVE GS-PERC-DESC-FORM-FT-CD   TO PERC-DESC-FORM-FT-CD
           MOVE GS-PERC-DESC-COM-FT-CD    TO PERC-DESC-COM-FT-CD

           MOVE GS-PRECO-MOLDURA          TO PRECO-MOLDURA-MTG-E
           MOVE GS-QTDE-ACIMA-DE-MOLDURA  TO QTDE-ACIMA-DE-MOLDURA
           MOVE GS-PERC-DESC-FORM-MOLDURA TO PERC-DESC-FORM-MOLDURA
           MOVE GS-PERC-DESC-COM-MOLDURA  TO PERC-DESC-COM-MOLDURA

           MOVE GS-PRECO-PORTA-DVD        TO PRECO-PORTA-DVD-MTG-E
           MOVE GS-QTDE-ACIMA-DE-PT-DVD   TO QTDE-ACIMA-DE-PT-DVD
           MOVE GS-PERC-DESC-FORM-PT-DVD  TO PERC-DESC-FORM-PT-DVD
           MOVE GS-PERC-DESC-COM-PT-DVD   TO PERC-DESC-COM-PT-DVD

           MOVE GS-PRECO-BOOK             TO PRECO-BOOK-MTG-E
           MOVE GS-QTDE-ACIMA-DE-BOOK     TO QTDE-ACIMA-DE-BOOK
           MOVE GS-PERC-DESC-FORM-BOOK    TO PERC-DESC-FORM-BOOK
           MOVE GS-PERC-DESC-COM-BOOK     TO PERC-DESC-COM-BOOK

           MOVE GS-PRECO-PORTA-RETRATO      TO PRECO-PORTA-RETRATO-MTG-E
           MOVE GS-QTDE-ACIMA-DE-PORTA-RET  TO QTDE-ACIMA-DE-PT-RET
           MOVE GS-PERC-DESC-FORM-PORTA-RET TO PERC-DESC-FORM-PT-RET
           MOVE GS-PERC-DESC-COM-PORTA-RET  TO PERC-DESC-COM-PT-RET

           MOVE GS-PRECO-PENDRIVE           TO PRECO-PENDRIVE-MTG-E
           MOVE GS-QTDE-ACIMA-DE-PENDRIVE   TO QTDE-ACIMA-DE-PENDRIVE
           MOVE GS-PERC-DESC-FORM-PENDRIVE  TO PERC-DESC-FORM-PENDRIVE
           MOVE GS-PERC-DESC-COM-PENDRIVE   TO PERC-DESC-COM-PENDRIVE

           MOVE GS-PRECO-VIDEO-HD             TO PRECO-VIDEO-HD-MTG-E
           MOVE GS-QTDE-ACIMA-DE-PRECO-VIDEO  TO QTDE-ACIMA-DE-VIDEO-HD
           MOVE GS-PERC-DESC-FORM-PRECO-VIDEO TO PERC-DESC-FORM-VIDEO-HD
           MOVE GS-PERC-DESC-COM-PRECO-VIDEO  TO PERC-DESC-COM-VIDEO-HD

           MOVE GS-PRECO-REVISTA              TO PRECO-REVISTA-MTG-E
           MOVE GS-QTDE-ACIMA-DE-REVISTA      TO QTDE-ACIMA-DE-REVISTA
           MOVE GS-PERC-DESC-FORM-REVISTA     TO PERC-DESC-FORM-REVISTA
           MOVE GS-PERC-DESC-COM-REVISTA      TO PERC-DESC-COM-REVISTA

           MOVE GS-PRECO-CALENDARIO           TO PRECO-CALENDARIO-MTG-E
           MOVE GS-QTDE-ACIMA-DE-CALENDARIO   TO QTDE-ACIMA-DE-CALEND
           MOVE GS-PERC-DESC-FORM-CALENDARIO  TO PERC-DESC-FORM-CALEND
           MOVE GS-PERC-DESC-COM-CALENDARIO   TO PERC-DESC-COM-CALEND

           MOVE GS-TAXA-JUROS                 TO TAXA-JUROS-MTG-E
           MOVE GS-CARENCIA-JUROS             TO CARENCIA-JUROS-MTG-E
           MOVE GS-TAXA-DESCONTO              TO TAXA-DESCONTO-MTG-E

           MOVE GS-PM-AVISTA                  TO PM-AVISTA-MTG-E.


       GRAVA-DADOS SECTION.
           CLOSE    MTD020E
           OPEN I-O LOG005 MTD020E
           MOVE SPACES TO MENSAGEM

           WRITE REG-MTD020E INVALID KEY
                 MOVE SPACES                       TO MENSAGEM
                 STRING "Erro ao Gravar o MTD020E" X"0A"
                        "Status => " ST-MTD020E INTO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                 MOVE USUARIO-W     TO LOG5-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU   TO LOG5-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS   TO LOG5-HORAS
                 MOVE "I"           TO LOG5-OPERACAO
                 MOVE "MTD020E"     TO LOG5-ARQUIVO
                 MOVE "MTP020E"     TO LOG5-PROGRAMA
                 MOVE REG-MTD020E   TO LOG5-REGISTRO
                 WRITE REG-LOG005
                 END-WRITE
           END-WRITE

           CLOSE      LOG005 MTD020E
           OPEN INPUT MTD020E.

       REGRAVA-DADOS SECTION.
           CLOSE    MTD020E
           OPEN I-O LOG005 MTD020E
           REWRITE REG-MTD020E INVALID KEY
                 MOVE "Erro Regravacao MTD020E" TO GS-MENSAGEM-ERRO
                 MOVE ST-MTD020E TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 MOVE USUARIO-W    TO LOG5-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU  TO LOG5-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS  TO LOG5-HORAS
                 MOVE "A"          TO LOG5-OPERACAO
                 MOVE "MTD020E"    TO LOG5-ARQUIVO
                 MOVE "MTP020E"    TO LOG5-PROGRAMA
                 MOVE REG-MTD020E  TO LOG5-REGISTRO
                 WRITE REG-LOG005
                 END-WRITE
           END-REWRITE

           CLOSE      LOG005 MTD020E
           OPEN INPUT MTD020E.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       CARREGA-ULTIMOS SECTION.
           MOVE GS-CONTRATO          TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY
                MOVE SPACES          TO IDENTIFICACAO-CO40
           END-READ
           MOVE IDENTIFICACAO-CO40   TO GS-IDENTIFICACAO.

           MOVE GS-CONTRATO          TO CONTRATO-MTG-E
           READ MTD020E INVALID KEY
                INITIALIZE REG-MTD020E
                MOVE 0 TO GS-TIPO-GRAVACAO
           NOT INVALID KEY
                MOVE 1 TO GS-TIPO-GRAVACAO
           END-READ

           MOVE PRECO-ESTOJO-MTG-E         TO GS-PRECO-ESTOJO
           MOVE QTDE-ACIMA-DE-ESTOJO       TO GS-QTDE-ACIMA-DE-ESTOJO
           MOVE PERC-DESC-FORM-ESTOJO      TO GS-PERC-DESC-FORM-ESTOJO
           MOVE PERC-DESC-COM-ESTOJO       TO GS-PERC-DESC-COM-ESTOJO

           MOVE PRECO-ENCADER-MTG-E        TO GS-PRECO-ENCADER
           MOVE QTDE-ACIMA-DE-ENCADER      TO GS-QTDE-ACIMA-DE-ENCADER
           MOVE PERC-DESC-FORM-ENCADER     TO GS-PERC-DESC-FORM-ENCADER
           MOVE PERC-DESC-COM-ENCADER      TO GS-PERC-DESC-COM-ENCADER

           MOVE PRECO-FOLHA-MTG-E          TO GS-PRECO-FOLHA
           MOVE QTDE-ACIMA-DE-FOLHA        TO GS-QTDE-ACIMA-DE-FOLHA
           MOVE PERC-DESC-FORM-FOLHA       TO GS-PERC-DESC-FORM-FOLHA
           MOVE PERC-DESC-COM-FOLHA        TO GS-PERC-DESC-COM-FOLHA

           MOVE PRECO-FOTO-MTG-E           TO GS-PRECO-FOTO
           MOVE QTDE-ACIMA-DE-FOTO         TO GS-QTDE-ACIMA-DE-FOTO
           MOVE PERC-DESC-FORM-FOTO        TO GS-PERC-DESC-FORM-FOTO
           MOVE PERC-DESC-COM-FOTO         TO GS-PERC-DESC-COM-FOTO

           MOVE PRECO-FITA-MTG-E           TO GS-PRECO-FITA
           MOVE QTDE-ACIMA-DE-FITA         TO GS-QTDE-ACIMA-DE-FITA
           MOVE PERC-DESC-FORM-FITA        TO GS-PERC-DESC-FORM-FITA
           MOVE PERC-DESC-COM-FITA         TO GS-PERC-DESC-COM-FITA

           MOVE PRECO-DVD-MTG-E            TO GS-PRECO-DVD
           MOVE QTDE-ACIMA-DE-DVD          TO GS-QTDE-ACIMA-DE-DVD
           MOVE PERC-DESC-FORM-DVD         TO GS-PERC-DESC-FORM-DVD
           MOVE PERC-DESC-COM-DVD          TO GS-PERC-DESC-COM-DVD

           MOVE PRECO-POSTER-MTG-E         TO GS-PRECO-POSTER
           MOVE QTDE-ACIMA-DE-POSTER       TO GS-QTDE-ACIMA-DE-POSTER
           MOVE PERC-DESC-FORM-POSTER      TO GS-PERC-DESC-FORM-POSTER
           MOVE PERC-DESC-COM-POSTER       TO GS-PERC-DESC-COM-POSTER

           MOVE PRECO-PORTA-FITA-MTG-E     TO GS-PRECO-PORTA-FITA
           MOVE QTDE-ACIMA-DE-PT-FITA      TO GS-QTDE-ACIMA-DE-PT-FITA
           MOVE PERC-DESC-FORM-PT-FITA     TO GS-PERC-DESC-FORM-PT-FITA
           MOVE PERC-DESC-COM-PT-FITA      TO GS-PERC-DESC-COM-PT-FITA

           MOVE PRECO-FOTO-CD-MTG-E        TO GS-PRECO-FOTO-CD
           MOVE QTDE-ACIMA-DE-FT-CD        TO GS-QTDE-ACIMA-DE-FT-CD
           MOVE PERC-DESC-FORM-FT-CD       TO GS-PERC-DESC-FORM-FT-CD
           MOVE PERC-DESC-COM-FT-CD        TO GS-PERC-DESC-COM-FT-CD

           MOVE PRECO-MOLDURA-MTG-E        TO GS-PRECO-MOLDURA
           MOVE QTDE-ACIMA-DE-MOLDURA      TO GS-QTDE-ACIMA-DE-MOLDURA
           MOVE PERC-DESC-FORM-MOLDURA     TO GS-PERC-DESC-FORM-MOLDURA
           MOVE PERC-DESC-COM-MOLDURA      TO GS-PERC-DESC-COM-MOLDURA

           MOVE PRECO-PORTA-DVD-MTG-E      TO GS-PRECO-PORTA-DVD
           MOVE QTDE-ACIMA-DE-PT-DVD       TO GS-QTDE-ACIMA-DE-PT-DVD
           MOVE PERC-DESC-FORM-PT-DVD      TO GS-PERC-DESC-FORM-PT-DVD
           MOVE PERC-DESC-COM-PT-DVD       TO GS-PERC-DESC-COM-PT-DVD

           MOVE PRECO-BOOK-MTG-E           TO GS-PRECO-BOOK
           MOVE QTDE-ACIMA-DE-BOOK         TO GS-QTDE-ACIMA-DE-BOOK
           MOVE PERC-DESC-FORM-BOOK        TO GS-PERC-DESC-FORM-BOOK
           MOVE PERC-DESC-COM-BOOK         TO GS-PERC-DESC-COM-BOOK

           MOVE PRECO-PORTA-RETRATO-MTG-E TO GS-PRECO-PORTA-RETRATO
           MOVE QTDE-ACIMA-DE-PT-RET      TO GS-QTDE-ACIMA-DE-PORTA-RET
           MOVE PERC-DESC-FORM-PT-RET     TO GS-PERC-DESC-FORM-PORTA-RET
           MOVE PERC-DESC-COM-PT-RET      TO GS-PERC-DESC-COM-PORTA-RET

           MOVE PRECO-PENDRIVE-MTG-E      TO GS-PRECO-PENDRIVE
           MOVE QTDE-ACIMA-DE-PENDRIVE    TO GS-QTDE-ACIMA-DE-PENDRIVE
           MOVE PERC-DESC-FORM-PENDRIVE   TO GS-PERC-DESC-FORM-PENDRIVE
           MOVE PERC-DESC-COM-PENDRIVE    TO GS-PERC-DESC-COM-PENDRIVE

           MOVE PRECO-VIDEO-HD-MTG-E    TO GS-PRECO-VIDEO-HD
           MOVE QTDE-ACIMA-DE-VIDEO-HD  TO GS-QTDE-ACIMA-DE-PRECO-VIDEO
           MOVE PERC-DESC-FORM-VIDEO-HD TO GS-PERC-DESC-FORM-PRECO-VIDEO
           MOVE PERC-DESC-COM-VIDEO-HD  TO GS-PERC-DESC-COM-PRECO-VIDEO

           MOVE PRECO-REVISTA-MTG-E     TO GS-PRECO-REVISTA
           MOVE QTDE-ACIMA-DE-REVISTA   TO GS-QTDE-ACIMA-DE-REVISTA
           MOVE PERC-DESC-FORM-REVISTA  TO GS-PERC-DESC-FORM-REVISTA
           MOVE PERC-DESC-COM-REVISTA   TO GS-PERC-DESC-COM-REVISTA

           MOVE PRECO-CALENDARIO-MTG-E  TO GS-PRECO-CALENDARIO
           MOVE QTDE-ACIMA-DE-CALEND    TO GS-QTDE-ACIMA-DE-CALENDARIO
           MOVE PERC-DESC-FORM-CALEND   TO GS-PERC-DESC-FORM-CALENDARIO
           MOVE PERC-DESC-COM-CALEND    TO GS-PERC-DESC-COM-CALENDARIO

           MOVE TAXA-JUROS-MTG-E        TO GS-TAXA-JUROS
           MOVE CARENCIA-JUROS-MTG-E    TO GS-CARENCIA-JUROS
           MOVE TAXA-DESCONTO-MTG-E     TO GS-TAXA-DESCONTO

           MOVE PM-AVISTA-MTG-E         TO GS-PM-AVISTA

           REFRESH-OBJECT PRINCIPAL.

      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP020E"   TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
       EXIBIR-MENSAGEM SECTION.
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
           CLOSE COD040 CAD004 MTD020E.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP020E"           to logacess-programa
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

           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP500.
       DATE-WRITTEN. 15/03/2000.
       AUTHOR. MARELI AMÂNCIO VOLPATO.
      *PROGRAMA: CRONOGRAMA DE EVENTOS
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CAPX012.
           COPY COPX003.
           COPY COPX040.
           COPY COPX060.
           COPY COPX061.
           COPY IEPX011.
           COPY PARX001.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK = NOME-CIDADE-WK DATA-WK
                                           CONTRATO-WK EVENTO-WK
                                           HORAS-WK LOCAL-WK
                                           COD-EVENTO-WK.

           SELECT WORK2 ASSIGN TO VARIA-W2
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK2
                  RECORD KEY IS CHAVE-WK2 = DATA-WK2
      *                                     CIDADE-WK2
                                            CONTRATO-WK2.

           SELECT WORK3 ASSIGN TO VARIA-W3
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK3
                  RECORD KEY IS CHAVE-WK3 = DATA-WK3
                                            CONTRATO-WK3
                                            HORAS-WK3.

           SELECT WORK4 ASSIGN TO VARIA-W4
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK4
                  RECORD KEY IS CHAVE-WK4 = EVENTO-WK4
                                            COD-EVENTO-WK4
                                            DATA-WK4.


           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW012.
       COPY COPW003.
       COPY COPW040.
       COPY COPW060.
       COPY COPW061.
       COPY IEPW011.
       COPY PARW001.

       FD  WORK.
       01  REG-WORK.
           05  DATA-WK             PIC 9(8).
           05  CONTRATO-WK         PIC 9(4).
           05  CIDADE-WK           PIC 9(4).
           05  NOME-CIDADE-WK      PIC X(13).
           05  HORAS-WK            PIC X(5).
           05  LOCAL-WK            PIC X(25).
           05  COD-EVENTO-WK       PIC 9(5).
           05  EVENTO-WK           PIC X(5).
           05  CURSO-WK            PIC X(3).
           05  FORM-WK             PIC 9(4).
           05  COBERTURA-WK        PIC X(5).
           05  RELAT-ESTAT-WK      PIC 9(1).

       FD  WORK2.
       01  REG-WORK2.
           05  DATA-WK2             PIC 9(8).
           05  CONTRATO-WK2         PIC 9(4).
           05  CIDADE-WK2           PIC 9(4).
           05  NOME-CIDADE-WK2      PIC X(13).
           05  FORM-WK2             PIC 9(4).
           05  QT-CONTRATO-WK2      PIC 9(2).

       FD  WORK3.
       01  REG-WORK3.
           05  DATA-WK3             PIC 9(8).
           05  CONTRATO-WK3         PIC 9(4).
           05  HORAS-WK3            PIC x(5).

       FD  WORK4.
       01  REG-WORK4.
           05  DATA-WK4             PIC 9(8).
           05  COD-EVENTO-WK4       PIC 9(05).
           05  EVENTO-WK4           PIC x(15).
           05  QTDE-WK4             PIC 9(4).


       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(230).
       WORKING-STORAGE SECTION.
           COPY "REP500.CPB".
           COPY "REP500.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-COD061             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-PAR001             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK2              PIC XX       VALUE SPACES.
           05  ST-WORK3              PIC XX       VALUE SPACES.
           05  ST-WORK4              PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 99       VALUE ZEROS.
           05  AUX-EVENTO            PIC X(15)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VARIA-W2              PIC 9(8)     VALUE ZEROS.
           05  VARIA-W3              PIC 9(8)     VALUE ZEROS.
           05  VARIA-W4              PIC 9(8)     VALUE ZEROS.
           05  NOME-CIDADE-ANT       PIC X(13)    VALUE SPACES.
      *    CIDADE-ANT - CONTROLA QUAL A ULTIMA CIDADE UTILIZADA
           05  I-INI                 PIC 9(4)     VALUE ZEROS.
      *    I-INI = CONTROLA O CONTADOR DA 1o. LINHA ANTES DA CIDADE
           05  I-ULT                 PIC 9(4)     VALUE ZEROS.
      *    I-ULT = CONTROLA O ULTIMO CONTADOR
           05  I                     PIC 9(4)     VALUE ZEROS.
      *    I - CONTADOR DA LINHA CORRENTE
      *    I1..I7 - CONTROLE DO CONTADOR DENTRO DE CADA DATA
           05  I1                    PIC 9(4)     VALUE ZEROS.
           05  I2                    PIC 9(4)     VALUE ZEROS.
           05  I3                    PIC 9(4)     VALUE ZEROS.
           05  I4                    PIC 9(4)     VALUE ZEROS.
           05  I5                    PIC 9(4)     VALUE ZEROS.
           05  I6                    PIC 9(4)     VALUE ZEROS.
           05  I7                    PIC 9(4)     VALUE ZEROS.

      *    VARIAVEIS P/ CALCULAR A QTDE DE FOTOG/CINEG/FORM
           05  QTDE-W                PIC 9(8)     VALUE ZEROS.
           05  QTDE-E                PIC ZZ.ZZZ.ZZZ.
           05  FORM1-W               PIC 9(8)     VALUE ZEROS.
           05  FORM2-W               PIC 9(8)     VALUE ZEROS.
           05  FORM3-W               PIC 9(8)     VALUE ZEROS.
           05  FORM4-W               PIC 9(8)     VALUE ZEROS.
           05  FORM5-W               PIC 9(8)     VALUE ZEROS.
           05  FORM6-W               PIC 9(8)     VALUE ZEROS.
           05  FORM7-W               PIC 9(8)     VALUE ZEROS.

           05  LINHA-DETALHE OCCURS 1000 TIMES.
               10 LINDET-W           PIC X(226).
      *    DATA1-INV DATA2-INV E DATA3-INV = 3 DIAS APRESENTADOS
      *    OPCAO-SEMANA - QUAL INTERVALO DE SEMANA FOI APRESENTADO
           05  DATA1-INV              PIC 9(8)     VALUE ZEROS.
           05  DATA2-INV              PIC 9(8)     VALUE ZEROS.
           05  DATA3-INV              PIC 9(8)     VALUE ZEROS.
           05  OPCAO-SEMANA           PIC 9        VALUE ZEROS.
           05  DATA-E                 PIC ZZ/ZZ/ZZZZ.
      *    DATA-SEG..DATA-DOM = SEG..DOM
           05  DATA-SEG               PIC 9(8)     VALUE ZEROS.
           05  DATA-TER               PIC 9(8)     VALUE ZEROS.
           05  DATA-QUA               PIC 9(8)     VALUE ZEROS.
           05  DATA-QUI               PIC 9(8)     VALUE ZEROS.
           05  DATA-SEX               PIC 9(8)     VALUE ZEROS.
           05  DATA-SAB               PIC 9(8)     VALUE ZEROS.
           05  DATA-DOM               PIC 9(8)     VALUE ZEROS.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           05  TOT1-CONTRATO         PIC 9(8)     VALUE ZEROS.
           05  TOT2-CONTRATO         PIC 9(8)     VALUE ZEROS.
           05  TOT3-CONTRATO         PIC 9(8)     VALUE ZEROS.
           05  TOT4-CONTRATO         PIC 9(8)     VALUE ZEROS.
           05  TOT5-CONTRATO         PIC 9(8)     VALUE ZEROS.
           05  TOT6-CONTRATO         PIC 9(8)     VALUE ZEROS.
           05  TOT7-CONTRATO         PIC 9(8)     VALUE ZEROS.
           05  TOT1-EVENTOS          PIC 9(8)     VALUE ZEROS.
           05  TOT2-EVENTOS          PIC 9(8)     VALUE ZEROS.
           05  TOT3-EVENTOS          PIC 9(8)     VALUE ZEROS.
           05  TOT4-EVENTOS          PIC 9(8)     VALUE ZEROS.
           05  TOT5-EVENTOS          PIC 9(8)     VALUE ZEROS.
           05  TOT6-EVENTOS          PIC 9(8)     VALUE ZEROS.
           05  TOT7-EVENTOS          PIC 9(8)     VALUE ZEROS.
           05  TOT1-EVENTOS2         PIC 9(8)     VALUE ZEROS.
           05  NUM-CONTROLE          PIC 9(05)    VALUE ZEROS.
           05  AUX-QTDE              PIC 9(08)    VALUE ZEROS.
           05  AUX-QTDE2             PIC 9(08)    VALUE ZEROS.
           05  WS-STATUS-REVENDIDO   PIC 9(02)    VALUE ZEROS.
           05  WS-STATUS-ANALISE     PIC 9(02)    VALUE ZEROS.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  AUX-DATA                    PIC 9(08).
       01  FILLER REDEFINES AUX-DATA.
           05 AUX-ANO                  PIC 9(04).
           05 AUX-MES                  PIC 9(02).
           05 AUX-DIA                  PIC 9(02).

       01  CAB01.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(50)  VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(20)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(2)    VALUE SPACES.
      *    05  FILLER              PIC X(226)   VALUE
           05  FILLER              PIC X(032)   VALUE
           "CRONOGRAMA DE EVENTOS           ".
           05  FILLER              PIC X(13)
               VALUE "QUAL CIDADE: ".
           05  CIDADE-REL          PIC X(20).
       01  CAB03.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(226)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  FILLER              PIC X(226)  VALUE
           "ICIDADE              ICON CUR-EVEN HORAS-FOR PVTBCICON CUR-E
      -    "VEN HORAS-FOR PVTBCICON CUR-EVEN HORAS-FOR PVTBCICON CUR-EVE
      -    "N HORAS-FOR PVTBCICON CUR-EVEN HORAS-FOR PVTBCICON CUR-EVEN
      -    "HORAS-FOR PVTBCICON CUR-EVEN HORAS-FOR PVTBC".


       01  CAB01-P.
           05  FILLER              PIC X(20)  VALUE SPACES.
           05  EMPRESA-REL-P       PIC X(50)  VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL-P       PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL-P          PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(20)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL-P            PIC Z9      VALUE ZEROS.
       01  CAB02-P.
           05  FILLER              PIC X(20)    VALUE SPACES.
      *    05  FILLER              PIC X(226)   VALUE
           05  FILLER              PIC X(032)   VALUE
           "CRONOGRAMA DE EVENTOS           ".
           05  FILLER              PIC X(13)
               VALUE "QUAL CIDADE: ".
           05 CIDADE-REL-P         PIC X(20).
       01  CAB03-P.
           05  FILLER              PIC X(20)   VALUE SPACES.
           05  FILLER              PIC X(226)  VALUE ALL "=".

       01  CAB04-P.
           05  FILLER              PIC X(20)   VALUE SPACES.
           05  FILLER              PIC X(226)  VALUE
           "ICIDADE              ICON CUR-EVEN HORAS-FOR PVTBCICON CUR-E
      -    "VEN HORAS-FOR PVTBCICON CUR-EVEN HORAS-FOR PVTBCI".

       01  LINDET.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  LINDET-REL          PIC X(226)   VALUE SPACES.

       01  LINDET-P.
           05  FILLER              PIC X(20)    VALUE SPACES.
           05  LINDET-REL-P        PIC X(226)   VALUE SPACES.

       01 MENSAGEM                 PIC X(200).
       01 TIPO-MSG                 PIC X(01).
       01 RESP-MSG                 PIC X(01).

           copy impressora.

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
           MOVE "COD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD060"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "COD061"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD061.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "PAR001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PAR001.
           OPEN INPUT CAD010 COD003 COD040 COD060 COD061 IED011
                      CAD012 PAR001.
           ACCEPT VARIA-W FROM TIME.
           ADD 1000       TO VARIA-W2
           ADD 2000       TO VARIA-W3
           ADD 3000       TO VARIA-W4
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD061 <> "00"
              MOVE "ERRO ABERTURA COD061: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD061 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE 1 TO CHAVE-PAR001
           READ PAR001 INVALID KEY
                MOVE "Parametrização do Brinde Não Cadastrada"
                TO GS-MENSAGEM-ERRO
                PERFORM CARREGA-MENSAGEM-ERRO
           NOT INVALID KEY
                IF STATUS-REVENDIDO-PAR001 IS NOT NUMERIC
                   MOVE 0 TO STATUS-REVENDIDO-PAR001
                END-IF
                MOVE STATUS-REVENDIDO-PAR001 TO WS-STATUS-REVENDIDO
                IF STATUS-ANALISE-PAR001 IS NOT NUMERIC
                   MOVE 0 TO STATUS-ANALISE-PAR001
                END-IF
                MOVE STATUS-ANALISE-PAR001   TO WS-STATUS-ANALISE
           END-READ

           CLOSE PAR001

           MOVE EMPRESA-REL    TO EMPRESA-REL-P
           MOVE EMISSAO-REL    TO EMISSAO-REL-P
           MOVE HORA-REL       TO HORA-REL-P

           MOVE "Integral" TO GS-TIPO-IMPRESSAO
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
               WHEN GS-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
               WHEN GS-GRAVA-WORK-FLG-TRUE
                    PERFORM GRAVA-WORK
                    PERFORM CARREGAR-DADOS-0
                    MOVE 2  TO GS-OPCAO
                    PERFORM TESTAR-OPCAO
                    PERFORM CARREGA-LISTA
               WHEN GS-VERIFICA-DATA-TRUE
                    PERFORM VERIFICA-DATA
               WHEN GS-CARREGA-OPCAO-TRUE
                    PERFORM TESTAR-OPCAO
                    PERFORM CARREGA-LISTA
               WHEN GS-POP-UP-TRUE
                    PERFORM POP-UP
               WHEN GS-LER-REGIAO-TRUE
                    PERFORM LER-REGIAO
               WHEN GS-CHAMAR-POP-CIDADE-TRUE
                    PERFORM CHAMAR-POPUP-CIDADE
               WHEN GS-LE-CIDADE-TRUE
                    PERFORM LE-CIDADE
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CHAMAR-POPUP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CAP010T"
           MOVE PASSAR-STRING-1(1: 30) TO GS-DESC-CIDADE
           MOVE PASSAR-STRING-1(35: 4) TO GS-CIDADE.

       LE-CIDADE SECTION.
           MOVE GS-CIDADE        TO CIDADE
           READ CAD010 INVALID KEY
                MOVE SPACES      TO NOME-CID
           END-READ
           MOVE NOME-CID         TO GS-DESC-CIDADE.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       POP-UP SECTION.
           CALL   "CAP012T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CAP012T"
           MOVE PASSAR-STRING-1(1: 32) TO GS-DESC-REGIAO
           MOVE PASSAR-STRING-1(33: 2) TO GS-ACP-REGIAO.

       LER-REGIAO SECTION.
           MOVE GS-ACP-REGIAO TO CODIGO-REG
           READ CAD012 INVALID KEY
               MOVE SPACES    TO NOME-REG
           END-READ
           MOVE NOME-REG      TO GS-DESC-REGIAO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       VERIFICA-DATA SECTION.
           MOVE GS-DATA   TO GRTIME-DATE.
           MOVE 1         TO GRTIME-TYPE.
           MOVE 8         TO GRTIME-FUNCTION.
           CALL "GRTIME" USING PARAMETROS-GRTIME.
           CANCEL "GRTIME".
           IF GRTIME-WEEK-NUM <> 2
              MOVE "ERRO-DATA-SEMANA" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           ELSE
             PERFORM VERIFICA-DIA-SEMANA.
       VERIFICA-DIA-SEMANA SECTION.
      *    memoriza os 7 dias da semana(iniciando pela segunda)
           MOVE GS-DATA   TO DATA-INV
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV  TO DATA-SEG

           MOVE DATA-SEG  TO GRTIME-DATE
           PERFORM CHAMA-GRTIME-ADAY
           MOVE GRTIME-DATE-FINAL TO DATA-TER

           MOVE DATA-TER   TO GRTIME-DATE
           PERFORM CHAMA-GRTIME-ADAY
           MOVE GRTIME-DATE-FINAL TO DATA-QUA

           MOVE DATA-QUA   TO GRTIME-DATE
           PERFORM CHAMA-GRTIME-ADAY
           MOVE GRTIME-DATE-FINAL TO DATA-QUI

           MOVE DATA-QUI   TO GRTIME-DATE
           PERFORM CHAMA-GRTIME-ADAY
           MOVE GRTIME-DATE-FINAL TO DATA-SEX

           MOVE DATA-SEX   TO GRTIME-DATE
           PERFORM CHAMA-GRTIME-ADAY
           MOVE GRTIME-DATE-FINAL TO DATA-SAB

           MOVE DATA-SAB   TO GRTIME-DATE
           PERFORM CHAMA-GRTIME-ADAY
           MOVE GRTIME-DATE-FINAL TO DATA-DOM.

       CHAMA-GRTIME-ADAY SECTION.
           MOVE 2         TO GRTIME-TYPE
           MOVE 1         TO GRTIME-FUNCTION
           MOVE 1         TO GRTIME-DAYS
           CALL "GRTIME"  USING PARAMETROS-GRTIME
           CANCEL "GRTIME".

       GRAVA-WORK SECTION.
           OPEN OUTPUT WORK WORK2 WORK3 WORK4
           CLOSE       WORK WORK2 WORK3 WORK4
           OPEN I-O    WORK WORK2 WORK3 WORK4

           MOVE DATA-SEG TO DATAREALIZA-CO60.
           START COD060 KEY IS NOT < DATAREALIZA-CO60 INVALID KEY
                 MOVE "10" TO ST-COD060.
           PERFORM UNTIL ST-COD060 = "10"
             READ COD060 NEXT RECORD AT END
                  MOVE "10" TO ST-COD060
             NOT AT END
                 IF DATAREALIZA-CO60 > DATA-DOM
                    MOVE "10"             TO ST-COD060
                 ELSE
                    MOVE DATAREALIZA-CO60 TO AUX-DATA
                    IF AUX-MES < 1 OR AUX-MES > 12
                       MOVE SPACES TO MENSAGEM
                       STRING "CONTRATO = " NR-CONTRATO-CO60 X"0DA0"
                              "ITEM = " ITEM-CO60 X"0DA0"
                              "DATA REALIZACAO = " AUX-DIA "/"
                              AUX-MES "/" AUX-ANO INTO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                    ELSE
                      MOVE DATAREALIZA-CO60 TO DATA-WK
                                               DATA-WK2
                                               DATA-WK3
                                               DATA-WK4
                      MOVE ITEM-CO60        TO COD-EVENTO-WK
                      MOVE NR-CONTRATO-CO60 TO NR-CONTRATO-CO40
                                               CONTRATO-WK
                                               CONTRATO-WK2
                                               CONTRATO-WK3
                      READ COD040 INVALID KEY
                           MOVE ZEROS       TO CIDADE-CO40
                      END-READ

                      MOVE CIDADE-CO40      TO CIDADE-WK
                                               CIDADE
                                               CIDADE-WK2
                      IF GS-CLASSIF-CID = 1
                         IF CIDADE-CO60 > 0
                            MOVE CIDADE-CO60 TO CIDADE-WK
                                                CIDADE
                                                CIDADE-WK2
                         END-IF
                      END-IF
                      READ CAD010 INVALID KEY
                           MOVE SPACES TO NOME-CID
                      END-READ
                      IF STATUS-CO40 <> WS-STATUS-REVENDIDO AND
                                        WS-STATUS-ANALISE
                         IF GS-ACP-UF = SPACES OR UF-CID = GS-ACP-UF
                            IF GS-ACP-REGIAO = 0 OR GS-ACP-REGIAO =
                               REGIAO-CID
                               IF GS-CIDADE = 0 OR CIDADE
                               MOVE NOME-CID         TO NOME-CIDADE-WK
                                                        NOME-CIDADE-WK2
      *                        VERIFICA CURSO
                               PERFORM VERIFICA-CURSO
      *                        CURSO = 999 IMPLANTAÇÃO
                               IF CURSO-CO61 = 999 OR CURSO-CO61 = 000
                                  MOVE IDENTIFICACAO-CO40 TO CURSO-WK
                               ELSE
                                  MOVE CURSO-CO61       TO CODIGO-IE11
                                  READ IED011 INVALID KEY
                                       MOVE SPACES TO NOME-IE11
                                  END-READ
                                  MOVE NOME-IE11     TO CURSO-WK
                               END-IF
                               MOVE CODEVENTO-CO60   TO CODIGO-CO03
                               READ COD003 INVALID KEY
                                    MOVE SPACES      TO NOME-CO03
                                    MOVE ZEROS       TO RELAT-ESTAT-CO03
                               END-READ
                               MOVE RELAT-ESTAT-CO03 TO RELAT-ESTAT-WK
                               MOVE NOME-CO03        TO EVENTO-WK
                                                        EVENTO-WK4
                               MOVE HORARIO-CO60     TO HORAS-WK
                                                        HORAS-WK3
                               MOVE ITEM-CO60        TO COD-EVENTO-WK4
                               WRITE REG-WORK3
                               END-WRITE
                               MOVE LOCAL-CO60       TO LOCAL-WK
                               MOVE QT-PARTICIPANTE-CO60 TO FORM-WK
                               READ WORK4 INVALID KEY
                                  MOVE QT-PARTICIPANTE-CO60 TO QTDE-WK4
                                  WRITE REG-WORK4
                                  END-WRITE
                               NOT INVALID KEY
                                  ADD QT-PARTICIPANTE-CO60  TO QTDE-WK4
                                  REWRITE REG-WORK4
                                  END-REWRITE
                               END-READ
                               IF RELAT-ESTAT-CO03 = 1
                                  MOVE DATAREALIZA-CO60 TO DATA-WK2
                                  MOVE NR-CONTRATO-CO60 TO CONTRATO-WK2
                                  READ WORK2 INVALID KEY
                                      MOVE QT-PARTICIPANTE-CO60
                                        TO FORM-WK2
                                      MOVE 1 TO QT-CONTRATO-WK2

                                      WRITE REG-WORK2
                                      END-WRITE
                                  NOT INVALID KEY
                                      ADD QT-PARTICIPANTE-CO60 TO
                                          FORM-WK2
                                      ADD 1 TO QT-CONTRATO-WK2

                                      REWRITE REG-WORK2
                                      END-REWRITE
                                  END-READ
                               ELSE
                                  MOVE DATAREALIZA-CO60 TO DATA-WK2
                                  MOVE NR-CONTRATO-CO60 TO CONTRATO-WK2
                                  READ WORK2 INVALID KEY
                                      MOVE 0  TO FORM-WK2
                                      MOVE 0  TO QT-CONTRATO-WK2

                                      WRITE REG-WORK2
                                      END-WRITE
                                  END-READ
                               END-IF
                               MOVE PADRAO-CO40    TO COBERTURA-WK(1: 1)
                               IF VIDEO-CO60 = 1
                                    MOVE "V"       TO COBERTURA-WK(2: 1)
                               ELSE MOVE " "       TO COBERTURA-WK(2: 1)
                               END-IF
                               MOVE QT-TELAO-CO60  TO COBERTURA-WK(3: 1)
                               IF BECA-CO60 = 1
                                    MOVE "B"       TO COBERTURA-WK(4: 1)
                               ELSE MOVE " "       TO COBERTURA-WK(4: 1)
                               END-IF
                               IF CLIP-CO60 = 1
                                    MOVE "C"       TO COBERTURA-WK(5: 1)
                               ELSE MOVE " "       TO COBERTURA-WK(5: 1)
                               END-IF
                               WRITE REG-WORK
                               END-WRITE
                            END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
             END-READ
           END-PERFORM.
           CLOSE WORK WORK2 WORK3 WORK4.
       VERIFICA-CURSO SECTION.
      *  SE HOUVER VARIOS CURSOS P/O EVENTO IRÁ CONSIDERAR APENAS O 1o.
           MOVE NR-CONTRATO-CO60 TO NR-CONTRATO-CO61
           MOVE ITEM-CO60        TO ITEM-CO61
           MOVE ZEROS            TO CURSO-CO61
           MOVE SPACES           TO TURMA-CO61
           START COD061 KEY IS NOT < CHAVE-CO61 INVALID KEY
                 MOVE "10" TO ST-COD061.
           PERFORM UNTIL ST-COD061 = "10"
                 READ COD061 NEXT RECORD AT END
                      MOVE "10" TO ST-COD061
                 NOT AT END
                     IF NR-CONTRATO-CO61 <> NR-CONTRATO-CO60 OR
                        ITEM-CO61 <> ITEM-CO60
                        MOVE ZEROS TO CURSO-CO61
                     END-IF
                     MOVE "10" TO ST-COD061
                 END-READ
           END-PERFORM.
      *--------------------------------------------------------------
       CARREGAR-DADOS-0 SECTION.
      *    CARREGAR TODAS AS LINHAS DO RELATORIO (LINDET-W)

           OPEN INPUT WORK WORK2 WORK3 WORK4
           MOVE ZEROS TO FORM1-W FORM2-W FORM3-W FORM4-W
                         FORM5-W FORM6-W FORM7-W
                         TOT1-EVENTOS TOT2-EVENTOS TOT3-EVENTOS
                         TOT4-EVENTOS TOT5-EVENTOS TOT6-EVENTOS
                         TOT7-EVENTOS.



           MOVE ZEROS TO I-INI I-ULT I.
           INITIALIZE REG-WORK
           MOVE SPACES TO NOME-CIDADE-ANT NOME-CIDADE-WK
           MOVE ZEROS TO DATA-WK EVENTO-WK CONTRATO-WK.
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   COMPUTE I = I-ULT + 1
                   MOVE I TO I-ULT
                   PERFORM TRACO-COLUNA
                   COMPUTE I = I-ULT + 1
                   MOVE I TO I-ULT
                   PERFORM TRACO-COLUNA

                   PERFORM TRACO-SEPARADOR
                   MOVE "10" TO ST-WORK
              NOT AT END
                   IF NOME-CIDADE-WK <> NOME-CIDADE-ANT
                      IF NOME-CIDADE-ANT <> SPACES
                         COMPUTE I = I-ULT + 1
                         MOVE I TO I-ULT
                         PERFORM TRACO-COLUNA
                         COMPUTE I = I-ULT + 1
                         MOVE I TO I-ULT
                         PERFORM TRACO-COLUNA
                      END-IF
                      PERFORM TRACO-SEPARADOR
                      PERFORM CABECALHO-CIDADE
                      MOVE NOME-CIDADE-WK TO NOME-CIDADE-ANT
                      PERFORM CARREGAR-DADOS
                   ELSE
                      PERFORM CARREGAR-DADOS
                   END-IF
              END-READ
           END-PERFORM.

           MOVE DATA-SEG TO DATA1-INV
           MOVE DATA-TER TO DATA2-INV
           MOVE DATA-QUA TO DATA3-INV
           MOVE 1        TO OPCAO-SEMANA.


           INITIALIZE REG-WORK2
                      FORM1-W
                      FORM2-W
                      FORM3-W
                      FORM4-W
                      FORM5-W
                      FORM6-W
                      FORM7-W
                      TOT1-CONTRATO
                      TOT2-CONTRATO
                      TOT3-CONTRATO
                      TOT4-CONTRATO
                      TOT5-CONTRATO
                      TOT6-CONTRATO
                      TOT7-CONTRATO

           START WORK2 KEY IS NOT LESS CHAVE-WK2 INVALID KEY
               MOVE "10" TO ST-WORK2.
           PERFORM UNTIL ST-WORK2 = "10"
               READ WORK2 NEXT AT END
                   MOVE "10" TO ST-WORK2
               NOT AT END
                   EVALUATE DATA-WK2
                           WHEN DATA-SEG
                                COMPUTE FORM1-W = FORM1-W + (FORM-WK2 /
                                                      QT-CONTRATO-WK2)
                                ADD 1 TO TOT1-CONTRATO
                           WHEN DATA-TER
                                COMPUTE FORM2-W = FORM2-W + (FORM-WK2 /
                                                      QT-CONTRATO-WK2)
                                ADD 1 TO TOT2-CONTRATO
                           WHEN DATA-QUA
                                COMPUTE FORM3-W = FORM3-W + (FORM-WK2 /
                                                      QT-CONTRATO-WK2)
                                ADD 1 TO TOT3-CONTRATO
                           WHEN DATA-QUI
                                COMPUTE FORM4-W = FORM4-W + (FORM-WK2 /
                                                      QT-CONTRATO-WK2)
                                ADD 1 TO TOT4-CONTRATO
                           WHEN DATA-SEX
                                COMPUTE FORM5-W = FORM5-W + (FORM-WK2 /
                                                      QT-CONTRATO-WK2)
                                ADD 1 TO TOT5-CONTRATO
                           WHEN DATA-SAB
                                COMPUTE FORM6-W = FORM6-W + (FORM-WK2 /
                                                      QT-CONTRATO-WK2)
                                ADD 1 TO TOT6-CONTRATO
                           WHEN DATA-DOM
                                COMPUTE FORM7-W = FORM7-W + (FORM-WK2 /
                                                      QT-CONTRATO-WK2)
                                ADD 1 TO TOT7-CONTRATO
                   END-EVALUATE
               END-READ
           END-PERFORM

           INITIALIZE REG-WORK3
                      TOT1-EVENTOS
                      TOT2-EVENTOS
                      TOT3-EVENTOS
                      TOT4-EVENTOS
                      TOT5-EVENTOS
                      TOT6-EVENTOS
                      TOT7-EVENTOS

           START WORK3 KEY IS NOT LESS CHAVE-WK3 INVALID KEY
               MOVE "10" TO ST-WORK3.
           PERFORM UNTIL ST-WORK3 = "10"
               READ WORK3 NEXT AT END
                   MOVE "10" TO ST-WORK3
               NOT AT END
                   EVALUATE DATA-WK3
                           WHEN DATA-SEG
                                ADD 1 TO TOT1-EVENTOS
                           WHEN DATA-TER
                                ADD 1 TO TOT2-EVENTOS
                           WHEN DATA-QUA
                                ADD 1 TO TOT3-EVENTOS
                           WHEN DATA-QUI
                                ADD 1 TO TOT4-EVENTOS
                           WHEN DATA-SEX
                                ADD 1 TO TOT5-EVENTOS
                           WHEN DATA-SAB
                                ADD 1 TO TOT6-EVENTOS
                           WHEN DATA-DOM
                                ADD 1 TO TOT7-EVENTOS
                   END-EVALUATE
               END-READ
           END-PERFORM

           PERFORM TOTALIZA-TABELA
           CLOSE WORK WORK2 WORK3 WORK4.

       TOTALIZA-TABELA SECTION.
           ADD 1 TO I.
           PERFORM TRACO-COLUNA.
           MOVE "FORMANDO"         TO LINDET-W(I)(4: 15)
           MOVE FORM1-W            TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(31: 10)
           MOVE FORM2-W            TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(60: 10)
           MOVE FORM3-W            TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(89: 10)
           MOVE FORM4-W            TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(118: 10)
           MOVE FORM5-W            TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(147: 10)
           MOVE FORM6-W            TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(176: 10)
           MOVE FORM7-W            TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(205: 10).

           ADD 1 TO I
           PERFORM TRACO-COLUNA
           MOVE ZEROS TO AUX-QTDE2
           MOVE "FOTOGRAFO"        TO LINDET-W(I)(4: 15)
           IF FORM1-W = ZEROS MOVE ZEROS TO QTDE-W
           ELSE IF FORM1-W < 30 MOVE 3 TO QTDE-W
                ELSE COMPUTE QTDE-W = FORM1-W / 10.
           ADD QTDE-W TO AUX-QTDE2
           MOVE QTDE-W             TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(31: 10)
           IF FORM2-W = ZEROS MOVE ZEROS TO QTDE-W
           ELSE IF FORM2-W < 30 MOVE 3 TO QTDE-W
                ELSE COMPUTE QTDE-W = FORM2-W / 10.
           ADD QTDE-W TO AUX-QTDE2
           MOVE QTDE-W             TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(60: 10)
           IF FORM3-W = ZEROS MOVE ZEROS TO QTDE-W
           ELSE IF FORM3-W < 30 MOVE 3 TO QTDE-W
                ELSE COMPUTE QTDE-W = FORM3-W / 10.
           ADD QTDE-W TO AUX-QTDE2
           MOVE QTDE-W             TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(89: 10)
           IF FORM4-W = ZEROS MOVE ZEROS TO QTDE-W
           ELSE IF FORM4-W < 30 MOVE 3 TO QTDE-W
                ELSE COMPUTE QTDE-W = FORM4-W / 10.
           ADD QTDE-W TO AUX-QTDE2
           MOVE QTDE-W             TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(118: 10)
           IF FORM5-W = ZEROS MOVE ZEROS TO QTDE-W
           ELSE IF FORM5-W < 30 MOVE 3 TO QTDE-W
                ELSE COMPUTE QTDE-W = FORM5-W / 10.
           ADD QTDE-W TO AUX-QTDE2
           MOVE QTDE-W             TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(147: 10)
           IF FORM6-W = ZEROS MOVE ZEROS TO QTDE-W
           ELSE IF FORM6-W < 30 MOVE 3 TO QTDE-W
                ELSE COMPUTE QTDE-W = FORM6-W / 10.
           ADD QTDE-W TO AUX-QTDE2
           MOVE QTDE-W             TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(176: 10)
           IF FORM7-W = ZEROS MOVE ZEROS TO QTDE-W
           ELSE IF FORM7-W < 30 MOVE 3 TO QTDE-W
                ELSE COMPUTE QTDE-W = FORM7-W / 10.
           ADD QTDE-W TO AUX-QTDE2
           MOVE QTDE-W             TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(205: 10).

           ADD 1 TO I.
           PERFORM TRACO-COLUNA.
           MOVE "CINEGRAFISTA"     TO LINDET-W(I)(4: 15)
           EVALUATE FORM1-W
             WHEN ZEROS MOVE 0 TO QTDE-W
             WHEN < 80  MOVE 2 TO QTDE-W
             WHEN < 129 MOVE 3 TO QTDE-W
             WHEN OTHER COMPUTE QTDE-W = ((FORM1-W - 80) / 50) + 3
           END-EVALUATE
           MOVE QTDE-W             TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(31: 10)
           EVALUATE FORM2-W
             WHEN ZEROS MOVE 0 TO QTDE-W
             WHEN < 80  MOVE 2 TO QTDE-W
             WHEN < 129 MOVE 3 TO QTDE-W
             WHEN OTHER COMPUTE QTDE-W = ((FORM2-W - 80) / 50) + 3
           END-EVALUATE
           MOVE QTDE-W             TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(60: 10)
           EVALUATE FORM3-W
             WHEN ZEROS MOVE 0 TO QTDE-W
             WHEN < 80  MOVE 2 TO QTDE-W
             WHEN < 129 MOVE 3 TO QTDE-W
             WHEN OTHER COMPUTE QTDE-W = ((FORM3-W - 80) / 50) + 3
           END-EVALUATE
           MOVE QTDE-W             TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(89: 10)
           EVALUATE FORM4-W
             WHEN ZEROS MOVE 0 TO QTDE-W
             WHEN < 80  MOVE 2 TO QTDE-W
             WHEN < 129 MOVE 3 TO QTDE-W
             WHEN OTHER COMPUTE QTDE-W = ((FORM4-W - 80) / 50) + 3
           END-EVALUATE
           MOVE QTDE-W             TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(118: 10)
           EVALUATE FORM5-W
             WHEN ZEROS MOVE 0 TO QTDE-W
             WHEN < 80  MOVE 2 TO QTDE-W
             WHEN < 129 MOVE 3 TO QTDE-W
             WHEN OTHER COMPUTE QTDE-W = ((FORM5-W - 80) / 50) + 3
           END-EVALUATE
           MOVE QTDE-W             TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(147: 10)
           EVALUATE FORM6-W
             WHEN ZEROS MOVE 0 TO QTDE-W
             WHEN < 80  MOVE 2 TO QTDE-W
             WHEN < 129 MOVE 3 TO QTDE-W
             WHEN OTHER COMPUTE QTDE-W = ((FORM6-W - 80) / 50) + 3
           END-EVALUATE
           MOVE QTDE-W             TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(176: 10)
           EVALUATE FORM7-W
             WHEN ZEROS MOVE 0 TO QTDE-W
             WHEN < 80  MOVE 2 TO QTDE-W
             WHEN < 129 MOVE 3 TO QTDE-W
             WHEN OTHER COMPUTE QTDE-W = ((FORM7-W - 80) / 50) + 3
           END-EVALUATE
           MOVE QTDE-W             TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(205: 10).

           ADD 1 TO I.
           PERFORM TRACO-COLUNA.
           MOVE "CONTRATOS   "     TO LINDET-W(I)(4: 15)
           MOVE TOT1-CONTRATO      TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(31: 10)
           MOVE TOT2-CONTRATO      TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(60: 10)
           MOVE TOT3-CONTRATO      TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(89: 10)
           MOVE TOT4-CONTRATO      TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(118: 10)
           MOVE TOT5-CONTRATO      TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(147: 10)
           MOVE TOT6-CONTRATO      TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(176: 10)
           MOVE TOT7-CONTRATO      TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(205: 10).

           ADD 1 TO I.
           PERFORM TRACO-COLUNA.
           MOVE "EVENTOS     "     TO LINDET-W(I)(4: 15)
           MOVE TOT1-EVENTOS       TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(31: 10)
           MOVE TOT2-EVENTOS       TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(60: 10)
           MOVE TOT3-EVENTOS       TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(89: 10)
           MOVE TOT4-EVENTOS       TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(118: 10)
           MOVE TOT5-EVENTOS       TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(147: 10)
           MOVE TOT6-EVENTOS       TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(176: 10)
           MOVE TOT7-EVENTOS       TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(205: 10).

           MOVE I TO I-ULT.
           PERFORM TRACO-SEPARADOR.

           INITIALIZE REG-WORK4 AUX-EVENTO
           move zeros to data-wk4
           move spaces to evento-wk4
           ADD 1 TO I
           PERFORM TRACO-COLUNA

           START WORK4 KEY IS NOT LESS CHAVE-WK4 INVALID KEY
               MOVE "10" TO ST-WORK4.
           PERFORM UNTIL ST-WORK4 = "10"
               READ WORK4 NEXT AT END
                   MOVE "10" TO ST-WORK4
               NOT AT END
                   IF AUX-EVENTO <> EVENTO-WK4
                      IF AUX-EVENTO <> SPACES
                         ADD 1 TO I
                      END-IF
                      MOVE EVENTO-WK4 TO AUX-EVENTO
                      PERFORM TRACO-COLUNA
                   END-IF
                   MOVE EVENTO-WK4         TO LINDET-W(I)(4: 15)
                   EVALUATE DATA-WK4
                           WHEN DATA-SEG
                                MOVE QTDE-WK4           TO QTDE-E
                                MOVE QTDE-E             TO LINDET-W(I)
                                                           (31: 10)
                           WHEN DATA-TER
                                MOVE QTDE-WK4           TO QTDE-E
                                MOVE QTDE-E             TO LINDET-W(I)
                                                           (60: 10)
                           WHEN DATA-QUA
                                MOVE QTDE-WK4           TO QTDE-E
                                MOVE QTDE-E             TO LINDET-W(I)
                                                           (89: 10)
                           WHEN DATA-QUI
                                MOVE QTDE-WK4           TO QTDE-E
                                MOVE QTDE-E             TO LINDET-W(I)
                                                           (118: 10)
                           WHEN DATA-SEX
                                MOVE QTDE-WK4           TO QTDE-E
                                MOVE QTDE-E             TO LINDET-W(I)
                                                           (147: 10)
                           WHEN DATA-SAB
                                MOVE QTDE-WK4           TO QTDE-E
                                MOVE QTDE-E             TO LINDET-W(I)
                                                           (176: 10)
                           WHEN DATA-DOM
                                MOVE QTDE-WK4           TO QTDE-E
                                MOVE QTDE-E             TO LINDET-W(I)
                                                           (205: 10)
                   END-EVALUATE
               END-READ
           END-PERFORM

           MOVE I TO I-ULT.
           PERFORM TRACO-SEPARADOR.



           INITIALIZE REG-WORK4 AUX-EVENTO
                      TOT1-EVENTOS2
                      NUM-CONTROLE
           move zeros to data-wk4
           move spaces to evento-wk4
           ADD 1 TO I
           START WORK4 KEY IS NOT LESS CHAVE-WK4 INVALID KEY
               MOVE "10" TO ST-WORK4.

           PERFORM UNTIL ST-WORK4 = "10"
               READ WORK4 NEXT AT END
                   MOVE "10" TO ST-WORK4
               NOT AT END
                   IF AUX-EVENTO <> EVENTO-WK4
                      IF AUX-EVENTO <> SPACES
                         MOVE TOT1-EVENTOS2      TO QTDE-E
                         MOVE QTDE-E             TO LINDET-W(I)(31: 10)
                         ADD 1 TO NUM-CONTROLE
                         EVALUATE NUM-CONTROLE
                            WHEN 1 MOVE "FORMANDO"  TO LINDET-W(I)
                                                      (60: 10)
                                   COMPUTE QTDE-E = FORM1-W +
                                                    FORM2-W +
                                                    FORM3-W +
                                                    FORM4-W +
                                                    FORM5-W +
                                                    FORM6-W +
                                                    FORM7-W
                                   MOVE QTDE-E      TO LINDET-W(I)
                                                      (89: 10)
                            WHEN 2 MOVE "FOTOGRAFO" TO LINDET-W(I)
                                                      (60: 10)

                                   MOVE AUX-QTDE2   TO QTDE-E
                                   MOVE QTDE-E      TO LINDET-W(I)
                                                      (89: 10)
                            WHEN 3 MOVE "CINEGRAFISTA" TO LINDET-W(I)
                                                      (60: 10)
                                   MOVE ZEROS TO AUX-QTDE
                                   COMPUTE QTDE-W = ((FORM1-W - 80) /
                                                      50) + 3
                                   ADD QTDE-W       TO AUX-QTDE
                                   COMPUTE QTDE-W = ((FORM2-W - 80) /
                                                      50) + 3
                                   ADD QTDE-W       TO AUX-QTDE
                                   COMPUTE QTDE-W = ((FORM3-W - 80) /
                                                      50) + 3
                                   ADD QTDE-W       TO AUX-QTDE
                                   COMPUTE QTDE-W = ((FORM4-W - 80) /
                                                      50) + 3
                                   ADD QTDE-W       TO AUX-QTDE
                                   COMPUTE QTDE-W = ((FORM5-W - 80) /
                                                      50) + 3
                                   ADD QTDE-W       TO AUX-QTDE
                                   COMPUTE QTDE-W = ((FORM6-W - 80) /
                                                      50) + 3
                                   ADD QTDE-W       TO AUX-QTDE
                                   COMPUTE QTDE-W = ((FORM7-W - 80) /
                                                      50) + 3
                                   ADD QTDE-W       TO AUX-QTDE
                                   MOVE AUX-QTDE   TO QTDE-E
                                   MOVE QTDE-E     TO LINDET-W(I)
                                                      (89: 10)
                            WHEN 4 MOVE "CONTRATOS" TO LINDET-W(I)
                                                      (60: 10)
                                   COMPUTE QTDE-E = TOT1-CONTRATO +
                                                    TOT2-CONTRATO +
                                                    TOT3-CONTRATO +
                                                    TOT4-CONTRATO +
                                                    TOT5-CONTRATO +
                                                    TOT6-CONTRATO +
                                                    TOT7-CONTRATO
                                   MOVE QTDE-E     TO LINDET-W(I)
                                                      (89: 10)
                            WHEN 5 MOVE "EVENTOS"  TO LINDET-W(I)
                                                      (60: 10)
                                   COMPUTE QTDE-E = TOT1-EVENTOS +
                                                    TOT2-EVENTOS +
                                                    TOT3-EVENTOS +
                                                    TOT4-EVENTOS +
                                                    TOT5-EVENTOS +
                                                    TOT6-EVENTOS +
                                                    TOT7-EVENTOS
                                   MOVE QTDE-E     TO LINDET-W(I)
                                                      (89: 10)
                            WHEN OTHER MOVE SPACES  TO LINDET-W(I)
                                                       (60: 10)
                                       MOVE SPACES  TO LINDET-W(I)
                                                       (89: 10)
                         END-EVALUATE
                         MOVE SPACES             TO LINDET-W(I)(118: 10)
                         MOVE SPACES             TO LINDET-W(I)(147: 10)
                         MOVE SPACES             TO LINDET-W(I)(176: 10)
                         MOVE SPACES             TO LINDET-W(I)(205: 10)
                         ADD 1 TO I
                         MOVE ZEROS TO TOT1-EVENTOS2
                      END-IF
                      MOVE EVENTO-WK4 TO AUX-EVENTO
                   END-IF
                   PERFORM TRACO-COLUNA
                   MOVE EVENTO-WK4         TO LINDET-W(I)(4: 15)
                   ADD QTDE-WK4            TO TOT1-EVENTOS2
               END-READ
           END-PERFORM

           MOVE TOT1-EVENTOS2      TO QTDE-E
           MOVE QTDE-E             TO LINDET-W(I)(31: 10)
           MOVE SPACES             TO LINDET-W(I)(60: 10)
           MOVE SPACES             TO LINDET-W(I)(89: 10)
           MOVE SPACES             TO LINDET-W(I)(118: 10)
           MOVE SPACES             TO LINDET-W(I)(147: 10)
           MOVE SPACES             TO LINDET-W(I)(176: 10)
           MOVE SPACES             TO LINDET-W(I)(205: 10)



           MOVE I TO I-ULT.
           PERFORM TRACO-SEPARADOR.

       CARREGAR-DADOS SECTION.
      *    MONTA TODA A PARTE DA TABELA QUE SERÁ APRESENTADA
           EVALUATE DATA-WK
              WHEN DATA-SEG ADD 1 TO I1
                           COMPUTE I = I1 + I-INI
              WHEN DATA-TER ADD 1 TO I2
                           COMPUTE I = I2 + I-INI
              WHEN DATA-QUA ADD 1 TO I3
                           COMPUTE I = I3 + I-INI
              WHEN DATA-QUI ADD 1 TO I4
                           COMPUTE I = I4 + I-INI
              WHEN DATA-SEX ADD 1 TO I5
                           COMPUTE I = I5 + I-INI
              WHEN DATA-SAB ADD 1 TO I6
                           COMPUTE I = I6 + I-INI
              WHEN DATA-DOM ADD 1 TO I7
                           COMPUTE I = I7 + I-INI
           END-EVALUATE
           IF I > I-ULT PERFORM TRACO-COLUNA
                        MOVE I TO I-ULT.

           EVALUATE DATA-WK
             WHEN DATA-SEG
                  MOVE CONTRATO-WK    TO LINDET-W(I)(23: 4)
                  MOVE CURSO-WK       TO LINDET-W(I)(27: 3)
                  MOVE "-"            TO LINDET-W(I)(30: 1)
                  MOVE EVENTO-WK      TO LINDET-W(I)(31: 5)
                  MOVE HORAS-WK       TO LINDET-W(I)(36: 5)
                  MOVE "-"            TO LINDET-W(I)(41: 1)
                  MOVE FORM-WK        TO LINDET-W(I)(42: 4)
                  MOVE COBERTURA-WK   TO LINDET-W(I)(46: 5)
                  ADD  1              TO TOT1-EVENTOS
      *           IF RELAT-ESTAT-WK = 1
      *              ADD FORM-WK         TO FORM1-W
      *           END-IF
             WHEN DATA-TER
                  MOVE CONTRATO-WK    TO LINDET-W(I)(52: 4)
                  MOVE CURSO-WK       TO LINDET-W(I)(56: 3)
                  MOVE "-"            TO LINDET-W(I)(59: 1)
                  MOVE EVENTO-WK      TO LINDET-W(I)(60: 5)
                  MOVE HORAS-WK       TO LINDET-W(I)(65: 5)
                  MOVE "-"            TO LINDET-W(I)(70: 1)
                  MOVE FORM-WK        TO LINDET-W(I)(71: 4)
                  MOVE COBERTURA-WK   TO LINDET-W(I)(75: 5)
                  ADD  1              TO TOT2-EVENTOS

      *           IF RELAT-ESTAT-WK = 1
      *              ADD FORM-WK         TO FORM2-W
      *           END-IF
             WHEN DATA-QUA
                  MOVE CONTRATO-WK    TO LINDET-W(I)(81: 4)
                  MOVE CURSO-WK       TO LINDET-W(I)(85: 3)
                  MOVE "-"            TO LINDET-W(I)(88: 1)
                  MOVE EVENTO-WK      TO LINDET-W(I)(89: 5)
                  MOVE HORAS-WK       TO LINDET-W(I)(94: 5)
                  MOVE "-"            TO LINDET-W(I)(99: 1)
                  MOVE FORM-WK        TO LINDET-W(I)(100: 4)
                  MOVE COBERTURA-WK   TO LINDET-W(I)(104: 5)
                  ADD  1              TO TOT3-EVENTOS

      *           IF RELAT-ESTAT-WK = 1
      *              ADD FORM-WK         TO FORM3-W
      *           END-IF
             WHEN DATA-QUI
                  MOVE CONTRATO-WK    TO LINDET-W(I)(110: 4)
                  MOVE CURSO-WK       TO LINDET-W(I)(114: 3)
                  MOVE "-"            TO LINDET-W(I)(117: 1)
                  MOVE EVENTO-WK      TO LINDET-W(I)(118: 5)
                  MOVE HORAS-WK       TO LINDET-W(I)(123: 5)
                  MOVE "-"            TO LINDET-W(I)(128: 1)
                  MOVE FORM-WK        TO LINDET-W(I)(129: 4)
                  MOVE COBERTURA-WK   TO LINDET-W(I)(133: 5)
                  ADD  1              TO TOT4-EVENTOS

      *           IF RELAT-ESTAT-WK = 1
      *              ADD FORM-WK         TO FORM4-W
      *           END-IF
             WHEN DATA-SEX
                  MOVE CONTRATO-WK    TO LINDET-W(I)(139: 4)
                  MOVE CURSO-WK       TO LINDET-W(I)(143: 3)
                  MOVE "-"            TO LINDET-W(I)(146: 1)
                  MOVE EVENTO-WK      TO LINDET-W(I)(147: 5)
                  MOVE HORAS-WK       TO LINDET-W(I)(152: 5)
                  MOVE "-"            TO LINDET-W(I)(157: 1)
                  MOVE FORM-WK        TO LINDET-W(I)(158: 4)
                  MOVE COBERTURA-WK   TO LINDET-W(I)(162: 5)
                  ADD  1              TO TOT5-EVENTOS

      *           IF RELAT-ESTAT-WK = 1
      *              ADD FORM-WK         TO FORM5-W
      *           END-IF
             WHEN DATA-SAB
                  MOVE CONTRATO-WK    TO LINDET-W(I)(168: 4)
                  MOVE CURSO-WK       TO LINDET-W(I)(172: 3)
                  MOVE "-"            TO LINDET-W(I)(175: 1)
                  MOVE EVENTO-WK      TO LINDET-W(I)(176: 5)
                  MOVE HORAS-WK       TO LINDET-W(I)(181: 5)
                  MOVE "-"            TO LINDET-W(I)(186: 1)
                  MOVE FORM-WK        TO LINDET-W(I)(187: 4)
                  MOVE COBERTURA-WK   TO LINDET-W(I)(191: 5)
                  ADD  1              TO TOT6-EVENTOS

      *           IF RELAT-ESTAT-WK = 1
      *              ADD FORM-WK         TO FORM6-W
      *           END-IF
             WHEN DATA-DOM
                  MOVE CONTRATO-WK    TO LINDET-W(I)(197: 4)
                  MOVE CURSO-WK       TO LINDET-W(I)(201: 3)
                  MOVE "-"            TO LINDET-W(I)(204: 1)
                  MOVE EVENTO-WK      TO LINDET-W(I)(205: 5)
                  MOVE HORAS-WK       TO LINDET-W(I)(210: 5)
                  MOVE "-"            TO LINDET-W(I)(215: 1)
                  MOVE FORM-WK        TO LINDET-W(I)(216: 4)
                  MOVE COBERTURA-WK   TO LINDET-W(I)(220: 5)
                  ADD  1              TO TOT7-EVENTOS

      *           IF RELAT-ESTAT-WK = 1
      *              ADD FORM-WK         TO FORM7-W
      *           END-IF
           END-EVALUATE.

       TRACO-SEPARADOR SECTION.
           COMPUTE I = I-ULT + 1.
           MOVE I TO I-INI I-ULT.
           MOVE ZEROS TO I1 I2 I3 I4 I5 I6 I7.
           MOVE "I--------------------I"        TO LINDET-W(I)(1: 22)
           MOVE "----------------------------I" TO LINDET-W(I)(23: 29)
           MOVE "----------------------------I" TO LINDET-W(I)(52: 29)
           MOVE "----------------------------I" TO LINDET-W(I)(81: 29)
           MOVE "----------------------------I" TO LINDET-W(I)(110: 29)
           MOVE "----------------------------I" TO LINDET-W(I)(139: 29)
           MOVE "----------------------------I" TO LINDET-W(I)(168: 29)
           MOVE "----------------------------I" TO LINDET-W(I)(197: 29).
       CABECALHO-CIDADE SECTION.
           COMPUTE I = I-INI + 1.
           PERFORM TRACO-COLUNA.
           MOVE CIDADE-WK          TO LINDET-W(I)(3: 4)
           MOVE "-"                TO LINDET-W(I)(7: 1)
           MOVE NOME-CIDADE-WK     TO LINDET-W(I)(8: 13)
           MOVE I TO I-ULT.
       TRACO-COLUNA SECTION.
           MOVE "I                    I"        TO LINDET-W(I)(1: 22)
           MOVE "                            I" TO LINDET-W(I)(23: 29)
           MOVE "                            I" TO LINDET-W(I)(52: 29)
           MOVE "                            I" TO LINDET-W(I)(81: 29)
           MOVE "                            I" TO LINDET-W(I)(110: 29)
           MOVE "                            I" TO LINDET-W(I)(139: 29)
           MOVE "                            I" TO LINDET-W(I)(168: 29)
           MOVE "                            I" TO LINDET-W(I)(197: 29).
      *--------------------------------------------------------------
       TESTAR-OPCAO SECTION.
      *    VERIFICAR OS 3 DIAS QUE SERÃO APRESENTADOS
           EVALUATE GS-OPCAO
             WHEN 1
               EVALUATE DATA1-INV
                  WHEN DATA-SEG CONTINUE
                  WHEN DATA-TER
                       MOVE 1        TO OPCAO-SEMANA
                       MOVE DATA-SEG TO DATA1-INV
                       MOVE DATA-TER TO DATA2-INV
                       MOVE DATA-QUA TO DATA3-INV
                  WHEN DATA-QUA
                       MOVE 2        TO OPCAO-SEMANA
                       MOVE DATA-TER TO DATA1-INV
                       MOVE DATA-QUA TO DATA2-INV
                       MOVE DATA-QUI TO DATA3-INV
                  WHEN DATA-QUI
                       MOVE 3        TO OPCAO-SEMANA
                       MOVE DATA-QUA TO DATA1-INV
                       MOVE DATA-QUI TO DATA2-INV
                       MOVE DATA-SEX TO DATA3-INV
                  WHEN DATA-SEX
                       MOVE 4        TO OPCAO-SEMANA
                       MOVE DATA-QUI TO DATA1-INV
                       MOVE DATA-SEX TO DATA2-INV
                       MOVE DATA-SAB TO DATA3-INV
               END-EVALUATE
             WHEN 2
                       MOVE 1        TO OPCAO-SEMANA
                       MOVE DATA-SEG TO DATA1-INV
                       MOVE DATA-TER TO DATA2-INV
                       MOVE DATA-QUA TO DATA3-INV
             WHEN 3
                       MOVE 4        TO OPCAO-SEMANA
                       MOVE DATA-QUI TO DATA1-INV
                       MOVE DATA-SEX TO DATA2-INV
                       MOVE DATA-SAB TO DATA3-INV
             WHEN 4
                       MOVE 5        TO OPCAO-SEMANA
                       MOVE DATA-SEX TO DATA1-INV
                       MOVE DATA-SAB TO DATA2-INV
                       MOVE DATA-DOM TO DATA3-INV
             WHEN 5
               EVALUATE DATA3-INV
                  WHEN DATA-QUA
                       MOVE 2        TO OPCAO-SEMANA
                       MOVE DATA-TER TO DATA1-INV
                       MOVE DATA-QUA TO DATA2-INV
                       MOVE DATA-QUI TO DATA3-INV
                  WHEN DATA-QUI
                       MOVE 3        TO OPCAO-SEMANA
                       MOVE DATA-QUA TO DATA1-INV
                       MOVE DATA-QUI TO DATA2-INV
                       MOVE DATA-SEX TO DATA3-INV
                  WHEN DATA-SEX
                       MOVE 4        TO OPCAO-SEMANA
                       MOVE DATA-QUI TO DATA1-INV
                       MOVE DATA-SEX TO DATA2-INV
                       MOVE DATA-SAB TO DATA3-INV
                  WHEN DATA-SAB
                       MOVE 5        TO OPCAO-SEMANA
                       MOVE DATA-SEX TO DATA1-INV
                       MOVE DATA-SAB TO DATA2-INV
                       MOVE DATA-DOM TO DATA3-INV
                  WHEN DATA-DOM CONTINUE
               END-EVALUATE
           END-EVALUATE.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX"     TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES               TO GS-LINDET.
           MOVE "REFRESH-DATA"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE DATA1-INV            TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           PERFORM MOVER-DIA-SEMANA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > I-ULT
              IF LINDET-W(I) = SPACES
                 MOVE 501 TO I
              ELSE
                MOVE LINDET-W(I)(1: 22) TO GS-LINDET(1: 22)
                EVALUATE OPCAO-SEMANA
                  WHEN 1 MOVE LINDET-W(I)(23: 87)  TO GS-LINDET(23: 87)
                  WHEN 2 MOVE LINDET-W(I)(52: 87)  TO GS-LINDET(23: 87)
                  WHEN 3 MOVE LINDET-W(I)(81: 87)  TO GS-LINDET(23: 87)
                  WHEN 4 MOVE LINDET-W(I)(110: 87) TO GS-LINDET(23: 87)
                  WHEN 5 MOVE LINDET-W(I)(139: 87) TO GS-LINDET(23: 87)
                END-EVALUATE
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
              END-IF
           END-PERFORM.
           MOVE "SETAR-POSICAO-CURSOR" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DIA-SEMANA SECTION.
           MOVE DATA1-INV  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV   TO DATA-E
           MOVE DATA-E     TO GS-DIA1(1: 10)
           MOVE DATA2-INV  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV   TO DATA-E
           MOVE DATA-E     TO GS-DIA2(1: 10)
           MOVE DATA3-INV  TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV   TO DATA-E
           MOVE DATA-E     TO GS-DIA3(1: 10)

           EVALUATE OPCAO-SEMANA
             WHEN 1 MOVE "-SEGUNDA"  TO GS-DIA1(11: 8)
                    MOVE "-TERCA  "  TO GS-DIA2(11: 8)
                    MOVE "-QUARTA "  TO GS-DIA3(11: 8)
             WHEN 2 MOVE "-TERCA  "  TO GS-DIA1(11: 8)
                    MOVE "-QUARTA "  TO GS-DIA2(11: 8)
                    MOVE "-QUINTA "  TO GS-DIA3(11: 8)
             WHEN 3 MOVE "-QUARTA "  TO GS-DIA1(11: 8)
                    MOVE "-QUINTA "  TO GS-DIA2(11: 8)
                    MOVE "-SEXTA  "  TO GS-DIA3(11: 8)
             WHEN 4 MOVE "-QUINTA "  TO GS-DIA1(11: 8)
                    MOVE "-SEXTA  "  TO GS-DIA2(11: 8)
                    MOVE "-SABADO "  TO GS-DIA3(11: 8)
             WHEN 5 MOVE "-SEXTA  "  TO GS-DIA1(11: 8)
                    MOVE "-SABADO "  TO GS-DIA2(11: 8)
                    MOVE "-DOMINGO"  TO GS-DIA3(11: 8)
           END-EVALUATE.
           MOVE "REFRESH-DIA-SEMANA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP500" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           EVALUATE GS-TIPO-IMPRESSAO
               WHEN       "Parcial"   PERFORM IMPRIMIR-PARCIAL
               WHEN       "Integral"  PERFORM IMPRIMIR-INTEGRAL
               WHEN OTHER MOVE SPACES TO MENSAGEM
                          STRING "Tipo de Impressão Inválida," X"0DA0"
                                 "Parcial" X"0DA0"
                                 "Integral" INTO MENSAGEM
                           MOVE  "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
           END-EVALUATE.

       IMPRIMIR-PARCIAL SECTION.
           copy impressora.chama.

           if lnk-mapeamento <> spaces
              perform continuar-impressao1.

       continuar-impressao1 section.

           MOVE ZEROS TO PAG-W.

           copy condensa.

           MOVE ZEROS  TO LIN
           PERFORM CABECALHO2
           MOVE SPACES TO LINDET-REL

           MOVE 2        TO GS-LINHA
           MOVE SPACES   TO GS-LINDET
           MOVE "LER-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINDET = SPACES
               MOVE GS-LINDET TO LINDET-REL-P
               WRITE REG-RELAT FROM LINDET-P
               ADD 1          TO LIN
               IF LIN > 56
                  PERFORM CABECALHO2
               END-IF
               ADD 1         TO GS-LINHA
               MOVE SPACES   TO GS-LINDET
               MOVE "LER-LB" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           copy descondensa.

       IMPRIMIR-INTEGRAL SECTION.
           MOVE ZEROS TO PAG-W.

           copy condensa.

           MOVE ZEROS  TO LIN
           PERFORM CABECALHO
           MOVE SPACES TO LINDET-REL

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > I-ULT
              IF LINDET-W(I) = SPACES
                 MOVE 501 TO I
              ELSE
                 MOVE LINDET-W(I)        TO LINDET-REL
                 WRITE REG-RELAT FROM LINDET
                 ADD 1 TO LIN
                 IF LIN > 56
                    PERFORM CABECALHO
                 END-IF
              END-IF
           END-PERFORM.

           copy descondensa.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       CABECALHO SECTION.
           EVALUATE GS-CLASSIF-CID
               WHEN 1 MOVE "EVENTO"        TO CIDADE-REL
               WHEN 2 MOVE "CONTRATO"      TO CIDADE-REL
           END-EVALUATE
           ADD 1 TO PAG-W.
           MOVE PAG-W TO PG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.

           MOVE "I--------------------I----------------------------I----
      -     "------------------------I----------------------------I-----
      -     "-----------------------I----------------------------I------
      -     "----------------------I----------------------------I"
                   TO LINDET-REL
           WRITE REG-RELAT FROM LINDET AFTER 2

           MOVE "I     CIDADE         I                -SEGUNDA    I
      -     "            -TERCA      I                -QUARTA     I
      -     "           -QUINTA     I                -SEXTA      I
      -     "          -SABADO     I                -DOMINGO    I"
                   TO LINDET-REL
           MOVE DATA-SEG    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV    TO DATA-E
           MOVE DATA-E      TO LINDET-REL(29: 10)
           MOVE DATA-TER    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV    TO DATA-E
           MOVE DATA-E      TO LINDET-REL(58: 10)
           MOVE DATA-QUA    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV    TO DATA-E
           MOVE DATA-E      TO LINDET-REL(87: 10)
           MOVE DATA-QUI    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV    TO DATA-E
           MOVE DATA-E      TO LINDET-REL(116: 10)
           MOVE DATA-SEX    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV    TO DATA-E
           MOVE DATA-E      TO LINDET-REL(145: 10)
           MOVE DATA-SAB    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV    TO DATA-E
           MOVE DATA-E      TO LINDET-REL(174: 10)
           MOVE DATA-DOM    TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV    TO DATA-E
           MOVE DATA-E      TO LINDET-REL(203: 10)
           WRITE REG-RELAT FROM LINDET
           WRITE REG-RELAT FROM CAB04.

           MOVE "I--------------------I----------------------------I----
      -     "------------------------I----------------------------I-----
      -     "-----------------------I----------------------------I------
      -     "----------------------I----------------------------I"
                   TO LINDET-REL
           WRITE REG-RELAT FROM LINDET
           MOVE 7 TO LIN.

       CABECALHO2 SECTION.
           EVALUATE GS-CLASSIF-CID
               WHEN 1 MOVE "EVENTO"        TO CIDADE-REL-P
               WHEN 2 MOVE "CONTRATO"      TO CIDADE-REL-P
           END-EVALUATE

           ADD 1      TO PAG-W.
           MOVE PAG-W TO PG-REL-P
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01-P
           ELSE
              WRITE REG-RELAT FROM CAB01-P AFTER PAGE.

           WRITE REG-RELAT FROM CAB02-P AFTER 2.

           MOVE "I--------------------I----------------------------I----
      -         "------------------------I----------------------------I"
                   TO LINDET-REL-P
           WRITE REG-RELAT FROM LINDET-P AFTER 2


           MOVE SPACES TO LINDET-REL-P
           STRING "I                    I   " GS-DIA1(1:25) "I   "
                                              GS-DIA2(1:25) "I   "
                                              GS-DIA3(1:25) "I"
             INTO LINDET-REL-P

           WRITE REG-RELAT FROM LINDET-P
           WRITE REG-RELAT FROM CAB04-P.

           MOVE "I--------------------I----------------------------I----
      -     "------------------------I----------------------------I"
             TO LINDET-REL-P
           WRITE REG-RELAT FROM LINDET-P
           MOVE 7 TO LIN.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD003 COD040 COD060 COD061 IED011 CAD012.
           DELETE FILE WORK.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

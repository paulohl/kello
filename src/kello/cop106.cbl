      $set mfoo
      $set ooctrl(+P)
       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP106.
      *DATA-WRITTEN. 11/01/2000.
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: Relação de Contrato - individual
      *FUNÇÃO: Listar todos os contratos c/ status => 50 dentro
      *        do "mes/ano previsto" solicitado
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window          is class "wclass"
           MSExcel         is class "$OLE$Excel.Application".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CGPX001.
           COPY CAPX004.
           COPY COPX001.
           COPY COPX005.
           COPY COPX040.
           COPY COPX049.
           COPY COPX106.
           COPY IEPX010.
           COPY IEPX011.
           COPY CAPX010.
           COPY CAPX012.
           COPY LBPX027.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CONTRATO-WK
                  ALTERNATE RECORD KEY IS CURSO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS INSTITUICAO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS CIDADE-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS REGIAO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS REPRESENT-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PADRAO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS MESANO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS STATUS-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS TIPO-FOTO-WK WITH DUPLICATES
                  ALTERNATE RECORD KEY IS PREPOSTO-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.


       DATA DIVISION.
       FILE SECTION.
       COPY CGPW001.
       COPY CAPW004.
       COPY IEPW010.
       COPY IEPW011.
       COPY CAPW010.
       COPY CAPW012.
       COPY COPW001.
       COPY COPW005.
       COPY COPW040.
       COPY COPW049.
       COPY COPW106.
       COPY LBPW027.

       FD  WORK.
       01  REG-WORK.
           05  CONTRATO-WK         PIC 9(4).
      *    05  CURSO-WK            PIC X(15).
           05  CURSO-WK            PIC X(20).
      *    05  INSTITUICAO-WK      PIC X(11).
           05  INSTITUICAO-WK      PIC X(30).
           05  CIDADE-WK           PIC X(12).
           05  REGIAO-WK           PIC X(10).
      *    05  REPRESENT-WK        PIC X(13).
           05  REPRESENT-WK        PIC X(25).
           05  QT-FORM-WK          PIC 9(4).
           05  PADRAO-WK           PIC X.
           05  QT-FOTOS-WK         PIC 9(5).
           05  MESANO-WK           PIC 9(6).
           05  COD-STATUS-WK       PIC 9(2).
      *    05  STATUS-WK           PIC X(13).
           05  STATUS-WK           PIC X(30).
           05  TIPO-FOTO-WK        PIC X(20).
           05  PREPOSTO-WK         PIC X(30).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP106.CPB".
           COPY "COP106.CPY".
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".

       01 ExcelObject          object reference.
       01 WorkBooksCollection  object reference.
       01 WorkBook             object reference.
       01 Cell                 object reference.
       01 CellRange            object reference.
       77 NumeroDeColunas      pic 9(9) comp-5.
       77 lsContador           pic 9(09).
       77 lsContadorEx         pic 9(09) comp-5.
       01 indiceLinha          pic 9(06) value zeros.
       77 lsLetra              pic x(01).
       77 lsLetraNulo          pic x(02).
       01 lsColunaString       pic x(255) value spaces.
       01 lsTexto              pic x(255) value spaces.

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(70).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD012             PIC XX       VALUE SPACES.
           05  ST-IED010             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-COD001             PIC XX       VALUE SPACES.
           05  ST-COD005             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD049             PIC XX       VALUE SPACES.
           05  ST-COD106             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-LBD027             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  MESANO-INI            PIC 9(8)     VALUE ZEROS.
           05  MESANO-FIM            PIC 9(8)     VALUE ZEROS.
           05  MESANO-INI-ANT        PIC 9(8)     VALUE ZEROS.
           05  MESANO-FIM-ANT        PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  GRAVAR-W              PIC 9        VALUE ZEROS.
      *    GRAVAR-W = VERIFICA SE É DO TIPO SELECIONADO 1-SIM
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  CURSO-ANT             PIC X(20)    VALUE SPACES.
           05  INSTITUICAO-ANT       PIC X(30)    VALUE SPACES.
           05  CIDADE-ANT            PIC X(12)    VALUE SPACES.
           05  REGIAO-ANT            PIC X(10)    VALUE SPACES.
           05  REPRESENT-ANT         PIC X(25)    VALUE SPACES.
           05  PADRAO-ANT            PIC X        VALUE SPACES.
           05  MESANO-ANT            PIC 9(6)     VALUE ZEROS.
           05  STATUS-ANT            PIC X(30)    VALUE SPACES.
           05  TIPO-FOTO-ANT         PIC X(20)    VALUE SPACES.
           05  DATAINI               PIC 9(8)     VALUE ZEROS.
           05  DATAFIM               PIC 9(8)     VALUE ZEROS.
           05  PREPOSTO-ANT          PIC X(30)    VALUE SPACES.
           05  QT-CURSO              PIC 9        VALUE ZEROS.
           05  CURSO-W               PIC X(15)    VALUE SPACES.
           05  MESANO-I              PIC 9(6)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-E              PIC 99/9999.
           05  TOTAL-FORM            PIC 9(6)     VALUE ZEROS.
           05  TOTAL-FOTOS           PIC 9(7)     VALUE ZEROS.
           05  TOTAL-CONTRATOS       PIC 9(06)    VALUE ZEROS.
           05  TOTAL-FORM-G          PIC 9(6)     VALUE ZEROS.
           05  TOTAL-FOTOS-G         PIC 9(7)     VALUE ZEROS.
           05  TOTAL-CONTRATOS-G     PIC 9(06)    VALUE ZEROS.

           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  PASSAR-STRING         PIC X(20)    VALUE SPACES.
           05  MASC-QTDE4            PIC ZZZ9    BLANK WHEN ZEROS.
           05  MASC-QTDE5            PIC ZZZZ9   BLANK WHEN ZEROS.
           05  MASC-QTDE6            PIC ZZZZZ9  BLANK WHEN ZEROS.
           05  MASC-QTDE7            PIC ZZZZZZ9 BLANK WHEN ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(29)   VALUE
           "RELACAO DE CONTRATO - ORDEM: ".
           05  ORDEM-REL           PIC X(15)   VALUE SPACES.
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  FILLER              PIC X(09)   VALUE "MES/ANO: ".
           05  MESANO-INI-REL      PIC 99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  MESANO-FIM-REL      PIC 99/9999.
           05  FILLER              PIC X(02)   VALUE SPACES.
           05  FILLER              PIC X(12)
               VALUE "ASSINATURA: ".
           05  DATAINI-REL         PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE ' a '.
           05  DATAFIM-REL         PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(159)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(159)  VALUE
           "CONT CURSO           INSTITUICAO CIDADE       REGIAO     REP
      -    "RESENT     FORM PAD FOTOS MES/ANO STATUS        TIPO FOTO
      -    "         PREPOSTO                      ".
       01  LINDET.
           05  LINDET-REL          PIC X(200)  VALUE SPACES.

       01 MENSAGEM                 PIC X(200).
       01 TIPO-MSG                 PIC X(001).
       01 RESP-MSG                 PIC X(001).
       01 ACHEI                    PIC X(001).

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
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD012"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD012.
           MOVE "IED010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED010.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "COD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD001.
           MOVE "COD005"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD005.
           MOVE "COD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD049"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD049.
           MOVE "COD106"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD106.
           MOVE "LBD027"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LBD027.
           OPEN I-O   COD106
           CLOSE      COD106
           OPEN INPUT COD106
           OPEN INPUT CAD010 CAD012 IED010 IED011 COD001 COD040 COD049
                      COD005 CGD001 LBD027
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD012 <> "00"
              MOVE "ERRO ABERTURA CAD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED010 <> "00"
              MOVE "ERRO ABERTURA IED010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD001 <> "00"
              MOVE "ERRO ABERTURA COD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD005 <> "00"
              MOVE "ERRO ABERTURA COD005: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD005 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD049 <> "00"
              MOVE "ERRO ABERTURA COD049: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD049 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD106 <> "00"
              MOVE "ERRO ABERTURA COD106: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD106 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD027 <> "00"
              MOVE "ERRO ABERTURA LBD027: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD027 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

           ACCEPT VARIA-W FROM TIME.

           CLOSE       WORK
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK.


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
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CARREGA-LISTA-FLG-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM CARREGA-LISTA
               WHEN GS-CHAMAR-POP-UP-TRUE
                    PERFORM CHAMAR-POPUP
               WHEN GS-LE-DESCRICAO-TRUE
                    PERFORM LE-DESCRICAO
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
               WHEN GS-EXPORTAR-EXCEL-TRUE
                    PERFORM ZERA-VARIAVEIS
                    PERFORM EXPORTAR-EXCEL
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       EXPORTAR-EXCEL SECTION.
           invoke MSExcel "new" returning ExcelObject
           invoke ExcelObject "setVisible" using by value 1
           invoke ExcelObject "getWorkBooks"
                  returning WorkBooksCollection

           invoke WorkBooksCollection "Add" returning WorkBook


           move 13 to numeroDeColunas
           move 1  to indiceLinha

           initialize lsContadorEx
           perform numeroDeColunas times
               add 1 to lsContadorEx

               move spaces to lsTexto
               evaluate lsContadorEx
                   when 1  move "CONTRATO"      to lsColunaString
                   when 2  move "CURSO"         to lsColunaString
                   when 3  move "INSTITUIÇÃO"   to lsColunaString
                   when 4  move "CIDADE"        to lsColunaString
                   when 5  move "REGIÃO"        to lsColunaString
                   when 6  move "REPRESENTANTE" to lsColunaString
                   when 7  move "FORM"          to lsColunaString
                   when 8  move "PAD"           to lsColunaString
                   when 9  move "FOTOS"         to lsColunaString
                   when 10 move "MÊS/ANO"       to lsColunaString
                   when 11 move "STATUS"        to lsColunaString
                   when 12 move "TIPO FOTO"     to lsColunaString
                   when 13 move "PREPOSTO"      to lsColunaString
               end-evaluate
               invoke ExcelObject "getCells"
                      using by value indiceLinha
                            by value lsContadorEx returning Cell
               string lsColunaString x"00" delimited by "     "
                      into lsTexto
               invoke Cell "SetValue" using lsTexto
               invoke Cell "finalize" returning Cell
           end-perform

           perform ordem

           perform until st-work = "10"
               read work next record at end
                    move "10" to st-work
               not at end
                    if gs-ativo-true and cod-status-wk not < 50 or
                       gs-inativo-true and cod-status-wk < 50
                       perform mover-dados-excel
                    else
                       continue
                    end-if
               end-read
           end-perform

           perform totaliza-excel

           perform totaliza-excel-geral

           initialize lsContador
           perform numeroDeColunas times
              add 1 to lsContador

              evaluate lsContador
                  when 1  move "A" to lsLetra
                  when 2  move "B" to lsLetra
                  when 3  move "C" to lsLetra
                  when 4  move "D" to lsLetra
                  when 5  move "E" to lsLetra
                  when 6  move "F" to lsLetra
                  when 7  move "G" to lsLetra
                  when 8  move "H" to lsLetra
                  when 9  move "I" to lsLetra
                  when 10 move "J" to lsLetra
                  when 11 move "K" to lsLetra
                  when 12 move "L" to lsLetra
                  when 13 move "M" to lsLetra
                  when 14 move "N" to lsLetra
                  when 15 move "O" to lsLetra
                  when 16 move "P" to lsLetra
                  when 17 move "Q" to lsLetra
                  when 18 move "R" to lsLetra
                  when 19 move "S" to lsLetra
                  when 20 move "T" to lsLetra
                  when 21 move "U" to lsLetra
                  when 22 move "V" to lsLetra
                  when 23 move "W" to lsLetra
                  when 24 move "X" to lsLetra
                  when 25 move "Y" to lsLetra
                  when 26 move "Z" to lsLetra
              end-evaluate

              string lsLetra x"00" into lsLetraNulo

              invoke ExcelObject "getColumns" using lsLetraNulo
                                          returning cellRange
              invoke cellRange "Select"
              invoke cellRange "AutoFit"
              invoke cellRange "finalize" returning cellRange
           end-perform.

       mover-dados-excel section.
           EVALUATE GS-ORDEM
             WHEN 1
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK
                    PERFORM TOTALIZA-EXCEL
             WHEN 2
              IF CURSO-ANT  NOT = SPACES
                 IF CURSO-ANT NOT = CURSO-WK
                    PERFORM TOTALIZA-EXCEL
             WHEN 3
              IF INSTITUICAO-ANT NOT = SPACES
                 IF INSTITUICAO-ANT NOT = INSTITUICAO-WK
                    PERFORM TOTALIZA-EXCEL
             WHEN 4
              IF CIDADE-ANT NOT = SPACES
                 IF CIDADE-ANT NOT = CIDADE-WK
                    PERFORM TOTALIZA-EXCEL
             WHEN 5
              IF REGIAO-ANT NOT = SPACES
                 IF REGIAO-ANT NOT = REGIAO-WK
                    PERFORM TOTALIZA-EXCEL
             WHEN 6
              IF REPRESENT-ANT NOT = SPACES
                 IF REPRESENT-ANT NOT = REPRESENT-WK
                    PERFORM TOTALIZA-EXCEL
             WHEN 7
              IF PADRAO-ANT NOT = SPACES
                 IF PADRAO-ANT NOT = PADRAO-WK
                    PERFORM TOTALIZA-EXCEL
             WHEN 8
              IF MESANO-ANT NOT = ZEROS
                 IF MESANO-ANT NOT = MESANO-WK
                    PERFORM TOTALIZA-EXCEL
             WHEN 9
              IF STATUS-ANT NOT = SPACES
                 IF STATUS-ANT NOT = STATUS-WK
                    PERFORM TOTALIZA-EXCEL
             WHEN 10
              IF TIPO-FOTO-ANT NOT = SPACES
                 IF TIPO-FOTO-ANT NOT = TIPO-FOTO-WK
                    PERFORM TOTALIZA-EXCEL
             WHEN 11
              IF PREPOSTO-ANT NOT = SPACES
                 IF PREPOSTO-ANT NOT = PREPOSTO-WK
                    PERFORM TOTALIZA-EXCEL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-EXCEL.

       MOVER-EXCEL SECTION.
           add 1                              to indiceLinha
           move 0                             to lsContadorEx

      *>Contrato
           add  1                             to lsContadorEx
           move contrato-wk                   to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>Curso
           add  1                             to lsContadorEx
           move curso-wk                      to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>Instituição
           add  1                             to lsContadorEx
           move instituicao-wk                to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>Cidade
           add  1                             to lsContadorEx
           move cidade-wk                     to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>Região
           add  1                             to lsContadorEx
           move regiao-wk                     to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>Representante
           add  1                             to lsContadorEx
           move represent-wk                  to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>Quantidade de formandos
           add  1                             to lsContadorEx
           move qt-form-wk                    to masc-qtde4
           move masc-qtde4                    to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell

           ADD QT-FORM-WK TO TOTAL-FORM.
      *>Padrao
           add  1                             to lsContadorEx
           move padrao-wk                     to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>Quantidade de fotos
           add  1                             to lsContadorEx
           move qt-fotos-wk                   to masc-qtde5
           move masc-qtde5                    to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell

           add qt-fotos-wk                    to total-fotos
           add 1                              to total-contratos
      *>Mês/Ano
           add  1                             to lsContadorEx
           move mesano-wk                     to mesano-i
           move mesano-i(1: 4)                to mesano-w(3: 4)
           move mesano-i(5: 2)                to mesano-w(1: 2)
           move mesano-w                      to mesano-e
           move mesano-e                      to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>Status
           add  1                             to lsContadorEx
           move status-wk                     to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>Tipo Foto
           add  1                             to lsContadorEx
           move tipo-foto-wk                  to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>Preposto
           add  1                             to lsContadorEx
           move preposto-wk                   to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "     "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell.

       TOTALIZA-EXCEL SECTION.
           add 1                              to indiceLinha
           move 0                             to lsContadorEx

      *>Total parcial Contratos
           add  1                             to lsContadorEx
           move spaces                        to lsColunaString
           move total-contratos               to masc-qtde6
           string "TOTAL PARCIAL: " masc-qtde6 into lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "       "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell

      *>Curso
           add  1                             to lsContadorEx
      *>Instituição
           add  1                             to lsContadorEx
      *>Cidade
           add  1                             to lsContadorEx
      *>Região
           add  1                             to lsContadorEx
      *>Representante
           add  1                             to lsContadorEx
      *>Quantidade de formandos
           add  1                             to lsContadorEx
           move total-form                    to masc-qtde6
           move masc-qtde6                    to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "        "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>Padrao
           add  1                             to lsContadorEx
      *>Quantidade de fotos
           add  1                             to lsContadorEx
           move total-fotos                   to masc-qtde7
           move masc-qtde7                    to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "        "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>Mês/Ano
           add  1                             to lsContadorEx
      *>Status
           add  1                             to lsContadorEx
      *>Tipo Foto
           add  1                             to lsContadorEx
      *>Preposto
           add  1                             to lsContadorEx

           add total-contratos                to total-contratos-g
           add total-form                     to total-form-g
           add total-fotos                    to total-fotos-g

           move zeros                         to total-form
                                                 total-fotos
                                                 total-contratos

           add 1                              to indiceLinha.


       totaliza-excel-geral section.
           add 1                              to indiceLinha
           move 0                             to lsContadorEx

      *>Total parcial Contratos
           add  1                             to lsContadorEx
           move spaces                        to lsColunaString
           move total-contratos-g             to masc-qtde6
           string "TOTAL GERAL: " masc-qtde6 into lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "       "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell

      *>Curso
           add  1                             to lsContadorEx
      *>Instituição
           add  1                             to lsContadorEx
      *>Cidade
           add  1                             to lsContadorEx
      *>Região
           add  1                             to lsContadorEx
      *>Representante
           add  1                             to lsContadorEx
      *>Quantidade de formandos
           add  1                             to lsContadorEx
           move total-form-g                  to masc-qtde6
           move masc-qtde6                    to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "        "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>Padrao
           add  1                             to lsContadorEx
      *>Quantidade de fotos
           add  1                             to lsContadorEx
           move total-fotos-g                 to masc-qtde7
           move masc-qtde7                    to lsColunaString
           invoke ExcelObject "getCells"
                  using by value indiceLinha
                        by value lsContadorEx returning Cell
           string lsColunaString x"00" delimited by "        "
                  into lsTexto
           invoke Cell "SetValue" using lsTexto
           invoke Cell "finalize" returning Cell
      *>Mês/Ano
           add  1                             to lsContadorEx
      *>Status
           add  1                             to lsContadorEx
      *>Tipo Foto
           add  1                             to lsContadorEx
      *>Preposto
           add  1                             to lsContadorEx.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       VERIFICAR-SENHA-STATUS SECTION.
           OPEN INPUT CAD004
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA48"          TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
               DISABLE-OBJECT PB12
           NOT INVALID KEY
               ENABLE-OBJECT PB12.

           CLOSE CAD004.

       GRAVA-STATUS SECTION.
           CLOSE    COD106
           OPEN I-O COD106

           INITIALIZE REG-COD106
           START COD106 KEY IS NOT LESS CODIGO-COP106 INVALID KEY
                MOVE "10" TO ST-COD106.
           PERFORM UNTIL ST-COD106 = "10"
                READ COD106 NEXT AT END
                     MOVE "10" TO ST-COD106
                NOT AT END
                     DELETE COD106 INVALID KEY
                         MOVE "Erro de Exclusão...COD106" TO MENSAGEM
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
               MOVE GS-LINHA-STATUS(1:2)   TO CODIGO-COP106
               WRITE REG-COD106
               ADD 1 TO GS-CONT
               MOVE SPACES      TO GS-LINHA-STATUS
               MOVE "LER-LINHA" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      COD106
           OPEN INPUT COD106.

       CARREGAR-STATUS SECTION.
           MOVE "LIMPAR-STATUS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "N" TO ACHEI

           INITIALIZE REG-COD106
           START COD106 KEY IS NOT LESS CODIGO-COP106 INVALID KEY
               MOVE "10" TO ST-COD106.

           PERFORM UNTIL ST-COD106 = "10"
               READ COD106 NEXT AT END
                    MOVE "10" TO ST-COD106
               NOT AT END
                    MOVE CODIGO-COP106 TO CODIGO-CO01
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
              CLOSE      COD106
              OPEN I-O   COD106
              INITIALIZE REG-COD001
              MOVE 50        TO CODIGO-CO01
              START COD001 KEY IS NOT LESS CODIGO-CO01 INVALID KEY
                   MOVE "10" TO ST-COD001
              END-START

              PERFORM UNTIL ST-COD001 = "10"
                   READ COD001 NEXT AT END
                        MOVE "10" TO ST-COD001
                   NOT AT END
                        MOVE CODIGO-CO01      TO CODIGO-COP106
                        WRITE REG-COD106

                        MOVE CODIGO-CO01      TO GS-LINHA-STATUS(1:2)
                        MOVE STATUS-CO01      TO GS-LINHA-STATUS(4:30)
                        MOVE "INSERIR-LINHA"  TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                   END-READ
              END-PERFORM
              CLOSE      COD106
              OPEN INPUT COD106.

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
       CHAMAR-POPUP SECTION.
           EVALUATE GS-TIPO-SELECIONADO
            WHEN 1 CONTINUE
            WHEN 3 CALL   "IEP010T" USING PARAMETROS-W PASSAR-STRING-1
                   CANCEL "IEP010T"
                   MOVE PASSAR-STRING-1(63: 5) TO GS-OPCAO-SELECIONADA
                   MOVE PASSAR-STRING-1(22: 30) TO GS-DESCR-SELECIONADA
            WHEN 4 CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1
                   CANCEL "CAP010T"
                   MOVE PASSAR-STRING-1(35: 4) TO GS-OPCAO-SELECIONADA
                   MOVE PASSAR-STRING-1(1: 30) TO GS-DESCR-SELECIONADA
            WHEN 5 CALL   "CAP012T" USING PARAMETROS-W PASSAR-STRING-1
                   CANCEL "CAP012T"
                   MOVE PASSAR-STRING-1(33: 2) TO GS-OPCAO-SELECIONADA
                   MOVE PASSAR-STRING-1(1: 20) TO GS-DESCR-SELECIONADA
            WHEN 6 CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
                   CANCEL "CGP001T"
                   MOVE PASSAR-STRING-1(33: 6) TO GS-OPCAO-SELECIONADA
                   MOVE PASSAR-STRING-1(1: 20) TO GS-DESCR-SELECIONADA
            WHEN 7 CALL   "LBP027T" USING PARAMETROS-W PASSAR-STRING-1
                   CANCEL "LBP027T"
                   MOVE PASSAR-STRING-1(33:2) TO GS-OPCAO-SELECIONADA
                   MOVE PASSAR-STRING-1(1:30) TO GS-DESCR-SELECIONADA
            WHEN 8 CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
                   CANCEL "CGP001T"
                   MOVE PASSAR-STRING-1(33: 6) TO GS-OPCAO-SELECIONADA
                   MOVE PASSAR-STRING-1(1: 20) TO GS-DESCR-SELECIONADA
            WHEN 9 CALL   "COP001T" USING PARAMETROS-W PASSAR-STRING-1
                   CANCEL "COP001T"
                   MOVE PASSAR-STRING-1(33: 2) TO GS-OPCAO-SELECIONADA
                   MOVE PASSAR-STRING-1(1: 30) TO GS-DESCR-SELECIONADA

           END-EVALUATE.

       LE-DESCRICAO SECTION.
           EVALUATE GS-TIPO-SELECIONADO
            WHEN 1 MOVE GS-OPCAO-SELECIONADA TO NR-CONTRATO-CO40
                   READ COD040 INVALID KEY
                        MOVE ZEROS TO INSTITUICAO-CO40
                   END-READ
                   MOVE INSTITUICAO-CO40 TO CODIGO-IE10
                   READ IED010 INVALID KEY MOVE SPACES TO NOME-IE10
                   END-READ
                   MOVE NOME-IE10  TO GS-DESCR-SELECIONADA
            WHEN 3 MOVE GS-OPCAO-SELECIONADA TO CODIGO-IE10
                   READ IED010 INVALID KEY MOVE SPACES TO NOME-IE10
                   END-READ
                   MOVE NOME-IE10  TO GS-DESCR-SELECIONADA
            WHEN 4 MOVE GS-OPCAO-SELECIONADA TO CIDADE
                   READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID
                   END-READ
                   MOVE NOME-CID   TO GS-DESCR-SELECIONADA
            WHEN 5 MOVE GS-OPCAO-SELECIONADA TO CODIGO-REG
                   READ CAD012 INVALID KEY MOVE SPACES TO NOME-REG
                   END-READ
                   MOVE NOME-REG   TO GS-DESCR-SELECIONADA
            WHEN 6 MOVE GS-OPCAO-SELECIONADA TO CODIGO-CG01
                   READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01  TO GS-DESCR-SELECIONADA
            WHEN 7 MOVE GS-OPCAO-SELECIONADA TO CODIGO-LB27
                   READ LBD027 INVALID KEY
                        MOVE SPACES TO DESCRICAO-LB27
                   END-READ
                   MOVE DESCRICAO-LB27 TO GS-DESCR-SELECIONADA
            WHEN 8 MOVE GS-OPCAO-SELECIONADA TO CODIGO-CG01
                   READ CGD001 INVALID KEY
                        MOVE SPACES    TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01      TO GS-DESCR-SELECIONADA
            WHEN 9 MOVE GS-OPCAO-SELECIONADA TO CODIGO-CO01
                   READ COD001 INVALID KEY MOVE SPACES TO STATUS-CO01
                   END-READ
                   MOVE STATUS-CO01 TO GS-DESCR-SELECIONADA
           END-EVALUATE.

       GRAVA-WORK SECTION.
           IF ST-WORK NOT = "35"
              CLOSE       WORK
              DELETE FILE WORK.

           CLOSE       WORK
           OPEN OUTPUT WORK
           CLOSE       WORK
           OPEN I-O    WORK

           MOVE "TELA-AGUARDA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE GS-MESANO-INI      TO MESANO-INI-REL
           MOVE GS-MESANO-FIM      TO MESANO-FIM-REL
           MOVE GS-DATA-INI        TO DATAINI-REL
           MOVE GS-DATA-FIM        TO DATAFIM-REL

           INITIALIZE REG-COD040

           EVALUATE GS-OP-DATA
               WHEN 1 MOVE GS-MESANO-INI  TO MESANO-W
                                             MESANO-INI-ANT
                                             MESANO-INI-REL
                      MOVE MESANO-W(1: 2) TO MESANO-I(5: 2)
                      MOVE MESANO-W(3: 4) TO MESANO-I(1: 4)
                      MOVE MESANO-I       TO MESANO-INI


                      MOVE GS-MESANO-FIM  TO MESANO-W
                                             MESANO-FIM-REL
                      MOVE MESANO-W(1: 2) TO MESANO-I(5: 2)
                      MOVE MESANO-W(3: 4) TO MESANO-I(1: 4)
                      MOVE MESANO-I       TO MESANO-FIM

                      MOVE MESANO-INI     TO MESANO-PREV-CO40
                      MOVE ZEROS          TO NR-CONTRATO-CO40
                      START COD040 KEY IS NOT < ALT1-CO40 INVALID KEY
                           MOVE "10" TO ST-COD040
                      END-START
               WHEN 2 STRING GS-DATA-INI(5:4)
                             GS-DATA-INI(3:2)
                             GS-DATA-INI(1:2) INTO DATAINI
                      MOVE   DATAINI            TO ASSINATURA-CO40
                      MOVE   GS-DATA-INI        TO DATAINI-REL

                      STRING GS-DATA-FIM(5:4)
                             GS-DATA-FIM(3:2)
                             GS-DATA-FIM(1:2) INTO DATAFIM
                      MOVE   GS-DATA-FIM        TO DATAFIM-REL
                      START COD040 KEY IS NOT < ASSINATURA-CO40
                                                             INVALID KEY
                           MOVE "10" TO ST-COD040
                      END-START
           END-EVALUATE
           PERFORM UNTIL ST-COD040 = "10"
               READ COD040 NEXT RECORD AT END
                    MOVE "10" TO ST-COD040
               NOT AT END
                    IF STATUS-CO40 < 50 AND GS-INATIVO = ZEROS OR
                       STATUS-CO40 NOT < 50 AND GS-ATIVO = ZEROS
                       CONTINUE
                    ELSE
                       PERFORM PESQUISAR-STATUS
                       IF ACHEI = "S"
                          EVALUATE GS-OP-DATA
                               WHEN 1
                                    IF MESANO-PREV-CO40 > MESANO-FIM
                                       MOVE "10" TO ST-COD040
                                    END-IF
                               WHEN 2
                                    IF ASSINATURA-CO40 > DATAFIM
                                       MOVE "10" TO ST-COD040
                                    END-IF
                          END-EVALUATE
                          IF ST-COD040 <> "10"
                             PERFORM VERIFICA-TIPO-SELECIONADO
                             IF GRAVAR-W = 1
                                PERFORM VERIFICAR-ENCERRADO
                                IF GRAVAR-W = 1
                                   PERFORM MOVER-DADOS-WORK
                                END-IF
                             END-IF
                          END-IF
                       END-IF
                    END-IF
               END-READ
           END-PERFORM.
           CLOSE      WORK
           OPEN INPUT WORK
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


       VERIFICA-TIPO-SELECIONADO SECTION.
           EVALUATE GS-TIPO-SELECIONADO
            WHEN 1 IF GS-OPCAO-SELECIONADA = 0 OR NR-CONTRATO-CO40
                      MOVE 1 TO GRAVAR-W
                   ELSE
                      MOVE 2 TO GRAVAR-W
                   END-IF
            WHEN 2 MOVE CIDADE-CO40 TO CIDADE
                   READ CAD010 INVALID KEY
                        MOVE ZEROS TO REGIAO-CID
                        MOVE "***" TO UF-CID
                   END-READ
                   IF GS-UF = SPACES OR UF-CID
                      MOVE 1 TO GRAVAR-W
                   ELSE
                      MOVE 2 TO GRAVAR-W
                   END-IF
            WHEN 3 IF GS-OPCAO-SELECIONADA = 0 OR
                      INSTITUICAO-CO40
                      MOVE 1 TO GRAVAR-W
                   ELSE
                      MOVE 2 TO GRAVAR-W
                   END-IF
            WHEN 4 IF GS-OPCAO-SELECIONADA = 0 OR
                      CIDADE-CO40
                      MOVE 1 TO GRAVAR-W
                   ELSE
                      MOVE 2 TO GRAVAR-W
                   END-IF
            WHEN 5 MOVE CIDADE-CO40 TO CIDADE
                   READ CAD010 INVALID KEY
                        MOVE ZEROS TO REGIAO-CID
                   END-READ
                   MOVE REGIAO-CID TO CODIGO-REG
                   IF GS-OPCAO-SELECIONADA = 0 OR
                      REGIAO-CID
                      MOVE 1 TO GRAVAR-W
                   ELSE
                      MOVE 2 TO GRAVAR-W
                   END-IF
            WHEN 6 IF GS-OPCAO-SELECIONADA = 0 OR
                      REPRESENTANTE-CO40
                      MOVE 1 TO GRAVAR-W
                   ELSE
                      MOVE 2 TO GRAVAR-W
                   END-IF
            WHEN 7 IF GS-OPCAO-SELECIONADA = 0 OR
                      TIPO-FOTOG-CO40
                      MOVE 1 TO GRAVAR-W
                   ELSE
                      MOVE 2 TO GRAVAR-W
                   END-IF
            WHEN 8 MOVE NR-CONTRATO-CO40 TO NR-CONTRATO-CO49
                   READ COD049 INVALID KEY
                        INITIALIZE REG-COD049
                   END-READ
                   IF GS-OPCAO-SELECIONADA = 0 OR
                         PREPOSTO-CO49
                      MOVE 1 TO GRAVAR-W
                   ELSE
                      MOVE 2 TO GRAVAR-W
                   END-IF
            WHEN 9 IF GS-OPCAO-SELECIONADA = 0 OR
                      STATUS-CO40
                      MOVE 1 TO GRAVAR-W
                   ELSE
                      MOVE 2 TO GRAVAR-W
                   END-IF
           END-EVALUATE.

       VERIFICAR-ENCERRADO SECTION.
           MOVE NR-CONTRATO-CO40 TO NR-CONTRATO-CO49
           READ COD049 INVALID KEY
                INITIALIZE REG-COD049
           END-READ
           IF CANCELADO-CO49 IS NOT NUMERIC
              MOVE 0 TO CANCELADO-CO49
           END-IF
           IF GS-ENCERRADOS = CANCELADO-CO49
              MOVE 1 TO GRAVAR-W
           ELSE
              MOVE 2 TO GRAVAR-W
           END-IF.


       MOVER-DADOS-WORK SECTION.
           MOVE CIDADE-CO40 TO CIDADE
           READ CAD010 INVALID KEY
                MOVE ZEROS TO REGIAO-CID
                MOVE "***" TO UF-CID
           END-READ
           IF GS-UF = SPACES OR UF-CID
              MOVE MESANO-PREV-CO40   TO MESANO-WK
                                         GS-EXIBE-VENCTO
              MOVE NR-CONTRATO-CO40   TO CONTRATO-WK
              MOVE INSTITUICAO-CO40   TO CODIGO-IE10
              READ IED010 INVALID KEY
                   MOVE SPACES TO NOME-IE10
              END-READ
              MOVE NOME-IE10          TO INSTITUICAO-WK
              MOVE CIDADE-CO40        TO CIDADE
              READ CAD010 INVALID KEY
                   MOVE SPACES TO NOME-CID
              END-READ
              MOVE NOME-CID           TO CIDADE-WK
              MOVE REGIAO-CID         TO CODIGO-REG
              READ CAD012 INVALID KEY
                   MOVE SPACES TO NOME-REG
              END-READ
              MOVE NOME-REG           TO REGIAO-WK
              MOVE STATUS-CO40        TO COD-STATUS-WK CODIGO-CO01
              READ COD001 INVALID KEY
                   MOVE SPACES TO STATUS-CO01
              END-READ
              MOVE STATUS-CO01        TO STATUS-WK
              MOVE REPRESENTANTE-CO40 TO CODIGO-CG01
              READ CGD001 INVALID KEY
                   MOVE SPACES TO NOME-CG01
              END-READ
              MOVE NOME-CG01          TO REPRESENT-WK
              MOVE QTDE-FORM-CO40     TO QT-FORM-WK
              MOVE IDENTIFICACAO-CO40 TO CURSO-WK
              MOVE PADRAO-CO40        TO PADRAO-CO05 PADRAO-WK
              READ COD005 INVALID KEY
                   MOVE ZEROS TO PREV-FOTOS-CO05
              END-READ
              COMPUTE QT-FOTOS-WK = PREV-FOTOS-CO05 * QT-FORM-WK
              MOVE TIPO-FOTOG-CO40    TO CODIGO-LB27
              READ LBD027 INVALID KEY
                  MOVE SPACES TO TIPO-FOTO-WK
              NOT INVALID KEY
                  MOVE DESCRICAO-LB27 TO TIPO-FOTO-WK
              END-READ

              MOVE NR-CONTRATO-CO40 TO NR-CONTRATO-CO49
              READ COD049 INVALID KEY
                  INITIALIZE REG-COD049
              END-READ

              MOVE PREPOSTO-CO49    TO CODIGO-CG01
              READ CGD001 INVALID KEY
                  MOVE "**********" TO PREPOSTO-WK
              NOT INVALID KEY
                  MOVE NOME-CG01    TO PREPOSTO-WK
              END-READ

              MOVE "TELA-AGUARDA1" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              WRITE REG-WORK.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE SPACES TO GS-LINDET.
           PERFORM ORDEM.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE CAB03 TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE CAB04 TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE CAB03 TO GS-LINDET
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET

           INITIALIZE TOTAL-CONTRATOS-G

           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END
                   MOVE "10" TO ST-WORK
              NOT AT END
                IF GS-ATIVO-TRUE AND COD-STATUS-WK NOT < 50 OR
                   GS-INATIVO-TRUE AND COD-STATUS-WK < 50
                   PERFORM MOVER-DADOS-LINDET
                ELSE
                   CONTINUE
                END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA
           MOVE "TOTAL GERAL: "   TO GS-LINDET(1: 30)
           MOVE TOTAL-CONTRATOS-G TO MASC-QTDE6
           MOVE MASC-QTDE6        TO GS-LINDET(14:06)
           MOVE TOTAL-FORM-G      TO MASC-QTDE6
           MOVE MASC-QTDE6        TO GS-LINDET(70: 6)
           MOVE TOTAL-FOTOS-G     TO MASC-QTDE7
           MOVE MASC-QTDE7        TO GS-LINDET(79: 7)
           MOVE "INSERE-LIST"     TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *    MOVE "REFRESH-DATA" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM.
       ORDEM SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
                MOVE "CONTRATO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO CONTRATO-WK
                START WORK KEY IS NOT < CONTRATO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 2
                MOVE "CURSO" TO GS-DESCR-ORDEM
                MOVE SPACES TO CURSO-WK
                START WORK KEY IS NOT < CURSO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 3
                MOVE "INSTITUIÇÃO" TO GS-DESCR-ORDEM
                MOVE SPACES TO INSTITUICAO-WK
                START WORK KEY IS NOT < INSTITUICAO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 4
                MOVE "CIDADE" TO GS-DESCR-ORDEM
                MOVE SPACES TO CIDADE-WK
                START WORK KEY IS NOT < CIDADE-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 5
                MOVE "REGIÃO" TO GS-DESCR-ORDEM
                MOVE SPACES TO REGIAO-WK
                START WORK KEY IS NOT < REGIAO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 6
                MOVE "REPRESENTANTE" TO GS-DESCR-ORDEM
                MOVE SPACES TO REPRESENT-WK
                START WORK KEY IS NOT < REPRESENT-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 7
                MOVE "PADRÃO" TO GS-DESCR-ORDEM
                MOVE SPACES TO PADRAO-WK
                START WORK KEY IS NOT < PADRAO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 8
                MOVE "MES/ANO" TO GS-DESCR-ORDEM
                MOVE ZEROS TO MESANO-WK
                START WORK KEY IS NOT < MESANO-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 9
                MOVE "STATUS" TO GS-DESCR-ORDEM
                MOVE SPACES TO STATUS-WK
                START WORK KEY IS NOT < STATUS-WK INVALID KEY
                      MOVE "10" TO ST-WORK
             WHEN 10
                MOVE "TIPO FOTO" TO GS-DESCR-ORDEM
                MOVE SPACES      TO TIPO-FOTO-WK
                START WORK KEY IS NOT < TIPO-FOTO-WK INVALID KEY
                      MOVE "10"  TO ST-WORK
             WHEN 11
                MOVE "PREPOSTO"  TO GS-DESCR-ORDEM
                MOVE SPACES      TO PREPOSTO-WK
                START WORK KEY IS NOT < PREPOSTO-WK INVALID KEY
                      MOVE "10"  TO ST-WORK
           END-EVALUATE.
       MOVER-DADOS-LINDET SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK
                    PERFORM TOTALIZA
             WHEN 2
              IF CURSO-ANT  NOT = SPACES
                 IF CURSO-ANT NOT = CURSO-WK
                    PERFORM TOTALIZA
             WHEN 3
              IF INSTITUICAO-ANT NOT = SPACES
                 IF INSTITUICAO-ANT NOT = INSTITUICAO-WK
                    PERFORM TOTALIZA
             WHEN 4
              IF CIDADE-ANT NOT = SPACES
                 IF CIDADE-ANT NOT = CIDADE-WK
                    PERFORM TOTALIZA
             WHEN 5
              IF REGIAO-ANT NOT = SPACES
                 IF REGIAO-ANT NOT = REGIAO-WK
                    PERFORM TOTALIZA
             WHEN 6
              IF REPRESENT-ANT NOT = SPACES
                 IF REPRESENT-ANT NOT = REPRESENT-WK
                    PERFORM TOTALIZA
             WHEN 7
              IF PADRAO-ANT NOT = SPACES
                 IF PADRAO-ANT NOT = PADRAO-WK
                    PERFORM TOTALIZA
             WHEN 8
              IF MESANO-ANT NOT = ZEROS
                 IF MESANO-ANT NOT = MESANO-WK
                    PERFORM TOTALIZA
             WHEN 9
              IF STATUS-ANT NOT = SPACES
                 IF STATUS-ANT NOT = STATUS-WK
                    PERFORM TOTALIZA
             WHEN 10
              IF TIPO-FOTO-ANT NOT = SPACES
                 IF TIPO-FOTO-ANT NOT = TIPO-FOTO-WK
                    PERFORM TOTALIZA
             WHEN 11
              IF PREPOSTO-ANT NOT = SPACES
                 IF PREPOSTO-ANT NOT = PREPOSTO-WK
                    PERFORM TOTALIZA
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           PERFORM MOVER-DADOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       MOVER-DADOS SECTION.
           MOVE CONTRATO-WK       TO GS-LINDET(1: 5)
           MOVE CURSO-WK          TO GS-LINDET(6: 16)
           MOVE INSTITUICAO-WK    TO GS-LINDET(22: 12)
           MOVE CIDADE-WK         TO GS-LINDET(34: 13)
           MOVE REGIAO-WK         TO GS-LINDET(47: 11)
           MOVE REPRESENT-WK      TO GS-LINDET(58: 14)
           MOVE QT-FORM-WK        TO MASC-QTDE4
           MOVE MASC-QTDE4        TO GS-LINDET(72: 5)
           ADD QT-FORM-WK TO TOTAL-FORM.
           MOVE PADRAO-WK         TO GS-LINDET(77: 4)
           MOVE QT-FOTOS-WK       TO MASC-QTDE5
           MOVE MASC-QTDE5        TO GS-LINDET(81: 6)
           ADD QT-FOTOS-WK TO TOTAL-FOTOS.
           ADD 1           TO TOTAL-CONTRATOS
           MOVE MESANO-WK         TO MESANO-I
           MOVE MESANO-I(1: 4)    TO MESANO-W(3: 4)
           MOVE MESANO-I(5: 2)    TO MESANO-W(1: 2)
           MOVE MESANO-W          TO MESANO-E
           MOVE MESANO-E          TO GS-LINDET(87: 8)
           MOVE STATUS-WK         TO GS-LINDET(95: 13)
           MOVE TIPO-FOTO-WK      TO GS-LINDET(109:20).
           MOVE PREPOSTO-WK       TO GS-LINDET(130:30).

       ZERA-VARIAVEIS SECTION.
           MOVE ZEROS TO CONTRATO-ANT MESANO-ANT TOTAL-FOTOS-G
                         TOTAL-FORM-G TOTAL-FORM TOTAL-FOTOS
                         TOTAL-CONTRATOS-G TOTAL-CONTRATOS.
           MOVE SPACES TO REGIAO-ANT INSTITUICAO-ANT CURSO-ANT
                         CIDADE-ANT REPRESENT-ANT PADRAO-ANT STATUS-ANT
                         TIPO-FOTO-ANT PREPOSTO-ANT.
       MOVER-CHAVE-ANT SECTION.
           MOVE CURSO-WK          TO CURSO-ANT.
           MOVE CIDADE-WK         TO CIDADE-ANT.
           MOVE REGIAO-WK         TO REGIAO-ANT.
           MOVE INSTITUICAO-WK    TO INSTITUICAO-ANT.
           MOVE CONTRATO-WK       TO CONTRATO-ANT
           MOVE MESANO-WK         TO MESANO-ANT
           MOVE STATUS-WK         TO STATUS-ANT.
           MOVE REPRESENT-WK      TO REPRESENT-ANT
           MOVE PADRAO-WK         TO PADRAO-ANT
           MOVE TIPO-FOTO-WK      TO TIPO-FOTO-ANT.
           MOVE PREPOSTO-WK       TO PREPOSTO-ANT.
       TOTALIZA SECTION.
           MOVE SPACES            TO GS-LINDET
           MOVE "TOTAL PARCIAL: " TO GS-LINDET(1: 30)
           MOVE TOTAL-CONTRATOS   TO MASC-QTDE6
           MOVE MASC-QTDE6        TO GS-LINDET(14:06)

           MOVE TOTAL-FORM        TO MASC-QTDE6
           MOVE MASC-QTDE6        TO GS-LINDET(70: 6)
           MOVE TOTAL-FOTOS       TO MASC-QTDE7
           MOVE MASC-QTDE7        TO GS-LINDET(79: 7)
           ADD TOTAL-CONTRATOS    TO TOTAL-CONTRATOS-G
           ADD TOTAL-FORM         TO TOTAL-FORM-G
           ADD TOTAL-FOTOS        TO TOTAL-FOTOS-G
           MOVE ZEROS TO TOTAL-FORM TOTAL-FOTOS TOTAL-CONTRATOS.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP106" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           PERFORM ORDEM.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           PERFORM UNTIL ST-WORK = "10"
              READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
              NOT AT END
                IF GS-ATIVO-TRUE AND COD-STATUS-WK NOT < 50 OR
                   GS-INATIVO-TRUE AND COD-STATUS-WK < 50
                      PERFORM MOVER-DADOS-RELATORIO
                ELSE CONTINUE
                END-IF
              END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL
           MOVE SPACES        TO LINDET-REL

           MOVE "TOTAL GERAL: "   TO LINDET-REL(1: 30)
           MOVE TOTAL-CONTRATOS-G TO MASC-QTDE6
           MOVE MASC-QTDE6        TO LINDET-REL(14:06)
           MOVE TOTAL-FORM-G      TO MASC-QTDE6
           MOVE MASC-QTDE6        TO LINDET-REL(60: 6)
           MOVE TOTAL-FOTOS-G     TO MASC-QTDE7
           MOVE MASC-QTDE7        TO LINDET-REL(69: 7)
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.

           COPY DESCONDENSA.

       MOVER-DADOS-RELATORIO SECTION.
           EVALUATE GS-ORDEM
             WHEN 1
              IF CONTRATO-ANT NOT = ZEROS
                 IF CONTRATO-ANT NOT = CONTRATO-WK
                    PERFORM TOTALIZA-REL
             WHEN 2
              IF CURSO-ANT  NOT = SPACES
                 IF CURSO-ANT NOT = CURSO-WK
                    PERFORM TOTALIZA-REL
             WHEN 3
              IF INSTITUICAO-ANT NOT = SPACES
                 IF INSTITUICAO-ANT NOT = INSTITUICAO-WK
                    PERFORM TOTALIZA-REL
             WHEN 4
              IF CIDADE-ANT NOT = SPACES
                 IF CIDADE-ANT NOT = CIDADE-WK
                    PERFORM TOTALIZA-REL
             WHEN 5
              IF REGIAO-ANT NOT = SPACES
                 IF REGIAO-ANT NOT = REGIAO-WK
                    PERFORM TOTALIZA-REL
             WHEN 6
              IF REPRESENT-ANT NOT = SPACES
                 IF REPRESENT-ANT NOT = REPRESENT-WK
                    PERFORM TOTALIZA-REL
             WHEN 7
              IF PADRAO-ANT NOT = SPACES
                 IF PADRAO-ANT NOT = PADRAO-WK
                    PERFORM TOTALIZA-REL
             WHEN 8
              IF MESANO-ANT NOT = ZEROS
                 IF MESANO-ANT NOT = MESANO-WK
                    PERFORM TOTALIZA-REL
             WHEN 9
              IF STATUS-ANT NOT = SPACES
                 IF STATUS-ANT NOT = STATUS-WK
                    PERFORM TOTALIZA-REL
             WHEN 10
              IF TIPO-FOTO-ANT NOT = SPACES
                 IF TIPO-FOTO-ANT NOT = TIPO-FOTO-WK
                    PERFORM TOTALIZA-REL
             WHEN 11
              IF PREPOSTO-ANT NOT = SPACES
                 IF PREPOSTO-ANT NOT = PREPOSTO-WK
                    PERFORM TOTALIZA-REL
           END-EVALUATE.
           PERFORM MOVER-CHAVE-ANT.
           MOVE CONTRATO-WK       TO LINDET-REL(1: 5)
           MOVE CURSO-WK          TO LINDET-REL(6: 16)
           MOVE INSTITUICAO-WK    TO LINDET-REL(22: 12)
           MOVE CIDADE-WK         TO LINDET-REL(34: 13)
           MOVE REGIAO-WK         TO LINDET-REL(47: 11)
           MOVE REPRESENT-WK      TO LINDET-REL(58: 14)
           MOVE QT-FORM-WK        TO MASC-QTDE4
           MOVE MASC-QTDE4        TO LINDET-REL(72: 5)
           ADD QT-FORM-WK         TO TOTAL-FORM
           MOVE PADRAO-WK         TO LINDET-REL(77: 4)
           MOVE QT-FOTOS-WK       TO MASC-QTDE5
           MOVE MASC-QTDE5        TO LINDET-REL(81: 6)
           ADD QT-FOTOS-WK        TO TOTAL-FOTOS
           ADD 1                  TO TOTAL-CONTRATOS
           MOVE MESANO-WK         TO MESANO-I
           MOVE MESANO-I(1: 4)    TO MESANO-W(3: 4)
           MOVE MESANO-I(5: 2)    TO MESANO-W(1: 2)
           MOVE MESANO-W          TO MESANO-E
           MOVE MESANO-E          TO LINDET-REL(87: 8)
           MOVE STATUS-WK         TO LINDET-REL(95: 13)
           MOVE TIPO-FOTO-WK      TO LINDET-REL(109:20)
           MOVE PREPOSTO-WK       TO LINDET-REL(130:30)

           WRITE REG-RELAT FROM LINDET.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
       TOTALIZA-REL SECTION.
           MOVE SPACES        TO LINDET-REL
           MOVE "TOTAL PARCIAL: " TO LINDET-REL(1: 30)
           MOVE TOTAL-CONTRATOS   TO MASC-QTDE6
           MOVE MASC-QTDE6        TO LINDET-REL(14:06)
           MOVE TOTAL-FORM        TO MASC-QTDE6
           MOVE MASC-QTDE6        TO LINDET-REL(60: 6)
           MOVE TOTAL-FOTOS       TO MASC-QTDE7
           MOVE MASC-QTDE7        TO LINDET-REL(69: 7)
           MOVE ZEROS TO TOTAL-FORM TOTAL-FOTOS TOTAL-CONTRATOS.
           WRITE REG-RELAT FROM LINDET-REL.
           ADD 1 TO LIN.

           MOVE SPACES TO LINDET-REL.
           WRITE REG-RELAT FROM LINDET-REL.
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
           CLOSE COD001 COD040 IED010 IED011 CAD010 CAD012
                 CGD001 COD005 LBD027 COD049 COD106.
           CLOSE WORK.  DELETE FILE WORK.

           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

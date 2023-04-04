       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IMP001.
       AUTHOR.        ALFREDO SAVIOLLI NETO
      *EMISSÃO DE RELATÓRIO DE RETORNO DO ITAU
      *CONFORME O TIPO DE OCORRENCIA PELO BANCO O SISTEMA FARÁ:
      *02 - ACEITO PELO BANCO - GRAVA O NR-BANCO NO OUTRO-DOCTO-CR20
      *03 - REJEITADO PELO BANCO - MUDA O PORTADOR DO ARQUIVO CRD020
      *06 - BAIXA DE TÍTULO - FAZ A BAIXA DO TÍTULO NO CRD020
      *NO MOMENTO DA ATUALIZACAO DO CRD020 O TÍTULO NÃO FOR ENCONTRADO
      *O SISTEMA VAI GERAR O ARQUIVO PROBLEMA, QUE TEM POR OBJETIVO
      *LISTAR OS PROBLEMAS P/ QUE O USUÁRIO POSSA ACERTÁ-LO MANUALMENTE.

       DATE-WRITTEN.  11-09-2012.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
               AListview          is class "alistview"
               Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CRPX020.

           COPY PARX002.

           COPY CRPX020B.

           COPY LOGACESS.SEL.

           COPY CAPX018.

           COPY RETPORT.SEL.

           COPY CRPX200.

           COPY CRPX201.

           SELECT RETORNO  ASSIGN       TO ARQUIVO-RETORNO
                           ORGANIZATION IS LINE SEQUENTIAL
                           ACCESS MODE  IS      SEQUENTIAL
                           STATUS       IS          ST-RET.

           SELECT RELAT    ASSIGN TO PRINTER NOME-IMPRESSORA.



       DATA DIVISION.
       FILE SECTION.

           COPY CRPW020.

           COPY PARW002.

           COPY CRPW020B.

           COPY CAPW018.

           COPY RETPORT.FD.

           COPY LOGACESS.FD.

           COPY CRPW200.

           COPY CRPW201.

       FD  RETORNO.
       01  REG-RETORNO          PIC X(400).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "IMP001.CPB".
           COPY "IMP001.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  AREAS-DE-TRABALHO.
           05 ERRO-1              PIC XX     VALUE SPACES.
           05 ST-CAD001           PIC XX     VALUE SPACES.
           05 ST-CAD018           PIC XX     VALUE SPACES.
           05 ST-CRD020           PIC XX     VALUE SPACES.
           05 ST-CRD020B          PIC XX     VALUE SPACES.
           05 ST-CRD200           PIC XX       VALUE SPACES.
           05 ST-CRD201           PIC XX       VALUE SPACES.
           05 ST-RET              PIC XX     VALUE SPACES.
           05 ST-PAR002           PIC XX     VALUE SPACES.
           05 ST-PROBLEMA         PIC XX     VALUE SPACES.
           05 FS-LOGACESS         PIC XX     VALUE SPACES.
           05 FS-RETPORT          PIC XX     VALUE SPACES.
           05 OPCAO               PIC 9      VALUE ZEROS.
           05 VALOR-W             PIC ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZEROS.
           05 VALOR-W1            PIC S9(11)V99 VALUE ZEROS.
           05 DIFERENCA-W         PIC S9(13)  VALUE ZEROS.
           05 DIFERENCA-W1        PIC S9(11)V99  VALUE ZEROS.
           05 VALOR-TOTAL         PIC 9(11)V99 VALUE ZEROS.
           05 ACRESCIMO-TOTAL     PIC S9(11)V99 VALUE ZEROS.
           05 LETRA               PIC X      VALUE SPACES.
           05 NAO-ENCONTRADO      PIC 9      VALUE ZEROS.
           05 TRACO               PIC X(80)  VALUE ALL '-'.
           05 DATA-DIA            PIC 9(6)   VALUE ZEROS.
           05 DATA-DIA-I          PIC 9(8)   VALUE ZEROS.
           05 DATA-VENCTO         PIC 9(8)   VALUE ZEROS.
           05 DATA8               PIC 9(8)   VALUE ZEROS.
           05 DATA-E              PIC 99/99/9999.
           05 VALOR-E             PIC ZZZ.ZZZ.ZZZ,ZZ.
           05 VALOR-E1            PIC ZZ.ZZZ.ZZZ,ZZ-.
           05 CONF                PIC X      VALUE SPACES.
           05 ERRO-W              PIC 9      VALUE ZEROS.
           05 QTDE-PARC           PIC 9(4)        VALUE ZEROS.
           05 LIN                 PIC 9(2)        VALUE ZEROS.
           05 MENSAGEM            PIC X(200)    VALUE SPACES.
           05 TIPO-MSG            PIC X(01).
           05 RESP-MSG            PIC X(01).
           05 VALOR-TITULO        PIC 9(09)V99 VALUE ZEROS.
           05 VALOR-PAGO          PIC 9(09)V99 VALUE ZEROS.
           05 VALOR-DESCONTO      PIC 9(09)V99 VALUE ZEROS.
           05 VALOR-JUROS         PIC 9(09)V99 VALUE ZEROS.
           05 VALOR-MULTA         PIC 9(09)V99 VALUE ZEROS.
           05 OCORRENCIA          PIC 9(02) VALUE ZEROS.
           05 COL1                PIC 9(3)     VALUE ZEROS.
           05 ANOTACAO-W          PIC X(80)    VALUE SPACES.
           05 ULT-SEQ             PIC 9(5)     VALUE ZEROS.
           05 DATAW.
              10  DIA-W       PIC 99.
              10  MES-W       PIC 99.
              10  ANO-W       PIC 99.
           05 DATA-W REDEFINES DATAW PIC 9(6).
           05 DATAI.
              10  ANO-I       PIC 99.
              10  MES-I       PIC 99.
              10  DIA-I       PIC 99.
           05 DATA-I REDEFINES DATAI PIC 9(6).
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  GS-ANOTACAO           PIC X(640) VALUE SPACES.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  lnkusu.
           copy "usuario.cpy".


       77 POP-UP                       PIC X(65).

       01 RET-RETORNO.
          05 RET-EMPRESA               PIC X(60).
          05 RET-EMPRESA-GER           PIC X(60).
          05 RET-BANCO                 PIC X(60).
          05 RET-AGENCIA               PIC X(60).
          05 RET-CONTA                 PIC X(60).
          05 RET-CEDENTE               PIC X(60).
          05 RET-NOME-CEDENTE          PIC X(60).
          05 RET-CARTEIRA              PIC X(60).
          05 RET-NUM-CONTROLE          PIC X(60).
          05 RET-NOSSO-NUM             PIC X(60).
          05 RET-NUM-DOCTO             PIC X(60).
          05 RET-DATA-VENCTO           PIC X(60).
          05 RET-DATA-OCORRENCIA       PIC X(60).
          05 RET-DATA-CREDITO          PIC X(60).
          05 RET-VALOR-TITULO          PIC X(60).
          05 RET-VALOR-PAGO            PIC X(60).
          05 RET-VALOR-DESCONTO        PIC X(60).
          05 RET-VALOR-JUROS           PIC X(60).
          05 RET-VALOR-MULTA           PIC X(60).
          05 RET-NOME-SACADO           PIC X(60).
          05 RET-OCORRENCIA            PIC X(60).
          05 RET-MOTIVO1               PIC X(60).
          05 RET-MOTIVO2               PIC X(60).
          05 RET-MOTIVO3               PIC X(60).
          05 RET-MOTIVO4               PIC X(60).
          05 RET-MOTIVO5               PIC X(60).
          05 RET-D-MOTIVO1             PIC X(60).
          05 RET-D-MOTIVO2             PIC X(60).
          05 RET-D-MOTIVO3             PIC X(60).
          05 RET-D-MOTIVO4             PIC X(60).
          05 RET-D-MOTIVO5             PIC X(60).

       01 lnktabela.
          02 lnkobjetoscoluna  object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas    pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnktabelaP.
          02 lnkobjetoscolunaP  object reference occurs 99 times.
       01 lnktabelaColP.
          02 lnkcolunasP   pic 9(09) comp-5 value zeros occurs 99 times.

       01 indice             pic 9(02).
       01 wsItem             pic 9(09) comp-5 value zeros.
       01 wsSize             pic 9(09) comp-5 value zeros.
       01 wsColunaDocto      pic 9(09) comp-5 value zeros.
       01 wsColunaNossoNum   pic 9(09) comp-5 value zeros.
       01 wsColunaOcorrencia pic 9(09) comp-5 value zeros.
       01 wsColunaValorPg    pic 9(09) comp-5 value zeros.
       01 wsColunaDataCred   pic 9(09) comp-5 value zeros.

       01 wsColunaNumDocto   pic 9(09) comp-5 value zeros.
       01 wsColunaValorDocto pic 9(09) comp-5 value zeros.
       01 wsColunaDtVencto   pic 9(09) comp-5 value zeros.
       01 wsColunaRej1       pic 9(09) comp-5 value zeros.


       77 aItem            object reference value null.
       77 wsTexto          pic x(255) value spaces.
       77 aObjeto          object reference value null.


       01 AUX-DATA                     PIC 9(08).
       01 file-details.
          05 file-size              pic x(8) comp-x.
          05 file-date.
             10 dia                 pic x comp-x.
             10 month               pic x comp-x.
             10 year                pic x(2) comp-x.
          05 file-time.
             10 hours               pic x comp-x.
             10 minutes             pic x comp-x.
             10 seconds             pic x comp-x.
             10 hundredths          pic x comp-x.

       01 status-code               PIC X(2) COMP-5.

       01  CAB01.
           05  FILLER               PIC X(85)  VALUE
               'RELATORIO DE RETORNO - BANESTADO'.
           05  FILLER               PIC X(07)  VALUE 'EMIS.:'.
           05  EMISSAO-REL          PIC 99/99/99.
       01  CAB01A.
           05  FILLER               PIC X(85)  VALUE
               'RELATORIO DE PROBLEMAS NA ATUALIZACAO - ITAU'.
           05  FILLER               PIC X(07)  VALUE 'EMIS.:'.
           05  EMISSAO-REL1         PIC 99/99/99.

       01  CAB02.
           05  FILLER               PIC X(100) VALUE ALL '='.

       01  CAB04.
           05  FILLER               PIC X(3)   VALUE "C".
           05  FILLER               PIC X(11)  VALUE 'CONT-ALB'.
           05  FILLER               PIC X(11)  VALUE 'NR-TITULO'.
           05  FILLER               PIC X(14)  VALUE 'NOSSO-NR'.
           05  FILLER               PIC X(18)  VALUE
                                      '    VALOR-REC-TIT'.
           05  FILLER               PIC X(10)  VALUE 'DTA-PGTO'.
           05  FILLER               PIC X(10)  VALUE "DTA-VCTO".
           05  FILLER               PIC X(11)  VALUE 'VLR-ACRESC'.
           05  FILLER               PIC X(3)   VALUE "OC".
           05  FILLER               PIC X(2)   VALUE "RJ".
       01  LINDET.
           05  CLASS-REL            PIC X      VALUE SPACES.
           05  FILLER               PIC XX     VALUE SPACES.
           05  CONTALB-REL          PIC X(9)   VALUE SPACES.
           05  FILLER               PIC XX     VALUE SPACES.
           05  NR-TITULO-REL        PIC X(10)  VALUE SPACES.
           05  FILLER               PIC X      VALUE SPACES.
           05  NOSSO-NR-REL         PIC X(12)  VALUE SPACES.
           05  FILLER               PIC XX     VALUE SPACES.
           05  VALOR-REC-TIT-REL    PIC ZZ.ZZZ.ZZZ.ZZZ,ZZ.
           05  FILLER               PIC X      VALUE SPACES.
           05  DATA-PGTO-REL        PIC X(08)  VALUE SPACES.
           05  FILLER               PIC XX     VALUE SPACES.
           05  DATA-VENCTO-REL      PIC X(08)  VALUE SPACES.
           05  FILLER               PIC XX     VALUE SPACES.
           05  ACRESCIMO-REL        PIC ZZZ.ZZZ,ZZ.
           05  FILLER               PIC X      VALUE SPACES.
           05  OCORRENCIA-REL       PIC 99     VALUE ZEROS.
           05  FILLER               PIC X      VALUE SPACES.
           05  REJEICAO-REL         PIC 99     VALUE ZEROS.
       01  CAB03.
           05  FILLER               PIC X(100) VALUE ALL '-'.
       01  LINTOT.
           05  FILLER               PIC X(18)  VALUE SPACES.
           05  FILLER               PIC X(09)  VALUE 'QT.PARC: '.
           05  QTDE-PARC-TOT        PIC ZZZZ.
           05  FILLER               PIC X(5)   VALUE SPACES.
           05  VALOR-REC-TIT-TOT    PIC ZZ.ZZZ.ZZZ.ZZZ,ZZ.
           05  FILLER               PIC X(10)  VALUE SPACES.
           05  ACRESCIMO-TOT        PIC Z.ZZZ.ZZZ.ZZZ,ZZ-.

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

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           ACCEPT DATA-I FROM DATE.

           ACCEPT DATA6-W FROM DATE.
           ACCEPT HORA-BRA FROM TIME.
           MOVE DATA6-W TO DATA-INV(3: 6).
           MOVE DATA6-W(1: 2) TO ANO-V.
           IF ANO-V > 80 MOVE "19" TO DATA-INV(1: 2)
           ELSE MOVE "20" TO DATA-INV(1: 2).
           CALL "GRIDAT1" USING DATA-INV.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV      TO DATA-DIA-I.
           ACCEPT HORA-BRA FROM TIME.

           MOVE DIA-I TO DIA-W. MOVE MES-I TO MES-W.
           MOVE ANO-I TO ANO-W. MOVE DATA-W TO EMISSAO-REL EMISSAO-REL1.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "RETPORT"  TO ARQ-REC. MOVE EMPRESA-REF  TO PATH-RETPORT
           MOVE "PAR002"   TO ARQ-REC. MOVE EMPRESA-REF  TO PATH-PAR002
           MOVE "CAD018"   TO ARQ-REC. MOVE EMPRESA-REF  TO PATH-CAD018
           MOVE "CRD020"   TO ARQ-REC. MOVE EMPRESA-REF  TO PATH-CRD020
           MOVE "CRD020B"  TO ARQ-REC. MOVE EMPRESA-REF  TO PATH-CRD020B
           MOVE "CRD200"   TO ARQ-REC. MOVE EMPRESA-REF  TO PATH-CRD200
           MOVE "CRD201"   TO ARQ-REC. MOVE EMPRESA-REF  TO PATH-CRD201
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF  TO
                                                       ARQUIVO-LOGACESS

           OPEN I-O    CRD020 CRD020B CAD018 RETPORT PAR002
           CLOSE       CRD020 CRD020B CAD018 RETPORT PAR002
           OPEN INPUT  CRD020 CRD020B CAD018 RETPORT PAR002

           MOVE "\PROGRAMA\KELLO\*" TO LNK-PATH-SIS
           MOVE "SUPERV"            TO LNK-USUARIO
           MOVE "PADRAO"            TO LNK-EMPRESA

           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018 "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF FS-RETPORT <> "00"
              MOVE "ERRO ABERTURA RETPORT "  TO GS-MENSAGEM-ERRO
              MOVE FS-RETPORT TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020 "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020B <> "00"
              MOVE "ERRO ABERTURA CRD020B"  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020B TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PAR002 <> "00"
              MOVE "ERRO ABERTURA PAR002"  TO GS-MENSAGEM-ERRO
              MOVE ST-PAR002  TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.


           MOVE "IMP001"  TO PROGRAMA-PAR002
           READ PAR002 INVALID KEY
                MOVE "PARAMETRIZACAO DA CONTA NÃO REALIZADA" TO
                GS-MENSAGEM-ERRO
                PERFORM CARREGA-MENSAGEM-ERRO.


           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "IMP001"          to logacess-programa
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

           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM CRIAR-LISTVIEW
                   PERFORM CRIAR-LISTVIEW-PROBLEMA
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GERAR-RELATORIO-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-ATUALIZA-CTA-REC-TRUE
                    MOVE SPACES TO MENSAGEM
                    PERFORM ATUALIZA-A-RECEBER
                    IF MENSAGEM EQUAL SPACES
                       MOVE "Atualização realizada com sucesso" TO
                       MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                    ELSE
                       PERFORM LISTA-NAO-ENCONTRADOS
                       SHOW-WINDOW WIN1
                    END-IF
               WHEN GS-LISTA-NAO-ENCONTR-TRUE
                    PERFORM LISTA-NAO-ENCONTRADOS
               WHEN GS-IMPRIME-NAO-ENCON-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-NAO-ENCONTRADO
                    END-IF
               WHEN GS-VALIDA-ARQUIVO-TRUE
                    PERFORM VALIDAR-ARQUIVO
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LER-PORTADOR
               WHEN GS-POP-PORTADOR-TRUE
                    PERFORM POP-PORTADOR
               WHEN GS-CARREGA-PORTADOR-TRUE
                    PERFORM CARREGAR-PORTADOR
               WHEN GS-GRAVA-PORTADOR-TRUE
                    PERFORM GRAVAR-PORTADOR
               WHEN GS-TRATAR-EVENTO-TRUE
                    EVALUATE GS-QUAL-LIST
                        WHEN 1 PERFORM TRATAR-EVENTO
                        WHEN 2 PERFORM TRATAR-EVENTO-P
                    END-EVALUATE
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       TRATAR-EVENTO SECTION.
           EVALUATE GS-ACP-EVENTO
               WHEN 34123 PERFORM CHAMAR-COLUNAS-FAVO
               WHEN 34027 SET-FOCUS EF5.

       TRATAR-EVENTO-P SECTION.
           EVALUATE GS-ACP-EVENTO
               WHEN 34123 PERFORM CHAMAR-COLUNAS-FAVOP
               WHEN 34027 UNSHOW-WINDOW WIN1 PRINCIPAL
                          SET-FOCUS EF5.

       CRIAR-LISTVIEW SECTION.
          initialize indice

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"Origem" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "centered"
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"Gerada" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "centered"
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"Banco" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "centered"
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"Agência" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "centered"
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
             using z"Conta Corrente" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "centered"
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Cedente" returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
            using z"Nome Cedente" returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"Carteira" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "centered"
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
           using z"Número Controle" returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)
                         wsColunaDocto

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
           using z"Nosso Número" returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)
                         wsColunaNossoNum

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
           using z"Número Docto" returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)
                         wsColunaNumDocto

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
           using z"Data Vencto" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "centered"
          move indice to lnkcolunas(indice)
                         wsColunaDtVencto

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
           using z"Data Ocorrência" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "centered"
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
           using z"Data Crédito" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "centered"
          move indice to lnkcolunas(indice)
                         wsColunaDataCred

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
           using z"Valor Título" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "RightJustified"
          move indice to lnkcolunas(indice)
                         wsColunaValorDocto

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
           using z"Valor Pago" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "RightJustified"
          move indice to lnkcolunas(indice)
                         wsColunaValorPg

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
           using z"Valor Desconto" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "RightJustified"
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
           using z"Valor Juros" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "RightJustified"
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
           using z"Valor Multa" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "RightJustified"
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
           using z"Nome Sacado" returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
           using z"Ocorrência" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "centered"
          move indice to lnkcolunas(indice)
                         wsColunaOcorrencia

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
         using z"Motivo Ocorrência 1" returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)
                         wsColunaRej1

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
         using z"Motivo Ocorrência 2" returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
         using z"Motivo Ocorrência 3" returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
         using z"Motivo Ocorrência 4" returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
         using z"Motivo Ocorrência 5" returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"Descrição Ocorrência 1"
                     returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"Descrição Ocorrência 2"
                     returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"Descrição Ocorrência 3"
                     returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"Descrição Ocorrência 4"
                     returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

          add 1 to indice
          invoke gs-acp-listview "adicionarColunaZ"
                 using z"Descrição Ocorrência 5"
                     returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

           perform mostrar-fonte-favorita
           perform mostrar-colunas-favoritas

           invoke gs-acp-listview "gridLines"
           invoke gs-acp-listview "noBorder".
       CRIAR-LISTVIEW-FIM.
           EXIT.

       mostrar-colunas-favoritas section.
          initialize wsTexto
          move "listview-imp001" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-acp-listview
                                  wsTexto
                                  lnktabela.
       mostrar-colunas-favoritas-fim.
           exit.

       mostrar-fonte-favorita section.
           move "listview-imp001" to wsTexto
           invoke aListview "criarFonte"
                             using lnkusu gs-acp-listview wsTexto.
       mostrar-fonte-favorita-fim.
           exit.

       zebrar-itens section.
           move "listview-imp001" to wsTexto
           invoke aListview "zebrarCor"
                             using lnkusu gs-acp-listview wsTexto
           invoke gs-acp-listview "redrawallitems".
       zebrar-itens-fim.
           exit.

       chamar-colunas-favo section.
           move "listview-imp001" to wsTexto
           call "COLFAV" using lnkusu
                               gs-acp-listview
                               wsTexto
                               lnktabela

           perform mostrar-colunas-favoritas
           perform mostrar-fonte-favorita
           perform zebrar-itens.
       chamar-colunas-favo-fim.
           exit.


       CRIAR-LISTVIEW-PROBLEMA SECTION.
          initialize indice

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
                 using z"Origem" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "centered"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
                 using z"Gerada" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "centered"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
                 using z"Banco" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "centered"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
                 using z"Agência" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "centered"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
             using z"Conta Corrente" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "centered"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
            using z"Cedente" returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
            using z"Nome Cedente" returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
                 using z"Carteira" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "centered"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
           using z"Número Controle" returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
           using z"Nosso Número" returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
           using z"Número Docto" returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
           using z"Data Vencto" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "centered"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
           using z"Data Ocorrência" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "centered"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
           using z"Data Crédito" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "centered"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
           using z"Valor Título" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "RightJustified"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
           using z"Valor Pago" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "RightJustified"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
           using z"Valor Desconto" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "RightJustified"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
           using z"Valor Juros" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "RightJustified"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
           using z"Valor Multa" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "RightJustified"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
           using z"Nome Sacado" returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
           using z"Ocorrência" returning lnkobjetoscolunaP(indice)
          invoke lnkobjetoscolunaP(indice) "centered"
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
         using z"Motivo Ocorrência 1"
                                     returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
         using z"Motivo Ocorrência 2"
                                     returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
         using z"Motivo Ocorrência 3"
                                     returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
         using z"Motivo Ocorrência 4"
                                     returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
         using z"Motivo Ocorrência 5"
                                     returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
                 using z"Descrição Ocorrência 1"
                     returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
                 using z"Descrição Ocorrência 2"
                     returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
                 using z"Descrição Ocorrência 3"
                     returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
                 using z"Descrição Ocorrência 4"
                     returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

          add 1 to indice
          invoke gs-acp-problema "adicionarColunaZ"
                 using z"Descrição Ocorrência 5"
                     returning lnkobjetoscolunaP(indice)
          move indice to lnkcolunasP(indice)

           perform mostrar-fonte-favoP
           perform mostrar-colunas-favoP

           invoke gs-acp-problema "gridLines"
           invoke gs-acp-problema "noBorder".
       CRIAR-LISTVIEW-PROBLEMA-FIM.
           EXIT.

       mostrar-colunas-favoP section.
          initialize wsTexto
          move "listview-imp001" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-acp-problema
                                  wsTexto
                                  lnktabelaP.
       mostrar-colunas-favoP-fim.
           exit.

       mostrar-fonte-favoP section.
           move "listview-imp001" to wsTexto
           invoke aListview "criarFonte"
                             using lnkusu gs-acp-problema wsTexto.
       mostrar-fonte-favoP-fim.
           exit.

       zebrar-itensP section.
           move "listview-imp001" to wsTexto
           invoke aListview "zebrarCor"
                             using lnkusu gs-acp-problema wsTexto
           invoke gs-acp-problema "redrawallitems".
       zebrar-itensP-fim.
           exit.

       chamar-colunas-favoP section.
           move "listview-imp001" to wsTexto
           call "COLFAV" using lnkusu
                               gs-acp-problema
                               wsTexto
                               lnktabelaP

           perform mostrar-colunas-favoP
           perform mostrar-fonte-favoP
           perform zebrar-itensP.
       chamar-colunas-favoP-fim.
           exit.

       LER-PORTADOR SECTION.
           MOVE GS-ACP-PORTADOR        TO PORTADOR
           READ CAD018 INVALID KEY
                MOVE "***********"     TO NOME-PORT
           END-READ
           MOVE NOME-PORT              TO GS-DESC-PORTADOR.

       POP-PORTADOR SECTION.
           CALL   "CAP018T" USING PARAMETROS-W POP-UP
           CANCEL "CAP018T"
           MOVE POP-UP(1: 30)            TO GS-DESC-PORTADOR
           MOVE POP-UP(33: 4)            TO GS-ACP-PORTADOR.

       CARREGAR-PORTADOR SECTION.
           MOVE "IMP001"               TO RETPORT-PROGRAMA
           READ RETPORT INVALID KEY
                INITIALIZE REG-RETPORT.

           MOVE RETPORT-PORTADOR       TO GS-ACP-PORTADOR
           READ CAD018 INVALID KEY
                MOVE "***********"     TO NOME-PORT
           END-READ
           MOVE NOME-PORT              TO GS-DESC-PORTADOR.

       GRAVAR-PORTADOR SECTION.
           MOVE "IMP001"               TO RETPORT-PROGRAMA
           MOVE GS-ACP-PORTADOR        TO RETPORT-PORTADOR
           WRITE REG-RETPORT INVALID KEY
                 REWRITE REG-RETPORT INVALID KEY
                      MOVE "Erro de Gravação...RETPORT" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM.

       VALIDAR-ARQUIVO SECTION.
           call "CBL_CHECK_FILE_EXIST"     using gs-acp-caminho
                                                 file-details
                                       returning status-code
           if status-code <> 0
              move "Arquivo Não Encontrado" to mensagem
              move "C" to tipo-msg
              perform exibir-mensagem
              move 1 to gs-flag-critica
           else
              move gs-acp-caminho to arquivo-retorno.

       CENTRALIZAR SECTION.
          move-object-handle win1 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE 1            TO ERRO-W GS-EXIT-FLG.

       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CARREGA-LISTA SECTION.
           CLOSE      RETORNO
           OPEN INPUT RETORNO

           IF ST-RET <> "00"
              STRING "Erro de Abertura..." ARQUIVO-RETORNO INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              MOVE "REFRESH-DATA" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM

              INVOKE GS-ACP-LISTVIEW "DeleteAll"

              MOVE ZEROS TO LIN
                            VALOR-TOTAL
                            ACRESCIMO-TOTAL
                            QTDE-PARC
              PERFORM UNTIL ST-RET = "10"
                  READ RETORNO AT END
                       MOVE "10" TO ST-RET
                  NOT AT END
                       INITIALIZE RET-RETORNO
                       UNSTRING REG-RETORNO DELIMITED BY ";"
                            INTO RET-EMPRESA
                                 RET-EMPRESA-GER
                                 RET-BANCO
                                 RET-AGENCIA
                                 RET-CONTA
                                 RET-CEDENTE
                                 RET-NOME-CEDENTE
                                 RET-CARTEIRA
                                 RET-NUM-CONTROLE
                                 RET-NOSSO-NUM
                                 RET-NUM-DOCTO
                                 RET-DATA-VENCTO
                                 RET-DATA-OCORRENCIA
                                 RET-DATA-CREDITO
                                 RET-VALOR-TITULO
                                 RET-VALOR-PAGO
                                 RET-VALOR-DESCONTO
                                 RET-VALOR-JUROS
                                 RET-VALOR-MULTA
                                 RET-NOME-SACADO
                                 RET-OCORRENCIA
                                 RET-MOTIVO1
                                 RET-MOTIVO2
                                 RET-MOTIVO3
                                 RET-MOTIVO4
                                 RET-MOTIVO5
                                 RET-D-MOTIVO1
                                 RET-D-MOTIVO2
                                 RET-D-MOTIVO3
                                 RET-D-MOTIVO4
                                 RET-D-MOTIVO5

                       MOVE SPACES             TO GS-LINDET1
                       MOVE RET-NUM-DOCTO      TO GS-EXIBE-CODIGO
                       MOVE "REFRESH-DATA"     TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM

                       IF RET-EMPRESA <> EMPRESA-W
                          MOVE "10" TO ST-RET
                          MOVE "Arquivo processado não é da empresa loga
      -                        "da no Sistema" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                       ELSE
                          initialize indice
                          invoke gs-acp-listview "adicionarItem"
                              returning wsItem

                          add 1 to indice
                          initialize wsTexto
                          string RET-EMPRESA X"00" delimited by "  "
                                 into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-EMPRESA-GER X"00" delimited by "  "
                                 into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-BANCO X"00" delimited by "  "
                                 into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-AGENCIA X"00" delimited by "  "
                                 into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-CONTA X"00" delimited by "  "
                                 into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-CEDENTE X"00" delimited by "  "
                                 into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-NOME-CEDENTE X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-CARTEIRA X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-NUM-CONTROLE X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-NOSSO-NUM X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-NUM-DOCTO X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-DATA-VENCTO X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-DATA-OCORRENCIA X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          STRING RET-DATA-CREDITO X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto


                          add 1 to indice
                          initialize wsTexto

                          INSPECT RET-VALOR-TITULO REPLACING
                                                   all "." by ","
                          MOVE FUNCTION NUMVAL(RET-VALOR-TITULO)
                                                  TO VALOR-TITULO

                          MOVE VALOR-TITULO       TO VALOR-W

                          string VALOR-W X"00" into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          INSPECT RET-VALOR-PAGO REPLACING
                                                   all "." by ","
                          MOVE FUNCTION NUMVAL(RET-VALOR-PAGO)
                                                  TO VALOR-PAGO
                          MOVE VALOR-PAGO         TO VALOR-W
                          string VALOR-W X"00" into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          INSPECT RET-VALOR-DESCONTO REPLACING
                                                   all "." by ","
                          MOVE FUNCTION NUMVAL(RET-VALOR-DESCONTO)
                                                  TO VALOR-DESCONTO
                          MOVE VALOR-DESCONTO     TO VALOR-W
                          string VALOR-W X"00" into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          INSPECT RET-VALOR-JUROS REPLACING
                                                   all "." by ","
                          MOVE FUNCTION NUMVAL(RET-VALOR-JUROS)
                                                  TO VALOR-JUROS
                          MOVE VALOR-JUROS        TO VALOR-W
                          string VALOR-W X"00" into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          INSPECT RET-VALOR-MULTA REPLACING
                                                   all "." by ","
                          MOVE FUNCTION NUMVAL(RET-VALOR-MULTA)
                                                  TO VALOR-MULTA
                          MOVE VALOR-MULTA        TO VALOR-W
                          string VALOR-W X"00" into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-NOME-SACADO X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-OCORRENCIA X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-MOTIVO1 X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-MOTIVO2 X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-MOTIVO3 X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-MOTIVO4 X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          string RET-MOTIVO5 X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          STRING RET-D-MOTIVO1 X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          STRING RET-D-MOTIVO2 X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          STRING RET-D-MOTIVO3 X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          STRING RET-D-MOTIVO4 X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                          add 1 to indice
                          initialize wsTexto
                          STRING RET-D-MOTIVO5 X"00"
                                 delimited by "  " into wsTexto
                          invoke gs-acp-listview "preencherColunaZ"
                                 using wsItem lnkcolunas(indice) wsTexto

                       END-IF
                  END-READ
              END-PERFORM
           END-IF
           CLOSE      RETORNO
           OPEN INPUT RETORNO
           perform mostrar-colunas-favoritas
           perform mostrar-fonte-favorita
           perform zebrar-itens
           refresh-object principal.
      *--------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO LIN.

           COPY CONDENSA.

           INITIALIZE VALOR-TOTAL
                      ACRESCIMO-TOTAL
                      QTDE-PARC

           PERFORM CABECALHO.

           PERFORM UNTIL ST-RET = "10"
                READ RETORNO AT END
                     MOVE "10" TO ST-RET
                NOT AT END
                     INITIALIZE RET-RETORNO
                     UNSTRING REG-RETORNO DELIMITED BY ";"
                          INTO RET-EMPRESA
                               RET-EMPRESA-GER
                               RET-BANCO
                               RET-AGENCIA
                               RET-CONTA
                               RET-CEDENTE
                               RET-NOME-CEDENTE
                               RET-CARTEIRA
                               RET-NUM-CONTROLE
                               RET-NOSSO-NUM
                               RET-NUM-DOCTO
                               RET-DATA-VENCTO
                               RET-DATA-OCORRENCIA
                               RET-DATA-CREDITO
                               RET-VALOR-TITULO
                               RET-VALOR-PAGO
                               RET-VALOR-DESCONTO
                               RET-VALOR-JUROS
                               RET-VALOR-MULTA
                               RET-NOME-SACADO
                               RET-OCORRENCIA
                               RET-MOTIVO1
                               RET-MOTIVO2
                               RET-MOTIVO3
                               RET-MOTIVO4
                               RET-MOTIVO5
                               RET-D-MOTIVO1
                               RET-D-MOTIVO2
                               RET-D-MOTIVO3
                               RET-D-MOTIVO4
                               RET-D-MOTIVO5

                     INITIALIZE LINDET
                     MOVE RET-NUM-DOCTO      TO GS-EXIBE-CODIGO
                     MOVE "REFRESH-DATA"     TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM

                     IF RET-EMPRESA <> EMPRESA-W
                        MOVE "10" TO ST-RET
                        MOVE "Arquivo processado não é da empresa loga
      -                      "da no Sistema" TO MENSAGEM
                        MOVE "C" TO TIPO-MSG
                        PERFORM EXIBIR-MENSAGEM
                     ELSE
                        MOVE RET-NOSSO-NUM      TO NOSSO-NR-REL
                        MOVE RET-NUM-CONTROLE(1:1) TO CLASS-REL
                        MOVE RET-NUM-CONTROLE(2:8) TO CONTALB-REL
                        MOVE RET-NUM-DOCTO      TO NR-TITULO-REL
                        MOVE RET-DATA-VENCTO    TO DATA-VENCTO-REL

                        INSPECT RET-VALOR-TITULO REPLACING
                                                 all "." by ","
                        MOVE FUNCTION NUMVAL(RET-VALOR-TITULO)
                                                TO VALOR-TITULO

                        MOVE VALOR-TITULO       TO VALOR-W
                        MOVE VALOR-W            TO VALOR-REC-TIT-REL
                        MOVE FUNCTION NUMVAL(RET-MOTIVO1)
                                                TO REJEICAO-REL
                        IF RET-DATA-CREDITO <> SPACES
                           MOVE FUNCTION NUMVAL(RET-DATA-CREDITO)
                                                TO AUX-DATA
                           STRING AUX-DATA(1:2) "/"
                                  AUX-DATA(3:2) "/"
                                  AUX-DATA(7:2) INTO DATA-PGTO-REL
                        ELSE
                           MOVE SPACES          TO DATA-PGTO-REL
                        END-IF
                        MOVE FUNCTION NUMVAL(RET-OCORRENCIA) TO
                                             OCORRENCIA
                        IF OCORRENCIA = 06
                           INSPECT RET-VALOR-PAGO REPLACING
                                                  all "." by ","
                           MOVE FUNCTION NUMVAL(RET-VALOR-PAGO)
                                                 TO VALOR-PAGO

                           PERFORM CALCULA-DIFERENCA
                           ADD VALOR-TITULO      TO VALOR-TOTAL
                           ADD 1                 TO QTDE-PARC

                           MOVE VALOR-E1         TO ACRESCIMO-REL
                           ADD  VALOR-W1         TO ACRESCIMO-TOTAL
                        END-IF
                        MOVE RET-OCORRENCIA     TO OCORRENCIA-REL

                        WRITE REG-RELAT FROM LINDET
                        END-WRITE
                        ADD 1 TO LIN
                        IF LIN > 56
                           PERFORM CABECALHO
                        END-IF
                        INITIALIZE LINDET
                     END-IF
                END-READ
           END-PERFORM
           PERFORM TOTALIZA-REL.

       TOTALIZA-REL SECTION.
           WRITE REG-RELAT FROM CAB03
           MOVE VALOR-TOTAL     TO VALOR-REC-TIT-TOT
           MOVE QTDE-PARC       TO QTDE-PARC-TOT
           MOVE ACRESCIMO-TOTAL TO ACRESCIMO-TOT
           WRITE REG-RELAT FROM LINTOT

           COPY DESCONDENSA.
           CLOSE      RETORNO
           OPEN INPUT RETORNO.

       CABECALHO SECTION.
           IF LIN = ZEROS
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB02.
           MOVE 4 TO LIN.
      *----------------------------------------------------------
      * STATUS DA OCORRENCIA DO ARQUIVO DE RETORNO
      * 02 - ENTRADA CONFIRMADA (NAO FAZER NADA)
      * 03 - ENTRADA REJEITADA (MUDAR P/ PORTADOR PROBLEMATICO)
      * 06 - LIQUIDACAO NORMAL (BAIXAR O TÍTULO)
       ATUALIZA-A-RECEBER SECTION.
           CLOSE      CRD020 CRD020B
           OPEN I-O   CRD020 CRD020B

           INVOKE GS-ACP-PROBLEMA "DeleteAll"


           INITIALIZE WSITEM
           INVOKE GS-ACP-LISTVIEW "Size" RETURNING WSSIZE

           PERFORM WSSIZE TIMES
               ADD 1 TO WSITEM

               INVOKE GS-ACP-LISTVIEW "ITEMATINDEX"
                      USING WSITEM RETURNING AITEM

               INVOKE AITEM "GETCOLUMNVALUE"
                      USING wsColunaOcorrencia RETURNING AOBJETO
               INITIALIZE WSTEXTO
               INVOKE AOBJETO "GETVALUE"  RETURNING WSTEXTO

               MOVE FUNCTION NUMVAL(WSTEXTO) TO OCORRENCIA

               IF OCORRENCIA <> 2 AND 3 AND 6
                  PERFORM TITULOS-NAO-ENCONTRADO
                  move 1 to mensagem
               ELSE
                  INVOKE AITEM "GETCOLUMNVALUE"
                         USING wsColunaDocto RETURNING AOBJETO
                  INITIALIZE WSTEXTO
                  INVOKE AOBJETO "GETVALUE"  RETURNING WSTEXTO

                  MOVE WSTEXTO(1:9)  TO COD-COMPL-CR20
                  MOVE WSTEXTO(10:5) TO SEQ-CR20

                  READ CRD020 INVALID KEY
                       PERFORM TITULOS-NAO-ENCONTRADO
                       move 1 to mensagem
                  NOT INVALID KEY
                       EVALUATE OCORRENCIA
                         WHEN 02 PERFORM GRAVA-NR-BANCO
                         WHEN 03 PERFORM ENTRADA-REJEITADA
                         WHEN 06 PERFORM BAIXAR-TITULO
                       END-EVALUATE
                  END-READ
               END-IF
           END-PERFORM.


           CLOSE      RETORNO CRD020 CRD020B
           OPEN INPUT RETORNO CRD020 CRD020B

           MOVE "UNSHOW-WIN3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       GRAVA-ANOTACAO SECTION.
           CLOSE    CRD200 CRD201
           OPEN I-O CRD200 CRD201
           IF ST-CRD200 = "35"  CLOSE CRD200  OPEN OUTPUT CRD200
                                CLOSE CRD200  OPEN I-O CRD200.
           IF ST-CRD201 = "35"  CLOSE CRD201  OPEN OUTPUT CRD201
                                CLOSE CRD201  OPEN I-O CRD201.
           IF ST-CRD200 <> "00"
              MOVE "ERRO ABERTURA CRD200: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD200 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD201 <> "00"
              MOVE "ERRO ABERTURA CRD201: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD201 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR200
           MOVE ZEROS TO ULT-SEQ.
           MOVE ALL "9" TO SEQ-CR200
           START CRD200 KEY IS LESS THAN CHAVE-CR200 NOT INVALID KEY
                READ CRD200 PREVIOUS NOT AT END
                     IF COD-COMPL-CR200 = COD-COMPL-CR20
                        MOVE SEQ-CR200 TO ULT-SEQ.

           ADD 1 TO ULT-SEQ.
           MOVE ULT-SEQ      TO SEQ-CR200.
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR200.
           MOVE ZEROS        TO DATA-RETORNO-CR200
           MOVE USUARIO-W    TO USUARIO-CR200
           MOVE DATA-DIA-I   TO DATA-MOVTO-CR200
           MOVE HORA-BRA(1: 4) TO HORA-MOVTO-CR200

           MOVE ZEROS TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
              WRITE REG-CRD200 INVALID KEY
                    ADD 1 TO SEQ-CR200
                    CONTINUE
              NOT INVALID KEY
                    MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200      TO SEQ-CR201.
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR201.
           MOVE ZEROS          TO SUBSEQ-CR201.
           MOVE "ALTERACAO EFETUADA NO TITULO            - MOTIVO: "
                  TO ANOTACAO-CR201(1: 80)
           MOVE NR-DOCTO-CR20  TO ANOTACAO-CR201(30: 10).
           MOVE ZEROS TO ST-CRD201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
                   ADD 1 TO SUBSEQ-CR201
                   CONTINUE
             NOT INVALID KEY
                   MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.

           PERFORM VARYING COL1 FROM 1 BY 80 UNTIL COL1 > 640
              MOVE GS-ANOTACAO(COL1: 80) TO ANOTACAO-W
              MOVE ANOTACAO-W TO ANOTACAO-CR201
              IF ANOTACAO-W <> SPACES
                 ADD 1 TO SUBSEQ-CR201
                 WRITE REG-CRD201 INVALID KEY
                       ADD 1 TO SUBSEQ-CR201
                       WRITE REG-CRD201
                       END-WRITE
                 END-WRITE
              END-IF
           END-PERFORM.
           CLOSE      CRD200 CRD201.

       GRAVA-NR-BANCO SECTION.
           INVOKE AITEM "GETCOLUMNVALUE"
                  USING wsColunaNossoNum RETURNING AOBJETO
           INITIALIZE WSTEXTO
           INVOKE AOBJETO "GETVALUE"  RETURNING WSTEXTO

           MOVE WSTEXTO               TO OUTRO-DOCTO-CR20
           REWRITE REG-CRD020 INVALID KEY
                PERFORM TITULOS-NAO-ENCONTRADO
           NOT INVALID KEY
                MOVE SPACES TO GS-ANOTACAO
                STRING "ENTRADA CONFIRMADA NOSSO NUMERO ATUALIZADO "
                       OUTRO-DOCTO-CR20 INTO GS-ANOTACAO
                PERFORM GRAVA-ANOTACAO
           END-REWRITE.
       ENTRADA-REJEITADA SECTION.
           MOVE PORTADOR-PAR002-R TO PORTADOR-CR20
      *    MOVE 01 TO PORTADOR-CR20.
           REWRITE REG-CRD020 INVALID KEY
                PERFORM TITULOS-NAO-ENCONTRADO
           NOT INVALID KEY
                MOVE SPACES TO GS-ANOTACAO
                STRING "ENTRADA REJEITADA" INTO GS-ANOTACAO
                PERFORM GRAVA-ANOTACAO
           END-REWRITE.
       BAIXAR-TITULO SECTION.
           INVOKE AITEM "GETCOLUMNVALUE"
                  USING wsColunaValorPg  RETURNING AOBJETO
           INITIALIZE WSTEXTO
           INVOKE AOBJETO "GETVALUE"  RETURNING WSTEXTO
           MOVE FUNCTION NUMVAL(WSTEXTO) TO VALOR-W

           MOVE VALOR-W                 TO VALOR-LIQ-CR20
           IF VALOR-LIQ-CR20 <> VALOR-TOT-CR20
              COMPUTE DIFERENCA-W1 = VALOR-LIQ-CR20 - VALOR-TOT-CR20
              IF DIFERENCA-W1 > 0
                 MOVE DIFERENCA-W1      TO JURO-RCTO-CR20
              ELSE
                 MOVE DIFERENCA-W1      TO DESCONTO-CR20
              END-IF
           END-IF

           INVOKE AITEM "GETCOLUMNVALUE"
                  USING wsColunaDataCred RETURNING AOBJETO
           INITIALIZE WSTEXTO
           INVOKE AOBJETO "GETVALUE"  RETURNING WSTEXTO
           MOVE FUNCTION NUMVAL(WSTEXTO) TO AUX-DATA

           MOVE AUX-DATA(1:2)           TO DATA-RCTO-CR20(7:2)
           MOVE AUX-DATA(3:2)           TO DATA-RCTO-CR20(5:2)
           MOVE AUX-DATA(5:4)           TO DATA-RCTO-CR20(1:4)
           MOVE 2                       TO SITUACAO-CR20
           MOVE ZEROS                   TO VALOR-SALDO-CR20
           MOVE "4-Receb.Bco."          TO FORMA-PAGTO-CR20
           REWRITE REG-CRD020 INVALID KEY
                PERFORM TITULOS-NAO-ENCONTRADO
           NOT INVALID KEY
                INITIALIZE REG-CRD020B
                MOVE CLASS-CLIENTE-CR20 TO CLASS-CLIENTE-CR20B
                MOVE CLIENTE-CR20       TO CLIENTE-CR20B
                MOVE SEQ-CR20           TO SEQ-CR20B
                MOVE VALOR-TOT-CR20     TO VALOR-TOT-CR20B
                MOVE JURO-RCTO-CR20     TO JURO-RCTO-CR20B
                MOVE DESCONTO-CR20      TO DESCONTO-CR20B
                MOVE VALOR-TOT-CR20     TO VALOR-BAIXA-CR20B
                MOVE MULTA-RCTO-CR20    TO MULTA-RCTO-CR20B
                MOVE DATA-RCTO-CR20     TO DATA-RCTO-CR20B
                MOVE VALOR-LIQ-CR20     TO VALOR-LIQ-CR20B
                MOVE SEQ-CAIXA-CR20     TO SEQ-CAIXA-CR20B
                MOVE "4-Receb.Bco."     TO FORMA-PAGTO-CR20B
                MOVE DCR-MEM-CR20       TO DCR-MEM-CR20B
                WRITE REG-CRD020B
                END-WRITE
                MOVE SPACES TO GS-ANOTACAO
                STRING "RECEBIDO PELO RETORNO BANCARIO NOSSO NUMERO "
                       OUTRO-DOCTO-CR20 INTO GS-ANOTACAO
                PERFORM GRAVA-ANOTACAO
           END-REWRITE.
       PROCURA-TITULO SECTION.
      *    procura o título através da chave vencto, docto e valor
           MOVE FUNCTION NUMVAL(RET-DATA-VENCTO) TO AUX-DATA
           MOVE AUX-DATA(1:2)   TO DATA-VENCTO(7:2)
           MOVE AUX-DATA(3:2)   TO DATA-VENCTO(5:2)
           MOVE AUX-DATA(5:4)   TO DATA-VENCTO(1:4)

           MOVE DATA-VENCTO     TO DATA-VENCTO-CR20

           MOVE ZEROS           TO SITUACAO-CR20
                                   COD-COMPL-CR20
           MOVE ZEROS           TO NAO-ENCONTRADO

           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD AT END
                      MOVE "10"      TO ST-CRD020
                 NOT AT END
                     IF SITUACAO-CR20    <> 0 OR
                        DATA-VENCTO-CR20 <> DATA-VENCTO
                           MOVE "10" TO ST-CRD020
                     ELSE
                        IF COD-COMPL-CR20 <> RET-NUM-CONTROLE(1:9)
                           CONTINUE
                        ELSE
                           IF VALOR-TOT-CR20 <> RET-VALOR-TITULO
                              CONTINUE
                           ELSE
                              MOVE FUNCTION NUMVAL(RET-OCORRENCIA)
                                         TO OCORRENCIA
                              EVALUATE OCORRENCIA
                                WHEN 02 PERFORM GRAVA-NR-BANCO
                                WHEN 03 PERFORM ENTRADA-REJEITADA
                                WHEN 06 PERFORM BAIXAR-TITULO
                              END-EVALUATE
                              MOVE 1 TO NAO-ENCONTRADO
                              MOVE "10" TO ST-CRD020
                           END-IF
                        END-IF
                     END-IF
                 END-READ
           END-PERFORM.
           IF NAO-ENCONTRADO = 0
              PERFORM TITULOS-NAO-ENCONTRADO.

       TITULOS-NAO-ENCONTRADO SECTION.
           invoke gs-acp-problema "AddItem" using aItem.

       ERRO-GRAVACAO-CRD020 SECTION.
           MOVE "ERRO GRAVAÇÃO CRD020" TO GS-MENSAGEM-ERRO
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CALCULA-DIFERENCA SECTION.
           COMPUTE DIFERENCA-W1 = VALOR-PAGO - VALOR-TITULO
           MOVE DIFERENCA-W1             TO VALOR-W1
           MOVE VALOR-W1                 TO VALOR-E1.
      *----------------------------------------------------------
      * Títulos retornados do banco, não encontrados no arquivo CRD020
       LISTA-NAO-ENCONTRADOS SECTION.
           perform mostrar-colunas-favoP
           perform mostrar-fonte-favoP
           perform zebrar-itensP
           refresh-object win1.
      *----------------------------------------------------------
       IMPRIME-NAO-ENCONTRADO SECTION.
           MOVE ZEROS TO LIN.

           COPY CONDENSA.

           PERFORM CABECALHO1

           INITIALIZE VALOR-TOTAL
                      ACRESCIMO-TOTAL
                      QTDE-PARC

           initialize wsItem

           INVOKE GS-ACP-PROBLEMA "Size" returning wsSize

           perform wsSize times
               add 1 to wsItem

               INVOKE GS-ACP-LISTVIEW "ITEMATINDEX"
                      USING WSITEM RETURNING AITEM

      *>Coluna documento
               INVOKE AITEM "GETCOLUMNVALUE"
                      USING wsColunaDocto RETURNING AOBJETO
               INITIALIZE WSTEXTO
               INVOKE AOBJETO "GETVALUE"  RETURNING WSTEXTO
               MOVE WSTEXTO(1:1)       TO CLASS-REL
               MOVE WSTEXTO(2:8)       TO CONTALB-REL

      *>Coluna Nosso Número
               INVOKE AITEM "GETCOLUMNVALUE"
                      USING wsColunaNossoNum RETURNING AOBJETO
               INITIALIZE WSTEXTO
               INVOKE AOBJETO "GETVALUE"  RETURNING WSTEXTO
               MOVE WSTEXTO            TO NOSSO-NR-REL

      *>Coluna Numero docto
               INVOKE AITEM "GETCOLUMNVALUE"
                      USING wsColunaNumDocto RETURNING AOBJETO
               INITIALIZE WSTEXTO
               INVOKE AOBJETO "GETVALUE"  RETURNING WSTEXTO
               MOVE WSTEXTO            TO NR-TITULO-REL

      *>Coluna Valor Título
               INVOKE AITEM "GETCOLUMNVALUE"
                      USING wsColunaValorDocto RETURNING AOBJETO
               INITIALIZE WSTEXTO
               INVOKE AOBJETO "GETVALUE"  RETURNING WSTEXTO
               MOVE FUNCTION NUMVAL(WSTEXTO)  TO VALOR-REC-TIT-REL
                                                 VALOR-TITULO

      *>Coluna Data Vencimento
               INVOKE AITEM "GETCOLUMNVALUE"
                      USING wsColunaDtVencto RETURNING AOBJETO
               INITIALIZE WSTEXTO
               INVOKE AOBJETO "GETVALUE"  RETURNING WSTEXTO
               MOVE FUNCTION NUMVAL(WSTEXTO)  TO AUX-DATA
               MOVE SPACES                    TO DATA-VENCTO-REL
               STRING AUX-DATA(1:2) "/" AUX-DATA(3:2) "/"
                      AUX-DATA(7:2)         INTO DATA-VENCTO-REL

      *>Coluna Data Crédito
               INVOKE AITEM "GETCOLUMNVALUE"
                      USING wsColunaDataCred RETURNING AOBJETO
               INITIALIZE WSTEXTO
               INVOKE AOBJETO "GETVALUE"  RETURNING WSTEXTO
               MOVE FUNCTION NUMVAL(WSTEXTO)  TO AUX-DATA

               MOVE SPACES                    TO DATA-PGTO-REL
               IF AUX-DATA > 0
                  STRING AUX-DATA(1:2) "/" AUX-DATA(3:2) "/"
                         AUX-DATA(7:2)      INTO DATA-PGTO-REL
               END-IF

      *>Coluna Ocorrencia
               INVOKE AITEM "GETCOLUMNVALUE"
                      USING wsColunaOcorrencia RETURNING AOBJETO
               INITIALIZE WSTEXTO
               INVOKE AOBJETO "GETVALUE"  RETURNING WSTEXTO
               MOVE FUNCTION NUMVAL(WSTEXTO)  TO OCORRENCIA

      *>Coluna Valor Pago
               INVOKE AITEM "GETCOLUMNVALUE"
                      USING wsColunaValorPg RETURNING AOBJETO
               INITIALIZE WSTEXTO
               INVOKE AOBJETO "GETVALUE"  RETURNING WSTEXTO
               MOVE FUNCTION NUMVAL(WSTEXTO)  TO VALOR-PAGO


               IF OCORRENCIA = 06
                  ADD 1                       TO QTDE-PARC
                  ADD VALOR-TITULO            TO VALOR-TOTAL
                  PERFORM CALCULA-DIFERENCA
                  MOVE VALOR-E1               TO ACRESCIMO-REL
                  ADD  VALOR-W1               TO ACRESCIMO-TOTAL
               END-IF
               MOVE OCORRENCIA                TO OCORRENCIA-REL

      *>Coluna Rejeicao1
               INVOKE AITEM "GETCOLUMNVALUE"
                      USING wsColunaRej1  RETURNING AOBJETO
               INITIALIZE WSTEXTO
               INVOKE AOBJETO "GETVALUE"  RETURNING WSTEXTO
               MOVE FUNCTION NUMVAL(WSTEXTO)  TO REJEICAO-REL

               WRITE REG-RELAT FROM LINDET
               END-WRITE
               ADD 1 TO LIN
               IF LIN > 56
                  PERFORM CABECALHO1
               END-IF
               INITIALIZE LINDET

           end-perform

           COPY DESCONDENSA.
       CABECALHO1 SECTION.
           IF LIN = ZEROS
              WRITE REG-RELAT FROM CAB01A
           ELSE WRITE REG-RELAT FROM CAB01A AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB02.
           MOVE 4 TO LIN.
      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "IMP001" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       exibir-mensagem section.
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
           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "IMP001"            to logacess-programa
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

           CLOSE CRD020 CRD020B RETORNO CAD018 RETPORT PAR002
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

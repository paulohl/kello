       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCAIXA.
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
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CRPX020.

           COPY PARX002.

           COPY CRPX020B.

           COPY LOGACESS.SEL.

           COPY CAPX018.

           COPY RETPORT.SEL.

           SELECT RETORNO  ASSIGN       TO ARQUIVO-RETORNO
                           ORGANIZATION IS LINE SEQUENTIAL
                           ACCESS MODE  IS      SEQUENTIAL
                           STATUS       IS          ST-RET.

           SELECT PROBLEMA ASSIGN       TO      "PROBLEMA"
                           ORGANIZATION IS LINE SEQUENTIAL
                           ACCESS MODE  IS      SEQUENTIAL
                           STATUS       IS     ST-PROBLEMA.

           SELECT RELAT    ASSIGN TO PRINTER NOME-IMPRESSORA.



       DATA DIVISION.
       FILE SECTION.

           COPY CRPW020.

           COPY PARW002.

           COPY CRPW020B.

           COPY CAPW018.

           COPY RETPORT.FD.

           COPY LOGACESS.FD.


       FD  RETORNO.
       01  REG-RETORNO          PIC X(240).

       FD  PROBLEMA.
       01  REG-PROBLEMA         PIC X(240).

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "RCAIXA.CPB".
           COPY "RCAIXA.CPY".
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
           05 ST-RET              PIC XX     VALUE SPACES.
           05 ST-PAR002           PIC XX     VALUE SPACES.
           05 ST-PROBLEMA         PIC XX     VALUE SPACES.
           05 FS-LOGACESS         PIC XX     VALUE SPACES.
           05 FS-RETPORT          PIC XX     VALUE SPACES.
           05 OPCAO               PIC 9      VALUE ZEROS.
           05 VALOR-W             PIC ZZZ.ZZZ.ZZZ,ZZ.
           05 VALOR-W1            PIC S9(11)V99 VALUE ZEROS.
           05 DIFERENCA-W         PIC S9(13)  VALUE ZEROS.
           05 DIFERENCA-W1        PIC S9(11)V99  VALUE ZEROS.
           05 VALOR-TOTAL         PIC 9(11)V99 VALUE ZEROS.
           05 ACRESCIMO-TOTAL     PIC S9(11)V99 VALUE ZEROS.
           05 LETRA               PIC X      VALUE SPACES.
           05 NAO-ENCONTRADO      PIC 9      VALUE ZEROS.
           05 TRACO               PIC X(80)  VALUE ALL '-'.
           05 DATA-DIA            PIC 9(6)   VALUE ZEROS.
           05 DATA-DIA-I          PIC 9(6)   VALUE ZEROS.
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
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       77 POP-UP                       PIC X(65).
       01 REGISTRO-T.
          05 RT-BANCO                  PIC 9(03).
          05 RT-LOTE                   PIC 9(04).
          05 RT-REGISTRO               PIC X(01).
          05 RT-NUMERO-REGISTRO        PIC 9(05).
          05 RT-SEGMENTO               PIC X(01).
          05 RT-CNAB                   PIC X(01).
          05 RT-COD-MOV                PIC 9(02).
          05 RT-AG-CONTA               PIC 9(05).
          05 RT-DG-CONTA               PIC 9(01).
          05 RT-NR-CONTA               PIC 9(12).
          05 RT-DG-NR-CONTA            PIC 9(01).
          05 RT-DG-AG-NR-CONTA         PIC 9(01).
          05 RT-USO                    PIC X(09).
          05 RT-NOSSO-NUMERO           PIC 9(11).
          05 RT-CARTEIRA               PIC 9(01).
          05 RT-NUMERO-DOCTO           PIC X(15).
          05 RT-VENCTO                 PIC 9(08).
          05 RT-VLRTITULO              PIC 9(13)V99.
          05 RT-UF-CEDENTE             PIC X(02).
          05 RT-BRANCO                 PIC X(01).
          05 RT-COD-ENT-CED            PIC 9(05).
          05 RT-DG-COD-ENT-CED         PIC 9(01).
          05 RT-NOME-CED               PIC X(25).
          05 RT-COD-MOEDA              PIC 9(02).
          05 RT-TIPO-INSC              PIC 9(01).
          05 RT-NR-INSC                PIC 9(15).
          05 RT-NOME                   PIC X(40).
          05 RT-CNAB2                  PIC X(10).
          05 RT-VLR-TARIFA             PIC 9(13)V99.
          05 RT-OCORRENCIA             PIC 9(10).
          05 RT-CNAB3                  PIC X(17).

       01 REGISTRO-U.
          05 RU-BANCO                  PIC 9(03).
          05 RU-LOTE                   PIC 9(04).
          05 RU-REGISTRO               PIC X(01).
          05 RU-NR-REGISTRO            PIC 9(05).
          05 RU-SEGMENTO               PIC X(01).
          05 RU-CNAB                   PIC X(01).
          05 RU-COD-MOV                PIC 9(02).
          05 RU-JUROS                  PIC 9(13)V99.
          05 RU-DESCONTOS              PIC 9(13)V99.
          05 RU-ABATIMENTO             PIC 9(13)V99.
          05 RU-IOF                    PIC 9(13)V99.
          05 RU-VALOR-PAGO             PIC 9(13)V99.
          05 RU-VALOR-CRED-BRUTO       PIC 9(13)V99.
          05 RU-OUTRAS-DESPESAS        PIC 9(13)V99.
          05 RU-PERC-CRED              PIC 9(03)V99.
          05 RU-BRANCOS                PIC X(10).
          05 RU-DATA-OCORRENCIA        PIC 9(08).
          05 RU-DATA-CREDITO           PIC 9(08).
          05 RU-CNAB2                  PIC X(87).

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
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN I-O    CRD020 CRD020B CAD018 RETPORT PAR002
           CLOSE       CRD020 CRD020B CAD018 RETPORT PAR002
           OPEN INPUT  CRD020 CRD020B CAD018 RETPORT PAR002

           OPEN OUTPUT PROBLEMA

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


           MOVE "RCAIXA"  TO PROGRAMA-PAR002
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
           move "RCAIXA"          to logacess-programa
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
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GERAR-RELATORIO-TRUE
                    PERFORM CARREGA-LISTA
               WHEN GS-ATUALIZA-CTA-REC-TRUE
                    PERFORM ATUALIZA-A-RECEBER
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
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

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
           MOVE "RCAIXA"               TO RETPORT-PROGRAMA
           READ RETPORT INVALID KEY
                INITIALIZE REG-RETPORT.

           MOVE RETPORT-PORTADOR       TO GS-ACP-PORTADOR
           READ CAD018 INVALID KEY
                MOVE "***********"     TO NOME-PORT
           END-READ
           MOVE NOME-PORT              TO GS-DESC-PORTADOR.

       GRAVAR-PORTADOR SECTION.
           MOVE "RCAIXA"               TO RETPORT-PROGRAMA
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

              MOVE ZEROS TO LIN
                            VALOR-TOTAL
                            ACRESCIMO-TOTAL
                            QTDE-PARC
              PERFORM UNTIL ST-RET = "10"
                  READ RETORNO AT END
                       MOVE "10" TO ST-RET
                  NOT AT END
                       IF REG-RETORNO(1:8) = "10499999"
                          MOVE "10" TO ST-RET
                       ELSE
                       MOVE SPACES             TO GS-LINDET1
                       MOVE REG-RETORNO(9:5)   TO GS-EXIBE-CODIGO
                       MOVE "REFRESH-DATA"     TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                       EVALUATE REG-RETORNO(14:1)
                          WHEN "T" MOVE REG-RETORNO     TO REGISTRO-T
                                   MOVE RT-NOSSO-NUMERO TO
                                        GS-LINDET(27:12)
                                   MOVE RT-NUMERO-DOCTO(1:1) TO
                                        GS-LINDET(1:1)
                                   MOVE RT-NUMERO-DOCTO(2:8) TO
                                        GS-LINDET(3:8)
                                   MOVE RT-NUMERO-DOCTO TO
                                        GS-LINDET(13:16)
                                   STRING RT-VENCTO(1:2) "/"
                                          RT-VENCTO(3:2) "/"
                                          RT-VENCTO(5:4)
                                     INTO GS-LINDET(81:11)
                                   MOVE RT-VLRTITULO TO VALOR-W
                                   MOVE VALOR-W      TO GS-LINDET(40:15)
                                   MOVE RT-OCORRENCIA TO
                                        GS-LINDET(110:10)

                          WHEN "U" MOVE REG-RETORNO TO REGISTRO-U
                                   IF RU-DATA-CREDITO > 0
                                      STRING RU-DATA-CREDITO(1:2) "/"
                                             RU-DATA-CREDITO(3:2) "/"
                                             RU-DATA-CREDITO(5:4)
                                        INTO GS-LINDET(55:11)
                                   ELSE
                                      MOVE SPACES TO GS-LINDET(55:11)
                                   END-IF

                                   IF RU-COD-MOV = 06
                                      PERFORM CALCULA-DIFERENCA
                                      ADD RT-VLRTITULO TO VALOR-TOTAL
                                      ADD 1            TO QTDE-PARC

                                      MOVE VALOR-E1 TO GS-LINDET(92:15)
                                      ADD  VALOR-W1 TO ACRESCIMO-TOTAL

                                      MOVE RU-VALOR-PAGO TO VALOR-W
                                      MOVE VALOR-W  TO GS-LINDET(66:15)
                                   END-IF

                                   MOVE RU-COD-MOV TO GS-LINDET(107:3)

                                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                                   PERFORM CALL-DIALOG-SYSTEM
                                   INITIALIZE GS-LINDET
                       END-EVALUATE
                       END-IF
                  END-READ
              END-PERFORM
              MOVE "Valor Total: "  TO GS-LINTOT(1: 13)
              MOVE VALOR-TOTAL      TO VALOR-E
              MOVE VALOR-E          TO GS-LINTOT(14: 14)
              MOVE "Qtde Parc: "    TO GS-LINTOT(35: 11)
              MOVE QTDE-PARC        TO GS-LINTOT(46: 05)
              MOVE "Acresc.Total: " TO GS-LINTOT(60: 14)
              MOVE ACRESCIMO-TOTAL  TO VALOR-E1
              MOVE VALOR-E1         TO GS-LINTOT(74: 13)
              MOVE "REFRESH-DATA" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
           END-IF
           CLOSE      RETORNO
           OPEN INPUT RETORNO.
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
                    IF REG-RETORNO(1:8) = "10499999"
                       MOVE "10" TO ST-RET
                    ELSE
                     EVALUATE REG-RETORNO(14:1)
                          WHEN "T" MOVE REG-RETORNO     TO REGISTRO-T
                                   MOVE RT-NOSSO-NUMERO TO
                                        NOSSO-NR-REL
                                   MOVE RT-NUMERO-DOCTO(1:1) TO
                                        CLASS-REL
                                   MOVE RT-NUMERO-DOCTO(2:8) TO
                                        CONTALB-REL
                                   MOVE RT-NUMERO-DOCTO TO
                                        NR-TITULO-REL
                                   STRING RT-VENCTO(1:2) "/"
                                          RT-VENCTO(3:2) "/"
                                          RT-VENCTO(7:2)
                                     INTO DATA-VENCTO-REL
                                   MOVE RT-VLRTITULO TO VALOR-W
                                   MOVE VALOR-W     TO VALOR-REC-TIT-REL
                                   MOVE RT-OCORRENCIA TO
                                        REJEICAO-REL

                          WHEN "U" MOVE REG-RETORNO TO REGISTRO-U
                                   IF RU-DATA-CREDITO > 0
                                      STRING RU-DATA-CREDITO(1:2) "/"
                                             RU-DATA-CREDITO(3:2) "/"
                                             RU-DATA-CREDITO(7:2)
                                        INTO DATA-PGTO-REL
                                   ELSE
                                      MOVE ZEROS  TO DATA-PGTO-REL
                                   END-IF

                                   IF RU-COD-MOV = 06
                                      ADD RT-VLRTITULO TO VALOR-TOTAL
                                      ADD 1            TO QTDE-PARC
                                      PERFORM CALCULA-DIFERENCA
                                      ADD  VALOR-W1 TO ACRESCIMO-TOTAL

                                      MOVE VALOR-E1 TO ACRESCIMO-REL

                                      MOVE RU-VALOR-PAGO TO VALOR-W
                                      MOVE VALOR-W  TO GS-LINDET(66:15)
                                   END-IF

                                   MOVE RU-COD-MOV TO OCORRENCIA-REL

                                   WRITE REG-RELAT FROM LINDET
                                   END-WRITE
                                   ADD 1 TO LIN
                                   IF LIN > 56
                                      PERFORM CABECALHO
                                   END-IF
                                   INITIALIZE LINDET
                       END-EVALUATE
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

           CLOSE      RETORNO
           OPEN INPUT RETORNO
           MOVE ZEROS TO ST-RET
           PERFORM UNTIL ST-RET = "10"
               READ RETORNO AT END
                    MOVE "10" TO ST-RET
               NOT AT END
                    IF REG-RETORNO(1:8) = "10499999"
                       MOVE "10" TO ST-RET
                    ELSE
                       MOVE REG-RETORNO(9:5)   TO GS-EXIBE-CODIGO
                       MOVE "REFRESH-DISPLAY"  TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                       EVALUATE REG-RETORNO(14:1)
                          WHEN "T" MOVE REG-RETORNO TO REGISTRO-T
                          WHEN "U" MOVE REG-RETORNO TO REGISTRO-U
                                   IF RT-COD-MOV <> 6 AND 3 AND 2
                                      PERFORM TITULOS-NAO-ENCONTRADO
                                   ELSE
                                      PERFORM PROCURA-TITULO
                                   END-IF
                       END-EVALUATE
                    END-IF
               END-READ
           END-PERFORM
           CLOSE      RETORNO CRD020 CRD020B
           OPEN INPUT RETORNO CRD020 CRD020B

           MOVE "UNSHOW-WIN3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       GRAVA-NR-BANCO SECTION.
           MOVE RT-NOSSO-NUMERO TO OUTRO-DOCTO-CR20
           REWRITE REG-CRD020 INVALID KEY
                PERFORM TITULOS-NAO-ENCONTRADO
           END-REWRITE.
       ENTRADA-REJEITADA SECTION.
           MOVE PORTADOR-PAR002-R TO PORTADOR-CR20
      *    MOVE 01 TO PORTADOR-CR20.
           REWRITE REG-CRD020 INVALID KEY
                PERFORM TITULOS-NAO-ENCONTRADO
           END-REWRITE.
       BAIXAR-TITULO SECTION.
           MOVE RU-VALOR-PAGO           TO VALOR-W
           MOVE VALOR-W                 TO VALOR-LIQ-CR20
           IF VALOR-LIQ-CR20 <> VALOR-TOT-CR20
              COMPUTE DIFERENCA-W1 = VALOR-LIQ-CR20 - VALOR-TOT-CR20
              IF DIFERENCA-W1 > 0
                 MOVE DIFERENCA-W1      TO JURO-RCTO-CR20
              ELSE
                 MOVE DIFERENCA-W1      TO DESCONTO-CR20
              END-IF
           END-IF
           MOVE RU-DATA-CREDITO(1:2)    TO DATA-RCTO-CR20(7:2)
           MOVE RU-DATA-CREDITO(3:2)    TO DATA-RCTO-CR20(5:2)
           MOVE RU-DATA-CREDITO(5:4)    TO DATA-RCTO-CR20(1:4)
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
           END-REWRITE.
       PROCURA-TITULO SECTION.
      *    procura o título através da chave vencto, docto e valor
           MOVE RT-VENCTO(1:2)  TO DATA-VENCTO(7:2)
           MOVE RT-VENCTO(3:2)  TO DATA-VENCTO(5:2)
           MOVE RT-VENCTO(5:4)  TO DATA-VENCTO(1:4)

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
                        IF RT-NOSSO-NUMERO = OUTRO-DOCTO-CR20
                           IF VALOR-TOT-CR20 = RT-VLRTITULO
                              EVALUATE RU-COD-MOV
                                WHEN 02 PERFORM GRAVA-NR-BANCO
                                WHEN 03 PERFORM ENTRADA-REJEITADA
                                WHEN 06 PERFORM BAIXAR-TITULO
                              END-EVALUATE
                              MOVE 1 TO NAO-ENCONTRADO
                              MOVE "10" TO ST-CRD020
                           END-IF
                        ELSE
                           IF COD-COMPL-CR20 <> RT-NUMERO-DOCTO(1:9)
                              CONTINUE
                           ELSE
                              IF VALOR-TOT-CR20 <> RT-VLRTITULO
                                 CONTINUE
                              ELSE
                                 EVALUATE RU-COD-MOV
                                   WHEN 02 PERFORM GRAVA-NR-BANCO
                                   WHEN 03 PERFORM ENTRADA-REJEITADA
                                   WHEN 06 PERFORM BAIXAR-TITULO
                                 END-EVALUATE
                                 MOVE 1 TO NAO-ENCONTRADO
                                 MOVE "10" TO ST-CRD020
                              END-IF
                           END-IF
                        END-IF
                     END-IF
                 END-READ
           END-PERFORM.
           IF NAO-ENCONTRADO = 0
              PERFORM TITULOS-NAO-ENCONTRADO.

       TITULOS-NAO-ENCONTRADO SECTION.
           MOVE REGISTRO-T         TO REG-PROBLEMA
           WRITE REG-PROBLEMA
           MOVE REGISTRO-U         TO REG-PROBLEMA
           WRITE REG-PROBLEMA.
       ERRO-GRAVACAO-CRD020 SECTION.
           MOVE "ERRO GRAVAÇÃO CRD020" TO GS-MENSAGEM-ERRO
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CALCULA-DIFERENCA SECTION.
           COMPUTE DIFERENCA-W1 = RU-VALOR-PAGO - RT-VLRTITULO
           MOVE DIFERENCA-W1             TO VALOR-W1
           MOVE VALOR-W1                 TO VALOR-E1.
      *----------------------------------------------------------
      * Títulos retornados do banco, não encontrados no arquivo CRD020
       LISTA-NAO-ENCONTRADOS SECTION.
           CLOSE      PROBLEMA
           OPEN INPUT PROBLEMA

           MOVE "CLEAR-LIST-BOX2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "REFRESH-LIST-BOX2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE VALOR-TOTAL
                      ACRESCIMO-TOTAL
                      QTDE-PARC

           PERFORM UNTIL ST-PROBLEMA = "10"
                READ PROBLEMA AT END
                     MOVE "10" TO ST-PROBLEMA
                NOT AT END
                     EVALUATE REG-PROBLEMA(14:1)
                         WHEN "T" MOVE REG-PROBLEMA TO REGISTRO-T
                                  MOVE RT-NUMERO-DOCTO(1:1) TO
                                       GS-LINDET1(1:1)
                                  MOVE RT-NUMERO-DOCTO(2:8) TO
                                       GS-LINDET1(3:10)
                                  MOVE RT-NUMERO-DOCTO      TO
                                       GS-LINDET1(13:11)
                                  MOVE RT-NOSSO-NUMERO      TO
                                       GS-LINDET1(24:16)
                                  MOVE RT-VLRTITULO         TO
                                       VALOR-W
                                  MOVE VALOR-W      TO GS-LINDET1(40:15)
                                  STRING RT-VENCTO(1:2) "/"
                                         RT-VENCTO(3:2) "/"
                                         RT-VENCTO(5:4)
                                    INTO GS-LINDET1(66:11)
                         WHEN "U" MOVE REG-PROBLEMA TO REGISTRO-U
                                  IF RU-COD-MOV = 06
                                     ADD RT-VLRTITULO    TO VALOR-TOTAL
                                     ADD 1               TO QTDE-PARC
                                     PERFORM CALCULA-DIFERENCA
                                     MOVE VALOR-E1 TO GS-LINDET1(77:15)
                                     ADD  VALOR-W1 TO ACRESCIMO-TOTAL
                                  END-IF
                                  IF RU-DATA-CREDITO > 0
                                     STRING RU-DATA-CREDITO(1:2) "/"
                                            RU-DATA-CREDITO(3:2) "/"
                                            RU-DATA-CREDITO(5:4)
                                       INTO GS-LINDET1(55:11)
                                  ELSE
                                     MOVE SPACES TO GS-LINDET1(55:11)
                                  END-IF
                                  MOVE RU-COD-MOV         TO
                                       GS-LINDET1(92:3)
                                  MOVE RT-OCORRENCIA(1:2) TO
                                       GS-LINDET1(95:2)
                                  MOVE "INSERE-LIST2"  TO DS-PROCEDURE
                                  PERFORM CALL-DIALOG-SYSTEM

                                  INITIALIZE GS-LINDET1
                     END-EVALUATE
                END-READ
           END-PERFORM.
      *----------------------------------------------------------
       IMPRIME-NAO-ENCONTRADO SECTION.
           MOVE ZEROS TO LIN.

           COPY CONDENSA.

           CLOSE      PROBLEMA
           OPEN INPUT PROBLEMA
           PERFORM CABECALHO1
           PERFORM UNTIL ST-PROBLEMA = "10"
                 READ PROBLEMA AT END
                      MOVE "10" TO ST-PROBLEMA
                 NOT AT END
                      EVALUATE REG-PROBLEMA(14:1)
                           WHEN "T" MOVE REG-PROBLEMA TO REGISTRO-T
                                    MOVE RT-NUMERO-DOCTO(1:1) TO
                                         CLASS-REL
                                    MOVE RT-NUMERO-DOCTO(2:8) TO
                                         CONTALB-REL
                                    MOVE RT-NUMERO-DOCTO      TO
                                         NR-TITULO-REL
                                    MOVE RT-NOSSO-NUMERO      TO
                                         NOSSO-NR-REL
                                    MOVE RT-VLRTITULO         TO
                                         VALOR-W
                                    MOVE VALOR-W              TO
                                         VALOR-REC-TIT-REL
                                    STRING RT-VENCTO(1:2) "/"
                                           RT-VENCTO(3:2) "/"
                                           RT-VENCTO(5:4)
                                      INTO DATA-VENCTO-REL
                           WHEN "U" MOVE REG-PROBLEMA TO REGISTRO-U
                                    IF RU-DATA-CREDITO > 0
                                       STRING RU-DATA-CREDITO(1:2) "/"
                                              RU-DATA-CREDITO(3:2) "/"
                                              RU-DATA-CREDITO(5:4)
                                         INTO DATA-PGTO-REL
                                    ELSE
                                       MOVE SPACES TO DATA-PGTO-REL
                                    END-IF
                                    IF RU-COD-MOV = 06
                                       ADD 1 TO QTDE-PARC
                                       ADD RT-VLRTITULO TO VALOR-TOTAL
                                       PERFORM CALCULA-DIFERENCA
                                       MOVE VALOR-E1 TO ACRESCIMO-REL
                                       ADD  VALOR-W1 TO ACRESCIMO-TOTAL
                                    END-IF
                                    MOVE RU-COD-MOV TO OCORRENCIA-REL
                                    MOVE RT-OCORRENCIA TO REJEICAO-REL
                                    WRITE REG-RELAT FROM LINDET
                                    END-WRITE
                                    ADD 1 TO LIN
                                    IF LIN > 56
                                       PERFORM CABECALHO1
                                    END-IF
                                    INITIALIZE LINDET
                      END-EVALUATE
                 END-READ
           END-PERFORM

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
           MOVE "RCAIXA" TO DS-SET-NAME
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
           move "RCAIXA"            to logacess-programa
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

           CLOSE CRD020 CRD020B RETORNO PROBLEMA CAD018 RETPORT PAR002
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

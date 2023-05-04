       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP9112.
       AUTHOR.        ALFREDO SAVIOLLI NETO
      *EMISSÃO DE RELATÓRIO DE RETORNO DO BANCO DO BRASIL
      *CONFORME O TIPO DE OCORRENCIA PELO BANCO O SISTEMA FARÁ:
      *02 - ACEITO PELO BANCO - GRAVA O NR-BANCO NO OUTRO-DOCTO-CR20
      *03 - REJEITADO PELO BANCO - MUDA O PORTADOR DO ARQUIVO CRD020
      *06 - BAIXA DE TÍTULO - FAZ A BAIXA DO TÍTULO NO CRD020
      *NO MOMENTO DA ATUALIZACAO DO CRD020 O TÍTULO NÃO FOR ENCONTRADO
      *O SISTEMA VAI GERAR O ARQUIVO PROBLEMA, QUE TEM POR OBJETIVO
      *LISTAR OS PROBLEMAS P/ QUE O USUÁRIO POSSA ACERTÁ-LO MANUALMENTE.
       DATE-WRITTEN.  15/04/1999.
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
           SELECT RETORNO ASSIGN TO "RETORNO"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  STATUS IS ST-RET.
           SELECT PROBLEMA ASSIGN TO "PROBLEMA"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  STATUS IS ST-PROBLEMA.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CRPW020.
       COPY PARW002.
       COPY CRPW020B.
       COPY LOGACESS.FD.
       FD  RETORNO.
       01  REG-RETORNO.
           05  COMECO-RET       PIC X(13).
           05  VALIDA-RET       PIC X(01).
           05  RESTANTE-RET     PIC X(226).

       FD  PROBLEMA.
       01  REG-PROBLEMA.
           05  DADOS-PRO-T      PIC X(240).
           05  DADOS-PRO-U      PIC X(240).
           05  SEQ-PRO          PIC 9(6).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(140).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP9112.CPB".
           COPY "CRP9112.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  AREAS-DE-TRABALHO.
           05 ERRO-1              PIC XX     VALUE SPACES.
           05 ST-CAD001           PIC XX     VALUE SPACES.
           05 ST-PAR002           PIC XX     VALUE SPACES.
           05 ST-CRD020           PIC XX     VALUE SPACES.
           05 ST-CRD020B          PIC XX     VALUE SPACES.
           05 ST-RET              PIC XX     VALUE SPACES.
           05 ST-PROBLEMA         PIC XX     VALUE SPACES.
           05 FS-LOGACESS         PIC XX     VALUE SPACES.
           05 OPCAO               PIC 9      VALUE ZEROS.
           05 VALOR-W             PIC 9(13)V99 VALUE ZEROS.
           05 VALOR-W1            PIC S9(13)V99 VALUE ZEROS.
           05 DIFERENCA-W         PIC S9(13) VALUE ZEROS.
           05 DIFERENCA-W1        PIC S9(13)V99 VALUE ZEROS.
           05 VALOR-TOTAL         PIC 9(13)V99 VALUE ZEROS.
           05 ACRESCIMO-TOTAL     PIC S9(13)V99 VALUE ZEROS.
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
           05 LIN                 PIC 9(02)  VALUE ZEROS.
           05 ERRO-W              PIC 9      VALUE ZEROS.
           05 QTDE-PARC           PIC 9(4)        VALUE ZEROS.
           05 SEQ-RET             PIC 9(06).
           05 DESC-REJEI          PIC X(30)  VALUE SPACES.
           05 REJEICAO            PIC 9(02)  VALUE ZEROS.
           05 COD-COMPL-CR20-W    PIC 9(09)  VALUE ZEROS.
           05 ACHEI               PIC X(01)  VALUE SPACES.
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
           05 RET-SEGMENTO-T.
              10  COD-BANCO-COMP-T    PIC 9(03).
              10  LOTE-SERVICO-T      PIC 9(04).
              10  REGISTRO-DETALHE-T  PIC X(01).
              10  SEQUENCIAL-LOTE-T   PIC 9(05).
              10  COD-SEGMENTO-T      PIC X(01).
              10  FEBRABAN-T          PIC X(01).
              10  CODIGO-MOVIMENTO-T  PIC 9(02).
              10  AGENC-MANT-CONT-T   PIC 9(05).
              10  DIGITO-AGENC-T      PIC 9(01).
              10  CONTA-CORRENTE-T    PIC 9(12).
              10  DIGITO-CONTA-T      PIC 9(01).
              10  DIGITO-CONTA-AG-T   PIC 9(01).
              10  IDENTIF-TIT-BANCO-T PIC X(20).
              10  CODIGO-CARTEIRA-T   PIC 9(01).
              10  NUMERO-DOC-COB-T    PIC X(15).
              10  DATA-VENCTO-TIT-T   PIC 9(08).
              10  VALOR-NOMINAL-T     PIC 9(13)V99.
              10  NUMERO-BANCO-T      PIC 9(03).
              10  AGENCIA-COB-REC-T   PIC 9(05).
              10  DIGITO-V-AGENC-T    PIC 9(01).
              10  IDENTIF-TIT-EMP-T   PIC X(25).
              10  CODIGO-MOEDA-T      PIC 9(02).
              10  TIPO-INSCRICAO-T    PIC 9(01).
              10  NUMERO-INSCRICAO-T  PIC 9(15).
              10  NOME-T              PIC X(40).
              10  N-CONTROL-OPE-T     PIC 9(10).
              10  VALOR-TARIFA-T      PIC 9(13)V99.
              10  IDENTIF-REJEICAO-T  PIC 9(02).
              10  SOBRA-T             PIC 9(08).
              10  USO-FEBRABAN-T      PIC 9(17).
           05 RET-SEGMENTO-U.
              10  COD-BANCO-COMP-U    PIC 9(03).
              10  LOTE-SERVICO-U      PIC 9(04).
              10  REGISTRO-DETALHE-U  PIC X(01).
              10  SEQUENCIAL-LOTE-U   PIC 9(05).
              10  COD-SEGMENTO-U      PIC X(01).
              10  FEBRABAN-U          PIC X(01).
              10  CODIGO-MOVIMENTO-U  PIC 9(02).
              10  JUROS-MULTAS-U      PIC 9(13)V99.
              10  VALOR-DESCONTO-U    PIC 9(13)V99.
              10  VALOR-ABATIMENTO-U  PIC 9(13)V99.
              10  VALOR-IOF-U         PIC 9(13)V99.
              10  VALOR-PAGO-U        PIC 9(13)V99.
              10  VALOR-LIQUIDO-U     PIC 9(13)V99.
              10  VALOR-OUTRAS-DESP-U PIC 9(13)V99.
              10  VALOR-OUTROS-CRED-U PIC 9(13)V99.
              10  DATA-OCORRENCIA-U   PIC 9(08).
              10  DATA-EFETIVACAO-U   PIC 9(08).
              10  COD-OCORRENCIA-U    PIC X(04).
              10  DATA-OCORR-SACADO-U PIC 9(08).
              10  VALOR-OCORR-SACA-U  PIC 9(13)V99.
              10  COMPL-OCORR-SACA-U  PIC X(30).
              10  CODIGO-BCO-COMP-U   PIC 9(03).
              10  NOSSO-NUM-BCO-U     PIC X(20).
              10  FEBRA-U             PIC X(07).
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05 DATA-AUX.
              10 AUX-ANO            PIC 9(04).
              10 AUX-MES            PIC 9(02).
              10 AUX-DIA            PIC 9(02).
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  FILLER               PIC X(85)  VALUE
               'RELATORIO DE RETORNO - BANCO DO BRASIL  '.
           05  FILLER               PIC X(07)  VALUE 'EMIS.:'.
           05  EMISSAO-REL          PIC 99/99/99.

       01  CAB01A.
           05  FILLER               PIC X(85)  VALUE
               'RELATORIO DE PROBLEMAS NA ATUALIZACAO-BANCO DO BRASIL'.
           05  FILLER               PIC X(07)  VALUE 'EMIS.:'.
           05  EMISSAO-REL1         PIC 99/99/99.
       01  CAB02.
           05  FILLER               PIC X(100) VALUE ALL '='.

       01  CAB04.
           05  FILLER               PIC X(3)   VALUE "C".
           05  FILLER               PIC X(11)  VALUE 'CONT-ALB'.
           05  FILLER               PIC X(11)  VALUE 'NR-TITULO'.
           05  FILLER               PIC X(23)  VALUE 'NOSSO-NR'.
           05  FILLER               PIC X(18)  VALUE
                                      '    VALOR-REC-TIT'.
           05  FILLER               PIC X(10)  VALUE ' DTA-PGTO'.
           05  FILLER               PIC X(10)  VALUE "  DTA-VCTO".
           05  FILLER               PIC X(15)  VALUE '   VLR-ACRESC'.
           05  FILLER               PIC X(3)   VALUE "OC".
           05  FILLER               PIC X(2)   VALUE "RJ".
       01  LINDET.
           05  CLASS-REL            PIC X      VALUE SPACES.
           05  FILLER               PIC XX     VALUE SPACES.
           05  CONTALB-REL          PIC X(9)   VALUE SPACES.
           05  FILLER               PIC XX     VALUE SPACES.
           05  NR-TITULO-REL        PIC X(10)  VALUE SPACES.
           05  FILLER               PIC X      VALUE SPACES.
           05  PARCE-REL            PIC Z9/    VALUE ZEROS.
           05  QTDE-REL             PIC Z9     VALUE ZEROS.
           05  FILLER               PIC X      VALUE SPACES.
           05  NOSSO-NR-REL         PIC X(21)  VALUE SPACES.
           05  FILLER               PIC XX     VALUE SPACES.
           05  VALOR-REC-TIT-REL    PIC ZZ.ZZZ.ZZZ.ZZZ,ZZ.
           05  FILLER               PIC X      VALUE SPACES.
           05  DATA-PGTO-REL        PIC X(10).
           05  FILLER               PIC XX     VALUE SPACES.
           05  DATA-VENCTO-REL      PIC X(10).
           05  FILLER               PIC XX     VALUE SPACES.
           05  ACRESCIMO-REL        PIC ZZZ.ZZZ,ZZ.
           05  FILLER               PIC X      VALUE SPACES.
           05  OCORRENCIA-REL       PIC 99     VALUE ZEROS.
           05  FILLER               PIC X      VALUE SPACES.
           05  REJEICAO-REL         PIC 99     VALUE ZEROS.
           05  FILLER               PIC X(01).
           05  DESC-REJEICAO-REL    PIC X(30)  VALUE SPACES.
       01  CAB03.
           05  FILLER               PIC X(100) VALUE ALL '-'.
       01  LINTOT.
           05  FILLER               PIC X(18)  VALUE SPACES.
           05  FILLER               PIC X(09)  VALUE 'QT.PARC: '.
           05  QTDE-PARC-TOT        PIC ZZZZ.
           05  FILLER               PIC X(5)   VALUE SPACES.
           05  VALOR-REC-TIT-TOT    PIC ZZ.ZZZ.ZZZ.ZZZ,ZZ.
           05  FILLER               PIC X(10)  VALUE SPACES.
           05  ACRESCIMO-TOT        PIC ZZ.ZZZ.ZZZ.ZZZ,ZZ.

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
           MOVE DIA-I TO DIA-W.  MOVE MES-I TO MES-W.
           MOVE ANO-I TO ANO-W.  MOVE DATA-W TO EMISSAO-REL
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CRD020"   TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020
           MOVE "PAR002"   TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PAR002
           MOVE "CRD020B"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020B
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN I-O    CRD020 CRD020B PAR002
           CLOSE       CRD020 CRD020B PAR002
           OPEN INPUT  CRD020 CRD020B PAR002

           OPEN OUTPUT PROBLEMA
           OPEN INPUT  RETORNO

           IF ST-RET <> "00"
              MOVE "ERRO ABERTURA RETORNO" TO GS-MENSAGEM-ERRO
              MOVE ST-RET TO GS-MENSAGEM-ERRO(23: 2)
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
              MOVE ST-PAR002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE "CRP9112" TO PROGRAMA-PAR002
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
           move "CRP9112"           to logacess-programa
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
           MOVE 1 TO ERRO-W GS-EXIT-FLG.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE ZEROS TO LIN VALOR-TOTAL ACRESCIMO-TOTAL QTDE-PARC.
           PERFORM UNTIL ST-RET = "10"
            READ RETORNO AT END
                 MOVE "10" TO ST-RET
            NOT AT END
                 MOVE COMECO-RET  TO GS-EXIBE-CODIGO
                 MOVE "REFRESH-DATA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 EVALUATE VALIDA-RET
                     WHEN "T" MOVE REG-RETORNO TO RET-SEGMENTO-T

                              MOVE "0"           TO GS-LINDET(1: 2)
                              MOVE IDENTIF-TIT-EMP-T(2: 9)
                                                 TO GS-LINDET(03:10)
                              MOVE NUMERO-DOC-COB-T TO
                                                    GS-LINDET(13:11)

                              MOVE "0"                      TO
                                   CLASS-CLIENTE-CR20
                              MOVE IDENTIF-TIT-EMP-T(2: 9)  TO
                                   CLIENTE-CR20
                              MOVE IDENTIF-TIT-EMP-T(11: 5) TO
                                   SEQ-CR20
                              READ CRD020 INVALID KEY
                                   MOVE ZEROS TO NR-PARC-CR20
                                                 TOT-PARC-CR20
                              END-READ

                              STRING NR-PARC-CR20 "/" TOT-PARC-CR20
                              INTO GS-LINDET(24:6)

                              MOVE IDENTIF-TIT-BANCO-T TO
                                                    GS-LINDET(30:22)
                              MOVE VALOR-NOMINAL-T   TO VALOR-E
                              MOVE VALOR-E       TO GS-LINDET(52: 15)

                              IF CODIGO-MOVIMENTO-T = 06
                                 ADD VALOR-NOMINAL-T TO VALOR-TOTAL
                                 ADD 1       TO QTDE-PARC
                              END-IF

                              MOVE DATA-VENCTO-TIT-T   TO DATA-E
                              MOVE DATA-E           TO GS-LINDET(78: 11)


                              IF IDENTIF-REJEICAO-T NOT NUMERIC
                                 MOVE ZEROS TO IDENTIF-REJEICAO-T
                              END-IF
                              MOVE IDENTIF-REJEICAO-T TO
                                                        GS-LINDET(107:2)


                              MOVE IDENTIF-REJEICAO-T TO REJEICAO
                              MOVE SPACES     TO DESC-REJEI
                              EVALUATE REJEICAO
                                 WHEN 01 MOVE "LIQ. Por Saldo"
                                         TO DESC-REJEI
                                 WHEN 02 MOVE "LIQ. Por Conta"
                                         TO DESC-REJEI
                                 WHEN 03 MOVE "LIQ. No Proprio banco"
                                         TO DESC-REJEI
                                 WHEN 04
                                      MOVE "LIQ. Compensacao eletronica"
                                         TO DESC-REJEI
                                 WHEN 05
                                    MOVE "LIQ. Compensacao convencional"
                                         TO DESC-REJEI
                                 WHEN 06 MOVE "LIQ. Por meio eletronico"
                                         TO DESC-REJEI
                                 WHEN 07 MOVE "LIQ. Apos feriado local"
                                         TO DESC-REJEI
                                 WHEN 08 MOVE "LIQ. Em cartorio"
                                         TO DESC-REJEI
                                 WHEN 09 MOVE "BXA. Comandada banco"
                                         TO DESC-REJEI
                                 WHEN 10
                                   MOVE "BXA. Comandada cliente arquivo"
                                         TO DESC-REJEI
                                 WHEN 11
                                   MOVE "BXA. Comandada cliente on-line"
                                         TO DESC-REJEI
                                 WHEN 12
                                   MOVE "BXA. Decurso prazo - cliente"
                                         TO DESC-REJEI
                                 WHEN 13
                                   MOVE "BXA. Decurso prazo - banco"
                                         TO DESC-REJEI
                              END-EVALUATE
                              MOVE DESC-REJEI TO GS-LINDET(110:30)
                     WHEN "U" MOVE REG-RETORNO TO RET-SEGMENTO-U
                              IF CODIGO-MOVIMENTO-T = 06
                                 PERFORM CALCULA-DIFERENCA
                                 MOVE VALOR-E1    TO GS-LINDET(90: 15)
                                 ADD VALOR-W1     TO ACRESCIMO-TOTAL
                              END-IF
                              MOVE DATA-EFETIVACAO-U TO DATA-E
                              MOVE DATA-E        TO GS-LINDET(67: 11)
                              MOVE CODIGO-MOVIMENTO-T TO
                                   GS-LINDET(104:3)
                              MOVE "INSERE-LIST" TO DS-PROCEDURE
                              PERFORM CALL-DIALOG-SYSTEM
                              MOVE SPACES TO GS-LINDET
                 END-EVALUATE
            END-READ
           END-PERFORM.
           MOVE "Valor Total: "  TO GS-LINTOT(1: 13)
           MOVE VALOR-TOTAL      TO VALOR-E
           MOVE VALOR-E          TO GS-LINTOT(14: 14)
           MOVE "Qtde Parc: "    TO GS-LINTOT(35: 11)
           MOVE QTDE-PARC        TO GS-LINTOT(46: 05)
           MOVE "Acresc.Total: " TO GS-LINTOT(60: 14)
           MOVE ACRESCIMO-TOTAL  TO VALOR-E
           MOVE VALOR-E          TO GS-LINTOT(74: 15)
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           CLOSE RETORNO.  OPEN INPUT RETORNO.
       CALCULA-DIFERENCA SECTION.
           COMPUTE DIFERENCA-W1 = VALOR-PAGO-U - VALOR-NOMINAL-T.
           MOVE DIFERENCA-W1             TO VALOR-W1
           MOVE VALOR-W1                 TO VALOR-E1.
      *--------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO LIN
                         VALOR-TOTAL
                         QTDE-PARC

           COPY CONDENSA.

           PERFORM CABECALHO.
           PERFORM UNTIL ST-RET = "10"
                READ RETORNO AT END
                     MOVE "10" TO ST-RET
                NOT AT END
                     EVALUATE VALIDA-RET
                         WHEN "T" MOVE REG-RETORNO TO RET-SEGMENTO-T
                                  MOVE "0"           TO CLASS-REL
                                  MOVE IDENTIF-TIT-EMP-T(2: 9)
                                                        TO CONTALB-REL
                                  MOVE NUMERO-DOC-COB-T TO
                                                           NR-TITULO-REL
                                  MOVE IDENTIF-TIT-BANCO-T TO
                                                           NOSSO-NR-REL

                                  MOVE VALOR-NOMINAL-T   TO VALOR-E
                                  MOVE VALOR-E      TO VALOR-REC-TIT-REL

                              IF CODIGO-MOVIMENTO-T = 06
                                 ADD VALOR-NOMINAL-T TO VALOR-TOTAL
                                 ADD 1       TO QTDE-PARC
                              END-IF

                              MOVE DATA-VENCTO-TIT-T  TO DATA-E
                              MOVE DATA-E             TO DATA-VENCTO-REL
                              IF IDENTIF-REJEICAO-T NOT NUMERIC
                                 MOVE ZEROS TO IDENTIF-REJEICAO-T
                              END-IF
                              MOVE IDENTIF-REJEICAO-T TO REJEICAO-REL

                         WHEN "U" MOVE REG-RETORNO TO RET-SEGMENTO-U
                                  IF CODIGO-MOVIMENTO-T = 06
                                     PERFORM CALCULA-DIFERENCA
                                     MOVE VALOR-E1  TO ACRESCIMO-REL
                                  END-IF

                                  MOVE DATA-EFETIVACAO-U TO DATA-E
                                  MOVE DATA-E        TO DATA-PGTO-REL
                                  MOVE CODIGO-MOVIMENTO-T
                                                     TO OCORRENCIA-REL
                                  WRITE REG-RELAT FROM LINDET
                                  END-WRITE
                                  ADD 1 TO LIN
                                  IF LIN > 56 PERFORM CABECALHO
                                  END-IF
                                  INITIALIZE LINDET
                     END-EVALUATE
                END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL.
       TOTALIZA-REL SECTION.
           WRITE REG-RELAT FROM CAB03.
           MOVE VALOR-TOTAL TO VALOR-REC-TIT-TOT.
           MOVE QTDE-PARC   TO QTDE-PARC-TOT.
           MOVE ACRESCIMO-TOTAL TO ACRESCIMO-TOT.
           WRITE REG-RELAT FROM LINTOT.

           COPY DESCONDENSA.

           CLOSE RETORNO.  OPEN INPUT RETORNO.
       CABECALHO SECTION.
           IF LIN = ZEROS
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
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
           CLOSE    CRD020 CRD020B
           OPEN I-O CRD020 CRD020B

           CLOSE RETORNO.  OPEN INPUT RETORNO.
           MOVE ZEROS TO ST-RET SEQ-RET
           PERFORM UNTIL ST-RET = "10"
               READ RETORNO AT END
                    MOVE "10" TO ST-RET
               NOT AT END
                    ADD 1 TO SEQ-RET
                    MOVE IDENTIF-TIT-EMP-T(1: 10) TO GS-EXIBE-CODIGO
                    MOVE "REFRESH-DISPLAY" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                    EVALUATE VALIDA-RET
                        WHEN "T" MOVE REG-RETORNO TO RET-SEGMENTO-T
                        WHEN "U" MOVE REG-RETORNO TO RET-SEGMENTO-U
                                 IF CODIGO-MOVIMENTO-U <> 06 AND 03 AND
                                                          02 AND 09
                                    PERFORM TITULOS-NAO-ENCONTRADO
                                 ELSE
                                    MOVE IDENTIF-TIT-EMP-T(1:1) TO LETRA
                                    IF LETRA <> "X"
                                       PERFORM PROCURA-TITULO
                                    ELSE
      *                                PERFORM PROCURA-TITULO2
                                       MOVE "0"                     TO
                                       CLASS-CLIENTE-CR20
                                       MOVE IDENTIF-TIT-EMP-T(2: 9) TO
                                                          CLIENTE-CR20
                                       MOVE IDENTIF-TIT-EMP-T(11: 5) TO
                                                          SEQ-CR20
                                       READ CRD020 INVALID KEY
                                          PERFORM TITULOS-NAO-ENCONTRADO
                                       NOT INVALID KEY
                                          IF SITUACAO-CR20 <> 00
                                          PERFORM TITULOS-NAO-ENCONTRADO
                                          ELSE
                                            EVALUATE CODIGO-MOVIMENTO-U
                                             WHEN 02
                                               PERFORM GRAVA-NR-BANCO
                                             WHEN 03
                                               PERFORM ENTRADA-REJEITADA
                                             WHEN 06
                                               PERFORM BAIXAR-TITULO
                                            END-EVALUATE
                                            MOVE IDENTIF-REJEICAO-T TO
                                            REJEICAO
                                            EVALUATE REJEICAO
                                               WHEN 09
                                               PERFORM
                                                       MOVER-PORTADOR-96
                                            END-EVALUATE
                                          END-IF
                                       END-READ
                                    END-IF
                                 END-IF
                    END-EVALUATE
               END-READ
           END-PERFORM
           CLOSE      RETORNO CRD020 CRD020B
           OPEN INPUT RETORNO CRD020 CRD020B.

           MOVE "UNSHOW-WIN3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       MOVER-PORTADOR-96 SECTION.
      *    MOVE 96                TO PORTADOR-CR20
           MOVE PORTADOR-PAR002-D TO PORTADOR-CR20
           REWRITE REG-CRD020 INVALID KEY
               PERFORM TITULOS-NAO-ENCONTRADO
           END-REWRITE.


       PROCURA-TITULO2 SECTION.
           STRING DATA-VENCTO-TIT-T(1:2) INTO AUX-DIA
           STRING DATA-VENCTO-TIT-T(3:4) INTO AUX-MES
           STRING DATA-VENCTO-TIT-T(5:8) INTO AUX-ANO
           MOVE DATA-AUX                 TO DATA-VENCTO

           MOVE "N" TO ACHEI
           INITIALIZE REG-CRD020
           MOVE "0"                      TO CLASS-CLIENTE-CR20
           MOVE IDENTIF-TIT-EMP-T(2: 9)  TO CLIENTE-CR20
           MOVE COD-COMPL-CR20           TO COD-COMPL-CR20-W

           START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
               MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
               READ CRD020 NEXT AT END
                   MOVE "10" TO ST-CRD020
               NOT AT END
                   IF COD-COMPL-CR20 <> COD-COMPL-CR20-W
                      MOVE "10" TO ST-CRD020
                   ELSE
                      IF DATA-VENCTO-CR20 = DATA-VENCTO
                         IF SITUACAO-CR20 = 0
                            MOVE "S" TO ACHEI
                            EVALUATE CODIGO-MOVIMENTO-U
                             WHEN 02 PERFORM GRAVA-NR-BANCO
                             WHEN 03 PERFORM ENTRADA-REJEITADA
                             WHEN 06 PERFORM BAIXAR-TITULO
                            END-EVALUATE
                         END-IF
                      END-IF
                   END-IF
               END-READ
           END-PERFORM
           IF ACHEI = "N"
              PERFORM TITULOS-NAO-ENCONTRADO.
       GRAVA-NR-BANCO SECTION.
           MOVE IDENTIF-TIT-BANCO-T TO OUTRO-DOCTO-CR20.
           REWRITE REG-CRD020 INVALID KEY
                PERFORM TITULOS-NAO-ENCONTRADO
           END-REWRITE.
       ENTRADA-REJEITADA SECTION.
      *    MOVE 01 TO PORTADOR-CR20.
           MOVE PORTADOR-PAR002-R TO PORTADOR-CR20
           REWRITE REG-CRD020 INVALID KEY
                PERFORM TITULOS-NAO-ENCONTRADO
           END-REWRITE.
       BAIXAR-TITULO SECTION.
           MOVE VALOR-PAGO-U             TO VALOR-LIQ-CR20
           IF VALOR-LIQ-CR20 <> VALOR-TOT-CR20
              COMPUTE DIFERENCA-W1 = VALOR-LIQ-CR20 - VALOR-TOT-CR20
              IF DIFERENCA-W1 > 0
                 MOVE DIFERENCA-W1 TO JURO-RCTO-CR20
              ELSE
                 MOVE DIFERENCA-W1 TO DESCONTO-CR20
              END-IF
           END-IF

           MOVE DATA-EFETIVACAO-U(1:2)   TO AUX-DIA
           MOVE DATA-EFETIVACAO-U(3:2)   TO AUX-MES
           MOVE DATA-EFETIVACAO-U(5:4)   TO AUX-ANO
           MOVE DATA-AUX                 TO DATA-RCTO-CR20
           MOVE "4-Receb.Bco."           TO FORMA-PAGTO-CR20

           MOVE 2                        TO SITUACAO-CR20
           MOVE ZEROS                    TO VALOR-SALDO-CR20
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
                MOVE FORMA-PAGTO-CR20   TO FORMA-PAGTO-CR20B
                MOVE DCR-MEM-CR20       TO DCR-MEM-CR20B
                WRITE REG-CRD020B
                END-WRITE
           END-REWRITE.
       PROCURA-TITULO SECTION.
      *    procura o título através da chave vencto, docto e valor

           STRING DATA-VENCTO-TIT-T(1:2) INTO AUX-DIA
           STRING DATA-VENCTO-TIT-T(3:4) INTO AUX-MES
           STRING DATA-VENCTO-TIT-T(5:8) INTO AUX-ANO

           MOVE DATA-AUX                 TO DATA-VENCTO-CR20
                                            DATA-VENCTO
           MOVE ZEROS                    TO SITUACAO-CR20
                                            COD-COMPL-CR20
           MOVE ZEROS                    TO NAO-ENCONTRADO


           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT RECORD AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      IF SITUACAO-CR20 <> 0 OR
                         DATA-VENCTO-CR20 <> DATA-VENCTO
                         MOVE "10" TO ST-CRD020
                      ELSE
                         IF NR-DOCTO-CR20 <> NUMERO-DOC-COB-T
                            CONTINUE
                         ELSE
                            MOVE VALOR-NOMINAL-T     TO VALOR-W
                            IF VALOR-TOT-CR20 <> VALOR-W
                               CONTINUE
                            ELSE
                               EVALUATE CODIGO-MOVIMENTO-T
                                  WHEN 02 PERFORM GRAVA-NR-BANCO
                                  WHEN 03 PERFORM ENTRADA-REJEITADA
                                  WHEN 06 PERFORM BAIXAR-TITULO
                               END-EVALUATE
                               MOVE 1    TO NAO-ENCONTRADO
                               MOVE "10" TO ST-CRD020
                           END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.
           IF NAO-ENCONTRADO = 0
              PERFORM TITULOS-NAO-ENCONTRADO.
       TITULOS-NAO-ENCONTRADO SECTION.
            MOVE RET-SEGMENTO-T TO DADOS-PRO-T
            MOVE RET-SEGMENTO-U TO DADOS-PRO-U
            MOVE SEQ-RET     TO SEQ-PRO
            WRITE REG-PROBLEMA.
       ERRO-GRAVACAO-CRD020 SECTION.
           MOVE "ERRO GRAVAÇÃO CRD020" TO GS-MENSAGEM-ERRO
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *----------------------------------------------------------
      * Títulos retornados do banco, não encontrados no arquivo CRD020
       LISTA-NAO-ENCONTRADOS SECTION.
           CLOSE PROBLEMA.  OPEN INPUT PROBLEMA.
           MOVE "CLEAR-LIST-BOX2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE "REFRESH-LIST-BOX2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL ST-PROBLEMA = "10"
               READ PROBLEMA AT END
                    MOVE "10" TO ST-PROBLEMA
               NOT AT END
                    MOVE DADOS-PRO-T TO RET-SEGMENTO-T
                    MOVE DADOS-PRO-U TO RET-SEGMENTO-U
                    INITIALIZE GS-LINDET1
                    MOVE IDENTIF-TIT-EMP-T(1:1) TO LETRA
                    IF LETRA = "X" OR "A"
                       MOVE IDENTIF-TIT-EMP-T(2: 1) TO GS-LINDET1(1: 2)
                       MOVE IDENTIF-TIT-EMP-T(3: 8) TO GS-LINDET1(3: 10)
                    ELSE
                       MOVE "0"                     TO GS-LINDET1(1: 2)
                       MOVE IDENTIF-TIT-EMP-T(2: 9) TO GS-LINDET1(3: 10)
                    END-IF
                    MOVE NUMERO-DOC-COB-T         TO GS-LINDET1(13:11)
                    MOVE IDENTIF-TIT-BANCO-T      TO GS-LINDET1(24: 22)
                    MOVE VALOR-NOMINAL-T          TO VALOR-E
                    MOVE VALOR-E                  TO GS-LINDET1(46: 15)
                    MOVE DATA-EFETIVACAO-U        TO DATA-E
                    MOVE DATA-E                   TO GS-LINDET1(61: 11)
                    MOVE DATA-VENCTO-TIT-T        TO DATA-E
                    MOVE DATA-E                   TO GS-LINDET1(72: 11)
                    IF CODIGO-MOVIMENTO-T = 06
                       PERFORM CALCULA-DIFERENCA
                       MOVE VALOR-E1              TO GS-LINDET1(73: 15)
                    END-IF
                    MOVE CODIGO-MOVIMENTO-T       TO GS-LINDET1(98: 3)
                    MOVE IDENTIF-REJEICAO-T       TO GS-LINDET1(101:2)
                    PERFORM VER-REJEICAO
                    MOVE DESC-REJEI               TO GS-LINDET1(104:30)
                    MOVE "INSERE-LIST2" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
            END-READ
           END-PERFORM.
      *-----------------------------------------------------------------
       VER-REJEICAO SECTION.
           MOVE IDENTIF-REJEICAO-T TO REJEICAO
           MOVE SPACES     TO DESC-REJEI
           EVALUATE REJEICAO
               WHEN 01 MOVE "LIQ. Por Saldo" TO DESC-REJEI
               WHEN 02 MOVE "LIQ. Por Conta" TO DESC-REJEI
               WHEN 03 MOVE "LIQ. No Proprio banco" TO DESC-REJEI
               WHEN 04 MOVE "LIQ. Compensacao eletronica" TO DESC-REJEI
               WHEN 05 MOVE "LIQ. Compensacao convencional"TO DESC-REJEI
               WHEN 06 MOVE "LIQ. Por meio eletronico" TO DESC-REJEI
               WHEN 07 MOVE "LIQ. Apos feriado local" TO DESC-REJEI
               WHEN 08 MOVE "LIQ. Em cartorio" TO DESC-REJEI
               WHEN 09 MOVE "BXA. Comandada banco" TO DESC-REJEI
               WHEN 10 MOVE "BXA. Comandada cliente arquivo" TO
                                                            DESC-REJEI
               WHEN 11 MOVE "BXA. Comandada cliente on-line" TO
                                                            DESC-REJEI
               WHEN 12 MOVE "BXA. Decurso prazo - cliente"   TO
                                                            DESC-REJEI
               WHEN 13 MOVE "BXA. Decurso prazo - banco"     TO
                                                           DESC-REJEI.

      *-----------------------------------------------------------------
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
                     MOVE DADOS-PRO-T TO RET-SEGMENTO-T
                     MOVE DADOS-PRO-U TO RET-SEGMENTO-U
                     INITIALIZE LINDET
                     MOVE IDENTIF-TIT-EMP-T(1:1) TO LETRA
                     IF LETRA = "X" OR "A"
                        MOVE IDENTIF-TIT-EMP-T(2: 1) TO CLASS-REL
                        MOVE IDENTIF-TIT-EMP-T(3: 8) TO CONTALB-REL
                     ELSE
                        MOVE "0"                     TO CLASS-REL
                        MOVE IDENTIF-TIT-EMP-T(1: 9) TO CONTALB-REL
                     END-IF

                     MOVE "0"                      TO
                          CLASS-CLIENTE-CR20
                     MOVE IDENTIF-TIT-EMP-T(2: 9)  TO
                          CLIENTE-CR20
                     MOVE IDENTIF-TIT-EMP-T(11: 5) TO
                          SEQ-CR20
                     READ CRD020 INVALID KEY
                          MOVE ZEROS TO NR-PARC-CR20
                                        TOT-PARC-CR20
                     END-READ

                     MOVE NR-PARC-CR20             TO PARCE-REL
                     MOVE TOT-PARC-CR20            TO QTDE-REL

                     MOVE NUMERO-DOC-COB-T         TO NR-TITULO-REL
                     MOVE IDENTIF-TIT-BANCO-T      TO NOSSO-NR-REL
                     MOVE VALOR-NOMINAL-T          TO VALOR-E
                     MOVE VALOR-E                  TO VALOR-REC-TIT-REL
                     MOVE DATA-EFETIVACAO-U        TO DATA-E
                     MOVE DATA-E                   TO DATA-PGTO-REL
                     MOVE DATA-VENCTO-TIT-T        TO DATA-E
                     MOVE DATA-E                   TO DATA-VENCTO-REL
                     IF CODIGO-MOVIMENTO-T = 06
                        PERFORM CALCULA-DIFERENCA
                        MOVE VALOR-E1              TO ACRESCIMO-REL
                     END-IF
                     MOVE CODIGO-MOVIMENTO-T       TO OCORRENCIA-REL
                     MOVE IDENTIF-REJEICAO-T       TO REJEICAO-REL
                     PERFORM VER-REJEICAO
                     MOVE DESC-REJEI               TO DESC-REJEICAO-REL
                     WRITE REG-RELAT FROM LINDET
                     END-WRITE
                     ADD 1 TO LIN
                     IF LIN > 56
                        PERFORM CABECALHO1
                     END-IF
                END-READ
           END-PERFORM.

           COPY DESCONDENSA.

       CABECALHO1 SECTION.
           IF LIN = ZEROS
              WRITE REG-RELAT FROM CAB01A
           ELSE WRITE REG-RELAT FROM CAB01A AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB02.
           MOVE 4 TO LIN.
      *----------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP9112" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

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
           move "CRP9112"           to logacess-programa
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

           CLOSE CRD020 CRD020B RETORNO PROBLEMA PAR002.
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP9116a.
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
           COPY CGPX010.
           COPY LOGACESS.SEL.
           SELECT RETORNO ASSIGN TO ARQUIVO-RETORNO
      *           "RETORNO.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  STATUS IS ST-RET.
           SELECT PROBLEMA ASSIGN TO "PROBLEMA"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  STATUS IS ST-PROBLEMA.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

           SELECT REJEICOES ASSIGN   TO ARQUIVO-REJEICOES
                            ORGANIZATION  IS      INDEXED
                            ACCESS MODE   IS      DYNAMIC
                            RECORD KEY    IS    REJ-CHAVE
                            LOCK   MODE   IS    AUTOMATIC
                            WITH   LOCK   ON       RECORD
                            FILE   STATUS IS FS-REJEICOES.

       DATA DIVISION.
       FILE SECTION.
       COPY CRPW020.
       COPY PARW002.
       COPY CRPW020B.
       COPY CGPW010.
       COPY LOGACESS.FD.
       FD  RETORNO.
       01  REG-RETORNO.
           05  COMECO-RET       PIC X(01).
           05  RESTANTE-RET     PIC X(399).

       FD  PROBLEMA.
       01  REG-PROBLEMA.
           05  DADOS-RETORNO    PIC X(399).
           05  SEQ-PRO          PIC 9(6).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(140).

       FD REJEICOES.
       01 REG-REJEICOES.
          05 REJ-CHAVE             PIC 9(02).
          05 REJ-DESCRICAO         PIC X(100).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP9116A.CPB".
           COPY "CRP9116A.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  AREAS-DE-TRABALHO.
           05 ERRO-1              PIC XX        VALUE SPACES.
           05 ST-CAD001           PIC XX        VALUE SPACES.
           05 ST-PAR002           PIC XX        VALUE SPACES.
           05 ST-CRD020           PIC XX        VALUE SPACES.
           05 ST-CGD010           PIC XX        VALUE SPACES.
           05 ST-CRD020B          PIC XX        VALUE SPACES.
           05 ST-RET              PIC XX        VALUE SPACES.
           05 ST-PROBLEMA         PIC XX        VALUE SPACES.
           05 FS-LOGACESS         PIC XX        VALUE SPACES.
           05 FS-REJEICOES        PIC XX        VALUE SPACES.
           05 OPCAO               PIC 9         VALUE ZEROS.
           05 VALOR-W             PIC 9(13)V99  VALUE ZEROS.
           05 VALOR-W1            PIC S9(13)V99 VALUE ZEROS.
           05 DIFERENCA-W         PIC S9(13)    VALUE ZEROS.
           05 DIFERENCA-W1        PIC S9(13)V99 VALUE ZEROS.
           05 VALOR-TOTAL         PIC 9(13)V99  VALUE ZEROS.
           05 QTDE-TOTAL          PIC 9(05)     VALUE ZEROS.
           05 QTDE-RECEB          PIC 9(05)     VALUE ZEROS.
           05 VALOR-RECEBIDO      PIC 9(13)V99  VALUE ZEROS.
           05 QTDE-REJEITADO      PIC 9(05)     VALUE ZEROS.
           05 VALOR-REJEITADO     PIC 9(13)V99  VALUE ZEROS.
           05 QTDE-CONF           PIC 9(05)     VALUE ZEROS.
           05 VALOR-CONFIRMADO    PIC 9(13)V99  VALUE ZEROS.
           05 QTDE-BAIXADO        PIC 9(05)     VALUE ZEROS.
           05 VALOR-BAIXADO       PIC 9(13)V99  VALUE ZEROS.
           05 ACRESCIMO-TOTAL     PIC S9(13)V99 VALUE ZEROS.
           05 LETRA               PIC X         VALUE SPACES.
           05 NAO-ENCONTRADO      PIC 9         VALUE ZEROS.
           05 CONT-REJEICOES      PIC 9(02)     VALUE ZEROS.
           05 TRACO               PIC X(80)     VALUE ALL '-'.
           05 DATA-DIA            PIC 9(6)      VALUE ZEROS.
           05 DATA-DIA-I          PIC 9(6)      VALUE ZEROS.
           05 DATA-VENCTO         PIC 9(8)      VALUE ZEROS.
           05 DATA8               PIC 9(8)      VALUE ZEROS.
           05 DATA-E              PIC 99/99/9999.
           05 VALOR-E             PIC ZZZ.ZZZ.ZZZ,ZZ.
           05 VALOR-E1            PIC ZZ.ZZZ.ZZZ,ZZ-.
           05 QTDE-E              PIC ZZZ.ZZ9   VALUE ZEROS.
           05 CONF                PIC X         VALUE SPACES.
           05 LIN                 PIC 9(02)     VALUE ZEROS.
           05 ERRO-W              PIC 9         VALUE ZEROS.
           05 QTDE-PARC           PIC 9(4)      VALUE ZEROS.
           05 SEQ-RET             PIC 9(06)     VALUE ZEROS.
           05 DESC-REJEI          PIC X(30)     VALUE SPACES.
           05 COD-COMPL-CR20-W    PIC 9(09)     VALUE ZEROS.
           05 ACHEI               PIC X(01)     VALUE SPACES.
           05 AUX-OCORRENCIA      PIC X(30)     VALUE ZEROS.
           05 MASC-VALOR          PIC ZZ.ZZ9,99 VALUE ZEROS.
           05 REJEICAO            PIC X(30)     VALUE SPACES.
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
           05 TIPO-01.
              10 T1-TIPO-EMPRESA               PIC 9(02).
              10 T1-CGC-CPF                    PIC 9(14).
              10 T1-FILLER                     PIC X(03).
              10 T1-IDENTIF-EMPRESA-BANCO      PIC X(17).
              10 T1-NUM-CONTROLE-PART          PIC X(25).
              10 T1-FILLER2                    PIC X(08).
              10 T1-IDENTIF-TITULO-BANCO       PIC X(12).
              10 T1-USO-BANCO1                 PIC X(10).
              10 T1-USO-BANCO2                 PIC X(12).
              10 T1-INDICADOR-RATEIO           PIC X(01).
              10 T1-FILLER3                    PIC X(02).
              10 T1-CARTEIRA                   PIC X(01).
              10 T1-IDENTIF-OCORRENCIA         PIC X(02).
              10 T1-DATA-OCORRENCIA.
                 15 T1-DIA-OCORRENCIA          PIC 9(02).
                 15 T1-MES-OCORRENCIA          PIC 9(02).
                 15 T1-ANO-OCORRENCIA          PIC 9(02).
              10 T1-NUMERO-DOCTO               PIC X(10).
              10 T1-IDENTIF-TITULO             PIC X(20).
              10 T1-DATA-VENCTO.
                 15 T1-DIA-VENCTO              PIC 9(02).
                 15 T1-MES-VENCTO              PIC 9(02).
                 15 T1-ANO-VENCTO              PIC 9(02).
              10 T1-VALOR-TITULO               PIC 9(11)V99.
              10 T1-BANCO-COBRADOR             PIC 9(03).
              10 T1-AGENCIA-COBRADORA          PIC 9(05).
              10 T1-ESPECIE-TITULO             PIC X(02).
              10 T1-DESPESAS-COBRANCA          PIC 9(11)V99.
              10 T1-OUTRAS-DESPESAS            PIC 9(11)V99.
              10 T1-JUROS-ATRASO               PIC 9(11)V99.
              10 T1-IOF-DEVIDO                 PIC 9(11)V99.
              10 T1-ABATIMENTO-CONCEDIDO       PIC 9(11)V99.
              10 T1-DESCONTO-CONCEDIDO         PIC 9(11)V99.
              10 T1-VALOR-PAGO                 PIC 9(11)V99.
              10 T1-JUROS-MORA                 PIC 9(11)V99.
              10 T1-OUTROS-CREDITO             PIC 9(11)V99.
              10 T1-BRANCOS                    PIC X(02).
              10 T1-MOTIVO-OCORRENCIA          PIC X(01).
              10 T1-DATA-CREDITO.
                 15 T1-DIA-CREDITO             PIC 9(02).
                 15 T1-MES-CREDITO             PIC 9(02).
                 15 T1-ANO-CREDITO             PIC 9(02).
              10 T1-FILLER4                    PIC X(17).
              10 T1-MOTIVOS-REJEICAO           PIC 9(10).
              10 FILLER REDEFINES T1-MOTIVOS-REJEICAO OCCURS 5 TIMES.
                 15 T1-REJEICAO                PIC 9(02).
              10 T1-FILLER5                    PIC X(66).
              10 T1-SEQUENCIAL-REGISTRO        PIC 9(06).
           05 IND                   PIC 9(02) VALUE ZEROS.
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
               'RELATORIO DE RETORNO - BANCO BRADESCO  '.
           05  FILLER               PIC X(07)  VALUE 'EMIS.:'.
           05  EMISSAO-REL          PIC 99/99/99.

       01  CAB01A.
           05  FILLER               PIC X(85)  VALUE
               'RELATORIO DE PROBLEMAS NA ATUALIZACAO-BANCO BRADESCO'.
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
           MOVE "PAR002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PAR002.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CRD020B" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020B.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           MOVE "REJEICOES" TO ARQUIVO-REJEICOES

           OPEN I-O    CRD020 CRD020B CGD010 PAR002
           CLOSE       CGD010 CRD020 CRD020B PAR002
           OPEN INPUT  CGD010 CRD020 CRD020B PAR002

           OPEN OUTPUT PROBLEMA
      *    OPEN INPUT  RETORNO

           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020 "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010 "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020B <> "00"
              MOVE "ERRO ABERTURA CRD020B"  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020B TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-PAR002 <> "00"
              MOVE "ERRO ABERTURA PAR002"  TO GS-MENSAGEM-ERRO
              MOVE ST-PAR002  TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.


           MOVE "CRP9116A"    TO PROGRAMA-PAR002
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
           move "CRP9116A"          to logacess-programa
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
           END-EVALUATE
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       VALIDAR-ARQUIVO SECTION.
           call "CBL_CHECK_FILE_EXIST"     using gs-acp-caminho
                                                 file-details
                                       returning status-code
           if status-code <> 0
              move "Arquivo Não Encontrado" to mensagem
              move "C" to tipo-msg
              perform exibir-mensagem
              move 1 to gs-flag-critica.

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

           MOVE GS-ACP-CAMINHO TO ARQUIVO-RETORNO

           OPEN INPUT RETORNO

      *    IF ST-RET <> "00"
      *       MOVE "ERRO ABERTURA RETORNO" TO GS-MENSAGEM-ERRO
      *       MOVE ST-RET TO GS-MENSAGEM-ERRO(23: 2)
      *       PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE ZEROS TO LIN
                         QTDE-TOTAL     VALOR-TOTAL
                         QTDE-RECEB     VALOR-RECEBIDO   ACRESCIMO-TOTAL
                         QTDE-REJEITADO VALOR-REJEITADO
                         QTDE-CONF      VALOR-CONFIRMADO
                         QTDE-BAIXADO   VALOR-BAIXADO

           PERFORM UNTIL ST-RET = "10"
                READ RETORNO AT END
                     MOVE "10" TO ST-RET
                NOT AT END
                     MOVE COMECO-RET  TO GS-EXIBE-CODIGO
                     MOVE "REFRESH-DATA" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM

                     IF COMECO-RET = "1"
                        OPEN OUTPUT REJEICOES
                        CLOSE       REJEICOES
                        OPEN I-O    REJEICOES

                        MOVE ZEROS  TO CONT-REJEICOES

                        MOVE RESTANTE-RET TO TIPO-01

                        ADD  1              TO QTDE-TOTAL
                        ADD T1-VALOR-TITULO TO VALOR-TOTAL

                        MOVE SPACES                  TO GS-LINDET
                        MOVE T1-IDENTIF-TITULO-BANCO TO GS-LINDET(1:12)
                        EVALUATE T1-IDENTIF-OCORRENCIA
                            WHEN "02" MOVE "Entrada Confirmada"
                                                      TO AUX-OCORRENCIA
                                      ADD T1-VALOR-TITULO TO
                                          VALOR-CONFIRMADO
                                      ADD 1               TO QTDE-CONF
                                      PERFORM TRATAR-REJEICAO
                            WHEN "03" MOVE "Entrada Rejeitada"
                                                      TO AUX-OCORRENCIA
                                      ADD T1-VALOR-TITULO TO
                                          VALOR-REJEITADO
                                      ADD 1            TO QTDE-REJEITADO
                                      PERFORM TRATAR-REJEICAO
                            WHEN "06" MOVE "Liquidacao Normal"
                                                       TO AUX-OCORRENCIA
                                     ADD T1-VALOR-TITULO TO
                                                         VALOR-RECEBIDO
                                     ADD 1               TO QTDE-RECEB
                            WHEN "09" MOVE "Baixado Automaticamente"
                                                      TO AUX-OCORRENCIA
                                      ADD 1 TO QTDE-BAIXADO
                                      ADD T1-VALOR-TITULO TO
                                               VALOR-BAIXADO
                                      PERFORM TRATAR-REJEICAO
                            WHEN "10" MOVE "Baixado Conf. Inst. Agencia"
                                                      TO AUX-OCORRENCIA
                                      ADD 1 TO QTDE-BAIXADO
                                      ADD T1-VALOR-TITULO TO
                                               VALOR-BAIXADO
                                      PERFORM TRATAR-REJEICAO
                            WHEN "11" MOVE "Em Ser - Titulos Pendentes"
                                                      TO AUX-OCORRENCIA
                            WHEN "12" MOVE "Abatimento Concedido"
                                                      TO AUX-OCORRENCIA
                            WHEN "13" MOVE "Abatimento Cancelado"
                                                      TO AUX-OCORRENCIA
                            WHEN "14" MOVE "Vencimento Alterado"
                                                      TO AUX-OCORRENCIA
                            WHEN "15" MOVE "Liquidacao em Cartorio"
                                                      TO AUX-OCORRENCIA
                            WHEN "17" MOVE "Liquidacao apos Baixa"
                                                      TO AUX-OCORRENCIA
                            WHEN "18" MOVE "Acerto de Depositaria"
                                                      TO AUX-OCORRENCIA
                            WHEN "19" MOVE "Conf. Recebimento Protesto"
                                                      TO AUX-OCORRENCIA
                            WHEN "20" MOVE "Conf. Recebimento Sustacao"
                                                      TO AUX-OCORRENCIA
                            WHEN "21" MOVE "Acerto Control Participante"
                                                      TO AUX-OCORRENCIA
                            WHEN "23" MOVE "Entrada Titulo em Cartorio"
                                                      TO AUX-OCORRENCIA
                            WHEN "24" MOVE "Entrada Rejeitada CEP"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "27" MOVE "Baixa Rejeitada"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "28" MOVE "Débito de Tarifas/Custas"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "30" MOVE "Alteracao Dados Rejeitados"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "32" MOVE "Instrucao Rejeitada"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "33" MOVE "Conf. Pedido Alt. Dados"
                                                      TO AUX-OCORRENCIA
                            WHEN "34" MOVE "Retirado Cartorio Manut."
                                                      TO AUX-OCORRENCIA
                            WHEN "35" MOVE "Desagendamento Debito Aut"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "68" MOVE "Acerto Dados Rateio Cred."
                                                      TO AUX-OCORRENCIA
                            WHEN "69" MOVE "Cancelamento Dados Rateio"
                                                      TO AUX-OCORRENCIA
                            WHEN OTHER MOVE "Não Encontrei STATUS"
                                                      TO AUX-OCORRENCIA
                        END-EVALUATE
                        MOVE SPACES TO GS-LINDET(14:25)
                        STRING T1-IDENTIF-OCORRENCIA "-" AUX-OCORRENCIA
                                  INTO GS-LINDET(14:25)

                        STRING T1-DIA-OCORRENCIA "/" T1-MES-OCORRENCIA
                           "/" T1-ANO-OCORRENCIA INTO GS-LINDET(40:08)

                        MOVE T1-NUMERO-DOCTO       TO GS-LINDET(49:10)

                        STRING T1-DIA-VENCTO "/"
                               T1-MES-VENCTO "/"
                               T1-ANO-VENCTO     INTO GS-LINDET(60:08)

                        MOVE T1-VALOR-TITULO       TO MASC-VALOR
                        MOVE MASC-VALOR            TO GS-LINDET(69:09)

                        MOVE T1-JUROS-ATRASO       TO MASC-VALOR
                        MOVE MASC-VALOR            TO GS-LINDET(79:09)

                        MOVE T1-VALOR-PAGO         TO MASC-VALOR
                        MOVE MASC-VALOR            TO GS-LINDET(89:09)

                        ADD T1-JUROS-MORA          TO ACRESCIMO-TOTAL

                        IF T1-DIA-CREDITO > 0
                           STRING T1-DIA-CREDITO "/"
                                  T1-MES-CREDITO "/"
                                  T1-ANO-CREDITO INTO GS-LINDET(99:08)
                        ELSE
                           MOVE SPACES TO GS-LINDET(99:08)
                        END-IF

                        MOVE "INSERE-LIST" TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM
                        MOVE SPACES        TO GS-LINDET

                        CLOSE      REJEICOES
                        OPEN INPUT REJEICOES

                        INITIALIZE REG-REJEICOES
                        START REJEICOES KEY IS NOT LESS REJ-CHAVE
                                                             INVALID KEY
                              MOVE "10" TO FS-REJEICOES
                        END-START
                        PERFORM UNTIL FS-REJEICOES = "10"
                              READ REJEICOES NEXT AT END
                                   MOVE "10" TO FS-REJEICOES
                              NOT AT END
                                   MOVE SPACES TO GS-LINDET
                                   MOVE REJ-DESCRICAO TO
                                        GS-LINDET(14:33)
                                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                                   PERFORM CALL-DIALOG-SYSTEM
                                   MOVE SPACES        TO GS-LINDET
                              END-READ
                        END-PERFORM

                        CLOSE      REJEICOES
                     END-IF
                END-READ
           END-PERFORM.

           MOVE "Valor Receb: "  TO GS-LINTOT(3:13)
           MOVE VALOR-RECEBIDO   TO VALOR-E
           MOVE VALOR-E          TO GS-LINTOT(16:14)
           MOVE "Qtde Receb: "   TO GS-LINTOT(33:13)
           MOVE QTDE-RECEB       TO QTDE-E
           MOVE QTDE-E           TO GS-LINTOT(46:08)
           MOVE "Acrescimo.: "   TO GS-LINTOT(56:13)
           MOVE ACRESCIMO-TOTAL  TO VALOR-E
           MOVE VALOR-E          TO GS-LINTOT(69:13)


           MOVE "Valor Rejei: "  TO GS-LINTOT2(3:13)
           MOVE VALOR-REJEITADO  TO VALOR-E
           MOVE VALOR-E          TO GS-LINTOT2(16:14)
           MOVE "Qtde Rejei.: "  TO GS-LINTOT2(33:13)
           MOVE QTDE-REJEITADO   TO QTDE-E
           MOVE QTDE-E           TO GS-LINTOT2(46:08)
           MOVE "Valor Conf.: "  TO GS-LINTOT2(56:13)
           MOVE VALOR-CONFIRMADO TO VALOR-E
           MOVE VALOR-E          TO GS-LINTOT2(69:14)
           MOVE "Qtde Conf..: "  TO GS-LINTOT2(86:13)
           MOVE QTDE-CONF        TO QTDE-E
           MOVE QTDE-E           TO GS-LINTOT2(99:08)


           MOVE "Valor Baixa: "  TO GS-LINTOT3(3:13)
           MOVE VALOR-BAIXADO    TO VALOR-E
           MOVE VALOR-E          TO GS-LINTOT3(16:14)
           MOVE "Qtde Baixa.: "  TO GS-LINTOT3(33:13)
           MOVE QTDE-BAIXADO     TO QTDE-E
           MOVE QTDE-E           TO GS-LINTOT3(46:08)
           MOVE "Valor Total: "  TO GS-LINTOT3(56:13)
           MOVE VALOR-TOTAL      TO VALOR-E
           MOVE VALOR-E          TO GS-LINTOT3(69:14)
           MOVE "Qtde Total.: "  TO GS-LINTOT3(86:13)
           MOVE QTDE-PARC        TO QTDE-E
           MOVE QTDE-E           TO GS-LINTOT3(99:08)

           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           CLOSE      RETORNO
           OPEN INPUT RETORNO.

       TRATAR-REJEICAO SECTION.
           MOVE SPACES TO REJEICAO
           EVALUATE T1-IDENTIF-OCORRENCIA
               WHEN "02" PERFORM OCORRENCIA-02
               WHEN "03" PERFORM OCORRENCIA-03
               WHEN "09" PERFORM OCORRENCIA-09
               WHEN "10" PERFORM OCORRENCIA-10
               WHEN "24" PERFORM OCORRENCIA-24
               WHEN "27" PERFORM OCORRENCIA-27
               WHEN "28" PERFORM OCORRENCIA-28
               WHEN "30" PERFORM OCORRENCIA-30
               WHEN "32" PERFORM OCORRENCIA-32
               WHEN "35" PERFORM OCORRENCIA-35
           END-EVALUATE.

       OCORRENCIA-02 SECTION.
           MOVE SPACES   TO REJEICAO
           MOVE 0        TO IND
           PERFORM UNTIL IND = 5
               ADD 1     TO IND
               EVALUATE T1-REJEICAO(IND)
                   WHEN "00" IF IND = 1
                                MOVE "Entrada Confirmada" TO REJEICAO
                             END-IF
                   WHEN "17" MOVE "Data Vencto Anterior Emissao" TO
                                  REJEICAO
                   WHEN "21" MOVE "Especie Titulo Inválido" TO REJEICAO
                   WHEN "24" MOVE "Data Emissao Invalida"   TO REJEICAO
                   WHEN "38" MOVE "Prazo Protesto Invalido" TO REJEICAO
                   WHEN "39" MOVE "Pedido Protesto Nao Permitido"
                                                            TO REJEICAO
                   WHEN "43" MOVE "Prazo Baixa e Devolucao Invalido"
                                                            TO REJEICAO
                   WHEN "45" MOVE "Nome Sacado Invalido"    TO REJEICAO
                   WHEN "46" MOVE "Tipo Inscr. Invalido"    TO REJEICAO
                   WHEN "47" MOVE "Endereco Sacado Nao Informado" TO
                                                               REJEICAO
                   WHEN "48" MOVE "CEP Irregular" TO REJEICAO
                   WHEN "50" MOVE "CEP Ref. Banco Correspondente" TO
                                                               REJEICAO
                   WHEN "53" MOVE "N. Inscr. Sacador Avalista Invalido"
                                                            TO REJEICAO
                   WHEN "54" MOVE "Sacador/Avalista Nao Informado"
                                                            TO REJEICAO
                   WHEN "67" MOVE "Debito Automatico Agendado"
                                                            TO REJEICAO
                   WHEN "68" MOVE "Debito Nao Agendado - Erro Remessa"
                                                            TO REJEICAO
                   WHEN "69" MOVE "Debito Nao Agendado - Sacado nao Cons
      -                           "ta" TO REJEICAO
                   WHEN "70" MOVE "Debito Nao Agendado - Cedente nao Aut
      -                           "orizado" TO REJEICAO
                   WHEN "71" MOVE "Debito Nao Agendado - Cedente nao par
      -                           "ticipa" TO REJEICAO
                   WHEN "72" MOVE "Debito Nao Agendado - Codigo Moeda"
                                           TO REJEICAO
                   WHEN "73" MOVE "Debito Nao Agendado - Data Vencto Inv
      -                           "álida"  TO REJEICAO
                   WHEN "75" MOVE "Debito Nao Agendado - Tipo do Numero
      -                           "Inscr. Invalido" TO REJEICAO
                   WHEN "86" MOVE "Seu Numero do Documento Invalido"
                                                    TO REJEICAO
               END-EVALUATE
               IF REJEICAO <> SPACES
                  PERFORM GRAVAR-REJEICAO
               END-IF
           END-PERFORM.

       OCORRENCIA-03 SECTION.
           MOVE SPACES   TO REJEICAO
           MOVE 0        TO IND
           PERFORM UNTIL IND = 5
               ADD 1     TO IND
               EVALUATE T1-REJEICAO(IND)
                   WHEN "02" MOVE "Codigo de Registro Invalido"
                                                             TO REJEICAO
                   WHEN "03" MOVE "Codigo da Ocorrencia Invalida"
                                                             TO REJEICAO
                   WHEN "04" MOVE "Codigo de Ocorrencia Nao Permitida pa
      -                           "ra Carteira"              TO REJEICAO
                   WHEN "05" MOVE "Codigo de Ocorrencia nao Numerico"
                                                             TO REJEICAO
                   WHEN "07" MOVE "Agencia/Conta/Digito - Invalido"
                                                             TO REJEICAO
                   WHEN "08" MOVE "Nosso Numero Invalido"    TO REJEICAO
                   WHEN "09" MOVE "Nosso Numero Duplicado"   TO REJEICAO
                   WHEN "10" MOVE "Carteira Invalida"        TO REJEICAO
                   WHEN "16" MOVE "Data de Vencimento Invalida"
                                                             TO REJEICAO
                   WHEN "18" MOVE "Vencimento fora do prazo de Operação"
                                                             TO REJEICAO
                   WHEN "20" MOVE "Valor do Titulo Invalido" TO REJEICAO
                   WHEN "21" MOVE "Especie do Titulo Invalido"
                                                             TO REJEICAO
                   WHEN "22" MOVE "Especie nao permitida para carteira"
                                                             TO REJEICAO
                   WHEN "24" MOVE "Data de Emissao Invalida" TO REJEICAO
                   WHEN "44" MOVE "Agencia Cedente Nao Prevista"
                                                             TO REJEICAO
                   WHEN "50" MOVE "CEP Irregular - Banco Correspondente"
                                                             TO REJEICAO
                   WHEN "63" MOVE "Entrada para Titulo ja Cadastrado"
                                                             TO REJEICAO
                   WHEN "68" MOVE "Debito nao agendado - Erro na Remessa
      -                           ""                         TO REJEICAO
                   WHEN "69" MOVE "Debito nao agendado - Sacado nao Cons
      -                           "ta"                       TO REJEICAO
                   WHEN "70" MOVE "Debito nao agendado - Cedente nao aut
      -                           "orizado"                  TO REJEICAO
                   WHEN "71" MOVE "Debito nao agendado - Cedente nao par
      -                           "ticipa"                   TO REJEICAO
                   WHEN "72" MOVE "Debito nao agendado - Codigo de Moeda
      -                            ""                        TO REJEICAO
                   WHEN "73" MOVE "Debito nao agendado - Data Vencto"
                                                             TO REJEICAO
                   WHEN "74" MOVE "Debito nao agendado - Titulo nao Regi
      -                           "strado"                   TO REJEICAO
                   WHEN "75" MOVE "Debito nao agendado - Tipo de N. Insc
      -                           "ricao"                    TO REJEICAO
               END-EVALUATE
               IF REJEICAO <> SPACES
                  PERFORM GRAVAR-REJEICAO
               END-IF
           END-PERFORM.

       OCORRENCIA-09 SECTION.
           MOVE SPACES   TO REJEICAO
           MOVE 0        TO IND
           PERFORM UNTIL IND = 5
               ADD 1     TO IND
               EVALUATE T1-REJEICAO(IND)
                   WHEN "10" MOVE "Baixa Comandada pelo Cliente"
                                                             TO REJEICAO
               END-EVALUATE
               IF REJEICAO <> SPACES
                  PERFORM GRAVAR-REJEICAO
               END-IF
           END-PERFORM.

       OCORRENCIA-10 SECTION.
           MOVE SPACES   TO REJEICAO
           MOVE 0        TO IND
           PERFORM UNTIL IND = 5
               ADD 1     TO IND
               EVALUATE T1-REJEICAO(IND)
                   WHEN "00" MOVE "Baixa Comandada"          TO REJEICAO
                   WHEN "14" MOVE "Titulo Protestado"        TO REJEICAO
                   WHEN "15" MOVE "Titulo Excluido"          TO REJEICAO
               END-EVALUATE
               IF REJEICAO <> SPACES
                  PERFORM GRAVAR-REJEICAO
               END-IF
           END-PERFORM.

       OCORRENCIA-24 SECTION.
           MOVE SPACES   TO REJEICAO
           MOVE 0        TO IND
           PERFORM UNTIL IND = 5
               ADD 1     TO IND
               EVALUATE T1-REJEICAO(IND)
                   WHEN "48" MOVE "CEP Inválido"             TO REJEICAO
               END-EVALUATE
               IF REJEICAO <> SPACES
                  PERFORM GRAVAR-REJEICAO
               END-IF
           END-PERFORM.

       OCORRENCIA-27 SECTION.
           MOVE SPACES   TO REJEICAO
           MOVE 0        TO IND
           PERFORM UNTIL IND = 5
               ADD 1     TO IND
               EVALUATE T1-REJEICAO(IND)
                   WHEN "04" MOVE "Codigo Ocorrencia nao permitido para
      -                           "Carteira"                TO REJEICAO
                   WHEN "07" MOVE "Agencia/Conta/Digito Invalidos" TO
                                                               REJEICAO
                   WHEN "08" MOVE "Nosso Numero Invalido"   TO REJEICAO
                   WHEN "10" MOVE "Carteira Invalida"       TO REJEICAO
                   WHEN "15" MOVE "Carteira/Agencia/Conta/Nosso Nº Inval
      -                           "ido"                     TO REJEICAO
                   WHEN "40" MOVE "Titulo com Ordem de Protesto Emitido"
                                                            TO REJEICAO
                   WHEN "42" MOVE "Codigo para baixa/devolucao via Teleb
      -                           "radesco"                 TO REJEICAO
                   WHEN "60" MOVE "Movimento para Titulo nao Cadastrado"
                                                            TO REJEICAO
                   WHEN "77" MOVE "Transferencia para Desconto não Permi
      -                           "tido para Carteira"      TO REJEICAO
                   WHEN "85" MOVE "Título com Pagamento Vinculado" TO
                                                               REJEICAO
               END-EVALUATE
               IF REJEICAO <> SPACES
                  PERFORM GRAVAR-REJEICAO
               END-IF
           END-PERFORM.

       OCORRENCIA-28 SECTION.
           MOVE SPACES   TO REJEICAO
           MOVE 0        TO IND
           PERFORM UNTIL IND = 5
               ADD 1     TO IND
               EVALUATE T1-REJEICAO(IND)
                   WHEN "03" MOVE "Tarifa de Sustação"     TO REJEICAO
                   WHEN "04" MOVE "Tarifa de Protesto"     TO REJEICAO
                   WHEN "08" MOVE "Custas de Protesto"     TO REJEICAO
               END-EVALUATE
               IF REJEICAO <> SPACES
                  PERFORM GRAVAR-REJEICAO
               END-IF
           END-PERFORM.

       OCORRENCIA-30 SECTION.
           MOVE SPACES   TO REJEICAO
           MOVE 0        TO IND
           PERFORM UNTIL IND = 5
               ADD 1     TO IND
               EVALUATE T1-REJEICAO(IND)
                   WHEN "01" MOVE "Código do Banco Invalido" TO REJEICAO
                   WHEN "04" MOVE "Código de Ocorrência Nao Permitido pa
      -                           "ra carteira"              TO REJEICAO
                   WHEN "05" MOVE "Código da Ocorrência Não Numérico"
                                                             TO REJEICAO
                   WHEN "08" MOVE "Nosso Número Inválido"    TO REJEICAO
                   WHEN "15" MOVE "Característica da Cobrança Incompatív
      -                           "el"                       TO REJEICAO
                   WHEN "16" MOVE "Data de Vencimento Inválido"
                                                             TO REJEICAO
                   WHEN "17" MOVE "Data de Vencimento Anterior a Data de
      -                           "Emissão"                  TO REJEICAO
                   WHEN "18" MOVE "Vencimento fora do Prazo de Operação"
                                                             TO REJEICAO
                   WHEN "24" MOVE "Data de Emissão Inválida" TO REJEICAO
                   WHEN "29" MOVE "Valor do Desconto maior/igual ao Valo
      -                           "r do Título"              TO REJEICAO
                   WHEN "30" MOVE "Desconto a Conceder não Confere"
                                                             TO REJEICAO
                   WHEN "31" MOVE "Concessão de Desconto já Existente"
                                                             TO REJEICAO
                   WHEN "33" MOVE "Valor do Abatimento Inválido"
                                                             TO REJEICAO
                   WHEN "34" MOVE "Valor do Abatimento maior/igual ao Va
      -                           "lor do Título"            TO REJEICAO
                   WHEN "38" MOVE "Prazo para protesto inválido"
                                                             TO REJEICAO
                   WHEN "39" MOVE "Pedido de Protesto Não Permitido para
      -                           " o Título"                TO REJEICAO
                   WHEN "40" MOVE "Título com Ordem de Protesto Emitido"
                                                             TO REJEICAO
                   WHEN "42" MOVE "Código para Baixa/Devolução Inválido"
                                                             TO REJEICAO
                   WHEN "60" MOVE "Movimento para Título Não Cadastrado"
                                                             TO REJEICAO
                   WHEN "85" MOVE "Título com Pagamento Vinculado"
                                                             TO REJEICAO
               END-EVALUATE
               IF REJEICAO <> SPACES
                  PERFORM GRAVAR-REJEICAO
               END-IF
           END-PERFORM.

       OCORRENCIA-32 SECTION.
           MOVE SPACES   TO REJEICAO
           MOVE 0        TO IND
           PERFORM UNTIL IND = 5
               ADD 1     TO IND
               EVALUATE T1-REJEICAO(IND)
                   WHEN "01" MOVE "Código do Banco Inválido" TO REJEICAO
                   WHEN "02" MOVE "Código do Registro Detalhe Inválido"
                                                             TO REJEICAO
                   WHEN "04" MOVE "Código de Ocorrência não Permitido pa
      -                           "ra Carteira"              TO REJEICAO
                   WHEN "05" MOVE "Código de Ocorrência não Numérico"
                                                             TO REJEICAO
                   WHEN "07" MOVE "Agência/Conta/Dígito Inválidos"
                                                             TO REJEICAO
                   WHEN "08" MOVE "Nosso Número Inválido"    TO REJEICAO
                   WHEN "10" MOVE "Carteira Inválida"        TO REJEICAO
                   WHEN "15" MOVE "Características da Cobrança Incompatí
      -                           "veis"                     TO REJEICAO
                   WHEN "16" MOVE "Data de Vencimento Inválida"
                                                             TO REJEICAO
                   WHEN "17" MOVE "Data de Vencimento Anterior a Emissão
      -                           ""                         TO REJEICAO
                   WHEN "18" MOVE "Vencimento fora do Prazo de Operação"
                                                             TO REJEICAO
                   WHEN "20" MOVE "Valor do Título Inválido" TO REJEICAO
                   WHEN "21" MOVE "Espécie do Título Inválido"
                                                             TO REJEICAO
                   WHEN "22" MOVE "Espécie não permitida para a Carteira
      -                           ""                         TO REJEICAO
                   WHEN "24" MOVE "Data de Emissão Inválida" TO REJEICAO
                   WHEN "28" MOVE "Código de Desconto Via Telebradesco I
      -                           "nválido"                  TO REJEICAO
                   WHEN "29" MOVE "Valor do Desconto maior/Igual ao Valo
      -                           "r do Título"              TO REJEICAO
                   WHEN "30" MOVE "Desconto a Conceder Não Confere"
                                                             TO REJEICAO
                   WHEN "31" MOVE "Concessão Abatimento - Já Existe Abat
      -                           "imento Anterior"          TO REJEICAO
                   WHEN "33" MOVE "Valor do Abatimento Inválido"
                                                             TO REJEICAO
                   WHEN "34" MOVE "Valor do Abatimento Maior/Igual ao Va
      -                           "lor do Título"            TO REJEICAO
                   WHEN "36" MOVE "Concessão Abatimento - Já Existe Abat
      -                           "imento Anterior"          TO REJEICAO
                   WHEN "38" MOVE "Prazo para Protesto Inválido"
                                                             TO REJEICAO
                   WHEN "39" MOVE "Pedido de Protesto Não Permitido para
      -                           " o Título"                TO REJEICAO
                   WHEN "40" MOVE "Título com Ordem de Protesto Emitido"
                                                             TO REJEICAO
                   WHEN "41" MOVE "Pedido Cancelamento/Sustação para Tít
      -                           "ulo sem Instrução de Protesto"
                                                             TO REJEICAO
                   WHEN "42" MOVE "Código para Baixa/Devolução Inválido"
                                                             TO REJEICAO
                   WHEN "45" MOVE "Nome do Sacado Não Informado"
                                                             TO REJEICAO
                   WHEN "46" MOVE "Tipo/Número de Inscrição do Sacador A
      -                           "valista Inválido"         TO REJEICAO
                   WHEN "47" MOVE "Endereço do Sacado Não Informado"
                                                             TO REJEICAO
                   WHEN "48" MOVE "CEP Inválido"             TO REJEICAO
                   WHEN "50" MOVE "CEP Referente a um Banco Corresponden
      -                           "te"                       TO REJEICAO
                   WHEN "60" MOVE "Movimento para Título não Cadastrado"
                                                             TO REJEICAO
                   WHEN "85" MOVE "Título com Pagamento Vinculado"
                                                             TO REJEICAO
                   WHEN "86" MOVE "Seu Número Inválido"      TO REJEICAO
               END-EVALUATE
               IF REJEICAO <> SPACES
                  PERFORM GRAVAR-REJEICAO
               END-IF
           END-PERFORM.

       OCORRENCIA-35 SECTION.
           MOVE SPACES   TO REJEICAO
           MOVE 0        TO IND
           PERFORM UNTIL IND = 5
               ADD 1     TO IND
               EVALUATE T1-REJEICAO(IND)
                   WHEN "81" MOVE "Tentativas Esgotadas, Baixado"
                                                           TO REJEICAO
                   WHEN "82" MOVE "Tentativas Esgotadas, Pendente"
                                                           TO REJEICAO
               END-EVALUATE
               IF REJEICAO <> SPACES
                  PERFORM GRAVAR-REJEICAO
               END-IF
           END-PERFORM.

       GRAVAR-REJEICAO SECTION.
           ADD  1                  TO CONT-REJEICOES
           MOVE CONT-REJEICOES     TO REJ-CHAVE
           MOVE SPACES             TO REJ-DESCRICAO
           STRING T1-REJEICAO(IND) "-" REJEICAO INTO REJ-DESCRICAO
           MOVE REJEICAO           TO REJ-DESCRICAO

           WRITE REG-REJEICOES
           MOVE SPACES TO REJEICAO.

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
                     IF COMECO-RET = "1"
                        OPEN OUTPUT REJEICOES
                        CLOSE       REJEICOES
                        OPEN I-O    REJEICOES

                        MOVE ZEROS  TO CONT-REJEICOES

                        MOVE RESTANTE-RET TO TIPO-01

                        MOVE SPACES                  TO GS-LINDET
                        MOVE T1-IDENTIF-TITULO-BANCO TO GS-LINDET(1:12)
                        MOVE T1-NUM-CONTROLE-PART    TO GS-LINDET(14:25)
                        EVALUATE T1-IDENTIF-OCORRENCIA
                            WHEN "02" MOVE "Entrada Confirmada"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "03" MOVE "Entrada Rejeitada"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "06" MOVE "Liquidacao Normal"
                                                       TO AUX-OCORRENCIA
                                     ADD T1-VALOR-TITULO TO VALOR-TOTAL
                                     ADD 1               TO QTDE-PARC
                            WHEN "09" MOVE "Baixado Automaticamente"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "10" MOVE "Baixado Conf. Inst. Agencia"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "11" MOVE "Em Ser - Titulos Pendentes"
                                                      TO AUX-OCORRENCIA
                            WHEN "12" MOVE "Abatimento Concedido"
                                                      TO AUX-OCORRENCIA
                            WHEN "13" MOVE "Abatimento Cancelado"
                                                      TO AUX-OCORRENCIA
                            WHEN "14" MOVE "Vencimento Alterado"
                                                      TO AUX-OCORRENCIA
                            WHEN "15" MOVE "Liquidacao em Cartorio"
                                                      TO AUX-OCORRENCIA
                            WHEN "17" MOVE "Liquidacao apos Baixa"
                                                      TO AUX-OCORRENCIA
                            WHEN "18" MOVE "Acerto de Depositaria"
                                                      TO AUX-OCORRENCIA
                            WHEN "19" MOVE "Conf. Recebimento Protesto"
                                                      TO AUX-OCORRENCIA
                            WHEN "20" MOVE "Conf. Recebimento Sustacao"
                                                      TO AUX-OCORRENCIA
                            WHEN "21" MOVE "Acerto Control Participante"
                                                      TO AUX-OCORRENCIA
                            WHEN "23" MOVE "Entrada Titulo em Cartorio"
                                                      TO AUX-OCORRENCIA
                            WHEN "24" MOVE "Entrada Rejeitada CEP"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "27" MOVE "Baixa Rejeitada"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "28" MOVE "Débito de Tarifas/Custas"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "30" MOVE "Alteracao Dados Rejeitados"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "32" MOVE "Instrucao Rejeitada"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "33" MOVE "Conf. Pedido Alt. Dados"
                                                      TO AUX-OCORRENCIA
                            WHEN "34" MOVE "Retirado Cartorio Manut."
                                                      TO AUX-OCORRENCIA
                            WHEN "35" MOVE "Desagendamento Debito Aut"
                                                      TO AUX-OCORRENCIA
                                      PERFORM TRATAR-REJEICAO
                            WHEN "68" MOVE "Acerto Dados Rateio Cred."
                                                      TO AUX-OCORRENCIA
                            WHEN "69" MOVE "Cancelamento Dados Rateio"
                                                      TO AUX-OCORRENCIA
                            WHEN OTHER MOVE "Não Encontrei STATUS"
                                                      TO AUX-OCORRENCIA
                        END-EVALUATE
                        MOVE SPACES TO GS-LINDET(40:33)
                        STRING T1-IDENTIF-OCORRENCIA "-" AUX-OCORRENCIA
                                  INTO GS-LINDET(40:33)

                        STRING T1-DIA-OCORRENCIA "/" T1-MES-OCORRENCIA
                           "/" T1-ANO-OCORRENCIA INTO GS-LINDET(74:08)

                        MOVE T1-NUMERO-DOCTO       TO GS-LINDET(83:10)

                        STRING T1-DIA-VENCTO "/"
                               T1-MES-VENCTO "/"
                               T1-ANO-VENCTO     INTO GS-LINDET(94:08)

                        MOVE T1-VALOR-TITULO       TO MASC-VALOR
                        MOVE MASC-VALOR            TO GS-LINDET(103:09)

                        MOVE T1-JUROS-ATRASO       TO MASC-VALOR
                        MOVE MASC-VALOR            TO GS-LINDET(113:09)

                        MOVE T1-DESPESAS-COBRANCA  TO MASC-VALOR
                        MOVE MASC-VALOR            TO GS-LINDET(123:09)

                        MOVE T1-OUTRAS-DESPESAS    TO MASC-VALOR
                        MOVE MASC-VALOR            TO GS-LINDET(133:09)

                        MOVE T1-JUROS-MORA         TO MASC-VALOR
                        MOVE MASC-VALOR            TO GS-LINDET(143:09)

                        MOVE T1-VALOR-PAGO         TO MASC-VALOR
                        MOVE MASC-VALOR            TO GS-LINDET(153:09)

                        ADD T1-JUROS-MORA          TO ACRESCIMO-TOTAL

                        IF T1-DIA-CREDITO > 0
                           STRING T1-DIA-CREDITO "/"
                                  T1-MES-CREDITO "/"
                                  T1-ANO-CREDITO INTO GS-LINDET(163:08)
                        ELSE
                           MOVE SPACES TO GS-LINDET(163:08)
                        END-IF

                        WRITE REG-RELAT FROM GS-LINDET
                        END-WRITE
                        ADD 1 TO LIN
                        IF LIN > 56
                           PERFORM CABECALHO
                        END-IF
                        INITIALIZE GS-LINDET

                        CLOSE      REJEICOES
                        OPEN INPUT REJEICOES

                        INITIALIZE REG-REJEICOES
                        START REJEICOES KEY IS NOT LESS REJ-CHAVE
                                                             INVALID KEY
                              MOVE "10" TO FS-REJEICOES
                        END-START
                        PERFORM UNTIL FS-REJEICOES = "10"
                              READ REJEICOES NEXT AT END
                                   MOVE "10" TO FS-REJEICOES
                              NOT AT END
                                   MOVE REJ-DESCRICAO TO GS-LINDET

                                   WRITE REG-RELAT FROM GS-LINDET
                                   END-WRITE
                                   ADD 1 TO LIN
                                   IF LIN > 56
                                      PERFORM CABECALHO
                                   END-IF
                                   INITIALIZE GS-LINDET
                              END-READ
                        END-PERFORM

                        CLOSE      REJEICOES
                     END-IF
                END-READ
           END-PERFORM.
           PERFORM TOTALIZA-REL.
       TOTALIZA-REL SECTION.
           WRITE REG-RELAT FROM CAB03
           MOVE VALOR-TOTAL TO VALOR-REC-TIT-TOT
           MOVE QTDE-PARC   TO QTDE-PARC-TOT
           MOVE ACRESCIMO-TOTAL TO ACRESCIMO-TOT
           WRITE REG-RELAT FROM LINTOT

           COPY DESCONDENSA.

           CLOSE      RETORNO
           OPEN INPUT RETORNO.

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
           CLOSE      CRD020 CRD020B
           OPEN I-O   CRD020 CRD020B

           CLOSE      RETORNO
           OPEN INPUT RETORNO
           MOVE ZEROS TO ST-RET
                         SEQ-RET

           PERFORM UNTIL ST-RET = "10"
               READ RETORNO AT END
                    MOVE "10" TO ST-RET
               NOT AT END
                    ADD 1                         TO SEQ-RET
                    MOVE T1-IDENTIF-TITULO-BANCO  TO GS-EXIBE-CODIGO
                    MOVE "REFRESH-DISPLAY"        TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM

                    IF COMECO-RET = "1"
                       MOVE RESTANTE-RET          TO TIPO-01
                       IF T1-IDENTIF-OCORRENCIA <> 02 AND 03 AND 06 AND
                                                      09 AND 10
                          PERFORM TITULOS-NAO-ENCONTRADO
                       ELSE
                          MOVE T1-NUM-CONTROLE-PART(1:1)  TO
                               CLASS-CLIENTE-CR20
                          MOVE T1-NUM-CONTROLE-PART(2:8)  TO
                               CLIENTE-CR20
                          MOVE T1-NUM-CONTROLE-PART(10:5) TO
                               SEQ-CR20
                          READ CRD020 INVALID KEY
                               PERFORM PROCURAR-PELO-NOSSO-NR
                          NOT INVALID KEY
                               IF SITUACAO-CR20 <> 0
                                  PERFORM TITULOS-NAO-ENCONTRADO
                               ELSE
                                  EVALUATE T1-IDENTIF-OCORRENCIA
                                      WHEN 02  PERFORM GRAVA-NR-BANCO
                                      WHEN 03  PERFORM ENTRADA-REJEITADA
                                      WHEN 06  PERFORM BAIXAR-TITULO
                                      WHEN 09
                                        EVALUATE T1-REJEICAO(1)
                                           WHEN "10"
                                                   PERFORM BAIXAR-TITULO
                                        END-EVALUATE
                                      WHEN 10
                                        EVALUATE T1-REJEICAO(1)
                                           WHEN "10"
                                                   PERFORM BAIXAR-TITULO
                                        END-EVALUATE
                                  END-EVALUATE
                               END-IF
                          END-READ
                       END-IF
                    END-IF
               END-READ
           END-PERFORM
           CLOSE      RETORNO CRD020 CRD020B
           OPEN INPUT RETORNO CRD020 CRD020B

           MOVE "Atualização do Contas a Receber Realizada com SUCESSO"
             TO MENSAGEM
           MOVE "C" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM.

           MOVE "UNSHOW-WIN3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

       PROCURAR-PELO-NOSSO-NR SECTION.
           MOVE "N" TO ACHEI
           INITIALIZE REG-CRD020
           MOVE T1-IDENTIF-TITULO-BANCO TO OUTRO-DOCTO-CR20
           START CRD020 KEY IS NOT LESS ALT8-CR20 INVALID KEY
                MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
                READ CRD020 NEXT AT END
                     MOVE "10" TO ST-CRD020
                NOT AT END
                     IF T1-IDENTIF-TITULO-BANCO <> OUTRO-DOCTO-CR20
                        MOVE "10" TO ST-CRD020
                     ELSE
                        MOVE "10" TO ST-CRD020
                        MOVE "S"  TO ACHEI
                     END-IF
                END-READ
           END-PERFORM

           IF ACHEI = "S"
              IF SITUACAO-CR20 <> 00
                 PERFORM TITULOS-NAO-ENCONTRADO
              ELSE
                 EVALUATE T1-IDENTIF-OCORRENCIA
                     WHEN 02  PERFORM GRAVA-NR-BANCO
                     WHEN 03  PERFORM ENTRADA-REJEITADA
                     WHEN 06  PERFORM BAIXAR-TITULO
                     WHEN 09
                       EVALUATE T1-REJEICAO(1)
                          WHEN "10"
                                  PERFORM BAIXAR-TITULO
                       END-EVALUATE
                     WHEN 10
                       EVALUATE T1-REJEICAO(1)
                          WHEN "10"
                                  PERFORM BAIXAR-TITULO
                       END-EVALUATE
                 END-EVALUATE
              END-IF
           ELSE
              PERFORM ACHAR-POR-VENCIMENTO.

       ACHAR-POR-VENCIMENTO SECTION.
           MOVE "N" TO ACHEI
           INITIALIZE REG-CRD020
           STRING "20"
                  T1-ANO-VENCTO
                  T1-MES-VENCTO
                  T1-DIA-VENCTO INTO DATA-VENCTO-CR20
           MOVE DATA-VENCTO-CR20  TO DATA-VENCTO
           START CRD020 KEY IS NOT LESS ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.

           PERFORM UNTIL ST-CRD020 = "10"
                 READ CRD020 NEXT AT END
                      MOVE "10" TO ST-CRD020
                 NOT AT END
                      IF SITUACAO-CR20 <> 0 OR
                         DATA-VENCTO-CR20 <> DATA-VENCTO
                         MOVE "10" TO ST-CRD020
                      ELSE
                         IF T1-VALOR-TITULO = VALOR-TOT-CR20
                            MOVE VALOR-TOT-CR20 TO MASC-VALOR
                            MOVE COD-COMPL-CR20 TO COD-COMPL-CG10
                            READ CGD010 INVALID KEY
                                 MOVE "******"  TO COMPRADOR-CG10
                            END-READ
                            MOVE SPACES TO MENSAGEM
                            STRING "Possível Título . . . " x"0da0"
                                   "Contrato/Álbum.: "
                                    CLIENTE-CR20(1:4) "/"
                                    CLIENTE-CR20(5:4)       x"0da0"
                                   "Data Vencto.: "
                                    DATA-VENCTO-CR20(7:2) "/"
                                    DATA-VENCTO-CR20(5:2) "/"
                                    DATA-VENCTO-CR20(1:4)   x"0da0"
                                   "Valor Título.: "
                                    MASC-VALOR              x"0da0"
                                   "Nosso Número.: "
                                    T1-IDENTIF-TITULO-BANCO x"0da0"
                                   "Cliente.:"
                                    COMPRADOR-CG10          x"0da0"
                                   "Título Encontrado?" INTO MENSAGEM
                           MOVE "Q" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                           IF RESP-MSG = "S"
                              MOVE "S"  TO ACHEI
                              MOVE T1-IDENTIF-TITULO-BANCO
                                TO OUTRO-DOCTO-CR20
                              REWRITE REG-CRD020 INVALID KEY
                                   PERFORM TITULOS-NAO-ENCONTRADO
                              END-REWRITE
                              MOVE "10" TO ST-CRD020
                           END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM

           IF ACHEI = "S"
              EVALUATE T1-IDENTIF-OCORRENCIA
                  WHEN 02  PERFORM GRAVA-NR-BANCO
                  WHEN 03  PERFORM ENTRADA-REJEITADA
                  WHEN 06  PERFORM BAIXAR-TITULO
                  WHEN 09
                    EVALUATE T1-REJEICAO(1)
                       WHEN "10" PERFORM BAIXAR-TITULO
                    END-EVALUATE
                  WHEN 10
                    EVALUATE T1-REJEICAO(1)
                       WHEN "10"
                               PERFORM BAIXAR-TITULO
                    END-EVALUATE
              END-EVALUATE
           ELSE
              PERFORM TITULOS-NAO-ENCONTRADO.


       GRAVA-NR-BANCO SECTION.
           MOVE T1-IDENTIF-TITULO-BANCO TO OUTRO-DOCTO-CR20.
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
           MOVE T1-VALOR-PAGO TO VALOR-SALDO-CR20
           MOVE ZEROS         TO JURO-RCTO-CR20
           MOVE ZEROS         TO DESCONTO-CR20
           IF VALOR-SALDO-CR20 <> VALOR-TOT-CR20
              COMPUTE DIFERENCA-W1 = VALOR-SALDO-CR20 - VALOR-TOT-CR20
              IF DIFERENCA-W1 > 0
                 MOVE DIFERENCA-W1 TO JURO-RCTO-CR20
              ELSE
                 MOVE DIFERENCA-W1 TO DESCONTO-CR20
              END-IF
           END-IF

           COMPUTE VALOR-LIQ-CR20 = VALOR-TOT-CR20 - DESCONTO-CR20 +
                                    JURO-RCTO-CR20

           MOVE T1-DIA-CREDITO(1:2)    TO AUX-DIA
           MOVE T1-MES-CREDITO(3:2)    TO AUX-MES
           STRING "20" T1-ANO-CREDITO INTO AUX-ANO
           MOVE DATA-AUX               TO DATA-RCTO-CR20
           MOVE "4-Receb.Bco."         TO FORMA-PAGTO-CR20
           MOVE ZEROS                  TO VALOR-SALDO-CR20
           MOVE 2                      TO SITUACAO-CR20

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

           STRING T1-DIA-VENCTO          INTO AUX-DIA
           STRING T1-MES-VENCTO          INTO AUX-MES
           STRING "21" T1-ANO-VENCTO     INTO AUX-ANO

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
                       IF NR-DOCTO-CR20 <> T1-NUM-CONTROLE-PART
                          CONTINUE
                       ELSE
                          MOVE T1-VALOR-TITULO TO VALOR-W
                          IF VALOR-TOT-CR20 <> VALOR-W
                             CONTINUE
                          ELSE
                             EVALUATE T1-IDENTIF-OCORRENCIA
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
            MOVE RESTANTE-RET TO DADOS-RETORNO
            MOVE SEQ-RET      TO SEQ-PRO
            WRITE REG-PROBLEMA.
       ERRO-GRAVACAO-CRD020 SECTION.
           MOVE "ERRO GRAVAÇÃO CRD020" TO GS-MENSAGEM-ERRO
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *----------------------------------------------------------
      * Títulos retornados do banco, não encontrados no arquivo CRD020
       LISTA-NAO-ENCONTRADOS SECTION.
           CLOSE      PROBLEMA
           OPEN INPUT PROBLEMA

           MOVE "CLEAR-LIST-BOX2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE "REFRESH-LIST-BOX2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL ST-PROBLEMA = "10"
               READ PROBLEMA AT END
                    MOVE "10" TO ST-PROBLEMA
               NOT AT END
                    MOVE DADOS-RETORNO         TO TIPO-01

                    OPEN OUTPUT REJEICOES
                    CLOSE       REJEICOES
                    OPEN I-O    REJEICOES

                    MOVE ZEROS  TO CONT-REJEICOES

                    MOVE RESTANTE-RET TO TIPO-01

                    MOVE SPACES                  TO GS-LINDET1
                    MOVE T1-IDENTIF-TITULO-BANCO TO GS-LINDET1(1:12)
                    MOVE T1-NUM-CONTROLE-PART    TO GS-LINDET1(14:25)
                    EVALUATE T1-IDENTIF-OCORRENCIA
                        WHEN "02" MOVE "Entrada Confirmada"
                                                  TO AUX-OCORRENCIA
                                  PERFORM TRATAR-REJEICAO
                        WHEN "03" MOVE "Entrada Rejeitada"
                                                  TO AUX-OCORRENCIA
                                  PERFORM TRATAR-REJEICAO
                        WHEN "06" MOVE "Liquidacao Normal"
                                                   TO AUX-OCORRENCIA
                                 ADD T1-VALOR-TITULO TO VALOR-TOTAL
                                 ADD 1               TO QTDE-PARC
                        WHEN "09" MOVE "Baixado Automaticamente"
                                                  TO AUX-OCORRENCIA
                                  PERFORM TRATAR-REJEICAO
                        WHEN "10" MOVE "Baixado Conf. Inst. Agencia"
                                                  TO AUX-OCORRENCIA
                                  PERFORM TRATAR-REJEICAO
                        WHEN "11" MOVE "Em Ser - Titulos Pendentes"
                                                  TO AUX-OCORRENCIA
                        WHEN "12" MOVE "Abatimento Concedido"
                                                  TO AUX-OCORRENCIA
                        WHEN "13" MOVE "Abatimento Cancelado"
                                                  TO AUX-OCORRENCIA
                        WHEN "14" MOVE "Vencimento Alterado"
                                                  TO AUX-OCORRENCIA
                        WHEN "15" MOVE "Liquidacao em Cartorio"
                                                  TO AUX-OCORRENCIA
                        WHEN "17" MOVE "Liquidacao apos Baixa"
                                                  TO AUX-OCORRENCIA
                        WHEN "18" MOVE "Acerto de Depositaria"
                                                  TO AUX-OCORRENCIA
                        WHEN "19" MOVE "Conf. Recebimento Protesto"
                                                  TO AUX-OCORRENCIA
                        WHEN "20" MOVE "Conf. Recebimento Sustacao"
                                                  TO AUX-OCORRENCIA
                        WHEN "21" MOVE "Acerto Control Participante"
                                                  TO AUX-OCORRENCIA
                        WHEN "23" MOVE "Entrada Titulo em Cartorio"
                                                  TO AUX-OCORRENCIA
                        WHEN "24" MOVE "Entrada Rejeitada CEP"
                                                  TO AUX-OCORRENCIA
                                  PERFORM TRATAR-REJEICAO
                        WHEN "27" MOVE "Baixa Rejeitada"
                                                  TO AUX-OCORRENCIA
                                  PERFORM TRATAR-REJEICAO
                        WHEN "28" MOVE "Débito de Tarifas/Custas"
                                                  TO AUX-OCORRENCIA
                                  PERFORM TRATAR-REJEICAO
                        WHEN "30" MOVE "Alteracao Dados Rejeitados"
                                                  TO AUX-OCORRENCIA
                                  PERFORM TRATAR-REJEICAO
                        WHEN "32" MOVE "Instrucao Rejeitada"
                                                  TO AUX-OCORRENCIA
                                  PERFORM TRATAR-REJEICAO
                        WHEN "33" MOVE "Conf. Pedido Alt. Dados"
                                                  TO AUX-OCORRENCIA
                        WHEN "34" MOVE "Retirado Cartorio Manut."
                                                  TO AUX-OCORRENCIA
                        WHEN "35" MOVE "Desagendamento Debito Aut"
                                                  TO AUX-OCORRENCIA
                                  PERFORM TRATAR-REJEICAO
                        WHEN "68" MOVE "Acerto Dados Rateio Cred."
                                                  TO AUX-OCORRENCIA
                        WHEN "69" MOVE "Cancelamento Dados Rateio"
                                                  TO AUX-OCORRENCIA
                        WHEN OTHER MOVE "Não Encontrei STATUS"
                                                  TO AUX-OCORRENCIA
                    END-EVALUATE
                    MOVE SPACES TO GS-LINDET1(40:33)
                    STRING T1-IDENTIF-OCORRENCIA "-" AUX-OCORRENCIA
                              INTO GS-LINDET1(40:33)

                    STRING T1-DIA-OCORRENCIA "/" T1-MES-OCORRENCIA
                       "/" T1-ANO-OCORRENCIA INTO GS-LINDET1(74:08)

                    MOVE T1-NUMERO-DOCTO       TO GS-LINDET1(83:10)

                    STRING T1-DIA-VENCTO "/"
                           T1-MES-VENCTO "/"
                           T1-ANO-VENCTO     INTO GS-LINDET1(94:08)

                    MOVE T1-VALOR-TITULO       TO MASC-VALOR
                    MOVE MASC-VALOR            TO GS-LINDET1(103:09)

                    MOVE T1-JUROS-ATRASO       TO MASC-VALOR
                    MOVE MASC-VALOR            TO GS-LINDET1(113:09)

                    MOVE T1-DESPESAS-COBRANCA  TO MASC-VALOR
                    MOVE MASC-VALOR            TO GS-LINDET1(123:09)

                    MOVE T1-OUTRAS-DESPESAS    TO MASC-VALOR
                    MOVE MASC-VALOR            TO GS-LINDET1(133:09)

                    MOVE T1-JUROS-MORA         TO MASC-VALOR
                    MOVE MASC-VALOR            TO GS-LINDET1(143:09)

                    MOVE T1-VALOR-PAGO         TO MASC-VALOR
                    MOVE MASC-VALOR            TO GS-LINDET1(153:09)

                    ADD T1-JUROS-MORA          TO ACRESCIMO-TOTAL

                    IF T1-DIA-CREDITO > 0
                       STRING T1-DIA-CREDITO "/"
                              T1-MES-CREDITO "/"
                              T1-ANO-CREDITO INTO GS-LINDET1(163:08)
                    ELSE
                       MOVE SPACES TO GS-LINDET1(163:08)
                    END-IF

                    MOVE "INSERE-LIST2" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                    MOVE SPACES         TO GS-LINDET1

                    CLOSE      REJEICOES
                    OPEN INPUT REJEICOES

                    INITIALIZE REG-REJEICOES
                    START REJEICOES KEY IS NOT LESS REJ-CHAVE
                                                         INVALID KEY
                          MOVE "10" TO FS-REJEICOES
                    END-START
                    PERFORM UNTIL FS-REJEICOES = "10"
                          READ REJEICOES NEXT AT END
                               MOVE "10" TO FS-REJEICOES
                          NOT AT END
                               MOVE REJ-DESCRICAO  TO GS-LINDET1
                               MOVE "INSERE-LIST2" TO DS-PROCEDURE
                               PERFORM CALL-DIALOG-SYSTEM
                               MOVE SPACES        TO GS-LINDET1
                          END-READ
                    END-PERFORM

                    CLOSE      REJEICOES
               END-READ
           END-PERFORM.

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
                     OPEN OUTPUT REJEICOES
                     CLOSE       REJEICOES
                     OPEN I-O    REJEICOES

                     MOVE ZEROS  TO CONT-REJEICOES

                     MOVE DADOS-RETORNO             TO TIPO-01

                     MOVE T1-NUM-CONTROLE-PART(1:1) TO CLASS-REL
                     MOVE T1-NUM-CONTROLE-PART(2:8) TO CONTALB-REL
                     MOVE T1-NUM-CONTROLE-PART      TO NR-TITULO-REL
                     MOVE T1-IDENTIF-TITULO-BANCO   TO NOSSO-NR-REL
                     MOVE T1-VALOR-PAGO             TO VALOR-REC-TIT-REL
                     MOVE SPACES                    TO DATA-PGTO-REL
                     STRING T1-DIA-CREDITO "/"
                            T1-MES-CREDITO "/"
                            T1-ANO-CREDITO        INTO DATA-PGTO-REL
                     MOVE SPACES                    TO DATA-VENCTO-REL
                     STRING T1-DIA-VENCTO "/"
                            T1-MES-VENCTO "/"
                            T1-ANO-VENCTO         INTO DATA-VENCTO-REL
                     MOVE T1-JUROS-ATRASO           TO ACRESCIMO-REL
                     MOVE T1-IDENTIF-OCORRENCIA     TO OCORRENCIA-REL
                     MOVE T1-MOTIVOS-REJEICAO       TO REJEICAO-REL

                    WRITE REG-RELAT FROM LINDET
                    END-WRITE
                    ADD 1 TO LIN
                    IF LIN > 56
                       PERFORM CABECALHO1
                    END-IF

                    CLOSE      REJEICOES
                    OPEN INPUT REJEICOES

                    INITIALIZE REG-REJEICOES
                    START REJEICOES KEY IS NOT LESS REJ-CHAVE
                                                         INVALID KEY
                          MOVE "10" TO FS-REJEICOES
                    END-START
                    PERFORM UNTIL FS-REJEICOES = "10"
                          READ REJEICOES NEXT AT END
                               MOVE "10" TO FS-REJEICOES
                          NOT AT END
                               WRITE REG-RELAT FROM REJ-DESCRICAO
                               END-WRITE
                               ADD 1 TO LIN
                               IF LIN > 56
                                  PERFORM CABECALHO1
                               END-IF
                          END-READ
                    END-PERFORM

                    CLOSE      REJEICOES

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
           MOVE "CRP9116A" TO DS-SET-NAME
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
           move "CRP9116A"          to logacess-programa
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

           CLOSE CRD020 RETORNO PROBLEMA CRD020B PAR002
           MOVE DS-QUIT-SET TO DS-CONTROL
           PERFORM CALL-DIALOG-SYSTEM
           EXIT PROGRAM.

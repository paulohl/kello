       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRP9111.
       AUTHOR.        MARELI AMANCIO VOLPATO
      *EMISSÃO DE RELATÓRIO DE RETORNO DO BANESTADO
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
           05  ID-REG-RET       PIC 9.
           05  DADOS-RET        PIC X(393).
           05  SEQ-RET          PIC 9(6).
       FD  PROBLEMA.
       01  REG-PROBLEMA.
           05  DADOS-PRO        PIC X(393).
           05  SEQ-PRO          PIC 9(6).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP9111.CPB".
           COPY "CRP9111.CPY".
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
           05 VALOR-W             PIC 9(11)V99 VALUE ZEROS.
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
           05 RET-TIPO1.
              10  COD-EMP-T1       PIC 9(2).
              10  CGC-EMP-T1       PIC 9(14).
              10  BRANCO0-T1       PIC X(7).
              10  AGENC-CED-T1     PIC 9(5).
              10  CONTA-CORR-T1    PIC 9(7).
              10  DIGITO-CONTA-T1  PIC 9.
              10  NR-CONTR-T1      PIC X(25).
              10  NOSSO-NR-T1      PIC 9(10).
              10  BRANCO1-T1       PIC X(35).
              10  CARTEIRA-T1      PIC 9.
              10  OCORRENCIA-T1    PIC 9(2).
              10  DATA-PGTO-T1     PIC 9(6).
              10  NR-TITULO-T1     PIC X(10).
              10  BRANCO5-T1       PIC X(20).
              10  VENCTO-TIT-T1    PIC 9(6).
              10  VALOR-TIT-T1     PIC 9(13).
              10  BANCO-COBR-T1    PIC 9(3).
              10  AGENC-DEPOS-T1   PIC 9(5).
              10  ESPECIE-TIT-T1   PIC 9(2).
              10  VLR-TARIFA-T1    PIC 9(13).
              10  VLR-OUT-DESP-T1  PIC 9(13).
              10  VALOR-ATRASO-T1  PIC 9(13).
              10  VALOR-ZEROS-T1   PIC 9(13).
              10  VALOR-ABATIM-T1  PIC 9(13).
              10  VALOR-DESCONTO-T1 PIC 9(13).
              10  VALOR-COBR-TIT-T1 PIC 9(13).
              10  VALOR-JR-COBR-T1  PIC 9(13).
              10  VALOR-OUT-CRED-T1 PIC 9(13).
              10  BRANCO6-T1        PIC X(3).
              10  DATA-CRED-LIQ-T1  PIC 9(6).
              10  COD-REJEICAO-T1   PIC 9(2).
              10  BRANCO3-T1        PIC X(91).
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

       01  CAB01.
           05  FILLER               PIC X(85)  VALUE
               'RELATORIO DE RETORNO - BANESTADO'.
           05  FILLER               PIC X(07)  VALUE 'EMIS.:'.
           05  EMISSAO-REL          PIC 99/99/99.
       01  CAB01A.
           05  FILLER               PIC X(85)  VALUE
               'RELATORIO DE PROBLEMAS NA ATUALIZACAO - BANESTADO'.
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
           05  DATA-PGTO-REL        PIC 99/99/99.
           05  FILLER               PIC XX     VALUE SPACES.
           05  DATA-VENCTO-REL      PIC 99/99/99.
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
           MOVE "PAR002"   TO ARQ-REC. MOVE EMPRESA-REF TO PATH-PAR002
           MOVE "CRD020"   TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020
           MOVE "CRD020B"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020B
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS

           OPEN I-O    CRD020 CRD020B PAR002
           CLOSE       CRD020 CRD020B PAR002
           OPEN INPUT  CRD020 CRD020B PAR002

           OPEN INPUT  RETORNO
           OPEN OUTPUT PROBLEMA

           IF ST-RET <> "00"
              MOVE "ERRO ABERTURA RETORNO "  TO GS-MENSAGEM-ERRO
              MOVE ST-RET TO GS-MENSAGEM-ERRO(23: 02)
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


           MOVE "CRP9111" TO PROGRAMA-PAR002
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
           move "CRP9111"           to logacess-programa
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
           MOVE 1 TO ERRO-W
           move 1 to gs-exit-flg.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       CARREGA-LISTA SECTION.
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
                    MOVE SEQ-RET  TO GS-EXIBE-CODIGO
                    MOVE "REFRESH-DATA" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                    IF ID-REG-RET = 9
                       MOVE "10" TO ST-RET
                    ELSE
                       MOVE DADOS-RET TO RET-TIPO1
                       IF ID-REG-RET = 0
                          CONTINUE
                       ELSE
                          MOVE NR-CONTR-T1(1: 1)     TO LETRA
                          IF LETRA = "X" OR "A"
                             MOVE NR-CONTR-T1(2: 1)  TO GS-LINDET(1: 2)
                             MOVE NR-CONTR-T1(3: 8)  TO GS-LINDET(3: 10)
                          ELSE MOVE "0"              TO GS-LINDET(1: 2)
                               MOVE NR-CONTR-T1(1: 9)
                                                     TO GS-LINDET(3: 10)
                          END-IF
                          MOVE NR-TITULO-T1         TO GS-LINDET(13: 11)
                          MOVE NOSSO-NR-T1          TO GS-LINDET(24: 16)
                          MOVE VALOR-TIT-T1(1: 11)  TO VALOR-W(1: 11)
                          MOVE VALOR-TIT-T1(12: 2)  TO VALOR-W(12: 2)
                          MOVE VALOR-W              TO VALOR-E
                          MOVE VALOR-E              TO GS-LINDET(40: 15)
                          IF OCORRENCIA-T1 = 06
                             ADD VALOR-W            TO VALOR-TOTAL
                             ADD 1                  TO QTDE-PARC
                          END-IF
                          MOVE DATA-PGTO-T1         TO DATA-W
                          MOVE DATA-W(1: 4)         TO DATA8(1: 4)
                          MOVE ANO-W                TO DATA8(7: 2)
                          IF ANO-W > 90
                             MOVE 19                TO DATA8(5: 2)
                          ELSE
                             MOVE 20                TO DATA8(5: 2)
                          END-IF
                          MOVE DATA8                TO DATA-E
                          MOVE DATA-E               TO GS-LINDET(55: 11)
                          MOVE VENCTO-TIT-T1        TO DATA-W
                          MOVE DATA-W(1: 4)         TO DATA8(1: 4)
                          MOVE ANO-W                TO DATA8(7: 2)
                          IF ANO-W > 90
                             MOVE 19                TO DATA8(5: 2)
                          ELSE
                             MOVE 20                TO DATA8(7: 2)
                          END-IF
                          MOVE DATA8                TO DATA-E
                          MOVE DATA-E               TO GS-LINDET(66: 11)
                          IF OCORRENCIA-T1 = 06
                             PERFORM CALCULA-DIFERENCA
                             MOVE VALOR-E1          TO GS-LINDET(77: 15)
                             ADD VALOR-W1           TO ACRESCIMO-TOTAL
                          END-IF
                          MOVE OCORRENCIA-T1        TO GS-LINDET(92: 3)
                          MOVE COD-REJEICAO-T1      TO GS-LINDET(95: 2)
                          MOVE "INSERE-LIST" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                       END-IF
                    END-IF
               END-READ
           END-PERFORM.
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
           CLOSE      RETORNO
           OPEN INPUT RETORNO.
      *--------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO LIN.

           COPY CONDENSA.

           PERFORM CABECALHO.
           PERFORM UNTIL ST-RET = "10"
            READ RETORNO AT END MOVE "10" TO ST-RET
             NOT AT END
             IF ID-REG-RET = 9 MOVE "10" TO ST-RET
             ELSE
              MOVE DADOS-RET TO RET-TIPO1
              IF ID-REG-RET = 0 OR NR-TITULO-T1 = SPACES
                  CONTINUE
              ELSE
      *    NR-TITULO-T1 = SPACES - TRATA-SE DE ORGANIZA€ÇO DE EVENTOS
      *    NR-TITULO-T1 NOT = SPACES - TRATA-SE DE CONTAS A RECEBER
                 MOVE NR-CONTR-T1(1: 1)        TO LETRA
                 IF LETRA = "X" OR "A"
                    MOVE NR-CONTR-T1(2: 1)     TO CLASS-REL
                    MOVE NR-CONTR-T1(3: 8)     TO CONTALB-REL
                 ELSE MOVE "0"                 TO CLASS-REL
                      MOVE NR-CONTR-T1(1: 9)   TO CONTALB-REL
                 END-IF
                 MOVE NR-TITULO-T1             TO NR-TITULO-REL
                 MOVE NOSSO-NR-T1              TO NOSSO-NR-REL
                 MOVE VALOR-TIT-T1(1: 11)      TO VALOR-W(1: 11)
                 MOVE VALOR-TIT-T1(12: 2)      TO VALOR-W(12: 2)
                 MOVE VALOR-W                  TO VALOR-REC-TIT-REL
                 MOVE DATA-PGTO-T1             TO DATA-PGTO-REL
                 MOVE VENCTO-TIT-T1            TO DATA-VENCTO-REL
                 IF OCORRENCIA-T1 = 06
                    PERFORM CALCULA-DIFERENCA
                    MOVE VALOR-E1                 TO ACRESCIMO-REL
                 END-IF
                 MOVE OCORRENCIA-T1            TO OCORRENCIA-REL
                 MOVE COD-REJEICAO-T1          TO REJEICAO-REL
                 WRITE REG-RELAT FROM LINDET
                 END-WRITE
                 ADD 1 TO LIN
                 IF LIN > 56 PERFORM CABECALHO
                 END-IF
              END-IF
             END-IF
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
           CLOSE      CRD020 CRD020B
           OPEN I-O   CRD020 CRD020B

           CLOSE      RETORNO
           OPEN INPUT RETORNO
           MOVE ZEROS TO ST-RET.
           PERFORM UNTIL ST-RET = "10"
               READ RETORNO AT END
                    MOVE "10" TO ST-RET
               NOT AT END
                    MOVE NR-CONTR-T1(1: 10) TO GS-EXIBE-CODIGO
                    MOVE "REFRESH-DISPLAY" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                    MOVE DADOS-RET TO RET-TIPO1
                    IF ID-REG-RET = 0 OR
                       OCORRENCIA-T1 <> 06 AND OCORRENCIA-T1 <> 03
                       AND OCORRENCIA-T1 <> 02
                         PERFORM TITULOS-NAO-ENCONTRADO
                         CONTINUE
                    ELSE
                       IF ID-REG-RET = 9
                          CLOSE      RETORNO
                          OPEN INPUT RETORNO
                          MOVE "10" TO ST-RET
      *  OS TÍTULOS ANTIGOS QUE NÃO TEM CLASSIFICAÇÃO-VERIFICAR
                       ELSE
                          MOVE NR-CONTR-T1(1: 1) TO LETRA
                          IF LETRA <> "X"
                             PERFORM PROCURA-TITULO
      *                      não faz parte do layout novo = cod-compl+seq
                          ELSE
                             MOVE NR-CONTR-T1(2: 9)  TO COD-COMPL-CR20
                             MOVE NR-CONTR-T1(10: 5) TO SEQ-CR20
                             READ CRD020 INVALID KEY
                                  PERFORM TITULOS-NAO-ENCONTRADO
                                  CONTINUE
                             NOT INVALID KEY
      *  Situacao <> 00(título em aberto)
                                  IF SITUACAO-CR20 <> 00
                                     PERFORM TITULOS-NAO-ENCONTRADO
                                  ELSE
                                     EVALUATE OCORRENCIA-T1
                                       WHEN 02 PERFORM GRAVA-NR-BANCO
                                       WHEN 03 PERFORM ENTRADA-REJEITADA
                                       WHEN 06 PERFORM BAIXAR-TITULO
                                     END-EVALUATE
                                  END-IF
                             END-READ
                          END-IF
                       END-IF
                    END-IF
               END-READ
           END-PERFORM
           CLOSE      CRD020 CRD020B
           OPEN INPUT CRD020 CRD020B
           MOVE "UNSHOW-WIN3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       GRAVA-NR-BANCO SECTION.
           MOVE NOSSO-NR-T1 TO OUTRO-DOCTO-CR20.
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
           MOVE VALOR-COBR-TIT-T1(1: 11) TO VALOR-W(1: 11).
           MOVE VALOR-COBR-TIT-T1(12: 2) TO VALOR-W(12: 2)
           MOVE VALOR-W                  TO VALOR-LIQ-CR20
           IF VALOR-LIQ-CR20 <> VALOR-TOT-CR20
              COMPUTE DIFERENCA-W1 = VALOR-LIQ-CR20 - VALOR-TOT-CR20
              IF DIFERENCA-W1 > 0
                 MOVE DIFERENCA-W1 TO JURO-RCTO-CR20
              ELSE
                 MOVE DIFERENCA-W1 TO DESCONTO-CR20
              END-IF
           END-IF
           MOVE DATA-PGTO-T1 TO DATA-W
           MOVE DIA-W TO DATA-RCTO-CR20(7: 2)
           MOVE MES-W TO DATA-RCTO-CR20(5: 2)
           MOVE ANO-W TO DATA-RCTO-CR20(3: 2)
           IF ANO-W > 90
              MOVE 19 TO DATA-RCTO-CR20(1: 2)
           ELSE
              MOVE 20 TO DATA-RCTO-CR20(1: 2)
           END-IF
           MOVE 2 TO SITUACAO-CR20
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
           MOVE VENCTO-TIT-T1 TO DATA-W.
           MOVE DIA-W TO DIA-I.  MOVE MES-W TO MES-I.
           MOVE ANO-W TO ANO-I.
           MOVE DATA-I TO DATA-VENCTO-CR20(3: 6) DATA-VENCTO(3: 6).
           IF ANO-I > 90 MOVE 19 TO DATA-VENCTO-CR20(1: 2)
                                    DATA-VENCTO(1: 2)
           ELSE MOVE 20 TO DATA-VENCTO-CR20(1: 2) DATA-VENCTO(1: 2).
           MOVE ZEROS TO SITUACAO-CR20 COD-COMPL-CR20.
           MOVE ZEROS TO NAO-ENCONTRADO.
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
                        IF NR-DOCTO-CR20 <> NR-TITULO-T1
                           CONTINUE
                        ELSE
                           MOVE VALOR-TIT-T1(1: 11) TO VALOR-W(1: 11)
                           MOVE VALOR-TIT-T1(12: 2) TO VALOR-W(12: 2)
                           IF VALOR-TOT-CR20 <> VALOR-W
                              CONTINUE
                           ELSE
                              EVALUATE OCORRENCIA-T1
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
           IF ID-REG-RET = 0 OR = 9 CONTINUE
           ELSE
            MOVE DADOS-RET TO DADOS-PRO
            MOVE SEQ-RET   TO SEQ-PRO
            WRITE REG-PROBLEMA.
       ERRO-GRAVACAO-CRD020 SECTION.
           MOVE "ERRO GRAVAÇÃO CRD020" TO GS-MENSAGEM-ERRO
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CALCULA-DIFERENCA SECTION.
           COMPUTE DIFERENCA-W = VALOR-COBR-TIT-T1 - VALOR-TIT-T1.
           MOVE DIFERENCA-W(1: 11)       TO VALOR-W1(1: 11)
           MOVE DIFERENCA-W(12: 2)       TO VALOR-W1(12: 2)
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

           PERFORM UNTIL ST-PROBLEMA = "10"
                READ PROBLEMA AT END
                     MOVE "10" TO ST-PROBLEMA
                NOT AT END
                     MOVE DADOS-PRO TO RET-TIPO1
                     MOVE NR-CONTR-T1(1: 1)        TO LETRA
                     IF LETRA = "X" OR "A"
                        MOVE NR-CONTR-T1(2: 1)     TO GS-LINDET1(1: 2)
                        MOVE NR-CONTR-T1(3: 8)     TO GS-LINDET1(3: 10)
                     ELSE MOVE "0"                 TO GS-LINDET1(1: 2)
                          MOVE NR-CONTR-T1(1: 9)   TO GS-LINDET1(3: 10)
                     END-IF
                     MOVE NR-TITULO-T1             TO GS-LINDET1(13: 11)
                     MOVE NOSSO-NR-T1              TO GS-LINDET1(24: 16)
                     MOVE VALOR-TIT-T1(1: 11)      TO VALOR-W(1: 11)
                     MOVE VALOR-TIT-T1(12: 2)      TO VALOR-W(12: 2)
                     MOVE VALOR-W                  TO VALOR-E
                     MOVE VALOR-E                  TO GS-LINDET1(40: 15)
                     MOVE DATA-PGTO-T1             TO DATA-W
                     MOVE DATA-W(1: 4)             TO DATA8(1: 4)
                     MOVE ANO-W                    TO DATA8(7: 2)
                     IF ANO-W > 90 MOVE 19         TO DATA8(5: 2)
                     ELSE MOVE 20                  TO DATA8(5: 2)
                     END-IF
                     MOVE DATA8                    TO DATA-E
                     MOVE DATA-E                   TO GS-LINDET1(55: 11)
                     MOVE VENCTO-TIT-T1            TO DATA-W
                     MOVE DATA-W(1: 4)             TO DATA8(1: 4)
                     MOVE ANO-W                    TO DATA8(7: 2)
                     IF ANO-W > 90
                        MOVE 19                    TO DATA8(5: 2)
                     ELSE
                        MOVE 20                    TO DATA8(7: 2)
                     END-IF
                     MOVE DATA8                    TO DATA-E
                     MOVE DATA-E                   TO GS-LINDET1(66: 11)
                     IF OCORRENCIA-T1 = 06
                        PERFORM CALCULA-DIFERENCA
                        MOVE VALOR-E1              TO GS-LINDET1(77: 15)
                     END-IF
                     MOVE OCORRENCIA-T1            TO GS-LINDET1(92: 3)
                     MOVE COD-REJEICAO-T1          TO GS-LINDET1(95: 2)
                     MOVE "INSERE-LIST2"           TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
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
                      MOVE DADOS-PRO TO RET-TIPO1
                      MOVE NR-CONTR-T1(1: 1)        TO LETRA
                      IF LETRA = "X" OR "A"
                         MOVE NR-CONTR-T1(2: 1)     TO CLASS-REL
                         MOVE NR-CONTR-T1(3: 8)     TO CONTALB-REL
                      ELSE
                         MOVE "0"                   TO CLASS-REL
                         MOVE NR-CONTR-T1(1: 9)     TO CONTALB-REL
                      END-IF
                      MOVE NR-TITULO-T1             TO NR-TITULO-REL
                      MOVE NOSSO-NR-T1              TO NOSSO-NR-REL
                      MOVE VALOR-TIT-T1(1: 11)      TO VALOR-W(1: 11)
                      MOVE VALOR-TIT-T1(12: 2)      TO VALOR-W(12: 2)
                      MOVE VALOR-W                  TO VALOR-REC-TIT-REL
                      MOVE DATA-PGTO-T1             TO DATA-PGTO-REL
                      MOVE VENCTO-TIT-T1            TO DATA-VENCTO-REL
                      IF OCORRENCIA-T1 = 06
                         PERFORM CALCULA-DIFERENCA
                         MOVE VALOR-E1           TO ACRESCIMO-REL
                         ADD VALOR-W1            TO ACRESCIMO-TOTAL
                      END-IF
                      MOVE OCORRENCIA-T1            TO OCORRENCIA-REL
                      MOVE COD-REJEICAO-T1          TO REJEICAO-REL
                      WRITE REG-RELAT FROM LINDET
                      END-WRITE
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO1
                      END-IF
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
           MOVE "CRP9111" TO DS-SET-NAME
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
           move "CRP9111"           to logacess-programa
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

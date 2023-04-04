       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CRP9110.
       AUTHOR.        MARELI AMANCIO VOLPATO
      *EMISSÃO DE RELATÓRIO DE RETORNO DO BRADESCO DO CONTAS A RECEBER
      *E ATUALIZA O CONTAS A RECEBER
       DATE-WRITTEN.  14/04/1999.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CRPX020.
           SELECT RETORNO ASSIGN TO "RETORNO"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CRPW020.
       FD  RETORNO.
       01  REG-RETORNO.
           05  ID-REG-RET       PIC 9.
           05  DADOS-RET        PIC X(393).
           05  SEQ-RET          PIC 9(6).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(80).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.

       01  AREAS-DE-TRABALHO.
           05 ERRO-1              PIC XX     VALUE SPACES.
           05 ST-CAD001           PIC XX     VALUE SPACES.
           05 ST-CRD020           PIC XX     VALUE SPACES.
           05 ST-RET              PIC XX     VALUE SPACES.
           05 OPCAO               PIC 9      VALUE ZEROS.
           05 VALOR-W             PIC 9(11)V99 VALUE ZEROS.
           05 DIFERENCA-W         PIC 9(11)V99 VALUE ZEROS.
           05 LETRA               PIC X      VALUE SPACES.
           05 TRACO               PIC X(80)  VALUE ALL '-'.
           05 DATA-DIA            PIC 9(6)   VALUE ZEROS.
           05 DATA-DIA-I          PIC 9(6)   VALUE ZEROS.
           05  DATA-E             PIC 99/99/99.
           05 CONF                PIC X      VALUE SPACES.
           05  QTDE-PARC       PIC 9(4)        VALUE ZEROS.
           05  REC-FILE.
               10  DRI-VE      PIC X           VALUE "\".
               10  EMP-REC     PIC XXX.
               10  BAR-RA      PIC X           VALUE "\".
               10  RES-TO      PIC X(7)        VALUE SPACES.
           05  RECFILE REDEFINES REC-FILE PIC X(12).
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
              10  BRANCO0-T1       PIC X(3).
              10  IDENT-EMP-T1     PIC X(17).
              10  NR-CONTR-T1      PIC X(25).
              10  BRANCO7-T1       PIC 9(8).
              10  NOSSO-NR-T1      PIC X(12).
              10  BRANCO1-T1       PIC X(26).
              10  OCORRENCIA-T1    PIC 9(2).
              10  DATA-PGTO-T1     PIC 9(6).
              10  NR-TITULO-T1     PIC X(10).
              10  BRANCO5-T1       PIC X(20).
              10  VENCTO-TIT-T1    PIC 9(6).
              10  VALOR-TIT-T1     PIC 9(13).
              10  BANCO-COBR-T1    PIC 9(3).
              10  AGENC-DEPOS-T1   PIC 9(5).
              10  ESPECIE-TIT-T1   PIC X(2).
              10  VLR-TARIFA-T1    PIC 9(13).
              10  VLR-OUT-DESP-T1  PIC 9(13).
              10  VALOR-ATRASO-T1  PIC 9(13).
              10  VALOR-ZEROS-T1   PIC 9(13).
              10  VALOR-ABATIM-T1  PIC 9(13).
              10  VALOR-DESCONTO-T1 PIC 9(13).
              10  VALOR-COBR-TIT-T1 PIC 9(13).
              10  VALOR-JR-COBR-T1  PIC 9(13).
              10  BRANCO8-T1        PIC 9(13).
              10  BRANCO6-T1        PIC X(2).
              10  MOTIVO-OCORRE-T1  PIC X(1).
              10  DATA-CRED-LIQ-T1  PIC 9(6).
              10  BRANCO3-T1        PIC X(17).
              10  MOTIVO-REJEIC-T1  PIC 9(10).
              10  BRANCO9-T1        PIC X(66).
       01  CAB01.
           05  FILLER               PIC X(65)  VALUE
               'RELACAO DE PGTOS EFETUADOS PELOS CLIENTES (BRADESCO)'.
           05  FILLER               PIC X(07)  VALUE 'EMIS.:'.
           05  EMISSAO-REL          PIC 99/99/99.

       01  CAB02.
           05  FILLER               PIC X(80)  VALUE ALL '='.

       01  CAB04.
           05  FILLER               PIC X(11)  VALUE 'CONT-ALB'.
           05  FILLER               PIC X(11)  VALUE 'NR-TITULO'.
           05  FILLER               PIC X(14)  VALUE 'NOSSO-NR'.
           05  FILLER               PIC X(18)  VALUE
                                      '    VALOR-REC-TIT'.
           05  FILLER               PIC X(10)  VALUE 'DTA-PGTO'.
           05  FILLER               PIC X(11)  VALUE 'VLR-ACRESC'.
           05  FILLER               PIC X(3)   VALUE "OC".
           05  FILLER               PIC X(2)   VALUE "RJ".
       01  LINDET.
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
           05  ACRESCIMO-REL        PIC ZZZ.ZZZ,ZZ.
           05  FILLER               PIC X      VALUE SPACES.
           05  OCORRENCIA-REL       PIC 99     VALUE ZEROS.
           05  FILLER               PIC X      VALUE SPACES.
           05  REJEICAO-REL         PIC 99     VALUE ZEROS.
       01  CAB03.
           05  FILLER               PIC X(80)  VALUE ALL '-'.
       01  LINTOT.
           05  FILLER               PIC X(18)  VALUE SPACES.
           05  FILLER               PIC X(09)  VALUE 'QT.PARC: '.
           05  QTDE-PARC-TOT        PIC ZZZZ.
           05  FILLER               PIC X(5)   VALUE SPACES.
           05  VALOR-REC-TIT-TOT    PIC ZZ.ZZZ.ZZZ.ZZZ,ZZ.
           05  FILLER               PIC X(10)  VALUE SPACES.
           05  ACRESCIMO-TOT        PIC ZZ.ZZZ.ZZZ.ZZZ,ZZ.

       LINKAGE SECTION.

       SCREEN SECTION.

       01  TELA2.
           04  BACK 1 FORE 3 HIGHLIGHT.
           05  LINE 02 COLUMN 11 VALUE
           'CRP9110 - ARQUIVO DE RETORNO DO BRADESCO - CTAS A RECEBER'.
           05  LINE 03 COLUMN 08 VALUE "--------------------------------
      -    "---------------------------".
           05  LINE 10 COLUMN 11 VALUE '1- EMITE RELATORIO'.
           05  LINE 12 COLUMN 11 VALUE '2- ATUALIZA CTAS A RECEBER'.
           05  LINE 14 COLUMN 11 VALUE '9- SAIR'.
           05  LINE 16 COLUMN 11 VALUE '                   OPCAO >> '.
           05  LINE 22 COLUMN 08 VALUE "--------------------------------
      -    "---------------------------".
           05  LINE 23 COLUMN 50 VALUE  'CONFIRME (S/N) >> '.

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY (1, 1) SPACES WITH BACK 1 FORE 7.
           OPEN INPUT CONTROLE.
           IF ERRO-1 NOT = '00'
              DISPLAY (24, 28) 'ERRO CONTROLE : '  ERRO-1
              CLOSE CONTROLE   GO FINAL1.
           MOVE ZEROS TO EMPRESA.
           READ CONTROLE INVALID KEY
                CLOSE CONTROLE   GO FINAL1.
           MOVE AUX-EMP TO EMP-REC, EMPRESA.
           READ CONTROLE INVALID KEY
                CLOSE CONTROLE   GO FINAL1.
           MOVE "CRD020" TO RES-TO.    MOVE RECFILE TO CRD20.
           MOVE "CRD022" TO RES-TO.    MOVE RECFILE TO CRD22.
           MOVE "CAD001" TO RES-TO.    MOVE RECFILE TO CAD01.
           CLOSE CONTROLE.

           OPEN INPUT RETORNO.
           OPEN I-O CRD020 CRD022 CAD001.
           IF ST-CRD020 = "35"
              CLOSE CRD020  OPEN OUTPUT CRD020  CLOSE CRD020
              OPEN I-O CRD020.
           IF ST-CRD022 = "35"
              CLOSE CRD022  OPEN OUTPUT CRD022  CLOSE CRD022
              OPEN I-O CRD022.
           IF ST-CAD001 = "35"
              CLOSE CAD001  OPEN OUTPUT CAD001  CLOSE CAD001
              OPEN I-O CAD001.

           IF ST-CRD020 NOT = "00"
              DISPLAY (23, 27) "ERRO ABERTURA CRD020  " ST-CRD020
              ACCEPT (23, 70) CONF WITH BEEP            GO FIM.
           IF ST-CAD001 NOT = "00"
              DISPLAY (23, 27) "ERRO ABERTURA CAD001  " ST-CAD001
              ACCEPT (23, 70) CONF WITH BEEP            GO FIM.

           IF ST-CRD022 NOT = "00"
              DISPLAY (23, 27) "ERRO ABERTURA CRD022  " ST-CRD022
              ACCEPT (23, 70) CONF WITH BEEP            GO FIM.

           ACCEPT DATA-I FROM DATE.
           MOVE DIA-I TO DIA-W.
           MOVE MES-I TO MES-W.
           MOVE ANO-I TO ANO-W.
           MOVE DATA-I TO DATA-DIA-I.
           MOVE DATA-W TO DATA-DIA EMISSAO-REL.

       EMITE-RELATORIO SECTION.
           MOVE ZEROS TO LIN VALOR-TOTAL ACRESCIMO-TOTAL QTDE-PARC.
           COPY "COND-IMP".
           OPEN OUTPUT RELAT.
           PERFORM CABECALHO.
           PERFORM UNTIL ST-RET = "10"
            READ RETORNO AT END MOVE "10" ST-RET
             NOT AT END
             MOVE SEQ-RET  TO GS-EXIBE-CODIGO
             MOVE "REFRESH-DATA" TO DS-PROCEDURE
             PERFORM CALL-DIALOG-SYSTEM
             IF ID-REG-RET = 9 MOVE "10" TO ST-RET
             ELSE
              MOVE DADOS-RET TO RET-TIPO1
              IF ID-REG-RET = 0 OR NR-TITULO-T1 = SPACES
                  CONTINUE
              ELSE
      *    NR-TITULO-T1 = SPACES - TRATA-SE DE ORGANIZA€ÇO DE EVENTOS
      *    NR-TITULO-T1 NOT = SPACES - TRATA-SE DE CONTAS A RECEBER
                 MOVE NR-CONTR-T1         TO CONTALB-REL
                 MOVE NR-TITULO-T1        TO NR-TITULO-REL
                 MOVE NOSSO-NR-T1         TO NOSSO-NR-REL
                 MOVE VALOR-TIT-T1(1: 11)   TO VALOR-W(1: 11)
                 MOVE VALOR-TIT-T1(12: 2)   TO VALOR-W(12: 2)
                 MOVE VALOR-W             TO VALOR-REC-TIT-REL
                 IF OCORRENCIA-T1 = 06 ADD VALOR-W TO VALOR-TOTAL
                                       ADD 1       TO QTDE-PARC
                 END-IF
                 MOVE DATA-PGTO-T1        TO DATA-PGTO-REL
                 MOVE VALOR-JR-COBR-T1(1: 11)  TO VALOR-W(1: 11)
                 MOVE VALOR-JR-COBR-T1(12: 2)  TO VALOR-W(12: 2)
                 MOVE OCORRENCIA-T1            TO OCORRENCIA-REL
                 MOVE MOTIVO-REJEIC-T1         TO REJEICAO-REL
                 MOVE VALOR-W             TO ACRESCIMO-REL
                 IF OCORRENCIA-T1 = 06 ADD VALOR-W TO ACRESCIMO-TOTAL
                 END-IF
                 WRITE REG-RELAT FROM LINDET
                 END-WRITE
                 ADD 1 TO LIN
                 IF LIN > 60 PERFORM CABECALHO
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
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.
           COPY "DESC-IMP".
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
       ATUALIZA-A-RECEBER SECTION.
           CLOSE RETORNO.  OPEN INPUT RETORNO.
           MOVE ZEROS TO ST-RET.
           PERFORM UNTIL ST-RET = "10"
            READ RETORNO AT END MOVE "10" TO ST-RET
             NOT AT END
              MOVE DADOS-RET TO RET-TIPO1
              IF ID-REG-RET = 0 OR OCORRENCIA-T1 <> 06 OR
                 NR-TITULO-T1 = SPACES CONTINUE
              ELSE
                IF ID-REG-RET = 9 CLOSE RETORNO OPEN INPUT RETORNO
                      MOVE "10" TO ST-RET
      *  OS TÍTULOS ANTIGOS QUE NÃO TEM CLASSIFICAÇÃO-VERIFICAR
                ELSE
                  MOVE NR-CONTR-T1(1: 1) TO LETRA
                  IF LETRA <> X CONTINUE
      *             não faz parte do layout novo = cod-compl+seq
                  ELSE
                  MOVE NR-CONTR-T1(2: 9)  TO COD-COMPL-CR20
                  MOVE NR-CONTR-T1(10: 5) TO SEQ-CR20
                  READ CRD020 INVALID KEY CONTINUE
                  NOT INVALID KEY
                   MOVE VALOR-COBR-TIT-T1(1: 11) TO VALOR-W(1: 11)
                   MOVE VALOR-COBR-TIT-T1(12: 2) TO VALOR-W(12: 2)
                   MOVE VALOR-W                  TO VALOR-LIQ-CR20
                   IF VALOR-W <> VALOR-TOT-CR20
                      COMPUTE DIFERENCA-W = VALOR-TOT-CR20 - VALOR-W
                      IF DIFERENCA-W > 0
                         MOVE DIFERENCA-W TO JURO-RCTO-CR20
                      ELSE MOVE DIFERENCA-W TO DESCONTO-CR20
                      END-IF
                   END-IF
                   MOVE DATA-CRED-LIQ-T1 TO DATA-W
                   MOVE DIA-W TO DATA-RCTO-CR20(7: 2)
                   MOVE MES-W TO DATA-RCTO-CR20(5: 2)
                   MOVE ANO-W TO DATA-RCTO-CR20(3: 2)
                   IF ANO-W > 90 MOVE 19 TO DATA-RCTO-CR20(1: 2)
                   ELSE MOVE 20 TO DATA-RCTO-CR20(1: 2)
                   END-IF
                   MOVE 2 TO SITUACAO-CR20
                   REWRITE REG-CRD020 INVALID KEY
                        PERFORM ERRO-GRAVACAO
                   END-REWRITE
                  END-READ
                END-IF
              END-IF
            END-READ
           END-PERFORM.
           MOVE "UNSHOW-WIN3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO CRD020" TO GS-MENSAGEM-ERRO
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *----------------------------------------------------------
       FIM.
           CLOSE CRD020 RETORNO.
       FINAL1.
           EXIT PROGRAM.

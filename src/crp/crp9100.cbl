       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CRP9100.
       AUTHOR.        MARELI AMANCIO VOLPATO
      * GERA ARQUIVO CBDDMMxx.REM P/ BRADESCO
      * NO MOMENTO DA GERAÇÃO DO ARQUIVO O SISTEMA ESTARÁ ALTERANDO O
      * PORTADOR DO ARQUIVO DE CONTAS A RECEBER
       DATE-WRITTEN.  14/04/1999.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
                      PRINTER IS LPRINTER.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.
           COPY CAPX018.
           COPY CRPX020.
           COPY CGPX010.
           COPY CGPX011.
           COPY CRPX200.
           COPY CRPX201.
           SELECT SEQREC ASSIGN TO "\111\SEQREC"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-SEQREC
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  RECORD KEY IS CODIGO-REC.
           SELECT REMESSA ASSIGN TO REMESSA-NOME
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS SEQ-WK
                  ALTERNATE RECORD KEY IS NOME-WK WITH DUPLICATES.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW001.
       COPY CAPW018.
       COPY CRPW020.
       COPY CGPW010.
       COPY CGPW011.
       COPY CRPW200.
       COPY CRPW201.
       FD  REMESSA.
       01  REG-REMESSA.
           05  ID-REG-REM       PIC 9.
           05  DADOS-REM        PIC X(393).
           05  SEQ-REM          PIC 9(6).

       FD  SEQREC.
       01  REG-SEQREC.
           05  CODIGO-REC      PIC 9.
           05  SEQUENC-REC     PIC 9(6).
       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK           PIC 9(3).
           05  NOME-WK          PIC X(40).
           05  ENDERECO-WK      PIC X(40).
           05  CEP-WK           PIC 9(8).
           05  DOCTO-WK         PIC X(10).
           05  VALOR-WK         PIC 9(8)V99.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER          PIC X(132).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP9100.CPB".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  AREAS-DE-TRABALHO.
           05 ERRO-1              PIC XX     VALUE SPACES.
           05 ST-SEQREC           PIC XX     VALUE SPACES.
           05 ST-CAD018           PIC XX     VALUE SPACES.
           05 ST-CRD020           PIC XX     VALUE SPACES.
           05 ST-REM              PIC XX     VALUE SPACES.
           05 ST-CGD010           PIC XX     VALUE SPACES.
           05 ST-CGD011           PIC XX     VALUE SPACES.
           05 ST-CRD200           PIC XX     VALUE SPACES.
           05 ST-CRD201           PIC XX     VALUE SPACES.
           05 ST-WORK             PIC XX     VALUE SPACES.
           05 VARIA-W             PIC 9(8)   VALUE ZEROS.
           05 VALOR-W             PIC 9(11)V99 VALUE ZEROS.
           05 REMESSA-NOME        PIC X(12)  VALUE SPACES.
           05 PRIMREG-W           PIC 9      VALUE ZEROS.
           05 OPCAO               PIC 9      VALUE ZEROS.
           05 DATA-DIA            PIC 9(6)   VALUE ZEROS.
           05 DATA-DIA-I          PIC 9(8)   VALUE ZEROS.
           05 HORA-BRA            PIC 9(8)   VALUE ZEROS.
           05 ULT-SEQ             PIC 9(5)   VALUE ZEROS.
           05 VALOR-ATRASO        PIC 9(11)V99 VALUE ZEROS.
           05 CONF                PIC X      VALUE SPACES.
           05 VALOR-TOTAL         PIC 9(12)V99 VALUE ZEROS.
           05 QTDE-TITULOS        PIC 9(6)     VALUE ZEROS.
           05 VENCTO-INI-INV      PIC 9(8)     VALUE ZEROS.
           05 VENCTO-FIM-INV      PIC 9(8)     VALUE ZEROS.
           05 ERRO-W              PIC 9        VALUE ZEROS.
           05 SEQUENCIA-W         PIC 9(6)     VALUE ZEROS.
           05 LIN                 PIC 9(02)    VALUE ZEROS.
           05 TIPO-W              PIC 9        VALUE ZEROS.
      *     TIPO-W P/ IDENTIFICAR O TIPO DE REMESSA
           05 DATA-INV            PIC 9(8)     VALUE ZEROS.
           05  NOME-EMP-W      PIC X(40)       VALUE SPACES.
           05  SEQRE           PIC X(12)       VALUE SPACES.
           05  ALB-BUM.
               10  CONTR       PIC 9(04).
               10  ALB         PIC 9(04).
           05  ALBUM-W REDEFINES ALB-BUM PIC 9(08).
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
           05 REM-TIPO0.
              10  IDENT-ARQ-T0              PIC 9.
              10  LITERAL-REMESSA-T0        PIC X(7).
              10  CODIGO-SERVICO-T0         PIC 9(2).
              10  LITERAL-SERVICO-T0        PIC X(15).
              10  COD-EMPRESA-T0            PIC 9(10).
              10  COD-EMPRESA1-T0           PIC 9(10).
              10  NOME-EMP-T0               PIC X(30).
              10  NR-BANCO-T0               PIC 9(3).
              10  NOME-BANCO-T0             PIC X(15).
              10  DDMMAA-T0                 PIC 9(6).
              10  BRANCO-T0                 PIC X(8).
              10  IDENT-SISTEMA-T0          PIC X(2).
              10  SEQUENCIA-T0              PIC 9(7).
              10  BRANCO1-T0                PIC X(277).

           05 REM-TIPO1.
              10  AGENC-DEB-T1     PIC 9(5).
              10  DIGITO-AG-DEB-T1 PIC X.
              10  RAZAO-CONTA-T1   PIC 9(5).
              10  CONTA-CORR-T1    PIC 9(7).
              10  DIGITO-CONTA-T1  PIC X.
              10  IDENT-EMP-T1     PIC X(17).
              10  NR-CONTR-T1      PIC X(25).
              10  COD-BANCO-T1     PIC 9(3).
              10  BRANCO1-T1       PIC 9(5).
              10  IDENT-TIT-BCO-T1 PIC X(12).
              10  DESCONTO-T1      PIC 9(10).
              10  CONDIC-EMIS-T1   PIC 9(1).
              10  IDENT-SE-EMITE-T1 PIC X.
              10  IDENT-OPERAC-T1  PIC X(10).
              10  INDICADOR-RAT-T1 PIC X.
              10  ENDER-AVISO-T1   PIC 9.
              10  BRANCO2-T1       PIC X(2).
              10  IDENT-OCORR-T1   PIC 9(2).
              10  NR-DOCTO-T1      PIC X(10).
              10  VENCTO-TIT-T1    PIC 9(6).
              10  VALOR-TIT-T1     PIC 9(13).
              10  BANCO-COBR-T1    PIC 9(3).
              10  AGENC-DEPOS-T1   PIC 9(5).
              10  ESPECIE-TIT-T1   PIC 9(2).
              10  IDENTIFICACAO-T1 PIC X.
              10  EMISSAO-TIT-T1   PIC 9(6).
              10  INSTRUCAO1-T1    PIC 9(2).
              10  INSTRUCAO2-T1    PIC 9(2).
              10  VALOR-ATRASO-T1  PIC 9(13).
              10  DATA-LIMITE-T1   PIC 9(6).
              10  VALOR-DESCONTO-T1 PIC 9(13).
              10  VALOR-IOF-T1     PIC 9(13).
              10  VALOR-ABATIM-T1  PIC 9(13).
              10  IDENT-INSC-T1    PIC 9(2).
              10  NR-INSC-SAC-T1   PIC 9(14).
              10  NOME-SACADO-T1   PIC X(40).
              10  ENDERECO-COMPL-T1 PIC X(40).
              10  MENSAGEM1-T1     PIC X(12).
              10  CEP-T1           PIC 9(5).
              10  SUFIXO-CEP-T1    PIC 9(3).
              10  MENSAGEM2-T1     PIC X(60).

           05 EMP-REFERENCIA.
              10  VAR1              PIC X VALUE "\".
              10  EMP-REC           PIC XXX.
              10  VAR2              PIC X VALUE "\".
              10  ARQ-REC           PIC X(7).
           05 EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           COPY "PARAMETR".

       01  CAB01.
           05  FILLER               PIC X(115) VALUE
           'RELATORIO DE REMESSA - BRADESCO'.
           05  FILLER               PIC X(09) VALUE 'EMISSAO: '.
           05  EMISSAO-REL          PIC 99/99/99.
       01  CAB02.
           05  FILLER               PIC X(132) VALUE ALL "=".
       01  CAB03.
           05  FILLER               PIC X(132) VALUE
           "NOME                                     ENDERECO
      -    "                      CEP       DOCUMENTO          VALOR".
       01  LINDET.
           05  NOME-REL             PIC X(40) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  ENDERECO-REL         PIC X(40) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  CEP-REL              PIC ZZZZZ.
           05  FILLER               PIC X     VALUE ".".
           05  SUFIXO-REL           PIC ZZZ.
           05  FILLER               PIC X     VALUE SPACES.
           05  DOCTO-REL            PIC X(11) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  VALOR-REL            PIC ZZ.ZZZ.ZZZ,ZZ.
       01  LINDET1.
           05  FILLER               PIC X(20) VALUE 'VALOR TOTAL.: '.
           05  VALOR-TOTAL-REL      PIC ZZ.ZZZ.ZZZ.ZZZ,ZZ.
           05  FILLER               PIC X(20) VALUE SPACES.
           05  FILLER               PIC X(20) VALUE 'QTDE TITULOS: '.
           05  QTDE-TIT-TOTAL-REL   PIC ZZZ.ZZZ.

       01  STRING-1               PIC X(65) VALUE SPACES.

       LINKAGE SECTION.
       77  POP-UP                  PIC X(40).
       PROCEDURE DIVISION USING POP-UP.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           ACCEPT DATA-I FROM DATE.
           MOVE DIA-I TO DIA-W.
           MOVE MES-I TO MES-W.
           MOVE ANO-I TO ANO-W.
           MOVE DATA-W TO DATA-DIA.
           MOVE DATA-I(3: 4) TO DATA-DIA-I(5: 4)
           MOVE ANO-I        TO DATA-DIA-I(3: 2)
           IF ANO-I > 90 MOVE "19" TO DATA-DIA-I(1: 2)
           ELSE MOVE 20 TO DATA-DIA-I(1: 2).
           ACCEPT HORA-BRA FROM TIME.
           MOVE ZEROS TO ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
      *    MOVE "SEQREC" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-SEQREC.
           OPEN INPUT CGD010 CGD011 CAD018.
           OPEN I-O CRD020.
           CLOSE CONTROLE.
           OPEN I-O SEQREC.
           IF ST-SEQREC = "35"
              CLOSE SEQREC   OPEN OUTPUT SEQREC   CLOSE SEQREC
              OPEN I-O SEQREC
              MOVE 1 TO CODIGO-REC
              MOVE ZEROS TO SEQUENC-REC
              WRITE REG-SEQREC.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CRD020 <> "00"
              MOVE "ERRO ABERTURA CRD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CRD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD018 <> "00"
              MOVE "ERRO ABERTURA CAD018: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD018 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF ERRO-W = ZEROS
                PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-IMPRIMIR-RELATORIO-TRUE
                    PERFORM IMPRIME-RELATORIO
               WHEN GS-GERAR-REMESSA-TRUE
                    PERFORM GERA-ARQ-REMESSA
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LER-PORTADOR
               WHEN GS-POPUP-PORTADOR-TRUE
                    PERFORM POPUP-PORTADOR
               WHEN GS-ATUALIZA-PORTADOR-TRUE
                    PERFORM ATUALIZA-PORTADOR-RECEBER
               WHEN GS-ATUALIZ-SEQUENCIA-TRUE
                    PERFORM ATUALIZA-SEQUENCIA
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.
       GERA-ARQ-REMESSA SECTION.
           MOVE GS-NOME-ARQ-REMESSA  TO REMESSA-NOME.
           OPEN OUTPUT REMESSA.
           MOVE GS-VENCTO-INI     TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO VENCTO-INI-INV.
           MOVE GS-VENCTO-FIM     TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE 1 TO CODIGO-REC.
           READ SEQREC.
           MOVE SEQUENC-REC       TO SEQUENCIA-W.

           MOVE DATA-INV          TO VENCTO-FIM-INV.
           MOVE VENCTO-INI-INV    TO DATA-VENCTO-CR20.
           MOVE ZEROS             TO SITUACAO-CR20 COD-COMPL-CR20.
           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           MOVE ZEROS TO PRIMREG-W VALOR-TOTAL QTDE-TITULOS.
           PERFORM MOVER-DADOS-TIPO0.
           PERFORM UNTIL ST-CRD020 = "10"
              READ CRD020 NEXT RECORD AT END MOVE "10" TO ST-CRD020
                NOT AT END
                  IF SITUACAO-CR20 > 0
                    OR DATA-VENCTO-CR20 > VENCTO-FIM-INV
                       MOVE "10" TO ST-CRD020
                  ELSE
                    IF PORTADOR-CR20 NOT = GS-PORTADOR
                       CONTINUE
                    ELSE
                     MOVE GS-SEQ-INICIO TO GS-SEQ
                     MOVE "REFRESH-DATA" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                     PERFORM MOVER-DADOS-TIPO1
                    END-IF
                  END-IF
              END-READ
           END-PERFORM.
           MOVE ZEROS TO GS-SEQ
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM MOVER-DADOS-TIPO4.
           CLOSE REMESSA.
           PERFORM CARREGA-LISTA.
       ATUALIZA-PORTADOR-RECEBER SECTION.
           OPEN INPUT REMESSA.
           PERFORM ABRE-ARQUIVO-ANOTACAO.
           MOVE ZEROS TO ST-REM.
           PERFORM UNTIL ST-REM = "10"
            READ REMESSA AT END MOVE "10" TO ST-REM
             NOT AT END
              MOVE REG-REMESSA(395: 6) TO GS-EXIBE-SEQ
              MOVE "REFRESH-WIN3" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM
              MOVE REG-REMESSA(1: 1) TO TIPO-W
              IF TIPO-W = 0 OR TIPO-W = 9 CONTINUE
              ELSE
                 MOVE REG-REMESSA(2: 393) TO REM-TIPO1
                 MOVE NR-CONTR-T1(2: 9)   TO COD-COMPL-CR20
                 MOVE NR-CONTR-T1(11: 5)  TO SEQ-CR20
                 READ CRD020 INVALID KEY CONTINUE
                   NOT INVALID KEY
                      PERFORM GRAVA-ANOTACAO
                      MOVE 37 TO PORTADOR-CR20
                      REWRITE REG-CRD020
                      END-REWRITE
                 END-READ
              END-IF
            END-READ
           END-PERFORM.
           CLOSE CRD200 CRD201.
           MOVE "UNSHOW-WIN3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE REMESSA.
       ATUALIZA-SEQUENCIA SECTION.
           MOVE 1 TO CODIGO-REC.
           READ SEQREC.
           MOVE SEQUENCIA-W       TO SEQUENC-REC.
           REWRITE REG-SEQREC.
       ABRE-ARQUIVO-ANOTACAO SECTION.
           OPEN I-O CRD200 CRD201.
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
       GRAVA-ANOTACAO SECTION.
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR200
           MOVE ZEROS TO SEQ-CR200 ULT-SEQ.
           START CRD200 KEY IS NOT < CHAVE-CR200 INVALID KEY
                 MOVE "10" TO ST-CRD200.
           PERFORM UNTIL ST-CRD200 = "10"
             READ CRD200 NEXT RECORD AT END MOVE "10" TO ST-CRD200
               NOT AT END
                 IF COD-COMPL-CR200 <> COD-COMPL-CR20
                              MOVE "10" TO ST-CRD200
                 ELSE MOVE SEQ-CR200 TO ULT-SEQ
                      CONTINUE
             END-READ
           END-PERFORM.
           MOVE ZEROS          TO SITUACAO-ANOTACAO-CR200
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
               NOT INVALID KEY MOVE "10" TO ST-CRD200
           END-PERFORM.

           MOVE SEQ-CR200      TO SEQ-CR201.
           MOVE COD-COMPL-CR20 TO COD-COMPL-CR201.
           MOVE "TRANSF.PORTADOR- DOCTO: XXXXXXXXXX - 01-XXXXXXXXXXXXXXX
      -    "X P/ 99-XXXXXXXXXXXXXXXX" TO ANOTACAO-CR201.
           MOVE NR-DOCTO-CR20       TO ANOTACAO-CR201(25: 11)
           MOVE PORTADOR-CR20       TO ANOTACAO-CR201(38: 4) PORTADOR
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT           TO ANOTACAO-CR201(43: 16)
           MOVE 37                  TO ANOTACAO-CR201(63: 4) PORTADOR
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT           TO ANOTACAO-CR201(68: 16)
           MOVE ZEROS TO ST-CRD201.
           MOVE 1              TO SUBSEQ-CR201.
           PERFORM UNTIL ST-CRD201 = "10"
             WRITE REG-CRD201 INVALID KEY
               ADD 1 TO SUBSEQ-CR201
               CONTINUE
              NOT INVALID KEY
                MOVE "10" TO ST-CRD201
             END-WRITE
           END-PERFORM.
       MOVER-DADOS-TIPO0 SECTION.
      *     MOVE 1 TO GS-SEQ-INICIO.
           MOVE 0                        TO ID-REG-REM.
           MOVE 1                        TO IDENT-ARQ-T0
           MOVE "REMESSA"                TO LITERAL-REMESSA-T0
           MOVE 01                       TO CODIGO-SERVICO-T0
           MOVE "COBRANCA"               TO LITERAL-SERVICO-T0
           MOVE ZEROS                    TO COD-EMPRESA-T0
           MOVE 0000072496               TO COD-EMPRESA1-T0
           MOVE "3F EMPRESA FOTOGRAFICA LTDA "
                                         TO NOME-EMP-T0
           MOVE 237                      TO NR-BANCO-T0
           MOVE "BRADESCO"               TO NOME-BANCO-T0
           MOVE DATA-DIA                 TO DDMMAA-T0
           MOVE SPACES                   TO BRANCO-T0
           MOVE "MX"                     TO IDENT-SISTEMA-T0
           ADD 1 TO SEQUENCIA-W
           MOVE SEQUENCIA-W              TO SEQUENCIA-T0
           MOVE SPACES                   TO BRANCO1-T0
           MOVE GS-SEQ-INICIO            TO SEQ-REM.
           MOVE REM-TIPO0 TO DADOS-REM.
           WRITE REG-REMESSA.

       MOVER-DADOS-TIPO1 SECTION.
           MOVE 1                        TO ID-REG-REM
           MOVE ZEROS                    TO AGENC-DEB-T1
           MOVE SPACES                   TO DIGITO-AG-DEB-T1
           MOVE ZEROS                    TO RAZAO-CONTA-T1
           MOVE ZEROS                    TO CONTA-CORR-T1
           MOVE SPACES                   TO DIGITO-CONTA-T1
           MOVE 00190006901376063        TO IDENT-EMP-T1

      *    X - é para identificar o novo layout de contas a receber,
      *    ou seja, chave = cod-compl-cr20 + seq-cr20
           MOVE "X"                      TO NR-CONTR-T1(1: 1)
           MOVE COD-COMPL-CR20           TO NR-CONTR-T1(2: 9)
           MOVE SEQ-CR20                 TO NR-CONTR-T1(11: 05)
           MOVE SPACES                   TO NR-CONTR-T1(16: 10)

           MOVE ZEROS                    TO COD-BANCO-T1
           MOVE ZEROS                    TO BRANCO1-T1
           MOVE ZEROS                    TO IDENT-TIT-BCO-T1
           MOVE ZEROS                    TO DESCONTO-T1
           MOVE 001                      TO CONDIC-EMIS-T1
           MOVE SPACES                   TO IDENT-SE-EMITE-T1
           MOVE SPACES                   TO IDENT-OPERAC-T1
           MOVE SPACES                   TO INDICADOR-RAT-T1
           MOVE ZEROS                    TO ENDER-AVISO-T1
           MOVE SPACES                   TO BRANCO2-T1
           MOVE 01                       TO IDENT-OCORR-T1
           MOVE NR-DOCTO-CR20            TO NR-DOCTO-T1
           MOVE DATA-VENCTO-CR20         TO DATA-INV.
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV(1: 4)           TO VENCTO-TIT-T1(1: 4)
           MOVE DATA-INV(7: 2)           TO VENCTO-TIT-T1(5: 2)
           MOVE VALOR-TOT-CR20           TO VALOR-W
           MOVE VALOR-W(1: 11)           TO VALOR-TIT-T1(1: 11)
           MOVE VALOR-W(12: 2)           TO VALOR-TIT-T1(12: 2)
           ADD VALOR-W TO VALOR-TOTAL.
           ADD 1 TO QTDE-TITULOS.
           MOVE ZEROS                    TO BANCO-COBR-T1
           MOVE ZEROS                    TO AGENC-DEPOS-T1
           MOVE 01                       TO ESPECIE-TIT-T1
           MOVE  "A"                     TO IDENTIFICACAO-T1
           MOVE DATA-EMISSAO-CR20(1: 4)  TO EMISSAO-TIT-T1(1: 4)
           MOVE DATA-EMISSAO-CR20(7: 2)  TO EMISSAO-TIT-T1(5: 2)
           MOVE 06                       TO INSTRUCAO1-T1
           MOVE 05                       TO INSTRUCAO2-T1
           COMPUTE VALOR-ATRASO = (VALOR-TOT-CR20 * 0,05) / 30
           MOVE VALOR-ATRASO(1: 11)      TO VALOR-ATRASO-T1(1: 11)
           MOVE VALOR-ATRASO(12: 2)      TO VALOR-ATRASO-T1(12: 2)
           MOVE ZEROS                    TO DATA-LIMITE-T1
           MOVE ZEROS                    TO VALOR-DESCONTO-T1
           MOVE ZEROS                    TO VALOR-IOF-T1
           MOVE ZEROS                    TO VALOR-ABATIM-T1
           MOVE 01                       TO IDENT-INSC-T1
           MOVE COD-COMPL-CR20  TO COD-COMPL-CG10 COD-COMPL-CG11
           READ CGD010 INVALID KEY INITIALIZE REG-CGD010.
           READ CGD011 INVALID KEY INITIALIZE REG-CGD011.
           MOVE CPF-CG11(3: 14)          TO NR-INSC-SAC-T1
           MOVE COMPRADOR-CG10           TO NOME-SACADO-T1
           MOVE ENDERECO1-CG11           TO ENDERECO-COMPL-T1
           MOVE SPACES                   TO MENSAGEM1-T1
           MOVE CEP1-CG11(1: 5)          TO CEP-T1
           MOVE CEP1-CG11(6: 3)          TO SUFIXO-CEP-T1
           MOVE "MULTA DE 2% APOS 10 DIAS DO VENCIMENTO"
                                         TO MENSAGEM2-T1
           ADD 1 TO GS-SEQ-INICIO.
           MOVE GS-SEQ-INICIO            TO SEQ-REM.
           MOVE REM-TIPO1 TO DADOS-REM.
           WRITE REG-REMESSA.
       MOVER-DADOS-TIPO4 SECTION.
           MOVE 9                        TO ID-REG-REM.
           MOVE SPACES                   TO DADOS-REM.
           MOVE GS-SEQ-INICIO            TO SEQ-REM.
           WRITE REG-REMESSA.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           OPEN INPUT REMESSA.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO ST-REM.
           PERFORM UNTIL ST-REM = "10"
            READ REMESSA AT END MOVE "10" TO ST-REM
             NOT AT END
              MOVE REG-REMESSA(1: 1) TO TIPO-W
              EVALUATE TIPO-W
                WHEN 0 PERFORM CABECALHO-TIPO0
                       PERFORM LINDET-TIPO0
                       PERFORM CABECALHO-TIPO1
                WHEN 1 PERFORM LINDET-TIPO1
                WHEN 9 PERFORM CABECALHO-TIPO9
                               PERFORM LINDET-TIPO9
      *                        MOVE "10" TO ST-REM
              END-EVALUATE
            END-READ
           END-PERFORM.
           MOVE VALOR-TOTAL        TO GS-VALOR-TOTAL
           MOVE QTDE-TITULOS       TO GS-QTDE-TITULO
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE REMESSA.
       CABECALHO SECTION.
           MOVE DATA-DIA TO EMISSAO-REL.
           IF LIN = ZEROS
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB02.
           MOVE 4 TO LIN.
       IMPRIME-RELATORIO SECTION.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.
           OPEN INPUT REMESSA.
           MOVE ZEROS TO SEQ-WK.
           MOVE ZEROS TO ST-REM.
           PERFORM UNTIL ST-REM = "10"
             READ REMESSA AT END MOVE "10" TO ST-REM
               NOT AT END
                MOVE REG-REMESSA(1: 1) TO TIPO-W
                IF TIPO-W <> 1 CONTINUE
                ELSE
                 MOVE REG-REMESSA(2: 393) TO REM-TIPO1
                 ADD 1 TO SEQ-WK
                 MOVE NOME-SACADO-T1    TO NOME-WK
                 MOVE ENDERECO-COMPL-T1 TO ENDERECO-WK
                 MOVE CEP-T1            TO CEP-WK(1: 5)
                 MOVE SUFIXO-CEP-T1     TO CEP-WK(6: 3)
                 MOVE NR-DOCTO-T1       TO DOCTO-WK
                 MOVE VALOR-TIT-T1(4: 8) TO VALOR-WK(1: 8)
                 MOVE VALOR-TIT-T1(12: 2) TO VALOR-WK(9: 2)
                 WRITE REG-WORK
                 END-WRITE
                END-IF
             END-READ
           END-PERFORM.

           COPY "COND-IMP".
           OPEN OUTPUT RELAT.
           CLOSE WORK.   OPEN INPUT WORK.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO NOME-WK.
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END MOVE "10" TO ST-WORK
               NOT AT END
                 MOVE NOME-WK           TO NOME-REL
                 MOVE ENDERECO-WK       TO ENDERECO-REL
                 MOVE CEP-WK(1: 5)      TO CEP-REL
                 MOVE CEP-WK(6: 3)      TO SUFIXO-REL
                 MOVE DOCTO-WK          TO DOCTO-REL
                 MOVE VALOR-WK          TO VALOR-REL
                 WRITE REG-RELAT FROM LINDET
                 ADD 1 TO LIN
                 IF LIN > 56 PERFORM CABECALHO
                 END-IF
             END-READ
           END-PERFORM.
           MOVE VALOR-TOTAL        TO VALOR-TOTAL-REL.
           MOVE QTDE-TITULOS       TO QTDE-TIT-TOTAL-REL.
           WRITE REG-RELAT FROM LINDET1 AFTER 3.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT REMESSA WORK.
           DELETE FILE WORK.
           COPY "DESC-IMP".
       LER-PORTADOR SECTION.
           MOVE GS-PORTADOR TO PORTADOR.
           READ CAD018 INVALID KEY MOVE "*******"  TO NOME-PORT.
           MOVE NOME-PORT TO GS-DESCR-PORTADOR.
       POPUP-PORTADOR SECTION.
           CALL "CAP018T" USING PARAMETROS-W STRING-1
           CANCEL "CAP018T"
           MOVE STRING-1(1: 30) TO GS-DESCR-PORTADOR
           MOVE STRING-1(33: 4) TO GS-PORTADOR.
      *------------------------------------------------------------
       CABECALHO-TIPO0 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "I"                 TO GS-LINDET(1: 2)
           MOVE "LIT.REM"           TO GS-LINDET(3: 8)
           MOVE "CS"                TO GS-LINDET(11: 3)
           MOVE "LIT-SERVICO"       TO GS-LINDET(14: 16)
           MOVE "CD-EMPRESA"        TO GS-LINDET(30: 11)
           MOVE "CD-EMPR-1"         TO GS-LINDET(41: 11)
           MOVE "NOME-EMPRESA"      TO GS-LINDET(52: 31)
           MOVE "BCO"               TO GS-LINDET(83: 4)
           MOVE "NOME-BANCO"        TO GS-LINDET(87: 16)
           MOVE "DDMMAA"            TO GS-LINDET(103: 7)
           MOVE "BRANCO"            TO GS-LINDET(110: 9)
           MOVE "IS"                TO GS-LINDET(119: 3)
           MOVE "SEQUENC"           TO GS-LINDET(122: 8)
           MOVE "BRANCOS"           TO GS-LINDET(130: 277)
           MOVE "SEQUEN"            TO GS-LINDET(408: 6)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LINDET-TIPO0 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE REG-REMESSA(2: 393) TO REM-TIPO0.
           MOVE IDENT-ARQ-T0        TO GS-LINDET(1: 2)
           MOVE LITERAL-REMESSA-T0  TO GS-LINDET(3: 8)
           MOVE CODIGO-SERVICO-T0   TO GS-LINDET(11: 3)
           MOVE LITERAL-SERVICO-T0  TO GS-LINDET(14: 16)
           MOVE COD-EMPRESA-T0      TO GS-LINDET(30: 11)
           MOVE COD-EMPRESA1-T0     TO GS-LINDET(41: 11)
           MOVE NOME-EMP-T0         TO GS-LINDET(52: 31)
           MOVE NR-BANCO-T0         TO GS-LINDET(83: 4)
           MOVE NOME-BANCO-T0       TO GS-LINDET(87: 16)
           MOVE DDMMAA-T0           TO GS-LINDET(103: 7)
           MOVE BRANCO-T0           TO GS-LINDET(110: 9)
           MOVE IDENT-SISTEMA-T0    TO GS-LINDET(119: 3)
           MOVE SEQUENCIA-T0        TO GS-LINDET(122: 8)
           MOVE BRANCO1-T0          TO GS-LINDET(130: 278).
           MOVE REG-REMESSA(395: 6) TO GS-LINDET(408: 6).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CABECALHO-TIPO1 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "AG-DB"             TO GS-LINDET(1: 6)
           MOVE "D"                 TO GS-LINDET(7: 2)
           MOVE "RAZAO"             TO GS-LINDET(09: 6)
           MOVE "CTA.COR"           TO GS-LINDET(15: 08)
           MOVE "DC"                TO GS-LINDET(23: 02)
           MOVE "IDENT-EMPRESA"     TO GS-LINDET(25: 18)
           MOVE "NR-CONTROLE"       TO GS-LINDET(43: 26)
           MOVE "BCO"               TO GS-LINDET(69: 4)
           MOVE "BRANC"             TO GS-LINDET(73: 06)
           MOVE "IDENT-TIT-BC"      TO GS-LINDET(79: 13)
           MOVE "DESCONTO"          TO GS-LINDET(92: 11)
           MOVE "C"                 TO GS-LINDET(103: 2)
           MOVE "E"                 TO GS-LINDET(105: 2)
           MOVE "ID-OPER"           TO GS-LINDET(107: 11)
           MOVE "I"                 TO GS-LINDET(118: 2)
           MOVE "E"                 TO GS-LINDET(120: 2)
           MOVE "BR"                TO GS-LINDET(122: 03)
           MOVE "IO"                TO GS-LINDET(125: 03)
           MOVE "NR-DOCTO"          TO GS-LINDET(128: 11)
           MOVE "VENCTO"            TO GS-LINDET(139: 07)
           MOVE "        VALOR"     TO GS-LINDET(146: 14)
           MOVE "BCO"               TO GS-LINDET(160: 04)
           MOVE "AGENC"             TO GS-LINDET(164: 06)
           MOVE "ES"                TO GS-LINDET(170: 03)
           MOVE "I"                 TO GS-LINDET(173: 2)
           MOVE "EMISS."            TO GS-LINDET(175: 7)
           MOVE "I1"                TO GS-LINDET(182: 3)
           MOVE "I2"                TO GS-LINDET(185: 03)
           MOVE " VALOR-ATRASO"     TO GS-LINDET(188: 14)
           MOVE "DT-LIM"            TO GS-LINDET(202: 7)
           MOVE " VALOR-DESCON"     TO GS-LINDET(209: 14)
           MOVE "    VALOR-IOF"     TO GS-LINDET(223: 14)
           MOVE " VALOR-ABATIM"     TO GS-LINDET(237: 14)
           MOVE "II"                TO GS-LINDET(251: 03)
           MOVE "NR.INSCRICAO"      TO GS-LINDET(254: 15)
           MOVE "NOME SACADO"       TO GS-LINDET(269: 41)
           MOVE "ENDEERCO"          TO GS-LINDET(310: 41)
           MOVE "MENSAGEM1"         TO GS-LINDET(351: 13)
           MOVE "CEP"               TO GS-LINDET(364: 6)
           MOVE "SUF"               TO GS-LINDET(370: 4)
           MOVE "MENSAGEM2"         TO GS-LINDET(374: 61)
           MOVE "SEQUEN"            TO GS-LINDET(435: 6)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       LINDET-TIPO1 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE REG-REMESSA(2: 393) TO REM-TIPO1.
           MOVE AGENC-DEB-T1        TO GS-LINDET(1: 6)
           MOVE DIGITO-AG-DEB-T1    TO GS-LINDET(7: 2)
           MOVE RAZAO-CONTA-T1      TO GS-LINDET(09: 6)
           MOVE CONTA-CORR-T1       TO GS-LINDET(15: 08)
           MOVE DIGITO-CONTA-T1     TO GS-LINDET(23: 02)
           MOVE IDENT-EMP-T1        TO GS-LINDET(25: 18)
           MOVE NR-CONTR-T1         TO GS-LINDET(43: 26)
           MOVE COD-BANCO-T1        TO GS-LINDET(69: 4)
           MOVE BRANCO1-T1          TO GS-LINDET(73: 06)
           MOVE IDENT-TIT-BCO-T1    TO GS-LINDET(79: 13)
           MOVE DESCONTO-T1         TO GS-LINDET(92: 11)
           MOVE CONDIC-EMIS-T1      TO GS-LINDET(103: 2)
           MOVE IDENT-SE-EMITE-T1   TO GS-LINDET(105: 2)
           MOVE IDENT-OPERAC-T1     TO GS-LINDET(107: 11)
           MOVE INDICADOR-RAT-T1    TO GS-LINDET(118: 2)
           MOVE ENDER-AVISO-T1      TO GS-LINDET(120: 2)
           MOVE BRANCO2-T1          TO GS-LINDET(122: 03)
           MOVE IDENT-OCORR-T1      TO GS-LINDET(125: 03)
           MOVE NR-DOCTO-T1         TO GS-LINDET(128: 11)
           MOVE VENCTO-TIT-T1       TO GS-LINDET(139: 07)
           MOVE VALOR-TIT-T1        TO GS-LINDET(146: 14)
           MOVE BANCO-COBR-T1       TO GS-LINDET(160: 04)
           MOVE AGENC-DEPOS-T1      TO GS-LINDET(164: 06)
           MOVE ESPECIE-TIT-T1      TO GS-LINDET(170: 03)
           MOVE IDENTIFICACAO-T1    TO GS-LINDET(173: 2)
           MOVE EMISSAO-TIT-T1      TO GS-LINDET(175: 7)
           MOVE INSTRUCAO1-T1       TO GS-LINDET(182: 3)
           MOVE INSTRUCAO2-T1       TO GS-LINDET(185: 03)
           MOVE VALOR-ATRASO-T1     TO GS-LINDET(188: 14)
           MOVE DATA-LIMITE-T1      TO GS-LINDET(202: 7)
           MOVE VALOR-DESCONTO-T1   TO GS-LINDET(209: 14)
           MOVE VALOR-IOF-T1        TO GS-LINDET(223: 14)
           MOVE VALOR-ABATIM-T1     TO GS-LINDET(237: 14)
           MOVE IDENT-INSC-T1       TO GS-LINDET(251: 03)
           MOVE NR-INSC-SAC-T1      TO GS-LINDET(254: 15)
           MOVE NOME-SACADO-T1      TO GS-LINDET(269: 41)
           MOVE ENDERECO-COMPL-T1   TO GS-LINDET(310: 41)
           MOVE MENSAGEM1-T1        TO GS-LINDET(351: 13)
           MOVE CEP-T1              TO GS-LINDET(364: 6)
           MOVE SUFIXO-CEP-T1       TO GS-LINDET(370: 4)
           MOVE MENSAGEM2-T1        TO GS-LINDET(374: 61)
           MOVE REG-REMESSA(395: 6) TO GS-LINDET(435: 6)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CABECALHO-TIPO9 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "BRANCOS"     TO GS-LINDET(1: 394)
           MOVE "SEQUEN"      TO GS-LINDET(434: 6)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LINDET-TIPO9 SECTION.
           MOVE SPACES              TO GS-LINDET.
           MOVE REG-REMESSA(395: 6) TO GS-LINDET(434: 6).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------------
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W GS-EXIT-FLG.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP9100" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CRD020 SEQREC CGD010 CGD011 CAD018.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

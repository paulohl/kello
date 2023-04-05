       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CRP9101.
       AUTHOR.        ALFREDO SAVIOLLI NETO.
      *GERA ARQUIVO XXXXXXXX.REM P/ ITAU
       DATE-WRITTEN.  01/04/2005.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
                      PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CAPX018.
           COPY CRPX020.
           COPY CGPX010.
           COPY CGPX011.
           COPY CRPX200.
           COPY CRPX201.
           COPY LOGACESS.SEL.
           SELECT SEQBAN ASSIGN TO PATH-SEQBAN
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-SEQ
                  RECORD KEY IS CONT-SEQUENCIA.
           SELECT REMESSA ASSIGN TO REMESSA-NOME
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT REMESSA2 ASSIGN TO REMESSA-NOME2
                  ORGANIZATION IS SEQUENTIAL
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
       COPY CAPW010.
       COPY CAPW018.
       COPY CRPW020.
       COPY CGPW010.
       COPY CGPW011.
       COPY CRPW200.
       COPY CRPW201.
       COPY LOGACESS.FD.
       FD  SEQBAN.
       01  REG-SEQBAN.
           05  CONT-SEQUENCIA  PIC 9.
           05  SEQUENCIA       PIC 9(10).
       FD  REMESSA
           label record is omitted.
       01  REG-REMESSA         PIC X(400).

       FD  REMESSA2
           label record is omitted.
       01  REG-REMESSA2.
           05  ID-REG-REM2       PIC 9.
           05  DADOS-REM2        PIC X(393).
           05  SEQ-REM2          PIC 9(6).
           05  PULA-REM2         PIC X(02).

       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK           PIC 9(3).
           05  NOME-WK          PIC X(40).
           05  ENDERECO-WK      PIC X(40).
           05  BAIRRO-WK        PIC X(12).
           05  CEP-WK           PIC 9(8).
           05  CIDADE-WK        PIC X(15).
           05  UF-WK            PIC XX.
           05  DOCTO-WK         PIC X(10).
           05  VALOR-WK         PIC 9(8)V99.

       FD  RELAT.
       01  REG-RELAT.
           05  FILLER          PIC X(132).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP9101.CPB".
           COPY "CRP9101.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  AREAS-DE-TRABALHO.
           05 ERRO-1              PIC XX     VALUE SPACES.
           05 ST-CAD010           PIC XX     VALUE SPACES.
           05 ST-CAD018           PIC XX     VALUE SPACES.
           05 ST-CRD020           PIC XX     VALUE SPACES.
           05 ST-CGD010           PIC XX     VALUE SPACES.
           05 ST-CGD011           PIC XX     VALUE SPACES.
           05 ST-CRD200           PIC XX     VALUE SPACES.
           05 ST-CRD201           PIC XX     VALUE SPACES.
           05 FS-LOGACESS         PIC XX     VALUE SPACES.
           05 ST-REM              PIC XX     VALUE SPACES.
           05 ST-SEQ              PIC XX     VALUE SPACES.
           05 ST-WORK             PIC XX     VALUE SPACES.
           05 VARIA-W             PIC 9(8)   VALUE ZEROS.
           05 VALOR-W             PIC 9(11)V99 VALUE ZEROS.
           05 SEQ-W               PIC 9(6)   VALUE ZEROS.
           05 OPCAO               PIC 9      VALUE ZEROS.
           05 SEQUENCIA-W         PIC 9(10)     VALUE ZEROS.
           05 TIPO-W              PIC 9      VALUE ZEROS.
           05 DATA-DIA            PIC 9(6)   VALUE ZEROS.
           05 DATA-DIA-I          PIC 9(6)   VALUE ZEROS.
           05 HORA-BRA            PIC 9(8)   VALUE ZEROS.
           05 ULT-SEQ             PIC 9(5)   VALUE ZEROS.
           05 DATA-E              PIC 99/99/99.
           05 AUX-TIPO-DOCTO      PIC X(20)  VALUE SPACES.
           05 REMESSA-NOME        PIC X(12)  VALUE SPACES.
           05 REMESSA-NOME2       PIC X(12)  VALUE SPACES.
           05 VALOR-ATRASO        PIC 9(11)V99 VALUE ZEROS.
           05 CONF                PIC X      VALUE SPACES.
           05 VALOR-TOTAL         PIC 9(12)V99 VALUE ZEROS.
           05 ERRO-W              PIC 9        VALUE ZEROS.
           05 DATA-INV            PIC 9(8)     VALUE ZEROS.
           05 VENCTO-INI-INV      PIC 9(8)     VALUE ZEROS.
           05 VENCTO-FIM-INV      PIC 9(8)     VALUE ZEROS.
           05 LIN                 PIC 9(02)    VALUE ZEROS.
           05 IND                 PIC 9(03)    VALUE ZEROS.
           05 QTDE-TIT-T2      PIC 9(8).
           05 VALOR-TOT-TIT-T2 PIC 9(14).
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
              10  AGENCIA-T0                PIC 9(4).
              10  ZEROS-T0                  PIC X(2).
              10  CONTA-T0                  PIC 9(5).
              10  VERIF-EMP-T0              PIC 9.
              10  BRANCO-T0                 PIC X(8).
              10  NOME-EMP-T0               PIC X(30).
              10  NR-BANCO-T0               PIC 9(3).
              10  NOME-BANCO-T0             PIC X(15).
              10  DDMMAA-T0                 PIC 9(6).
              10  BRANCO1-T0                PIC X(294).

           05 REM-TIPO1.
              10  COD-EMP-T1        PIC 9(2).
              10  CGC-EMP-T1        PIC 9(14).
              10  AGENC-CED-T1      PIC 9(4).
              10  ZEROS-T1          PIC 9(2).
              10  CONTA-CORR-T1     PIC 9(5).
              10  DIGITO-CONTA-T1   PIC 9.
              10  BRANCO0-T1        PIC X(4).
              10  INSTRUCAO-T1      PIC 9(4).
              10  NR-CONTR-T1       PIC X(25).
              10  NOSSO-NR-T1       PIC 9(8).
              10  QTDE-MOEDA-T1     PIC 9(13).
              10  NR-CARTEIRA-T1    PIC 9(03).
              10  BRANCO1-T1        PIC X(21).
              10  CARTEIRA-T1       PIC X.
              10  COD-OCORRENCIA-T1 PIC 9(02).
              10  NR-TITULO-T1      PIC X(10).
              10  VENCTO-TIT-T1     PIC 9(6).
              10  VALOR-TIT-T1      PIC 9(13).
              10  BANCO-COBR-T1     PIC 9(3).
              10  AGENC-DEPOS-T1    PIC 9(5).
              10  ESPECIE-TIT-T1    PIC x(2).
              10  IDENTIFICACAO-T1  PIC X.
              10  EMISSAO-TIT-T1    PIC 9(6).
              10  INSTRUCAO1-T1     PIC x(2).
              10  INSTRUCAO2-T1     PIC x(2).
              10  VALOR-ATRASO-T1   PIC 9(13).
              10  DATA-LIMITE-T1    PIC 9(6).
              10  VALOR-DESCONTO-T1 PIC 9(13).
              10  VALOR-ZEROS-T1    PIC 9(13).
              10  VALOR-ABATIM-T1   PIC 9(13).
              10  COD-INSC-T1       PIC 9(02).
              10  NR-INSC-SAC-T1    PIC 9(14).
              10  NOME-SACADO-T1    PIC X(30).
              10  BRANCO2-T1        PIC X(10).
              10  ENDERECO-COMPL-T1 PIC X(40).
              10  BAIRRO-T1         PIC X(12).
              10  CEP-T1            PIC 9(5).
              10  SUFIXO-CEP-T1     PIC 9(3).
              10  NOME-CIDADE-T1    PIC X(15).
              10  ESTADO-T1         PIC XX.
              10  BRANCO3-T1        PIC X(34).
              10  DATA-MORA-T1      PIC 9(06).
              10  PRAZO-T1          PIC 9(02).
              10  BRANCO4-T1        PIC X(01).

           05 REM-TIPO2.
      *       10  QTDE-TIT-T2      PIC 9(8).
      *       10  VALOR-TOT-TIT-T2 PIC 9(14).
              10  BRANCO-T2        PIC X(393).

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
           05  FILLER               PIC X(115) VALUE
           'RELATORIO DE REMESSA - BANESTADO'.
           05  FILLER               PIC X(09) VALUE 'EMISSAO: '.
           05  EMISSAO-REL          PIC 99/99/99.
       01  CAB02.
           05  FILLER               PIC X(132) VALUE ALL "=".
       01  CAB03.
           05  FILLER               PIC X(132) VALUE
           "NOME                           ENDERECO
      -    "    BAIRRO       CEP       CIDADE          UF DOCUMENTO
      -    "     VALOR".
       01  LINDET.
           05  NOME-REL             PIC X(30) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  ENDERECO-REL         PIC X(30) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  BAIRRO-REL           PIC X(15) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  CEP-REL              PIC ZZZZZ.
           05  FILLER               PIC X     VALUE ".".
           05  SUFIXO-REL           PIC ZZZ.
           05  FILLER               PIC X     VALUE SPACES.
           05  CIDADE-REL           PIC X(15) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  UF-REL               PIC XX    VALUE SPACES.
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

       01  STRING-1               PIC X(65) VALUE SPACES.

       PROCEDURE DIVISION.

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
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "SEQBAN" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-SEQBAN.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN INPUT CGD010 CGD011 CAD010 CAD018.
           OPEN I-O   SEQBAN

           OPEN I-O   CRD020
           CLOSE      CRD020
           OPEN INPUT CRD020

           IF ST-SEQ = "35"
              CLOSE SEQBAN    OPEN OUTPUT SEQBAN
              MOVE 1 TO CONT-SEQUENCIA
              MOVE 100000 TO SEQUENCIA
              WRITE REG-SEQBAN
              CLOSE SEQBAN   OPEN I-O SEQBAN.

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

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "CRP9101"           to logacess-programa
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
               WHEN GS-IMPRIMIR-RELATORIO-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
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

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GERA-ARQ-REMESSA SECTION.
           MOVE "AUXILIAR"           TO REMESSA-NOME
           MOVE GS-NOME-ARQ-REMESSA  TO REMESSA-NOME2.
           OPEN OUTPUT REMESSA
                       REMESSA2
           MOVE GS-VENCTO-INI     TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO VENCTO-INI-INV.
           MOVE GS-VENCTO-FIM     TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO VENCTO-FIM-INV.

           MOVE 1 TO CONT-SEQUENCIA.
           READ SEQBAN INVALID KEY
               MOVE ZEROS         TO SEQUENCIA.

           MOVE SEQUENCIA         TO SEQUENCIA-W.

           MOVE VENCTO-INI-INV    TO DATA-VENCTO-CR20.
           MOVE ZEROS             TO SITUACAO-CR20 COD-COMPL-CR20.
           START CRD020 KEY IS NOT < ALT2-CR20 INVALID KEY
                 MOVE "10" TO ST-CRD020.
           MOVE ZEROS TO VALOR-TOTAL SEQ-W.
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
                     MOVE SEQ-W         TO GS-SEQ
                     MOVE "REFRESH-DATA" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                     PERFORM MOVER-DADOS-TIPO1
                    END-IF
                  END-IF
              END-READ
           END-PERFORM.
           MOVE ZEROS TO GS-SEQ.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           PERFORM MOVER-DADOS-TIPO4.
           CLOSE REMESSA REMESSA2.
           PERFORM CARREGA-LISTA.

       ATUALIZA-PORTADOR-RECEBER SECTION.
           CLOSE    CRD020
           OPEN I-O CRD020

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
                MOVE GS-CARTEIRA(1: 1)   TO CARTEIRA-CR20
                READ CRD020 INVALID KEY CONTINUE
                  NOT INVALID KEY
                     PERFORM GRAVA-ANOTACAO
                     MOVE 29 TO PORTADOR-CR20
                     REWRITE REG-CRD020
                     END-REWRITE
                END-READ
              END-IF
            END-READ
           END-PERFORM
           CLOSE      CRD020
           OPEN INPUT CRD020
           MOVE "UNSHOW-WIN3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE REMESSA.
       ATUALIZA-SEQUENCIA SECTION.
           MOVE 1 TO CONT-SEQUENCIA.
           READ SEQBAN.
           MOVE SEQUENCIA-W       TO SEQUENCIA.
           REWRITE REG-SEQBAN.
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
           MOVE 29                  TO ANOTACAO-CR201(63: 4) PORTADOR
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
           MOVE 1 TO SEQ-W.
           INITIALIZE REM-TIPO0
           MOVE 0                        TO ID-REG-REM2.
           MOVE 1                        TO IDENT-ARQ-T0
           MOVE "REMESSA"                TO LITERAL-REMESSA-T0
           MOVE 01                       TO CODIGO-SERVICO-T0
           MOVE "COBRANCA"               TO LITERAL-SERVICO-T0
           MOVE 3899                     TO AGENCIA-T0                   -> ver com o anderson
           MOVE 00                       TO ZEROS-T0
           MOVE 07904                    TO CONTA-T0                     -> ver com o anderson
           MOVE 4                        TO VERIF-EMP-T0                 -> ver com o anderson
           MOVE SPACES                   TO BRANCO-T0
           MOVE "FERREIRA FOTO E VIDEO LTDA "
                                         TO NOME-EMP-T0
           MOVE 341                      TO NR-BANCO-T0
           MOVE "BANCO ITAU SA"          TO NOME-BANCO-T0
           MOVE DATA-DIA                 TO DDMMAA-T0
           MOVE SPACES                   TO BRANCO1-T0
           MOVE SEQ-W                    TO SEQ-REM2.
           MOVE SPACES                   TO DADOS-REM2
           MOVE REM-TIPO0                TO DADOS-REM2.
           MOVE X"0D0A"    TO  PULA-REM2
           WRITE REG-REMESSA2.
           MOVE REG-REMESSA2             TO REG-REMESSA
           WRITE REG-REMESSA
           MOVE ZEROS TO VALOR-TOTAL.

       MOVER-DADOS-TIPO1 SECTION.
           INITIALIZE REM-TIPO1
           MOVE 1                        TO ID-REG-REM2

           MOVE 02                       TO COD-EMP-T1
           MOVE 79981940000169           TO CGC-EMP-T1                   ver anderson nº cnpj
           MOVE 3899                     TO AGENC-CED-T1                 ver anderson nº agencia
           MOVE ZEROS                    TO ZEROS-T1
           MOVE 07904                    TO CONTA-CORR-T1                ver anderson nº conta
           MOVE 4                        TO DIGITO-CONTA-T1              ver anderson nº digito verificador
           MOVE SPACES                   TO BRANCO0-T1
           MOVE ZEROS                    TO INSTRUCAO-T1
      *    X - é para identificar o novo layout de contas a receber,
      *    ou seja, chave = cod-compl-cr20 + seq-cr20
           MOVE "X"                      TO NR-CONTR-T1(1: 1)
           MOVE COD-COMPL-CR20           TO NR-CONTR-T1(2: 9)
           MOVE SEQ-CR20                 TO NR-CONTR-T1(11: 05)
           MOVE SPACES                   TO NR-CONTR-T1(16: 10)
           MOVE ZEROS                    TO QTDE-MOEDA-T1
           MOVE 109                      TO NR-CARTEIRA-T1
           MOVE SPACES                   TO BRANCO1-T1
           EVALUATE GS-CARTEIRA(1:1)
             WHEN 1 MOVE "I"             TO CARTEIRA-T1
             WHEN 2 MOVE "C"             TO CARTEIRA-T1
             WHEN 3 MOVE "V"             TO CARTEIRA-T1
             WHEN OTHER MOVE " "         TO CARTEIRA-T1
           END-EVALUATE
           MOVE 01                       TO COD-OCORRENCIA-T1
           ADD 1 TO SEQUENCIA-W
           MOVE SEQUENCIA-W              TO NOSSO-NR-T1
           MOVE NR-DOCTO-CR20            TO NR-TITULO-T1(1: 10)
           MOVE DATA-VENCTO-CR20 TO DATA-INV.
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV(1: 4)           TO VENCTO-TIT-T1(1: 4)
           MOVE DATA-INV(7: 2)           TO VENCTO-TIT-T1(5: 2)
           MOVE VALOR-TOT-CR20           TO VALOR-W
           ADD VALOR-W                   TO VALOR-TOTAL
           MOVE VALOR-W(1: 11)           TO VALOR-TIT-T1(1: 11)
           MOVE VALOR-W(12: 2)           TO VALOR-TIT-T1(12: 2)
           MOVE 341                      TO BANCO-COBR-T1
           MOVE ZEROS                    TO AGENC-DEPOS-T1
           MOVE 06                       TO ESPECIE-TIT-T1               ver com anderson => nota 10
           MOVE "N"                      TO IDENTIFICACAO-T1
           MOVE DATA-EMISSAO-CR20(1: 4)  TO EMISSAO-TIT-T1(1: 4)
           MOVE DATA-EMISSAO-CR20(7: 2)  TO EMISSAO-TIT-T1(5: 2)
           MOVE 06                       TO INSTRUCAO1-T1                ver com anderson => nota 11
           MOVE ZEROS                    TO INSTRUCAO2-T1                ver com anderson => nota 11

           COMPUTE VALOR-ATRASO = (VALOR-TOT-CR20 * 0,05) / 30
           MOVE VALOR-ATRASO(1: 11)      TO VALOR-ATRASO-T1(1: 11)
           MOVE VALOR-ATRASO(12: 2)      TO VALOR-ATRASO-T1(12: 2)

           MOVE ZEROS                    TO DATA-LIMITE-T1
           MOVE ZEROS                    TO VALOR-DESCONTO-T1
           MOVE ZEROS                    TO VALOR-ZEROS-T1
           MOVE ZEROS                    TO VALOR-ABATIM-T1

           MOVE COD-COMPL-CR20     TO COD-COMPL-CG10 COD-COMPL-CG11.
           READ CGD010 INVALID KEY
                INITIALIZE REG-CGD010.

           READ CGD011 INVALID KEY
                INITIALIZE REG-CGD011.

           MOVE 01               TO COD-INSC-T1
           MOVE CPF1-CG11(3: 14) TO NR-INSC-SAC-T1.
           MOVE COMPRADOR-CG10   TO NOME-SACADO-T1
           MOVE SPACES           TO BRANCO2-T1
           MOVE ENDERECO1-CG11   TO ENDERECO-COMPL-T1.
           MOVE BAIRRO1-CG11     TO BAIRRO-T1.
           MOVE CEP1-CG11(1: 5)  TO CEP-T1.
           MOVE CEP1-CG11(6: 3)  TO SUFIXO-CEP-T1.

           MOVE CIDADE1-CG11     TO CIDADE.
           READ CAD010 INVALID KEY
                MOVE SPACES TO NOME-CID UF-CID.
           MOVE NOME-CID        TO NOME-CIDADE-T1
           MOVE UF-CID          TO ESTADO-T1
           MOVE SPACES          TO BRANCO3-T1
           MOVE DATA-EMISSAO-CR20(1: 4) TO DATA-MORA-T1(1: 4)
           MOVE DATA-EMISSAO-CR20(7: 2) TO DATA-MORA-T1(5: 2)
           MOVE 15              TO PRAZO-T1
           MOVE SPACES          TO BRANCO4-T1

           ADD 1  TO SEQ-W.
           MOVE SEQ-W                    TO SEQ-REM2.
           MOVE REM-TIPO1 TO DADOS-REM2.
           MOVE X"0D0A"    TO  PULA-REM2
           WRITE REG-REMESSA2
           MOVE REG-REMESSA2 TO REG-REMESSA.
           WRITE REG-REMESSA.

       MOVER-DADOS-TIPO4 SECTION.
           INITIALIZE REM-TIPO2
           MOVE 9                        TO ID-REG-REM2.
           MOVE ZEROS                    TO DADOS-REM2.
           MOVE VALOR-TOTAL(1: 12)       TO VALOR-TOT-TIT-T2(1: 12)
           MOVE VALOR-TOTAL(13: 2)       TO VALOR-TOT-TIT-T2(13: 2)
           MOVE SPACES                   TO BRANCO-T2
           ADD 1 TO SEQ-W
           MOVE SEQ-W                    TO SEQ-REM2.
           SUBTRACT 2 FROM SEQ-W GIVING QTDE-TIT-T2
           MOVE REM-TIPO2                TO DADOS-REM2.
           MOVE X"0D0A"    TO  PULA-REM2
           WRITE REG-REMESSA2
           MOVE REG-REMESSA2 TO REG-REMESSA
           WRITE REG-REMESSA.

       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           OPEN INPUT REMESSA.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO ST-REM.
           PERFORM UNTIL ST-REM = "10"
                READ REMESSA AT END
                     MOVE "10" TO ST-REM
                NOT AT END
                     MOVE REG-REMESSA(1: 1) TO TIPO-W
                     EVALUATE TIPO-W
                        WHEN 0 PERFORM CABECALHO-TIPO0
                               PERFORM LINDET-TIPO0
                               PERFORM CABECALHO-TIPO1
                        WHEN 1 PERFORM LINDET-TIPO1
                        WHEN 9 PERFORM CABECALHO-TIPO9
                               PERFORM LINDET-TIPO9
                    END-EVALUATE
                END-READ
           END-PERFORM.
           MOVE VALOR-TOTAL        TO GS-VALOR-TOTAL
           MOVE QTDE-TIT-T2        TO GS-QTDE-TITULO
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
             READ REMESSA AT END
                  MOVE "10" TO ST-REM
             NOT AT END
                  MOVE REG-REMESSA(1: 1) TO TIPO-W
                  IF TIPO-W <> 1
                     CONTINUE
                  ELSE
                     MOVE REG-REMESSA(2: 393) TO REM-TIPO1
                     ADD 1 TO SEQ-WK
                     MOVE NOME-SACADO-T1      TO NOME-WK
                     MOVE ENDERECO-COMPL-T1   TO ENDERECO-WK
                     MOVE BAIRRO-T1           TO BAIRRO-WK
                     MOVE NOME-CIDADE-T1      TO CIDADE-WK
                     MOVE ESTADO-T1           TO UF-WK
                     MOVE CEP-T1              TO CEP-WK(1: 5)
                     MOVE SUFIXO-CEP-T1       TO CEP-WK(6: 3)
                     MOVE NR-TITULO-T1        TO DOCTO-WK
                     MOVE VALOR-TIT-T1(4: 8)  TO VALOR-WK(1: 8)
                     MOVE VALOR-TIT-T1(12: 2) TO VALOR-WK(9: 2)
                     WRITE REG-WORK
                     END-WRITE
                  END-IF
             END-READ
           END-PERFORM.

           COPY CONDENSA.

           CLOSE WORK. OPEN INPUT WORK.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO NOME-WK.
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD AT END
                  MOVE "10" TO ST-WORK
             NOT AT END
                  MOVE NOME-WK           TO NOME-REL
                  MOVE ENDERECO-WK       TO ENDERECO-REL
                  MOVE BAIRRO-WK         TO BAIRRO-REL
                  MOVE CIDADE-WK         TO CIDADE-REL
                  MOVE UF-WK             TO UF-REL
                  MOVE CEP-WK(1: 5)      TO CEP-REL
                  MOVE CEP-WK(6: 3)      TO SUFIXO-REL
                  MOVE DOCTO-WK          TO DOCTO-REL
                  MOVE VALOR-WK          TO VALOR-REL
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 56
                     PERFORM CABECALHO
                  END-IF
             END-READ
           END-PERFORM.
           MOVE VALOR-TOTAL        TO VALOR-TOTAL-REL.
           MOVE QTDE-TIT-T2        TO QTDE-TIT-TOTAL-REL.
           WRITE REG-RELAT FROM LINDET1 AFTER 3.
           CLOSE REMESSA WORK.
           DELETE FILE WORK.

           COPY DESCONDENSA.
       LER-PORTADOR SECTION.
           MOVE GS-PORTADOR TO PORTADOR.
           READ CAD018 INVALID KEY MOVE "*******"  TO NOME-PORT.
           MOVE NOME-PORT TO GS-DESCR-PORTADOR.
       POPUP-PORTADOR SECTION.
           CALL "CAP018T" USING PARAMETROS-W STRING-1
           CANCEL "CAP018T"
           MOVE STRING-1(1: 30) TO GS-DESCR-PORTADOR
           MOVE STRING-1(33: 4) TO GS-PORTADOR.
      *--------------------------------------------------------------
       CABECALHO-TIPO0 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "I"                 TO GS-LINDET(1: 2)
           MOVE "LIT.REM"           TO GS-LINDET(3: 8)
           MOVE "CS"                TO GS-LINDET(11: 3)
           MOVE "LIT-SERVICO"       TO GS-LINDET(14: 16)
           MOVE "BRANCO"            TO GS-LINDET(30: 08)
           MOVE "AGENC"             TO GS-LINDET(38: 06)
           MOVE "CONTA"             TO GS-LINDET(44: 08)
           MOVE "D"                 TO GS-LINDET(52: 02)
           MOVE "NOME-EMPRESA"      TO GS-LINDET(54: 31)
           MOVE "BCO"               TO GS-LINDET(85: 4)
           MOVE "NOME-BANCO"        TO GS-LINDET(89: 16)
           MOVE "DDMMAA"            TO GS-LINDET(105: 7)
           MOVE "BRANCO"            TO GS-LINDET(112: 297)
           MOVE "SEQUEN"            TO GS-LINDET(409: 6)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LINDET-TIPO0 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE REG-REMESSA(2: 393) TO REM-TIPO0.
           MOVE IDENT-ARQ-T0        TO GS-LINDET(1: 2)
           MOVE LITERAL-REMESSA-T0  TO GS-LINDET(3: 8)
           MOVE CODIGO-SERVICO-T0   TO GS-LINDET(11: 3)
           MOVE LITERAL-SERVICO-T0  TO GS-LINDET(14: 16)
           MOVE BRANCO-T0           TO GS-LINDET(30: 08)
           MOVE AGENCIA-T0          TO GS-LINDET(38: 06)
           MOVE CONTA-T0            TO GS-LINDET(44: 08)
           MOVE VERIF-EMP-T0        TO GS-LINDET(52: 02)
           MOVE NOME-EMP-T0         TO GS-LINDET(54: 31)
           MOVE NR-BANCO-T0         TO GS-LINDET(85: 4)
           MOVE NOME-BANCO-T0       TO GS-LINDET(89: 16)
           MOVE DDMMAA-T0           TO GS-LINDET(105: 7)
           MOVE BRANCO1-T0          TO GS-LINDET(122: 287)
           MOVE REG-REMESSA(395: 6) TO GS-LINDET(409: 6)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CABECALHO-TIPO1 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "CD"                TO GS-LINDET(1: 3)
           MOVE "CGC"               TO GS-LINDET(4: 15)
           MOVE "AGEN"              TO GS-LINDET(19: 06)
           MOVE "ZE"                TO GS-LINDET(25:03)
           MOVE "CTA.COR"           TO GS-LINDET(28: 08)
           MOVE "D"                 TO GS-LINDET(36: 02)
           MOVE "BRAN"              TO GS-LINDET(38: 05)
           MOVE "ALEG"              TO GS-LINDET(43: 05)
           MOVE "NR-CONTROLE"       TO GS-LINDET(48: 26)
           MOVE "NOSSO-NR"          TO GS-LINDET(69: 11)
           MOVE "QTDE-MOEDA"        TO GS-LINDET(80: 14)
           MOVE "CAR"               TO GS-LINDET(94: 04)
           MOVE "USO BANCO"         TO GS-LINDET(98:22)
           MOVE "I"                 TO GS-LINDET(120:2)
           MOVE "OC"                TO GS-LINDET(122: 03)
           MOVE "NR-DOCTO"          TO GS-LINDET(125: 11)
           MOVE "VENCTO"            TO GS-LINDET(136: 07)
           MOVE "        VALOR"     TO GS-LINDET(143: 14)
           MOVE "BCO"               TO GS-LINDET(157: 04)
           MOVE "AGENC"             TO GS-LINDET(161: 06)
           MOVE "ES"                TO GS-LINDET(167: 03)
           MOVE "A"                 TO GS-LINDET(170: 2)
           MOVE "EMISS."            TO GS-LINDET(172: 7)
           MOVE "I1"                TO GS-LINDET(179: 3)
           MOVE "I2"                TO GS-LINDET(182: 03)

           MOVE " VALOR-ATRASO"     TO GS-LINDET(185: 14)
           MOVE "DT-LIM"            TO GS-LINDET(199: 7)
           MOVE " VALOR-DESCON"     TO GS-LINDET(206: 14)
           MOVE "    VALOR-IOF"     TO GS-LINDET(220: 14)
           MOVE " VALOR-ABATIM"     TO GS-LINDET(234: 14)
           MOVE "II"                TO GS-LINDET(248: 03)
           MOVE "NR.INSCRICAO"      TO GS-LINDET(252: 15)
           MOVE "NOME SACADO"       TO GS-LINDET(267: 41)
           MOVE "ENDERECO"          TO GS-LINDET(308: 41)
           MOVE "BAIRRO"            TO GS-LINDET(349: 13)
           MOVE "CEP"               TO GS-LINDET(362: 6)
           MOVE "SUF"               TO GS-LINDET(368: 4)
           MOVE "NOME-CIDADE"       TO GS-LINDET(372: 16)
           MOVE "UF"                TO GS-LINDET(388: 03)
           MOVE "DT-MORA"           TO GS-LINDET(391: 08)
           MOVE "PRAZO"             TO GS-LINDET(399: 06)
           MOVE "SEQUEN"            TO GS-LINDET(405: 6)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       LINDET-TIPO1 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE REG-REMESSA(2: 393) TO REM-TIPO1.
           MOVE COD-EMP-T1          TO GS-LINDET(1: 3)
           MOVE CGC-EMP-T1          TO GS-LINDET(4: 15)
           MOVE AGENC-CED-T1        TO GS-LINDET(19: 06)
           MOVE ZEROS-T1            TO GS-LINDET(25: 02)
           MOVE CONTA-CORR-T1       TO GS-LINDET(28: 08)
           MOVE DIGITO-CONTA-T1     TO GS-LINDET(36: 02)
           MOVE BRANCO0-T1          TO GS-LINDET(38: 04)
           MOVE INSTRUCAO-T1        TO GS-LINDET(43: 04)
           MOVE NR-CONTR-T1         TO GS-LINDET(48: 26)
           MOVE NOSSO-NR-T1         TO GS-LINDET(69: 11)
           MOVE QTDE-MOEDA-T1       TO GS-LINDET(80: 14)
           MOVE NR-CARTEIRA-T1      TO GS-LINDET(94: 04)
           MOVE BRANCO1-T1          TO GS-LINDET(98:22)
           MOVE CARTEIRA-T1         TO GS-LINDET(120:2)
           MOVE COD-OCORRENCIA-T1   TO GS-LINDET(122: 03)
           MOVE NR-TITULO-T1        TO GS-LINDET(125: 11)
           MOVE VENCTO-TIT-T1       TO GS-LINDET(136: 07)
           MOVE VALOR-TIT-T1        TO GS-LINDET(143: 14)
           MOVE BANCO-COBR-T1       TO GS-LINDET(157: 04)
           MOVE AGENC-DEPOS-T1      TO GS-LINDET(161: 06)
           MOVE ESPECIE-TIT-T1      TO GS-LINDET(167: 03)
           MOVE IDENTIFICACAO-T1    TO GS-LINDET(170: 02)
           MOVE EMISSAO-TIT-T1      TO GS-LINDET(172: 07)
           MOVE INSTRUCAO1-T1       TO GS-LINDET(179: 03)
           MOVE INSTRUCAO2-T1       TO GS-LINDET(182: 03)
           MOVE VALOR-ATRASO-T1     TO GS-LINDET(185: 14)
           MOVE DATA-LIMITE-T1      TO GS-LINDET(199: 7)
           MOVE VALOR-DESCONTO-T1   TO GS-LINDET(206: 14)
           MOVE VALOR-ZEROS-T1      TO GS-LINDET(220: 14)
           MOVE VALOR-ABATIM-T1     TO GS-LINDET(234: 14)
           MOVE COD-INSC-T1         TO GS-LINDET(248: 03)
           MOVE NR-INSC-SAC-T1      TO GS-LINDET(252: 15)
           MOVE NOME-SACADO-T1      TO GS-LINDET(267: 41)
           MOVE ENDERECO-COMPL-T1   TO GS-LINDET(308: 41)
           MOVE BAIRRO-T1           TO GS-LINDET(349: 13)
           MOVE CEP-T1              TO GS-LINDET(362: 6)
           MOVE SUFIXO-CEP-T1       TO GS-LINDET(368: 4)
           MOVE NOME-CIDADE-T1      TO GS-LINDET(372: 16)
           MOVE ESTADO-T1           TO GS-LINDET(388: 03)
           MOVE DATA-MORA-T1        TO GS-LINDET(391: 11)
           MOVE PRAZO-T1            TO GS-LINDET(402: 02)
           MOVE REG-REMESSA(395: 6) TO GS-LINDET(405: 6)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CABECALHO-TIPO9 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "QT-TIT"      TO GS-LINDET(1: 9)
           MOVE "   VALOR-TOTAL" TO GS-LINDET(10: 15)
           MOVE "BRANCOS"     TO GS-LINDET(25: 372)
           MOVE "SEQUEN"      TO GS-LINDET(397: 6)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LINDET-TIPO9 SECTION.
           MOVE SPACES              TO GS-LINDET.
           MOVE REG-REMESSA(2: 393) TO REM-TIPO2.
           MOVE QTDE-TIT-T2         TO GS-LINDET(1: 9)
           MOVE VALOR-TOT-TIT-T2    TO GS-LINDET(10: 15)
           MOVE BRANCO-T2           TO GS-LINDET(25: 372)
           MOVE REG-REMESSA(395: 6) TO GS-LINDET(397: 6).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *---------------------------------------------------
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
           MOVE "CRP9101" TO DS-SET-NAME
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
           move "CRP9101"           to logacess-programa
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

           CLOSE CRD020 SEQBAN CGD010 CGD011 CAD010 CAD018.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

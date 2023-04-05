       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CRP9102A.
       AUTHOR.        ALFREDO SAVIOLLI NETO.
      *GERA ARQUIVO REMESSA P/ BANCO DO BRASIL
       DATE-WRITTEN.  18/06/05.
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
           COPY CGPX014.
           COPY CRPX200.
           COPY CRPX201.
           COPY RETPORT.SEL.
           COPY LOGACESS.SEL.
           SELECT SEQBRAS ASSIGN TO PATH-SEQBRA
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
       COPY CGPW014.
       COPY CRPW200.
       COPY CRPW201.
       COPY RETPORT.FD.
       COPY LOGACESS.FD.
       FD  SEQBRAS.
       01  REG-SEQBRAS.
           05  CONT-SEQUENCIA  PIC 9.
           05  SEQUENCIA       PIC 9(10).
       FD  REMESSA.
       01  REG-REMESSA.
           05  ID-REG-REM       PIC X(02).
           05  DADOS-REM        PIC X(498).
       FD  REMESSA2
           label record is omitted.
       01  REG-REMESSA2.
           05  ID-REG-REM2      PIC X(02).
           05  DADOS-REM2       PIC X(498).
           05  pula-rem2        pic x(02).

       FD  WORK.
       01  REG-WORK.
           05  SEQ-WK           PIC 9(3).
           05  NOME-WK          PIC X(35).
           05  ENDERECO-WK      PIC X(35).
           05  CEP-WK           PIC 9(8).
           05  CIDADE-WK        PIC X(15).
           05  UF-WK            PIC XX.
           05  DOCTO-WK         PIC X(10).
           05  VALOR-WK         PIC 9(8)V99.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER          PIC X(132).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP9102A.CPB".
           COPY "CRP9102A.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  AREAS-DE-TRABALHO.
           05 ERRO-1              PIC XX     VALUE SPACES.
           05 ST-CAD010           PIC XX     VALUE SPACES.
           05 ST-CAD018           PIC XX     VALUE SPACES.
           05 ST-CRD020           PIC XX     VALUE SPACES.
           05 ST-REM              PIC XX     VALUE SPACES.
           05 ST-CGD010           PIC XX     VALUE SPACES.
           05 ST-CGD011           PIC XX     VALUE SPACES.
           05 ST-CGD014           PIC XX     VALUE SPACES.
           05 ST-CRD200           PIC XX     VALUE SPACES.
           05 ST-CRD201           PIC XX     VALUE SPACES.
           05 FS-RETPORT          PIC XX     VALUE SPACES.
           05 FS-LOGACESS         PIC XX     VALUE SPACES.
           05 ST-SEQ              PIC XX     VALUE SPACES.
           05 ST-WORK             PIC XX     VALUE SPACES.
           05 VARIA-W             PIC 9(8)   VALUE ZEROS.
           05 VALOR-W             PIC 9(11)V99 VALUE ZEROS.
           05 TIPO-W              PIC 99     VALUE ZEROS.
           05 SEQ-W               PIC 9(6)   VALUE ZEROS.
           05 OPCAO               PIC 9      VALUE ZEROS.
           05 DATA-DIA            PIC 9(6)   VALUE ZEROS.
           05 DATA-DIA-I          PIC 9(8)   VALUE ZEROS.
           05 HORA-BRA            PIC 9(8)   VALUE ZEROS.
           05 ULT-SEQ             PIC 9(5)   VALUE ZEROS.
           05 DATA-E             PIC 99/99/99.
           05 REMESSA-NOME        PIC X(12)  VALUE SPACES.
           05 SEQUENCIA-W         PIC 9(10)     VALUE ZEROS.
           05 VALOR-ATRASO        PIC 9(11)V99 VALUE ZEROS.
           05 COD-COMPL-CR20-W    PIC 9(09)  VALUE ZEROS.
           05 CONF                PIC X      VALUE SPACES.
           05 VALOR-TOTAL         PIC 9(12)V99 VALUE ZEROS.
           05 QTDE-TIT            PIC 9(4)     VALUE ZEROS.
           05 ERRO-W              PIC 9        VALUE ZEROS.
           05 DATA-INV            PIC 9(8)     VALUE ZEROS.
           05 VENCTO-INI-INV      PIC 9(8)     VALUE ZEROS.
           05 VENCTO-FIM-INV      PIC 9(8)     VALUE ZEROS.
           05 MOVTO-INI-INV       PIC 9(8)     VALUE ZEROS.
           05 MOVTO-FIM-INV       PIC 9(8)     VALUE ZEROS.
           05 TAXA-JUROS          PIC 9(03)V99 VALUE ZEROS.
           05 LIN                 PIC 9(02)    VALUE ZEROS.
           05 IND                 PIC 9(02)    VALUE ZEROS.
           05 TOTAL               PIC 9(4)     VALUE ZEROS.
           05 VALOR               PIC 9(4)     VALUE ZEROS.
           05 RESTO               PIC 9(2)     VALUE ZEROS.
           05 AUX-TIPO            PIC 9(1)     VALUE ZEROS.
           05 AUX-TIPO-DOCTO      PIC X(20)    VALUE SPACES.
           05 MENSAGEM            PIC X(200)    VALUE SPACES.
           05 TIPO-MSG            PIC X(01).
           05 RESP-MSG            PIC X(01).
           05 DIGITO-VERIFICADOR  PIC X(1)     VALUE SPACES.
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
              10  TIPO-INSC-T0              PIC X(02) VALUE SPACES.
              10  INSC-T0                   PIC X(14) VALUE SPACES.
              10  NOME-EMP-T0               PIC X(30) VALUE SPACES.
              10  TIPO-ARQUIVO-T0           PIC X(06)  VALUE SPACES.
              10  BRANCO1-T0                PIC X(446) VALUE SPACE.
           05 REM-TIPO1.
              10  CONVENIO-T1               PIC 9(06) VALUE ZEROS.
              10  CARTEIRA-T1               PIC 9(02) VALUE ZEROS.
              10  VARIACAO-T1               PIC 9(03) VALUE ZEROS.
              10  SEU-NUMERO-T1             PIC X(10) VALUE SPACES.
              10  NOSSO-NUMERO-T1           PIC X(20) VALUE SPACES.
              10  CONTROLE-T1               PIC X(25) VALUE SPACES.
              10  SIGLA-ESPECIE-T1          PIC X(05) VALUE SPACES.
              10  DATA-EMISSAO-T1           PIC 9(08) VALUE ZEROS.
              10  DATA-VENCTO-T1            PIC 9(08) VALUE ZEROS.
              10  VALOR-TITULO-T1           PIC 9(13) VALUE ZEROS.
              10  CODIGO-MOEDA-T1           PIC X(05) VALUE SPACES.
              10  QUANTIDADE-MOEDA-T1       PIC 9(13) VALUE ZEROS.
              10  ACEITE-T1                 PIC X(01) VALUE SPACES.
              10  VALOR-JUROS-T1            PIC 9(13) VALUE ZEROS.
              10  DATA-LIMITE-DESC-T1       PIC 9(08) VALUE ZEROS.
              10  VALOR-DESCONTO-T1         PIC 9(13) VALUE ZEROS.
              10  VALOR-ABATIMENTO-T1       PIC 9(13) VALUE ZEROS.
              10  QUANTIDADE-DIA-PRO-T1     PIC 9(02) VALUE ZEROS.
              10  MENSAGEM-T1               PIC X(40) VALUE SPACES.
              10  CONVENIO7-POS-T1          PIC 9(09) VALUE ZEROS.
              10  CODIGO-MULTA-T1           PIC 9(01) VALUE ZEROS.
              10  DATA-MULTA-T1             PIC 9(08) VALUE ZEROS.
              10  VALOR-MULTA-T1            PIC 9(13) VALUE ZEROS.
              10  FILLER-T1                 PIC X(09) VALUE SPACES.
              10  TIPO-INSCRICAO-AVAL-T1    PIC X(02) VALUE SPACES.
              10  INSCRICAO-AVALISTA-T1     PIC X(14) VALUE SPACES.
              10  NOME-AVALISTA-T1          PIC X(37) VALUE SPACES.
              10  PENDENTE-IMPRESSAO-T1     PIC X(01) VALUE SPACES.
              10  TIPO-INSCRICAO-CLI-T1     PIC 9(02) VALUE ZEROS.
              10  INSCRICAO-CLIENTE-T1      PIC X(14) VALUE SPACES.
              10  NOME-CLIENTE-T1           PIC X(37) VALUE SPACES.
              10  ENDERECO-CLIENTE-T1       PIC X(37) VALUE SPACES.
              10  CEP-CLIENTE-T1            PIC 9(08) VALUE ZEROS.
              10  CIDADE-CLIENTE-T1         PIC X(15) VALUE SPACES.
              10  UF-CLIENTE-T1             PIC X(02) VALUE SPACES.
              10  ESTADO-DESCRICAO-T1       PIC X(40) VALUE SPACES.
              10  DATA-PAGAMENTO-T1         PIC 9(08) VALUE ZEROS.
              10  VALOR-PAGO-T1             PIC 9(13) VALUE ZEROS.
              10  TIPO-MODALIDADE-T1        PIC 9(02) VALUE ZEROS.
              10  ESTADO-DO-TITULO-T1       PIC 9(02) VALUE ZEROS.
              10  USO-DO-BANCO-T1           PIC X(16) VALUE SPACES.
           05 REM-TIPO2.
              10 QUANTIDADE-T2     PIC 9(06)  VALUE ZEROS.
              10 BRANCO-T2         PIC X(492) VALUE SPACES.

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

       01  MASC-DIA                 PIC Z9.


       01  CAB01.
           05  FILLER               PIC X(115) VALUE
           'RELATORIO DE REMESSA - BANCO DO BRASIL'.
           05  FILLER               PIC X(09) VALUE 'EMISSAO: '.
           05  EMISSAO-REL          PIC 99/99/99.
       01  CAB02.
           05  FILLER               PIC X(132) VALUE ALL "=".
       01  CAB03.
           05  FILLER               PIC X(132) VALUE
           "NOME                                ENDERECO
      -    "            CEP       CIDADE          UF DOCUMENTO
      -    " VALOR".
       01  LINDET.
           05  NOME-REL             PIC X(35) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  ENDERECO-REL         PIC X(35) VALUE SPACES.
           05  FILLER               PIC X     VALUE SPACES.
           05  CEP-REL              PIC ZZZZZ.ZZZ.
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

       01  TAB-SEQUENCIA            PIC 9(11).
       01  FILLER REDEFINES TAB-SEQUENCIA OCCURS 11 TIMES.
           05 TAB-SEQ               PIC 9(01).

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

       01  STRING-1                    PIC X(65) VALUE SPACES.

       77 POP-UP                       PIC X(65).


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
           MOVE "RETPORT" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RETPORT
           MOVE "CGD010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CGD014"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD014.
           MOVE "CAD010"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD018"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD200"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "SEQBRA9" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-SEQBRA.
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN INPUT CGD010 CGD011 CGD014 CAD010 CAD018.
           OPEN I-O   CRD020 RETPORT
           CLOSE      CRD020 RETPORT
           OPEN INPUT CRD020 RETPORT

           OPEN I-O SEQBRAS.
           IF ST-SEQ = "35"
              CLOSE SEQBRAS    OPEN OUTPUT SEQBRAS
              CLOSE SEQBRAS
              OPEN I-O SEQBRAS
              MOVE 1 TO CONT-SEQUENCIA
              MOVE ZEROS TO SEQUENCIA
              WRITE REG-SEQBRAS
              CLOSE SEQBRAS   OPEN I-O SEQBRAS.

           CLOSE SEQBRAS
           OPEN INPUT SEQBRAS

           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF FS-RETPORT <> "00"
              MOVE "ERRO ABERTURA RETPORT: "  TO GS-MENSAGEM-ERRO
              MOVE FS-RETPORT TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD014 <> "00"
              MOVE "ERRO ABERTURA CGD014: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD014 TO GS-MENSAGEM-ERRO(23: 02)
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
           move "CRP9102A"          to logacess-programa
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
                    PERFORM CARREGAR-PORTADOR
                    IF GS-ACP-PORTADOR NOT > ZEROS
                       MOVE "Portador para Transferência Não Parametriza
      -                     "do" TO MENSAGEM
                       MOVE "C"  TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                    END-IF
               WHEN GS-IMPRIMIR-RELATORIO-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-GERAR-REMESSA-TRUE
                    MOVE SPACES TO MENSAGEM
                    PERFORM CARREGAR-PORTADOR
                    IF GS-ACP-PORTADOR NOT > ZEROS
                       MOVE "Portador para Transferência Não Parametriza
      -                     "do" TO MENSAGEM
                       MOVE "C"  TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                    ELSE
                       PERFORM GERA-ARQ-REMESSA
                    END-IF
               WHEN GS-LE-PORTADOR-TRUE
                    PERFORM LER-PORTADOR
               WHEN GS-POPUP-PORTADOR-TRUE
                    PERFORM POPUP-PORTADOR
               WHEN GS-ATUALIZA-PORTADOR-TRUE
                    PERFORM ATUALIZA-PORTADOR-RECEBER
               WHEN GS-ATUALIZ-SEQUENCIA-TRUE
                    PERFORM ATUALIZA-SEQUENCIA
               WHEN GS-LER-PORTADOR-TRUE
                    PERFORM LE-PORTADOR
               WHEN GS-POP-PORTADOR-TRUE
                    PERFORM POP-PORTADOR
               WHEN GS-CARREGA-PORTADOR-TRUE
                    PERFORM CARREGAR-PORTADOR
               WHEN GS-GRAVA-PORTADOR-TRUE
                    PERFORM GRAVAR-PORTADOR
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       LE-PORTADOR SECTION.
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
           MOVE "CRP9102A"             TO RETPORT-PROGRAMA
           READ RETPORT INVALID KEY
                INITIALIZE REG-RETPORT.

           MOVE RETPORT-PORTADOR       TO GS-ACP-PORTADOR
                                          PORTADOR
           READ CAD018 INVALID KEY
                MOVE "***********"     TO NOME-PORT
           END-READ
           MOVE NOME-PORT              TO GS-DESC-PORTADOR.

       GRAVAR-PORTADOR SECTION.
           CLOSE      RETPORT
           OPEN I-O   RETPORT
           MOVE "CRP9102A"             TO RETPORT-PROGRAMA
           MOVE GS-ACP-PORTADOR        TO RETPORT-PORTADOR
           WRITE REG-RETPORT INVALID KEY
                 REWRITE REG-RETPORT INVALID KEY
                      MOVE "Erro de Gravação...RETPORT" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM.
           CLOSE      RETPORT
           OPEN INPUT RETPORT.

       CENTRALIZAR SECTION.
          move-object-handle win4 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       GERA-ARQ-REMESSA SECTION.
           MOVE GS-TIPO          TO AUX-TIPO-DOCTO
           STRING GS-TIPO(1:1) INTO AUX-TIPO

           MOVE "AUXILIAR"       TO REMESSA-NOME
           MOVE GS-NOME-ARQ-REMESSA  TO REMESSA-NOME2.
           OPEN OUTPUT REMESSA REMESSA2.


           MOVE GS-VENCTO-INI     TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO VENCTO-INI-INV.
           MOVE GS-VENCTO-FIM     TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO VENCTO-FIM-INV.

           MOVE GS-MOVTO-INI      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV
           MOVE DATA-INV          TO MOVTO-INI-INV.
           MOVE GS-MOVTO-FIM      TO DATA-INV.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV          TO MOVTO-FIM-INV.


           MOVE 1 TO CONT-SEQUENCIA.
           READ SEQBRAS.
           MOVE SEQUENCIA         TO SEQUENCIA-W.

           INITIALIZE REG-CRD020

           MOVE ZEROS TO VALOR-TOTAL SEQ-W.
           PERFORM MOVER-DADOS-TIPO0.

           IF GS-ALBUM > 0
              STRING "0" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CR20
              MOVE COD-COMPL-CR20 TO COD-COMPL-CR20-W
              START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
                   MOVE "10" TO ST-CRD020
              END-START

              PERFORM UNTIL ST-CRD020 = "10"
                   READ CRD020 NEXT WITH IGNORE LOCK AT END
                       MOVE "10" TO ST-CRD020
                   NOT AT END
                       IF COD-COMPL-CR20 <> COD-COMPL-CR20-W
                          MOVE "10" TO ST-CRD020
                       ELSE
                           IF SITUACAO-CR20 = 0
                              IF MOVTO-INI-INV = 0 OR
                                 (DATA-MOVTO-CR20 NOT < MOVTO-INI-INV
                                                   AND
                                 DATA-MOVTO-CR20 NOT > MOVTO-FIM-INV)
                                 IF VENCTO-INI-INV = 0 OR
                                    (DATA-VENCTO-CR20 NOT <
                                     VENCTO-INI-INV AND
                                     DATA-VENCTO-CR20 NOT >
                                     VENCTO-FIM-INV)
                                    PERFORM FAZER-OUTRAS-COMPARACOES
                                 END-IF
                              END-IF
                           END-IF
                       END-IF
                   END-READ
              END-PERFORM
           ELSE
              IF GS-CONTRATO > 0
                 STRING "0" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CR20
                 START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
                      MOVE "10" TO ST-CRD020
                 END-START
                 PERFORM UNTIL ST-CRD020 = "10"
                     READ CRD020 NEXT WITH IGNORE LOCK AT END
                          MOVE "10" TO ST-CRD020
                     NOT AT END
                          IF CLIENTE-CR20(1:4) <> GS-CONTRATO
                             MOVE "10" TO ST-CRD020
                          ELSE
                             IF SITUACAO-CR20 = 0
                                IF MOVTO-INI-INV = 0 OR
                                   (DATA-MOVTO-CR20 NOT < MOVTO-INI-INV
                                                    AND
                                    DATA-MOVTO-CR20 NOT > MOVTO-FIM-INV)
                                    IF VENCTO-INI-INV = 0 OR
                                       (DATA-VENCTO-CR20 NOT <
                                        VENCTO-INI-INV AND
                                        DATA-VENCTO-CR20 NOT >
                                        VENCTO-FIM-INV)
                                        PERFORM FAZER-OUTRAS-COMPARACOES
                                     END-IF
                                 END-IF
                             END-IF
                          END-IF
                     END-READ
                 END-PERFORM
              ELSE
                 IF GS-MOVTO-INI > 0
                    MOVE ZEROS TO SITUACAO-CR20
                    MOVE MOVTO-INI-INV TO DATA-MOVTO-CR20
                    START CRD020 KEY IS NOT LESS ALT3-CR20 INVALID KEY
                          MOVE "10" TO ST-CRD020
                    END-START

                    PERFORM UNTIL ST-CRD020 = "10"
                        READ CRD020 NEXT WITH IGNORE LOCK AT END
                             MOVE "10" TO ST-CRD020
                        NOT AT END
                             IF DATA-MOVTO-CR20 > MOVTO-FIM-INV OR
                                SITUACAO-CR20 <> 0
                                MOVE "10" TO ST-CRD020
                             ELSE
                                IF VENCTO-INI-INV = 0 OR
                                   (DATA-VENCTO-CR20 NOT <
                                    VENCTO-INI-INV AND
                                    DATA-VENCTO-CR20 NOT >
                                    VENCTO-FIM-INV)
                                    PERFORM FAZER-OUTRAS-COMPARACOES
                                END-IF
                             END-IF
                        END-READ
                    END-PERFORM
                 ELSE
                    MOVE ZEROS TO SITUACAO-CR20
                    MOVE VENCTO-INI-INV TO DATA-VENCTO-CR20
                    START CRD020 KEY IS NOT LESS ALT2-CR20 INVALID KEY
                          MOVE "10" TO ST-CRD020
                    END-START

                    PERFORM UNTIL ST-CRD020 = "10"
                        READ CRD020 NEXT WITH IGNORE LOCK AT END
                             MOVE "10" TO ST-CRD020
                        NOT AT END
                             IF DATA-VENCTO-CR20 > VENCTO-FIM-INV
                                MOVE "10" TO ST-CRD020
                             ELSE
                                IF SITUACAO-CR20 = 0
                                   IF MOVTO-INI-INV = 0 OR
                                      (DATA-MOVTO-CR20 NOT <
                                       MOVTO-INI-INV AND
                                       DATA-MOVTO-CR20 NOT >
                                       MOVTO-FIM-INV)
                                       PERFORM FAZER-OUTRAS-COMPARACOES
                                   END-IF
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
           MOVE AUX-TIPO-DOCTO TO GS-TIPO
           MOVE "REFRESH-DATA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.


       FAZER-OUTRAS-COMPARACOES SECTION.
           IF PORTADOR-CR20 = GS-PORTADOR
              IF TIPO-DOCTO-CR20 = AUX-TIPO
                 MOVE SEQ-W         TO GS-SEQ
                 MOVE "REFRESH-DATA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 PERFORM MOVER-DADOS-TIPO1.

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
              MOVE REG-REMESSA(1: 2) TO TIPO-W
              IF TIPO-W = 0 OR TIPO-W = 9
                 CONTINUE
              ELSE
                   MOVE REG-REMESSA(3: 498) TO REM-TIPO1
                   MOVE CONTROLE-T1(2: 9)   TO COD-COMPL-CR20
                   MOVE CONTROLE-T1(11: 5)  TO SEQ-CR20
                   READ CRD020 INVALID KEY
                        CONTINUE
                   NOT INVALID KEY
                        PERFORM GRAVA-ANOTACAO
                        MOVE GS-ACP-PORTADOR TO PORTADOR-CR20
                        REWRITE REG-CRD020
                        END-REWRITE
                   END-READ
              END-IF
            END-READ
           END-PERFORM.
           CLOSE      CRD200 CRD201 CRD020
           OPEN INPUT CRD020.
           MOVE "UNSHOW-WIN3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE REMESSA.
       ATUALIZA-SEQUENCIA SECTION.
           CLOSE SEQBRAS
           OPEN I-O SEQBRAS
           MOVE 1 TO CONT-SEQUENCIA.
           READ SEQBRAS.
           MOVE SEQUENCIA-W       TO SEQUENCIA.
           REWRITE REG-SEQBRAS.
           CLOSE SEQBRAS
           OPEN INPUT SEQBRAS.
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
           MOVE "TRANSF.PORTADOR- DOCTO: XXXXXXXXXX - 99-XXXXXXXXXXXXXXX
      -    "X P/ 99-XXXXXXXXXXXXXXXX" TO ANOTACAO-CR201.
           MOVE NR-DOCTO-CR20       TO ANOTACAO-CR201(25: 11)
           MOVE PORTADOR-CR20       TO ANOTACAO-CR201(38: 4) PORTADOR
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT           TO ANOTACAO-CR201(43: 16)
           MOVE GS-ACP-PORTADOR     TO ANOTACAO-CR201(63: 4) PORTADOR
           READ CAD018 INVALID KEY MOVE SPACES TO NOME-PORT.
           MOVE NOME-PORT           TO ANOTACAO-CR201(67: 16)
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
           MOVE "00"                     TO ID-REG-REM.
           MOVE "02"                     TO TIPO-INSC-T0
           MOVE 11684219000173           TO INSC-T0
           MOVE "EK FORMATURAS LTDA"     TO NOME-EMP-T0
           MOVE "CBR500"                 TO TIPO-ARQUIVO-T0
           MOVE ZEROS                    TO BRANCO1-T0
           MOVE REM-TIPO0 TO DADOS-REM.
           WRITE REG-REMESSA.
           MOVE REG-REMESSA TO REG-REMESSA2
           MOVE X"0D0A"    TO  PULA-REM2
           WRITE REG-REMESSA2
      *    AFTER 1
           MOVE ZEROS TO VALOR-TOTAL QTDE-TIT.

       MOVER-DADOS-TIPO1 SECTION.
           INITIALIZE REM-TIPO1
           MOVE "01"                     TO ID-REG-REM
           MOVE ZEROS                    TO CONVENIO-T1
           MOVE GS-CARTEIRA-BCO          TO CARTEIRA-T1
           MOVE 019                      TO VARIACAO-T1
           MOVE SPACES                   TO SEU-NUMERO-T1
           MOVE NR-DOCTO-CR20            TO SEU-NUMERO-T1
      *    STRING CLIENTE-CR20(1:4) NR-DOCTO-CR20 INTO SEU-NUMERO-T1

      * CALCULO PARA O NOSSO NUMERO -> BANCO DO BRASIL

           ADD 1 TO SEQUENCIA-W
           STRING "2392243" SEQUENCIA-W INTO NOSSO-NUMERO-T1

      *    STRING "178157" SEQUENCIA-W INTO TAB-SEQUENCIA
      *    MOVE 0                        TO IND TOTAL
      *    PERFORM UNTIL IND = 11
      *        ADD 1 TO IND
      *        EVALUATE IND
      *            WHEN 1   COMPUTE VALOR = TAB-SEQ(IND) * 7
      *            WHEN 2   COMPUTE VALOR = TAB-SEQ(IND) * 8
      *            WHEN 3   COMPUTE VALOR = TAB-SEQ(IND) * 9
      *            WHEN 4   COMPUTE VALOR = TAB-SEQ(IND) * 2
      *            WHEN 5   COMPUTE VALOR = TAB-SEQ(IND) * 3
      *            WHEN 6   COMPUTE VALOR = TAB-SEQ(IND) * 4
      *            WHEN 7   COMPUTE VALOR = TAB-SEQ(IND) * 5
      *            WHEN 8   COMPUTE VALOR = TAB-SEQ(IND) * 6
      *            WHEN 9   COMPUTE VALOR = TAB-SEQ(IND) * 7
      *            WHEN 10  COMPUTE VALOR = TAB-SEQ(IND) * 8
      *            WHEN 11  COMPUTE VALOR = TAB-SEQ(IND) * 9
      *        END-EVALUATE
      *        COMPUTE TOTAL = TOTAL + VALOR
      *    END-PERFORM
      *
      *    DIVIDE TOTAL BY 11 GIVING RESTO
      *
      *    COMPUTE RESTO = TOTAL - (RESTO * 11)
      *
      *    IF RESTO = 10
      *       MOVE "X"   TO DIGITO-VERIFICADOR
      *    ELSE
      *       IF RESTO = 0
      *          MOVE "0" TO DIGITO-VERIFICADOR
      *       ELSE
      *          MOVE RESTO(2:1) TO DIGITO-VERIFICADOR
      *       END-IF
      *    END-IF
      *
      *    STRING "1133379" SEQUENCIA-W DIGITO-VERIFICADOR "         "
      *      INTO NOSSO-NUMERO-T1

      *    MOVE ZEROS                    TO NOSSO-NUMERO-T1

           MOVE "X"                      TO CONTROLE-T1(1: 1)
           MOVE COD-COMPL-CR20           TO CONTROLE-T1(2: 9)
           MOVE SEQ-CR20                 TO CONTROLE-T1(11: 05)
           MOVE SPACES                   TO CONTROLE-T1(16: 10)
           MOVE "DM"                     TO SIGLA-ESPECIE-T1
           MOVE DATA-EMISSAO-CR20        TO DATA-EMISSAO-T1
           MOVE DATA-VENCTO-CR20         TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV.
           MOVE DATA-INV                 TO DATA-VENCTO-T1
           MOVE VALOR-TOT-CR20           TO VALOR-W
           MOVE VALOR-W(1: 11)           TO VALOR-TITULO-T1(1: 11)
           MOVE VALOR-W(12: 2)           TO VALOR-TITULO-T1(12: 2)
           ADD VALOR-W                   TO VALOR-TOTAL
           ADD 1                         TO QTDE-TIT
           MOVE "09"                     TO CODIGO-MOEDA-T1
           MOVE ZEROS                    TO QUANTIDADE-MOEDA-T1
           MOVE "N"                      TO ACEITE-T1
           COMPUTE TAXA-JUROS = GS-TAXA-JURO / 100
           COMPUTE VALOR-ATRASO = (VALOR-TOT-CR20 * TAXA-JUROS) / 30
           MOVE VALOR-ATRASO(1: 11)      TO VALOR-JUROS-T1(1: 11)
           MOVE VALOR-ATRASO(12: 2)      TO VALOR-JUROS-T1(12: 2)
           MOVE ZEROS                    TO DATA-LIMITE-DESC-T1
           MOVE ZEROS                    TO VALOR-DESCONTO-T1
           MOVE ZEROS                    TO VALOR-ABATIMENTO-T1
           MOVE GS-PROTESTO              TO QUANTIDADE-DIA-PRO-T1
           IF GS-PROTESTO > 0
      *       MOVE GS-PROTESTO           TO MASC-DIA
              INITIALIZE MENSAGEM-T1
      *       STRING "PROTESTAR APOS " MASC-DIA " DIAS" INTO
      *               MENSAGEM-T1
           ELSE
              MOVE "NAO RECEBER APOS 30 DIAS DE VENCIDO" TO MENSAGEM-T1
           END-IF
           MOVE 002392243                TO CONVENIO7-POS-T1

           MOVE ZEROS                    TO CODIGO-MULTA-T1
           MOVE ZEROS                    TO DATA-MULTA-T1
           MOVE ZEROS                    TO VALOR-MULTA-T1

           MOVE SPACES                   TO FILLER-T1
           MOVE COD-COMPL-CR20           TO COD-COMPL-CG14
           READ CGD014 INVALID KEY
               MOVE SPACES                   TO TIPO-INSCRICAO-AVAL-T1
               MOVE SPACES                   TO INSCRICAO-AVALISTA-T1
               MOVE SPACES                   TO NOME-AVALISTA-T1
           NOT INVALID KEY
               IF CPF-CG14 > 100000000000
                  MOVE "02"               TO TIPO-INSCRICAO-AVAL-T1
                  MOVE CPF-CG14(1: 14)    TO INSCRICAO-AVALISTA-T1
               ELSE
                  MOVE "01"               TO TIPO-INSCRICAO-AVAL-T1
                  MOVE CPF-CG14(4: 11)    TO INSCRICAO-AVALISTA-T1
                  MOVE SPACES             TO INSCRICAO-AVALISTA-T1(12:3)
               END-IF
               MOVE NOME-CG14             TO NOME-AVALISTA-T1
           END-READ


           MOVE "S"                      TO PENDENTE-IMPRESSAO-T1
           MOVE COD-COMPL-CR20           TO COD-COMPL-CG10
                                            COD-COMPL-CG11
           READ CGD010 INVALID KEY
                INITIALIZE REG-CGD010.

           READ CGD011 INVALID KEY
                INITIALIZE REG-CGD011.

           MOVE COMPRADOR-CG10           TO NOME-CLIENTE-T1
           MOVE ENDERECO1-CG11           TO ENDERECO-CLIENTE-T1

           IF CPF1-CG11 > 100000000000
              MOVE "02"                   TO TIPO-INSCRICAO-CLI-T1
              MOVE CPF1-CG11(3: 16)       TO INSCRICAO-CLIENTE-T1
           ELSE
              MOVE "01"                   TO TIPO-INSCRICAO-CLI-T1
              MOVE CPF1-CG11(6: 16)       TO INSCRICAO-CLIENTE-T1
              MOVE SPACES                 TO INSCRICAO-CLIENTE-T1(12:3)
           END-IF

           MOVE CEP1-CG11                TO CEP-CLIENTE-T1
           MOVE CIDADE1-CG11             TO CIDADE
           READ CAD010 INVALID KEY
                MOVE SPACES TO NOME-CID UF-CID.

           MOVE NOME-CID                 TO CIDADE-CLIENTE-T1
           MOVE UF-CID                   TO UF-CLIENTE-T1
           CALL "UTI0080" USING UF-CLIENTE-T1 ESTADO-DESCRICAO-T1(1:20)
           CANCEL "UTI0080"
           MOVE ZEROS                    TO DATA-PAGAMENTO-T1
           MOVE ZEROS                    TO VALOR-PAGO-T1
           MOVE GS-MODALIDADE(1:2)       TO TIPO-MODALIDADE-T1
           MOVE "09"                     TO ESTADO-DO-TITULO-T1
           MOVE ALL ZEROS                TO USO-DO-BANCO-T1

           ADD 1  TO SEQ-W.
           MOVE REM-TIPO1 TO DADOS-REM.
           WRITE REG-REMESSA
           MOVE REG-REMESSA TO REG-REMESSA2
           MOVE X"0D0A"    TO  PULA-REM2
           WRITE REG-REMESSA2.
      *    AFTER 1.
       MOVER-DADOS-TIPO4 SECTION.
           MOVE "99"                     TO ID-REG-REM.
           MOVE SEQ-W                    TO QUANTIDADE-T2
           MOVE ZEROS                    TO BRANCO-T2
           MOVE REM-TIPO2                TO DADOS-REM.
           WRITE REG-REMESSA
           MOVE REG-REMESSA TO REG-REMESSA2
           MOVE X"0D0A"    TO  PULA-REM2
           WRITE REG-REMESSA2.
      *    AFTER 1.
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
                     MOVE REG-REMESSA(1: 2) TO TIPO-W
                     EVALUATE TIPO-W
                          WHEN 0  PERFORM CABECALHO-TIPO0
                                  PERFORM LINDET-TIPO0
                                  PERFORM CABECALHO-TIPO1
                          WHEN 1  PERFORM LINDET-TIPO1
                          WHEN 99 PERFORM CABECALHO-TIPO9
                                  PERFORM LINDET-TIPO9
                     END-EVALUATE
                END-READ
           END-PERFORM.
           MOVE VALOR-TOTAL        TO GS-VALOR-TOTAL
           MOVE QTDE-TIT           TO GS-QTDE-TITULO
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
                MOVE REG-REMESSA(1: 2) TO TIPO-W
                IF TIPO-W <> 1 CONTINUE
                ELSE
                 MOVE REG-REMESSA(3: 498) TO REM-TIPO1
                 ADD 1 TO SEQ-WK
                 MOVE NOME-CLIENTE-T1        TO NOME-WK
                 MOVE ENDERECO-CLIENTE-T1    TO ENDERECO-WK
                 MOVE CIDADE-CLIENTE-T1      TO CIDADE-WK
                 MOVE UF-CLIENTE-T1          TO UF-WK
                 MOVE CEP-CLIENTE-T1         TO CEP-WK
                 MOVE SEU-NUMERO-T1          TO DOCTO-WK
                 MOVE VALOR-TITULO-T1(4: 8)  TO VALOR-WK(1: 8)
                 MOVE VALOR-TITULO-T1(12: 2) TO VALOR-WK(9: 2)
                 WRITE REG-WORK
                 END-WRITE
                END-IF
             END-READ
           END-PERFORM.

           COPY CONDENSA.

           CLOSE WORK.  OPEN INPUT WORK.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           MOVE SPACES TO NOME-WK.
           START WORK KEY IS NOT < NOME-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
             READ WORK NEXT RECORD END MOVE "10" TO ST-WORK
               NOT AT END
                 MOVE NOME-WK           TO NOME-REL
                 MOVE ENDERECO-WK       TO ENDERECO-REL
                 MOVE CIDADE-WK         TO CIDADE-REL
                 MOVE UF-WK             TO UF-REL
                 MOVE CEP-WK            TO CEP-REL
                 MOVE DOCTO-WK          TO DOCTO-REL
                 MOVE VALOR-WK          TO VALOR-REL
                 WRITE REG-RELAT FROM LINDET
                 ADD 1 TO LIN
                 IF LIN > 56 PERFORM CABECALHO
                 END-IF
             END-READ
           END-PERFORM.
           MOVE VALOR-TOTAL             TO VALOR-TOTAL-REL.
           MOVE QTDE-TIT                TO QTDE-TIT-TOTAL-REL.
           WRITE REG-RELAT FROM LINDET1 AFTER 3.
           CLOSE REMESSA WORK.
           DELETE FILE WORK.

           COPY DESCONDENSA.
      *---------------------------------------------------
       LER-PORTADOR SECTION.
           MOVE GS-PORTADOR TO PORTADOR.
           READ CAD018 INVALID KEY MOVE "*******"  TO NOME-PORT.
           MOVE NOME-PORT TO GS-DESCR-PORTADOR.
       POPUP-PORTADOR SECTION.
           CALL "CAP018T" USING PARAMETROS-W STRING-1
           CANCEL "CAP018T"
           MOVE STRING-1(1: 30) TO GS-DESCR-PORTADOR
           MOVE STRING-1(33: 4) TO GS-PORTADOR.
      *----------------------------------------------------------
       CABECALHO-TIPO0 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "I"                 TO GS-LINDET(1: 2)
           MOVE "LIT.REM"           TO GS-LINDET(3: 8)
           MOVE "CS"                TO GS-LINDET(11: 3)
           MOVE "LIT-SERVICO"       TO GS-LINDET(14: 16)
           MOVE "BRANCO"            TO GS-LINDET(30: 08)
           MOVE "AGENC"             TO GS-LINDET(38: 05)
           MOVE "V"                 TO GS-LINDET(43: 2)
           MOVE "CONTA"             TO GS-LINDET(45: 09)
           MOVE "D"                 TO GS-LINDET(54: 02)
           MOVE "NR.CON"            TO GS-LINDET(56: 07)
           MOVE "NOME-EMPRESA"      TO GS-LINDET(63: 31)
           MOVE "NOME-BANCO"        TO GS-LINDET(94: 19)
           MOVE "DDMMAA"            TO GS-LINDET(113: 7)
           MOVE "SEQUEN"            TO GS-LINDET(120: 8)
           MOVE "BRANCO"            TO GS-LINDET(128: 288)
           MOVE "SEQUEN"            TO GS-LINDET(416: 06)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LINDET-TIPO0 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE REG-REMESSA(3: 498) TO REM-TIPO0.
      *    MOVE IDENT-ARQ-T0        TO GS-LINDET(1: 2)
      *    MOVE LITERAL-REMESSA-T0  TO GS-LINDET(3: 8)
      *    MOVE CODIGO-SERVICO-T0   TO GS-LINDET(11: 3)
      *    MOVE LITERAL-SERVICO-T0  TO GS-LINDET(14: 16)
      *    MOVE BRANCO-T0           TO GS-LINDET(30: 08)
      *    MOVE AGENCIA-T0          TO GS-LINDET(38: 05)
      *    MOVE VER-AGENCIA-T0      TO GS-LINDET(43: 2)
      *    MOVE CONTA-T0            TO GS-LINDET(45: 09)
      *    MOVE VERIF-EMP-T0        TO GS-LINDET(54: 02)
      *    MOVE NR-CONVENENTE-T0    TO GS-LINDET(56: 07)
      *    MOVE NOME-EMP-T0         TO GS-LINDET(63: 31)
      *    MOVE NOME-BANCO-T0       TO GS-LINDET(94: 19)
      *    MOVE DDMMAA-T0           TO GS-LINDET(113: 7)
      *    MOVE SEQUENCIAL-T0       TO GS-LINDET(120: 8)
      *    MOVE BRANCO1-T0          TO GS-LINDET(128: 288)
      *    MOVE REG-REMESSA(395: 6) TO GS-LINDET(416: 6)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CABECALHO-TIPO1 SECTION.
           MOVE SPACES              TO GS-LINDET.
           MOVE "INSERE-LIST"       TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "CONVEN"            TO GS-LINDET(1:7)
           MOVE "CT"                TO GS-LINDET(8:3)
           MOVE "VAR"               TO GS-LINDET(11:4)
           MOVE "SEU-NR"            TO GS-LINDET(15:11)
           MOVE "NOSSO-NR"          TO GS-LINDET(26:20)
           MOVE "CONTROLE"          TO GS-LINDET(46:25)
           MOVE "SIGLA"             TO GS-LINDET(71:6)
           MOVE "DT-EMISS"          TO GS-LINDET(77:9)
           MOVE "DT-VECTO"          TO GS-LINDET(86:9)
           MOVE "   VLR-TITULO"     TO GS-LINDET(108:14)
           MOVE "C-MOE"             TO GS-LINDET(122:6)
           MOVE "    QTD-MOEDA"     TO GS-LINDET(128:14)
           MOVE "A"                 TO GS-LINDEt(142:2)
           MOVE "      VLR-JRS"     TO GS-LINDET(144:14)
           MOVE "DT-LIMIT"          TO GS-LINDET(158:9)
           MOVE "     VLR-DESC"     TO GS-LINDET(167:14)
           MOVE "     VLR-ABAT"     TO GS-LINDET(183:14)
           MOVE "DP"                TO GS-LINDET(197:3)
           MOVE "MENSAGEM"          TO GS-LINDET(200:41)
           MOVE "CONV7POS"          TO GS-LINDET(241:10)
           MOVE "M"                 TO GS-LINDET(251:2)
           MOVE "DT-MULTA"          TO GS-LINDET(253:9)
           MOVE "    VLR-MULTA"     TO GS-LINDET(262:14)
           MOVE "FILLER"            TO GS-LINDET(278:10)
           MOVE "TA"                TO GS-LINDET(288:3)
           MOVE "INSC.AVAL"         TO GS-LINDET(291:15)
           MOVE "NOME AVALISTA"     TO GS-LINDET(306:38)
           MOVE "I"                 TO GS-LINDET(344:2)
           MOVE "TC"                TO GS-LINDET(346:3)
           MOVE "INSC.CLIENTE"      TO GS-LINDET(349:15)
           MOVE "NOME CLIENTE"      TO GS-LINDET(364:38)
           MOVE "ENDERECO"          TO GS-LINDET(402:38)
           MOVE "CEP"               TO GS-LINDET(440:9)
           MOVE "CIDADE"            TO GS-LINDET(449:16)
           MOVE "UF"                TO GS-LINDET(465:3)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       LINDET-TIPO1 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE REG-REMESSA(3: 498)    TO REM-TIPO1.
           MOVE CONVENIO-T1            TO GS-LINDET(1:7)
           MOVE CARTEIRA-T1            TO GS-LINDET(8:3)
           MOVE VARIACAO-T1            TO GS-LINDET(11:4)
           MOVE SEU-NUMERO-T1          TO GS-LINDET(15:11)
           MOVE NOSSO-NUMERO-T1        TO GS-LINDET(26:20)
           MOVE CONTROLE-T1            TO GS-LINDET(46:25)
           MOVE SIGLA-ESPECIE-T1       TO GS-LINDET(71:6)
           MOVE DATA-EMISSAO-T1        TO GS-LINDET(77:9)
           MOVE DATA-VENCTO-T1         TO GS-LINDET(86:9)
           MOVE VALOR-TITULO-T1        TO GS-LINDET(108:14)
           MOVE CODIGO-MOEDA-T1        TO GS-LINDET(122:6)
           MOVE QUANTIDADE-MOEDA-T1    TO GS-LINDET(128:14)
           MOVE ACEITE-T1              TO GS-LINDEt(142:2)
           MOVE VALOR-JUROS-T1         TO GS-LINDET(144:14)
           MOVE DATA-LIMITE-DESC-T1    TO GS-LINDET(158:9)
           MOVE VALOR-DESCONTO-T1      TO GS-LINDET(167:14)
           MOVE VALOR-ABATIMENTO-T1    TO GS-LINDET(183:14)
           MOVE QUANTIDADE-DIA-PRO-T1  TO GS-LINDET(197:3)
           MOVE MENSAGEM-T1            TO GS-LINDET(200:41)
           MOVE CONVENIO7-POS-T1       TO GS-LINDET(241:10)
           MOVE CODIGO-MULTA-T1        TO GS-LINDET(251:2)
           MOVE DATA-MULTA-T1          TO GS-LINDET(253:9)
           MOVE VALOR-MULTA-T1         TO GS-LINDET(262:14)
           MOVE FILLER-T1              TO GS-LINDET(278:10)
           MOVE TIPO-INSCRICAO-AVAL-T1 TO GS-LINDET(288:3)
           MOVE INSCRICAO-AVALISTA-T1  TO GS-LINDET(291:15)
           MOVE NOME-AVALISTA-T1       TO GS-LINDET(306:38)
           MOVE PENDENTE-IMPRESSAO-T1  TO GS-LINDET(344:2)
           MOVE TIPO-INSCRICAO-CLI-T1  TO GS-LINDET(346:3)
           MOVE INSCRICAO-CLIENTE-T1   TO GS-LINDET(349:15)
           MOVE NOME-CLIENTE-T1        TO GS-LINDET(364:38)
           MOVE ENDERECO-CLIENTE-T1    TO GS-LINDET(402:38)
           MOVE CEP-CLIENTE-T1         TO GS-LINDET(440:9)
           MOVE CIDADE-CLIENTE-T1      TO GS-LINDET(449:16)
           MOVE UF-CLIENTE-T1          TO GS-LINDET(465:3)

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CABECALHO-TIPO9 SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "BRANCOS"     TO GS-LINDET(01: 394)
           MOVE "SEQUEN"      TO GS-LINDET(395: 6)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LINDET-TIPO9 SECTION.
           MOVE SPACES              TO GS-LINDET.
           MOVE SPACES              TO GS-LINDET(1: 394)
           MOVE SEQ-W               TO GS-LINDET(395: 6).
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
      *---------------------------------------------------
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO GS-EXIT-FLG.
           MOVE 1 TO ERRO-W.
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CRP9102A" TO DS-SET-NAME
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
           move "CRP9102A"          to logacess-programa
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

           CLOSE CRD020 SEQBRAS CGD010 CGD011 CGD014 CAD010 CAD018
                 RETPORT.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CRP9104.
       AUTHOR.        ALFREDO SAVIOLLI NETO.
      *GERA ARQUIVO REMESSA P/ BANCO BANESTES
       DATE-WRITTEN.  10-05-2006.
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
           COPY CGPX016.
           COPY CRPX200.
           COPY CRPX201.
           COPY LOGACESS.SEL.
           SELECT SEQBANES ASSIGN TO PATH-SEQBANES
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
       COPY CGPW016.
       COPY CRPW200.
       COPY CRPW201.
       COPY LOGACESS.FD.
       FD  SEQBANES.
       01  REG-SEQBANES.
           05  CONT-SEQUENCIA  PIC 9.
           05  SEQUENCIA       PIC 9(06).
       FD  REMESSA.
       01  REG-REMESSA.
           05  DADOS-REM        PIC X(150).

       FD  REMESSA2
           label record is omitted.
       01  REG-REMESSA2.
           05  DADOS-REM2       PIC X(150).
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
           05  VALOR-WK         PIC 9(13)V99.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER          PIC X(132).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CRP9104.CPB".
           COPY "CRP9104.CPY".
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
           05 ST-CGD016           PIC XX     VALUE SPACES.
           05 ST-CRD200           PIC XX     VALUE SPACES.
           05 ST-CRD201           PIC XX     VALUE SPACES.
           05 FS-LOGACESS         PIC XX     VALUE SPACES.
           05 ST-SEQ              PIC XX     VALUE SPACES.
           05 ST-WORK             PIC XX     VALUE SPACES.
           05 VARIA-W             PIC 9(8)   VALUE ZEROS.
           05 VALOR-W             PIC 9(13)V99 VALUE ZEROS.
           05 TIPO-W              PIC 99     VALUE ZEROS.
           05 SEQ-W               PIC 9(6)   VALUE ZEROS.
           05 OPCAO               PIC 9      VALUE ZEROS.
           05 DATA-DIA            PIC 9(6)   VALUE ZEROS.
           05 DATA-DIA-I          PIC 9(8)   VALUE ZEROS.
           05 HORA-BRA            PIC 9(8)   VALUE ZEROS.
           05 ULT-SEQ             PIC 9(5)   VALUE ZEROS.
           05 DATA-E              PIC 99/99/99.
           05 REMESSA-NOME        PIC X(12)  VALUE SPACES.
           05 SEQUENCIA-W         PIC 9(06)     VALUE ZEROS.
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
           05 AUX-TIPO-DOCTO      PIC X(20)    VALUE SPACES.
           05 LIN                 PIC 9(02)    VALUE ZEROS.
           05 IND                 PIC 9(02)    VALUE ZEROS.
           05 TOTAL               PIC 9(4)     VALUE ZEROS.
           05 VALOR               PIC 9(4)     VALUE ZEROS.
           05 RESTO               PIC 9(2)     VALUE ZEROS.
           05 AUX-TIPO            PIC 9(1)     VALUE ZEROS.
           05 DIGITO-VERIFICADOR  PIC X(1)     VALUE SPACES.
           05 CONTADOR            PIC 9(02)    VALUE ZEROS.
           05 CONTADOR2           PIC 9(02)    VALUE ZEROS.
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
           05 REM-TIPOA.
              10  CAMPO-A1                  PIC X(01) VALUE "A".
              10  CAMPO-A2                  PIC 9(01) VALUE 1.
              10  CAMPO-A3                  PIC X(20) VALUE "3076".
              10  CAMPO-A4                  PIC X(20)
                  VALUE "RDA EMP. FOTOGRAFICA".
              10  CAMPO-A5                  PIC 9(03) VALUE 021.
              10  CAMPO-A6                  PIC X(20) VALUE "BANESTES".
              10  CAMPO-A7                  PIC 9(08) VALUE ZEROS.
              10  CAMPO-A8                  PIC 9(06) VALUE ZEROS.
              10  CAMPO-A9                  PIC 9(02) VALUE 04.
              10  CAMPO-A10                 PIC X(17) VALUE "DEBITO AUTO
      -       "MATICO".
              10  CAMPO-A11                 PIC X(52) VALUE SPACES.
           05 REM-TIPOB.
              10  CAMPO-B1                  PIC X(01) VALUE "B".
              10  CAMPO-B2                  PIC X(25) VALUE SPACES.
              10  CAMPO-B3                  PIC X(04) VALUE SPACES.
              10  CAMPO-B4                  PIC 9(10) VALUE ZEROS.
              10  CAMPO-B5                  PIC X(04) VALUE SPACES.
              10  CAMPO-B6                  PIC 9(08) VALUE ZEROS.
              10  CAMPO-B7                  PIC X(97) VALUE SPACES.
              10  CAMPO-B8                  PIC 9(01) VALUE ZEROS.
           05 REM-TIPOC.
              10  CAMPO-C1                  PIC X(01) VALUE "C".
              10  CAMPO-C2                  PIC X(25) VALUE SPACES.
              10  CAMPO-C3                  PIC X(04) VALUE SPACES.
              10  CAMPO-C4                  PIC 9(10) VALUE ZEROS.
              10  CAMPO-C5                  PIC X(04) VALUE SPACES.
              10  CAMPO-C6                  PIC X(40) VALUE SPACES.
              10  CAMPO-C7                  PIC X(40) VALUE SPACES.
              10  CAMPO-C8                  PIC X(25) VALUE SPACES.
              10  CAMPO-C9                  PIC 9(01) VALUE ZEROS.
           05 REM-TIPOD.
              10  CAMPO-D1                  PIC X(01) VALUE "D".
              10  CAMPO-D2                  PIC X(25).
              10  CAMPO-D3                  PIC X(04) VALUE SPACES.
              10  CAMPO-D4                  PIC 9(10) VALUE ZEROS.
              10  CAMPO-D5                  PIC X(04) VALUE SPACES.
              10  CAMPO-D6                  PIC X(25) VALUE SPACES.
              10  CAMPO-D7                  PIC x(60) VALUE SPACES.
              10  CAMPO-D8                  PIC X(20) VALUE SPACES.
              10  CAMPO-D9                  PIC 9(01) VALUE ZEROS.
           05 REM-TIPOE.
              10  CAMPO-E1                  PIC X(01) VALUE "E".
              10  CAMPO-E2                  PIC X(25) VALUE SPACES.
              10  CAMPO-E3                  PIC X(04) VALUE SPACES.
              10  CAMPO-E4                  PIC 9(10) VALUE ZEROS.
              10  CAMPO-E5                  PIC X(04) VALUE SPACES.
              10  CAMPO-E6                  PIC 9(08) VALUE ZEROS.
              10  CAMPO-E7                  PIC 9(13)V99 VALUE ZEROS.
              10  CAMPO-E8                  PIC X(02) VALUE SPACES.
              10  CAMPO-E9                  PIC X(60) VALUE SPACES.
              10  CAMPO-E10                 PIC X(20) VALUE SPACES.
              10  CAMPO-E11                 PIC 9(01) VALUE ZEROS.
           05 REM-TIPOF.
              10  CAMPO-F1                  PIC X(01) VALUE "F".
              10  CAMPO-F2                  PIC X(25) VALUE SPACES.
              10  CAMPO-F3                  PIC X(04) VALUE SPACES.
              10  CAMPO-F4                  PIC 9(10) VALUE ZEROS.
              10  CAMPO-F5                  PIC X(04) VALUE SPACES.
              10  CAMPO-F6                  PIC 9(08) VALUE ZEROS.
              10  CAMPO-F7                  PIC 9(15) VALUE ZEROS.
              10  CAMPO-F8                  PIC X(02) VALUE SPACES.
              10  CAMPO-F9                  PIC X(60) VALUE SPACES.
              10  CAMPO-F10                 PIC X(20) VALUE SPACES.
              10  CAMPO-F11                 PIC 9(01) VALUE ZEROS.
           05 REM-TIPOH.
              10  CAMPO-H1                  PIC X(01) VALUE "H".
              10  CAMPO-H2                  PIC X(25) VALUE SPACES.
              10  CAMPO-H3                  PIC X(04) VALUE SPACES.
              10  CAMPO-H4                  PIC 9(10) VALUE ZEROS.
              10  CAMPO-H5                  PIC X(04) VALUE SPACES.
              10  CAMPO-H6                  PIC X(25) VALUE SPACES.
              10  CAMPO-H7                  PIC X(58) VALUE SPACES.
              10  CAMPO-H8                  PIC X(22) VALUE SPACES.
              10  CAMPO-H9                  PIC 9(01) VALUE ZEROS.
           05 REM-TIPOI.
              10  CAMPO-I1                  PIC X(01) VALUE "I".
              10  CAMPO-I2                  PIC X(25) VALUE SPACES.
              10  CAMPO-I3                  PIC A(01) VALUE SPACES.
              10  CAMPO-I4                  PIC 9(14) VALUE ZEROS.
              10  CAMPO-I5                  PIC X(40) VALUE SPACES.
              10  CAMPO-I6                  PIC X(30) VALUE SPACES.
              10  CAMPO-I7                  PIC X(02) VALUE SPACES.
              10  CAMPO-I8                  PIC X(37) VALUE SPACES.
           05 REM-TIPOL.
              10  CAMPO-L1                  PIC X(01) VALUE "L".
              10  CAMPO-L2                  PIC 9(08) VALUE ZEROS.
              10  CAMPO-L3                  PIC 9(08) VALUE ZEROS.
              10  CAMPO-L4                  PIC 9(08) VALUE ZEROS.
              10  CAMPO-L5                  PIC 9(08) VALUE ZEROS.
              10  CAMPO-L6                  PIC X(117) VALUE SPACES.
           05 REM-TIPOT.
              10  CAMPO-T1                  PIC X(01) VALUE "T".
              10  CAMPO-T2                  PIC 9(06) VALUE ZEROS.
              10  CAMPO-T3                  PIC 9(17 ) VALUE ZEROS.
              10  CAMPO-T4                  PIC X(126) VALUE SPACES.
           05 REM-TIPOX.
              10  CAMPO-X1                  PIC X(01) VALUE "X".
              10  CAMPO-X2                  PIC X(04) VALUE SPACES.
              10  CAMPO-X3                  PIC X(30) VALUE SPACES.
              10  CAMPO-X4                  PIC X(30) VALUE SPACES.
              10  CAMPO-X5                  PIC X(05) VALUE SPACES.
              10  CAMPO-X6                  PIC X(05) VALUE SPACES.
              10  CAMPO-X7                  PIC X(03) VALUE SPACES.
              10  CAMPO-X8                  PIC X(20) VALUE SPACES.
              10  CAMPO-X9                  PIC X(02) VALUE SPACES.
              10  CAMPO-X10                 PIC X(01) VALUE SPACES.
              10  CAMPO-X11                 PIC X(49) VALUE SPACES.
           05 REM-TIPOZ.
              10  CAMPO-Z1                  PIC X(01) VALUE "Z".
              10  CAMPO-Z2                  PIC 9(06) VALUE ZEROS.
              10  CAMPO-Z3                  PIC 9(15)V99 VALUE ZEROS.
              10  CAMPO-Z4                  PIC X(126) VALUE SPACES.

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
              10  ESTADO-T1                 PIC X(40) VALUE SPACES.
              10  DATA-PAGAMENTO-T1         PIC 9(08) VALUE ZEROS.
              10  VALOR-PAGO-T1             PIC 9(13) VALUE ZEROS.
              10  TIPO-MODALIDADE-T1        PIC 9(02) VALUE ZEROS.
              10  ESTADO-DO-TITULO-T1       PIC 9(02) VALUE ZEROS.
              10  USO-BANCO-T1              PIC 9(16) VALUE ZEROS.
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

       01 TAB-NR-CONTA              PIC X(10).
       01 TAB-NR-CONTA-R REDEFINES TAB-NR-CONTA OCCURS 15 TIMES.
          05 TAB-CONTA              PIC 9(01).

       01 TABELA-NR-CONTA           PIC 9(10).
       01 TABELA-NR-CONTA-R REDEFINES TABELA-NR-CONTA OCCURS 10 TIMES.
          05 TABELA-CONTA           PIC 9(01).

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
           MOVE "CGD014" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD014.
           MOVE "CGD016" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD016.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD018" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "SEQBANE" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                             PATH-SEQBANES
           MOVE "LOGACESS" TO ARQ-REC.  MOVE EMPRESA-REF TO
                                                       ARQUIVO-LOGACESS
           OPEN INPUT CGD010 CGD011 CGD014 CGD016 CAD010 CAD018.

           OPEN I-O   CRD020
           CLOSE      CRD020
           OPEN INPUT CRD020

           OPEN I-O SEQBANES.
           MOVE 1 TO CONT-SEQUENCIA
           READ SEQBANES INVALID KEY
               MOVE ZEROS TO SEQUENCIA
               WRITE REG-SEQBANES.

           CLOSE SEQBANES
           OPEN INPUT SEQBANES

           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD011 <> "00"
              MOVE "ERRO ABERTURA CGD011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD014 <> "00"
              MOVE "ERRO ABERTURA CGD014: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD014 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD016 <> "00"
              MOVE "ERRO ABERTURA CGD016: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD016 TO GS-MENSAGEM-ERRO(23: 02)
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
           move "CRP9104"           to logacess-programa
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
                    MOVE GS-TIPO-DOCTO  TO AUX-TIPO-DOCTO
                    PERFORM GERA-ARQ-REMESSA
                    MOVE AUX-TIPO-DOCTO TO GS-TIPO-DOCTO
                    MOVE "REFRESH-DATA" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
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
           STRING GS-TIPO-DOCTO(1:1) INTO AUX-TIPO

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
           READ SEQBANES.
           MOVE SEQUENCIA         TO SEQUENCIA-W
           ADD  1                 TO SEQUENCIA-W.

           INITIALIZE REG-CRD020 CAMPO-Z3

           MOVE ZEROS TO VALOR-TOTAL SEQ-W.
           PERFORM MOVER-DADOS-TIPOA.

           IF GS-ALBUM > 0
              STRING "0" GS-CONTRATO GS-ALBUM INTO COD-COMPL-CR20
              MOVE COD-COMPL-CR20 TO COD-COMPL-CR20-W
              START CRD020 KEY IS NOT LESS CHAVE-CR20 INVALID KEY
                   MOVE "10" TO ST-CRD020
              END-START

              PERFORM UNTIL ST-CRD020 = "10"
                   READ CRD020 NEXT AT END
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
                     READ CRD020 NEXT AT END
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
                        READ CRD020 NEXT AT END
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
                        READ CRD020 NEXT AT END
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
           PERFORM MOVER-DADOS-TIPOZ.

           CLOSE REMESSA REMESSA2.
           PERFORM CARREGA-LISTA.


       FAZER-OUTRAS-COMPARACOES SECTION.
           IF PORTADOR-CR20 = GS-PORTADOR or GS-PORTADOR = 0
              IF TIPO-DOCTO-CR20 = AUX-TIPO or aux-tipo = 9
                 MOVE SEQ-W         TO GS-SEQ
                 MOVE "REFRESH-DATA" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
                 PERFORM MOVER-DADOS-TIPOE.

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
                        MOVE 24 TO PORTADOR-CR20
                        REWRITE REG-CRD020
                        END-REWRITE
                   END-READ
              END-IF
            END-READ
           END-PERFORM.
           CLOSE      CRD200 CRD201 CRD020
           OPEN INPUT CRD020

           MOVE "UNSHOW-WIN3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           CLOSE REMESSA.
       ATUALIZA-SEQUENCIA SECTION.
           CLOSE SEQBANES
           OPEN I-O SEQBANES
           MOVE 1 TO CONT-SEQUENCIA.
           READ SEQBANES.
           MOVE SEQUENCIA-W       TO SEQUENCIA.
           REWRITE REG-SEQBANES.
           CLOSE SEQBANES
           OPEN INPUT SEQBANES.
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
           MOVE 24                  TO ANOTACAO-CR201(63: 4) PORTADOR
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
       MOVER-DADOS-TIPOA SECTION.
           MOVE 1 TO SEQ-W.
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           MOVE WS-DATA-CPU TO CAMPO-A7
           MOVE SEQUENCIA-W TO CAMPO-A8
           MOVE REM-TIPOA   TO DADOS-REM.
           WRITE REG-REMESSA.
           MOVE REG-REMESSA TO REG-REMESSA2
           MOVE X"0D0A"     TO  PULA-REM2
           WRITE REG-REMESSA2
      *    AFTER 1
           MOVE ZEROS TO VALOR-TOTAL QTDE-TIT.

       MOVER-DADOS-TIPOE SECTION.
           MOVE SPACES                   TO CAMPO-E2
      *    MOVE "X"                      TO CAMPO-E2(1: 1)
           MOVE COD-COMPL-CR20           TO CAMPO-E2(1: 9)
      *    MOVE SEQ-CR20                 TO CAMPO-E2(11: 05)

           INITIALIZE REG-CGD016
           MOVE ZEROS                    TO TABELA-NR-CONTA
           MOVE COD-COMPL-CR20           TO CODIGO-CG16
           START CGD016 KEY IS NOT LESS CHAVE-CG16 INVALID KEY
               MOVE "10"   TO ST-CGD016.

           PERFORM UNTIL ST-CGD016 = "10"
               READ CGD016 NEXT AT END
                   MOVE "10" TO ST-CGD016
               NOT AT END
                   IF COD-COMPL-CR20 <> CODIGO-CG16
                      MOVE "10" TO ST-CGD016
                   ELSE
                      MOVE NR-CONTA-CG16(1:10) TO TAB-NR-CONTA
                      MOVE 10                  TO CONTADOR
                      MOVE 10                  TO CONTADOR2
                      MOVE ZEROS               TO TABELA-NR-CONTA
                      PERFORM UNTIL CONTADOR = 0 OR CONTADOR2 = 0
                           EVALUATE TAB-CONTA(CONTADOR)
                               WHEN "0" MOVE 0 TO
                                        TABELA-CONTA(CONTADOR2)
                                        COMPUTE CONTADOR2 = CONTADOR2 -
                                        1
                               WHEN "1" MOVE 1 TO
                                        TABELA-CONTA(CONTADOR2)
                                        COMPUTE CONTADOR2 = CONTADOR2 -
                                        1
                               WHEN "2" MOVE 2 TO
                                        TABELA-CONTA(CONTADOR2)
                                        COMPUTE CONTADOR2 = CONTADOR2 -
                                        1
                               WHEN "3" MOVE 3 TO
                                        TABELA-CONTA(CONTADOR2)
                                        COMPUTE CONTADOR2 = CONTADOR2 -
                                        1
                               WHEN "4" MOVE 4 TO
                                        TABELA-CONTA(CONTADOR2)
                                        COMPUTE CONTADOR2 = CONTADOR2 -
                                        1
                               WHEN "5" MOVE 5 TO
                                        TABELA-CONTA(CONTADOR2)
                                        COMPUTE CONTADOR2 = CONTADOR2 -
                                        1
                               WHEN "6" MOVE 6 TO
                                        TABELA-CONTA(CONTADOR2)
                                        COMPUTE CONTADOR2 = CONTADOR2 -
                                        1
                               WHEN "7" MOVE 7 TO
                                        TABELA-CONTA(CONTADOR2)
                                        COMPUTE CONTADOR2 = CONTADOR2 -
                                        1
                               WHEN "8" MOVE 8 TO
                                        TABELA-CONTA(CONTADOR2)
                                        COMPUTE CONTADOR2 = CONTADOR2 -
                                        1
                               WHEN "9" MOVE 9 TO
                                        TABELA-CONTA(CONTADOR2)
                                        COMPUTE CONTADOR2 = CONTADOR2 -
                                        1
                           END-EVALUATE
                           COMPUTE CONTADOR = CONTADOR - 1
                      END-PERFORM
                   END-IF
               END-READ
           END-PERFORM

           MOVE TABELA-NR-CONTA          TO CAMPO-E4

           MOVE DATA-VENCTO-CR20         TO CAMPO-E6

           MOVE VALOR-TOT-CR20           TO VALOR-W
           MOVE VALOR-W(1: 13)           TO CAMPO-E7(1: 13)
           MOVE VALOR-W(14: 2)           TO CAMPO-E7(14: 2)

           ADD CAMPO-E7 TO CAMPO-Z3

           MOVE "03"                     TO CAMPO-E8

           MOVE SPACES                   TO CAMPO-E9
           MOVE "X"                      TO CAMPO-E9(1: 1)
           MOVE COD-COMPL-CR20           TO CAMPO-E9(2: 9)
           MOVE SEQ-CR20                 TO CAMPO-E9(11: 05)
           MOVE NR-DOCTO-CR20            TO CAMPO-E9(17: 10)

           MOVE 0                        TO CAMPO-E11

           ADD 1  TO SEQ-W.
           MOVE REM-TIPOE TO DADOS-REM.
           WRITE REG-REMESSA
           MOVE REG-REMESSA TO REG-REMESSA2
           MOVE X"0D0A"    TO  PULA-REM2
           WRITE REG-REMESSA2.






























      /    INITIALIZE REM-TIPO1
      /    MOVE "01"                     TO ID-REG-REM
      /    MOVE ZEROS                    TO CONVENIO-T1
      /    MOVE GS-CARTEIRA-BCO          TO CARTEIRA-T1
      /    MOVE 019                      TO VARIACAO-T1
      /    MOVE SPACES                   TO SEU-NUMERO-T1
      *    STRING CLIENTE-CR20(1:4) NR-DOCTO-CR20 INTO SEU-NUMERO-T1
      /    MOVE NR-DOCTO-CR20            TO SEU-NUMERO-T1

      *    MOVE NR-DOCTO-CR20            TO SEU-NUMERO-T1

      * CALCULO PARA O NOSSO NUMERO -> BANCO DO BRASIL

      /    ADD 1 TO SEQUENCIA-W
      /    STRING "1191593" SEQUENCIA-W INTO NOSSO-NUMERO-T1

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

      /    MOVE "X"                      TO CONTROLE-T1(1: 1)
      /    MOVE COD-COMPL-CR20           TO CONTROLE-T1(2: 9)
      /    MOVE SEQ-CR20                 TO CONTROLE-T1(11: 05)
      /    MOVE SPACES                   TO CONTROLE-T1(16: 10)
      /    MOVE "DM"                     TO SIGLA-ESPECIE-T1
      /    MOVE DATA-EMISSAO-CR20        TO DATA-EMISSAO-T1
      /    MOVE DATA-VENCTO-CR20         TO DATA-INV
      /    CALL "GRIDAT1" USING DATA-INV.
      /    MOVE DATA-INV                 TO DATA-VENCTO-T1
      /    MOVE VALOR-TOT-CR20           TO VALOR-W
      /    MOVE VALOR-W(1: 11)           TO VALOR-TITULO-T1(1: 11)
      /    MOVE VALOR-W(12: 2)           TO VALOR-TITULO-T1(12: 2)
      /    ADD VALOR-W                   TO VALOR-TOTAL
      /    ADD 1                         TO QTDE-TIT
      /    MOVE "09"                     TO CODIGO-MOEDA-T1
      /    MOVE ZEROS                    TO QUANTIDADE-MOEDA-T1
      /    MOVE "N"                      TO ACEITE-T1
      /    COMPUTE TAXA-JUROS = GS-TAXA-JURO / 100
      /    COMPUTE VALOR-ATRASO = (VALOR-TOT-CR20 * TAXA-JUROS) / 30
      /    MOVE VALOR-ATRASO(1: 11)      TO VALOR-JUROS-T1(1: 11)
      /    MOVE VALOR-ATRASO(12: 2)      TO VALOR-JUROS-T1(12: 2)
      /    MOVE ZEROS                    TO DATA-LIMITE-DESC-T1
      /    MOVE ZEROS                    TO VALOR-DESCONTO-T1
      /    MOVE ZEROS                    TO VALOR-ABATIMENTO-T1
      /    MOVE GS-PROTESTO              TO QUANTIDADE-DIA-PRO-T1
      /    MOVE "NAO RECEBER APOS 30 DIAS DE VENCIDO" TO MENSAGEM-T1
      /    MOVE 001191593                TO CONVENIO7-POS-T1
      /
      /    MOVE ZEROS                    TO CODIGO-MULTA-T1
      /    MOVE ZEROS                    TO DATA-MULTA-T1
      /    MOVE ZEROS                    TO VALOR-MULTA-T1
      /
      /    MOVE SPACES                   TO FILLER-T1
      /    MOVE COD-COMPL-CR20           TO COD-COMPL-CG14
      /    READ CGD014 INVALID KEY
      /        MOVE SPACES                   TO TIPO-INSCRICAO-AVAL-T1
      /        MOVE SPACES                   TO INSCRICAO-AVALISTA-T1
      /        MOVE SPACES                   TO NOME-AVALISTA-T1
      /    NOT INVALID KEY
      /        IF CPF-CG14 > 100000000000
      /           MOVE "02"               TO TIPO-INSCRICAO-AVAL-T1
      /           MOVE CPF-CG14(3: 16)    TO INSCRICAO-AVALISTA-T1
      /        ELSE
      /           MOVE "01"               TO TIPO-INSCRICAO-AVAL-T1
      /           MOVE CPF-CG14(6: 16)    TO INSCRICAO-AVALISTA-T1
      /           MOVE SPACES             TO INSCRICAO-AVALISTA-T1(12:3)
      /        END-IF
      /        MOVE NOME-CG14             TO NOME-AVALISTA-T1
      /    END-READ
      /
      /
      /    MOVE "S"                      TO PENDENTE-IMPRESSAO-T1
      /    MOVE COD-COMPL-CR20           TO COD-COMPL-CG10
      /                                     COD-COMPL-CG11
      /    READ CGD010 INVALID KEY
      /         INITIALIZE REG-CGD010.
      /
      /    READ CGD011 INVALID KEY
      /         INITIALIZE REG-CGD011.
      /
      /    MOVE COMPRADOR-CG10           TO NOME-CLIENTE-T1
      /    MOVE ENDERECO1-CG11           TO ENDERECO-CLIENTE-T1
      /
      /    IF CPF-CG11 > 100000000000
      /       MOVE "02"                  TO TIPO-INSCRICAO-CLI-T1
      /       MOVE CPF-CG11(3: 16)       TO INSCRICAO-CLIENTE-T1
      /    ELSE
      /       MOVE "01"                  TO TIPO-INSCRICAO-CLI-T1
      /       MOVE CPF-CG11(6: 16)       TO INSCRICAO-CLIENTE-T1
      /       MOVE SPACES                TO INSCRICAO-CLIENTE-T1(12:3)
      /    END-IF
      /
      /    MOVE CEP1-CG11                TO CEP-CLIENTE-T1
      /    MOVE CIDADE1-CG11             TO CIDADE
      /    READ CAD010 INVALID KEY
      /         MOVE SPACES TO NOME-CID UF-CID.
      /
      /    MOVE NOME-CID                 TO CIDADE-CLIENTE-T1
      /    MOVE UF-CID                   TO UF-CLIENTE-T1
      /    MOVE SPACES                   TO ESTADO-T1
      /    MOVE ZEROS                    TO DATA-PAGAMENTO-T1
      /    MOVE ZEROS                    TO VALOR-PAGO-T1
      /    MOVE 07                       TO TIPO-MODALIDADE-T1
      /    MOVE ZEROS                    TO USO-BANCO-T1
      /    MOVE "09"                     TO ESTADO-DO-TITULO-T1
      /
      /    ADD 1  TO SEQ-W.
      /    MOVE REM-TIPO1 TO DADOS-REM.
      /    WRITE REG-REMESSA
      /    MOVE REG-REMESSA TO REG-REMESSA2
      /    MOVE X"0D0A"    TO  PULA-REM2
      /    WRITE REG-REMESSA2.
      *    AFTER 1.
       MOVER-DADOS-TIPOZ SECTION.
           ADD 1                         TO SEQ-W
           MOVE SEQ-W                    TO CAMPO-Z2
           MOVE REM-TIPOZ                TO DADOS-REM.
           WRITE REG-REMESSA
           MOVE REG-REMESSA TO REG-REMESSA2
           MOVE X"0D0A"    TO  PULA-REM2
           WRITE REG-REMESSA2.








      /    MOVE SEQ-W                    TO QUANTIDADE-T2
      /    MOVE ZEROS                    TO BRANCO-T2
      /    MOVE REM-TIPO2                TO DADOS-REM.
      /    WRITE REG-REMESSA
      /    MOVE REG-REMESSA TO REG-REMESSA2
      /    MOVE X"0D0A"    TO  PULA-REM2
      /    WRITE REG-REMESSA2.
      *    AFTER 1.
       CARREGA-LISTA SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           OPEN INPUT REMESSA.
           MOVE SPACES TO GS-LINDET.
           MOVE ZEROS TO ST-REM
                         VALOR-TOTAL
                         QTDE-TIT.

           PERFORM UNTIL ST-REM = "10"
                 READ REMESSA AT END
                      MOVE "10" TO ST-REM
                 NOT AT END
                      MOVE REG-REMESSA(1: 2) TO TIPO-W
                      EVALUATE REG-REMESSA(1:1)
                        WHEN "A"  PERFORM CABECALHO-TIPOA
                                  PERFORM LINDET-TIPOA
                                  PERFORM CABECALHO-TIPOE
                        WHEN "E"  PERFORM LINDET-TIPOE
                        WHEN "Z"  PERFORM CABECALHO-TIPOZ
                                  PERFORM LINDET-TIPOZ
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
             READ REMESSA AT END
                  MOVE "10" TO ST-REM
             NOT AT END
                  MOVE REG-REMESSA(1: 2) TO TIPO-W
                  EVALUATE REG-REMESSA(1:1)
                    WHEN "E" ADD 1 TO SEQ-WK
                             MOVE REG-REMESSA TO REM-TIPOE
                             MOVE CAMPO-E2(1:9) TO COD-COMPL-CG10
                             READ CGD010 INVALID KEY
                                  MOVE "*********" TO NOME-CLIENTE-T1
                                  MOVE "*********" TO
                                                     ENDERECO-CLIENTE-T1
                                  MOVE "********"  TO CIDADE-CLIENTE-T1
                                  MOVE "**"        TO UF-CLIENTE-T1
                                  MOVE ZEROS       TO CEP-CLIENTE-T1
                             NOT INVALID KEY
                                  MOVE COMPRADOR-CG10 TO NOME-CLIENTE-T1
                                  MOVE COD-COMPL-CG10 TO COD-COMPL-CG11
                                  READ CGD011 INVALID KEY
                                       MOVE "*********" TO
                                            ENDERECO-CLIENTE-T1
                                       MOVE "********"  TO
                                            CIDADE-CLIENTE-T1
                                       MOVE "**"        TO
                                            UF-CLIENTE-T1
                                       MOVE ZEROS       TO
                                            CEP-CLIENTE-T1
                                  NOT INVALID KEY
                                      MOVE ENDERECO1-CG11 TO
                                           ENDERECO-CLIENTE-T1
                                      MOVE CIDADE1-CG11   TO CIDADE
                                      READ CAD010 INVALID KEY
                                           MOVE "********" TO
                                           CIDADE-CLIENTE-T1
                                           MOVE "**"       TO
                                           UF-CLIENTE-T1
                                      NOT INVALID KEY
                                           MOVE NOME-CID TO
                                                CIDADE-CLIENTE-T1
                                           MOVE UF-CID TO UF-CLIENTE-T1
                                      END-READ
                                      MOVE CEP1-CG11 TO CEP-CLIENTE-T1
                                  END-READ
                             END-READ
                             MOVE CAMPO-E9(17:10)  TO SEU-NUMERO-T1

                             MOVE CAMPO-E7(1:13)   TO VALOR-WK(1:13)
                             MOVE CAMPO-E7(14:2)   TO VALOR-WK(14:2)

                             MOVE NOME-CLIENTE-T1     TO NOME-WK
                             MOVE ENDERECO-CLIENTE-T1 TO ENDERECO-WK
                             MOVE CIDADE-CLIENTE-T1   TO CIDADE-WK
                             MOVE UF-CLIENTE-T1       TO UF-WK
                             MOVE CEP-CLIENTE-T1      TO CEP-WK
                             MOVE SEU-NUMERO-T1       TO DOCTO-WK

                             WRITE REG-WORK
                             END-WRITE
                  END-EVALUATE
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
       CABECALHO-TIPOA SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "I"                 TO GS-LINDET(1:1)
           MOVE "R"                 TO GS-LINDET(2:1)
           MOVE "COD.CONVENIO"      TO GS-LINDET(3:20)
           MOVE "NOME EMPRESA"      TO GS-LINDET(23:20)
           MOVE "DT GERAC"          TO GS-LINDET(66:08)
           MOVE "SEQUENC"           TO GS-LINDET(74:06)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LINDET-TIPOA SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE REG-REMESSA(1: 150) TO GS-LINDET.
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

       CABECALHO-TIPOE SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "I"                 TO GS-LINDET(1:1)
           MOVE "CLIENTE EMPRESA"   TO GS-LINDET(2:25)
           MOVE "CONTA C/C"         TO GS-LINDET(31:10)
           MOVE "DT VENC"           TO GS-LINDET(45:8)
           MOVE "          VALOR"   TO GS-LINDET(53:15)
           MOVE "MO"                TO GS-LINDET(68:2)
           MOVE "USO DA EMPRESA"    TO GS-LINDET(70:60)
           MOVE "C"                 TO GS-LINDET(150:1)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       LINDET-TIPOE SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE REG-REMESSA(1: 150)    TO REM-TIPOE
                                          GS-LINDET
           ADD CAMPO-E7 TO VALOR-TOTAL
           ADD 1        TO QTDE-TIT

           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

       CABECALHO-TIPOZ SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE "I"           TO GS-LINDET(1:1)
           MOVE "REGISTR"     TO GS-LINDET(2:14)
           MOVE "VLR TOTAL"   TO GS-LINDET(16:17)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       LINDET-TIPOZ SECTION.
           MOVE SPACES TO GS-LINDET.
           MOVE REG-REMESSA(1: 150)    TO GS-LINDET
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
           MOVE "CRP9104" TO DS-SET-NAME
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
           move "CRP9104"           to logacess-programa
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

           CLOSE CRD020 SEQBANES CGD010 CGD011 CGD014 CAD010 CAD018
           CGD016.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.



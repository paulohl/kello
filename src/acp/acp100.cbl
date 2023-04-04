       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACP100.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 02/03/2005.
      *FUNÇÃO: Movimento de ATENDIMENTO A CLIENTE


       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.
      *  PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY ACPX010.
           COPY ACPX020.
           COPY ACPX030.
           COPY ACPX100.
           COPY ACPX101.
           COPY ACPX102.
           COPY ACPX103.
           COPY ACPX104.
           COPY ACPX105.
           COPY ACPX106.
           COPY CGPX001.
           COPY CGPX010.
           COPY CGPX011.
           COPY CGPX013.
           COPY COPX040.
           COPY CHPX010.
           COPY RCPX100.
           COPY CRPX020.
           COPY CRPX200.
           COPY CRPX201.
           COPY MTPX002.
           COPY MTPX019.
           COPY MTPX020.

           SELECT WORK ASSIGN TO VARIA-W
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-WORK
                  RECORD KEY IS CHAVE-WK.


       DATA DIVISION.
       FILE SECTION.

       COPY CAPW010.
       COPY ACPW010.
       COPY ACPW020.
       COPY ACPW030.
       COPY ACPW100.
       COPY ACPW101.
       COPY ACPW102.
       COPY ACPW103.
       COPY ACPW104.
       COPY ACPW105.
       COPY ACPW106.
       COPY CGPW001.
       COPY CGPW010.
       COPY CGPW011.
       COPY CGPW013.
       COPY COPW040.
       COPY CHPW010.
       COPY RCPW100.
       COPY CRPW020.
       COPY CRPW200.
       COPY CRPW201.
       COPY MTPW002.
       COPY MTPW019.
       COPY MTPW020.

       FD  WORK.
       01  REG-WORK.
           05  CHAVE-WK.
               10 DATA-WK         PIC 9(08).
               10 CLASSIF-WK      PIC 9.
               10 SEQUENCIA-WK    PIC 9(8).
               10 SEQ-WK          PIC 9(03).

      * CLASSIF-WK = 0 CONTRATO, 1 - COMUM, 2 - KAC

       WORKING-STORAGE SECTION.
           COPY "ACP100.CPB".
           COPY "ACP100.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-ACD010             PIC XX       VALUE SPACES.
           05  ST-ACD020             PIC XX       VALUE SPACES.
           05  ST-ACD030             PIC XX       VALUE SPACES.
           05  ST-ACD100             PIC XX       VALUE SPACES.
           05  ST-ACD101             PIC XX       VALUE SPACES.
           05  ST-ACD102             PIC XX       VALUE SPACES.
           05  ST-ACD103             PIC XX       VALUE SPACES.
           05  ST-ACD104             PIC XX       VALUE SPACES.
           05  ST-ACD105             PIC XX       VALUE SPACES.
           05  ST-ACD106             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CGD013             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-CHD010             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD200             PIC XX       VALUE SPACES.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-MTD002             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  ULT-SEQ               PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  CONTRATO-ANT          PIC 9(4)     VALUE ZEROS.
           05  WS-COL                PIC 9(03)    VALUE ZEROS.
           05  AUX-USUARIO           PIC X(05)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  WS-REFERENTE          PIC 9(01)    VALUE ZEROS.
           05  WS-ANOTACAO           PIC X(80)    VALUE SPACES.
           05  WS-VALOR10            PIC ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  WS-VALOR9             PIC ZZ.ZZZ,ZZ  BLANK WHEN ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  FIM-ARQ               PIC X(01)    VALUE 'N'.
               88  FIM-ARQUIVO                    VALUE 'S'.
               88  NO-FIM-ARQUIVO                 VALUE 'N'.

           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.

           05  CT-N80                PIC 9(02)    VALUE 80.
           05  CT-N640               PIC 9(03)    VALUE 640.
           05  CT-N1                 PIC 9(01)    VALUE 1.
           05  TIPO                  PIC 9(01)    VALUE ZEROS.
           05  TIPO-CADASTRO         PIC 9(01)    VALUE ZEROS.
           05  PRIMEIRA              PIC X(01)    VALUE SPACES.
           05  MESANO-W              PIC 9(06)    VALUE ZEROS.
           05  ACP-REFERENTE         PIC 9(01)    VALUE ZEROS.
           05  AUX-CLIENTE           PIC 9(08)    VALUE ZEROS.
           05  HORA-E                PIC X(5)     VALUE SPACES.
           05  AUX-SEQ               PIC 9(03)    VALUE ZEROS.
           05  AUX-ASSUNTO           PIC 9(01)    VALUE ZEROS.
           05  AUX-PROCURA           PIC X(06)    VALUE SPACES.
           05  JA                    PIC X(01)    VALUE SPACES.


       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.


       01 WS-DATA-SYS.
          05 WS-DATA-CPU.
             10 WS-ANO-CPU             PIC 9(04).
             10 WS-MES-CPU             PIC 9(02).
             10 WS-DIA-CPU             PIC 9(02).

           COPY "PARAMETR".
       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).


       01 DET-11.
          05 FILLER           PIC X(10) VALUE "TELEFONE".
          05 FILLER           PIC X(14) VALUE "CIDADE".
          05 FILLER           PIC X(03) VALUE "UF".
          05 FILLER           PIC X(11) VALUE "POSSE".
          05 FILLER           PIC X(31) VALUE "NOME POSSE".
          05 FILLER           PIC X(08) VALUE "MÊS/ANO".
          05 FILLER           PIC X(12) VALUE "CÓD.COMISSÃO".


       01 DET-01.
          05 DET-FONE              PIC ZZZZB9999.
          05 FILLER                PIC X(01).
          05 DET-NOME-CID          PIC X(13).
          05 FILLER                PIC X(01).
          05 DET-ESTADO            PIC X(02).
          05 FILLER                PIC X(01).
          05 DET-POSSE             PIC X(10).
          05 FILLER                PIC X(01).
          05 DET-DESC-POSSE        PIC X(30).
          05 FILLER                PIC X(01).
          05 DET-ANOMES-VISITA     PIC 99/9999.
          05 FILLER                PIC X(01).
          05 DET-VISITA            PIC 9(01).

       01 DET-22.
          05 FILLER                PIC X(07) VALUE "ESTOJO".
          05 FILLER                PIC X(07) VALUE "ENCARD".
          05 FILLER                PIC X(07) VALUE "FOLHA ".
          05 FILLER                PIC X(07) VALUE "FOTO  ".
          05 FILLER                PIC X(07) VALUE "FITA  ".
          05 FILLER                PIC X(07) VALUE "POSTER".
          05 FILLER                PIC X(07) VALUE "P.FITA".
          05 FILLER                PIC X(15) VALUE "FOGO  ".
          05 FILLER                PIC X(10) VALUE "DATA FOGO".

       01 DET-02.
          05 FILLER                PIC X(05).
          05 DET-QT-ESTOJO         PIC 9(01).
          05 FILLER                PIC X(06).
          05 DET-QT-ENCADER        PIC 9(01).
          05 FILLER                PIC X(02).
          05 DET-QT-FOLHA          PIC ZZZ9.
          05 FILLER                PIC X(02).
          05 DET-QT-FOTO           PIC ZZZ9.
          05 FILLER                PIC X(06).
          05 DET-QT-FITA           PIC 9(01).
          05 FILLER                PIC X(08).
          05 DET-QT-POSTER         PIC 9(01).
          05 FILLER                PIC X(06).
          05 DET-QT-PORTA-FITA     PIC 9(01).
          05 FILLER                PIC X(01).
          05 DET-FOGO              PIC X(14).
          05 FILLER                PIC X(01).
          05 DET-DATA-FOGO         PIC 99/99/9999.

       01  WS-HORA-SYS                 PIC 9(08).
       01  FILLER REDEFINES WS-HORA-SYS.
           03 WS-HO-SYS                PIC 9(02).
           03 WS-MI-SYS                PIC 9(02).
           03 WS-SE-SYS                PIC 9(02).
           03 WS-MS-SYS                PIC 9(02).

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
      *    COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           INITIALIZE DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC

           MOVE "ACD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-ACD010.
           MOVE "ACD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-ACD020.
           MOVE "ACD030" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-ACD030.
           MOVE "ACD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-ACD100.
           MOVE "ACD101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-ACD101.
           MOVE "ACD102" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-ACD102.
           MOVE "ACD103" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-ACD103.
           MOVE "ACD104" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-ACD104.
           MOVE "ACD105" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-ACD105.
           MOVE "ACD106" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-ACD106.

           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CGD013" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD013.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "CHD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CHD010.
           MOVE "RCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "CRD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD020.
           MOVE "CRD200" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD200.
           MOVE "CRD201" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CRD201.
           MOVE "MTD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD002.
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020.
           OPEN I-O ACD100 ACD101 ACD102 ACD103 ACD104 ACD105 ACD106
                    CGD013.

           OPEN INPUT ACD010 ACD020 ACD030
                      CGD001 CGD010 CGD011
                      MTD002 MTD019 MTD020
                      CRD020 CRD200 CRD201
                      RCD100 CHD010
                      COD040 CAD010.

           ACCEPT VARIA-W FROM TIME.

           MOVE ZEROS TO FLAG-CRITICA

           IF ST-ACD100 = "35"
              CLOSE ACD100      OPEN OUTPUT ACD100
              CLOSE ACD100      OPEN I-O ACD100
           END-IF.
           IF ST-ACD101 = "35"
              CLOSE ACD101      OPEN OUTPUT ACD101
              CLOSE ACD101      OPEN I-O ACD101
           END-IF.
           IF ST-ACD102 = "35"
              CLOSE ACD102      OPEN OUTPUT ACD102
              CLOSE ACD102      OPEN I-O ACD102
           END-IF.
           IF ST-ACD103 = "35"
              CLOSE ACD103      OPEN OUTPUT ACD103
              CLOSE ACD103      OPEN I-O ACD103
           END-IF.
           IF ST-ACD104 = "35"
              CLOSE ACD104      OPEN OUTPUT ACD104
              CLOSE ACD104      OPEN I-O ACD104
           END-IF.
           IF ST-ACD105 = "35"
              CLOSE ACD105      OPEN OUTPUT ACD105
              CLOSE ACD105      OPEN I-O ACD105
           END-IF.
           IF ST-ACD106 = "35"
              CLOSE ACD106      OPEN OUTPUT ACD106
              CLOSE ACD106      OPEN I-O ACD106
           END-IF.
           IF ST-CGD013 = "35"
              CLOSE CGD013      OPEN OUTPUT CGD013
              CLOSE CGD013      OPEN I-O CGD013
           END-IF.
           IF ST-ACD010 <> "00"
              STRING "ERRO ABERTURA ACD010: "  ST-ACD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-ACD020 <> "00"
              STRING "ERRO ABERTURA ACD020: " ST-ACD020 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-ACD030 <> "00"
              STRING "ERRO ABERTURA ACD030: " ST-ACD030 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-ACD100 <> "00"
              STRING "ERRO ABERTURA ACD100: " ST-ACD100 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-ACD101 <> "00"
              STRING "ERRO ABERTURA ACD101: " ST-ACD101 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-ACD102 <> "00"
              STRING "ERRO ABERTURA ACD102: " ST-ACD102 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-ACD103 <> "00"
              STRING "ERRO ABERTURA ACD103: " ST-ACD103 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-ACD104 <> "00"
              STRING "ERRO ABERTURA ACD104: " ST-ACD104 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-ACD105 <> "00"
              STRING "ERRO ABERTURA ACD105: " ST-ACD105 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-ACD106 <> "00"
              STRING "ERRO ABERTURA ACD106: " ST-ACD106 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CAD010 <> "00"
              STRING "ERRO ABERTURA CAD010: " ST-CAD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CGD001 <> "00"
              STRING "ERRO ABERTURA CGD001: " ST-CGD001 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CGD010 <> "00"
              STRING "ERRO ABERTURA CGD010: " ST-CGD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CGD011 <> "00"
              STRING "ERRO ABERTURA CGD011: " ST-CGD011 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CGD013 <> "00"
              STRING "ERRO ABERTURA CGD013: " ST-CGD013 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-COD040 <> "00"
              STRING "ERRO ABERTURA COD040: " ST-COD040 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CHD010 <> "00"
              STRING "ERRO ABERTURA CHD010: " ST-CHD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CRD020 <> "00"
              STRING "ERRO ABERTURA CRD020: " ST-CRD020 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CRD200 <> "00"
              STRING "ERRO ABERTURA CRD200: " ST-CRD200 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CRD201 <> "00"
              STRING "ERRO ABERTURA CRD201: " ST-CRD201 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-RCD100 <> "00"
              STRING "ERRO ABERTURA RCD100: " ST-RCD100 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-MTD002 <> "00"
              STRING "ERRO ABERTURA MTD002: " ST-MTD002 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-MTD019 <> "00"
              STRING "ERRO ABERTURA MTD019: " ST-MTD019 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-MTD020 <> "00"
              STRING "ERRO ABERTURA MTD020: " ST-MTD020 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF FLAG-CRITICA = 1
              MOVE 1 TO EXIT-FLG
           ELSE
              MOVE USUARIO-W TO AUX-USUARIO
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CARREGA-ULT-TRUE
                    PERFORM CARREGA-ULT-SEQ
                    PERFORM LIMPAR-TODAS-LB
               WHEN LE-ATENDIMENTO-TRUE
                    PERFORM LER-ATENDIMENTOS
               WHEN CHAMA-POP-UP-AT-TRUE
                    PERFORM CHAMA-POP-ATENDIMENTO
               WHEN VALIDA-TRUE
                    PERFORM VALIDAR-CAMPOS
               WHEN LE-CLIENTE-TRUE
                    PERFORM LER-CLIENTE
               WHEN CHAMA-POP-UP-CLI-TRUE
                    PERFORM CHAMA-POP-CLIENTE
               WHEN LE-CIDADE-TRUE
                    PERFORM LER-CIDADE
               WHEN CHAMA-POP-UP-CID-TRUE
                    PERFORM CHAMA-POP-CIDADE
               WHEN LE-PROCEDIMENTO-TRUE
                    PERFORM LER-PROCEDIMENTO
               WHEN CHAMA-POP-UP-PRO-TRUE
                    PERFORM CHAMA-POP-PROCEDIMENTO
               WHEN INCLUIR-PROCEDIMENTO-TRUE
                    PERFORM INCLUSAO-PROCEDIMENTO
               WHEN INCLUIR-INFORMACAO-TRUE
                    PERFORM INCLUSAO-INFORMACAO
               WHEN SAVE-FLG-TRUE
                    PERFORM SALVAR-DADOS
                    IF TIPO-GRAVACAO = 1
                       PERFORM REGRAVA-DADOS
                    ELSE
                       PERFORM GRAVA-DADOS
                    END-IF
                    PERFORM LIMPAR-DADOS
      *             PERFORM VERIFICAR-ASSUNTO
               WHEN CONSULTA-ATENDIMENTO-TRUE
                    PERFORM CONSULTAR-ATENDIMENTO
      *        WHEN GS-CARREGA-SB-TRUE
      *             PERFORM CARREGAR-SB
      *        WHEN GS-CARREGA-SEQ-TRUE
      *             PERFORM LIMPAR-DADOS
      *             PERFORM CARREGA-ULT-SEQ
      *        WHEN GS-CARREGA-DISPLAY-TRUE
      *             PERFORM CARREGAR-DISPLAY
      *        WHEN GS-CARREGA-HISTORICO-TRUE
      *             PERFORM CARREGAR-HISTORICO
      *        WHEN GS-EXCLUI-FLG-TRUE
      *             PERFORM EXCLUI
      *             PERFORM LIMPAR-DADOS
      *        WHEN GS-CLR-FLG-TRUE
      *             PERFORM LIMPAR-DADOS
      *        WHEN GS-ALTERA-DADOS-TRUE
      *             PERFORM INVERTE-DATA-MOVTO
      *             MOVE DATA-MOVTO-I     TO DATAMOV-AC100
      *             MOVE GS-SEQ           TO SEQ-AC100
      *
      *             START ACD100 KEY IS = CHAVE-AC100
      *               INVALID KEY
      *                   CONTINUE
      *                   MOVE 0          TO GS-TIPO-GRAVACAO
      *               NOT INVALID KEY
      *                   PERFORM CARREGAR-DADOS
      *             END-START
      *        WHEN GS-LE-CLIENTE-TRUE
      *             PERFORM LE-CLIENTE
      *        WHEN GS-LE-CODIGO-TRUE
      *             PERFORM LE-CODIGO
      *        WHEN GS-CHAMAR-POP-UP-TRUE
      *             PERFORM CHAMAR-POP-UP
      *        WHEN GS-VALIDA-TIPO-TRUE
      *             PERFORM VALIDAR-TIPO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.


       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

      *----------------------------------------------------------------
       CARREGA-ULT-SEQ SECTION.
           MOVE ALL "9" TO NR-ATENDIMENTO-AC100
           START ACD100 KEY IS LESS THAN CHAVE-AC100
           READ ACD100 NEXT RECORD
               IF ST-ACD100 = "00" OR "9d"
                  MOVE NR-ATENDIMENTO-AC100 TO ULT-SEQ
               ELSE
                  MOVE ZEROS TO ULT-SEQ.

           ADD 1 TO ULT-SEQ
           MOVE ULT-SEQ            TO ACP-ATENDIMENTO
           MOVE AUX-USUARIO        TO ACP-ULT-USUARIO
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           MOVE WS-DATA-CPU        TO DATA-INV
           CALL "GRIDAT1"  USING DATA-INV.
           MOVE DATA-INV           TO ACP-DT-ABERTURA

           accept WS-HORA-SYS from time

           STRING WS-HO-SYS ":" WS-MI-SYS ":" WS-SE-SYS
                                                   INTO ACP-HR-ABERTURA

           PERFORM SET-UP-FOR-REFRESH-SCREEN
           PERFORM CALL-DIALOG-SYSTEM.
      *-----------------------------------------------------------------
       LIMPAR-TODAS-LB SECTION.
           MOVE "LIMPA-LB-TODAS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.

      *-----------------------------------------------------------------
       LER-ATENDIMENTOS SECTION.
           EVALUATE NUMERO-ATENDIMENTO
               WHEN 1 IF ACP-ATENDIMENTO1 = ZEROS
                         MOVE "Código do Atendimento Não Informado" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                      ELSE
                         MOVE ACP-ATENDIMENTO1(1:3) TO TIPO-AC10
                         MOVE ACP-ATENDIMENTO1(4:3) TO CODIGO-AC10
                         PERFORM LER-ACD010
                         MOVE DESCRICAO-AC10        TO DESC-ATENDIMENTO1
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
               WHEN 2 IF ACP-ATENDIMENTO2 > 0
                         IF ACP-ATENDIMENTO2 <> ACP-ATENDIMENTO1
                            MOVE ACP-ATENDIMENTO2(1:3) TO TIPO-AC10
                            MOVE ACP-ATENDIMENTO2(4:3) TO CODIGO-AC10
                            PERFORM LER-ACD010
                            MOVE DESCRICAO-AC10    TO DESC-ATENDIMENTO2
                            PERFORM SET-UP-FOR-REFRESH-SCREEN
                            PERFORM CALL-DIALOG-SYSTEM
                         ELSE
                            MOVE "Atendimento já Informado" TO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                         END-IF
                      ELSE
                         MOVE SPACES TO DESC-ATENDIMENTO2
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
               WHEN 3 IF ACP-ADICIONAL1 > 0
                         IF ACP-ADICIONAL1 <> ACP-ATENDIMENTO1 AND
                            ACP-ADICIONAL1 <> ACP-ATENDIMENTO2
                            MOVE ACP-ADICIONAL1(1:3)   TO TIPO-AC10
                            MOVE ACP-ADICIONAL1(4:3)   TO CODIGO-AC10
                            PERFORM LER-ACD010
                            MOVE DESCRICAO-AC10       TO DESC-ADICIONAL1
                            PERFORM SET-UP-FOR-REFRESH-SCREEN
                            PERFORM CALL-DIALOG-SYSTEM
                         ELSE
                            MOVE "Atendimento já Informado" TO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                         END-IF
                      ELSE
                         MOVE SPACES TO DESC-ADICIONAL1
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
               WHEN 4 IF ACP-ADICIONAL2 > 0
                         IF ACP-ADICIONAL2 <> ACP-ATENDIMENTO1 AND
                            ACP-ADICIONAL2 <> ACP-ATENDIMENTO2 AND
                            ACP-ADICIONAL2 <> ACP-ADICIONAL1
                            MOVE ACP-ADICIONAL2(1:3)   TO TIPO-AC10
                            MOVE ACP-ADICIONAL2(4:3)   TO CODIGO-AC10
                            PERFORM LER-ACD010
                            MOVE DESCRICAO-AC10       TO DESC-ADICIONAL2
                            PERFORM SET-UP-FOR-REFRESH-SCREEN
                            PERFORM CALL-DIALOG-SYSTEM
                         ELSE
                            MOVE "Atendimento já Informado" TO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                         END-IF
                      ELSE
                         MOVE SPACES TO DESC-ADICIONAL2
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
               WHEN 5 IF ACP-ADICIONAL3 > 0
                         IF ACP-ADICIONAL3 <> ACP-ATENDIMENTO1 AND
                            ACP-ADICIONAL3 <> ACP-ATENDIMENTO2 AND
                            ACP-ADICIONAL3 <> ACP-ADICIONAL1   AND
                            ACP-ADICIONAL3 <> ACP-ADICIONAL2
                            MOVE ACP-ADICIONAL3(1:3)   TO TIPO-AC10
                            MOVE ACP-ADICIONAL3(4:3)   TO CODIGO-AC10
                            PERFORM LER-ACD010
                            MOVE DESCRICAO-AC10       TO DESC-ADICIONAL3
                            PERFORM SET-UP-FOR-REFRESH-SCREEN
                            PERFORM CALL-DIALOG-SYSTEM
                         ELSE
                            MOVE "Atendimento já Informado" TO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM EXIBIR-MENSAGEM
                         END-IF
                      ELSE
                         MOVE SPACES TO DESC-ADICIONAL3
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
               WHEN OTHER MOVE "Algum erro Interno Ocorreu procurar o Pr
      -                   "ogramador" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM.

      *-----------------------------------------------------------------
       LER-ACD010 SECTION.
           READ ACD010 INVALID KEY
               MOVE "******" TO DESCRICAO-AC10
               MOVE "Código Informado Inválido" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM.

      *-----------------------------------------------------------------
       CHAMA-POP-ATENDIMENTO SECTION.
           CALL "ACP010T" USING PASSAR-STRING-1
           CANCEL "ACP010T"
           MOVE PASSAR-STRING-1(1: 60) TO DESCRICAO-AC10
           MOVE PASSAR-STRING-1(61: 1) TO TIPO-AC10
           MOVE PASSAR-STRING-1(62: 3) TO CODIGO-AC10

           EVALUATE NUMERO-ATENDIMENTO
               WHEN 1 STRING TIPO-AC10 CODIGO-AC10 INTO ACP-ATENDIMENTO1
                      MOVE DESCRICAO-AC10 TO DESC-ATENDIMENTO1
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN 2 STRING TIPO-AC10 CODIGO-AC10 INTO ACP-ATENDIMENTO2
                      MOVE DESCRICAO-AC10 TO DESC-ATENDIMENTO2
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN 3 STRING TIPO-AC10 CODIGO-AC10 INTO ACP-ADICIONAL1
                      MOVE DESCRICAO-AC10 TO DESC-ADICIONAL1
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN 4 STRING TIPO-AC10 CODIGO-AC10 INTO ACP-ADICIONAL2
                      MOVE DESCRICAO-AC10 TO DESC-ADICIONAL2
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN 5 STRING TIPO-AC10 CODIGO-AC10 INTO ACP-ADICIONAL3
                      MOVE DESCRICAO-AC10 TO DESC-ADICIONAL3
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN OTHER MOVE "Algum erro Interno Ocorreu procurar o Pr
      -                   "ogramador" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM.

      *-----------------------------------------------------------------
       VALIDAR-CAMPOS SECTION.
           EVALUATE CAMPO-CRITICA
               WHEN "STATUS" PERFORM VALIDAR-STATUS
               WHEN OTHER STRING "CAMPO INFORMADO PARA VALIDAÇÃO NÃO ENC
      -                          "ONTRADO " CAMPO-CRITICA INTO MENSAGEM
                          MOVE   "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM.

      *-----------------------------------------------------------------
       VALIDAR-STATUS SECTION.
           IF ACP-STATUS-ATUAL <> "1-Aberto" and "2-Em Andamento" and
                                  "3-Finalizado"
              MOVE "Status Atual ou Não Informado ou Inválido" TO
                                                                MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

      *-----------------------------------------------------------------
       LER-CLIENTE SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CLIENTE = ZEROS
                   MOVE "Cliente Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   MOVE ACP-CLIENTE TO ALBUMMT19
                   READ MTD019 INVALID KEY
                       MOVE "Código Informado Formando Não Encontrado"
                       TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                   NOT INVALID KEY
                       MOVE NOME-FORM-MT19 TO ACP-FORMANDO
                       MOVE 0              TO CLASSIF-CG10
                       MOVE ACP-CLIENTE    TO CODIGO-CG10
                       READ CGD010 INVALID KEY
                            INITIALIZE REG-CGD010
                       END-READ
                       MOVE COMPRADOR-CG10 TO ACP-COMPRADOR
                       MOVE COD-COMPL-CG10 TO COD-COMPL-CG11
                       READ CGD011 INVALID KEY
                            INITIALIZE REG-CGD011
                       END-READ
                       MOVE ENDERECO1-CG11     TO ACP-ENDERECO-RES
                       MOVE BAIRRO1-CG11       TO ACP-BAIRRO-RES
                       MOVE CIDADE1-CG11       TO ACP-CIDADE-RES
                                                  CIDADE
                       READ CAD010 INVALID KEY
                           INITIALIZE REG-CAD010
                       END-READ
                       MOVE NOME-CID           TO DESC-CIDADE-RES
                       MOVE UF-CID             TO ACP-ESTADO-RES
                       MOVE DDD-CID            TO ACP-DDD-RES
                       MOVE CEP1-CG11          TO ACP-CEP-RES
                       MOVE FONE1-CG11         TO ACP-TELEFONE-RES

                       MOVE ENDERECO2-CG11     TO ACP-ENDERECO-COM
                       MOVE BAIRRO2-CG11       TO ACP-BAIRRO-COM
                       MOVE CIDADE2-CG11       TO ACP-CIDADE-COM
                                                  CIDADE
                       READ CAD010 INVALID KEY
                            INITIALIZE REG-CAD010
                       END-READ
                       MOVE NOME-CID           TO DESC-CIDADE-COM
                       MOVE UF-CID             TO ACP-ESTADO-COM
                       MOVE DDD-CID            TO ACP-DDD-COM
                       MOVE CEP2-CG11          TO ACP-CEP-COM
                       MOVE FONE2-CG11         TO ACP-TELEFONE-COM
                       MOVE E-MAIL2-CG11       TO ACP-EMAIL-RES
                       MOVE E-MAIL2-CG11       TO ACP-E-MAIL-COM
                       MOVE CELULAR2-CG11      TO ACP-CELULAR-RES
                                                  ACP-CELULAR-COM
                       MOVE CPF2-CG11          TO ACP-CPF
                       MOVE RG2-CG11           TO ACP-RG
                       MOVE DATA-NASC2-CG11    TO ACP-DTNASC
                       MOVE NOME-PAI-CG11      TO ACP-PAI
                       MOVE NOME-MAE-CG11      TO ACP-MAE
                       EVALUATE SITUACAO-CLI-CG11
                           WHEN 0 MOVE "OK" TO ACP-SITUACAO
                           WHEN 1 MOVE "PROTESTADO" TO ACP-SITUACAO
                       END-EVALUATE
                       MOVE COD-COMPL-CG10 TO COD-COMPL-CG13
                       READ CGD013 INVALID KEY
                           INITIALIZE REG-CGD013
                       END-READ
                       MOVE ENDERECO-CG13      TO ACP-ENDERECO-ENT
                       MOVE BAIRRO-CG13        TO ACP-BAIRRO-ENT
                       MOVE CEP-CG13           TO ACP-CEP-ENT
                       MOVE CIDADE-CG13        TO ACP-CIDADE-ENT
                                                  CIDADE
                       READ CAD010 INVALID KEY
                           INITIALIZE REG-CAD010
                       END-READ
                       MOVE NOME-CID           TO DESC-CIDADE-ENT
                       MOVE UF-CID             TO ACP-ESTADO-ENT
                       PERFORM SET-UP-FOR-REFRESH-SCREEN
                       PERFORM CALL-DIALOG-SYSTEM
                   END-READ
                   PERFORM SET-UP-FOR-REFRESH-SCREEN
                   PERFORM CALL-DIALOG-SYSTEM.

      *-----------------------------------------------------------------
       CHAMA-POP-CLIENTE SECTION.
           CALL   "MTP019T2" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "MTP019T2"
           MOVE PASSAR-STRING-1(1:30)  TO ACP-FORMANDO
           STRING PASSAR-STRING-1(40:4)
                  PASSAR-STRING-1(45:4) INTO ACP-CLIENTE.
      *-----------------------------------------------------------------
       LER-CIDADE SECTION.
           EVALUATE NUMERO-CIDADE
               WHEN 1 IF ACP-CIDADE-RES > 0
                         MOVE ACP-CIDADE-RES TO CIDADE
                         PERFORM LER-CAD010
                         MOVE NOME-CID           TO DESC-CIDADE-RES
                         MOVE UF-CID             TO ACP-ESTADO-RES
                         MOVE DDD-CID            TO ACP-DDD-RES
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      ELSE
                         INITIALIZE DESC-CIDADE-RES
                                    ACP-ESTADO-RES
                                    ACP-DDD-RES
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
               WHEN 2 IF ACP-CIDADE-COM > 0
                         MOVE ACP-CIDADE-COM TO CIDADE
                         PERFORM LER-CAD010
                         MOVE NOME-CID           TO DESC-CIDADE-COM
                         MOVE UF-CID             TO ACP-ESTADO-COM
                         MOVE DDD-CID            TO ACP-DDD-COM
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      ELSE
                         INITIALIZE DESC-CIDADE-COM
                                    ACP-ESTADO-COM
                                    ACP-DDD-COM
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
               WHEN 3 IF ACP-CIDADE-ENT > 0
                         MOVE ACP-CIDADE-ENT TO CIDADE
                         PERFORM LER-CAD010
                         MOVE NOME-CID           TO DESC-CIDADE-ENT
                         MOVE UF-CID             TO ACP-ESTADO-ENT
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      ELSE
                         INITIALIZE DESC-CIDADE-ENT
                                    ACP-ESTADO-ENT
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
               WHEN OTHER MOVE "Algum erro Interno Ocorreu procurar o Pr
      -                   "ogramador" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM.

      *-----------------------------------------------------------------
       LER-CAD010 SECTION.
           READ CAD010 INVALID KEY
               MOVE "Código da Cidade Inválido" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
               INITIALIZE REG-CAD010.

      *-----------------------------------------------------------------
       CHAMA-POP-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CAP010T"
           MOVE PASSAR-STRING-1(1: 34) TO NOME-CID
           MOVE PASSAR-STRING-1(35: 4) TO CIDADE

           EVALUATE NUMERO-CIDADE
               WHEN 1 MOVE CIDADE   TO ACP-CIDADE-RES
                      MOVE NOME-CID TO DESC-CIDADE-RES
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN 2 MOVE CIDADE   TO ACP-CIDADE-COM
                      MOVE NOME-CID TO DESC-CIDADE-COM
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN 3 MOVE CIDADE   TO ACP-CIDADE-ENT
                      MOVE NOME-CID TO DESC-CIDADE-ENT
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN OTHER MOVE "Algum erro Interno Ocorreu procurar o Pr
      -                   "ogramador" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM.



       LER-PROCEDIMENTO SECTION.
           EVALUATE NUMERO-PROCEDIMENTO
               WHEN 1 IF ACP-PROCEDIMENTO-KAC > 0
                         MOVE ACP-PROCEDIMENTO-KAC TO CODIGO-AC30
                         PERFORM LER-ACD030
                         MOVE DESCRICAO-AC30 TO DESC-PROCEDIMENTO-KAC
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      ELSE
                         MOVE SPACES TO DESC-PROCEDIMENTO-KAC
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
               WHEN 2 IF ACP-PROCEDIMENTO-AREA > 0
                         MOVE ACP-PROCEDIMENTO-AREA TO CODIGO-AC30
                         PERFORM LER-ACD030
                         MOVE DESCRICAO-AC30 TO DESC-PROCEDIMENTO-AREA
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      ELSE
                         MOVE SPACES TO DESC-PROCEDIMENTO-AREA
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
               WHEN OTHER MOVE "Algum erro Interno Ocorreu procurar o Pr
      -                   "ogramador" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM.
      *-----------------------------------------------------------------
       LER-ACD030 SECTION.
           READ ACD030 INVALID KEY
               MOVE "Procedimento Informado Inválido" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
               MOVE "*******" TO DESCRICAO-AC30.

      *-----------------------------------------------------------------
       CHAMA-POP-PROCEDIMENTO SECTION.
           CALL   "ACP030T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "ACP030T"
           MOVE PASSAR-STRING-1(1: 57) TO DESCRICAO-AC30
           MOVE PASSAR-STRING-1(59: 6) TO CODIGO-AC30

           EVALUATE NUMERO-PROCEDIMENTO
               WHEN 1 MOVE CODIGO-AC30 TO ACP-PROCEDIMENTO-KAC
                      MOVE DESCRICAO-AC30 TO DESC-PROCEDIMENTO-KAC
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN 2 MOVE CODIGO-AC30 TO ACP-PROCEDIMENTO-AREA
                      MOVE DESCRICAO-AC30 TO DESC-PROCEDIMENTO-AREA
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      PERFORM CALL-DIALOG-SYSTEM
               WHEN OTHER MOVE "Algum erro Interno Ocorreu procurar o Pr
      -                   "ogramador" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM.
      *-----------------------------------------------------------------
       INCLUSAO-PROCEDIMENTO SECTION.
           MOVE SPACES TO LINHA-DETALHE
           EVALUATE NUMERO-PROCEDIMENTO
               WHEN 1 IF ACP-PROCEDIMENTO-KAC = 0
                         MOVE "Procedimento do KAC não Informado" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                      ELSE
                      MOVE ACP-PROCEDIMENTO-KAC  TO AUX-PROCURA
                      PERFORM PROCURAR-JA-EXISTENTE
                      IF JA = "N"
                       MOVE ACP-PROCEDIMENTO-KAC  TO LINHA-DETALHE(1:6)
                       MOVE DESC-PROCEDIMENTO-KAC TO LINHA-DETALHE(8:60)
                      END-IF
                      END-IF
               WHEN 2 IF ACP-PROCEDIMENTO-AREA = 0
                         MOVE "Procedimento da AREA não Informado" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                      ELSE
                      MOVE ACP-PROCEDIMENTO-AREA TO AUX-PROCURA
                      PERFORM PROCURAR-JA-EXISTENTE
                      IF JA = "N"
                       MOVE ACP-PROCEDIMENTO-AREA TO LINHA-DETALHE(1:6)
                      MOVE DESC-PROCEDIMENTO-AREA TO LINHA-DETALHE(8:60)
                      END-IF
                      END-IF
               WHEN OTHER MOVE "Algum erro Interno Ocorreu procurar o Pr
      -                   "ogramador" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM.
      *-----------------------------------------------------------------
       PROCURAR-JA-EXISTENTE SECTION.
           MOVE "N" TO JA
           MOVE  1  TO CONT
           MOVE SPACES TO LINHA-DETALHE
           EVALUATE NUMERO-PROCEDIMENTO
               WHEN 1 MOVE "LER-LB1" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      PERFORM UNTIL LINHA-DETALHE = SPACES OR JA = "S"
                           IF LINHA-DETALHE(1:6) = AUX-PROCURA
                              MOVE "S" TO JA
                           END-IF
                           ADD 1 TO CONT
                           MOVE SPACES TO LINHA-DETALHE
                           MOVE "LER-LB1" TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                      END-PERFORM
               WHEN 2 MOVE "LER-LB3" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      PERFORM UNTIL LINHA-DETALHE = SPACES OR JA = "S"
                           IF LINHA-DETALHE(1:6) = AUX-PROCURA
                              MOVE "S" TO JA
                           END-IF
                           ADD 1 TO CONT
                           MOVE SPACES TO LINHA-DETALHE
                           MOVE "LER-LB3" TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                      END-PERFORM
           END-EVALUATE
           IF JA = "S"
              MOVE "Procedimento já Informado" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
      *-----------------------------------------------------------------
       INCLUSAO-INFORMACAO SECTION.
           MOVE SPACES TO LINHA-DETALHE
           EVALUATE NUMERO-INFORMACAO
               WHEN 1 MOVE ACP-INFORMACAO-KAC     TO LINHA-DETALHE
               WHEN 2 MOVE ACP-INFORMACAO-AREA    TO LINHA-DETALHE
               WHEN 3 IF ACP-DESC-SUMARIA = SPACES
                         MOVE "Descrição Sumária Não Informada" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                      ELSE
                         MOVE ACP-DESC-SUMARIA       TO LINHA-DETALHE
                      END-IF
               WHEN 4 IF ACP-RETORNO = SPACES
                         MOVE "Informação de Retorno Não Informada" TO
                         MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM
                      ELSE
                         MOVE ACP-RETORNO            TO LINHA-DETALHE
                      END-IF
               WHEN OTHER MOVE "Algum erro Interno Ocorreu procurar o Pr
      -                   "ogramador" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM.
      *-----------------------------------------------------------------
       SALVAR-DADOS SECTION.
           MOVE ALL "9" TO NR-ATENDIMENTO-AC100
           START ACD100 KEY IS NOT < CHAVE-AC100
           READ ACD100 NEXT RECORD
               IF ST-ACD100 = "00" OR "9d"
                  MOVE NR-ATENDIMENTO-AC100 TO ULT-SEQ
               ELSE
                  MOVE ZEROS TO ULT-SEQ.

           ADD 1 TO ULT-SEQ
           MOVE ULT-SEQ             TO ACP-ATENDIMENTO

           MOVE ACP-ATENDIMENTO     TO NR-ATENDIMENTO-AC100
           MOVE ACP-ULT-USUARIO     TO ULTIMO-ATENDENTE-AC100
           MOVE ACP-DT-ABERTURA     TO DATA-INV
           CALL "GRIDAT2"        USING DATA-INV
           MOVE DATA-INV            TO DATA-ABERTURA-AC100
           MOVE ACP-HR-ABERTURA     TO HORA-ABERTURA-AC100
           MOVE ACP-DT-ENCERRAMENTO TO DATA-INV
           CALL "GRIDAT2"        USING DATA-INV
           MOVE DATA-INV            TO DATA-ENCERRAMENTO-AC100
           MOVE ACP-HR-ENCERRAMENTO TO HORA-ENCERRAMENTO-AC100

           MOVE ACP-ATENDIMENTO1    TO ATENDIMENTO1-AC100
           MOVE ACP-ATENDIMENTO2    TO ATENDIMENTO2-AC100
           MOVE ACP-ADICIONAL1      TO ADICIONAL1-AC100
           MOVE ACP-ADICIONAL2      TO ADICIONAL2-AC100
           MOVE ACP-ADICIONAL3      TO ADICIONAL3-AC100

           MOVE ACP-STATUS-ATUAL    TO STATUS-ATUAL-AC100
           MOVE ACP-CLIENTE         TO CLIENTE-AC100.
      *-----------------------------------------------------------------
       GRAVA-DADOS SECTION.
           CLOSE    ACD100
           OPEN I-O ACD100

           MOVE ZEROS TO ST-ACD100.
           PERFORM UNTIL ST-ACD100 = "10"
             WRITE REG-ACD100 INVALID KEY
                 ADD 1 TO NR-ATENDIMENTO-AC100
               NOT INVALID KEY
                 MOVE "10" TO ST-ACD100.

           PERFORM GRAVA-DESCRICAO-SUMARIA
           IF ACP-ENDERECO-ENT <> SPACES OR ACP-BAIRRO-ENT <> SPACES OR
              ACP-CEP-ENT <> 0 OR ACP-CIDADE-ENT <> 0
              PERFORM GRAVA-ENTREGA
           END-IF
           PERFORM GRAVA-PROCEDIMENTOS-KAC
           PERFORM GRAVA-PROCEDIMENTOS-AREA
           PERFORM GRAVA-INFORMACAO-AREA
           PERFORM GRAVA-INFORMACAO-COMP-AREA
           MOVE ALL "9" TO NR-ATENDIMENTO-AC100
           START ACD100 KEY IS NOT < CHAVE-AC100
           READ ACD100 NEXT RECORD
               IF ST-ACD100 = "00" OR "9d"
                  MOVE NR-ATENDIMENTO-AC100 TO ULT-SEQ
               ELSE
                  MOVE ZEROS TO ULT-SEQ.

           ADD 1 TO ULT-SEQ
           MOVE ULT-SEQ             TO ACP-ATENDIMENTO

           CLOSE ACD100
           OPEN INPUT ACD100.
      *-----------------------------------------------------------------
       GRAVA-DESCRICAO-SUMARIA SECTION.
           CLOSE    ACD101
           OPEN I-O ACD101
           MOVE NR-ATENDIMENTO-AC100  TO NR-ATENDIMENTO-AC101
           MOVE ZEROS                 TO SEQ-REG-AC101

           MOVE 1         TO CONT
           MOVE SPACES    TO LINHA-DETALHE
           MOVE "LER-LB7" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL LINHA-DETALHE = SPACES
               MOVE CONT   TO SEQ-REG-AC101
               MOVE LINHA-DETALHE TO DESCRICAO-SUMARIA-AC101
               WRITE REG-ACD101
               ADD 1 TO CONT
               MOVE SPACES    TO LINHA-DETALHE
               MOVE "LER-LB7" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE ACD101
           OPEN INPUT ACD101.
      *-----------------------------------------------------------------
       GRAVA-ENTREGA SECTION.
           CLOSE CGD013
           OPEN I-O CGD013
           MOVE 0                     TO CLASSIF-CG13
           MOVE ACP-CLIENTE           TO CODIGO-CG13
           READ CGD013 INVALID KEY
                MOVE ACP-ENDERECO-ENT      TO ENDERECO-CG13
                MOVE ACP-BAIRRO-ENT        TO BAIRRO-CG13
                MOVE ACP-CEP-ENT           TO CEP-CG13
                MOVE ACP-CIDADE-ENT        TO CIDADE-CG13
                WRITE REG-CGD013
                END-WRITE
           NOT INVALID KEY
                MOVE ACP-ENDERECO-ENT      TO ENDERECO-CG13
                MOVE ACP-BAIRRO-ENT        TO BAIRRO-CG13
                MOVE ACP-CEP-ENT           TO CEP-CG13
                MOVE ACP-CIDADE-ENT        TO CIDADE-CG13
                REWRITE REG-CGD013
                END-REWRITE
           END-READ

           CLOSE      CGD013
           OPEN INPUT CGD013.

      *-----------------------------------------------------------------
       GRAVA-PROCEDIMENTOS-KAC SECTION.
           CLOSE      ACD102
           OPEN I-O   ACD102

           MOVE NR-ATENDIMENTO-AC100  TO NR-ATENDIMENTO-AC102
           MOVE ZEROS                 TO SEQ-REG-AC102

           MOVE 1         TO CONT
           MOVE SPACES    TO LINHA-DETALHE
           MOVE "LER-LB1" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL LINHA-DETALHE = SPACES
               MOVE CONT   TO SEQ-REG-AC102
               MOVE LINHA-DETALHE(1:6) TO PROCEDIMENTO-AC102
               WRITE REG-ACD102
               ADD 1 TO CONT
               MOVE SPACES    TO LINHA-DETALHE
               MOVE "LER-LB1" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM


           CLOSE      ACD102
           OPEN INPUT ACD102.

      *-----------------------------------------------------------------
       GRAVA-PROCEDIMENTOS-AREA SECTION.
           CLOSE      ACD103
           OPEN I-O   ACD103
           MOVE NR-ATENDIMENTO-AC100  TO NR-ATENDIMENTO-AC103
           MOVE ZEROS                 TO SEQ-REG-AC103

           MOVE 1         TO CONT
           MOVE SPACES    TO LINHA-DETALHE
           MOVE "LER-LB3" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL LINHA-DETALHE = SPACES
               MOVE CONT   TO SEQ-REG-AC103
               MOVE LINHA-DETALHE(1:6) TO PROCEDIMENTO-AC103
               WRITE REG-ACD103
               ADD 1 TO CONT
               MOVE SPACES    TO LINHA-DETALHE
               MOVE "LER-LB3" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      ACD103
           OPEN INPUT ACD103.
      *-----------------------------------------------------------------
       GRAVA-INFORMACAO-AREA SECTION.
           CLOSE    ACD104
           OPEN I-O ACD104
           MOVE NR-ATENDIMENTO-AC100  TO NR-ATENDIMENTO-AC104
           MOVE ZEROS                 TO SEQ-REG-AC104

           MOVE 1         TO CONT
           MOVE SPACES    TO LINHA-DETALHE
           MOVE "LER-LB4" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL LINHA-DETALHE = SPACES
               MOVE CONT   TO SEQ-REG-AC104
               MOVE LINHA-DETALHE TO INFORMACAO-AC104
               WRITE REG-ACD104
               ADD 1 TO CONT
               MOVE SPACES    TO LINHA-DETALHE
               MOVE "LER-LB4" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE ACD104
           OPEN INPUT ACD104.

      *-----------------------------------------------------------------
       GRAVA-INFORMACAO-COMP-AREA SECTION.
           CLOSE    ACD105
           OPEN I-O ACD105
           MOVE NR-ATENDIMENTO-AC100  TO NR-ATENDIMENTO-AC105
           MOVE ZEROS                 TO SEQ-REG-AC105

           MOVE 1         TO CONT
           MOVE SPACES    TO LINHA-DETALHE
           MOVE "LER-LB5" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL LINHA-DETALHE = SPACES
               MOVE CONT   TO SEQ-REG-AC105
               MOVE LINHA-DETALHE TO INFORMACAO-AC105
               WRITE REG-ACD105
               ADD 1 TO CONT
               MOVE SPACES    TO LINHA-DETALHE
               MOVE "LER-LB5" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE ACD105
           OPEN INPUT ACD105.

      *-----------------------------------------------------------------
       REGRAVA-DADOS SECTION.
           CLOSE    ACD100
           OPEN I-O ACD100

           REWRITE REG-ACD100 INVALID KEY
                 MOVE "Erro Regravacao ACD100" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                 PERFORM INICIALIZA-CHAVE-AC101
                 PERFORM EXCLUI-DESCRICAO-SUMARIA UNTIL FIM-ARQUIVO
                 PERFORM GRAVA-DESCRICAO-SUMARIA
                 PERFORM GRAVA-ENTREGA
                 PERFORM INICIALIZA-CHAVE-AC102
                 PERFORM EXCLUI-PROCEDIMENTO-KAC UNTIL FIM-ARQUIVO
                 PERFORM GRAVA-PROCEDIMENTOS-KAC
                 PERFORM INICIALIZA-CHAVE-AC103
                 PERFORM EXCLUI-PROCEDIMENTO-AREA UNTIL FIM-ARQUIVO
                 PERFORM GRAVA-PROCEDIMENTOS-AREA
                 PERFORM INICIALIZA-CHAVE-AC104
                 PERFORM EXCLUI-INFORMACAO-AREA UNTIL FIM-ARQUIVO
                 PERFORM GRAVA-INFORMACAO-AREA
                 PERFORM INICIALIZA-CHAVE-AC105
                 PERFORM EXCLUI-INFORMACAO-COMP-AREA UNTIL FIM-ARQUIVO
                 PERFORM GRAVA-INFORMACAO-COMP-AREA.

      *-----------------------------------------------------------------
       INICIALIZA-CHAVE-AC101 SECTION.
           MOVE NR-ATENDIMENTO-AC100  TO NR-ATENDIMENTO-AC101
           MOVE ZEROS                 TO SEQ-REG-AC101

           START ACD101 KEY IS NOT < CHAVE-AC101 INVALID KEY
                 SET FIM-ARQUIVO    TO TRUE
           NOT INVALID KEY
                 SET NO-FIM-ARQUIVO TO TRUE
           END-START.

      *-----------------------------------------------------------------
       EXCLUI-DESCRICAO-SUMARIA SECTION.
           CLOSE    ACD101
           OPEN I-O ACD101

           READ ACD101 NEXT RECORD AT END
                SET FIM-ARQUIVO       TO TRUE
             NOT AT END
                IF NR-ATENDIMENTO-AC101 <> NR-ATENDIMENTO-AC100
                   SET FIM-ARQUIVO    TO TRUE
                ELSE
                   DELETE ACD101
                END-IF
           END-READ

           CLOSE      ACD101
           OPEN INPUT ACD101.
      *-----------------------------------------------------------------
       INICIALIZA-CHAVE-AC102 SECTION.
           MOVE NR-ATENDIMENTO-AC100  TO NR-ATENDIMENTO-AC102
           MOVE ZEROS                 TO SEQ-REG-AC102

           START ACD102 KEY IS NOT < CHAVE-AC102 INVALID KEY
                 SET FIM-ARQUIVO    TO TRUE
           NOT INVALID KEY
                 SET NO-FIM-ARQUIVO TO TRUE
           END-START.

      *-----------------------------------------------------------------
       EXCLUI-PROCEDIMENTO-KAC SECTION.
           CLOSE    ACD102
           OPEN I-O ACD102

           READ ACD102 NEXT RECORD AT END
                SET FIM-ARQUIVO       TO TRUE
             NOT AT END
                IF NR-ATENDIMENTO-AC102 <> NR-ATENDIMENTO-AC100
                   SET FIM-ARQUIVO    TO TRUE
                ELSE
                   DELETE ACD102
                END-IF
           END-READ

           CLOSE      ACD102
           OPEN INPUT ACD102.
      *-----------------------------------------------------------------
       INICIALIZA-CHAVE-AC103 SECTION.
           MOVE NR-ATENDIMENTO-AC100  TO NR-ATENDIMENTO-AC103
           MOVE ZEROS                 TO SEQ-REG-AC103

           START ACD103 KEY IS NOT < CHAVE-AC103 INVALID KEY
                 SET FIM-ARQUIVO    TO TRUE
           NOT INVALID KEY
                 SET NO-FIM-ARQUIVO TO TRUE
           END-START.

      *-----------------------------------------------------------------
       EXCLUI-PROCEDIMENTO-AREA SECTION.
           CLOSE    ACD103
           OPEN I-O ACD103

           READ ACD103 NEXT RECORD AT END
                SET FIM-ARQUIVO       TO TRUE
             NOT AT END
                IF NR-ATENDIMENTO-AC103 <> NR-ATENDIMENTO-AC100
                   SET FIM-ARQUIVO    TO TRUE
                ELSE
                   DELETE ACD103
                END-IF
           END-READ

           CLOSE      ACD103
           OPEN INPUT ACD103.
      *-----------------------------------------------------------------
       INICIALIZA-CHAVE-AC104 SECTION.
           MOVE NR-ATENDIMENTO-AC100  TO NR-ATENDIMENTO-AC104
           MOVE ZEROS                 TO SEQ-REG-AC104

           START ACD104 KEY IS NOT < CHAVE-AC104 INVALID KEY
                 SET FIM-ARQUIVO    TO TRUE
           NOT INVALID KEY
                 SET NO-FIM-ARQUIVO TO TRUE
           END-START.

      *-----------------------------------------------------------------
       EXCLUI-INFORMACAO-AREA SECTION.
           CLOSE    ACD104
           OPEN I-O ACD104

           READ ACD104 NEXT RECORD AT END
                SET FIM-ARQUIVO       TO TRUE
             NOT AT END
                IF NR-ATENDIMENTO-AC104 <> NR-ATENDIMENTO-AC100
                   SET FIM-ARQUIVO    TO TRUE
                ELSE
                   DELETE ACD104
                END-IF
           END-READ

           CLOSE      ACD104
           OPEN INPUT ACD104.
      *-----------------------------------------------------------------
       INICIALIZA-CHAVE-AC105 SECTION.
           MOVE NR-ATENDIMENTO-AC100  TO NR-ATENDIMENTO-AC105
           MOVE ZEROS                 TO SEQ-REG-AC105

           START ACD105 KEY IS NOT < CHAVE-AC105 INVALID KEY
                 SET FIM-ARQUIVO    TO TRUE
           NOT INVALID KEY
                 SET NO-FIM-ARQUIVO TO TRUE
           END-START.

      *-----------------------------------------------------------------
       EXCLUI-INFORMACAO-COMP-AREA SECTION.
           CLOSE    ACD105
           OPEN I-O ACD105

           READ ACD105 NEXT RECORD AT END
                SET FIM-ARQUIVO       TO TRUE
             NOT AT END
                IF NR-ATENDIMENTO-AC105 <> NR-ATENDIMENTO-AC100
                   SET FIM-ARQUIVO    TO TRUE
                ELSE
                   DELETE ACD105
                END-IF
           END-READ

           CLOSE      ACD105
           OPEN INPUT ACD105.
      *-----------------------------------------------------------------

       LIMPAR-DADOS SECTION.
           MOVE "LIMPA-LB-TODAS" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-ACD100
           INITIALIZE DATA-BLOCK
           MOVE ULT-SEQ           TO ACP-ATENDIMENTO
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
      *-----------------------------------------------------------------
       CONSULTAR-ATENDIMENTO SECTION.
           MOVE "LIMPA-LB-TODAS" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           MOVE ACP-ATENDIMENTO TO NR-ATENDIMENTO-AC100
           READ ACD100 INVALID KEY
               INITIALIZE REG-ACD100
               MOVE 0 TO TIPO-GRAVACAO
           NOT INVALID KEY
               MOVE 1 TO TIPO-GRAVACAO
           END-READ

           MOVE NR-ATENDIMENTO-AC100    TO ACP-ATENDIMENTO
           MOVE ULTIMO-ATENDENTE-AC100  TO ACP-ULT-USUARIO
           MOVE DATA-ABERTURA-AC100     TO DATA-INV
           CALL "GRIDAT1"        USING DATA-INV
           MOVE DATA-INV                TO ACP-DT-ABERTURA
           MOVE HORA-ABERTURA-AC100     TO ACP-HR-ABERTURA
           MOVE DATA-ENCERRAMENTO-AC100 TO DATA-INV
           CALL "GRIDAT1"        USING DATA-INV
           MOVE DATA-INV                TO ACP-DT-ENCERRAMENTO
           MOVE HORA-ENCERRAMENTO-AC100 TO ACP-HR-ENCERRAMENTO

           MOVE ATENDIMENTO1-AC100      TO ACP-ATENDIMENTO1
           MOVE ATENDIMENTO1-AC100(1:3) TO TIPO-AC10
           MOVE ATENDIMENTO1-AC100(4:3) TO CODIGO-AC10
           READ ACD010 INVALID KEY
               INITIALIZE REG-ACD010
           END-READ
           MOVE DESCRICAO-AC10          TO DESC-ATENDIMENTO1

           MOVE ATENDIMENTO2-AC100      TO ACP-ATENDIMENTO2
           MOVE ATENDIMENTO2-AC100(1:3) TO TIPO-AC10
           MOVE ATENDIMENTO2-AC100(4:3) TO CODIGO-AC10
           READ ACD010 INVALID KEY
               INITIALIZE REG-ACD010
           END-READ
           MOVE DESCRICAO-AC10          TO DESC-ATENDIMENTO2

           MOVE ADICIONAL1-AC100        TO ACP-ADICIONAL1
           MOVE ADICIONAL1-AC100(1:3)   TO TIPO-AC10
           MOVE ADICIONAL1-AC100(4:3)   TO CODIGO-AC10
           READ ACD010 INVALID KEY
               INITIALIZE REG-ACD010
           END-READ
           MOVE DESCRICAO-AC10          TO DESC-ADICIONAL1

           MOVE ADICIONAL2-AC100        TO ACP-ADICIONAL2
           MOVE ADICIONAL2-AC100(1:3)   TO TIPO-AC10
           MOVE ADICIONAL2-AC100(4:3)   TO CODIGO-AC10
           READ ACD010 INVALID KEY
               INITIALIZE REG-ACD010
           END-READ
           MOVE DESCRICAO-AC10          TO DESC-ADICIONAL2

           MOVE ADICIONAL3-AC100        TO ACP-ADICIONAL3
           MOVE ADICIONAL3-AC100(1:3)   TO TIPO-AC10
           MOVE ADICIONAL3-AC100(4:3)   TO CODIGO-AC10
           READ ACD010 INVALID KEY
               INITIALIZE REG-ACD010
           END-READ
           MOVE DESCRICAO-AC10          TO DESC-ADICIONAL3

           MOVE STATUS-ATUAL-AC100      TO ACP-STATUS-ATUAL
           MOVE CLIENTE-AC100           TO ACP-CLIENTE
           IF ACP-CLIENTE > 0
              PERFORM LER-CLIENTE
              PERFORM CARREGAR-DESCRICAO-SUMARIA
              PERFORM CARREGAR-PROCEDIMENTO-KAC
              PERFORM CARREGAR-PROCEDIMENTO-AREA
              PERFORM CARREGAR-INFORMACAO-AREA
              PERFORM CARREGAR-INFORMACAO-COMP-AREA
           ELSE
              PERFORM LIMPAR-DADOS
           END-IF

           PERFORM SET-UP-FOR-REFRESH-SCREEN
           PERFORM CALL-DIALOG-SYSTEM.
      *-----------------------------------------------------------------
       CARREGAR-DESCRICAO-SUMARIA SECTION.
           INITIALIZE REG-ACD101
           MOVE NR-ATENDIMENTO-AC100 TO NR-ATENDIMENTO-AC101
           START ACD101 KEY IS NOT LESS CHAVE-AC101 INVALID KEY
               MOVE "10" TO ST-ACD101.
           PERFORM UNTIL ST-ACD101 = "10"
               READ ACD101 NEXT RECORD AT END
                   MOVE "10" TO ST-ACD101
               NOT AT END
                   IF NR-ATENDIMENTO-AC100 <> NR-ATENDIMENTO-AC101
                      MOVE "10" TO ST-ACD101
                   ELSE
                      MOVE SEQ-REG-AC101           TO CONT
                      MOVE DESCRICAO-SUMARIA-AC101 TO LINHA-DETALHE
                      MOVE "INSERIR-LB7"           TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM.
      *-----------------------------------------------------------------
       CARREGAR-PROCEDIMENTO-KAC SECTION.
           INITIALIZE REG-ACD102
           MOVE NR-ATENDIMENTO-AC100 TO NR-ATENDIMENTO-AC102
           START ACD102 KEY IS NOT LESS CHAVE-AC102 INVALID KEY
               MOVE "10" TO ST-ACD102.
           PERFORM UNTIL ST-ACD102 = "10"
               READ ACD102 NEXT RECORD AT END
                   MOVE "10" TO ST-ACD102
               NOT AT END
                   IF NR-ATENDIMENTO-AC100 <> NR-ATENDIMENTO-AC102
                      MOVE "10" TO ST-ACD102
                   ELSE
                      MOVE SPACES                  TO LINHA-DETALHE
                      MOVE SEQ-REG-AC102           TO CONT
                      MOVE PROCEDIMENTO-AC102      TO LINHA-DETALHE(1:6)
                                                      CODIGO-AC30
                      READ ACD030 INVALID KEY
                           MOVE "******"           TO DESCRICAO-AC30
                      END-READ
                      MOVE DESCRICAO-AC30         TO LINHA-DETALHE(8:60)

                      MOVE "INSERIR-LB1"           TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM.
      *-----------------------------------------------------------------
       CARREGAR-PROCEDIMENTO-AREA SECTION.
           INITIALIZE REG-ACD103
           MOVE NR-ATENDIMENTO-AC100 TO NR-ATENDIMENTO-AC103
           START ACD103 KEY IS NOT LESS CHAVE-AC103 INVALID KEY
               MOVE "10" TO ST-ACD103.
           PERFORM UNTIL ST-ACD103 = "10"
               READ ACD103 NEXT RECORD AT END
                   MOVE "10" TO ST-ACD103
               NOT AT END
                   IF NR-ATENDIMENTO-AC100 <> NR-ATENDIMENTO-AC103
                      MOVE "10" TO ST-ACD103
                   ELSE
                      MOVE SPACES                  TO LINHA-DETALHE
                      MOVE SEQ-REG-AC103           TO CONT
                      MOVE PROCEDIMENTO-AC103      TO LINHA-DETALHE(1:6)
                                                      CODIGO-AC30
                      READ ACD030 INVALID KEY
                           MOVE "******"           TO DESCRICAO-AC30
                      END-READ
                      MOVE DESCRICAO-AC30         TO LINHA-DETALHE(8:60)

                      MOVE "INSERIR-LB3"           TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM.
      *-----------------------------------------------------------------
       CARREGAR-INFORMACAO-AREA SECTION.
           INITIALIZE REG-ACD104
           MOVE NR-ATENDIMENTO-AC100 TO NR-ATENDIMENTO-AC104
           START ACD104 KEY IS NOT LESS CHAVE-AC104 INVALID KEY
               MOVE "10" TO ST-ACD104.
           PERFORM UNTIL ST-ACD104 = "10"
               READ ACD104 NEXT RECORD AT END
                   MOVE "10" TO ST-ACD104
               NOT AT END
                   IF NR-ATENDIMENTO-AC100 <> NR-ATENDIMENTO-AC104
                      MOVE "10" TO ST-ACD104
                   ELSE
                      MOVE SEQ-REG-AC104           TO CONT
                      MOVE INFORMACAO-AC104        TO LINHA-DETALHE
                      MOVE "INSERIR-LB4"           TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM.

      *-----------------------------------------------------------------
       CARREGAR-INFORMACAO-COMP-AREA SECTION.
           INITIALIZE REG-ACD105
           MOVE NR-ATENDIMENTO-AC100 TO NR-ATENDIMENTO-AC105
           START ACD105 KEY IS NOT LESS CHAVE-AC105 INVALID KEY
               MOVE "10" TO ST-ACD105.
           PERFORM UNTIL ST-ACD105 = "10"
               READ ACD105 NEXT RECORD AT END
                   MOVE "10" TO ST-ACD105
               NOT AT END
                   IF NR-ATENDIMENTO-AC100 <> NR-ATENDIMENTO-AC105
                      MOVE "10" TO ST-ACD105
                   ELSE
                      MOVE SEQ-REG-AC105           TO CONT
                      MOVE INFORMACAO-AC105        TO LINHA-DETALHE
                      MOVE "INSERIR-LB5"           TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                   END-IF
               END-READ
           END-PERFORM.

      *-----------------------------------------------------------------
      *VERIFICAR-ASSUNTO SECTION.
      *    EVALUATE AUX-ASSUNTO
      *        WHEN 1  PERFORM CHAMAR-MIN
      *        WHEN 2  PERFORM CHAMAR-DCR
      *        WHEN 3  PERFORM CHAMAR-OS
      *        WHEN 4  PERFORM CHAMAR-DPT
      *        WHEN 5  PERFORM CHAMAR-MEMO
      *        WHEN OTHER MOVE "Problema com relação ao Assunto" TO
      *                   MENSAGEM
      *                   MOVE "C" TO TIPO-MSG
      *                   PERFORM EXIBIR-MENSAGEM.
      *
      *CHAMAR-MIN SECTION.
      *    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
      *    MOVE SPACES TO PASSAR-STRING-1
      *    STRING TIPO-CADASTRO WS-DATA-CPU AUX-SEQ INTO PASSAR-STRING-1
      *    CALL "ACP110" USING PASSAR-STRING-1
      *    CANCEL "ACP110".
      *
      *CHAMAR-DCR SECTION.
      *    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
      *    MOVE SPACES TO PASSAR-STRING-1
      *    STRING TIPO-CADASTRO WS-DATA-CPU AUX-SEQ INTO PASSAR-STRING-1
      *    CALL "ACP120" USING PASSAR-STRING-1
      *    CANCEL "ACP120".
      *
      *CHAMAR-OS SECTION.
      *    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
      *    MOVE SPACES TO PASSAR-STRING-1
      *    STRING TIPO-CADASTRO WS-DATA-CPU AUX-SEQ INTO PASSAR-STRING-1
      *    CALL "ACP130" USING PASSAR-STRING-1
      *    CANCEL "ACP130".
      *
      *CHAMAR-DPT SECTION.
      *    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
      *    MOVE SPACES TO PASSAR-STRING-1
      *    STRING TIPO-CADASTRO WS-DATA-CPU AUX-SEQ INTO PASSAR-STRING-1
      *    CALL "ACP140" USING PASSAR-STRING-1
      *    CANCEL "ACP140".
      *
      *CHAMAR-MEMO SECTION.
      *    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
      *    MOVE SPACES TO PASSAR-STRING-1
      *    STRING TIPO-CADASTRO WS-DATA-CPU AUX-SEQ INTO PASSAR-STRING-1
      *    CALL "ACP150" USING PASSAR-STRING-1
      *    CANCEL "ACP150".
      *
      *VALIDAR-TIPO SECTION.
      *    MOVE ZEROS          TO TIPO-CADASTRO
      *    STRING GS-TIPO(1:1) INTO TIPO
      *    MOVE TIPO           TO CODIGO-AC20
      *    READ ACD020 INVALID KEY
      *        MOVE "Tipo Inválido" TO MENSAGEM
      *        MOVE "C" TO TIPO-MSG
      *        PERFORM EXIBIR-MENSAGEM
      *     NOT INVALID KEY
      *        MOVE TIPO-AC20  TO TIPO-CADASTRO.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1 to flag-critica
           move spaces to mensagem.

      *CARREGAR-SB SECTION.
      *    MOVE ZEROS           TO CODIGO-AC20 GS-CONT
      *     START ACD020 KEY IS NOT < CHAVE-AC20
      *       INVALID KEY MOVE "10" TO ST-ACD020.
      *
      *    PERFORM UNTIL ST-ACD020 = "10"
      *       READ ACD020 NEXT RECORD AT END MOVE "10" TO ST-ACD020
      *       NOT AT END
      *            ADD 1 TO GS-CONT
      *            MOVE SPACES TO GS-TIPO
      *            STRING CODIGO-AC20(3:1) "-" DESCRICAO-AC20 INTO
      *            GS-TIPO
      *            MOVE "INSERIR-SB" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *       END-READ
      *    END-PERFORM
      *    MOVE SPACES TO GS-TIPO
      *    MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
      *    STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO GS-DATA-MOVTO
      *    PERFORM CARREGA-ULT-SEQ.



      *CARREGAR-HISTORICO SECTION.
      *    OPEN OUTPUT WORK  CLOSE WORK  OPEN I-O WORK.
      *
      *    INITIALIZE REG-ACD100
      *
      *    MOVE GS-CLIENTE TO CLIENTE-AC100
      *
      *    START ACD100 KEY IS NOT LESS CLI-AC010 INVALID KEY
      *        MOVE "10" TO ST-ACD100.
      *
      *    PERFORM UNTIL ST-ACD100 = "10"
      *        READ ACD100 NEXT RECORD AT END
      *            MOVE "10" TO ST-ACD100
      *        NOT AT END
      *            IF GS-CLIENTE <> CLIENTE-AC100
      *               MOVE "10" TO ST-ACD100
      *            ELSE
      *               MOVE DATAMOV-AC100   TO DATA-WK
      *               MOVE SEQ-AC100       TO SEQUENCIA-WK
      *               MOVE 2               TO CLASSIF-WK
      *               MOVE 0               TO SEQ-WK
      *               WRITE REG-WORK
      *               END-WRITE
      *            END-IF
      *        END-READ
      *    END-PERFORM.
      *
      *    INITIALIZE REG-CRD200
      *
      *    MOVE 0          TO COD-COMPL-CR200(1:1)
      *    MOVE GS-CLIENTE TO COD-COMPL-CR200(2:8)
      *    MOVE COD-COMPL-CR200(2:8) TO AUX-CLIENTE
      *
      *    START CRD200 KEY IS NOT LESS CHAVE-CR200 INVALID KEY
      *        MOVE "10" TO ST-CRD200.
      *
      *    PERFORM UNTIL ST-CRD200 = "10"
      *        READ CRD200 NEXT RECORD AT END
      *            MOVE "10" TO ST-CRD200
      *        NOT AT END
      *            MOVE COD-COMPL-CR200(2:8) TO AUX-CLIENTE
      *            IF GS-CLIENTE <> AUX-CLIENTE
      *               MOVE "10" TO ST-CRD200
      *            ELSE
      *               MOVE DATA-MOVTO-CR200 TO DATA-WK
      *               MOVE COD-COMPL-CR200  TO SEQUENCIA-WK
      *               MOVE 0                TO CLASSIF-WK
      *               MOVE SEQ-CR200        TO SEQ-WK
      *
      *               WRITE REG-WORK
      *               END-WRITE
      *            END-IF
      *        END-READ
      *    END-PERFORM.
      *
      *    INITIALIZE REG-CRD200
      *
      *    MOVE 1          TO COD-COMPL-CR200(1:1)
      *    MOVE GS-CLIENTE TO COD-COMPL-CR200(2:8)
      *    MOVE COD-COMPL-CR200(2:8) TO AUX-CLIENTE
      *
      *    START CRD200 KEY IS NOT LESS CHAVE-CR200 INVALID KEY
      *        MOVE "10" TO ST-CRD200.
      *
      *    PERFORM UNTIL ST-CRD200 = "10"
      *        READ CRD200 NEXT RECORD AT END
      *            MOVE "10" TO ST-CRD200
      *        NOT AT END
      *            MOVE COD-COMPL-CR200(2:8) TO AUX-CLIENTE
      *            IF GS-CLIENTE <> AUX-CLIENTE
      *               MOVE "10" TO ST-CRD200
      *            ELSE
      *               MOVE DATA-MOVTO-CR200 TO DATA-WK
      *               MOVE COD-COMPL-CR200  TO SEQUENCIA-WK
      *               MOVE 1                TO CLASSIF-WK
      *               MOVE SEQ-CR200        TO SEQ-WK
      *
      *               WRITE REG-WORK
      *               END-WRITE
      *            END-IF
      *        END-READ
      *    END-PERFORM.
      *
      *    CLOSE WORK
      *
      *    OPEN INPUT WORK
      *
      *    INITIALIZE REG-WORK
      *
      *    START WORK KEY IS NOT LESS CHAVE-WK INVALID KEY
      *        MOVE "10" TO ST-WORK.
      *
      *    MOVE "CLEAR-LIST-LB2" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM
      *
      *    MOVE ZEROS TO GS-CONT
      *
      *    PERFORM UNTIL ST-WORK = "10"
      *        READ WORK NEXT RECORD AT END
      *            MOVE "10" TO ST-WORK
      *        NOT AT END
      *            EVALUATE CLASSIF-WK
      *                WHEN 0 PERFORM HISTORICO-CRD200
      *                WHEN 1 PERFORM HISTORICO-CRD200
      *                WHEN 2 PERFORM HISTORICO-ACD100
      *            END-EVALUATE
      *        END-READ
      *    END-PERFORM.
      *
      *    CLOSE WORK.
      *
      *HISTORICO-CRD200 SECTION.
      *    STRING CLASSIF-WK SEQUENCIA-WK INTO COD-COMPL-CR200
      *    MOVE SEQ-WK                    TO SEQ-CR200
      *
      *    READ CRD200 NOT INVALID KEY
      *        MOVE DATA-RETORNO-CR200 TO DATA-INV
      *        CALL "GRIDAT1" USING DATA-INV
      *        MOVE DATA-INV TO DATA-E
      *        MOVE DATA-E           TO GS-LINDET(1: 15)
      *        MOVE COD-COMPL-CR200  TO GS-LINDET(16: 11)
      *                                 COD-COMPL-CG10
      *        READ CGD010 INVALID KEY
      *                    MOVE SPACES TO COMPRADOR-CG10
      *        END-READ
      *        MOVE COMPRADOR-CG10        TO GS-LINDET(27: 20)
      *        MOVE DATA-MOVTO-CR200 TO DATA-INV
      *        CALL "GRIDAT1" USING DATA-INV
      *        MOVE DATA-INV         TO DATA-E
      *        MOVE DATA-E           TO GS-LINDET(58: 14)
      *        MOVE HORA-MOVTO-CR200(1: 2) TO HORA-E(1: 2)
      *        MOVE ":"                    TO HORA-E(3: 1)
      *        MOVE HORA-MOVTO-CR200(3: 2) TO HORA-E(4: 2)
      *        MOVE HORA-E           TO GS-LINDET(72: 08)
      *        MOVE USUARIO-CR200    TO GS-LINDET(80: 08)
      *        MOVE SEQ-CR200        TO GS-LINDET(88: 03)
      *        IF SITUACAO-ANOTACAO-CR200 = 0
      *           MOVE "PENDENTE" TO GS-LINDET(98: 10)
      *        ELSE
      *           MOVE "CHECADO" TO GS-LINDET(98: 10)
      *        END-IF
      *        MOVE "INSERE-HISTORICO" TO DS-PROCEDURE
      *        PERFORM CALL-DIALOG-SYSTEM
      *        PERFORM CARREGA-CRD201.
      *
      *CARREGA-CRD201 SECTION.
      *    MOVE COD-COMPL-CR200  TO COD-COMPL-CR201.
      *    MOVE SEQ-CR200        TO SEQ-CR201.
      *    MOVE ZEROS            TO SUBSEQ-CR201.
      *    START CRD201 KEY IS NOT < CHAVE-CR201 INVALID KEY
      *          MOVE "10" TO ST-CRD201.
      *    PERFORM UNTIL ST-CRD201 = "10"
      *       READ CRD201 NEXT RECORD AT END MOVE "10" TO ST-CRD201
      *            NOT AT END
      *              IF COD-COMPL-CR201 <> COD-COMPL-CR200 OR
      *                 SEQ-CR201 <> SEQ-CR200
      *                   MOVE "10" TO ST-CRD201
      *              ELSE
      *                 MOVE SPACES TO GS-LINDET
      *                 MOVE ANOTACAO-CR201 TO GS-LINDET(16: 80)
      *                 MOVE "INSERE-HISTORICO" TO DS-PROCEDURE
      *                 PERFORM CALL-DIALOG-SYSTEM
      *              END-IF
      *       END-READ
      *    END-PERFORM.
      *    MOVE SPACES TO GS-LINDET.
      *    MOVE "INSERE-HISTORICO" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *
      *
      *HISTORICO-ACD100 SECTION.
      *     MOVE DATA-WK          TO DATAMOV-AC100
      *     MOVE SEQUENCIA-WK     TO SEQ-AC100
      *
      *     READ ACD100 INVALID KEY
      *        MOVE "NÃO ACHEI" TO MENSAGEM
      *        MOVE "C" TO TIPO-MSG
      *        PERFORM EXIBIR-MENSAGEM
      *     NOT INVALID KEY
      *        MOVE DATAMOV-AC100 TO DATA-INV
      *        CALL "GRIDAT1" USING DATA-INV
      *        MOVE DATA-INV TO DATA-E
      *        MOVE DATA-E           TO GS-LINDET(1: 15)
      *        MOVE 0                TO CLASSIF-CG10
      *        MOVE CLIENTE-AC100    TO CODIGO-CG10
      *        MOVE COD-COMPL-CG10   TO GS-LINDET(16:11)
      *        READ CGD010 INVALID KEY
      *                    MOVE SPACES TO COMPRADOR-CG10
      *        END-READ
      *        MOVE COMPRADOR-CG10        TO GS-LINDET(27: 20)
      *        MOVE DATAMOV-AC100    TO DATA-INV
      *        CALL "GRIDAT1" USING DATA-INV
      *        MOVE DATA-INV         TO DATA-E
      *        MOVE DATA-E           TO GS-LINDET(58: 14)
      *        MOVE HORAMOV-AC100(1: 2)    TO HORA-E(1: 2)
      *        MOVE ":"                    TO HORA-E(3: 1)
      *        MOVE HORAMOV-AC100(3: 2)    TO HORA-E(4: 2)
      *        MOVE HORA-E           TO GS-LINDET(72: 08)
      *        MOVE USUARIO-AC100    TO GS-LINDET(80: 08)
      *        MOVE SEQ-AC100        TO GS-LINDET(88: 03)
      *        EVALUATE STATUS-AC100
      *            WHEN 1 MOVE "PENDENTE" TO GS-LINDET(98: 10)
      *            WHEN 2 MOVE "EXECUTADO"  TO GS-LINDET(98:10)
      *            WHEN 3 MOVE "VISTORIADO" TO GS-LINDET(98:10)
      *        END-EVALUATE
      *
      *        MOVE "INSERE-HISTORICO" TO DS-PROCEDURE
      *        PERFORM CALL-DIALOG-SYSTEM
      *        PERFORM CARREGA-ACD101.
      *
      *CARREGA-ACD101 SECTION.
      *    MOVE DATAMOV-AC100    TO DATAMOV-AC101
      *    MOVE SEQ-AC100        TO SEQ-AC101.
      *    MOVE ZEROS            TO SEQ-REG-AC101
      *    START ACD101 KEY IS NOT < CHAVE-AC101 INVALID KEY
      *          MOVE "10" TO ST-ACD101.
      *    PERFORM UNTIL ST-ACD101 = "10"
      *       READ ACD101 NEXT RECORD AT END MOVE "10" TO ST-ACD101
      *            NOT AT END
      *              IF DATAMOV-AC100 <> DATAMOV-AC101 OR
      *                 SEQ-AC101 <> SEQ-AC100
      *                   MOVE "10" TO ST-ACD101
      *              ELSE
      *                 MOVE SPACES TO GS-LINDET
      *                 MOVE ANOTACAO-AC101 TO GS-LINDET(16: 80)
      *                 MOVE "INSERE-HISTORICO" TO DS-PROCEDURE
      *                 PERFORM CALL-DIALOG-SYSTEM
      *              END-IF
      *       END-READ
      *    END-PERFORM.
      *    MOVE SPACES TO GS-LINDET.
      *    MOVE "INSERE-HISTORICO" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *
      *BUSCA-SEQUENCIA SECTION.
      *    PERFORM UNTIL ST-ACD100 = "10"
      *        READ ACD100 NEXT RECORD AT END
      *                    MOVE "10"     TO ST-ACD100
      *         NOT AT END
      *             IF DATAMOV-AC100 <> DATA-MOVTO-I
      *                MOVE "10"        TO ST-ACD100
      *             ELSE
      *                MOVE SEQ-AC100   TO GS-SEQ ULT-SEQ
      *             END-IF
      *        END-READ
      *    END-PERFORM.
      *    ADD 1 TO GS-SEQ
      *    ADD 1 TO ULT-SEQ.
      *INVERTE-DATA-MOVTO SECTION.
      *    MOVE GS-DATA-MOVTO            TO DATA-INV
      *    CALL "GRIDAT2"  USING DATA-INV.
      *    MOVE DATA-INV                 TO DATA-MOVTO-I.
      *
      *
      *CHAMAR-POP-UP SECTION.
      *    EVALUATE GS-OPCAO-POP-UP
      *      WHEN 1 MOVE GS-TIPO(1: 1)   TO PASSAR-STRING-1(65: 1)
      *             CALL "ACP010T" USING PASSAR-STRING-1
      *             CANCEL "ACP010T"
      *             MOVE PASSAR-STRING-1(1: 60) TO GS-DESCR-ATENDIMENTO
      *             MOVE PASSAR-STRING-1(62: 3) TO GS-CODIGO
      *             MOVE PASSAR-STRING-1(6: 65) TO GS-DESCR-ATENDIMENTO
      *             MOVE PASSAR-STRING-1(1: 3) TO GS-CODIGO
      *      WHEN 2 IF TIPO-CADASTRO = 1
      *                CALL "CGP010T" USING PASSAR-STRING-1
      *                CANCEL "CGP010T"
      *                MOVE PASSAR-STRING-1(1: 30) TO GS-DESCR-CLIENTE
      *                MOVE PASSAR-STRING-1(33: 8) TO GS-CLIENTE
      *             ELSE
      *                CALL "MTP019T2" USING PASSAR-STRING-1
      *                CANCEL "MTP019T2"
      *                MOVE PASSAR-STRING-1(1:30)  TO GS-DESCR-CLIENTE
      *                STRING PASSAR-STRING-1(40:4)
      *                       PASSAR-STRING-1(45:4) INTO GS-CLIENTE
      *             END-IF
      *      WHEN OTHER
      *             CONTINUE
      *    END-EVALUATE.
      *
      *LE-CODIGO SECTION.
      *    MOVE GS-TIPO(1: 1)      TO TIPO-AC10
      *    MOVE GS-CODIGO          TO CODIGO-AC10
      *    READ ACD010 INVALID KEY MOVE "********" TO DESCRICAO-AC10.
      *    MOVE DESCRICAO-AC10     TO GS-DESCR-ATENDIMENTO
      *    MOVE ASSUNTO-AC10       TO AUX-ASSUNTO.
      *
      *LE-CLIENTE SECTION.
      *    MOVE ZERO               TO CLASSIF-CG10
      *    MOVE GS-CLIENTE         TO CODIGO-CG10
      *    READ CGD010 INVALID KEY MOVE "********" TO COMPRADOR-CG10.
      *    MOVE COMPRADOR-CG10     TO GS-DESCR-CLIENTE.
      *
      *    IF GS-CLIENTE = 0
      *       MOVE "Cliente Não Informado" to mensagem
      *       MOVE "C" TO TIPO-MSG
      *       PERFORM EXIBIR-MENSAGEM.
      *
      *CABECALHO-CHEQUE SECTION.
      *    MOVE ALL "-" TO GS-LINDET
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *
      *    MOVE
      *      "BCO  NR.CHEQ DT.VENCTO  PO SITUACAO      VALOR VENDEDOR  "
      *                                 TO GS-TEXTO-REFERENCIA.
      *
      *    MOVE GS-TEXTO-REFERENCIA TO GS-LINDET
      *
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *
      *    MOVE ALL "-" TO GS-LINDET
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *
      *    MOVE SPACES TO GS-LINDET.
      *
      *CABECALHO-DUPLICATA SECTION.
      *    MOVE ALL "-" TO GS-LINDET
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *
      *    MOVE
      *      "NR.DOCTO   TIPO-DOCTO DATA-EMIS. DATA-VCTO  VALOR-TOT. VLR
      *      "-JUROS VLR-MULTA VLR-DESC. DATA-RECTO VLR-LIQUID"
      *                                 TO GS-TEXTO-REFERENCIA.
      *    MOVE GS-TEXTO-REFERENCIA TO GS-LINDET
      *
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *
      *    MOVE ALL "-" TO GS-LINDET
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *
      *    MOVE SPACES TO GS-LINDET.
      *
      *CABECALHO-VENDA SECTION.
      *    MOVE ALL "-" TO GS-LINDET
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *
      *    MOVE
      *      "DATA-VENDA EN FI PF FO FL ES VENDEDOR    TOT-RECTO"
      *                                 TO GS-TEXTO-REFERENCIA.
      *    MOVE GS-TEXTO-REFERENCIA TO GS-LINDET
      *
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *
      *    MOVE ALL "-" TO GS-LINDET
      *    MOVE "INSERE-LIST" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *
      *    MOVE SPACES TO GS-LINDET.
      *
      *CARREGAR-DISPLAY SECTION.
      *
      *    MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM
      *
      *    IF TIPO-CADASTRO = 2
      *       MOVE 4  TO ACP-REFERENTE
      *       PERFORM CARREGAR-PRE-VENDA
      *    ELSE
      *       PERFORM CARREGAR-POS-VENDA.
      *
      *CARREGAR-PRE-VENDA SECTION.
      *    MOVE GS-CLIENTE      TO ALBUM-MTG
      *    READ MTD020 INVALID KEY
      *         CONTINUE
      *    NOT INVALID KEY
      *         IF FOGO-MTG = 1
      *            MOVE "ALBUM VENDIDO" TO GS-LINDET
      *            MOVE "INSERE-LIST" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *         ELSE
      *            MOVE ALL "-" TO GS-LINDET
      *            MOVE "INSERE-LIST" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *            MOVE DET-11 TO GS-LINDET
      *            MOVE "INSERE-LIST" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *            MOVE ALL "-" TO GS-LINDET
      *            MOVE "INSERE-LIST" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *            PERFORM CONT-CARREGAR-DADOS
      *            MOVE DET-01     TO GS-LINDET
      *            MOVE "INSERE-LIST" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *            MOVE ALL "-" TO GS-LINDET
      *            MOVE "INSERE-LIST" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *            MOVE DET-22 TO GS-LINDET
      *            MOVE "INSERE-LIST" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *            MOVE ALL "-" TO GS-LINDET
      *            MOVE "INSERE-LIST" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *            PERFORM CONT-CARREGAR-DADOS
      *            MOVE DET-02     TO GS-LINDET
      *            MOVE "INSERE-LIST" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM.
      *
      *CONT-CARREGAR-DADOS SECTION.
      *    MOVE GS-CLIENTE(1:4)     TO NR-CONTRATO-CO40
      *                                CONTRATO-MT19
      *    READ COD040 INVALID KEY
      *         MOVE SPACES TO IDENTIFICACAO-CO40.
      *
      *    MOVE GS-CLIENTE(5:4)     TO SEQ-MT19.
      *    READ MTD019 INVALID KEY
      *        MOVE SPACES TO NOME-FORM-MT19.
      *
      *    PERFORM LE-CIDADE
      *
      *    MOVE FONE-MT19           TO DET-FONE
      *    MOVE QT-ESTOJO-MTG       TO DET-QT-ESTOJO
      *    MOVE QT-ENCADER-MTG      TO DET-QT-ENCADER
      *    MOVE QT-FOLHAS-MTG       TO DET-QT-FOLHA
      *    MOVE QT-FOTOS-MTG        TO DET-QT-FOTO
      *    MOVE QT-FITAS-MTG        TO DET-QT-FITA
      *    MOVE QT-POSTER-MTG       TO DET-QT-POSTER
      *    MOVE QT-PORTA-FITA-MTG   TO DET-QT-PORTA-FITA
      *    EVALUATE FOGO-MTG
      *      WHEN 0 MOVE "0-Montagem    "    TO DET-FOGO
      *      WHEN 1 MOVE "1-Vendido     "    TO GS-FOGO
      *      WHEN 8 MOVE "8-Vendido-Fogo"    TO DET-FOGO
      *      WHEN 9 MOVE "9-Fogo        "    TO DET-FOGO
      *    END-EVALUATE
      *
      *    MOVE DATA-FOGO-MTG             TO DATA-INV
      *    CALL "GRIDAT1" USING DATA-INV
      *    MOVE DATA-INV                  TO DET-DATA-FOGO
      *    MOVE ANOMES-VISITA-MTG(1: 4)   TO MESANO-W(3: 4)
      *    MOVE ANOMES-VISITA-MTG(5: 2)   TO MESANO-W(1: 2)
      *    MOVE MESANO-W                  TO DET-ANOMES-VISITA
      *    MOVE VISITA-MTG                TO DET-VISITA.
      *    EVALUATE POSSE-MTG
      *      WHEN 1 MOVE "1-Estoque"      TO DET-POSSE
      *             MOVE CODIGO-POSSE-MTG TO CODIGO-MT02
      *             READ MTD002 INVALID KEY
      *                  MOVE SPACES TO NOME-MT02
      *             END-READ
      *             MOVE NOME-MT02        TO DET-DESC-POSSE
      *      WHEN 2 MOVE "2-Vendedor"     TO DET-POSSE
      *             MOVE CODIGO-POSSE-MTG TO CODIGO-CG01
      *             READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
      *             END-READ
      *             MOVE NOME-CG01        TO DET-DESC-POSSE
      *    END-EVALUATE.
      *
      *LE-CIDADE SECTION.
      *    MOVE CIDADE-MT19   TO CIDADE.
      *    READ CAD010 INVALID KEY
      *        MOVE SPACES TO NOME-CID.
      *
      *    MOVE NOME-CID    TO DET-NOME-CID.
      *    MOVE UF-CID      TO DET-ESTADO.
      *
      *CARREGAR-POS-VENDA SECTION.
      *    MOVE ZEROS                  TO ACP-REFERENTE
      *
      *    MOVE GS-CLIENTE             TO CLIENTE-CH10
      *    MOVE ZERO                   TO CLASS-CLIENTE-CH10
      *    MOVE ZEROS                  TO DATA-VENCTO-CH10
      *
      *    START CHD010 KEY IS NOT < ALT-CH4 INVALID KEY
      *          MOVE "10" TO ST-CHD010
      *    END-START
      *
      *    PERFORM CARREGAR-CHEQUE
      *
      *    MOVE GS-CLIENTE             TO CLIENTE-CR20
      *    MOVE ZERO                   TO CLASS-CLIENTE-CR20
      *    MOVE ZEROS                  TO SEQ-CR20
      *
      *    START CRD020 KEY IS NOT < CHAVE-CR20 INVALID KEY
      *          MOVE "10" TO ST-CRD020
      *    END-START
      *
      *    PERFORM CARREGAR-DUPLICATA
      *
      *    MOVE GS-CLIENTE             TO ALBUM-REC
      *
      *    START RCD100 KEY IS NOT < ALBUM-REC INVALID KEY
      *          MOVE "10" TO ST-RCD100
      *    END-START
      *
      *    PERFORM CARREGAR-VENDA.
      *
      *CARREGAR-CHEQUE SECTION.
      *
      *    MOVE "S" TO PRIMEIRA
      *
      *    PERFORM UNTIL ST-CHD010 = '10'
      *       READ CHD010 NEXT RECORD AT END
      *            MOVE "10"           TO ST-CHD010
      *        NOT AT END
      *         MOVE SPACES TO GS-LINDET
      *
      *         IF CLIENTE-CH10 <> GS-CLIENTE
      *            MOVE "10"           TO ST-CHD010
      *          ELSE
      *            IF PRIMEIRA = "S"
      *               PERFORM CABECALHO-CHEQUE
      *               MOVE "N" TO PRIMEIRA
      *            END-IF
      *            MOVE 1                 TO ACP-REFERENTE
      *            MOVE BANCO-CH10        TO GS-LINDET(01: 05)
      *            MOVE NR-CHEQUE-CH10    TO GS-LINDET(06: 08)
      *            MOVE DATA-VENCTO-CH10  TO DATA-INV
      *            CALL "GRIDAT1" USING DATA-INV
      *            MOVE DATA-INV          TO DATA-E
      *            MOVE DATA-E            TO GS-LINDET(14: 11)
      *            MOVE PORTADOR-CH10     TO GS-LINDET(25: 03)
      *            EVALUATE SITUACAO-CH10
      *                WHEN 0     MOVE "0-OK    " TO GS-LINDET(28: 9)
      *                WHEN 2     MOVE "2-RECEB." TO GS-LINDET(28: 9)
      *                WHEN 3     MOVE "3-ESTORN" TO GS-LINDET(28: 9)
      *                WHEN 4     MOVE "4-CANCEL" TO GS-LINDET(28: 9)
      *                WHEN 5     MOVE "5-DEVOLV" TO GS-LINDET(28: 9)
      *                WHEN 6     MOVE "6-PROBL." TO GS-LINDET(28: 9)
      *                WHEN OTHER MOVE "        " TO GS-LINDET(28: 9)
      *            END-EVALUATE
      *
      *            MOVE VALOR-CH10        TO WS-VALOR10
      *            MOVE WS-VALOR10        TO GS-LINDET(37: 11)
      *            MOVE VENDEDOR-CH10     TO CODIGO-CG01
      *            READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
      *            END-READ
      *            MOVE NOME-CG01         TO GS-LINDET(48: 10)
      *
      *            MOVE "INSERE-LIST" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *         END-IF
      *       END-READ
      *    END-PERFORM.
      *
      *CARREGAR-DUPLICATA SECTION.
      *    MOVE "S" TO PRIMEIRA
      *    PERFORM UNTIL ST-CRD020 = '10'
      *       READ CRD020 NEXT RECORD AT END
      *            MOVE "10"           TO ST-CRD020
      *        NOT AT END
      *         MOVE SPACES TO GS-LINDET
      *
      *         IF CLIENTE-CR20 <> GS-CLIENTE
      *            MOVE "10"           TO ST-CRD020
      *         ELSE
      *            IF PRIMEIRA = "S"
      *               PERFORM CABECALHO-DUPLICATA
      *               MOVE "N" TO PRIMEIRA
      *            END-IF
      *            MOVE 2                 TO ACP-REFERENTE
      *            MOVE NR-DOCTO-CR20     TO GS-LINDET(01: 11)
      *            EVALUATE TIPO-DOCTO-CR20
      *               WHEN 0     MOVE "0-DUPLICAT" TO GS-LINDET(12: 11)
      *               WHEN 1     MOVE "1-NR.PROM." TO GS-LINDET(12: 11)
      *               WHEN 2     MOVE "2-ORG.EVEN" TO GS-LINDET(12: 11)
      *               WHEN OTHER CONTINUE
      *            END-EVALUATE
      *
      *            MOVE DATA-EMISSAO-CR20 TO DATA-E
      *            MOVE DATA-E            TO GS-LINDET(23: 11)
      *
      *            MOVE DATA-VENCTO-CR20  TO DATA-INV
      *            CALL "GRIDAT1" USING DATA-INV
      *            MOVE DATA-INV          TO DATA-E
      *            MOVE DATA-E            TO GS-LINDET(34: 11)
      *
      *            MOVE VALOR-TOT-CR20    TO WS-VALOR10
      *            MOVE WS-VALOR10        TO GS-LINDET(45: 11)
      *            MOVE JURO-RCTO-CR20    TO WS-VALOR9
      *            MOVE WS-VALOR9         TO GS-LINDET(56: 10)
      *            MOVE MULTA-RCTO-CR20   TO WS-VALOR9
      *            MOVE WS-VALOR9         TO GS-LINDET(66: 10)
      *            MOVE DESCONTO-CR20     TO WS-VALOR9
      *            MOVE WS-VALOR9         TO GS-LINDET(76: 10)
      *            MOVE DATA-RCTO-CR20    TO DATA-E
      *            MOVE DATA-E            TO DATA-INV
      *            CALL "GRIDAT1" USING DATA-INV
      *            MOVE DATA-INV          TO DATA-E
      *            MOVE DATA-E            TO GS-LINDET(86: 11)
      *            MOVE VALOR-LIQ-CR20    TO WS-VALOR10
      *            MOVE WS-VALOR10        TO GS-LINDET(97: 10)
      *
      *            MOVE "INSERE-LIST" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *         END-IF
      *       END-READ
      *    END-PERFORM.
      *
      *CARREGAR-VENDA SECTION.
      *    MOVE "S" TO PRIMEIRA
      *    PERFORM UNTIL ST-RCD100 = '10'
      *       READ RCD100 NEXT RECORD AT END
      *            MOVE "10"           TO ST-RCD100
      *        NOT AT END
      *         MOVE SPACES TO GS-LINDET
      *         IF ALBUM-REC <> GS-CLIENTE
      *            MOVE "10"           TO ST-RCD100
      *         ELSE
      *            IF PRIMEIRA = "S"
      *               PERFORM CABECALHO-VENDA
      *               MOVE "N" TO PRIMEIRA
      *            END-IF
      *            IF ACP-REFERENTE = 0
      *               MOVE 3              TO ACP-REFERENTE
      *            END-IF
      *            MOVE DATAVEN-REC       TO DATA-INV
      *            CALL "GRIDAT1" USING DATA-INV
      *            MOVE DATA-INV          TO DATA-E
      *            MOVE DATA-E            TO GS-LINDET(01: 11)
      *            MOVE QENCADER-REC      TO GS-LINDET(12: 3)
      *            MOVE QFITAS-REC        TO GS-LINDET(15: 3)
      *            MOVE QPFITA-REC        TO GS-LINDET(18: 3)
      *            MOVE QFOTOS-REC        TO GS-LINDET(21: 4)
      *            MOVE QFOLHAS-REC       TO GS-LINDET(25: 4)
      *            MOVE VENDEDOR-REC      TO CODIGO-CG01
      *            READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
      *            END-READ
      *            MOVE NOME-CG01         TO GS-LINDET(29: 10)
      *            MOVE TOTAL-REC         TO WS-VALOR10
      *            MOVE WS-VALOR10        TO GS-LINDET(40: 10)
      *
      *            MOVE "INSERE-LIST" TO DS-PROCEDURE
      *            PERFORM CALL-DIALOG-SYSTEM
      *         END-IF
      *       END-READ
      *    END-PERFORM.
      *
      *--------------------------------------------------------------
      *CARREGAR-DADOS SECTION.
      *    READ ACD100 INVALID KEY INITIALIZE REG-ACD100.
      *    EVALUATE TIPO-AC100
      *        WHEN 1
      *             MOVE "1-ATENDIMENTO" TO GS-TIPO
      *        WHEN 2
      *             MOVE "2-COBRANÇA   " TO GS-TIPO
      *        WHEN 3
      *             MOVE "3-RECLAMAÇÃO " TO GS-TIPO
      *        WHEN 4
      *             MOVE "4-SOLICITAÇÃO" TO GS-TIPO
      *    END-EVALUATE
      *    MOVE CODIGO-AC100             TO GS-CODIGO
      *    PERFORM LE-CODIGO
      *
      *    EVALUATE REFERENTE-AC100
      *        WHEN 1
      *             MOVE "1-CHEQUE     " TO GS-REFERENTE
      *        WHEN 2
      *             MOVE "2-DUPLICATA  " TO GS-REFERENTE
      *        WHEN 3
      *             MOVE "3-VENDA      " TO GS-REFERENTE
      *        WHEN OTHER
      *             CONTINUE
      *    END-EVALUATE
      *
      *    MOVE CLIENTE-AC100            TO GS-CLIENTE
      *    PERFORM LE-CLIENTE
      *
      *    EVALUATE STATUS-AC100
      *        WHEN 1
      *             MOVE "1-PENDENTE     " TO GS-STATUS
      *        WHEN 2
      *             MOVE "2-EXECUTADO    " TO GS-STATUS
      *        WHEN 3
      *             MOVE "3-AUTORIZADO   " TO GS-STATUS
      *        WHEN 3
      *             MOVE "4-SEM PENDENCIA" TO GS-STATUS
      *        WHEN OTHER
      *             CONTINUE
      *    END-EVALUATE.
      *
      *    PERFORM CARREGAR-DISPLAY
      *
      *    PERFORM INICIALIZA-CHAVE-AC101
      *
      *    MOVE 1                          TO WS-COL
      *    PERFORM CARREGAR-ANOTACAO UNTIL FIM-ARQUIVO.
      *
      *CARREGAR-ANOTACAO SECTION.
      *    READ ACD101 NEXT RECORD AT END
      *         SET FIM-ARQUIVO           TO TRUE
      *     NOT AT END
      *         IF DATAMOV-AC101 <> DATAMOV-AC100 OR
      *            SEQ-AC101 <> SEQ-AC100
      *                SET FIM-ARQUIVO    TO TRUE
      *         ELSE
      *            MOVE ANOTACAO-AC101    TO GS-ANOTACAO(WS-COL: CT-N80)
      *            ADD CT-N80             TO WS-COL
      *         END-IF
      *    END-READ.
      *---------------------------------------------------------------
      *CARREGA-MENSAGEM-ERRO SECTION.
      *    PERFORM LOAD-SCREENSET
      *    MOVE "EXIBE-ERRO" TO DS-PROCEDURE
      *    PERFORM CALL-DIALOG-SYSTEM.
      *    MOVE 1 TO ERRO-W.
      *
      *LIMPAR-DADOS SECTION.
      *    MOVE GS-DATA-MOVTO    TO DATA-MOVTO-W
      *    MOVE GS-SEQ           TO AUX-SEQ
      *    MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE.
      *    PERFORM CALL-DIALOG-SYSTEM
      *
      *    INITIALIZE REG-ACD100
      *    INITIALIZE GS-DATA-BLOCK
      *    MOVE ULT-SEQ           TO GS-SEQ
      *    MOVE DATA-MOVTO-W      TO GS-DATA-MOVTO.
      *    PERFORM SET-UP-FOR-REFRESH-SCREEN.
      *    PERFORM CALL-DIALOG-SYSTEM.
      *
      *EXCLUI SECTION.
      *    DELETE ACD100.
      *    PERFORM LIMPAR-DADOS.
      *
      *    PERFORM INICIALIZA-CHAVE-AC101
      *
      *    PERFORM EXCLUI-ANOTACAO UNTIL FIM-ARQUIVO.
      *
      *INICIALIZA-CHAVE-AC101 SECTION.
      *    MOVE DATAMOV-AC100         TO DATAMOV-AC101
      *    MOVE SEQ-AC100             TO SEQ-AC101
      *    MOVE ZEROS                 TO SEQ-REG-AC101
      *
      *    START ACD101 KEY IS NOT < CHAVE-AC101
      *       INVALID KEY
      *             SET FIM-ARQUIVO   TO TRUE
      *       NOT INVALID KEY
      *             SET NO-FIM-ARQUIVO TO TRUE
      *    END-START.
      *
      *EXCLUI-ANOTACAO SECTION.
      *    READ ACD101 NEXT RECORD AT END
      *         SET FIM-ARQUIVO       TO TRUE
      *      NOT AT END
      *         IF DATAMOV-AC101    <> DATAMOV-AC100 OR
      *            SEQ-AC101        <> SEQ-AC100
      *            SET FIM-ARQUIVO    TO TRUE
      *         ELSE
      *            DELETE ACD101
      *         END-IF
      *    END-READ.
      *
       CLEAR-FLAGS SECTION.
           INITIALIZE FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "ACP100" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-----------------------------------------------------

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE ACD010 ACD020 ACD100 ACD101 CGD001 CGD010 RCD100 CHD010
                 COD040 CRD020 MTD002 MTD019 MTD020 CAD010 CRD200 CRD201
                 ACD102 ACD103 ACD104 ACD105 ACD106 CGD013
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

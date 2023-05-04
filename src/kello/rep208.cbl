       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REP208.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 18/10/2000.
      *FUNÇÃO: CONSULTA de Eventos

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY COPX003.
           COPY COPX040.
           COPY COPX041.
           COPY COPX060.
           COPY COPX061.
           COPY CGPX010.
           COPY CGPX012.
           COPY IEPX011.
           COPY REPX100.
           COPY REPX101.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY COPW003.
       COPY COPW040.
       COPY COPW041.
       COPY COPW060.
       COPY COPW061.
       COPY CGPW010.
       COPY CGPW012.
       COPY IEPW011.
       COPY REPW100.
       COPY REPW101.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "REP208.CPB".
           COPY "REP208.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD012             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD041             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-COD060             PIC XX       VALUE SPACES.
           05  ST-COD061             PIC XX       VALUE SPACES.
           05  ST-RED100             PIC XX       VALUE SPACES.
           05  ST-RED101             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
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
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  LETRA                 PIC X        VALUE SPACES.
           05  LETRA1                PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           05  VALOR-E               PIC ZZZ.ZZZ,ZZ.
           05  VALOR-E1              PIC ZZ.ZZZ.ZZZ,ZZ.
           05  I                     PIC 99       VALUE ZEROS.
           05  L                     PIC 99       VALUE ZEROS.
           05  K                     PIC 99       VALUE ZEROS.
           05  J                     PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-E              PIC 99/9999.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(76)   VALUE
           "CONFERENCIA DO MOVIMENTO DE EVENTO".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(130)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONT IT  EVENTO                PART T DT-REALIZ. DT-SOLICIT
      -    "F V C B F A COMI HORARIO    LOCAL                      ".

       01  CAB05.
           05  FILLER              PIC X(130)  VALUE
           "ENDERECO                       REFERENCIA
      -    "  PESSOA-CONTATO                 UNIFORME             ORGANI
      -    "ZADOR   ".
       01  CAB06.
           05  FILLER              PIC X(130)  VALUE
           "OBSERVACAO
      -    "                     DATA-CANC. HORA  NR-PLAN. NR-REP.".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.
      * ------------- IMPRESSÃO INDIVIDUAL ----------------------
       01  LINDET01.
           05  FILLER              PIC X(77)  VALUE
           "MOVIMENTO DE EVENTO INDIVIDUAL ".
           05  FILLER              PIC X(14)  VALUE "DATA EMISSAO: ".
           05  EMISSAO1-REL        PIC 99/99/9999.
       01  LINDET02.
           05  FILLER              PIC X(101) VALUE ALL "=".
       01  LINDET03.
           05  FILLER              PIC X(15)  VALUE "CONTRATO/ITEM: ".
           05  CONTRATO-REL        PIC Z(4)   BLANK WHEN ZEROS.
           05  FILLER              PIC X      VALUE "/".
           05  ITEM-REL            PIC Z(3)   BLANK WHEN ZEROS.
           05  FILLER              PIC XX     VALUE SPACES.
           05  DESC-CONTRATO-REL   PIC X(57)  VALUE SPACES.
           05  FILLER              PIC X(12)  VALUE "COD.EVENTO: ".
           05  EVENTO-REL          PIC ZZZZZ  BLANK WHEN ZEROS.
           05  FILLER              PIC X      VALUE SPACES.
           05  NOME-EVENTO-REL     PIC X(15)  VALUE SPACES.
       01  LINDET04.
           05  FILLER              PIC X(15)  VALUE "QTDE PARTICIP: ".
           05  QTDE-PARTIC-REL     PIC Z(4)   BLANK WHEN ZEROS.
           05  FILLER              PIC X(37)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "ORGANIZADOR:   ".
           05  ORGANIZADOR-REL     PIC X(15)  VALUE SPACES.
       01  LINDET05.
           05  FILLER              PIC X(15)  VALUE "DATA REALIZAC: ".
           05  DATA-REALIZ-REL     PIC ZZ/ZZ/ZZZZ BLANK WHEN ZEROS.
           05  FILLER              PIC X(31)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "DATA SOLICIT.: ".
           05  DATA-SOLICIT-REL    PIC ZZ/ZZ/ZZZZ BLANK WHEN ZEROS.
       01  LINDET06.
           05  FILLER              PIC X(15)  VALUE "SOLICITANTE..: ".
           05  SOLICITANTE-REL     PIC Z(4)   BLANK WHEN ZEROS.
           05  FILLER              PIC X      VALUE SPACES.
           05  NOME-SOLICIT-REL    PIC X(36)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "QTDE TELAO...: ".
           05  QTDE-TELAO-REL      PIC Z      BLANK WHEN ZEROS.
       01  LINDET07.
           05  FILLER              PIC X(15)  VALUE "FOTO.........: ".
           05  FOTO-REL            PIC X(41)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "VIDEO........: ".
           05  VIDEO-REL           PIC X(30)  VALUE SPACES.
       01  LINDET08.
           05  FILLER              PIC X(15)  VALUE "BECA.........: ".
           05  BECA-REL            PIC X(41)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "CLIP.........: ".
           05  CLIP-REL            PIC X(30)  VALUE SPACES.
       01  LINDET09.
           05  FILLER              PIC X(15)  VALUE "FAX..........: ".
           05  FAX-REL             PIC X(41)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "APROVACAO....: ".
           05  APROVACAO-REL       PIC X(30)  VALUE SPACES.
       01  LINDET10.
           05  FILLER              PIC X(15)  VALUE "NR.PLANEJAM..: ".
           05  NR-PLANEJ-REL       PIC ZZZZ.ZZZZ.
           05  FILLER              PIC X(32)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "NR.REL.REPORT: ".
           05  NR-REL-REPORT-REL   PIC ZZZ.ZZZ.
       01  LINDET11.
           05  FILLER              PIC X(15)  VALUE "OBSERVACAO...: ".
           05  OBS-REL             PIC X(80)  VALUE SPACES.
       01  LINDET12.
           05  FILLER              PIC X(15)  VALUE "LOCAL........: ".
           05  LOCAL-REL           PIC X(41)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "HORARIO......: ".
           05  HORARIO-REL         PIC X(30)  VALUE SPACES.
       01  LINDET13.
           05  FILLER              PIC X(15)  VALUE "ENDERECO.....: ".
           05  ENDERECO-REL        PIC X(41)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "PONTO REFEREN: ".
           05  PONTO-REFER-REL     PIC X(30)  VALUE SPACES.
       01  LINDET14.
           05  FILLER              PIC X(15)  VALUE "PESSOA CONTAT: ".
           05  PESSOA-CONT-REL     PIC X(41)  VALUE SPACES.
           05  FILLER              PIC X(15)  VALUE "UNIFORME.....: ".
           05  UNIFORME-REL        PIC X(30)  VALUE SPACES.

           copy impressora.

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD012.
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD041" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD041.
           MOVE "COD060" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD060.
           MOVE "COD061" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD061.
           MOVE "IED011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "RED100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "RED101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RED101.
           OPEN INPUT COD060 COD061 IED011 COD003 CGD010 COD041 CGD012
                      CAD010 COD040 RED101 RED100.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD012 <> "00"
              MOVE "ERRO ABERTURA CGD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD041 <> "00"
              MOVE "ERRO ABERTURA COD041: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD041 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD060 <> "00"
              MOVE "ERRO ABERTURA COD060: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD060 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD061 <> "00"
              MOVE "ERRO ABERTURA COD061: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD061 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED100 <> "00"
              MOVE "ERRO ABERTURA RED100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RED101 <> "00"
              MOVE "ERRO ABERTURA RED101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RED101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                   copy impressora.chama.
                   if lnk-mapeamento <> spaces
                      PERFORM IMPRIME-RELATORIO
                   end-if
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   PERFORM CARREGAR-CURSOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 4)  TO NR-CONTRATO-CO60
                   MOVE GS-LINDET(6: 3)  TO ITEM-CO60
                                            GS-NR-ITEM
                   PERFORM CARREGAR-DADOS
                   PERFORM CARREGAR-CURSOS
               WHEN GS-IMPRIME-TELA-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-TELA
                    end-if
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGAR-DADOS SECTION.
           START COD060 KEY IS = CHAVE-CO60 INVALID KEY
                 CONTINUE.
           READ COD060 INVALID KEY
                 INITIALIZE REG-COD060.

           MOVE NR-CONTRATO-CO60     TO  GS-CONTRATO
                                         NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                INITIALIZE REG-COD040
           END-READ
           MOVE ITEM-CO60            TO  GS-NR-ITEM
           MOVE CODEVENTO-CO60       TO  CODIGO-CO03
           READ COD003 INVALID KEY
                MOVE "*******" TO NOME-CO03.
           MOVE NOME-CO03            TO  GS-NOME-EVENTO
           MOVE CODIGO-CO03          TO  GS-COD-EVENTO
           MOVE QT-PARTICIPANTE-CO60 TO  GS-NR-PARTICIPANTES
           MOVE QT-TELAO-CO60        TO  GS-QTDE-TELAO
           MOVE DATAREALIZA-CO60     TO  DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO  GS-DATA-REALIZACAO
           MOVE DATA-SOLIC-CO60      TO  GS-DATA-SOLICIT
           EVALUATE FOTO-CO60
             WHEN 0 MOVE "0-Não"     TO GS-FOTO
             WHEN 1 MOVE "1-Sim"     TO GS-FOTO
           END-EVALUATE.
           EVALUATE VIDEO-CO60
             WHEN 0 MOVE "0-Não"     TO GS-VIDEO
             WHEN 1 MOVE "1-Sim"     TO GS-VIDEO
           END-EVALUATE.
           EVALUATE BECA-CO60
             WHEN 0 MOVE "0-Não"     TO GS-BECA
             WHEN 1 MOVE "1-Sim"     TO GS-BECA
           END-EVALUATE.
           EVALUATE CLIP-CO60
             WHEN 0 MOVE "0-Não"     TO GS-CLIP
             WHEN 1 MOVE "1-Sim"     TO GS-CLIP
           END-EVALUATE.
           EVALUATE FAX-CO60
             WHEN 0 MOVE "0-Não"     TO GS-FAX
             WHEN 1 MOVE "1-Sim"     TO GS-FAX
           END-EVALUATE.
           EVALUATE APROVACAO-CO60
             WHEN 0 MOVE "0-Não"     TO GS-APROVACAO
             WHEN 1 MOVE "1-Sim"     TO GS-APROVACAO
           END-EVALUATE.
           MOVE 0                    TO CLASSIF-CG10
           MOVE NR-CONTRATO-CO60     TO CODIGO-CG10(1: 4).
           PERFORM DESCRICAO-CONTRATO.
           MOVE COD-COMISSAO-CO60    TO GS-SOLICITANTE
                CODIGO-CG10(5: 4)
           READ CGD010 INVALID KEY MOVE SPACES TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10       TO GS-NOME-SOLICITANTE.
           MOVE HORARIO-CO60         TO GS-HORARIO.
           MOVE LOCAL-CO60           TO GS-LOCAL
           MOVE ENDERECO-CO60        TO GS-ENDERECO
           MOVE REFERENCIA-CO60      TO GS-PONTO-REFERENCIA
           MOVE PESSOA-CONTATO-CO60  TO GS-PESSOA-CONTATO
           MOVE UNIFORME-CO60        TO GS-UNIFORME
           MOVE ORGANIZADOR-CO60     TO GS-ORGANIZADOR
           MOVE OBSERVACAO-CO60      TO GS-OBSERVACAO
           MOVE NR-PLANEJ-CO60       TO GS-NR-PLANEJAMENTO
           MOVE NR-REL-REPOR-CO60    TO GS-NR-REL-REPORT
           MOVE DATA-CANCELAM-CO60   TO GS-DATA-CANCELAMENTO
           MOVE HORA-CANCELAM-CO60   TO GS-HORA-CANCELAMENTO

           refresh-object principal.

      *ABORTADO ATÉ COLOCAR UM ITEM NO RED101

      *    PERFORM CALCULAR-QTDE-REAL.

      *CALCULAR-QTDE-REAL SECTION.
      *    MOVE ZEROS TO GS-NR-PARTICIPANTES-REL
      *
      *    INITIALIZE REG-RED101
      *    MOVE NR-CONTRATO-CO60 TO CONTRATO-R101
      *    START RED101 KEY IS NOT LESS CONTRATO-R101 INVALID KEY
      *        MOVE "10" TO ST-RED101.
      *
      *    PERFORM UNTIL ST-RED101 = "10"
      *        READ RED101 NEXT AT END
      *             MOVE "10" TO ST-RED101
      *        NOT AT END
      *             IF NR-CONTRATO-CO60 <> CONTRATO-R101
      *                MOVE "10" TO ST-RED101
      *             ELSE
      *                IF CODEVENTO-CO60 = EVENTO-R101
      *                   MOVE DOCTO-R101 TO DOCTO-R100
      *                   READ RED100 INVALID KEY
      *                        INITIALIZE REG-RED100
      *                   END-READ
      *                   IF DATA-MOV-R100
      *                END-IF
      *             END-IF
      *        END-READ
      *    END-PERFORM.

       DESCRICAO-CONTRATO SECTION.
           READ COD040 INVALID KEY INITIALIZE REG-COD040.
           MOVE IDENTIFICACAO-CO40   TO GS-DESC-CONTRATO(1: 21)
           MOVE CIDADE-CO40          TO CIDADE
           READ CAD010 INVALID KEY INITIALIZE REG-CAD010.
           MOVE NOME-CID             TO GS-DESC-CONTRATO(22: 14)
           MOVE MESANO-PREV-CO40(1: 4)    TO MESANO-W(3: 4)
           MOVE MESANO-PREV-CO40(5: 2)    TO MESANO-W(1: 2)
           MOVE MESANO-W             TO MESANO-E
           MOVE MESANO-E             TO GS-DESC-CONTRATO(36: 8)
           MOVE QTDE-FORM-CO40       TO GS-DESC-CONTRATO(44: 6)
           MOVE PADRAO-CO40          TO GS-DESC-CONTRATO(50: 1).
       CARREGAR-CURSOS SECTION.
           MOVE GS-CONTRATO          TO NR-CONTRATO-CO41.
           MOVE ZEROS                TO CURSO-CO41
           MOVE SPACES               TO TURMA-CO41.
           START COD041 KEY IS NOT < CHAVE-CO41 INVALID KEY
                 MOVE "10" TO ST-COD041.
           MOVE ZEROS TO GS-CONT-TURMA.
           PERFORM UNTIL ST-COD041 = "10"
             READ COD041 NEXT RECORD AT END MOVE "10" TO ST-COD041
               NOT AT END
                 IF NR-CONTRATO-CO41 <> GS-CONTRATO
                    MOVE "10" TO ST-COD041
                 ELSE
                    MOVE CURSO-CO41   TO CODIGO-IE11
                                         GS-LINDET-TURMA(1: 3)
                    READ IED011 INVALID KEY MOVE SPACES TO NOME-IE11
                    END-READ
                    MOVE NOME-IE11    TO GS-LINDET-TURMA(5: 31)
                    MOVE TURMA-CO41   TO GS-LINDET-TURMA(36: 3)
                    MOVE NR-PREV-FORM-CO41 TO GS-LINDET-TURMA(39: 4)
                    MOVE GS-CONTRATO      TO NR-CONTRATO-CO61
                    MOVE GS-NR-ITEM       TO ITEM-CO61
                    MOVE TURMA-CO41       TO TURMA-CO61
                    MOVE CURSO-CO41       TO CURSO-CO61
                    READ COD061 INVALID KEY MOVE 0 TO GS-ESTADO-SELECAO
                     NOT INVALID KEY MOVE 1 TO GS-ESTADO-SELECAO
                    END-READ
                    MOVE "INSERE-LIST-TURMA" TO DS-PROCEDURE
                    PERFORM CALL-DIALOG-SYSTEM
                 END-IF
             END-READ
           END-PERFORM.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-CONTRATO     TO NR-CONTRATO-CO60.
           MOVE ZEROS           TO ITEM-CO60 GS-NR-ITEM.
           START COD060 KEY IS NOT < CHAVE-CO60
                    INVALID KEY MOVE "10" TO ST-COD060.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-COD060 = "10"
              READ COD060 NEXT RECORD AT END MOVE "10" TO ST-COD060
              NOT AT END
                IF NR-CONTRATO-CO60 <> GS-CONTRATO
                   MOVE "10" TO ST-COD060
                ELSE
                   PERFORM MOVER-DADOS-LISTA
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                END-IF
              END-READ
           END-PERFORM.
      *    ADD 1 TO GS-NR-ITEM.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES               TO GS-LINDET
           MOVE NR-CONTRATO-CO60     TO GS-LINDET(1: 5)
           MOVE ITEM-CO60            TO GS-LINDET(6: 4)
           MOVE CODEVENTO-CO60       TO CODIGO-CO03
           READ COD003 INVALID KEY MOVE "*******" TO NOME-CO03.
           MOVE NOME-CO03            TO GS-LINDET(10: 24)
           MOVE QT-PARTICIPANTE-CO60 TO GS-LINDET(34: 5)
           MOVE DATAREALIZA-CO60     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO DATA-E
           MOVE DATA-E               TO GS-LINDET(39: 11)
           MOVE DATA-SOLIC-CO60      TO DATA-E
           MOVE DATA-E               TO GS-LINDET(50: 11)
           EVALUATE FOTO-CO60
             WHEN 0 MOVE "N"         TO GS-LINDET(61: 3)
             WHEN 1 MOVE "S"         TO GS-LINDET(61: 3)
           END-EVALUATE.
           EVALUATE VIDEO-CO60
             WHEN 0 MOVE "N"         TO GS-LINDET(64: 3)
             WHEN 1 MOVE "S"         TO GS-LINDET(64: 3)
           END-EVALUATE.
           EVALUATE BECA-CO60
             WHEN 0 MOVE "N"         TO GS-LINDET(67: 3)
             WHEN 1 MOVE "S"         TO GS-LINDET(67: 3)
           END-EVALUATE.
           EVALUATE CLIP-CO60
             WHEN 0 MOVE "N"         TO GS-LINDET(70: 3)
             WHEN 1 MOVE "S"         TO GS-LINDET(70: 3)
           END-EVALUATE.
           EVALUATE FAX-CO60
             WHEN 0 MOVE " N"        TO GS-LINDET(73: 4)
             WHEN 1 MOVE " S"        TO GS-LINDET(73: 4)
           END-EVALUATE.
           EVALUATE APROVACAO-CO60
             WHEN 0 MOVE "N"         TO GS-LINDET(78: 3)
             WHEN 1 MOVE "S"         TO GS-LINDET(78: 3)
           END-EVALUATE.
           MOVE QT-TELAO-CO60        TO GS-LINDET(81: 4)
           MOVE ORGANIZADOR-CO60     TO GS-LINDET(84: 15).
      *-----------------------------------------------------------
       IMPRIME-TELA SECTION.

           copy condensa.

           MOVE GS-CONTRATO          TO CONTRATO-REL
           MOVE GS-NR-ITEM           TO ITEM-REL
           MOVE GS-DESC-CONTRATO     TO DESC-CONTRATO-REL.
           MOVE GS-COD-EVENTO        TO EVENTO-REL
           MOVE GS-NOME-EVENTO       TO NOME-EVENTO-REL
           MOVE GS-NR-PARTICIPANTES  TO QTDE-PARTIC-REL
           MOVE GS-ORGANIZADOR       TO ORGANIZADOR-REL
           MOVE GS-DATA-REALIZACAO   TO DATA-REALIZ-REL
           MOVE GS-DATA-SOLICIT      TO DATA-SOLICIT-REL
           MOVE GS-SOLICITANTE       TO SOLICITANTE-REL
           MOVE GS-NOME-SOLICITANTE  TO NOME-SOLICIT-REL
           MOVE GS-QTDE-TELAO        TO QTDE-TELAO-REL
           MOVE GS-FOTO              TO FOTO-REL
           MOVE GS-VIDEO             TO VIDEO-REL
           MOVE GS-BECA              TO BECA-REL
           MOVE GS-CLIP              TO CLIP-REL
           MOVE GS-FAX               TO FAX-REL
           MOVE GS-APROVACAO         TO APROVACAO-REL
           MOVE GS-NR-PLANEJAMENTO   TO NR-PLANEJ-REL
           MOVE GS-NR-REL-REPORT     TO NR-REL-REPORT-REL
           MOVE GS-OBSERVACAO        TO OBS-REL
           MOVE GS-LOCAL             TO LOCAL-REL
           MOVE GS-HORARIO           TO HORARIO-REL
           MOVE GS-ENDERECO          TO ENDERECO-REL
           MOVE GS-PONTO-REFERENCIA  TO PONTO-REFER-REL
           MOVE GS-PESSOA-CONTATO    TO PESSOA-CONT-REL
           MOVE GS-UNIFORME          TO UNIFORME-REL
           WRITE REG-RELAT FROM LINDET01.
           WRITE REG-RELAT FROM LINDET02.
           WRITE REG-RELAT FROM LINDET03.
           WRITE REG-RELAT FROM LINDET04.
           WRITE REG-RELAT FROM LINDET05.
           WRITE REG-RELAT FROM LINDET06.
           WRITE REG-RELAT FROM LINDET07.
           WRITE REG-RELAT FROM LINDET08.
           WRITE REG-RELAT FROM LINDET09.
           WRITE REG-RELAT FROM LINDET10.
           WRITE REG-RELAT FROM LINDET11.
           WRITE REG-RELAT FROM LINDET12.
           WRITE REG-RELAT FROM LINDET13.
           WRITE REG-RELAT FROM LINDET14.

           copy descondensa.

      *-----------------------------------------------------------

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "REP208" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.

           MOVE ZEROS TO PAG-W.
           MOVE GS-CONTRATO    TO NR-CONTRATO-CO60.
           MOVE GS-NR-ITEM     TO ITEM-CO60.
           START COD060 KEY IS = CHAVE-CO60 INVALID KEY
                 MOVE "10" TO ST-COD060.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-COD060 = "10"
             READ COD060 NEXT RECORD AT END MOVE "10" TO ST-COD060
              NOT AT END
                IF NR-CONTRATO-CO60 <> GS-CONTRATO
                         MOVE "10" TO ST-COD060
                ELSE
                  PERFORM MOVER-DADOS-REL
                END-IF
             END-READ
           END-PERFORM.

           MOVE SPACES TO REG-RELAT.
           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.
       MOVER-DADOS-REL SECTION.
           ADD 4 TO LIN
           IF LIN > 56 PERFORM CABECALHO.
           MOVE NR-CONTRATO-CO60     TO LINDET-REL(1: 5)
           MOVE ITEM-CO60            TO LINDET-REL(6: 4)
           MOVE CODEVENTO-CO60       TO CODIGO-CO03
           READ COD003 INVALID KEY MOVE SPACES TO NOME-CO03.
           MOVE NOME-CO03            TO LINDET-REL(9: 22)
           MOVE QT-PARTICIPANTE-CO60 TO LINDET-REL(31: 5)
           MOVE QT-TELAO-CO60        TO LINDET-REL(35: 2)
           MOVE DATAREALIZA-CO60     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV             TO LINDET-REL(37: 11)
           MOVE DATA-SOLIC-CO60      TO LINDET-REL(48: 11)
           MOVE FOTO-CO60            TO LINDET-REL(59: 2)
           MOVE VIDEO-CO60           TO LINDET-REL(61: 2)
           MOVE CLIP-CO60            TO LINDET-REL(63: 2)
           MOVE BECA-CO60            TO LINDET-REL(65: 2)
           MOVE FAX-CO60             TO LINDET-REL(67: 2)
           MOVE APROVACAO-CO60       TO LINDET-REL(69: 2)
           MOVE COD-COMISSAO-CO60    TO LINDET-REL(71: 5)
           MOVE HORARIO-CO60         TO LINDET-REL(76: 11)
           MOVE LOCAL-CO60           TO LINDET-REL(87: 30)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES TO LINDET-REL
           MOVE ENDERECO-CO60(1: 30) TO LINDET-REL(01: 31)
           MOVE REFERENCIA-CO60      TO LINDET-REL(32: 31)
           MOVE PESSOA-CONTATO-CO60  TO LINDET-REL(63: 31)
           MOVE UNIFORME-CO60        TO LINDET-REL(94: 21)
           MOVE ORGANIZADOR-CO60     TO LINDET-REL(115: 15)
           WRITE REG-RELAT FROM LINDET


           MOVE SPACES TO LINDET-REL
           MOVE OBSERVACAO-CO60      TO LINDET-REL(01: 81)
           MOVE DATA-CANCELAM-CO60   TO LINDET-REL(82: 11)
           MOVE HORA-CANCELAM-CO60   TO LINDET-REL(93: 6).
           MOVE NR-PLANEJ-CO60       TO LINDET-REL(100: 9)
           MOVE NR-REL-REPOR-CO60    TO LINDET-REL(109: 7)
           WRITE REG-RELAT FROM LINDET
           MOVE SPACES TO LINDET-REL
           WRITE REG-RELAT FROM LINDET.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB05.
           WRITE REG-RELAT FROM CAB06.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD040
                 COD003 COD060 COD061 IED011 CGD010 COD041 CGD012
                 RED101 RED100.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

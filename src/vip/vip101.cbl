       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIP101.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 28/07/2000.
      *FUNÇÃO: Movimento de RECEBIMENTO DE AVALIACAO DE CINEGRAFISTAS

       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY COPX003.
           COPY CGPX001.
           COPY VIPX021.
           COPY VIPX022.
           COPY VIPX100.
           COPY VIPX101.
           COPY VIPX102.
           COPY VIPX103.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY COPW003.
       COPY CGPW001.
       COPY VIPW021.
       COPY VIPW022.
       COPY VIPW100.
       COPY VIPW101.
       COPY VIPW102.
       COPY VIPW103.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "VIP101.CPB".
           COPY "VIP101.CPY".
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
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-VID021             PIC XX       VALUE SPACES.
           05  ST-VID022             PIC XX       VALUE SPACES.
           05  ST-VID100             PIC XX       VALUE SPACES.
           05  ST-VID101             PIC XX       VALUE SPACES.
           05  ST-VID102             PIC XX       VALUE SPACES.
           05  ST-VID103             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  IDENTIFICADOR-W       PIC 9(9)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  TEMPO-E               PIC ZZZB99B99 BLANK WHEN ZEROS.
           05  OBS-W1                PIC X(78)    VALUE SPACES.
           05  OBS-W                 PIC X(100)   VALUE SPACES.
           05  I                     PIC 9(3)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
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
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  FILLER              PIC X(63)   VALUE
           "AVALIACAO DE CINEGRAFISTA                     ".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.

       02  LINHA-01                          PIC  X(079) VALUE ALL "_".
       02  LINHA-02.
           05 FILLER                         PIC  X(055) VALUE
              "| Avaliacao da Fita                                    ".
           05 FILLER                         PIC  X(028) VALUE
              "                           |".
       02  LINHA-03.
           05 FILLER                         PIC  X(055) VALUE
              "|                                                      ".
           05 FILLER                         PIC  X(028) VALUE
              "                           |".
       02  LINHA-04.
           05 FILLER                         PIC  X(006) VALUE "|CONT ".
           05 FILLER                         PIC  X(050) VALUE
              "N.FITA REVISOR                        DATA-REVIS. ".
           05 FILLER                         PIC  X(033) VALUE
              "AVALIACAO-GERAL           |".
       02  LINHA-05.
           05 FILLER                         PIC  X(055) VALUE
              "|======================================================".
           05 FILLER                         PIC  X(028) VALUE
              "===========================|".
       02  LINHA-06.
           05 FILLER                         PIC  X(002) VALUE "| ".
           05 CONTRATO-REL                   PIC  9999.
           05 FILLER                         PIC  X(01).
           05 NR-FITA-REL                    PIC  ZZ.ZZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 REVISOR-REL                    PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 DATA-REVISAO-REL               PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 AVAL-GERAL-REL                 PIC  X(009) VALUE SPACES.
           05 FILLER                         PIC  X(018) VALUE
              "                 |".
       02  LINHA-07.
           05 FILLER                         PIC  X(055) VALUE
              "|OBSERVACOES                                           ".
           05 FILLER                         PIC  X(028) VALUE
              "                           |".
       02  LINHA-08.
           05 FILLER                         PIC  X(055) VALUE
              "|===========                                           ".
           05 FILLER                         PIC  X(028) VALUE
              "                           |".
       02  LINHA-09.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 OBS-REL                        PIC  X(081) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-10.
           05 FILLER                         PIC  X(055) VALUE
              "|Avaliacao do Problema/Momento                         ".
           05 FILLER                         PIC  X(028) VALUE
              "                           |".
       02  LINHA-11.
           05 FILLER                         PIC  X(055) VALUE
              "|SEQ MOMENTO              PROBLEMA                     ".
           05 FILLER                         PIC  X(028) VALUE
              "  TEMPO(H/M/S)             |".
       02  LINHA-12.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 SEQ-REL                        PIC  Z(003) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 MOMENTO-REL                    PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 PROBLEMA-REL                   PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 TEMPO-REL                      PIC  ZZZB99B99
              BLANK WHEN ZEROS.
           05 FILLER                         PIC  X(017) VALUE
              "                |".
       02  LINHA-13.
           05 FILLER                         PIC  X(055) VALUE
              "|______________________________________________________".
           05 FILLER                         PIC  X(028) VALUE
              "___________________________|".

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
           MOVE DATA-INV TO DATA-MOVTO-W
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "VID021" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID021.
           MOVE "VID022" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID022.
           MOVE "VID100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID100.
           MOVE "VID101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID101.
           MOVE "VID102" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID102.
           MOVE "VID103" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID103.
           OPEN I-O VID101 VID102 VID103.
           OPEN INPUT CGD001 COD003 VID021 VID022 VID100.
           IF ST-VID101 = "35"
              CLOSE VID101      OPEN OUTPUT VID101
              CLOSE VID101      OPEN I-O VID101
           END-IF.
           IF ST-VID102 = "35"
              CLOSE VID102      OPEN OUTPUT VID102
              CLOSE VID102      OPEN I-O VID102
           END-IF.
           IF ST-VID103 = "35"
              CLOSE VID103      OPEN OUTPUT VID103
              CLOSE VID103      OPEN I-O VID103
           END-IF.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID022 <> "00"
              MOVE "ERRO ABERTURA VID022: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID022 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID021 <> "00"
              MOVE "ERRO ABERTURA VID021: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID100 <> "00"
              MOVE "ERRO ABERTURA VID100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID101 <> "00"
              MOVE "ERRO ABERTURA VID101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID101 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID102 <> "00"
              MOVE "ERRO ABERTURA VID102: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID102 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID103 <> "00"
              MOVE "ERRO ABERTURA VID103: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID103 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FITA-TRUE
                   PERFORM SALVAR-FITA
               WHEN GS-SAVE-PROB-MOM-TRUE
                   PERFORM SALVAR-PROB-MOM
               WHEN GS-EXCLUI-FITA-TRUE
                   PERFORM EXCLUIR-FITA
               WHEN GS-EXCLUI-PROB-MOM-TRUE
                    PERFORM EXCLUIR-PROB-MOM
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-CONTRATO      TO CONTRATO-V102
                   MOVE GS-NR-FITA       TO NR-FITA-V102
                   MOVE GS-LINDET(1: 3)  TO SEQ-V102
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-FITA-TRUE
                   PERFORM LE-NR-FITA
                   PERFORM VERIFICA-NR-FITA
               WHEN GS-LE-REVISOR-TRUE
                   PERFORM LE-REVISOR
               WHEN GS-LE-MOMENTO-TRUE
                   PERFORM LE-MOMENTO
               WHEN GS-LE-PROBLEMA-TRUE
                   PERFORM LE-PROBLEMA
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 CONTINUE
             WHEN 2 CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CGP001T"
                    MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-REVISOR
                    MOVE PASSAR-STRING-1(33: 6) TO GS-REVISOR
             WHEN 3 CALL   "VIP022T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "VIP022T"
                    MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-MOMENTO
                    MOVE PASSAR-STRING-1(33: 3) TO GS-MOMENTO
             WHEN 4 CALL   "VIP021T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "VIP021T"
                    MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-PROBLEMA
                    MOVE PASSAR-STRING-1(33: 3) TO GS-PROBLEMA
           END-EVALUATE.
      *----------------------------------------------------------------
       LE-REVISOR SECTION.
           MOVE GS-REVISOR         TO CODIGO-CG01.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           MOVE NOME-CG01          TO GS-NOME-REVISOR.

       LE-NR-FITA SECTION.
           MOVE GS-IDENTIFICADOR   TO IDENTIFICADOR-W
           MOVE "CLEAR-PRINCIPAL" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
      *    DEVIDO A FUNCAO CLEAR - RETORNAR O NR-FITA
           INITIALIZE REG-VID100
           MOVE IDENTIFICADOR-W    TO GS-IDENTIFICADOR
           MOVE GS-IDENTIFICADOR   TO IDENTIFICADOR-V100
           START VID100 KEY IS NOT < ALT4-V100 INVALID KEY
                 MOVE "10" TO ST-VID100.
           PERFORM UNTIL ST-VID100 = "10"
             READ VID100 NEXT RECORD AT END
                  MOVE "10" TO ST-VID100
             NOT AT END
                 IF GS-IDENTIFICADOR <> IDENTIFICADOR-V100
                    MOVE "10"             TO ST-VID100
                 ELSE
                   MOVE NR-FITA-V100      TO GS-NR-FITA
                   MOVE CONTRATO-V100     TO GS-CONTRATO
                   MOVE EVENTO-V100       TO CODIGO-CO03
                   READ COD003 INVALID KEY
                        MOVE SPACES TO NOME-CO03
                   END-READ
                   MOVE NOME-CO03         TO GS-NOME-EVENTO
                   MOVE CINEGRAFISTA-V100 TO CODIGO-CG01
                   READ CGD001 INVALID KEY
                        MOVE SPACES       TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01         TO GS-NOME-CINEGRAFISTA
                   MOVE "10" TO ST-VID100
                 END-IF
             END-READ
           END-PERFORM.
       LE-MOMENTO SECTION.
           MOVE GS-MOMENTO         TO CODIGO-V22.
           READ VID022 INVALID KEY
                MOVE SPACES        TO DESCRICAO-V22.
           MOVE DESCRICAO-V22      TO GS-DESC-MOMENTO.
       LE-PROBLEMA SECTION.
           MOVE GS-PROBLEMA        TO CODIGO-V21.
           READ VID021 INVALID KEY
                MOVE SPACES        TO DESCRICAO-V21.
           MOVE DESCRICAO-V21      TO GS-DESC-PROBLEMA.
      *--------------------------------------------------------------
       VERIFICA-NR-FITA SECTION.
           PERFORM CARREGAR-DADOS-AVAL.
           PERFORM CARREGAR-DADOS-OBS.
           PERFORM CARREGA-ULTIMOS.
       CARREGAR-DADOS SECTION.
           START VID102 KEY IS = CHAVE-V102 INVALID KEY CONTINUE.
           READ VID102 INVALID KEY INITIALIZE REG-VID102.
           MOVE SEQ-V102             TO  GS-SEQ
           MOVE PROBLEMA-V102        TO  GS-PROBLEMA
           PERFORM LE-PROBLEMA
           MOVE MOMENTO-V102         TO  GS-MOMENTO
           PERFORM LE-MOMENTO.
           MOVE TEMPO-V102           TO  GS-TEMPO.

       CARREGAR-DADOS-AVAL SECTION.
      *    DADOS DO ARQUIVO VID101
           MOVE GS-CONTRATO TO CONTRATO-V101
           MOVE GS-NR-FITA  TO NR-FITA-V101.
           READ VID101 INVALID KEY
                INITIALIZE REG-VID101.
           MOVE REVISOR-V101         TO  GS-REVISOR CODIGO-CG01.
           PERFORM LE-REVISOR
           MOVE DATA-REVISAO-V101    TO  GS-DATA-REVISAO
           EVALUATE AVALIACAO-GERAL-V101
             WHEN 1 MOVE "1-Pessima" TO  GS-AVAL-GERAL
             WHEN 2 MOVE "2-Ruim   " TO  GS-AVAL-GERAL
             WHEN 3 MOVE "3-Regular" TO  GS-AVAL-GERAL
             WHEN 4 MOVE "4-Boa    " TO  GS-AVAL-GERAL
             WHEN 5 MOVE "5-Ótima  " TO  GS-AVAL-GERAL
           END-EVALUATE.

       CARREGAR-DADOS-OBS SECTION.
      ******************* CARREGAR-DADOS-OBSERVACAO
           MOVE GS-CONTRATO         TO CONTRATO-V103
           MOVE GS-NR-FITA          TO NR-FITA-V103
           MOVE ZEROS               TO SEQ-V103.
           MOVE 1                   TO I.
           START VID103 KEY IS NOT < CHAVE-V103 INVALID KEY
                 MOVE "10" TO ST-VID103.
           PERFORM UNTIL ST-VID103 = "10"
             READ VID103 NEXT RECORD AT END MOVE "10" TO ST-VID103
               NOT AT END
                  IF CONTRATO-V103 <> GS-CONTRATO OR
                     NR-FITA-V103  <> GS-NR-FITA
                     MOVE "10" TO ST-VID103
                  ELSE
                    IF I < 502
                      MOVE OBS-V103   TO GS-OBS(I: 100)
                      ADD 100 TO I
                    END-IF
                  END-IF
             END-READ
           END-PERFORM.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-VID101
           INITIALIZE REG-VID102.
           INITIALIZE REG-VID103.
           INITIALIZE GS-DATA-BLOCK
           MOVE ULT-SEQ TO GS-SEQ
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

       EXCLUIR-FITA SECTION.
           MOVE GS-CONTRATO  TO CONTRATO-V101
           MOVE GS-NR-FITA   TO NR-FITA-V101
           READ VID101 INVALID KEY
                CONTINUE
           NOT INVALID KEY
                DELETE VID101
           END-READ.
           PERFORM EXCLUIR-OBS.
           PERFORM EXCLUIR-PROB-MOM-G.
           PERFORM LIMPAR-DADOS.

       EXCLUIR-OBS SECTION.
           MOVE GS-CONTRATO      TO CONTRATO-V103
           MOVE GS-NR-FITA       TO NR-FITA-V103.
           MOVE ZEROS            TO SEQ-V103.
           START VID103 KEY IS NOT < CHAVE-V103 INVALID KEY
                 MOVE "10" TO ST-VID103.
           PERFORM UNTIL ST-VID103 = "10"
             READ VID103 NEXT RECORD AT END
                  MOVE "10" TO ST-VID103
             NOT AT END
                 IF GS-CONTRATO  <> CONTRATO-V103 OR
                    NR-FITA-V103 <> GS-NR-FITA
                    MOVE "10" TO ST-VID103
                 ELSE
                    DELETE VID103
                 END-IF
             END-READ
           END-PERFORM.
       EXCLUIR-PROB-MOM-G SECTION.
           MOVE GS-CONTRATO  TO CONTRATO-V102
           MOVE GS-NR-FITA   TO NR-FITA-V102.
           MOVE ZEROS        TO SEQ-V102.
           START VID102 KEY IS NOT < CHAVE-V102 INVALID KEY
                 MOVE "10" TO ST-VID102.
           PERFORM UNTIL ST-VID102 = "10"
                 READ VID102 NEXT RECORD AT END
                      MOVE "10" TO ST-VID102
                 NOT AT END
                      IF CONTRATO-V102 <> GS-CONTRATO OR
                         NR-FITA-V102  <> GS-NR-FITA
                         MOVE "10" TO ST-VID102
                      ELSE
                         DELETE VID102
                      END-IF
                 END-READ
           END-PERFORM.
       EXCLUIR-PROB-MOM SECTION.
           MOVE GS-CONTRATO  TO CONTRATO-V102
           MOVE GS-NR-FITA   TO NR-FITA-V102.
           MOVE GS-SEQ       TO SEQ-V102.
           READ VID102 INVALID KEY CONTINUE
               NOT INVALID KEY DELETE VID102
           END-READ.
           MOVE ULT-SEQ  TO GS-SEQ.
       SALVAR-FITA SECTION.
           MOVE GS-CONTRATO           TO CONTRATO-V101
           MOVE GS-NR-FITA            TO NR-FITA-V101
           READ VID101 INVALID KEY
                PERFORM MOVER-DADOS-FITA
                WRITE REG-VID101
                END-WRITE
              NOT INVALID KEY
                PERFORM MOVER-DADOS-FITA
                REWRITE REG-VID101
                END-REWRITE
           END-READ.

           PERFORM EXCLUIR-OBS.
           MOVE GS-CONTRATO TO CONTRATO-V103
           MOVE GS-NR-FITA  TO NR-FITA-V103.
           MOVE ZEROS TO SEQ-V103.
           PERFORM VARYING I FROM 1 BY 100 UNTIL I > 600
             MOVE GS-OBS(I: 100)  TO OBS-W
             IF OBS-W <> SPACES
                MOVE OBS-W TO OBS-V103
                ADD 1 TO SEQ-V103
                WRITE REG-VID103
                END-WRITE
             ELSE MOVE 700 TO I
             END-IF
           END-PERFORM.
       MOVER-DADOS-FITA SECTION.
           MOVE GS-REVISOR            TO REVISOR-V101
           MOVE GS-AVAL-GERAL(1: 1)   TO AVALIACAO-GERAL-V101
           MOVE GS-DATA-REVISAO       TO DATA-REVISAO-V101.
       SALVAR-PROB-MOM SECTION.
           MOVE GS-CONTRATO           TO CONTRATO-V102
           MOVE GS-NR-FITA            TO NR-FITA-V102
           MOVE GS-SEQ                TO SEQ-V102
           READ VID102 INVALID KEY
                PERFORM MOVER-DADOS-PROB-MOM
                WRITE REG-VID102
                END-WRITE
                PERFORM MOVER-DADOS-LISTA
                MOVE "INSERE-LIST" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
                ADD 1 TO ULT-SEQ
              NOT INVALID KEY
                PERFORM MOVER-DADOS-PROB-MOM
                REWRITE REG-VID102
                END-REWRITE
                PERFORM MOVER-DADOS-LISTA
                MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
                PERFORM CALL-DIALOG-SYSTEM
           END-READ.
           MOVE ULT-SEQ TO GS-SEQ GS-ULT-SEQ.

       MOVER-DADOS-PROB-MOM SECTION.
           MOVE GS-MOMENTO            TO MOMENTO-V102
           MOVE GS-PROBLEMA           TO PROBLEMA-V102
           MOVE GS-TEMPO              TO TEMPO-V102
           MOVE USUARIO-W             TO DIGITADOR-V102.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-CONTRATO     TO CONTRATO-V102
           MOVE GS-NR-FITA      TO NR-FITA-V102
           MOVE ZEROS           TO SEQ-V102 GS-SEQ.
           START VID102 KEY IS NOT < CHAVE-V102 INVALID KEY
                 MOVE "10" TO ST-VID102.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-VID102 = "10"
                 READ VID102 NEXT RECORD AT END
                      MOVE "10" TO ST-VID102
                 NOT AT END
                      IF CONTRATO-V102 <> GS-CONTRATO OR
                         NR-FITA-V102  <> GS-NR-FITA
                         MOVE "10" TO ST-VID102
                      ELSE
                         PERFORM MOVER-DADOS-LISTA
                         MOVE SEQ-V102      TO GS-SEQ
                         MOVE "INSERE-LIST" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                 END-READ
           END-PERFORM.
           ADD 1 TO GS-SEQ.
           MOVE GS-SEQ TO ULT-SEQ GS-ULT-SEQ.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE SEQ-V102           TO GS-LINDET(1: 4)
           MOVE CONTRATO-V102      TO GS-LINDET(5: 4)
           MOVE NR-FITA-V102       TO GS-LINDET(10: 6)
           MOVE MOMENTO-V102       TO CODIGO-V22
           READ VID022 INVALID KEY
                MOVE SPACES TO DESCRICAO-V22.
           MOVE DESCRICAO-V22      TO GS-LINDET(16: 21)
           MOVE PROBLEMA-V102      TO CODIGO-V21.
           READ VID021 INVALID KEY
                MOVE SPACES TO DESCRICAO-V21.
           MOVE DESCRICAO-V21      TO GS-LINDET(38: 31)
           MOVE TEMPO-V102         TO TEMPO-E
           MOVE TEMPO-E            TO GS-LINDET(69: 10).

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "VIP101" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-----------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.
           OPEN OUTPUT RELAT.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.

           MOVE GS-NR-FITA  TO NR-FITA-V101.
           READ VID101 INVALID KEY
                INITIALIZE REG-VID101.

           MOVE GS-CONTRATO       TO CONTRATO-REL
           MOVE GS-NR-FITA        TO NR-FITA-REL.
           MOVE GS-NOME-REVISOR   TO REVISOR-REL.
           MOVE GS-DATA-REVISAO   TO DATA-REVISAO-REL
           MOVE GS-AVAL-GERAL     TO AVAL-GERAL-REL.
           WRITE REG-RELAT FROM LINHA-02
           WRITE REG-RELAT FROM LINHA-03
           WRITE REG-RELAT FROM LINHA-04
           WRITE REG-RELAT FROM LINHA-05
           WRITE REG-RELAT FROM LINHA-06
           WRITE REG-RELAT FROM LINHA-07 AFTER 2.
           WRITE REG-RELAT FROM LINHA-08.
           ADD 8 TO LIN.
           PERFORM VARYING I FROM 1 BY 78 UNTIL I > 600
              MOVE GS-OBS(I: 78)  TO OBS-W1
              IF OBS-W1 = SPACES MOVE 600 TO I
              ELSE MOVE OBS-W1     TO OBS-REL
                   WRITE REG-RELAT FROM LINHA-09
                   ADD 1 TO LIN
              END-IF
           END-PERFORM.

           WRITE REG-RELAT FROM LINHA-10 AFTER 3.
           WRITE REG-RELAT FROM LINHA-03.
           WRITE REG-RELAT FROM LINHA-11.
           WRITE REG-RELAT FROM LINHA-05.

           MOVE GS-CONTRATO      TO CONTRATO-V102
           MOVE GS-NR-FITA       TO NR-FITA-V102.
           MOVE ZEROS            TO SEQ-V102.
           START VID102 KEY IS NOT < CHAVE-V102 INVALID KEY
                 MOVE "10" TO ST-VID102.
           PERFORM UNTIL ST-VID102 = "10"
                 READ VID102 NEXT RECORD AT END
                      MOVE "10" TO ST-VID102
                 NOT AT END
                     IF CONTRATO-V102 <> GS-CONTRATO OR
                        NR-FITA-V102  <> GS-NR-FITA
                        MOVE "10" TO ST-VID102
                     ELSE
                        MOVE SEQ-V102       TO SEQ-REL
                        MOVE MOMENTO-V102   TO CODIGO-V22
                        READ VID022 INVALID KEY
                             MOVE SPACES TO DESCRICAO-V22
                        END-READ
                        MOVE DESCRICAO-V22  TO MOMENTO-REL
                        MOVE PROBLEMA-V102  TO CODIGO-V21
                        READ VID021 INVALID KEY
                             MOVE SPACES TO DESCRICAO-V21
                        END-READ
                        MOVE DESCRICAO-V21  TO PROBLEMA-REL
                        MOVE TEMPO-V102     TO TEMPO-REL
                        WRITE REG-RELAT FROM LINHA-12
                        ADD 1 TO LIN
                        IF LIN > 56
                           PERFORM CABECALHO
                        END-IF
                     END-IF
                 END-READ
           END-PERFORM.
           WRITE REG-RELAT FROM LINHA-13.

           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.


       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01 AFTER 0
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM LINHA-01 AFTER 2.
           MOVE 5 TO LIN.
      *--------------------------------------------------------
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CGD001 COD003 VID021 VID022  VID100
                 VID101 VID102 VID103.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

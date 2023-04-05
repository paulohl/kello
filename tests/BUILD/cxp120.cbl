       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CXP120.
      *DATA: 10/08/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *RELATÓRIO: Relação do Caixa
      *FUNÇÃO: Relaciona todo os lançamentos ocorridos
      *        na data de movto solicitada

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           OUtilitario        is class "outilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY CGPX001.
           COPY CXPX040.
           COPY CXPX100.
           COPY CXPX100I.
           COPY CXPX031.
           COPY CXPX020.
           COPY CPARX001.

           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY CGPW001.
       COPY CXPW040.
       COPY CXPW100.
       COPY CXPW100I.
       COPY CXPW031.
       COPY CXPW020.
       COPY CPARW001.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(132).

       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CXP120.CPB".
           COPY "CXP120.CPY".
           COPY "CBDATA.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CXD040             PIC XX       VALUE SPACES.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ST-CXD100I            PIC XX       VALUE SPACES.
           05  ST-CXD031             PIC XX       VALUE SPACES.
           05  ST-CXD020             PIC XX       VALUE SPACES.
           05  ST-CPAR001            PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MOVTO-INI             PIC 9(8)     VALUE ZEROS.
           05  MOVTO-FIM             PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  VALOR-E               PIC ZZ.ZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  VALOR1                PIC 9(08)V99 VALUE ZEROS.
           05  VALOR2                PIC 9(08)V99 VALUE ZEROS.
           05  PREV-DEF-W            PIC 9        VALUE ZEROS.
           05  MESANO-SALDO-ANT      PIC 9(6)     VALUE ZEROS.
           05  CONTADOR              PIC 9(02)    VALUE ZEROS.
      *  MES/ANO LIMITE PARA CALCULA O SALDO ANTERIOR
           05  SALDO-INICIAL         PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-FINAL           PIC S9(8)V99 VALUE ZEROS.
           05  SALDO-INTERVALO       PIC S9(8)V99 VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  AUX-DATA              PIC 9(08).
           05  AUX-SEQUENCIA         PIC 9(04).
           05  WSINDICE              PIC 9(03)  VALUE ZEROS.
           05  TENTATIVAS            PIC 9(05)  VALUE ZEROS.
           05  CAMINHO-IMAGEM        PIC X(500) VALUE SPACES.
           05  EXTENSAO-ARQ          PIC X(05)  VALUE SPACES.
           05  LETRA                 PIC X(01)  VALUE SPACES.
           05  Retorno               PIC s9(09) comp-5.
           05  umUtilitario          object reference.

           COPY "PARAMETR".

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1   PIC X(55).

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

       01 status-code               pic x(2) comp-5.

       01  CAB01.
           05  EMPRESA-REL         PIC X(59)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(51)   VALUE
           "RELACAO DO CAIXA  ".
           05  FILLER              PIC X(28)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "   PERIODO DE: ".
           05  MOVTO-INI-REL       PIC 99/99/9999.
           05  FILLER              PIC X(05)   VALUE " ATÉ ".
           05  MOVTO-FIM-REL       PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(132)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(132)  VALUE
           "   DATA    SEQ. LT TP            HISTORICO                 D
      -    "OCUMENTO      VALOR-ENT   VALOR-SAIDA RED              CONTA
      -    "-FORNEC".

       01  LINDET.
           05  LINDET-REL          PIC X(132)  VALUE SPACES.
       01  LINTOT.
           05  FILLER              PIC X(10)   VALUE SPACES.
           05  FILLER              PIC X(15)   VALUE "SALDO INICIAL: ".
           05  SALDO-INI-REL       PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FILLER              PIC X(11)   VALUE SPACES.
           05  FILLER              PIC X(14)   VALUE "SALDO INTERV: ".
           05  SALDO-INT-REL       PIC ZZ.ZZZ.ZZZ,ZZ-.
           05  FILLER              PIC X(12)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE "SALDO FINAL: ".
           05  SALDO-FIM-REL       PIC ZZ.ZZZ.ZZZ,ZZ-.
       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL CXP120-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE ZEROS TO ERRO-W.
           INITIALIZE CXP120-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE CXP120-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE CXP120-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CXD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD020.
           MOVE "CXD040"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD040.
           MOVE "CXD100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD100.
           MOVE "CXD100I" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD100I.
           MOVE "CXD031"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CXD031.
           MOVE "CPAR001" TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CPAR001
           OPEN I-O   CXD100I CPAR001
           CLOSE      CXD100I CPAR001
           OPEN INPUT CXD100 CGD001 CXD040 CXD031 CXD020 CXD100I
                      CPAR001 CAD004
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO CXP120-MENSAGEM-ERRO
              MOVE ST-CGD001 TO CXP120-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD020 <> "00"
              MOVE "ERRO ABERTURA CXD020: "  TO CXP120-MENSAGEM-ERRO
              MOVE ST-CXD020 TO CXP120-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD040 <> "00"
              MOVE "ERRO ABERTURA CXD040: "  TO CXP120-MENSAGEM-ERRO
              MOVE ST-CXD040 TO CXP120-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD100 <> "00"
              MOVE "ERRO ABERTURA CXD100: "  TO CXP120-MENSAGEM-ERRO
              MOVE ST-CXD100 TO CXP120-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD100I <> "00"
              MOVE "ERRO ABERTURA CXD100I: "  TO CXP120-MENSAGEM-ERRO
              MOVE ST-CXD100I TO CXP120-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CXD031 <> "00"
              MOVE "ERRO ABERTURA CXD031: "  TO CXP120-MENSAGEM-ERRO
              MOVE ST-CXD031 TO CXP120-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CPAR001 <> "00"
              MOVE "ERRO ABERTURA CPAR001: "  TO CXP120-MENSAGEM-ERRO
              MOVE ST-CPAR001 TO CXP120-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           MOVE 1                  TO CHAVE-CPAR001

           READ CPAR001 INVALID KEY
                MOVE "Parametro do Imagem do Caixa Nao Cadastrado" TO
                CXP120-MENSAGEM-ERRO
                PERFORM CARREGA-MENSAGEM-ERRO
           NOT INVALID KEY
                IF CAMINHO-IMAGEM-CX-CPAR001 EQUAL SPACES
                   MOVE "Parametro do Imagem do Caixa Nao Cadastrado" TO
                   CXP120-MENSAGEM-ERRO
                   PERFORM CARREGA-MENSAGEM-ERRO.

           invoke OUtilitario "new" returning umUtilitario

      *    MOVE 1 TO COD-USUARIO-W.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO CXP120-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = ZEROS
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CXP120-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN CXP120-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN CXP120-CARREGA-LISTA-TRUE
                    PERFORM CARREGA-LISTA
               WHEN CXP120-POPUP-TIPOLCTO-TRUE
                    PERFORM POPUP-TIPOLCTO
               WHEN CXP120-LER-TIPOLCTO-TRUE
                    PERFORM LER-TIPOLCTO
               WHEN CXP120-CRITICAR-TRUE
                    PERFORM CRITICAR-REGISTRO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       VALIDAR-USUARIO SECTION.
           MOVE "SENHA63"     TO PROGRAMA-CA004
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           READ CAD004 INVALID KEY
                MOVE "N" TO CXP120-HABILITA-EXCLUSAO
                DISABLE-OBJECT PB16
                DISABLE-OBJECT EF40
           NOT INVALID KEY
                MOVE "S" TO CXP120-HABILITA-EXCLUSAO
                ENABLE-OBJECT PB16
                ENABLE-OBJECT EF40
           END-READ.

       CRITICAR-REGISTRO SECTION.
           MOVE SPACES           TO MENSAGEM
           EVALUATE CXP120-CAMPO-CRITICA
               WHEN "IMAGEM"     PERFORM VALIDAR-IMAGEM
               WHEN "SALVAR"     PERFORM SALVAR-IMAGENS
               WHEN "ATUALIZA"   PERFORM ATUALIZAR-LB1
                                 PERFORM VALIDAR-USUARIO
               WHEN "CARREGAR"   PERFORM CARREGAR-IMAGENS
                                 PERFORM VALIDAR-USUARIO
               WHEN "ABRIR"      PERFORM ABRIR-ARQUIVO
           END-EVALUATE.

       ABRIR-ARQUIVO SECTION.
           INSPECT cxp120-linha-detalhe REPLACING ALL '"' BY " "

           invoke umUtilitario "ExecutarArquivo" using
                               cxp120-linha-detalhe returning
                               retorno.
       ABRIR-ARQUIVO-FIM.
           EXIT.


       VALIDAR-IMAGEM SECTION.
           IF MENSAGEM EQUAL SPACES
              IF CXP120-HABILITA-EXCLUSAO = "N"
                 MOVE "Usuario sem Permissao" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM 140-EXIBIR-MENSAGEM
              ELSE
                 IF CXP120-ACP-IMAGEM EQUAL SPACES
                    move "CHAMAR-OPENDIALOG" to ds-procedure
                    perform call-dialog-system
                 END-IF
                 IF CXP120-ACP-IMAGEM EQUAL SPACES
                    MOVE "Imagem do Formando Não Informada" TO MENSAGEM
                    MOVE "C" TO TIPO-MSG
                    PERFORM 140-EXIBIR-MENSAGEM
                 ELSE
                    CALL "CBL_CHECK_FILE_EXIST" USING CXP120-ACP-IMAGEM
                                                      FILE-DETAILS
                                                RETURNING STATUS-CODE
                    IF STATUS-CODE <> ZEROS
                       MOVE "Imagem Não Encontrada" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM 140-EXIBIR-MENSAGEM
                    ELSE
                       MOVE CXP120-ACP-IMAGEM TO CXP120-LINHA-DETALHE
                       MOVE "INSERIR-LB2" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                       MOVE SPACES TO CXP120-ACP-IMAGEM
                       REFRESH-OBJECT WIN3.
       VALIDAR-IMAGEM-FIM.
           EXIT.

       SALVAR-IMAGENS SECTION.
           CLOSE      CXD100I
           OPEN I-O   CXD100I

           INITIALIZE REG-CXD100I
           MOVE AUX-DATA           TO DATA-MOV-CX100I
           MOVE AUX-SEQUENCIA      TO SEQ-CX100I
           START CXD100I KEY IS NOT LESS CHAVE-CX100I INVALID KEY
                 MOVE "10" TO ST-CXD100I.

           PERFORM UNTIL ST-CXD100I = "10"
                 READ CXD100I NEXT AT END
                      MOVE "10" TO ST-CXD100I
                 NOT AT END
                      IF AUX-DATA      <> DATA-MOV-CX100I OR
                         AUX-SEQUENCIA <> SEQ-CX100I
                         MOVE "10" TO ST-CXD100I
                      ELSE
                         DELETE CXD100I INVALID KEY
                             MOVE "Erro de Exclusão...CXD100I" TO
                                         MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM 140-EXIBIR-MENSAGEM
                         END-DELETE
                      END-IF
                 END-READ
           END-PERFORM

           INITIALIZE CXP120-LINHA-DETALHE
           MOVE 1         TO CXP120-LINHA
           MOVE "LER-LB2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL CXP120-LINHA-DETALHE = SPACES

               PERFORM SALVAR-IMAGEM

               INITIALIZE CXP120-LINHA-DETALHE
               ADD  1         TO CXP120-LINHA
               MOVE "LER-LB2" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM

           CLOSE      CXD100I
           OPEN INPUT CXD100I.
       SALVAR-IMAGENS-FIM.
           EXIT.

       SALVAR-IMAGEM SECTION.
           move 255 to wsIndice
           perform until CXP120-LINHA-DETALHE(wsIndice:1) = "\" OR
                         WSINDICE = 0
              if CXP120-LINHA-DETALHE(wsIndice:1) = "."
                 move CXP120-LINHA-DETALHE(wsindice:5)
                   to EXTENSAO-ARQ
              end-if
              subtract 1   from wsIndice
           end-perform

           MOVE SPACES                    TO CAMINHO-IMAGEM
           STRING CAMINHO-IMAGEM-CX-CPAR001 DELIMITED BY " " "\"
                  EMPRESA-W INTO CAMINHO-IMAGEM

           call "CBL_CREATE_DIR" using     caminho-imagem
                                 returning status-code

           MOVE SPACES                    TO CAMINHO-IMAGEM
           STRING CAMINHO-IMAGEM-CX-CPAR001 DELIMITED BY " " "\"
                  EMPRESA-W "\" CXP120-LINDET(1:2) "-"
                                CXP120-LINDET(4:2) "-"
                                CXP120-LINDET(7:4)
             INTO CAMINHO-IMAGEM
           call "CBL_CREATE_DIR" using     caminho-imagem
                                 returning status-code

           PERFORM PEGAR-LETRA

           MOVE SPACES                    TO CAMINHO-IMAGEM
           STRING CAMINHO-IMAGEM-CX-CPAR001 DELIMITED BY " "
                  "\" EMPRESA-W "\" CXP120-LINDET(1:2) "-"
                                    CXP120-LINDET(4:2) "-"
                                    CXP120-LINDET(7:4) '\'
                  CXP120-LINDET(12:4) "-" LETRA
                  delimited by "  "
                  EXTENSAO-ARQ delimited by " "
                  INTO CAMINHO-IMAGEM

           IF CXP120-LINHA-DETALHE <> CAMINHO-IMAGEM
              move 1 to status-code
              move 0 to tentativas
              perform until status-code = 0 or tentativas = 20000
                   call "CBL_COPY_FILE" using CXP120-LINHA-DETALHE
                                              caminho-imagem
                                    returning status-code
                   add 1 to tentativas
              end-perform
              if tentativas = 20000
                 move "Não Consegui Salvar a Imagem" to mensagem
                 move "C" to tipo-msg
                 perform 140-exibir-mensagem.

           INSPECT CAMINHO-IMAGEM REPLACING ALL '"' BY " "

           MOVE CAMINHO-IMAGEM  TO CXP120-LINHA-DETALHE
           MOVE "ATUALIZAR-LB2" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM


           MOVE AUX-DATA        TO DATA-MOV-CX100I
           MOVE AUX-SEQUENCIA   TO SEQ-CX100I
           MOVE CAMINHO-IMAGEM  TO IMAGEM-CX100I

           WRITE REG-CXD100I INVALID KEY
                 MOVE "Erro de Gravacao...CXD100I" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM 140-EXIBIR-MENSAGEM
           END-WRITE.
       SALVAR-IMAGEM-FIM.
           EXIT.

       PEGAR-LETRA SECTION.
           EVALUATE CXP120-LINHA
               WHEN 1  MOVE "A" TO LETRA
               WHEN 2  MOVE "B" TO LETRA
               WHEN 3  MOVE "C" TO LETRA
               WHEN 4  MOVE "D" TO LETRA
               WHEN 5  MOVE "E" TO LETRA
               WHEN 6  MOVE "F" TO LETRA
               WHEN 7  MOVE "G" TO LETRA
               WHEN 8  MOVE "H" TO LETRA
               WHEN 9  MOVE "I" TO LETRA
               WHEN 10 MOVE "J" TO LETRA
               WHEN 11 MOVE "K" TO LETRA
               WHEN 12 MOVE "L" TO LETRA
               WHEN 13 MOVE "M" TO LETRA
               WHEN 14 MOVE "N" TO LETRA
               WHEN 15 MOVE "O" TO LETRA
               WHEN 16 MOVE "P" TO LETRA
               WHEN 17 MOVE "Q" TO LETRA
               WHEN 18 MOVE "R" TO LETRA
               WHEN 19 MOVE "S" TO LETRA
               WHEN 20 MOVE "T" TO LETRA
               WHEN 21 MOVE "U" TO LETRA
               WHEN 22 MOVE "V" TO LETRA
               WHEN 23 MOVE "X" TO LETRA
               WHEN 24 MOVE "Z" TO LETRA
           END-EVALUATE.
       PEGAR-LETRA-FIM.
           EXIT.

       ATUALIZAR-LB1 SECTION.
           MOVE "X"                   TO CXP120-LINDET(138:1)
           MOVE "ALTERAR-LB1"         TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       ATUALIZAR-LB1-FIM.
           EXIT.

       CARREGAR-IMAGENS SECTION.
           CLEAR-OBJECT LB2

           INITIALIZE REG-CXD100I
           MOVE CXP120-LINDET(01:2) TO DATA-MOV-CX100I(7:2)
           MOVE CXP120-LINDET(04:2) TO DATA-MOV-CX100I(5:2)
           MOVE CXP120-LINDET(07:4) TO DATA-MOV-CX100I(1:4)
           MOVE CXP120-LINDET(12:4) TO SEQ-CX100I

           MOVE DATA-MOV-CX100I     TO AUX-DATA
           MOVE SEQ-CX100I          TO AUX-SEQUENCIA
           START CXD100I KEY IS NOT LESS CHAVE-CX100I INVALID KEY
                 MOVE "10" TO ST-CXD100I.

           PERFORM UNTIL ST-CXD100I = "10"
                 READ CXD100I NEXT AT END
                      MOVE "10" TO ST-CXD100I
                 NOT AT END
                      IF DATA-MOV-CX100I <> AUX-DATA        OR
                         SEQ-CX100I      <> AUX-SEQUENCIA
                         MOVE "10" TO ST-CXD100I
                      ELSE
                         MOVE IMAGEM-CX100I TO CXP120-LINHA-DETALHE
                         MOVE "INSERIR-LB2" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                 END-READ
           END-PERFORM.
       CARREGAR-IMAGENS-FIM.
           EXIT.

       140-EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           MOVE    1 TO CXP120-FLAG-CRITICA.

       POPUP-TIPOLCTO SECTION.
           CALL "CXP031T" USING PARAMETROS-W PASSAR-PARAMETROS
           MOVE PASSAR-PARAMETROS(33: 2) TO CXP120-ACP-TIPOLCTO
           CANCEL "CXP031T"
           PERFORM LER-TIPOLCTO.
       POPUP-TIPOLCTO-FIM.
           EXIT.

       LER-TIPOLCTO SECTION.
           MOVE CXP120-ACP-TIPOLCTO  TO TIPO-LCTO-CX31
           READ CXD031 INVALID KEY
                MOVE SPACES          TO DESCRICAO-CX31
           END-READ

           MOVE DESCRICAO-CX31       TO CXP120-DESC-TIPOLCTO
           REFRESH-OBJECT PRINCIPAL.
       LER-TIPOLCTO-FIM.
           EXIT.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win3 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CALCULA-SALDO-INICIAL SECTION.
           PERFORM INVERTE-DATA.
           MOVE "Gerando saldo: " TO CXP120-TEXTO-AGUARDE.
           MOVE ZEROS             TO ANOMES-CX40 SALDO-INICIAL.
           START CXD040 KEY IS NOT < ANOMES-CX40 INVALID KEY
                 MOVE "10" TO ST-CXD040.
           PERFORM UNTIL ST-CXD040 = "10"
                 READ CXD040 NEXT RECORD AT END
                      MOVE "10" TO ST-CXD040
                 NOT AT END
                       MOVE ANOMES-CX40 TO CXP120-TEXTO-AGUARDE(16: 8)
                       MOVE "REFRESH-WIN1" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                       IF ANOMES-CX40 NOT < MESANO-SALDO-ANT
                          MOVE "10" TO ST-CXD040
                       ELSE
                          ADD SALDOE-CX40 TO SALDO-INICIAL
                          SUBTRACT SALDOS-CX40 FROM SALDO-INICIAL
                 END-READ
           END-PERFORM

           MOVE MESANO-SALDO-ANT TO DATA-MOV-CX100(1: 6).
           MOVE 01               TO DATA-MOV-CX100(7: 2).
           MOVE ZEROS            TO SEQ-CX100.
           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
                 READ CXD100 NEXT RECORD AT END
                      MOVE "10" TO ST-CXD100
                 NOT AT END
                      MOVE DATA-MOV-CX100 TO CXP120-TEXTO-AGUARDE(16: 8)
                      MOVE "REFRESH-WIN1" TO DS-PROCEDURE
                      IF DATA-MOV-CX100 NOT < MOVTO-INI
                         MOVE "10" TO ST-CXD100
                      ELSE
                         IF CXP120-ACP-TIPOLCTO = 0 OR TIPO-LCTO-CX100
                            IF TIPO-LCTO-CX100 NOT < 50
                               ADD VALOR-CX100 TO SALDO-INICIAL
                            ELSE
                               SUBTRACT VALOR-CX100 FROM SALDO-INICIAL
                            END-IF
                         END-IF
                      END-IF
               END-READ
           END-PERFORM.
           MOVE "UNSHOW-WIN1" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE CXP120-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
       INVERTE-DATA SECTION.
           MOVE CXP120-DATA-MOVTOI TO DATA-INV
                                      MOVTO-INI-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV           TO MOVTO-INI.
           MOVE DATA-INV(01: 06)   TO MESANO-SALDO-ANT.


           MOVE CXP120-DATA-MOVTOF TO DATA-INV
                                      MOVTO-FIM-REL.
           CALL "GRIDAT2" USING DATA-INV.
           MOVE DATA-INV           TO MOVTO-FIM.

       CARREGA-LISTA SECTION.
           CLEAR-OBJECT LB1
           REFRESH-OBJECT LB1

           PERFORM CALCULA-SALDO-INICIAL.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE SPACES TO CXP120-LINDET.
           MOVE MOVTO-INI         TO DATA-MOV-CX100.
           MOVE ZEROS             TO SEQ-CX100 VALOR1 VALOR2
           MOVE ZEROS             TO SALDO-INTERVALO.
           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
              READ CXD100 NEXT RECORD AT END
                  MOVE "10" TO ST-CXD100
              NOT AT END
                  IF DATA-MOV-CX100 > MOVTO-FIM
                     MOVE "10" TO ST-CXD100
                  ELSE
                     IF CXP120-ACP-TIPOLCTO = 0 OR TIPO-LCTO-CX100
                        IF CXP120-LOTE-INI = 0 OR
                          (CONTROLE-LOTE-CX100 NOT <
                           CXP120-LOTE-INI AND
                           CONTROLE-LOTE-CX100 NOT >
                           CXP120-LOTE-FIM)
                           MOVE SPACES            TO CXP120-LINDET
                           STRING DATA-MOV-CX100(7:2) "/"
                                  DATA-MOV-CX100(5:2) "/"
                                  DATA-MOV-CX100(1:4)
                             INTO CXP120-LINDET(1:10)
                           MOVE SEQ-CX100       TO CXP120-LINDET(12: 04)
                           MOVE CONTROLE-LOTE-CX100 TO
                                CXP120-LINDET(17:3)
                           MOVE TIPO-LCTO-CX100 TO CXP120-LINDET(20:02)
                                                   TIPO-LCTO-CX31
                           READ CXD031 INVALID KEY
                                MOVE SPACES     TO DESCRICAO-CX31
                           END-READ
                           MOVE DESCRICAO-CX31  TO CXP120-LINDET(23:15)

                           MOVE HISTORICO-CX100 TO CXP120-LINDET(39: 20)
                           MOVE DOCUMENTO-CX100 TO CXP120-LINDET(60: 11)
                           MOVE VALOR-CX100     TO VALOR-E
                           IF TIPO-LCTO-CX100 NOT < 50
                              MOVE VALOR-E      TO CXP120-LINDET(71: 13)
                              ADD VALOR-CX100   TO SALDO-INTERVALO
                              ADD VALOR-CX100   TO VALOR1
                           ELSE
                              MOVE VALOR-E      TO CXP120-LINDET(85: 13)
                              SUBTRACT VALOR-CX100 FROM SALDO-INTERVALO
                              ADD VALOR-CX100     TO VALOR2
                           END-IF
                           MOVE CONTA-REDUZ-CX100 TO
                                CXP120-LINDET(99: 05)
                                CODIGO-REDUZ-CX20
                           READ CXD020 INVALID KEY
                                INITIALIZE REG-CXD020
                           END-READ
                           MOVE DESCRICAO-CX20    TO
                                CXP120-LINDET(105: 06)
                           MOVE CONTAPART-CX100   TO
                                CXP120-LINDET(112: 06)
                                CODIGO-CG01
                           READ CGD001 INVALID KEY
                                MOVE SPACES       TO NOME-CG01
                           END-READ
                           MOVE NOME-CG01     TO CXP120-LINDET(119: 18)
                           MOVE "INSERE-LIST" TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF
                     END-IF
                  END-IF
              END-READ
           END-PERFORM.

           MOVE SPACES            TO CXP120-LINDET
           MOVE "TOTAL . . ."     TO CXP120-LINDET(06:31)
           MOVE VALOR1            TO VALOR-E
           MOVE VALOR-E           TO CXP120-LINDET(71: 13)
           MOVE VALOR2            TO VALOR-E
           MOVE VALOR-E           TO CXP120-LINDET(85: 13)
           MOVE "INSERE-LIST" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM CALCULA-SALDO-FINAL.
       CALCULA-SALDO-FINAL SECTION.
           COMPUTE SALDO-FINAL = SALDO-INICIAL + SALDO-INTERVALO.
           MOVE SALDO-INICIAL      TO CXP120-SALDO-ANTERIOR
                                      SALDO-INI-REL.
           MOVE SALDO-INTERVALO    TO CXP120-SALDO-DIA SALDO-INT-REL.
           MOVE SALDO-FINAL        TO CXP120-SALDO-FINAL SALDO-FIM-REL.
           MOVE "REFRESH-DATA"     TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CLEAR-FLAGS SECTION.
           INITIALIZE CXP120-FLAG-GROUP.
       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "CXP120"   TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE 0 TO CONTADOR
           PERFORM UNTIL CONTADOR = CXP120-COPIAS
               ADD 1 TO CONTADOR
               PERFORM IMPRIMIR
           END-PERFORM.
       IMPRIME-RELATORIO-FIM.
           EXIT.

       IMPRIMIR SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           MOVE SPACES TO LINDET-REL
           MOVE MOVTO-INI         TO DATA-MOV-CX100.
           MOVE ZEROS             TO SEQ-CX100 VALOR1 VALOR2.
           START CXD100 KEY IS NOT < CHAVE-CX100 INVALID KEY
                 MOVE "10" TO ST-CXD100.
           PERFORM UNTIL ST-CXD100 = "10"
                 READ CXD100 NEXT RECORD AT END
                      MOVE "10" TO ST-CXD100
                 NOT AT END
                      IF DATA-MOV-CX100 > MOVTO-FIM
                         MOVE "10" TO ST-CXD100
                      ELSE
                         IF CXP120-ACP-TIPOLCTO = 0 OR TIPO-LCTO-CX100
                            IF CXP120-LOTE-INI = 0 OR
                              (CONTROLE-LOTE-CX100 NOT <
                               CXP120-LOTE-INI AND
                               CONTROLE-LOTE-CX100 NOT >
                               CXP120-LOTE-FIM)

                              MOVE SPACES            TO LINDET-REL
                              STRING DATA-MOV-CX100(7:2) "/"
                                     DATA-MOV-CX100(5:2) "/"
                                     DATA-MOV-CX100(1:4)
                                                 INTO LINDET-REL(1:10)
                              MOVE SEQ-CX100       TO LINDET-REL(12: 04)

                              MOVE CONTROLE-LOTE-CX100 TO
                                                      LINDET-REL(17:3)

                              MOVE TIPO-LCTO-CX100 TO LINDET-REL(20: 02)
                                                      TIPO-LCTO-CX31
                              READ CXD031 INVALID KEY
                                   MOVE SPACES     TO DESCRICAO-CX31
                              END-READ
                              MOVE DESCRICAO-CX31  TO LINDET-REL(23: 10)
                              MOVE HISTORICO-CX100 TO LINDET-REL(34: 25)

                              MOVE DOCUMENTO-CX100 TO LINDET-REL(60: 10)

                              MOVE VALOR-CX100   TO VALOR-E
                              IF TIPO-LCTO-CX100 NOT < 50
                                 MOVE VALOR-E    TO LINDET-REL(71: 13)
                                 ADD VALOR-CX100 TO SALDO-INTERVALO
                                 ADD VALOR-CX100 TO VALOR1
                              ELSE
                                 MOVE VALOR-E    TO LINDET-REL(85: 13)
                                 SUBTRACT VALOR-CX100
                                     FROM SALDO-INTERVALO
                                 ADD VALOR-CX100 TO VALOR2
                              END-IF
                              MOVE CONTA-REDUZ-CX100 TO
                                   LINDET-REL(99: 05)
                                   CODIGO-REDUZ-CX20
                              READ CXD020 INVALID KEY
                                   INITIALIZE REG-CXD020
                              END-READ
                              MOVE DESCRICAO-CX20   TO
                                   LINDET-REL(105: 10)

                              MOVE CONTAPART-CX100   TO
                                   LINDET-REL(116: 06)
                                   CODIGO-CG01
                              READ CGD001 INVALID KEY
                                   MOVE SPACES  TO NOME-CG01
                              END-READ
                              MOVE NOME-CG01    TO LINDET-REL(123: 12)
                              WRITE REG-RELAT FROM LINDET
                              ADD 1 TO LIN
                              IF LIN > 56
                                 PERFORM CABECALHO
                              END-IF
                            END-IF
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.

           MOVE SPACES            TO LINDET-REL
           MOVE "TOTAL . . ."     TO LINDET-REL(09:27)
           MOVE VALOR1            TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(71: 13)
           MOVE VALOR2            TO VALOR-E
           MOVE VALOR-E           TO LINDET-REL(85: 13)
           WRITE REG-RELAT FROM LINDET
           ADD 1 TO LIN
           IF LIN > 56
              PERFORM CABECALHO
           END-IF


           WRITE REG-RELAT FROM LINTOT AFTER 2.

           COPY DESCONDENSA.

       IMPRIMIR-FIM.
           EXIT.

       CABECALHO SECTION.
           ADD 1 TO LIN PAG-W.
           MOVE PAG-W TO PG-REL.
           IF LIN = 1
              WRITE REG-RELAT FROM CAB01
           ELSE
              WRITE REG-RELAT FROM CAB01 AFTER PAGE.

           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 6 TO LIN.
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, CXP120-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CXD100 CGD001 CXD040 CXD020 CXD031 CXD100I CPAR001
                 CAD004
           invoke umUtilitario "Finalize" returning umUtilitario
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.


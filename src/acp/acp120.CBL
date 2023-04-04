       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACP120.
      *AUTORA: ALFREDO SAVIOLLI NETO
      *DATA: 26/11/2004.
      *DESCRIÇÃO: Cadastro do DEPARTAMENTO DE CONTAS A RECEBER
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY ACPX120.
           COPY ACPX121.
           COPY CGPX010.
           COPY CAPX010.
           COPY CAPX018.
           COPY MTPX019.

           select   ordserv assign       to arquivo-ordserv
                            organization is line sequential
                            access mode  is      sequential
                            file  status is      fs-ordserv.

       DATA DIVISION.
       FILE SECTION.

           COPY ACPW120.
           COPY ACPW121.
           COPY CGPW010.
           COPY CAPW010.
           COPY CAPW018.
           COPY MTPW019.

       fd ordserv.
       01 reg-ordserv                          pic x(300).

       WORKING-STORAGE SECTION.
           COPY "ACP120.CPB".
           COPY "ACP120.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       77  LIN                       PIC 9(02).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-ACD120             PIC XX       VALUE SPACES.
           05  ST-ACD121             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD018             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  FS-ORDSERV            PIC XX       VALUE SPACES.
           05  LNK-DETALHE           PIC X(300).
           05  DET-MES               PIC X(15).
           05  MASC-QTDE             PIC ZZZZ.
           05  MASC-CONTRATO         PIC 9999/9999.
           05  MASC-NUMERO           PIC ZZ.ZZZ.ZZZ.ZZ9.
           05  MASC-JUROS            PIC ZZ9,99.
           05  MASC-VALOR            PIC ZZ.ZZ9,99.
           05  MASC-PARCELA          PIC 99/99.
           05  MASC-PORTADOR         PIC Z9.
           05  MASC-BANCO            PIC 999.
           05  MASC-DATA             PIC 99/99/9999.
           05  ERRO-W                PIC 9(01)    VALUE ZEROS.
           05  GRAVA-W               PIC 9(01)    VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  WS-OK                 PIC X(01).
           05  SEQUENCIA             PIC 9(02).


       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.


       01 DATA-AUX.
          05 AUX-DIA                 PIC 9(02).
          05 FILLER                  PIC X(01) VALUE "/".
          05 AUX-MES                 PIC 9(02).
          05 FILLER                  PIC X(01) VALUE "/".
          05 AUX-ANO                 PIC 9(02).

       01 AUX-DATA.
          05 AUX-DIA2                PIC 9(02).
          05 AUX-MES2                PIC 9(02).
          05 AUX-ANO2                PIC 9(04).

       01 WS-DATA-SYS.
           05 WS-DATA-CPU.
              10 WS-ANO-CPU          PIC 9(04).
              10 WS-MES-CPU          PIC 9(02).
              10 WS-DIA-CPU          PIC 9(02).
           05 FILLER                 PIC X(13).

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).

       01 DET-LINDET.
          05 filler                    pic x(01).
          05 DET-SEQUENCIA             PIC 9(02).
          05 filler                    pic x(03).
          05 DET-NUMERO-DOC            PIC X(15).
          05 filler                    pic x(01).
          05 DET-PARCELA               PIC 99/99.
          05 filler                    pic x(02).
          05 DET-TIPO-DOC              PIC X(04).
          05 filler                    pic x(07).
          05 DET-BANCO                 PIC 9(03).
          05 filler                    pic x(03).
          05 DET-VALOR                 PIC ZZ.ZZ9,99.
          05 filler                    pic x(01).
          05 DET-VENCIMENTO            PIC 99/99/9999.
          05 filler                    pic x(03).
          05 DET-OBSER                 PIC X(10).



           COPY "PARAMETR".

           COPY "DCR.REL".

       01 PARAMETRO.
          05 TIPO-CADASTRO           PIC 9(01).
          05 NUMERO-W                PIC 9(11).

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           ACCEPT DATA6-W FROM DATE.
           ACCEPT HORA-BRA FROM TIME.

           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "ACD120" TO ARQ-REC.   MOVE EMPRESA-REF TO PATH-ACD120.
           MOVE "ACD121" TO ARQ-REC.   MOVE EMPRESA-REF TO PATH-ACD121.
           MOVE "CGD010" TO ARQ-REC.   MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CAD010" TO ARQ-REC.   MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD018" TO ARQ-REC.   MOVE EMPRESA-REF TO PATH-CAD018.
           MOVE "MTD019" TO ARQ-REC.   MOVE EMPRESA-REF TO PATH-MTD019.
           OPEN I-O   ACD120 ACD121
           OPEN INPUT CGD010 CAD010 CAD018 MTD019
           MOVE 1 TO GRAVA-W.
           IF ST-ACD120 = "35"
              CLOSE ACD120      OPEN OUTPUT ACD120
              CLOSE ACD120      OPEN I-O ACD120
           END-IF.
           IF ST-ACD121 = "35"
              CLOSE ACD121      OPEN OUTPUT ACD121
              CLOSE ACD121      OPEN I-O ACD121
           END-IF.
           IF ST-ACD120 <> "00"
              MOVE 1 TO ERRO-W
              STRING "ERRO ABERTURA ACD120: " ST-ACD120 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-ACD121 <> "00"
              MOVE 1 TO ERRO-W
              STRING "ERRO ABERTURA ACD121: " ST-ACD121 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CGD010 <> "00"
              MOVE 1 TO ERRO-W
              STRING "ERRO ABERTURA CGD010: " ST-CGD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CAD010 <> "00"
              MOVE 1 TO ERRO-W
              STRING "ERRO ABERTURA CAD010: " ST-CAD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CAD018 <> "00"
              MOVE 1 TO ERRO-W
              STRING "ERRO ABERTURA CAD018: " ST-CAD018 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-MTD019 <> "00"
              MOVE 1 TO ERRO-W
              STRING "ERRO ABERTURA MTD019: " ST-MTD019 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ERRO-W = 1
              MOVE 1 TO EXIT-FLG.

           IF ERRO-W = ZEROS
               PERFORM LIMPAR-DADOS
               PERFORM PROCURAR-PROXIMO
               PERFORM LOAD-SCREENSET.

       PROCURAR-PROXIMO SECTION.
           INITIALIZE REG-ACD120
           MOVE ALL "9" TO NUMERO-AC120
           START ACD120 KEY IS LESS THEN CHAVE-AC120 INVALID KEY
               MOVE 0 TO ACP-NUMERO
           NOT INVALID KEY
               READ ACD120 NEXT
               IF ST-ACD120 = "00" OR "02"
                  MOVE NUMERO-AC120 TO ACP-NUMERO
               ELSE
                  MOVE 0 TO ACP-NUMERO.

           ADD 1 TO ACP-NUMERO.

           MOVE ACP-NUMERO TO NUMERO-W
           MOVE 1          TO ACP-SEQ-NOVO
           MOVE 1          TO ACP-SEQ-ORIG.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN SAVE-FLG-TRUE
                    PERFORM SALVAR-DADOS
                    PERFORM LIMPAR-DADOS
                    MOVE ACP-NUMERO TO NUMERO-AC120
                    READ ACD120 INVALID KEY
                         PERFORM LIMPAR-DADOS
                         PERFORM PROCURAR-PROXIMO
                         MOVE "LIMPAR-LB" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                         MOVE 1 TO GRAVA-W
                    NOT INVALID KEY
                         PERFORM MONTAR-DATA-BLOCK
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                    END-READ
                    PERFORM LIMPAR-DADOS
                    PERFORM PROCURAR-PROXIMO
                    PERFORM SET-UP-FOR-REFRESH-SCREEN
                    MOVE 1 TO GRAVA-W
               WHEN CLR-FLG-TRUE
                    PERFORM LIMPAR-DADOS
               WHEN CRITICAR-TRUE
                    PERFORM CRITICAR-DADOS
               WHEN SUGESTAO-TRUE
                    PERFORM SUGESTAO-DADOS
           END-EVALUATE
           PERFORM CLEAR-FLAGS
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


       CRITICAR-DADOS SECTION.
           MOVE 0 TO FLAG-CRITICA
           EVALUATE CAMPO-CRITICA
               WHEN "EF-NUMERO"          PERFORM CRITICAR-NUMERO
               WHEN "EF-CONTRATO"        PERFORM CRITICAR-CONTRATO
               WHEN "EF-CIDADE"          PERFORM CRITICAR-CIDADE
               WHEN "EF-DOC-ORIG"        PERFORM CRITICAR-DOC-ORIG
               WHEN "EF-PARCELA-ORIG"    PERFORM CRITICAR-PARCELA-ORIG
               WHEN "SB-TIPO-ORIG"       PERFORM CRITICAR-TIPO-ORIG
               WHEN "EF-BANCO-ORIG"      PERFORM CRITICAR-BANCO-ORIG
               WHEN "EF-VALOR-ORIG"      PERFORM CRITICAR-VALOR-ORIG
               WHEN "EF-VENCTO-ORIG"     PERFORM CRITICAR-VENCTO-ORIG
               WHEN "EF-OBS-ORIG"        PERFORM CRITICAR-OBS-ORIG
               WHEN "EF-DOC-NOVO"        PERFORM CRITICAR-DOC-NOVO
               WHEN "EF-PARCELA-NOVO"    PERFORM CRITICAR-PARCELA-NOVO
               WHEN "SB-TIPO-NOVO"       PERFORM CRITICAR-TIPO-NOVO
               WHEN "EF-BANCO-NOVO"      PERFORM CRITICAR-BANCO-NOVO
               WHEN "EF-VALOR-NOVO"      PERFORM CRITICAR-VALOR-NOVO
               WHEN "EF-VENCTO-NOVO"     PERFORM CRITICAR-VENCTO-NOVO
               WHEN "EF-OBS-NOVO"        PERFORM CRITICAR-OBS-NOVO
               WHEN "EF-OBSERVACAO"      PERFORM CRITICAR-OBSERVACAO
               WHEN "EF-DATA-EMITENTE"   PERFORM CRITICAR-DATA-EMITENTE
               WHEN "INCLUIR-ORIG"       PERFORM INCLUIR-ORIG
               WHEN "ALTERAR-ORIG"       PERFORM ALTERAR-ORIG
               WHEN "INCLUIR-NOVO"       PERFORM INCLUIR-NOVO
               WHEN "ALTERAR-NOVO"       PERFORM ALTERAR-ORIG
               WHEN "GRAVAR"             PERFORM CRITICAR-NUMERO
                                            THRU CRITICAR-CONTRATO
           END-EVALUATE.


       CRITICAR-NUMERO SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-NUMERO EQUAL ZEROS
                  MOVE NUMERO-W TO ACP-NUMERO
                  PERFORM SET-UP-FOR-REFRESH-SCREEN
               ELSE
                  MOVE ACP-NUMERO TO NUMERO-AC120
                  READ ACD120 INVALID KEY
                      IF CAMPO-CRITICA = "EF-NUMERO"
                         PERFORM LIMPAR-DADOS
                         MOVE "LIMPAR-LB" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                      PERFORM PROCURAR-PROXIMO
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      MOVE 1 TO GRAVA-W
                  NOT INVALID KEY
                      IF CAMPO-CRITICA = "EF-NUMERO"
                         PERFORM MONTAR-DATA-BLOCK
                         PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-CONTRATO SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO EQUAL ZEROS
                   PERFORM SUGESTAO-CONTRATO
               END-IF
               IF ACP-CONTRATO EQUAL ZEROS
                   MOVE "Número do Contrato/Álbum Não Informado" TO
                   MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   MOVE ZERO               TO CLASSIF-CG10
                   MOVE ACP-CONTRATO       TO CODIGO-CG10
                   READ CGD010 INVALID KEY
                        MOVE "Contrato Não Encontrado" TO MENSAGEM
                        MOVE "C" TO TIPO-MSG
                        PERFORM EXIBIR-MENSAGEM
                        MOVE "********"    TO COMPRADOR-CG10
                   END-READ
                   MOVE COMPRADOR-CG10     TO DESC-NOME-COMPRADOR

                   MOVE ACP-CONTRATO       TO ALBUMMT19
                   READ MTD019 INVALID KEY
                      MOVE "Álbum Não Encontrado no Arquivo de Montagem"
                      TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                   NOT INVALID KEY
                      MOVE NOME-FORM-MT19  TO DESC-NOME-FORMANDO
                      MOVE CIDADE-MT19     TO ACP-CIDADE
                      MOVE ACP-CIDADE TO CIDADE
                      READ CAD010 INVALID KEY
                           MOVE "Cidade da Colação Inválida" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                      NOT INVALID KEY
                           MOVE NOME-COMPL-CID TO DESC-CIDADE
                      END-READ
                   END-READ
                   PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-CIDADE SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CIDADE EQUAL SPACES
                   MOVE "Cidade da Colação de Grau Não Informada" TO
                   MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   MOVE ACP-CIDADE TO CIDADE
                   READ CAD010 INVALID KEY
                       MOVE "Cidade da Colação Inválida" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                   NOT INVALID KEY
                       MOVE NOME-COMPL-CID TO DESC-CIDADE
                       PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-DOC-ORIG SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DOC-ORIG EQUAL SPACES
                   MOVE "Documento Original Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-PARCELA-ORIG SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-PARCELA-ORIG EQUAL ZEROS
                   MOVE "Número da Parcela Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-TIPO-ORIG SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-TIPO-ORIG <> "CHEQ" AND "DUPL"
                  MOVE "Tipo de Documento Informado Inválido" TO
                  MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM.

       CRITICAR-BANCO-ORIG SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-BANCO-ORIG EQUAL ZEROS
                   MOVE "Número do Banco Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-VALOR-ORIG SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-VALOR-ORIG EQUAL ZEROS
                   MOVE "Valor do Documento Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-VENCTO-ORIG SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-VENCTO-ORIG EQUAL ZEROS
                   MOVE "Data de Vencimento do Documento Não Informado"
                   TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   CALL "UTIVLDT" USING ACP-VENCTO-ORIG WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                      MOVE "Data de Vencimento do Documento Inválida" TO
                      MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM.

       CRITICAR-OBS-ORIG SECTION.

       CRITICAR-DOC-NOVO SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DOC-NOVO EQUAL SPACES
                   MOVE "Documento Novo Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-PARCELA-NOVO SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-PARCELA-NOVO EQUAL ZEROS
                   MOVE "Parcela Novo Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-TIPO-NOVO SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-TIPO-NOVO <> "CHEQ" AND "DUPL"
                   MOVE "Tipo de Documento Inválido" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-BANCO-NOVO SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-BANCO-NOVO EQUAL ZEROS
                   MOVE "Número do Banco Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-VALOR-NOVO SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-VALOR-NOVO EQUAL ZEROS
                   MOVE "Valor do Documento Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.


       CRITICAR-VENCTO-NOVO SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-VENCTO-NOVO EQUAL ZEROS
                   MOVE "Data de Vencimento do Documento Não Informado"
                   TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   CALL "UTIVLDT" USING ACP-VENCTO-NOVO WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                      MOVE "Data de Vencimento do Documento Inválida" TO
                      MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM.

       CRITICAR-OBS-NOVO SECTION.

       CRITICAR-OBSERVACAO SECTION.

       CRITICAR-DATA-EMITENTE SECTION.
           IF ACP-DATA-EMITENTE EQUAL ZEROS
               MOVE "Data do Emitente Não Informada" TO MENSAGEM
               MOVE "C" TO TIPO-MSG
               PERFORM EXIBIR-MENSAGEM
           ELSE
               CALL "UTIVLDT" USING ACP-DATA-EMITENTE WS-OK
               CANCEL "UTIVLDT"
               IF WS-OK EQUAL "N"
                  MOVE "Data Emitente Inválida" TO
                  MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM.

       INCLUIR-ORIG SECTION.
           PERFORM CRITICAR-DOC-ORIG
              THRU CRITICAR-OBS-ORIG

           IF MENSAGEM EQUAL SPACES
               MOVE ACP-SEQ-ORIG           TO DET-SEQUENCIA
               MOVE ACP-DOC-ORIG           TO DET-NUMERO-DOC
               MOVE ACP-PARCELA-ORIG       TO DET-PARCELA
               MOVE ACP-TIPO-ORIG          TO DET-TIPO-DOC
               MOVE ACP-BANCO-ORIG         TO DET-BANCO
               MOVE ACP-VALOR-ORIG         TO DET-VALOR
               MOVE ACP-VENCTO-ORIG        TO DET-VENCIMENTO
               MOVE ACP-OBS-ORIG           TO DET-OBSER

               MOVE DET-LINDET             TO LINDET

               MOVE "INSERIR-ORIG"         TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.

       ALTERAR-ORIG SECTION.
           MOVE LINDET                     TO DET-LINDET
           MOVE DET-SEQUENCIA              TO ACP-SEQ-ORIG
           MOVE DET-NUMERO-DOC             TO ACP-DOC-ORIG
           MOVE DET-PARCELA                TO ACP-PARCELA-ORIG
           MOVE DET-TIPO-DOC               TO ACP-TIPO-ORIG
           MOVE DET-BANCO                  TO ACP-BANCO-ORIG
           MOVE DET-VALOR                  TO ACP-VALOR-ORIG
           MOVE DET-VENCIMENTO             TO ACP-VENCTO-ORIG
           MOVE DET-OBSER                  TO ACP-OBS-ORIG
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       INCLUIR-NOVO SECTION.
           PERFORM CRITICAR-DOC-NOVO
              THRU CRITICAR-OBS-NOVO

           IF MENSAGEM EQUAL SPACES
               MOVE ACP-SEQ-NOVO           TO DET-SEQUENCIA
               MOVE ACP-DOC-NOVO           TO DET-NUMERO-DOC
               MOVE ACP-PARCELA-NOVO       TO DET-PARCELA
               MOVE ACP-TIPO-NOVO          TO DET-TIPO-DOC
               MOVE ACP-BANCO-NOVO         TO DET-BANCO
               MOVE ACP-VALOR-NOVO         TO DET-VALOR
               MOVE ACP-VENCTO-NOVO        TO DET-VENCIMENTO
               MOVE ACP-OBS-NOVO           TO DET-OBSER

               MOVE DET-LINDET             TO LINDET

               MOVE "INSERIR-NOVO"         TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM.

       ALTERAR-NOVO SECTION.
           MOVE LINDET                     TO DET-LINDET
           MOVE DET-SEQUENCIA              TO ACP-SEQ-NOVO
           MOVE DET-NUMERO-DOC             TO ACP-DOC-NOVO
           MOVE DET-PARCELA                TO ACP-PARCELA-NOVO
           MOVE DET-TIPO-DOC               TO ACP-TIPO-NOVO
           MOVE DET-BANCO                  TO ACP-BANCO-NOVO
           MOVE DET-VALOR                  TO ACP-VALOR-NOVO
           MOVE DET-VENCIMENTO             TO ACP-VENCTO-NOVO
           MOVE DET-OBSER                  TO ACP-OBS-NOVO
           PERFORM SET-UP-FOR-REFRESH-SCREEN.


       SUGESTAO-DADOS SECTION.
           EVALUATE CAMPO-CRITICA
               WHEN "EF-CONTRATO"    PERFORM SUGESTAO-CONTRATO
               WHEN "EF-CIDADE"      PERFORM SUGESTAO-CIDADE
               WHEN OTHER    MOVE "Sugestão Inexistente" TO MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM.

       SUGESTAO-CONTRATO SECTION.
           CALL   "CGP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(1:30) TO DESC-NOME-COMPRADOR
           MOVE PASSAR-STRING-1(33:8) TO ACP-CONTRATO
           MOVE ACP-CONTRATO       TO ALBUMMT19
           READ MTD019 INVALID KEY
              MOVE "Álbum Não Encontrado no Arquivo de Montagem"
              TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
              MOVE CIDADE-MT19     TO ACP-CIDADE
              MOVE NOME-FORM-MT19 TO DESC-NOME-FORMANDO
              MOVE ACP-CIDADE TO CIDADE
              READ CAD010 INVALID KEY
                   MOVE "Cidade da Colação Inválida" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                   MOVE NOME-COMPL-CID TO DESC-CIDADE
              END-READ
           END-READ
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       SUGESTAO-CIDADE SECTION.
           CALL   "CAP010T" USING PARAMETROS-W PASSAR-PARAMETROS.
           CANCEL "CAP010T".
           MOVE PASSAR-STRING-1(35: 4) TO ACP-CIDADE
           READ CAD010 INVALID KEY
                MOVE SPACES TO NOME-CID.
           MOVE NOME-CID               TO DESC-CIDADE
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       LIMPAR-DADOS SECTION.
           INITIALIZE REG-ACD120
           MOVE ACP-NUMERO  TO NUMERO-W
           INITIALIZE DATA-BLOCK
           MOVE NUMERO-W    TO ACP-NUMERO
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO
               ACP-DATA-EMITENTE

           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       SALVAR-DADOS SECTION.
           MOVE ACP-NUMERO              TO NUMERO-AC120

           MOVE ACP-CONTRATO            TO CONTRATO-AC120

           MOVE ACP-DTDEP1              TO DTDEP1-AC120
           MOVE ACP-DTDEP2              TO DTDEP2-AC120
           MOVE ACP-DTDEP3              TO DTDEP3-AC120
           MOVE ACP-DTDEP4              TO DTDEP4-AC120

           MOVE ACP-AGENCIA1            TO AGENCIA1-AC120
           MOVE ACP-AGENCIA2            TO AGENCIA2-AC120
           MOVE ACP-AGENCIA3            TO AGENCIA3-AC120
           MOVE ACP-AGENCIA4            TO AGENCIA4-AC120

           MOVE ACP-VLRORIG1            TO VLRORIG1-AC120
           MOVE ACP-VLRORIG2            TO VLRORIG2-AC120
           MOVE ACP-VLRORIG3            TO VLRORIG3-AC120
           MOVE ACP-VLRORIG4            TO VLRORIG4-AC120

           MOVE ACP-JUROS1              TO JUROS1-AC120
           MOVE ACP-JUROS2              TO JUROS2-AC120
           MOVE ACP-JUROS3              TO JUROS3-AC120
           MOVE ACP-JUROS4              TO JUROS4-AC120

           MOVE ACP-DESCONTO1           TO DESCONTO1-AC120
           MOVE ACP-DESCONTO2           TO DESCONTO2-AC120
           MOVE ACP-DESCONTO3           TO DESCONTO3-AC120
           MOVE ACP-DESCONTO4           TO DESCONTO4-AC120

           MOVE ACP-VLRTOTAL1           TO VLRTOTAL1-AC120
           MOVE ACP-VLRTOTAL2           TO VLRTOTAL2-AC120
           MOVE ACP-VLRTOTAL3           TO VLRTOTAL3-AC120
           MOVE ACP-VLRTOTAL4           TO VLRTOTAL4-AC120

           MOVE ACP-OBSERVACAO1         TO OBSERVACAO1-AC120
           MOVE ACP-OBSERVACAO2         TO OBSERVACAO2-AC120
           MOVE ACP-OBSERVACAO3         TO OBSERVACAO3-AC120
           MOVE ACP-OBSERVACAO4         TO OBSERVACAO4-AC120
           MOVE ACP-OBSERVACAO5         TO OBSERVACAO5-AC120
           MOVE ACP-OBSERVACAO6         TO OBSERVACAO6-AC120
           MOVE ACP-OBSERVACAO7         TO OBSERVACAO7-AC120
           MOVE ACP-OBSERVACAO8         TO OBSERVACAO8-AC120
           MOVE ACP-OBSERVACAO9         TO OBSERVACAO9-AC120
           MOVE ACP-OBSERVACAO10        TO OBSERVACAO10-AC120


           IF GRAVA-W = 1
              MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
              STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO
                                                   DATA-AC120
              WRITE REG-ACD120 INVALID KEY
                   PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                   PERFORM GRAVAR-ACD121
           ELSE
              MOVE ACP-DATA-EMITENTE    TO DATA-AC120
              REWRITE REG-ACD120 INVALID KEY
                   PERFORM ERRO-GRAVACAO
              NOT INVALID KEY
                   PERFORM EXCLUIR-ACD121
                   PERFORM GRAVAR-ACD121
           END-IF.

       EXCLUIR-ACD121 SECTION.
           INITIALIZE REG-ACD121
           MOVE ACP-NUMERO             TO NUMERO-AC121
           START ACD121 KEY IS NOT LESS CHAVE-AC121 INVALID KEY
               MOVE "10" TO ST-ACD121.

           PERFORM UNTIL ST-ACD121 = "10"
              READ ACD121 NEXT RECORD AT END
                   MOVE "10" TO ST-ACD121
              NOT AT END
                   IF ACP-NUMERO <> NUMERO-AC121
                       MOVE "10" TO ST-ACD121
                   ELSE
                       DELETE ACD121
                       END-DELETE
                   END-IF
              END-READ
           END-PERFORM.

       GRAVAR-ACD121 SECTION.
      * LER-DOCUMENTOS-ORIGINAIS
           MOVE SPACES TO LINDET
           MOVE 1      TO NUMERO-LINHA
           MOVE 0      TO SEQUENCIA
           MOVE "LER-LB-ORIG" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL LINDET = SPACES
               INITIALIZE REG-ACD121

               MOVE LINDET                     TO DET-LINDET

               MOVE ACP-NUMERO                 TO NUMERO-AC121
               MOVE 1                          TO TIPO-AC121
               ADD  1                          TO SEQUENCIA
               MOVE SEQUENCIA                  TO SEQ-AC121
               MOVE DET-NUMERO-DOC             TO NUMERO-DOCUMENTO-AC121
               MOVE DET-PARCELA                TO PARCELA-AC121
               MOVE DET-TIPO-DOC               TO TIPO-DOCUMENTO-AC121
               MOVE DET-BANCO                  TO BANCO-AC121
               MOVE DET-VALOR                  TO VALOR-AC121
               MOVE DET-VENCIMENTO             TO VENCIMENTO-AC121
               MOVE DET-OBSER                  TO OBSERVACAO-AC121

               WRITE REG-ACD121 INVALID KEY
                     MOVE "Erro de Gravação...ACD121" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
               END-WRITE

               MOVE SPACES                     TO LINDET
               ADD  1                          TO NUMERO-LINHA
               MOVE "LER-LB-ORIG" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM

           END-PERFORM.

      * LER-DOCUMENTOS-NOVOS
           MOVE SPACES TO LINDET
           MOVE 1      TO NUMERO-LINHA
           MOVE 0      TO SEQUENCIA
           MOVE "LER-LB-NOVO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           PERFORM UNTIL LINDET = SPACES
               INITIALIZE REG-ACD121

               MOVE LINDET                     TO DET-LINDET

               MOVE ACP-NUMERO                 TO NUMERO-AC121
               MOVE 2                          TO TIPO-AC121
               ADD  1                          TO SEQUENCIA
               MOVE SEQUENCIA                  TO SEQ-AC121
               MOVE DET-NUMERO-DOC             TO NUMERO-DOCUMENTO-AC121
               MOVE DET-PARCELA                TO PARCELA-AC121
               MOVE DET-TIPO-DOC               TO TIPO-DOCUMENTO-AC121
               MOVE DET-BANCO                  TO BANCO-AC121
               MOVE DET-VALOR                  TO VALOR-AC121
               MOVE DET-VENCIMENTO             TO VENCIMENTO-AC121
               MOVE DET-OBSER                  TO OBSERVACAO-AC121

               WRITE REG-ACD121 INVALID KEY
                     MOVE "Erro de Gravação...ACD121" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
               END-WRITE

               MOVE SPACES                     TO LINDET
               ADD  1                          TO NUMERO-LINHA
               MOVE "LER-LB-NOVO" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM

           END-PERFORM.

       ERRO-GRAVACAO SECTION.
           STRING "ERRO GRAVAÇÃO: " ST-ACD120 INTO MENSAGEM
           MOVE "C" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           PERFORM LOAD-SCREENSET.

       CLEAR-FLAGS SECTION.
           INITIALIZE FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "ACP120" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move    1 to flag-critica
           move spaces to mensagem.


       MONTAR-DATA-BLOCK SECTION.
           perform 800-limpar-campos-detalhes
           perform 520-abrir-impressora

           MOVE 2                                 TO GRAVA-W

           MOVE NUMERO-AC120                 TO ACP-NUMERO
                                                MASC-NUMERO

           MOVE MASC-NUMERO                  TO DET-NUMERO

      * DADOS DO 1º CONTRATO

           MOVE CONTRATO-AC120               TO ACP-CONTRATO
           MOVE ACP-CONTRATO                 TO MASC-CONTRATO
           MOVE MASC-CONTRATO                TO DET-CONTRATO

           MOVE ZERO                         TO CLASSIF-CG10
           MOVE ACP-CONTRATO                 TO CODIGO-CG10

           READ CGD010 INVALID KEY
                MOVE SPACES        TO COMPRADOR-CG10
           END-READ
           MOVE COMPRADOR-CG10     TO DESC-NOME-COMPRADOR

           MOVE ACP-CONTRATO       TO ALBUMMT19
           READ MTD019 INVALID KEY
               MOVE SPACES         TO DESC-NOME-FORMANDO
               MOVE SPACES         TO DESC-CIDADE
               MOVE 0              TO ACP-CIDADE
           NOT INVALID KEY
              MOVE NOME-FORM-MT19  TO DESC-NOME-FORMANDO
              MOVE CIDADE-MT19     TO ACP-CIDADE
              MOVE ACP-CIDADE TO CIDADE
              READ CAD010 INVALID KEY
                   MOVE "Cidade da Colação Inválida" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
              NOT INVALID KEY
                   MOVE NOME-COMPL-CID TO DESC-CIDADE
                   MOVE NOME-CID       TO DET-CIDADE
                   MOVE UF-CID         TO DET-ESTADO
              END-READ
           END-READ

           MOVE DESC-NOME-COMPRADOR TO DET-COMPRADOR
           MOVE DESC-NOME-FORMANDO  TO DET-FORMANDO

           MOVE DTDEP1-AC120                 TO ACP-DTDEP1
           IF ACP-DTDEP1 > 0
              MOVE ACP-DTDEP1                TO MASC-DATA
              MOVE MASC-DATA                 TO DET-DTDEP1
           ELSE
              MOVE SPACES                    TO DET-DTDEP1
           END-IF

           MOVE DTDEP2-AC120                 TO ACP-DTDEP2
           IF ACP-DTDEP2 > 0
              MOVE ACP-DTDEP2                TO MASC-DATA
              MOVE MASC-DATA                 TO DET-DTDEP2
           ELSE
              MOVE SPACES                    TO DET-DTDEP2
           END-IF

           MOVE DTDEP3-AC120                 TO ACP-DTDEP3
           IF ACP-DTDEP3 > 0
              MOVE ACP-DTDEP3                TO MASC-DATA
              MOVE MASC-DATA                 TO DET-DTDEP3
           ELSE
              MOVE SPACES                    TO DET-DTDEP3
           END-IF

           MOVE DTDEP4-AC120                 TO ACP-DTDEP4
           IF ACP-DTDEP4 > 0
              MOVE ACP-DTDEP4                TO MASC-DATA
              MOVE MASC-DATA                 TO DET-DTDEP4
           ELSE
              MOVE SPACES                    TO DET-DTDEP4
           END-IF

           MOVE AGENCIA1-AC120               TO ACP-AGENCIA1
                                                DET-AGENCIA1

           MOVE AGENCIA2-AC120               TO ACP-AGENCIA2
                                                DET-AGENCIA2

           MOVE AGENCIA3-AC120               TO ACP-AGENCIA3
                                                DET-AGENCIA3

           MOVE AGENCIA4-AC120               TO ACP-AGENCIA4
                                                DET-AGENCIA4

           MOVE VLRORIG1-AC120               TO ACP-VLRORIG1
           IF ACP-VLRORIG1 > 0
              MOVE ACP-VLRORIG1              TO MASC-VALOR
              MOVE MASC-VALOR                TO DET-VLRORIG1
           ELSE
              MOVE SPACES                    TO DET-VLRORIG1
           END-IF

           MOVE VLRORIG2-AC120               TO ACP-VLRORIG2
           IF ACP-VLRORIG2 > 0
              MOVE ACP-VLRORIG2              TO MASC-VALOR
              MOVE MASC-VALOR                TO DET-VLRORIG2
           ELSE
              MOVE SPACES                    TO DET-VLRORIG2
           END-IF

           MOVE VLRORIG3-AC120               TO ACP-VLRORIG3
           IF ACP-VLRORIG3 > 0
              MOVE ACP-VLRORIG3              TO MASC-VALOR
              MOVE MASC-VALOR                TO DET-VLRORIG3
           ELSE
              MOVE SPACES                    TO DET-VLRORIG3
           END-IF

           MOVE VLRORIG4-AC120               TO ACP-VLRORIG4
           IF ACP-VLRORIG4 > 0
              MOVE ACP-VLRORIG4              TO MASC-VALOR
              MOVE MASC-VALOR                TO DET-VLRORIG4
           ELSE
              MOVE SPACES                    TO DET-VLRORIG4
           END-IF

           MOVE JUROS1-AC120                 TO ACP-JUROS1
           IF ACP-JUROS1 > 0
              MOVE ACP-JUROS1                TO MASC-JUROS
              MOVE MASC-JUROS                TO DET-JUROS1
           ELSE
              MOVE SPACES                    TO DET-JUROS1
           END-IF

           MOVE JUROS2-AC120                 TO ACP-JUROS2
           IF ACP-JUROS2 > 0
              MOVE ACP-JUROS2                TO MASC-JUROS
              MOVE MASC-JUROS                TO DET-JUROS2
           ELSE
              MOVE SPACES                    TO DET-JUROS2
           END-IF

           MOVE JUROS3-AC120                 TO ACP-JUROS3
           IF ACP-JUROS3 > 0
              MOVE ACP-JUROS3                TO MASC-JUROS
              MOVE MASC-JUROS                TO DET-JUROS3
           ELSE
              MOVE SPACES                    TO DET-JUROS3
           END-IF

           MOVE JUROS4-AC120                 TO ACP-JUROS4
           IF ACP-JUROS4 > 0
              MOVE ACP-JUROS4                TO MASC-JUROS
              MOVE MASC-JUROS                TO DET-JUROS4
           ELSE
              MOVE SPACES                    TO DET-JUROS4
           END-IF

           MOVE DESCONTO1-AC120              TO ACP-DESCONTO1
           IF ACP-DESCONTO1 > 0
              MOVE ACP-DESCONTO1             TO MASC-JUROS
              MOVE MASC-JUROS                TO DET-DESCONTO1
           ELSE
              MOVE SPACES                    TO DET-DESCONTO1
           END-IF

           MOVE DESCONTO2-AC120              TO ACP-DESCONTO2
           IF ACP-DESCONTO2 > 0
              MOVE ACP-DESCONTO2             TO MASC-JUROS
              MOVE MASC-JUROS                TO DET-DESCONTO2
           ELSE
              MOVE SPACES                    TO DET-DESCONTO2
           END-IF

           MOVE DESCONTO3-AC120              TO ACP-DESCONTO3
           IF ACP-DESCONTO3 > 0
              MOVE ACP-DESCONTO3             TO MASC-JUROS
              MOVE MASC-JUROS                TO DET-DESCONTO3
           ELSE
              MOVE SPACES                    TO DET-DESCONTO3
           END-IF

           MOVE DESCONTO4-AC120              TO ACP-DESCONTO4
           IF ACP-DESCONTO4 > 0
              MOVE ACP-DESCONTO4             TO MASC-JUROS
              MOVE MASC-JUROS                TO DET-DESCONTO4
           ELSE
              MOVE SPACES                    TO DET-DESCONTO4
           END-IF

           MOVE VLRTOTAL1-AC120              TO ACP-VLRTOTAL1
           IF ACP-VLRTOTAL1 > 0
              MOVE ACP-VLRTOTAL1             TO MASC-VALOR
              MOVE MASC-VALOR                TO DET-VLRTOTAL1
           ELSE
              MOVE SPACES                    TO DET-DESCONTO1
           END-IF

           MOVE VLRTOTAL2-AC120              TO ACP-VLRTOTAL2
           IF ACP-VLRTOTAL2 > 0
              MOVE ACP-VLRTOTAL2             TO MASC-VALOR
              MOVE MASC-VALOR                TO DET-VLRTOTAL2
           ELSE
              MOVE SPACES                    TO DET-DESCONTO2
           END-IF

           MOVE VLRTOTAL3-AC120              TO ACP-VLRTOTAL3
           IF ACP-VLRTOTAL3 > 0
              MOVE ACP-VLRTOTAL3             TO MASC-VALOR
              MOVE MASC-VALOR                TO DET-VLRTOTAL3
           ELSE
              MOVE SPACES                    TO DET-DESCONTO3
           END-IF

           MOVE VLRTOTAL4-AC120              TO ACP-VLRTOTAL4
           IF ACP-VLRTOTAL4 > 0
              MOVE ACP-VLRTOTAL4             TO MASC-VALOR
              MOVE MASC-VALOR                TO DET-VLRTOTAL4
           ELSE
              MOVE SPACES                    TO DET-DESCONTO4
           END-IF




           MOVE OBSERVACAO1-AC120            TO ACP-OBSERVACAO1
                                                DET-OBSERVACAO1
           MOVE OBSERVACAO2-AC120            TO ACP-OBSERVACAO2
                                                DET-OBSERVACAO2
           MOVE OBSERVACAO3-AC120            TO ACP-OBSERVACAO3
                                                DET-OBSERVACAO3
           MOVE OBSERVACAO4-AC120            TO ACP-OBSERVACAO4
                                                DET-OBSERVACAO4
           MOVE OBSERVACAO5-AC120            TO ACP-OBSERVACAO5
                                                DET-OBSERVACAO5
           MOVE OBSERVACAO6-AC120            TO ACP-OBSERVACAO6
                                                DET-OBSERVACAO6
           MOVE OBSERVACAO7-AC120            TO ACP-OBSERVACAO7
                                                DET-OBSERVACAO7
           MOVE OBSERVACAO8-AC120            TO ACP-OBSERVACAO8
                                                DET-OBSERVACAO8
           MOVE OBSERVACAO9-AC120            TO ACP-OBSERVACAO9
                                                DET-OBSERVACAO9
           MOVE OBSERVACAO10-AC120           TO ACP-OBSERVACAO10
                                                DET-OBSERVACAO10


           MOVE DATA-AC120                   TO ACP-DATA-EMITENTE
                                                MASC-DATA
           IF DATA-AC120 > 0
              MOVE DATA-AC120                TO AUX-DATA
              EVALUATE AUX-MES2
                   WHEN 1  MOVE "Janeiro"   TO DET-MES
                   WHEN 2  MOVE "Fevereiro" TO DET-MES
                   WHEN 3  MOVE "Março"     TO DET-MES
                   WHEN 4  MOVE "Abril"     TO DET-MES
                   WHEN 5  MOVE "Maio"      TO DET-MES
                   WHEN 6  MOVE "Junho"     TO DET-MES
                   WHEN 7  MOVE "Julho"     TO DET-MES
                   WHEN 8  MOVE "Agosto"    TO DET-MES
                   WHEN 9  MOVE "Setembro"  TO DET-MES
                   WHEN 10 MOVE "Outubro"   TO DET-MES
                   WHEN 11 MOVE "Novembro"  TO DET-MES
                   WHEN 12 MOVE "Dezembro"  TO DET-MES
              END-EVALUATE

              STRING "SANTA FÉ, " AUX-DIA2 " de " DET-MES " de "
              AUX-ANO2 DELIMITED BY "  " INTO DET-DIA
           ELSE
              MOVE SPACES                    TO DET-DIA
           END-IF

           PERFORM CARREGAR-DOCUMENTOS

           MOVE DET-001                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-002                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-003                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-004                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-005                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-006                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-007                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-008                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-009                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-010                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-011                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-012                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-013                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-014                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-015                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-016                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-017                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-018                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-019                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-020                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-021                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-022                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-023                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-024                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-025                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-026                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-027                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-028                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-029                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-030                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-031                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-032                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-033                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-034                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-035                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-036                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-037                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-038                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-039                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-040                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-041                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-042                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-043                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-044                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-045                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-046                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-047                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-048                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-049                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-050                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-051                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-052                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-053                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-054                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-055                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-056                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-057                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-058                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-059                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-060                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-061                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-062                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-063                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-064                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-065                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-066                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-067                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-068                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-069                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-070                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-071                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-072                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-073                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-074                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-075                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-076                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-077                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-078                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-079                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-080                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-081                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-082                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-083                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-084                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-085                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-086                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-087                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-088                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-089                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-090                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-091                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-092                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-093                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-094                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-095                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-096                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-097                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-098                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-099                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-100                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-101                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-102                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-103                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-104                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-105                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-106                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-107                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-108                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-109                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-110                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-111                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-112                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-113                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-114                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-115                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-116                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-117                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-118                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-119                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-120                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-121                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-122                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-123                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-124                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-125                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-126                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-127                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-128                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-129                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-130                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-131                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-132                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-133                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-134                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-135                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-136                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-137                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-138                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-139                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-140                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-141                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-142                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-143                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-144                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-145                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-146                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-147                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-148                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-149                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-150                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-151                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-152                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-153                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-154                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-155                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-156                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-157                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-158                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-159                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-160                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-161                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-162                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-163                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-164                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-165                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-166                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-167                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-168                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-169                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-170                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-171                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-172                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-173                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-174                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-175                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-176                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-177                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-178                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-179                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-180                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-181                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-182                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-183                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-184                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-185                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-186                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-187                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-188                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-189                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-190                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-191                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-192                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-193                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-194                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-195                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-196                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-197                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-198                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-199                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-200                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-201                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-202                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-203                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-204                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-205                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-206                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-207                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-208                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-209                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-210                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-211                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-212                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-213                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-214                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-215                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-216                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-217                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-218                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-219                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-220                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-221                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-222                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-223                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-224                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-225                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-226                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-227                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-228                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-229                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-230                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-231                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-232                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-233                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-234                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-235                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-236                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-237                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-238                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-239                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-240                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-241                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-242                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-243                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-244                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-245                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-246                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-247                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-248                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-249                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-250                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-251                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-252                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-253                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-254                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-255                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-256                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-257                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-258                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-259                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-260                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-261                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-262                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-263                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-264                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-265                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-266                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-267                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-268                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-269                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-270                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA


           MOVE DET-271                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-272                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-273                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-274                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-275                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-276                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-277                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-278                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-279                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-280                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-281                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-282                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-283                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-284                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-285                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-286                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-287                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-288                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-289                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-290                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-291                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-292                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-293                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-294                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-295                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-296                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-297                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-298                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-299                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-300                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-301                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-302                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-303                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-304                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-305                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-306                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-307                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-308                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-309                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-310                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-311                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-312                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-313                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-314                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-315                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-316                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-317                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-318                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-319                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-320                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-321                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-322                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-323                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-324                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-325                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-326                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-327                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-328                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-329                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-330                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-331                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-332                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-333                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-334                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-335                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-336                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-337                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-338                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-339                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-340                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-341                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-342                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-343                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-344                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-345                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-346                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-347                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-348                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-349                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-350                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-351                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-352                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-353                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-354                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-355                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-356                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-357                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-358                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-359                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-360                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-361                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-362                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-363                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-364                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-365                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-366                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-367                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-368                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-369                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-370                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-371                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-372                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-373                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-374                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-375                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-376                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-377                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-378                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-379                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-380                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-381                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-382                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-383                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-384                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-385                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-386                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-387                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-388                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-389                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-390                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-391                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-392                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-393                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-394                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-395                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-396                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-397                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
      *    MOVE DET-398                    TO LNK-DETALHE
      *    PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-399                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-400                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-401                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-402                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-403                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-404                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-405                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-406                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-407                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-408                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-409                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-410                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-411                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-412                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-413                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-414                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-415                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-416                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-417                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-418                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-419                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-420                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-421                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-422                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-423                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-424                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-425                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-426                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-427                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-428                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-429                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-430                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-431                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-432                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-433                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-434                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-435                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-436                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-437                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-438                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-439                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-440                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-441                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-442                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-443                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-444                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-445                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-446                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-447                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-448                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-449                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-450                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-451                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-452                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-453                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-454                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-455                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-456                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-457                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-458                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-459                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-460                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-461                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-462                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-463                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-464                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-465                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-466                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-467                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-468                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-469                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-470                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-471                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-472                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-473                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-474                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-475                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-476                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-477                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-478                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-479                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-480                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-481                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-482                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-483                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-484                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-485                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-486                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-487                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-488                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-489                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-490                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-491                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-492                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-493                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-494                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-495                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-496                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-497                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-498                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-499                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-500                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-501                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-502                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-503                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-504                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-505                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-506                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-507                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-508                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-509                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-510                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-511                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-512                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-513                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-514                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-515                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-516                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-517                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-518                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-519                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-520                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-521                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-522                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-523                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-524                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-525                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-526                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-527                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-528                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-529                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-530                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-531                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-532                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-533                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-534                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-535                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-536                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-537                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-538                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-539                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-540                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-541                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-542                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-543                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-544                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-545                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-546                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-547                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-548                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-549                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-550                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-551                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-552                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-553                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-554                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-555                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-556                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-557                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-558                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-559                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-560                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-561                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-562                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-563                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-564                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-565                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-566                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-567                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-568                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-569                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-570                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-571                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-572                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-573                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-574                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-575                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-576                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-577                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-578                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-579                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-580                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-581                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-582                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-583                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-584                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-585                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-586                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-587                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-588                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-589                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-590                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-591                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-592                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-593                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-594                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-595                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-596                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-597                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-598                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-599                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-600                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-601                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-602                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-603                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-604                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-605                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-606                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-607                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-608                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-609                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-610                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-611                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-612                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-613                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-614                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-615                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-616                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-617                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-618                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-619                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-620                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-621                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-622                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-623                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-624                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-625                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-626                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-627                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-628                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-629                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-630                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-631                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-632                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-633                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-634                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-635                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-636                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-637                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-638                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-639                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-640                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-641                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-642                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-643                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-644                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-645                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-646                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-647                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-648                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-649                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-650                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-651                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-652                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-653                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-654                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-655                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-656                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-657                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-658                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-659                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-660                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-661                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-662                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-663                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-664                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-665                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-666                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-667                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-668                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-669                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-670                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-671                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-672                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-673                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-674                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-675                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-676                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-677                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-678                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-679                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-680                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-681                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-682                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-683                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-684                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-685                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-686                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-687                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-688                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-689                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-690                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           MOVE DET-691                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-692                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-693                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
           MOVE DET-694                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA

           perform 530-fechar-impressora.

           move "Visualizar o Departamento de Contas a Receber ?" to
           mensagem
           move "Q" to tipo-msg
           perform exibir-mensagem

           if resp-msg equal "S"
              STRING "C:\Arquiv~1\Intern~1\IEXPLORE.EXE "
                     "file:///C:\ORDSERV.HTM"
                     INTO ARQ-EXPLORER
              move "CHAMAR-EXPLORER" to ds-procedure
              perform call-dialog-system.

       CARREGAR-DOCUMENTOS SECTION.
           MOVE "LIMPAR-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           INITIALIZE REG-ACD121
           MOVE ACP-NUMERO         TO NUMERO-AC121
           START ACD121 KEY IS NOT LESS CHAVE-AC121 INVALID KEY
               MOVE "10" TO ST-ACD121.

           PERFORM UNTIL ST-ACD121 = "10"
               READ ACD121 NEXT RECORD AT END
                   MOVE "10" TO ST-ACD121
               NOT AT END
                   IF ACP-NUMERO <> NUMERO-AC121
                      MOVE "10" TO ST-ACD121
                   ELSE
                      IF TIPO-AC121 = 1
                         MOVE SEQ-AC121              TO DET-SEQUENCIA
                                                        ACP-SEQ-ORIG
                         MOVE NUMERO-DOCUMENTO-AC121 TO DET-NUMERO-DOC
                         MOVE PARCELA-AC121          TO DET-PARCELA
                         MOVE TIPO-DOCUMENTO-AC121   TO DET-TIPO-DOC
                         MOVE BANCO-AC121            TO DET-BANCO
                         MOVE VALOR-AC121            TO DET-VALOR
                         MOVE VENCIMENTO-AC121       TO DET-VENCIMENTO
                         MOVE OBSERVACAO-AC121       TO DET-OBSER
                         EVALUATE SEQ-AC121
                            WHEN 1  PERFORM MOVER-TIPO11
                            WHEN 2  PERFORM MOVER-TIPO12
                            WHEN 3  PERFORM MOVER-TIPO13
                            WHEN 4  PERFORM MOVER-TIPO14
                            WHEN 5  PERFORM MOVER-TIPO15
                            WHEN 6  PERFORM MOVER-TIPO16
                            WHEN 7  PERFORM MOVER-TIPO17
                            WHEN 8  PERFORM MOVER-TIPO18
                            WHEN 9  PERFORM MOVER-TIPO19
                            WHEN 10 PERFORM MOVER-TIPO110
                            WHEN 11 PERFORM MOVER-TIPO111
                            WHEN 12 PERFORM MOVER-TIPO112
                         END-EVALUATE

                         MOVE DET-LINDET             TO LINDET
                         MOVE "INSERIR-ORIG"         TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      ELSE
                         MOVE SEQ-AC121              TO DET-SEQUENCIA
                                                        ACP-SEQ-NOVO
                         MOVE NUMERO-DOCUMENTO-AC121 TO DET-NUMERO-DOC
                         MOVE PARCELA-AC121          TO DET-PARCELA
                         MOVE TIPO-DOCUMENTO-AC121   TO DET-TIPO-DOC
                         MOVE BANCO-AC121            TO DET-BANCO
                         MOVE VALOR-AC121            TO DET-VALOR
                         MOVE VENCIMENTO-AC121       TO DET-VENCIMENTO
                         MOVE OBSERVACAO-AC121       TO DET-OBSER
                         EVALUATE SEQ-AC121
                            WHEN 1  PERFORM MOVER-TIPO21
                            WHEN 2  PERFORM MOVER-TIPO22
                            WHEN 3  PERFORM MOVER-TIPO23
                            WHEN 4  PERFORM MOVER-TIPO24
                            WHEN 5  PERFORM MOVER-TIPO25
                            WHEN 6  PERFORM MOVER-TIPO26
                            WHEN 7  PERFORM MOVER-TIPO27
                            WHEN 8  PERFORM MOVER-TIPO28
                            WHEN 9  PERFORM MOVER-TIPO29
                            WHEN 10 PERFORM MOVER-TIPO210
                            WHEN 11 PERFORM MOVER-TIPO211
                            WHEN 12 PERFORM MOVER-TIPO212
                         END-EVALUATE


                         MOVE DET-LINDET             TO LINDET
                         MOVE "INSERIR-NOVO"         TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                   END-IF
               END-READ
           END-PERFORM

           ADD 1 TO ACP-SEQ-ORIG
           ADD 1 TO ACP-SEQ-NOVO
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       MOVER-TIPO11 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-ORIG1
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-ORIG1
          ELSE
             MOVE SPACES                 TO DET-PARCE-ORIG1
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-ORIG1
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-ORIG1
          ELSE
             MOVE SPACES                 TO DET-BANCO-ORIG1
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-ORIG1
          ELSE
             MOVE SPACES                 TO DET-VALOR-ORIG1
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-ORIG1
          ELSE
             MOVE SPACES                 TO DET-VENCTO-ORIG1
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-ORIG1.

       MOVER-TIPO12 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-ORIG2
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-ORIG2
          ELSE
             MOVE SPACES                 TO DET-PARCE-ORIG2
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-ORIG2
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-ORIG2
          ELSE
             MOVE SPACES                 TO DET-BANCO-ORIG2
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-ORIG2
          ELSE
             MOVE SPACES                 TO DET-VALOR-ORIG2
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-ORIG2
          ELSE
             MOVE SPACES                 TO DET-VENCTO-ORIG2
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-ORIG2.

       MOVER-TIPO13 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-ORIG3
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-ORIG3
          ELSE
             MOVE SPACES                 TO DET-PARCE-ORIG3
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-ORIG3
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-ORIG3
          ELSE
             MOVE SPACES                 TO DET-BANCO-ORIG3
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-ORIG3
          ELSE
             MOVE SPACES                 TO DET-VALOR-ORIG3
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-ORIG3
          ELSE
             MOVE SPACES                 TO DET-VENCTO-ORIG3
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-ORIG3.

       MOVER-TIPO14 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-ORIG4
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-ORIG4
          ELSE
             MOVE SPACES                 TO DET-PARCE-ORIG4
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-ORIG4
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-ORIG4
          ELSE
             MOVE SPACES                 TO DET-BANCO-ORIG4
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-ORIG4
          ELSE
             MOVE SPACES                 TO DET-VALOR-ORIG4
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-ORIG4
          ELSE
             MOVE SPACES                 TO DET-VENCTO-ORIG4
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-ORIG4.

       MOVER-TIPO15 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-ORIG5
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-ORIG5
          ELSE
             MOVE SPACES                 TO DET-PARCE-ORIG5
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-ORIG5
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-ORIG5
          ELSE
             MOVE SPACES                 TO DET-BANCO-ORIG5
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-ORIG5
          ELSE
             MOVE SPACES                 TO DET-VALOR-ORIG5
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-ORIG5
          ELSE
             MOVE SPACES                 TO DET-VENCTO-ORIG5
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-ORIG5.

       MOVER-TIPO16 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-ORIG6
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-ORIG6
          ELSE
             MOVE SPACES                 TO DET-PARCE-ORIG6
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-ORIG6
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-ORIG6
          ELSE
             MOVE SPACES                 TO DET-BANCO-ORIG6
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-ORIG6
          ELSE
             MOVE SPACES                 TO DET-VALOR-ORIG6
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-ORIG6
          ELSE
             MOVE SPACES                 TO DET-VENCTO-ORIG6
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-ORIG6.

       MOVER-TIPO17 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-ORIG7
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-ORIG7
          ELSE
             MOVE SPACES                 TO DET-PARCE-ORIG7
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-ORIG7
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-ORIG7
          ELSE
             MOVE SPACES                 TO DET-BANCO-ORIG7
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-ORIG7
          ELSE
             MOVE SPACES                 TO DET-VALOR-ORIG7
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-ORIG7
          ELSE
             MOVE SPACES                 TO DET-VENCTO-ORIG7
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-ORIG7.

       MOVER-TIPO18 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-ORIG8
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-ORIG8
          ELSE
             MOVE SPACES                 TO DET-PARCE-ORIG8
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-ORIG8
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-ORIG8
          ELSE
             MOVE SPACES                 TO DET-BANCO-ORIG8
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-ORIG8
          ELSE
             MOVE SPACES                 TO DET-VALOR-ORIG8
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-ORIG8
          ELSE
             MOVE SPACES                 TO DET-VENCTO-ORIG8
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-ORIG8.

       MOVER-TIPO19 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-ORIG9
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-ORIG9
          ELSE
             MOVE SPACES                 TO DET-PARCE-ORIG9
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-ORIG9
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-ORIG9
          ELSE
             MOVE SPACES                 TO DET-BANCO-ORIG9
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-ORIG9
          ELSE
             MOVE SPACES                 TO DET-VALOR-ORIG9
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-ORIG9
          ELSE
             MOVE SPACES                 TO DET-VENCTO-ORIG9
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-ORIG9.

       MOVER-TIPO110 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-ORIG10
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-ORIG10
          ELSE
             MOVE SPACES                 TO DET-PARCE-ORIG10
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-ORIG10
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-ORIG10
          ELSE
             MOVE SPACES                 TO DET-BANCO-ORIG10
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-ORIG10
          ELSE
             MOVE SPACES                 TO DET-VALOR-ORIG10
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-ORIG10
          ELSE
             MOVE SPACES                 TO DET-VENCTO-ORIG10
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-ORIG10.

       MOVER-TIPO111 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-ORIG11
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-ORIG11
          ELSE
             MOVE SPACES                 TO DET-PARCE-ORIG11
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-ORIG11
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-ORIG11
          ELSE
             MOVE SPACES                 TO DET-BANCO-ORIG11
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-ORIG11
          ELSE
             MOVE SPACES                 TO DET-VALOR-ORIG11
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-ORIG11
          ELSE
             MOVE SPACES                 TO DET-VENCTO-ORIG11
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-ORIG11.

       MOVER-TIPO112 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-ORIG12
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-ORIG12
          ELSE
             MOVE SPACES                 TO DET-PARCE-ORIG12
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-ORIG12
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-ORIG12
          ELSE
             MOVE SPACES                 TO DET-BANCO-ORIG12
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-ORIG12
          ELSE
             MOVE SPACES                 TO DET-VALOR-ORIG12
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-ORIG12
          ELSE
             MOVE SPACES                 TO DET-VENCTO-ORIG12
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-ORIG12.

      *  NOVOS

       MOVER-TIPO21 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-NOVO1
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-NOVO1
          ELSE
             MOVE SPACES                 TO DET-PARCE-NOVO1
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-NOVO1
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-NOVO1
          ELSE
             MOVE SPACES                 TO DET-BANCO-NOVO1
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-NOVO1
          ELSE
             MOVE SPACES                 TO DET-VALOR-NOVO1
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-NOVO1
          ELSE
             MOVE SPACES                 TO DET-VENCTO-NOVO1
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-NOVO1.

       MOVER-TIPO22 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-NOVO2
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-NOVO2
          ELSE
             MOVE SPACES                 TO DET-PARCE-NOVO2
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-NOVO2
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-NOVO2
          ELSE
             MOVE SPACES                 TO DET-BANCO-NOVO2
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-NOVO2
          ELSE
             MOVE SPACES                 TO DET-VALOR-NOVO2
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-NOVO2
          ELSE
             MOVE SPACES                 TO DET-VENCTO-NOVO2
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-NOVO2.

       MOVER-TIPO23 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-NOVO3
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCELA-NOVO3
          ELSE
             MOVE SPACES                 TO DET-PARCELA-NOVO3
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-NOVO3
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-NOVO3
          ELSE
             MOVE SPACES                 TO DET-BANCO-NOVO3
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-NOVO3
          ELSE
             MOVE SPACES                 TO DET-VALOR-NOVO3
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-NOVO3
          ELSE
             MOVE SPACES                 TO DET-VENCTO-NOVO3
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-NOVO3.

       MOVER-TIPO24 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-NOVO4
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-NOVO4
          ELSE
             MOVE SPACES                 TO DET-PARCE-NOVO4
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-NOVO4
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-NOVO4
          ELSE
             MOVE SPACES                 TO DET-BANCO-NOVO4
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-NOVO4
          ELSE
             MOVE SPACES                 TO DET-VALOR-NOVO4
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-NOVO4
          ELSE
             MOVE SPACES                 TO DET-VENCTO-NOVO4
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-NOVO4.

       MOVER-TIPO25 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-NOVO5
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-NOVO5
          ELSE
             MOVE SPACES                 TO DET-PARCE-NOVO5
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-NOVO5
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-NOVO5
          ELSE
             MOVE SPACES                 TO DET-BANCO-NOVO5
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-NOVO5
          ELSE
             MOVE SPACES                 TO DET-VALOR-NOVO5
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-NOVO5
          ELSE
             MOVE SPACES                 TO DET-VENCTO-NOVO5
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-NOVO5.

       MOVER-TIPO26 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-NOVO6
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-NOVO6
          ELSE
             MOVE SPACES                 TO DET-PARCE-NOVO6
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-NOVO6
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-NOVO6
          ELSE
             MOVE SPACES                 TO DET-BANCO-NOVO6
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-NOVO6
          ELSE
             MOVE SPACES                 TO DET-VALOR-NOVO6
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-NOVO6
          ELSE
             MOVE SPACES                 TO DET-VENCTO-NOVO6
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-NOVO6.

       MOVER-TIPO27 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-NOVO7
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-NOVO7
          ELSE
             MOVE SPACES                 TO DET-PARCE-NOVO7
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-NOVO7
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-NOVO7
          ELSE
             MOVE SPACES                 TO DET-BANCO-NOVO7
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-NOVO7
          ELSE
             MOVE SPACES                 TO DET-VALOR-NOVO7
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-NOVO7
          ELSE
             MOVE SPACES                 TO DET-VENCTO-NOVO7
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-NOVO7.

       MOVER-TIPO28 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-NOVO8
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-NOVO8
          ELSE
             MOVE SPACES                 TO DET-PARCE-NOVO8
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-NOVO8
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-NOVO8
          ELSE
             MOVE SPACES                 TO DET-BANCO-NOVO8
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-NOVO8
          ELSE
             MOVE SPACES                 TO DET-VALOR-NOVO8
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-NOVO8
          ELSE
             MOVE SPACES                 TO DET-VENCTO-NOVO8
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-NOVO8.

       MOVER-TIPO29 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-NOVO9
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-NOVO9
          ELSE
             MOVE SPACES                 TO DET-PARCE-NOVO9
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-NOVO9
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-NOVO9
          ELSE
             MOVE SPACES                 TO DET-BANCO-NOVO9
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-NOVO9
          ELSE
             MOVE SPACES                 TO DET-VALOR-NOVO9
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-NOVO9
          ELSE
             MOVE SPACES                 TO DET-VENCTO-NOVO9
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-NOVO9.

       MOVER-TIPO210 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-NOVO10
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-NOVO10
          ELSE
             MOVE SPACES                 TO DET-PARCE-NOVO10
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-NOVO10
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-NOVO10
          ELSE
             MOVE SPACES                 TO DET-BANCO-NOVO10
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-NOVO10
          ELSE
             MOVE SPACES                 TO DET-VALOR-NOVO10
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-NOVO10
          ELSE
             MOVE SPACES                 TO DET-VENCTO-NOVO10
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-NOVO10.

       MOVER-TIPO211 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-NOVO11
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-NOVO11
          ELSE
             MOVE SPACES                 TO DET-PARCE-NOVO11
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-NOVO11
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-NOVO11
          ELSE
             MOVE SPACES                 TO DET-BANCO-NOVO11
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-NOVO11
          ELSE
             MOVE SPACES                 TO DET-VALOR-NOVO11
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-NOVO11
          ELSE
             MOVE SPACES                 TO DET-VENCTO-NOVO11
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-NOVO11.

       MOVER-TIPO212 SECTION.
          MOVE DET-NUMERO-DOC            TO DET-DOC-NOVO12
          IF DET-PARCELA > 0
             MOVE DET-PARCELA            TO MASC-PARCELA
             MOVE MASC-PARCELA           TO DET-PARCE-NOVO12
          ELSE
             MOVE SPACES                 TO DET-PARCE-NOVO12
          END-IF
          MOVE DET-TIPO-DOC              TO DET-TIPO-NOVO12
          IF DET-BANCO > 0
             MOVE DET-BANCO              TO MASC-BANCO
             MOVE MASC-BANCO             TO DET-BANCO-NOVO12
          ELSE
             MOVE SPACES                 TO DET-BANCO-NOVO12
          END-IF
          IF VALOR-AC121 > 0
             MOVE VALOR-AC121              TO MASC-VALOR
             MOVE MASC-VALOR             TO DET-VALOR-NOVO12
          ELSE
             MOVE SPACES                 TO DET-VALOR-NOVO12
          END-IF
          IF DET-VENCIMENTO > 0
             MOVE DET-VENCIMENTO         TO MASC-DATA
             MOVE MASC-DATA              TO DET-VENCTO-NOVO12
          ELSE
             MOVE SPACES                 TO DET-VENCTO-NOVO12
          END-IF
          MOVE DET-OBSER                 TO DET-OBS-NOVO12.


       520-abrir-impressora section.
           MOVE "C:\ORDSERV.HTM" TO ARQUIVO-ORDSERV
           OPEN OUTPUT ORDSERV.
       520-abrir-impressora-fim.
           exit.

       530-fechar-impressora section.
           CLOSE ORDSERV.
       530-fechar-impressora-fim.
           exit.

       590-imprimir-linha section.
           move lnk-detalhe        to reg-ordserv
           write reg-ordserv.
       590-imprimir-linha-fim.
           exit.

       800-limpar-campos-detalhes section.
           MOVE SPACES TO DET-NUMERO
                          DET-FORMANDO
                          DET-COMPRADOR
                          DET-CONTRATO
                          DET-CIDADE
                          DET-ESTADO

                          DET-DOC-ORIG1        DET-DOC-NOVO1
                          DET-DOC-ORIG2        DET-DOC-NOVO2
                          DET-DOC-ORIG3        DET-DOC-NOVO3
                          DET-DOC-ORIG4        DET-DOC-NOVO4
                          DET-DOC-ORIG5        DET-DOC-NOVO5
                          DET-DOC-ORIG6        DET-DOC-NOVO6
                          DET-DOC-ORIG7        DET-DOC-NOVO7
                          DET-DOC-ORIG8        DET-DOC-NOVO8
                          DET-DOC-ORIG9        DET-DOC-NOVO9
                          DET-DOC-ORIG10       DET-DOC-NOVO10
                          DET-DOC-ORIG11       DET-DOC-NOVO11
                          DET-DOC-ORIG12       DET-DOC-NOVO12

                          DET-PARCE-ORIG1      DET-PARCE-NOVO1
                          DET-PARCE-ORIG2      DET-PARCE-NOVO2
                          DET-PARCE-ORIG3      DET-PARCELA-NOVO3
                          DET-PARCE-ORIG4      DET-PARCE-NOVO4
                          DET-PARCE-ORIG5      DET-PARCE-NOVO5
                          DET-PARCE-ORIG6      DET-PARCE-NOVO6
                          DET-PARCE-ORIG7      DET-PARCE-NOVO7
                          DET-PARCE-ORIG8      DET-PARCE-NOVO8
                          DET-PARCE-ORIG9      DET-PARCE-NOVO9
                          DET-PARCE-ORIG10     DET-PARCE-NOVO10
                          DET-PARCE-ORIG11     DET-PARCE-NOVO11
                          DET-PARCE-ORIG12     DET-PARCE-NOVO12

                          DET-TIPO-ORIG1       DET-TIPO-NOVO1
                          DET-TIPO-ORIG2       DET-TIPO-NOVO2
                          DET-TIPO-ORIG3       DET-TIPO-NOVO3
                          DET-TIPO-ORIG4       DET-TIPO-NOVO4
                          DET-TIPO-ORIG5       DET-TIPO-NOVO5
                          DET-TIPO-ORIG6       DET-TIPO-NOVO6
                          DET-TIPO-ORIG7       DET-TIPO-NOVO7
                          DET-TIPO-ORIG8       DET-TIPO-NOVO8
                          DET-TIPO-ORIG9       DET-TIPO-NOVO9
                          DET-TIPO-ORIG10      DET-TIPO-NOVO10
                          DET-TIPO-ORIG11      DET-TIPO-NOVO11
                          DET-TIPO-ORIG12      DET-TIPO-NOVO12

                          DET-BANCO-ORIG1      DET-BANCO-NOVO1
                          DET-BANCO-ORIG2      DET-BANCO-NOVO2
                          DET-BANCO-ORIG3      DET-BANCO-NOVO3
                          DET-BANCO-ORIG4      DET-BANCO-NOVO4
                          DET-BANCO-ORIG5      DET-BANCO-NOVO5
                          DET-BANCO-ORIG6      DET-BANCO-NOVO6
                          DET-BANCO-ORIG7      DET-BANCO-NOVO7
                          DET-BANCO-ORIG8      DET-BANCO-NOVO8
                          DET-BANCO-ORIG9      DET-BANCO-NOVO9
                          DET-BANCO-ORIG10     DET-BANCO-NOVO10
                          DET-BANCO-ORIG11     DET-BANCO-NOVO11
                          DET-BANCO-ORIG12     DET-BANCO-NOVO12

                          DET-VALOR-ORIG1      DET-VALOR-NOVO1
                          DET-VALOR-ORIG2      DET-VALOR-NOVO2
                          DET-VALOR-ORIG3      DET-VALOR-NOVO3
                          DET-VALOR-ORIG4      DET-VALOR-NOVO4
                          DET-VALOR-ORIG5      DET-VALOR-NOVO5
                          DET-VALOR-ORIG6      DET-VALOR-NOVO6
                          DET-VALOR-ORIG7      DET-VALOR-NOVO7
                          DET-VALOR-ORIG8      DET-VALOR-NOVO8
                          DET-VALOR-ORIG9      DET-VALOR-NOVO9
                          DET-VALOR-ORIG10     DET-VALOR-NOVO10
                          DET-VALOR-ORIG11     DET-VALOR-NOVO11
                          DET-VALOR-ORIG12     DET-VALOR-NOVO12

                          DET-VENCTO-ORIG1     DET-VENCTO-NOVO1
                          DET-VENCTO-ORIG2     DET-VENCTO-NOVO2
                          DET-VENCTO-ORIG3     DET-VENCTO-NOVO3
                          DET-VENCTO-ORIG4     DET-VENCTO-NOVO4
                          DET-VENCTO-ORIG5     DET-VENCTO-NOVO5
                          DET-VENCTO-ORIG6     DET-VENCTO-NOVO6
                          DET-VENCTO-ORIG7     DET-VENCTO-NOVO7
                          DET-VENCTO-ORIG8     DET-VENCTO-NOVO8
                          DET-VENCTO-ORIG9     DET-VENCTO-NOVO9
                          DET-VENCTO-ORIG10    DET-VENCTO-NOVO10
                          DET-VENCTO-ORIG11    DET-VENCTO-NOVO11
                          DET-VENCTO-ORIG12    DET-VENCTO-NOVO12

                          DET-OBS-ORIG1        DET-OBS-NOVO1
                          DET-OBS-ORIG2        DET-OBS-NOVO2
                          DET-OBS-ORIG3        DET-OBS-NOVO3
                          DET-OBS-ORIG4        DET-OBS-NOVO4
                          DET-OBS-ORIG5        DET-OBS-NOVO5
                          DET-OBS-ORIG6        DET-OBS-NOVO6
                          DET-OBS-ORIG7        DET-OBS-NOVO7
                          DET-OBS-ORIG8        DET-OBS-NOVO8
                          DET-OBS-ORIG9        DET-OBS-NOVO9
                          DET-OBS-ORIG10       DET-OBS-NOVO10
                          DET-OBS-ORIG11       DET-OBS-NOVO11
                          DET-OBS-ORIG12       DET-OBS-NOVO12

                          DET-DTDEP1           DET-DTDEP2
                          DET-DTDEP3           DET-DTDEP4

                          DET-AGENCIA1         DET-AGENCIA2
                          DET-AGENCIA3         DET-AGENCIA4

                          DET-VLRORIG1         DET-VLRORIG2
                          DET-VLRORIG3         DET-VLRORIG4

                          DET-JUROS1           DET-JUROS2
                          DET-JUROS3           DET-JUROS4

                          DET-DESCONTO1        DET-DESCONTO2
                          DET-DESCONTO3        DET-DESCONTO4

                          DET-VLRTOTAL1        DET-VLRTOTAL2
                          DET-VLRTOTAL3        DET-VLRTOTAL4

                          DET-OBSERVACAO1      DET-OBSERVACAO2
                          DET-OBSERVACAO3      DET-OBSERVACAO4
                          DET-OBSERVACAO5      DET-OBSERVACAO6
                          DET-OBSERVACAO7      DET-OBSERVACAO8
                          DET-OBSERVACAO9      DET-OBSERVACAO10

                          DET-DIA.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE ACD120 ACD121 CGD010 CAD010 CAD018 MTD019
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.


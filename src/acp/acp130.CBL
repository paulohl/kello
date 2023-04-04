       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACP130.
      *AUTORA: ALFREDO SAVIOLLI NETO
      *DATA: 03/11/2004.
      *DESCRIÇÃO: Cadastro de OS
       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY ACPX130.
           COPY CGPX010.
           COPY MTPX019.
           COPY CEAPX010.

           select   ordserv assign       to arquivo-ordserv
                            organization is line sequential
                            access mode  is      sequential
                            file  status is      fs-ordserv.

       DATA DIVISION.
       FILE SECTION.

           COPY ACPW130.
           COPY CGPW010.
           COPY MTPW019.
           COPY CEAPW010.

       fd ordserv.
       01 reg-ordserv                          pic x(300).

       WORKING-STORAGE SECTION.
           COPY "ACP130.CPB".
           COPY "ACP130.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       77  LIN                       PIC 9(02).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-ACD130             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-CEAD010            PIC XX       VALUE SPACES.
           05  FS-ORDSERV            PIC XX       VALUE SPACES.
           05  LNK-DETALHE           PIC X(300).
           05  MASC-QTDE             PIC ZZZZ.
           05  MASC-CONTRATO         PIC 9999/9999.
           05  MASC-NUMERO           PIC ZZ.ZZZ.ZZZ.ZZ9.
           05  MASC-DATA             PIC 99/99/9999.
           05  PROCEDIMENTO-W        PIC 9(01).
           05  DESTINO-W             PIC 9(01).
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


       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.


       01 DATA-AUX.
          05 AUX-DIA                 PIC 9(02).
          05 FILLER                  PIC X(01) VALUE "/".
          05 AUX-MES                 PIC 9(02).
          05 FILLER                  PIC X(01) VALUE "/".
          05 AUX-ANO                 PIC 9(02).

       01 WS-DATA-SYS.
           05 WS-DATA-CPU.
              10 WS-ANO-CPU          PIC 9(04).
              10 WS-MES-CPU          PIC 9(02).
              10 WS-DIA-CPU          PIC 9(02).
           05 FILLER                 PIC X(13).

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).

           COPY "PARAMETR".

           COPY "OS.REL".

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
           MOVE "ACD130" TO ARQ-REC.   MOVE EMPRESA-REF TO PATH-ACD130.
           MOVE "CGD010" TO ARQ-REC.   MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "MTD019" TO ARQ-REC.   MOVE EMPRESA-REF TO PATH-MTD019.
           OPEN I-O ACD130
           OPEN INPUT MTD019 CGD010 CEAD010
           MOVE 1 TO GRAVA-W.
           IF ST-ACD130 = "35"
              CLOSE ACD130      OPEN OUTPUT ACD130
              CLOSE ACD130      OPEN I-O ACD130
           END-IF.
           IF ST-ACD130 <> "00"
              MOVE 1 TO ERRO-W
              STRING "ERRO ABERTURA ACD130: " ST-ACD130 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CGD010 <> "00"
              MOVE 1 TO ERRO-W
              STRING "ERRO ABERTURA CGD010: " ST-CGD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-MTD019 <> "00"
              MOVE 1 TO ERRO-W
              STRING "ERRO ABERTURA MTD019: " ST-MTD019 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.
           IF ST-CEAD010 <> "00"
              MOVE 1 TO ERRO-W
              STRING "ERRO ABERTURA CEAD010: " ST-CEAD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

           IF ERRO-W = 1
              MOVE 1 TO EXIT-FLG.

           IF ERRO-W = ZEROS
               PERFORM LIMPAR-DADOS
               PERFORM PROCURAR-PROXIMO
               PERFORM LOAD-SCREENSET.

       PROCURAR-PROXIMO SECTION.
           INITIALIZE REG-ACD130
           MOVE ALL "9" TO NUMERO-AC130
           START ACD130 KEY IS LESS THEN CHAVE-AC130 INVALID KEY
               MOVE 0 TO ACP-NUMERO
           NOT INVALID KEY
               READ ACD130 NEXT
               IF ST-ACD130 = "00" OR "02"
                  MOVE NUMERO-AC130 TO ACP-NUMERO
               ELSE
                  MOVE 0 TO ACP-NUMERO.

           ADD 1 TO ACP-NUMERO.

           MOVE ACP-NUMERO TO NUMERO-W.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN SAVE-FLG-TRUE
                    PERFORM SALVAR-DADOS
                    PERFORM LIMPAR-DADOS
                    MOVE ACP-NUMERO TO NUMERO-AC130
                    READ ACD130 INVALID KEY
                         PERFORM LIMPAR-DADOS
                         PERFORM PROCURAR-PROXIMO
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
               WHEN "EF-NUMERO"      PERFORM CRITICAR-NUMERO
               WHEN "EF-CONTRATO1"   PERFORM CRITICAR-CONTRATO1
               WHEN "EF-PRODUTO1"    PERFORM CRITICAR-PRODUTO1
               WHEN "EF-QTDE1"       PERFORM CRITICAR-QTDE1
               WHEN "EF-ID1"         PERFORM CRITICAR-ID1
               WHEN "EF-DATA1"       PERFORM CRITICAR-DATA1
               WHEN "EF-DIA1"        PERFORM CRITICAR-DIA1
               WHEN "EF-CONTRATO2"   PERFORM CRITICAR-CONTRATO2
               WHEN "EF-PRODUTO2"    PERFORM CRITICAR-PRODUTO2
               WHEN "EF-QTDE2"       PERFORM CRITICAR-QTDE2
               WHEN "EF-ID2"         PERFORM CRITICAR-ID2
               WHEN "EF-DATA2"       PERFORM CRITICAR-DATA2
               WHEN "EF-DIA2"        PERFORM CRITICAR-DIA2
               WHEN "EF-CONTRATO3"   PERFORM CRITICAR-CONTRATO3
               WHEN "EF-PRODUTO3"    PERFORM CRITICAR-PRODUTO3
               WHEN "EF-QTDE3"       PERFORM CRITICAR-QTDE3
               WHEN "EF-ID3"         PERFORM CRITICAR-ID3
               WHEN "EF-DATA3"       PERFORM CRITICAR-DATA3
               WHEN "EF-DIA3"        PERFORM CRITICAR-DIA3
               WHEN "EF-CONTRATO4"   PERFORM CRITICAR-CONTRATO4
               WHEN "EF-PRODUTO4"    PERFORM CRITICAR-PRODUTO4
               WHEN "EF-QTDE4"       PERFORM CRITICAR-QTDE4
               WHEN "EF-ID4"         PERFORM CRITICAR-ID4
               WHEN "EF-DATA4"       PERFORM CRITICAR-DATA4
               WHEN "EF-DIA4"        PERFORM CRITICAR-DIA4
               WHEN "EF-CONTRATO5"   PERFORM CRITICAR-CONTRATO5
               WHEN "EF-PRODUTO5"    PERFORM CRITICAR-PRODUTO5
               WHEN "EF-QTDE5"       PERFORM CRITICAR-QTDE5
               WHEN "EF-ID5"         PERFORM CRITICAR-ID5
               WHEN "EF-DATA5"       PERFORM CRITICAR-DATA5
               WHEN "EF-DIA5"        PERFORM CRITICAR-DIA5
               WHEN "EF-CONTRATO6"   PERFORM CRITICAR-CONTRATO6
               WHEN "EF-PRODUTO6"    PERFORM CRITICAR-PRODUTO6
               WHEN "EF-QTDE6"       PERFORM CRITICAR-QTDE6
               WHEN "EF-ID6"         PERFORM CRITICAR-ID6
               WHEN "EF-DATA6"       PERFORM CRITICAR-DATA6
               WHEN "EF-DIA6"        PERFORM CRITICAR-DIA6
               WHEN "EF-CONTRATO7"   PERFORM CRITICAR-CONTRATO7
               WHEN "EF-PRODUTO7"    PERFORM CRITICAR-PRODUTO7
               WHEN "EF-QTDE7"       PERFORM CRITICAR-QTDE7
               WHEN "EF-ID7"         PERFORM CRITICAR-ID7
               WHEN "EF-DATA7"       PERFORM CRITICAR-DATA7
               WHEN "EF-DIA7"        PERFORM CRITICAR-DIA7
               WHEN "EF-CONTRATO8"   PERFORM CRITICAR-CONTRATO8
               WHEN "EF-PRODUTO8"    PERFORM CRITICAR-PRODUTO8
               WHEN "EF-QTDE8"       PERFORM CRITICAR-QTDE8
               WHEN "EF-ID8"         PERFORM CRITICAR-ID8
               WHEN "EF-DATA8"       PERFORM CRITICAR-DATA8
               WHEN "EF-DIA8"        PERFORM CRITICAR-DIA8
               WHEN "EF-CONTRATO9"   PERFORM CRITICAR-CONTRATO9
               WHEN "EF-PRODUTO9"    PERFORM CRITICAR-PRODUTO9
               WHEN "EF-QTDE9"       PERFORM CRITICAR-QTDE9
               WHEN "EF-ID9"         PERFORM CRITICAR-ID9
               WHEN "EF-DATA9"       PERFORM CRITICAR-DATA9
               WHEN "EF-DIA9"        PERFORM CRITICAR-DIA9
               WHEN "EF-INFORMACAO1" PERFORM CRITICAR-OBSERVACAO1
               WHEN "EF-INFORMACAO2" PERFORM CRITICAR-OBSERVACAO2
               WHEN "EF-INFORMACAO3" PERFORM CRITICAR-OBSERVACAO3
               WHEN "EF-INFORMACAO4" PERFORM CRITICAR-OBSERVACAO4
               WHEN "EF-DATA-DEST"   PERFORM CRITICAR-DATA-DEST
               WHEN "EF-DATA-RET"    PERFORM CRITICAR-DATA-RET
               WHEN "GRAVAR"         PERFORM CRITICAR-PROCEDIMENTO
                                        THRU CRITICAR-DATA-RET
           END-EVALUATE.

       CRITICAR-PROCEDIMENTO SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-PROCEDIMENTO = 0
                  MOVE "Procedimento Não Informado" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DESTINO SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DESTINO = 0
                  MOVE "Destino Não Informado" TO MENSAGEM
                  MOVE "C" TO TIPO-MSG
                  PERFORM EXIBIR-MENSAGEM.

       CRITICAR-NUMERO SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-NUMERO EQUAL ZEROS
                  MOVE NUMERO-W TO ACP-NUMERO
                  PERFORM SET-UP-FOR-REFRESH-SCREEN
               ELSE
                  MOVE ACP-NUMERO TO NUMERO-AC130
                  READ ACD130 INVALID KEY
                      IF CAMPO-CRITICA = "EF-NUMERO"
                         PERFORM LIMPAR-DADOS
                      END-IF
                      PERFORM PROCURAR-PROXIMO
                      PERFORM SET-UP-FOR-REFRESH-SCREEN
                      MOVE 1 TO GRAVA-W
                  NOT INVALID KEY
                      IF CAMPO-CRITICA = "EF-NUMERO"
                         PERFORM MONTAR-DATA-BLOCK
                         PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-CONTRATO1 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO1 EQUAL ZEROS
                   MOVE "Número do Contrato/Álbum Não Informado" TO
                   MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   MOVE ZERO               TO CLASSIF-CG10
                   MOVE ACP-CONTRATO1      TO CODIGO-CG10
                   READ CGD010 INVALID KEY
                        MOVE "Contrato Não Encontrado" TO MENSAGEM
                        MOVE "C" TO TIPO-MSG
                        PERFORM EXIBIR-MENSAGEM
                        MOVE "********"    TO COMPRADOR-CG10
                   END-READ
                   MOVE COMPRADOR-CG10     TO ACP-NOME1
                   PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-PRODUTO1 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CODPROD1 EQUAL ZEROS
                   PERFORM SUGESTAO-PRODUTO1
               END-IF
               IF ACP-CODPROD1 EQUAL ZEROS
                   MOVE "Produto Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   MOVE ACP-CODPROD1 TO PRODUTO
                   READ CEAD010 INVALID KEY
                       MOVE "Código do Produto Inválido" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                   NOT INVALID KEY
                       MOVE DESC-PROD TO ACP-PRODUTO1
                       PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-QTDE1 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-QTDE1 EQUAL ZEROS
                   MOVE "Quantidade do Produto Não Informada" TO
                   MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   COMPUTE ACP-QTDETOT = ACP-QTDE1 + ACP-QTDE2 +
                   ACP-QTDE3 + ACP-QTDE4 + ACP-QTDE5 + ACP-QTDE6 +
                   ACP-QTDE7 + ACP-QTDE8 + ACP-QTDE9
                   PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-ID1 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-ID1 <> "X" AND SPACES
                   MOVE "Identificação Inválida o X é utilizado para ide
      -            "ntificar" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DATA1 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DATA1 > ZEROS
                   CALL "UTIVLDT" USING ACP-DATA1 WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                       MOVE "1 Data de Atendimento Inválida" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM.


       CRITICAR-DIA1 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DIA1 EQUAL ZEROS
                   MOVE "Quantidade de dias para entrega não Informado"
                   TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-CONTRATO2 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO2 > ZEROS
                   MOVE ZERO               TO CLASSIF-CG10
                   MOVE ACP-CONTRATO2      TO CODIGO-CG10
                   READ CGD010 INVALID KEY
                        MOVE "Contrato Não Encontrado" TO MENSAGEM
                        MOVE "C" TO TIPO-MSG
                        PERFORM EXIBIR-MENSAGEM
                        MOVE "********"    TO COMPRADOR-CG10
                   END-READ
                   MOVE COMPRADOR-CG10     TO ACP-NOME2
                   PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-PRODUTO2 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CODPROD2 EQUAL ZEROS AND ACP-CONTRATO2 > 0
                   PERFORM SUGESTAO-PRODUTO2
               END-IF
               IF ACP-CODPROD2 EQUAL ZEROS AND ACP-CONTRATO2 > 0
                   MOVE "Produto Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   IF ACP-CODPROD2 > 0
                      MOVE ACP-CODPROD2 TO PRODUTO
                      READ CEAD010 INVALID KEY
                           MOVE "Código do Produto Inválido" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                      NOT INVALID KEY
                           MOVE DESC-PROD TO ACP-PRODUTO2
                           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-QTDE2 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO2 > 0
                   IF ACP-QTDE2 = 0
                      MOVE "Quantidade do Produto Não Informada" TO
                      MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                   ELSE
                      COMPUTE ACP-QTDETOT = ACP-QTDE1 + ACP-QTDE2 +
                      ACP-QTDE3 + ACP-QTDE4 + ACP-QTDE5 + ACP-QTDE6 +
                      ACP-QTDE7 + ACP-QTDE8 + ACP-QTDE9
                      PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-ID2 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-ID2 <> "X" AND SPACES
                   MOVE "Identificação Inválida o X é utilizado para ide
      -            "ntificar" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DATA2 SECTION.          
           IF MENSAGEM EQUAL SPACES
               IF ACP-DATA2 > ZEROS
                   CALL "UTIVLDT" USING ACP-DATA2 WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                       MOVE "2Data de Atendimento Inválida" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DIA2 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DIA2 EQUAL ZEROS AND ACP-CONTRATO2 > 0
                   MOVE "Quantidade de dias para entrega não Informado"
                   TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-CONTRATO3 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO3 > ZEROS
                   MOVE ZERO               TO CLASSIF-CG10
                   MOVE ACP-CONTRATO3      TO CODIGO-CG10
                   READ CGD010 INVALID KEY
                        MOVE "Contrato Não Encontrado" TO MENSAGEM
                        MOVE "C" TO TIPO-MSG
                        PERFORM EXIBIR-MENSAGEM
                        MOVE "********"    TO COMPRADOR-CG10
                   END-READ
                   MOVE COMPRADOR-CG10     TO ACP-NOME3
                   PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-PRODUTO3 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CODPROD3 EQUAL ZEROS AND ACP-CONTRATO3 > 0
                   PERFORM SUGESTAO-PRODUTO3
               END-IF
               IF ACP-CODPROD3 EQUAL ZEROS AND ACP-CONTRATO3 > 0
                   MOVE "Produto Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   IF ACP-CODPROD3 > 0
                      MOVE ACP-CODPROD3 TO PRODUTO
                      READ CEAD010 INVALID KEY
                           MOVE "Código do Produto Inválido" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                      NOT INVALID KEY
                           MOVE DESC-PROD TO ACP-PRODUTO3
                           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-QTDE3 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO3 > 0
                   IF ACP-QTDE3 = 0
                      MOVE "Quantidade do Produto Não Informada" TO
                      MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                   ELSE
                      COMPUTE ACP-QTDETOT = ACP-QTDE1 + ACP-QTDE2 +
                      ACP-QTDE3 + ACP-QTDE4 + ACP-QTDE5 + ACP-QTDE6 +
                      ACP-QTDE7 + ACP-QTDE8 + ACP-QTDE9
                      PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-ID3 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-ID3 <> "X" AND SPACES
                   MOVE "Identificação Inválida o X é utilizado para ide
      -            "ntificar" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DATA3 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DATA3 > ZEROS
                   CALL "UTIVLDT" USING ACP-DATA3 WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                       MOVE "3 Data de Atendimento Inválida" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DIA3 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DIA3 EQUAL ZEROS AND ACP-CONTRATO3 > 0
                   MOVE "Quantidade de dias para entrega não Informado"
                   TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-CONTRATO4 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO4 > ZEROS
                   MOVE ZERO               TO CLASSIF-CG10
                   MOVE ACP-CONTRATO4      TO CODIGO-CG10
                   READ CGD010 INVALID KEY
                        MOVE "Contrato Não Encontrado" TO MENSAGEM
                        MOVE "C" TO TIPO-MSG
                        PERFORM EXIBIR-MENSAGEM
                        MOVE "********"    TO COMPRADOR-CG10
                   END-READ
                   MOVE COMPRADOR-CG10     TO ACP-NOME4
                   PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-PRODUTO4 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CODPROD4 EQUAL ZEROS AND ACP-CONTRATO4 > 0
                   PERFORM SUGESTAO-PRODUTO4
               END-IF
               IF ACP-CODPROD4 EQUAL ZEROS AND ACP-CONTRATO4 > 0
                   MOVE "Produto Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   IF ACP-CODPROD4 > 0
                      MOVE ACP-CODPROD4 TO PRODUTO
                      READ CEAD010 INVALID KEY
                           MOVE "Código do Produto Inválido" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                      NOT INVALID KEY
                           MOVE DESC-PROD TO ACP-PRODUTO4
                           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-QTDE4 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO4 > 0
                   IF ACP-QTDE4 = 0
                      MOVE "Quantidade do Produto Não Informada" TO
                      MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                   ELSE
                      COMPUTE ACP-QTDETOT = ACP-QTDE1 + ACP-QTDE2 +
                      ACP-QTDE3 + ACP-QTDE4 + ACP-QTDE5 + ACP-QTDE6 +
                      ACP-QTDE7 + ACP-QTDE8 + ACP-QTDE9
                      PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-ID4 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-ID4 <> "X" AND SPACES
                   MOVE "Identificação Inválida o X é utilizado para ide
      -            "ntificar" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DATA4 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DATA4 > ZEROS
                   CALL "UTIVLDT" USING ACP-DATA4 WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                       MOVE "4 Data de Atendimento Inválida" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DIA4 SECTION.        
           IF MENSAGEM EQUAL SPACES
               IF ACP-DIA4 EQUAL ZEROS AND ACP-CONTRATO4 > 0
                   MOVE "Quantidade de dias para entrega não Informado"
                   TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-CONTRATO5 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO5 > ZEROS
                   MOVE ZERO               TO CLASSIF-CG10
                   MOVE ACP-CONTRATO5      TO CODIGO-CG10
                   READ CGD010 INVALID KEY
                        MOVE "Contrato Não Encontrado" TO MENSAGEM
                        MOVE "C" TO TIPO-MSG
                        PERFORM EXIBIR-MENSAGEM
                        MOVE "********"    TO COMPRADOR-CG10
                   END-READ
                   MOVE COMPRADOR-CG10     TO ACP-NOME5
                   PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-PRODUTO5 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CODPROD5 EQUAL ZEROS AND ACP-CONTRATO5 > 0
                   PERFORM SUGESTAO-PRODUTO5
               END-IF
               IF ACP-CODPROD5 EQUAL ZEROS AND ACP-CONTRATO5 > 0
                   MOVE "Produto Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   IF ACP-CODPROD5 > 0
                      MOVE ACP-CODPROD5 TO PRODUTO
                      READ CEAD010 INVALID KEY
                           MOVE "Código do Produto Inválido" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                      NOT INVALID KEY
                           MOVE DESC-PROD TO ACP-PRODUTO5
                           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-QTDE5 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO5 > 0
                   IF ACP-QTDE5 = 0
                      MOVE "Quantidade do Produto Não Informada" TO
                      MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                   ELSE
                      COMPUTE ACP-QTDETOT = ACP-QTDE1 + ACP-QTDE2 +
                      ACP-QTDE3 + ACP-QTDE4 + ACP-QTDE5 + ACP-QTDE6 +
                      ACP-QTDE7 + ACP-QTDE8 + ACP-QTDE9
                      PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-ID5 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-ID5 <> "X" AND SPACES
                   MOVE "Identificação Inválida o X é utilizado para ide
      -            "ntificar" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DATA5 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DATA5 > ZEROS
                   CALL "UTIVLDT" USING ACP-DATA5 WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                       MOVE "5 Data de Atendimento Inválida" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DIA5 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DIA5 EQUAL ZEROS AND ACP-CONTRATO5 > 0
                   MOVE "Quantidade de dias para entrega não Informado"
                   TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-CONTRATO6 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO6 > ZEROS
                   MOVE ZERO               TO CLASSIF-CG10
                   MOVE ACP-CONTRATO6      TO CODIGO-CG10
                   READ CGD010 INVALID KEY
                        MOVE "Contrato Não Encontrado" TO MENSAGEM
                        MOVE "C" TO TIPO-MSG
                        PERFORM EXIBIR-MENSAGEM
                        MOVE "********"    TO COMPRADOR-CG10
                   END-READ
                   MOVE COMPRADOR-CG10     TO ACP-NOME6
                   PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-PRODUTO6 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CODPROD6 EQUAL ZEROS AND ACP-CONTRATO6 > 0
                   PERFORM SUGESTAO-PRODUTO6
               END-IF
               IF ACP-CODPROD6 EQUAL ZEROS AND ACP-CONTRATO6 > 0
                   MOVE "Produto Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   IF ACP-CODPROD6 > 0
                      MOVE ACP-CODPROD6 TO PRODUTO
                      READ CEAD010 INVALID KEY
                           MOVE "Código do Produto Inválido" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                      NOT INVALID KEY
                           MOVE DESC-PROD TO ACP-PRODUTO6
                           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-QTDE6 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO6 > 0
                   IF ACP-QTDE6 = 0
                      MOVE "Quantidade do Produto Não Informada" TO
                      MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                   ELSE
                      COMPUTE ACP-QTDETOT = ACP-QTDE1 + ACP-QTDE2 +
                      ACP-QTDE3 + ACP-QTDE4 + ACP-QTDE5 + ACP-QTDE6 +
                      ACP-QTDE7 + ACP-QTDE8 + ACP-QTDE9
                      PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-ID6 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-ID6 <> "X" AND SPACES
                   MOVE "Identificação Inválida o X é utilizado para ide
      -            "ntificar" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DATA6 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DATA6 > ZEROS
                   CALL "UTIVLDT" USING ACP-DATA6 WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                       MOVE "6 Data de Atendimento Inválida" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DIA6 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DIA6 EQUAL ZEROS AND ACP-CONTRATO6 > 0
                   MOVE "Quantidade de dias para entrega não Informado"
                   TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-CONTRATO7 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO7 > ZEROS
                   MOVE ZERO               TO CLASSIF-CG10
                   MOVE ACP-CONTRATO7      TO CODIGO-CG10
                   READ CGD010 INVALID KEY
                        MOVE "Contrato Não Encontrado" TO MENSAGEM
                        MOVE "C" TO TIPO-MSG
                        PERFORM EXIBIR-MENSAGEM
                        MOVE "********"    TO COMPRADOR-CG10
                   END-READ
                   MOVE COMPRADOR-CG10     TO ACP-NOME7
                   PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-PRODUTO7 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CODPROD7 EQUAL ZEROS AND ACP-CONTRATO7 > 0
                   PERFORM SUGESTAO-PRODUTO7
               END-IF
               IF ACP-CODPROD7 EQUAL ZEROS AND ACP-CONTRATO7 > 0
                   MOVE "Produto Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   IF ACP-CODPROD7 > 0
                      MOVE ACP-CODPROD7 TO PRODUTO
                      READ CEAD010 INVALID KEY
                           MOVE "Código do Produto Inválido" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                      NOT INVALID KEY
                           MOVE DESC-PROD TO ACP-PRODUTO7
                           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-QTDE7 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO7 > 0
                   IF ACP-QTDE7 = 0
                      MOVE "Quantidade do Produto Não Informada" TO
                      MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                   ELSE
                      COMPUTE ACP-QTDETOT = ACP-QTDE1 + ACP-QTDE2 +
                      ACP-QTDE3 + ACP-QTDE4 + ACP-QTDE5 + ACP-QTDE6 +
                      ACP-QTDE7 + ACP-QTDE8 + ACP-QTDE9
                      PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-ID7 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-ID7 <> "X" AND SPACES
                   MOVE "Identificação Inválida o X é utilizado para ide
      -            "ntificar" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DATA7 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DATA7 > ZEROS
                   CALL "UTIVLDT" USING ACP-DATA7 WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                       MOVE "7 Data de Atendimento Inválida" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DIA7 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DIA7 EQUAL ZEROS AND ACP-CONTRATO7 > 0
                   MOVE "Quantidade de dias para entrega não Informado"
                   TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-CONTRATO8 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO8 > ZEROS
                   MOVE ZERO               TO CLASSIF-CG10
                   MOVE ACP-CONTRATO8      TO CODIGO-CG10
                   READ CGD010 INVALID KEY
                        MOVE "Contrato Não Encontrado" TO MENSAGEM
                        MOVE "C" TO TIPO-MSG
                        PERFORM EXIBIR-MENSAGEM
                        MOVE "********"    TO COMPRADOR-CG10
                   END-READ
                   MOVE COMPRADOR-CG10     TO ACP-NOME8
                   PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-PRODUTO8 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CODPROD8 EQUAL ZEROS AND ACP-CONTRATO8 > 0
                   PERFORM SUGESTAO-PRODUTO8
               END-IF
               IF ACP-CODPROD8 EQUAL ZEROS AND ACP-CONTRATO8 > 0
                   MOVE "Produto Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   IF ACP-CODPROD8 > 0
                      MOVE ACP-CODPROD8 TO PRODUTO
                      READ CEAD010 INVALID KEY
                           MOVE "Código do Produto Inválido" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                      NOT INVALID KEY
                           MOVE DESC-PROD TO ACP-PRODUTO8
                           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-QTDE8 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO8 > 0
                   IF ACP-QTDE8 = 0
                      MOVE "Quantidade do Produto Não Informada" TO
                      MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                   ELSE
                      COMPUTE ACP-QTDETOT = ACP-QTDE1 + ACP-QTDE2 +
                      ACP-QTDE3 + ACP-QTDE4 + ACP-QTDE5 + ACP-QTDE6 +
                      ACP-QTDE7 + ACP-QTDE8 + ACP-QTDE9
                      PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-ID8 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-ID8 <> "X" AND SPACES
                   MOVE "Identificação Inválida o X é utilizado para ide
      -            "ntificar" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DATA8 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DATA8 > ZEROS
                   CALL "UTIVLDT" USING ACP-DATA8 WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                       MOVE "8 Data de Atendimento Inválida" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DIA8 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DIA8 EQUAL ZEROS AND ACP-CONTRATO8 > 0
                   MOVE "Quantidade de dias para entrega não Informado"
                   TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-CONTRATO9 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO9 > ZEROS
                   MOVE ZERO               TO CLASSIF-CG10
                   MOVE ACP-CONTRATO9      TO CODIGO-CG10
                   READ CGD010 INVALID KEY
                        MOVE "Contrato Não Encontrado" TO MENSAGEM
                        MOVE "C" TO TIPO-MSG
                        PERFORM EXIBIR-MENSAGEM
                        MOVE "********"    TO COMPRADOR-CG10
                   END-READ
                   MOVE COMPRADOR-CG10     TO ACP-NOME9
                   PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-PRODUTO9 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CODPROD9 EQUAL ZEROS AND ACP-CONTRATO9 > 0
                   PERFORM SUGESTAO-PRODUTO9
               END-IF
               IF ACP-CODPROD9 EQUAL ZEROS AND ACP-CONTRATO9 > 0
                   MOVE "Produto Não Informado" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM
               ELSE
                   IF ACP-CODPROD9 > 0
                      MOVE ACP-CODPROD9 TO PRODUTO
                      READ CEAD010 INVALID KEY
                           MOVE "Código do Produto Inválido" TO MENSAGEM
                           MOVE "C" TO TIPO-MSG
                           PERFORM EXIBIR-MENSAGEM
                      NOT INVALID KEY
                           MOVE DESC-PROD TO ACP-PRODUTO9
                           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-QTDE9 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-CONTRATO9 > 0
                   IF ACP-QTDE9 = 0
                      MOVE "Quantidade do Produto Não Informada" TO
                      MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                   ELSE
                      COMPUTE ACP-QTDETOT = ACP-QTDE1 + ACP-QTDE2 +
                      ACP-QTDE3 + ACP-QTDE4 + ACP-QTDE5 + ACP-QTDE6 +
                      ACP-QTDE7 + ACP-QTDE8 + ACP-QTDE9
                      PERFORM SET-UP-FOR-REFRESH-SCREEN.

       CRITICAR-ID9 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-ID9 <> "X" AND SPACES
                   MOVE "Identificação Inválida o X é utilizado para ide
      -            "ntificar" TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.


       CRITICAR-DATA9 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DATA9 > ZEROS
                   CALL "UTIVLDT" USING ACP-DATA9 WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                       MOVE "9 Data de Atendimento Inválida" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DIA9 SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DIA9 EQUAL ZEROS AND ACP-CONTRATO9 > 0
                   MOVE "Quantidade de dias para entrega não Informado"
                   TO MENSAGEM
                   MOVE "C" TO TIPO-MSG
                   PERFORM EXIBIR-MENSAGEM.

       CRITICAR-OBSERVACAO1 SECTION.

       CRITICAR-OBSERVACAO2 SECTION.

       CRITICAR-OBSERVACAO3 SECTION.

       CRITICAR-OBSERVACAO4 SECTION.

       CRITICAR-DATA-DEST SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DATA-DESTINO > ZEROS
                   CALL "UTIVLDT" USING ACP-DATA-DESTINO WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                       MOVE "Data de Destino Inválida" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM.

       CRITICAR-DATA-RET SECTION.
           IF MENSAGEM EQUAL SPACES
               IF ACP-DATA-RETORNO > ZEROS
                   CALL "UTIVLDT" USING ACP-DATA-RETORNO WS-OK
                   CANCEL "UTIVLDT"
                   IF WS-OK EQUAL "N"
                       MOVE "Data de Retorno Inválida" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM.
       CRITICAR-DATA-RET-FIM.
           EXIT.

       SUGESTAO-DADOS SECTION.
           EVALUATE CAMPO-CRITICA
               WHEN "EF-CONTRATO1"   PERFORM SUGESTAO-CONTRATO1
               WHEN "EF-CONTRATO2"   PERFORM SUGESTAO-CONTRATO2
               WHEN "EF-CONTRATO3"   PERFORM SUGESTAO-CONTRATO3
               WHEN "EF-CONTRATO4"   PERFORM SUGESTAO-CONTRATO4
               WHEN "EF-CONTRATO5"   PERFORM SUGESTAO-CONTRATO5
               WHEN "EF-CONTRATO6"   PERFORM SUGESTAO-CONTRATO6
               WHEN "EF-CONTRATO7"   PERFORM SUGESTAO-CONTRATO7
               WHEN "EF-CONTRATO8"   PERFORM SUGESTAO-CONTRATO8
               WHEN "EF-CONTRATO9"   PERFORM SUGESTAO-CONTRATO9
               WHEN "EF-PRODUTO1"    PERFORM SUGESTAO-PRODUTO1
               WHEN "EF-PRODUTO2"    PERFORM SUGESTAO-PRODUTO2
               WHEN "EF-PRODUTO3"    PERFORM SUGESTAO-PRODUTO3
               WHEN "EF-PRODUTO4"    PERFORM SUGESTAO-PRODUTO4
               WHEN "EF-PRODUTO5"    PERFORM SUGESTAO-PRODUTO5
               WHEN "EF-PRODUTO6"    PERFORM SUGESTAO-PRODUTO6
               WHEN "EF-PRODUTO7"    PERFORM SUGESTAO-PRODUTO7
               WHEN "EF-PRODUTO8"    PERFORM SUGESTAO-PRODUTO8
               WHEN "EF-PRODUTO9"    PERFORM SUGESTAO-PRODUTO9
               WHEN OTHER    MOVE "Sugestão Inexistente" TO MENSAGEM
                             MOVE "C" TO TIPO-MSG
                             PERFORM EXIBIR-MENSAGEM.

       SUGESTAO-CONTRATO1 SECTION.
           CALL   "CGP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(1:30) TO ACP-NOME1
           MOVE PASSAR-STRING-1(33:8) TO ACP-CONTRATO1.

       SUGESTAO-CONTRATO2 SECTION.
           CALL   "CGP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(1:30) TO ACP-NOME2
           MOVE PASSAR-STRING-1(33:8) TO ACP-CONTRATO2.

       SUGESTAO-CONTRATO3 SECTION.
           CALL   "CGP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(1:30) TO ACP-NOME3
           MOVE PASSAR-STRING-1(33:8) TO ACP-CONTRATO3.

       SUGESTAO-CONTRATO4 SECTION.
           CALL   "CGP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(1:30) TO ACP-NOME4
           MOVE PASSAR-STRING-1(33:8) TO ACP-CONTRATO4.

       SUGESTAO-CONTRATO5 SECTION.
           CALL   "CGP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(1:30) TO ACP-NOME5
           MOVE PASSAR-STRING-1(33:8) TO ACP-CONTRATO5.

       SUGESTAO-CONTRATO6 SECTION.
           CALL   "CGP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(1:30) TO ACP-NOME6
           MOVE PASSAR-STRING-1(33:8) TO ACP-CONTRATO6.

       SUGESTAO-CONTRATO7 SECTION.
           CALL   "CGP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(1:30) TO ACP-NOME7
           MOVE PASSAR-STRING-1(33:8) TO ACP-CONTRATO7.

       SUGESTAO-CONTRATO8 SECTION.
           CALL   "CGP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(1:30) TO ACP-NOME8
           MOVE PASSAR-STRING-1(33:8) TO ACP-CONTRATO8.

       SUGESTAO-CONTRATO9 SECTION.
           CALL   "CGP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP010T".
           MOVE PASSAR-STRING-1(1:30) TO ACP-NOME9
           MOVE PASSAR-STRING-1(33:8) TO ACP-CONTRATO9.

       SUGESTAO-PRODUTO1 SECTION.
           CALL   "CEAP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CEAP010T".
           MOVE PASSAR-STRING-1(7: 40) TO ACP-PRODUTO1
           MOVE PASSAR-STRING-1(1: 5)  TO ACP-CODPROD1.

       SUGESTAO-PRODUTO2 SECTION.
           CALL   "CEAP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CEAP010T".
           MOVE PASSAR-STRING-1(7: 40) TO ACP-PRODUTO2
           MOVE PASSAR-STRING-1(1: 5)  TO ACP-CODPROD2.

       SUGESTAO-PRODUTO3 SECTION.
           CALL   "CEAP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CEAP010T".
           MOVE PASSAR-STRING-1(7: 40) TO ACP-PRODUTO3
           MOVE PASSAR-STRING-1(1: 5)  TO ACP-CODPROD3.

       SUGESTAO-PRODUTO4 SECTION.
           CALL   "CEAP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CEAP010T".
           MOVE PASSAR-STRING-1(7: 40) TO ACP-PRODUTO4
           MOVE PASSAR-STRING-1(1: 5)  TO ACP-CODPROD4.

       SUGESTAO-PRODUTO5 SECTION.
           CALL   "CEAP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CEAP010T".
           MOVE PASSAR-STRING-1(7: 40) TO ACP-PRODUTO5
           MOVE PASSAR-STRING-1(1: 5)  TO ACP-CODPROD5.

       SUGESTAO-PRODUTO6 SECTION.
           CALL   "CEAP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CEAP010T".
           MOVE PASSAR-STRING-1(7: 40) TO ACP-PRODUTO6
           MOVE PASSAR-STRING-1(1: 5)  TO ACP-CODPROD6.

       SUGESTAO-PRODUTO7 SECTION.
           CALL   "CEAP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CEAP010T".
           MOVE PASSAR-STRING-1(7: 40) TO ACP-PRODUTO7
           MOVE PASSAR-STRING-1(1: 5)  TO ACP-CODPROD7.

       SUGESTAO-PRODUTO8 SECTION.
           CALL   "CEAP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CEAP010T".
           MOVE PASSAR-STRING-1(7: 40) TO ACP-PRODUTO8
           MOVE PASSAR-STRING-1(1: 5)  TO ACP-CODPROD8.

       SUGESTAO-PRODUTO9 SECTION.
           CALL   "CEAP010T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CEAP010T".
           MOVE PASSAR-STRING-1(7: 40) TO ACP-PRODUTO9
           MOVE PASSAR-STRING-1(1: 5)  TO ACP-CODPROD9.

       LIMPAR-DADOS SECTION.
           INITIALIZE REG-ACD130
           MOVE ACP-NUMERO  TO NUMERO-W
           MOVE ACP-PROCEDIMENTO TO PROCEDIMENTO-W
           MOVE ACP-DESTINO      TO DESTINO-W
           INITIALIZE DATA-BLOCK
           MOVE NUMERO-W    TO ACP-NUMERO
           MOVE PROCEDIMENTO-W   TO ACP-PROCEDIMENTO
           MOVE DESTINO-W        TO ACP-DESTINO
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
           STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO
               ACP-DATA-ATENDIMENTO
           PERFORM SET-UP-FOR-REFRESH-SCREEN.

       SALVAR-DADOS SECTION.
           MOVE ACP-PROCEDIMENTO        TO PROCEDIMENTO-AC130
           MOVE ACP-DESTINO             TO DESTINO-AC130
           MOVE ACP-NUMERO              TO NUMERO-AC130
           MOVE ACP-CONTRATO1           TO CONTRATO1-AC130
           MOVE ACP-CODPROD1            TO PRODUTO1-AC130
           MOVE ACP-QTDE1               TO QTDE1-AC130
           MOVE ACP-ID1                 TO ID1-AC130
           MOVE ACP-DATA1               TO DATA1-AC130
           MOVE ACP-DIA1                TO DIA1-AC130
           MOVE ACP-CONTRATO2           TO CONTRATO2-AC130
           MOVE ACP-CODPROD2            TO PRODUTO2-AC130
           MOVE ACP-QTDE2               TO QTDE2-AC130
           MOVE ACP-ID2                 TO ID2-AC130
           MOVE ACP-DATA2               TO DATA2-AC130
           MOVE ACP-DIA2                TO DIA2-AC130
           MOVE ACP-CONTRATO3           TO CONTRATO3-AC130
           MOVE ACP-CODPROD3            TO PRODUTO3-AC130
           MOVE ACP-QTDE3               TO QTDE3-AC130
           MOVE ACP-ID3                 TO ID3-AC130
           MOVE ACP-DATA3               TO DATA3-AC130
           MOVE ACP-DIA3                TO DIA3-AC130
           MOVE ACP-CONTRATO4           TO CONTRATO4-AC130
           MOVE ACP-CODPROD4            TO PRODUTO4-AC130
           MOVE ACP-QTDE4               TO QTDE4-AC130
           MOVE ACP-ID4                 TO ID4-AC130
           MOVE ACP-DATA4               TO DATA4-AC130
           MOVE ACP-DIA4                TO DIA4-AC130
           MOVE ACP-CONTRATO5           TO CONTRATO5-AC130
           MOVE ACP-CODPROD5            TO PRODUTO5-AC130
           MOVE ACP-QTDE5               TO QTDE5-AC130
           MOVE ACP-ID5                 TO ID5-AC130
           MOVE ACP-DATA5               TO DATA5-AC130
           MOVE ACP-DIA5                TO DIA5-AC130
           MOVE ACP-CONTRATO6           TO CONTRATO6-AC130
           MOVE ACP-CODPROD6            TO PRODUTO6-AC130
           MOVE ACP-QTDE6               TO QTDE6-AC130
           MOVE ACP-ID6                 TO ID6-AC130
           MOVE ACP-DATA6               TO DATA6-AC130
           MOVE ACP-DIA6                TO DIA6-AC130
           MOVE ACP-CONTRATO7           TO CONTRATO7-AC130
           MOVE ACP-CODPROD7            TO PRODUTO7-AC130
           MOVE ACP-QTDE7               TO QTDE7-AC130
           MOVE ACP-ID7                 TO ID7-AC130
           MOVE ACP-DATA7               TO DATA7-AC130
           MOVE ACP-DIA7                TO DIA7-AC130
           MOVE ACP-CONTRATO8           TO CONTRATO8-AC130
           MOVE ACP-CODPROD8            TO PRODUTO8-AC130
           MOVE ACP-QTDE8               TO QTDE8-AC130
           MOVE ACP-ID8                 TO ID8-AC130
           MOVE ACP-DATA8               TO DATA8-AC130
           MOVE ACP-DIA8                TO DIA8-AC130
           MOVE ACP-CONTRATO9           TO CONTRATO9-AC130
           MOVE ACP-CODPROD9            TO PRODUTO9-AC130
           MOVE ACP-QTDE9               TO QTDE9-AC130
           MOVE ACP-ID9                 TO ID9-AC130
           MOVE ACP-DATA9               TO DATA9-AC130
           MOVE ACP-DIA9                TO DIA9-AC130
           MOVE ACP-QTDETOT             TO QTDETOT-AC130
           MOVE ACP-OBSERVACAO1         TO OBSERVACAO1-AC130
           MOVE ACP-OBSERVACAO2         TO OBSERVACAO2-AC130
           MOVE ACP-OBSERVACAO3         TO OBSERVACAO3-AC130
           MOVE ACP-OBSERVACAO4         TO OBSERVACAO4-AC130
           IF ACP-DATA-ATENDIMENTO = 0
               MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
               STRING WS-DIA-CPU WS-MES-CPU WS-ANO-CPU INTO
               DATA-KAC-ATENDIMENTO-AC130
           END-IF
           MOVE ACP-DATA-DESTINO        TO DATA-DESTINO-AC130
           MOVE ACP-DATA-RETORNO        TO DATA-KAC-RETORNO-AC130

           IF GRAVA-W = 1
              WRITE REG-ACD130 INVALID KEY
                   PERFORM ERRO-GRAVACAO
           ELSE REWRITE REG-ACD130 INVALID KEY
                PERFORM ERRO-GRAVACAO
           END-IF.

       ERRO-GRAVACAO SECTION.
           STRING "ERRO GRAVAÇÃO: " ST-ACD130 INTO MENSAGEM
           MOVE "C" TO TIPO-MSG
           PERFORM EXIBIR-MENSAGEM
           PERFORM LOAD-SCREENSET.

       CLEAR-FLAGS SECTION.
           INITIALIZE FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "ACP130" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move    1 to flag-critica
           move spaces to mensagem.


       MONTAR-DATA-BLOCK SECTION.
           perform 520-abrir-impressora

           MOVE 2                                    TO GRAVA-W

           MOVE PROCEDIMENTO-AC130                   TO ACP-PROCEDIMENTO
           EVALUATE ACP-PROCEDIMENTO
               WHEN 1 MOVE "PROCEDIMENTO-SOLICITACAO" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      MOVE "X"                   TO DET1-SOLICITACAO
                                                    DET2-SOLICITACAO
                      MOVE SPACES                TO DET1-REPASSE
                                                    DET2-REPASSE

               WHEN 2 MOVE "PROCEDIMENTO-REPASSE"     TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM

                      MOVE SPACES                TO DET1-SOLICITACAO
                                                    DET2-SOLICITACAO
                      MOVE "X"                   TO DET1-REPASSE
                                                    DET2-REPASSE

           END-EVALUATE

           MOVE DESTINO-AC130                    TO ACP-DESTINO
           EVALUATE ACP-DESTINO
               WHEN 1 MOVE "DESTINO-LABORATORIO" TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      MOVE "LABORATÓRIO"         TO DET1-DESTINATARIO
                                                    DET2-DESTINO
               WHEN 2 MOVE "DESTINO-COMERCIAL"   TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      MOVE "COMERCIAL"           TO DET1-DESTINATARIO
                                                    DET2-DESTINO
               WHEN 3 MOVE "DESTINO-VIDEO"       TO DS-PROCEDURE
                      PERFORM CALL-DIALOG-SYSTEM
                      MOVE "VÍDEO"               TO DET1-DESTINATARIO
                                                    DET2-DESTINO
           END-EVALUATE

           MOVE NUMERO-AC130                 TO ACP-NUMERO
                                                MASC-NUMERO

           MOVE MASC-NUMERO                  TO DET1-NUMERO
                                                DET2-NUMERO

      * DADOS DO 1º CONTRATO

           MOVE CONTRATO1-AC130              TO ACP-CONTRATO1
           MOVE ZERO                         TO CLASSIF-CG10
           MOVE ACP-CONTRATO1                TO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE SPACES                  TO ACP-NOME1
           NOT INVALID KEY
                MOVE COMPRADOR-CG10          TO ACP-NOME1
           END-READ

           MOVE ACP-NOME1                    TO DET1-NOME1
                                                DET2-NOME1
           IF CONTRATO1-AC130 > 0
              MOVE CONTRATO1-AC130           TO MASC-CONTRATO
              MOVE MASC-CONTRATO             TO DET1-CONTRATO1
                                                DET2-CONTRATO1
           ELSE
              MOVE SPACES                    TO DET1-CONTRATO1
                                                DET2-CONTRATO1
           END-IF

           MOVE ID1-AC130                    TO ACP-ID1
           MOVE ACP-ID1                      TO DET1-IDENTIFICADO1
                                                DET2-IDENTIFICADO1

           MOVE PRODUTO1-AC130               TO ACP-CODPROD1
           MOVE ACP-CODPROD1                 TO PRODUTO
           READ CEAD010 INVALID KEY
                MOVE SPACES                  TO ACP-PRODUTO1
           NOT INVALID KEY
                MOVE DESC-PROD TO ACP-PRODUTO1
           END-READ
           MOVE ACP-PRODUTO1                 TO DET1-PRODUTO1
                                                DET2-PRODUTO1

           MOVE QTDE1-AC130                  TO ACP-QTDE1 MASC-QTDE
           MOVE MASC-QTDE                    TO DET1-QTDE1
                                                DET2-QTDE1

           MOVE DATA1-AC130                  TO ACP-DATA1
           IF DATA1-AC130 > 0
              STRING DATA1-AC130(1:2) INTO AUX-DIA
              STRING DATA1-AC130(3:4) INTO AUX-MES
              STRING DATA1-AC130(7:8) INTO AUX-ANO
              MOVE DATA-AUX                  TO DET1-DATA1
                                                DET2-DATA1
           ELSE
              MOVE SPACES                    TO DET1-DATA1
                                                DET2-DATA1
           END-IF

           MOVE DIA1-AC130                   TO ACP-DIA1

      * DADOS DO 2º CONTRATO

           MOVE CONTRATO2-AC130              TO ACP-CONTRATO2
           MOVE ZERO                         TO CLASSIF-CG10
           MOVE ACP-CONTRATO2                TO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE SPACES                  TO ACP-NOME2
           NOT INVALID KEY
                MOVE COMPRADOR-CG10          TO ACP-NOME2
           END-READ

           MOVE ACP-NOME2                    TO DET1-NOME2
                                                DET2-NOME2

           IF CONTRATO2-AC130 > 0
              MOVE CONTRATO2-AC130           TO MASC-CONTRATO
              MOVE MASC-CONTRATO             TO DET1-CONTRATO2
                                                DET2-CONTRATO2
           ELSE
              MOVE SPACES                    TO DET1-CONTRATO2
                                                DET2-CONTRATO2
           END-IF

           MOVE ID2-AC130                    TO ACP-ID2
           MOVE ACP-ID2                      TO DET1-IDENTIFICADO2
                                                DET2-IDENTIFICADO2

           MOVE PRODUTO2-AC130               TO ACP-CODPROD2
           MOVE ACP-CODPROD2                 TO PRODUTO
           READ CEAD010 INVALID KEY
                MOVE SPACES                  TO ACP-PRODUTO2
           NOT INVALID KEY
                MOVE DESC-PROD TO ACP-PRODUTO2
           END-READ
           MOVE ACP-PRODUTO2                 TO DET1-PRODUTO2
                                                DET2-PRODUTO2

           MOVE QTDE2-AC130                  TO ACP-QTDE2 MASC-QTDE
           MOVE MASC-QTDE                    TO DET1-QTDE2
                                                DET2-QTDE2

           MOVE DATA2-AC130                  TO ACP-DATA2
           IF DATA2-AC130 > 0
              STRING DATA2-AC130(1:2) INTO AUX-DIA
              STRING DATA2-AC130(3:4) INTO AUX-MES
              STRING DATA2-AC130(7:8) INTO AUX-ANO
              MOVE DATA-AUX                  TO DET1-DATA2
                                                DET2-DATA2
           ELSE
              MOVE SPACES                    TO DET1-DATA2
                                                DET2-DATA2
           END-IF

           MOVE DIA2-AC130                   TO ACP-DIA2

      * DADOS DO 3º CONTRATO

           MOVE CONTRATO3-AC130              TO ACP-CONTRATO3
           MOVE ZERO                         TO CLASSIF-CG10
           MOVE ACP-CONTRATO3                TO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE SPACES                  TO ACP-NOME3
           NOT INVALID KEY
                MOVE COMPRADOR-CG10          TO ACP-NOME3
           END-READ

           MOVE ACP-NOME3                    TO DET1-NOME3
                                                DET2-NOME3

           IF CONTRATO3-AC130 > 0
              MOVE CONTRATO3-AC130           TO MASC-CONTRATO
              MOVE MASC-CONTRATO             TO DET1-CONTRATO3
                                                DET2-CONTRATO3
           ELSE
              MOVE SPACES                    TO DET1-CONTRATO3
                                                DET2-CONTRATO3
           END-IF

           MOVE ID3-AC130                    TO ACP-ID3
           MOVE ACP-ID3                      TO DET1-IDENTIFICADO3
                                                DET2-IDENTIFICADO3



           MOVE PRODUTO3-AC130               TO ACP-CODPROD3
           MOVE ACP-CODPROD3                 TO PRODUTO
           READ CEAD010 INVALID KEY
                MOVE SPACES                  TO ACP-PRODUTO3
           NOT INVALID KEY
                MOVE DESC-PROD TO ACP-PRODUTO3
           END-READ
           MOVE ACP-PRODUTO3                 TO DET1-PRODUTO3
                                                DET2-PRODUTO3

           MOVE QTDE3-AC130                  TO ACP-QTDE3 MASC-QTDE
           MOVE MASC-QTDE                    TO DET1-QTDE3
                                                DET2-QTDE3

           MOVE DATA3-AC130                  TO ACP-DATA3
           IF DATA3-AC130 > 0
              STRING DATA3-AC130(1:2) INTO AUX-DIA
              STRING DATA3-AC130(3:4) INTO AUX-MES
              STRING DATA3-AC130(7:8) INTO AUX-ANO
              MOVE DATA-AUX                  TO DET1-DATA3
                                                DET2-DATA3
           ELSE
              MOVE SPACES                    TO DET1-DATA3
                                                DET2-DATA3
           END-IF

           MOVE DIA3-AC130                   TO ACP-DIA3

      * DADOS DO 4º CONTRATO

           MOVE CONTRATO4-AC130              TO ACP-CONTRATO4
           MOVE ZERO                         TO CLASSIF-CG10
           MOVE ACP-CONTRATO4                TO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE SPACES                  TO ACP-NOME4
           NOT INVALID KEY
                MOVE COMPRADOR-CG10          TO ACP-NOME4
           END-READ

           MOVE ACP-NOME4                    TO DET1-NOME4
                                                DET2-NOME4

           IF CONTRATO4-AC130 > 0
              MOVE CONTRATO4-AC130           TO MASC-CONTRATO
              MOVE MASC-CONTRATO             TO DET1-CONTRATO4
                                                DET2-CONTRATO4
           ELSE
              MOVE SPACES                    TO DET1-CONTRATO4
                                                DET2-CONTRATO4
           END-IF

           MOVE ID4-AC130                    TO ACP-ID4
           MOVE ACP-ID4                      TO DET1-IDENTIFICADO4
                                                DET2-IDENTIFICADO4


           MOVE PRODUTO4-AC130               TO ACP-CODPROD4
           MOVE ACP-CODPROD4                 TO PRODUTO
           READ CEAD010 INVALID KEY
                MOVE SPACES                  TO ACP-PRODUTO4
           NOT INVALID KEY
                MOVE DESC-PROD TO ACP-PRODUTO4
           END-READ
           MOVE ACP-PRODUTO4                 TO DET1-PRODUTO4
                                                DET2-PRODUTO4

           MOVE QTDE4-AC130                  TO ACP-QTDE4 MASC-QTDE
           MOVE MASC-QTDE                    TO DET1-QTDE4
                                                DET2-QTDE4

           MOVE DATA4-AC130                  TO ACP-DATA4
           IF DATA4-AC130 > 0
              STRING DATA4-AC130(1:2) INTO AUX-DIA
              STRING DATA4-AC130(3:4) INTO AUX-MES
              STRING DATA4-AC130(7:8) INTO AUX-ANO
              MOVE DATA-AUX                  TO DET1-DATA4
                                                DET2-DATA4
           ELSE
              MOVE SPACES                    TO DET1-DATA4
                                                DET2-DATA4
           END-IF

           MOVE DIA4-AC130                   TO ACP-DIA4

      * DADOS DO 5º CONTRATO

           MOVE CONTRATO5-AC130              TO ACP-CONTRATO5
           MOVE ZERO                         TO CLASSIF-CG10
           MOVE ACP-CONTRATO5                TO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE SPACES                  TO ACP-NOME5
           NOT INVALID KEY
                MOVE COMPRADOR-CG10          TO ACP-NOME5
           END-READ

           MOVE ACP-NOME5                    TO DET1-NOME5
                                                DET2-NOME5

           IF CONTRATO5-AC130 > 0
              MOVE CONTRATO5-AC130           TO MASC-CONTRATO
              MOVE MASC-CONTRATO             TO DET1-CONTRATO5
                                                DET2-CONTRATO5
           ELSE
              MOVE SPACES                    TO DET1-CONTRATO5
                                                DET2-CONTRATO5
           END-IF

           MOVE ID5-AC130                    TO ACP-ID5
           MOVE ACP-ID5                      TO DET1-IDENTIFICADO5
                                                DET2-IDENTIFICADO5


           MOVE PRODUTO5-AC130               TO ACP-CODPROD5
           MOVE ACP-CODPROD5                 TO PRODUTO
           READ CEAD010 INVALID KEY
                MOVE SPACES                  TO ACP-PRODUTO5
           NOT INVALID KEY
                MOVE DESC-PROD TO ACP-PRODUTO5
           END-READ
           MOVE ACP-PRODUTO5                 TO DET1-PRODUTO5
                                                DET2-PRODUTO5

           MOVE QTDE5-AC130                  TO ACP-QTDE5 MASC-QTDE
           MOVE MASC-QTDE                    TO DET1-QTDE5
                                                DET2-QTDE5

           MOVE DATA5-AC130                  TO ACP-DATA5
           IF DATA5-AC130 > 0
              STRING DATA5-AC130(1:2) INTO AUX-DIA
              STRING DATA5-AC130(3:4) INTO AUX-MES
              STRING DATA5-AC130(7:8) INTO AUX-ANO
              MOVE DATA-AUX                  TO DET1-DATA5
                                                DET2-DATA5
           ELSE
              MOVE SPACES                    TO DET1-DATA5
                                                DET2-DATA5
           END-IF

           MOVE DATA5-AC130                  TO ACP-DIA5

      * DADOS DO 6º CONTRATO

           MOVE CONTRATO6-AC130              TO ACP-CONTRATO6
           MOVE ZERO                         TO CLASSIF-CG10
           MOVE ACP-CONTRATO6                TO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE SPACES                  TO ACP-NOME6
           NOT INVALID KEY
                MOVE COMPRADOR-CG10          TO ACP-NOME6
           END-READ

           MOVE ACP-NOME6                    TO DET1-NOME6
                                                DET2-NOME6
           IF CONTRATO6-AC130 > 0
              MOVE CONTRATO6-AC130           TO MASC-CONTRATO
              MOVE MASC-CONTRATO             TO DET1-CONTRATO6
                                                DET2-CONTRATO6
           ELSE
              MOVE SPACES                    TO DET1-CONTRATO6
                                                DET2-CONTRATO6
           END-IF

           MOVE ID6-AC130                    TO ACP-ID6
           MOVE ACP-ID6                      TO DET1-IDENTIFICADO6
                                                DET2-IDENTIFICADO6


           MOVE PRODUTO6-AC130               TO ACP-CODPROD6
           MOVE ACP-CODPROD6                 TO PRODUTO
           READ CEAD010 INVALID KEY
                MOVE SPACES                  TO ACP-PRODUTO6
           NOT INVALID KEY
                MOVE DESC-PROD TO ACP-PRODUTO6
           END-READ
           MOVE ACP-PRODUTO6                 TO DET1-PRODUTO6
                                                DET2-PRODUTO6

           MOVE QTDE6-AC130                  TO ACP-QTDE6 MASC-QTDE
           MOVE MASC-QTDE                    TO DET1-QTDE6
                                                DET2-QTDE6

           MOVE DATA6-AC130                  TO ACP-DATA6
           IF DATA6-AC130 > 0
              STRING DATA6-AC130(1:2) INTO AUX-DIA
              STRING DATA6-AC130(3:4) INTO AUX-MES
              STRING DATA6-AC130(7:8) INTO AUX-ANO
              MOVE DATA-AUX                  TO DET1-DATA6
                                                DET2-DATA6
           ELSE
              MOVE SPACES                    TO DET1-DATA6
                                                DET2-DATA6
           END-IF

           MOVE DIA6-AC130                   TO ACP-DIA6

      * DADOS DO 7º CONTRATO

           MOVE CONTRATO7-AC130              TO ACP-CONTRATO7
           MOVE ZERO                         TO CLASSIF-CG10
           MOVE ACP-CONTRATO7                TO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE SPACES                  TO ACP-NOME7
           NOT INVALID KEY
                MOVE COMPRADOR-CG10          TO ACP-NOME7
           END-READ

           MOVE ACP-NOME7                    TO DET1-NOME7
                                                DET2-NOME7
           IF CONTRATO7-AC130 > 0
              MOVE CONTRATO7-AC130           TO MASC-CONTRATO
              MOVE MASC-CONTRATO             TO DET1-CONTRATO7
                                                DET2-CONTRATO7
           ELSE
              MOVE SPACES                    TO DET1-CONTRATO7
                                                DET2-CONTRATO7
           END-IF

           MOVE ID7-AC130                    TO ACP-ID7
           MOVE ACP-ID7                      TO DET1-IDENTIFICADO7
                                                DET2-IDENTIFICADO7


           MOVE PRODUTO7-AC130               TO ACP-CODPROD7
           MOVE ACP-CODPROD7                 TO PRODUTO
           READ CEAD010 INVALID KEY
                MOVE SPACES                  TO ACP-PRODUTO7
           NOT INVALID KEY
                MOVE DESC-PROD TO ACP-PRODUTO7
           END-READ
           MOVE ACP-PRODUTO7                 TO DET1-PRODUTO7
                                                DET2-PRODUTO7

           MOVE QTDE7-AC130                  TO ACP-QTDE7 MASC-QTDE
           MOVE MASC-QTDE                    TO DET1-QTDE7
                                                DET2-QTDE7

           MOVE DATA7-AC130                  TO ACP-DATA7
           IF DATA7-AC130 > 0
              STRING DATA7-AC130(1:2) INTO AUX-DIA
              STRING DATA7-AC130(3:4) INTO AUX-MES
              STRING DATA7-AC130(7:8) INTO AUX-ANO
              MOVE DATA-AUX                  TO DET1-DATA7
                                                DET2-DATA7
           ELSE
              MOVE SPACES                    TO DET1-DATA7
                                                DET2-DATA7
           END-IF

           MOVE DIA7-AC130                   TO ACP-DIA7

      * DADOS DO 8º CONTRATO

           MOVE CONTRATO8-AC130              TO ACP-CONTRATO8
           MOVE ZERO                         TO CLASSIF-CG10
           MOVE ACP-CONTRATO8                TO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE SPACES                  TO ACP-NOME8
           NOT INVALID KEY
                MOVE COMPRADOR-CG10          TO ACP-NOME8
           END-READ

           MOVE ACP-NOME8                    TO DET1-NOME8
                                                DET2-NOME8
           IF CONTRATO8-AC130 > 0
              MOVE CONTRATO8-AC130           TO MASC-CONTRATO
              MOVE MASC-CONTRATO             TO DET1-CONTRATO8
                                                DET2-CONTRATO8
           ELSE
              MOVE SPACES                    TO DET1-CONTRATO8
                                                DET2-CONTRATO8
           END-IF

           MOVE ID8-AC130                    TO ACP-ID8
           MOVE ACP-ID8                      TO DET1-IDENTIFICADO8
                                                DET2-IDENTIFICADO8


           MOVE PRODUTO8-AC130               TO ACP-CODPROD8
           MOVE ACP-CODPROD8                 TO PRODUTO
           READ CEAD010 INVALID KEY
                MOVE SPACES                  TO ACP-PRODUTO8
           NOT INVALID KEY
                MOVE DESC-PROD TO ACP-PRODUTO8
           END-READ
           MOVE ACP-PRODUTO8                 TO DET1-PRODUTO8
                                                DET2-PRODUTO8

           MOVE QTDE8-AC130                  TO ACP-QTDE8 MASC-QTDE
           MOVE MASC-QTDE                    TO DET1-QTDE8
                                                DET2-QTDE8

           MOVE DATA8-AC130                  TO ACP-DATA8
           IF DATA8-AC130 > 0
              STRING DATA8-AC130(1:2) INTO AUX-DIA
              STRING DATA8-AC130(3:4) INTO AUX-MES
              STRING DATA8-AC130(7:8) INTO AUX-ANO
              MOVE DATA-AUX                  TO DET1-DATA8
                                                DET2-DATA8
           ELSE
              MOVE SPACES                    TO DET1-DATA8
                                                DET2-DATA8
           END-IF

           MOVE DIA8-AC130                   TO ACP-DIA8

      * DADOS DO 9º CONTRATO

           MOVE CONTRATO9-AC130              TO ACP-CONTRATO9
           MOVE ZERO                         TO CLASSIF-CG10
           MOVE ACP-CONTRATO9                TO CODIGO-CG10
           READ CGD010 INVALID KEY
                MOVE SPACES                  TO ACP-NOME9
           NOT INVALID KEY
                MOVE COMPRADOR-CG10          TO ACP-NOME9
           END-READ

           MOVE ACP-NOME9                    TO DET1-NOME9
                                                DET2-NOME9
           IF CONTRATO9-AC130 > 0
              MOVE CONTRATO9-AC130           TO MASC-CONTRATO
              MOVE MASC-CONTRATO             TO DET1-CONTRATO9
                                                DET2-CONTRATO9
           ELSE
              MOVE SPACES                    TO DET1-CONTRATO9
                                                DET2-CONTRATO9
           END-IF

           MOVE ID9-AC130                    TO ACP-ID9
           MOVE ACP-ID9                      TO DET1-IDENTIFICADO9
                                                DET2-IDENTIFICADO9

           MOVE PRODUTO9-AC130               TO ACP-CODPROD9
           MOVE ACP-CODPROD9                 TO PRODUTO
           READ CEAD010 INVALID KEY
                MOVE SPACES                  TO ACP-PRODUTO9
           NOT INVALID KEY
                MOVE DESC-PROD TO ACP-PRODUTO9
           END-READ
           MOVE ACP-PRODUTO9                 TO DET1-PRODUTO9
                                                DET2-PRODUTO9

           MOVE QTDE9-AC130                  TO ACP-QTDE9 MASC-QTDE
           MOVE MASC-QTDE                    TO DET1-QTDE9
                                                DET2-QTDE9

           MOVE DATA9-AC130                  TO ACP-DATA9
           IF DATA9-AC130 > 0
              STRING DATA9-AC130(1:2) INTO AUX-DIA
              STRING DATA9-AC130(3:4) INTO AUX-MES
              STRING DATA9-AC130(7:8) INTO AUX-ANO
              MOVE DATA-AUX                  TO DET1-DATA9
                                                DET2-DATA9
           ELSE
              MOVE SPACES                    TO DET1-DATA9
                                                DET2-DATA9
           END-IF


           MOVE DIA9-AC130                   TO ACP-DIA9


           MOVE QTDETOT-AC130                TO ACP-QTDETOT
           MOVE ACP-QTDETOT                  TO MASC-QTDE
           MOVE MASC-QTDE                    TO DET1-QTDTOTAL
                                                DET2-QTDETOTAL

           MOVE OBSERVACAO1-AC130            TO ACP-OBSERVACAO1
                                                DET1-OBSERVACAO1
                                                DET2-OBSERVACAO1

           MOVE OBSERVACAO2-AC130            TO ACP-OBSERVACAO2
                                                DET1-OBSERVACAO2
                                                DET2-OBSERVACAO2

           MOVE OBSERVACAO3-AC130            TO ACP-OBSERVACAO3
                                                DET1-OBSERVACAO3
                                                DET2-OBSERVACAO3

           MOVE OBSERVACAO4-AC130            TO ACP-OBSERVACAO4
                                                DET1-OBSERVACAO4
                                                DET2-OBSERVACAO4

           MOVE DATA-KAC-ATENDIMENTO-AC130   TO ACP-DATA-ATENDIMENTO

           IF DATA-KAC-ATENDIMENTO-AC130 > 0
              MOVE DATA-KAC-ATENDIMENTO-AC130 TO MASC-DATA
              MOVE MASC-DATA                  TO DET1-KAC-ENTRADA
                                                 DET2-KAC-ENTRADA
           ELSE
              MOVE SPACES                     TO DET1-KAC-ENTRADA
                                                 DET2-KAC-ENTRADA
           END-IF

           MOVE DATA-DESTINO-AC130           TO ACP-DATA-DESTINO

           IF DATA-DESTINO-AC130 > 0
              MOVE DATA-DESTINO-AC130        TO MASC-DATA
              MOVE MASC-DATA                 TO DET1-DEST-ENTRADA
                                                DET2-DEST-ENTRADA
           ELSE
              MOVE SPACES                    TO DET1-DEST-ENTRADA
                                                DET2-DEST-ENTRADA
           END-IF

           MOVE DATA-KAC-RETORNO-AC130       TO ACP-DATA-RETORNO

           IF DATA-KAC-RETORNO-AC130 > 0
               MOVE DATA-KAC-RETORNO-AC130   TO MASC-DATA
               MOVE MASC-DATA                TO DET1-KAC-RETORNO
                                                DET2-KAC-RETORNO
           ELSE
               MOVE SPACES                   TO DET1-KAC-RETORNO
                                                DET2-KAC-RETORNO
           END-IF

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
           MOVE DET-398                    TO LNK-DETALHE
           PERFORM 590-IMPRIMIR-LINHA
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

           perform 530-fechar-impressora.

           move "Visualizar a Ordem de Serviço ?" to mensagem
           move "Q" to tipo-msg
           perform exibir-mensagem

           if resp-msg equal "S"
              STRING "C:\Arquiv~1\Intern~1\IEXPLORE.EXE "
                     "file:///C:\ORDSERV.HTM"
                     INTO ARQ-EXPLORER
              move "CHAMAR-EXPLORER" to ds-procedure
              perform call-dialog-system.

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

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
             GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE ACD130 MTD019 CGD010 CEAD010.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.


       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BXREC.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 16/03/2015.
      *FUNÇÃO: BAIXA EM LOTE

       ENVIRONMENT DIVISION.
       class-control.
           AListview          is class "alistview"
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CAPX004.
           COPY CAPX030.
           COPY COPX040.
           COPY CGPX001.
           COPY MTPX002.
           COPY MTPX019.
           COPY MTPX020.
           COPY MTPX023.
           COPY RCPX100.
           COPY RCPX101.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW004.
       COPY CAPW030.
       COPY COPW040.
       COPY CGPW001.
       COPY MTPW002.
       COPY MTPW019.
       COPY MTPW020.
       COPY MTPW023.
       COPY RCPW100.
       COPY RCPW101.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "BXREC.CPB".
           COPY "BXREC.CPY".
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
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CAD030             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-MTD002             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ST-MTD023             PIC XX       VALUE SPACES.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
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
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  POSSE-W               PIC 9        VALUE ZEROS.
           05  IDENTIFICACAO-W       PIC X(30)    VALUE SPACES.
           05  DATA-ROMANEIO-W       PIC 9(08)    VALUE ZEROS.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  TIPO-FOGO             PIC 9        VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-W            PIC 9(8)     VALUE ZEROS.
           05  ULT-SEQ               PIC 9(4)     VALUE ZEROS.
           05  MENSAGEM              PIC X(200)   VALUE SPACES.
           05  TIPO-MSG              PIC X(01)    VALUE SPACES.
           05  RESP-MSG              PIC X(01)    VALUE SPACES.
           05  AUX-DATA              PIC 9(08)    VALUE ZEROS.
           COPY "PARAMETR".


       77 wsTexto                      pic x(255) value spaces.
       77 wsCheckEnable                pic 99     comp-5 value 0.
       77 wsItem                       pic 9(009) comp-5 value zeros.
       77 UMITEM                       object reference.
       77 UMOBJETO                     object reference.

       01 WSCOLUNACONTRATO             pic 9(009) comp-5 value zeros.
       01 WSCOLUNAALBUM                pic 9(009) comp-5 value zeros.

       01 lnktabelaPro.
          02 lnkobjetoscolPro          object reference occurs 99 times.
       01 lnktabelaColPro.
          02 lnkcolunasPro pic 9(09) comp-5 value zeros occurs 99 times.
       01 indice                   pic 9(02).
       01 wssize                   pic 9(09) comp-5 value zeros.
       01 wsIndice                 pic 9(09) comp-5 value zeros.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 QTDE-BAIXADA             PIC 9(09) VALUE ZEROS.
       01 AUX-VALOR                PIC 9(09)V99 VALUE ZEROS.

       01 ws-data-sys.
          05 ws-data-cpu               pic 9(08).
          05 filler redefines ws-data-cpu.
             10 ws-ano-cpu             pic 9(04).
             10 ws-mes-cpu             pic 9(02).
             10 ws-dia-cpu             pic 9(02).
          05 filler                    pic x(13).

       01 ws-ok                        pic x(01).

       01 det-01.
          05 det-vencimento            pic 99/99/9999.
          05 filler                    pic x(05).
          05 det-valor                 pic zzz.zz9,99.
          05 filler                    pic x(04).
          05 det-nr-docto              pic zzz.zz9.
          05 filler                    pic x(03).
          05 det-banco                 pic zz9.
          05 filler                    pic x(03).
          05 det-parcela               pic z9.
          05 filler                    pic x(03) value " / ".
          05 det-qtparce               pic z9.
          05 filler                    pic x(02).
          05 det-tipo                  pic x(20).



       01 lnkusu.
          copy usuario.cpy.

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           ACCEPT DATA-DIA-I FROM DATE.
           MOVE 20             TO DATA-DIA-I(1: 2)
           MOVE DATA-DIA-I     TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV       TO DATA-DIA-W.

           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           move "\PROGRAMA\KELLO\*" to lnk-path-sis
           move empresa-w           to lnk-empresa
           move USUARIO-W           to lnk-usuario
           MOVE "CAD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CAD030" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD030.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "MTD002" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD002.
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD020" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "MTD023" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD023.
           MOVE "RCD100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100.
           MOVE "RCD101" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD101.
           OPEN I-O   MTD023 MTD020 MTD019 RCD100 RCD101
           CLOSE      RCD100 RCD101
           OPEN INPUT COD040 CAD010 CGD001 MTD002 CAD004 RCD100 RCD101
                      CAD030.
           IF ST-MTD019 = "35"
              CLOSE MTD019      OPEN OUTPUT MTD019
              CLOSE MTD019      OPEN I-O MTD019
           END-IF.
           IF ST-MTD020 = "35"
              CLOSE MTD020      OPEN OUTPUT MTD020
              CLOSE MTD020      OPEN I-O MTD020
           END-IF.
           IF ST-MTD023 = "35"
              CLOSE MTD023      OPEN OUTPUT MTD023
              CLOSE MTD023      OPEN I-O MTD023
           END-IF.
           IF ST-RCD100 <> "00"
              MOVE "ERRO ABERTURA RCD100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-RCD100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-RCD101 <> "00"
              MOVE "ERRO ABERTURA RCD101: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD010 <> "00"
              MOVE "ERRO ABERTURA CAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD030 <> "00"
              MOVE "ERRO ABERTURA CAD030: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD030 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD002 <> "00"
              MOVE "ERRO ABERTURA MTD002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD020 <> "00"
              MOVE "ERRO ABERTURA MTD020: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD020 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD023 <> "00"
              MOVE "ERRO ABERTURA MTD023: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD023 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           CLOSE      MTD023 MTD020 MTD019
           OPEN INPUT MTD023 MTD020 MTD019

           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM CRIAR-LISTVIEW
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM REALIZAR-BAIXA
                   PERFORM LIMPAR-DADOS
               WHEN GS-LOAD-FLG-TRUE
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-CONTRATO-TRUE
                   PERFORM LE-CONTRATO
                   PERFORM CARREGAR-DADOS
               WHEN GS-POP-UP-VENDEDOR-TRUE
                   PERFORM POP-UP-VENDEDOR
               WHEN GS-POP-UP-CONTRATO-TRUE
                   PERFORM POP-UP-CONTRATO
               WHEN GS-VERIFICAR-CONTRATO-TRUE
                    PERFORM VERIFICA-CONTRATO
               WHEN GS-TRATAR-EVENTO-TRUE
                    PERFORM TRATAR-EVENTOS
               WHEN GS-CRITICAR-TRUE
                    PERFORM CRITICAR-CAMPO
               WHEN GS-POP-UP-BANCO-TRUE
                    PERFORM POP-UP-BANCO
               WHEN GS-SOMAR-TRUE
                    PERFORM SOMAR-TOTAL
               WHEN GS-MARCAR-TITULOS-TRUE
                    PERFORM MARCAR-TITULOS
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       REALIZAR-BAIXA SECTION.
           CLOSE    MTD020 MTD019 MTD023 RCD100 RCD101
           OPEN I-O MTD020 MTD019 MTD023 RCD100 RCD101

           INITIALIZE WSITEM MENSAGEM QTDE-BAIXADA

           INVOKE GS-LISTVIEW "Size" RETURNING WSSIZE
           PERFORM WSSIZE TIMES
               ADD 1                         TO WSITEM

               INVOKE GS-LISTVIEW "ITEMATINDEX"
                         USING WSITEM RETURNING UMITEM

               INITIALIZE WSCHECKENABLE
               INVOKE UMITEM "GETCHECKBOXVALUE"
                      RETURNING WSCHECKENABLE

               IF WSCHECKENABLE = 1
                  ADD 1 TO QTDE-BAIXADA
               END-IF
           END-PERFORM


           INITIALIZE WSITEM MENSAGEM

           INVOKE GS-LISTVIEW "Size" RETURNING WSSIZE
           PERFORM WSSIZE TIMES
              ADD 1                         TO WSITEM

              INVOKE GS-LISTVIEW "ITEMATINDEX"
                        USING WSITEM RETURNING UMITEM

              INITIALIZE WSCHECKENABLE
              INVOKE UMITEM "GETCHECKBOXVALUE"
                     RETURNING WSCHECKENABLE

              IF WSCHECKENABLE = 1
      *>Contrato
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING WSCOLUNACONTRATO RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO) TO CONTRATO-MTG
      *>Album
                 INVOKE UMITEM "GETCOLUMNVALUE"
                        USING WSCOLUNAALBUM RETURNING UMOBJETO
                 INITIALIZE WSTEXTO
                 INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO
                 MOVE FUNCTION NUMVAL(WSTEXTO) TO NRALBUM-MTG

                 READ MTD020 INVALID KEY
                      MOVE "Planilha de montagem não encontrada" TO
                                  MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                 NOT INVALID KEY
                      IF FOGO-MTG = 9
                         MOVE 8 TO FOGO-MTG
                      ELSE
                         MOVE 1 TO FOGO-MTG
                      END-IF
                      REWRITE REG-MTD020 INVALID KEY
                          MOVE "Erro de Regravação...MTD020" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM EXIBIR-MENSAGEM
                      NOT INVALID KEY
                          PERFORM APAGAR-RCD100
                          PERFORM GRAVAR-RCD100
                      END-REWRITE
                 END-READ
              END-IF
           END-PERFORM

           CLOSE      MTD020 MTD019 MTD023 RCD100 RCD101
           OPEN INPUT MTD020 MTD019 MTD023 RCD100 RCD101

           IF MENSAGEM EQUAL SPACES
              MOVE "Álbuns atualizados com sucesso" TO MENSAGEM
              MOVE "I" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       APAGAR-RCD100 SECTION.
           INITIALIZE REG-RCD100
           MOVE ALBUM-MTG TO ALBUM-REC
           READ RCD100 NOT INVALID KEY
                DELETE RCD100 INVALID KEY
                       MOVE "Erro de Exclusão...RCD100" TO MENSAGEM
                       MOVE "C" TO TIPO-MSG
                       PERFORM EXIBIR-MENSAGEM
                NOT INVALID KEY
                       INITIALIZE REG-RCD101
                       MOVE ALBUM-REC TO ALBUM-REC1
                       START RCD101 KEY IS NOT LESS CHAVE-REC1
                                                             INVALID KEY
                             MOVE "10" TO ST-RCD101
                       END-START
                       PERFORM UNTIL ST-RCD101 = "10"
                             READ RCD101 NEXT AT END
                                  MOVE "10" TO ST-RCD101
                             NOT AT END
                                  IF ALBUM-REC <> ALBUM-REC1
                                     MOVE "10" TO ST-RCD101
                                  ELSE
                                     DELETE RCD101 INVALID KEY
                                         MOVE "Erro de Exclusão...RCD101
      -                                       "" TO MENSAGEM
                                         MOVE "C" TO TIPO-MSG
                                         PERFORM EXIBIR-MENSAGEM
                                     END-DELETE
                                  END-IF
                             END-READ
                       END-PERFORM
                END-DELETE.

       GRAVAR-RCD100 SECTION.
           INITIALIZE REG-RCD100
           STRING GS-DATA-MOVTO(5:4) GS-DATA-MOVTO(3:2)
                  GS-DATA-MOVTO(1:2) INTO DATA-MOVTO-REC

           MOVE ALBUM-MTG              TO ALBUM-REC
           STRING GS-DATA-VENDA(5:4) GS-DATA-VENDA(3:2)
                  GS-DATA-VENDA(1:2) INTO AUX-DATA
           MOVE AUX-DATA               TO DATAVEN-REC

           MOVE VISITA-MTG             TO VISITA-REC
           MOVE QT-ENCADER-MTG         TO QENCADER-REC
           MOVE QT-ESTOJO-MTG          TO QESTOJO-REC
           MOVE QT-FOLHAS-MTG          TO QFOLHAS-REC
           MOVE QT-FOTOS-MTG           TO QFOTOS-REC
           MOVE QT-FITAS-MTG           TO QFITAS-REC
           MOVE QT-POSTER-MTG          TO QPOSTER-REC
           MOVE QT-PORTA-FITA-MTG      TO QPFITA-REC
           MOVE QT-FOTO-CD-MTG         TO QFOTO-CD-REC
           MOVE QT-MOLDURA-MTG         TO QMOLDURA-REC
           MOVE QT-PORTA-DVD-MTG       TO QPORTA-DVD-REC
           MOVE QT-DVD-MTG             TO QDVD-REC
           MOVE QT-BOOK-MTG            TO QBOOK-REC
           MOVE GS-VENDEDOR            TO VENDEDOR-REC

           COMPUTE TOTAL-REC ROUNDED = GS-TOTAL-RECEBIDO / QTDE-BAIXADA
           MOVE GS-TAXA                TO TAXA-REC

           WRITE REG-RCD100 INVALID KEY
                 MOVE "Erro de Gravação...RCD100" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM EXIBIR-MENSAGEM
           NOT INVALID KEY
                 PERFORM GRAVAR-RCD101.

       GRAVAR-RCD101 SECTION.
           MOVE 1        TO GS-LINHA
           MOVE SPACES   TO GS-LINHA-DETALHE
           MOVE "LER-LB" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM

           PERFORM UNTIL GS-LINHA-DETALHE = SPACES
               MOVE GS-LINHA-DETALHE            TO DET-01

               INITIALIZE REG-RCD101
               MOVE ALBUM-REC                   TO ALBUM-REC1
               MOVE DET-VENCIMENTO              TO AUX-DATA
               STRING AUX-DATA(5:4) AUX-DATA(3:2) AUX-DATA(1:2)
                 INTO VENCTO-REC1
               MOVE DET-VALOR                   TO AUX-VALOR
               COMPUTE VALOR-REC1 ROUNDED = AUX-VALOR / QTDE-BAIXADA
               MOVE DET-NR-DOCTO                TO NUMERO-REC1
               MOVE DET-PARCELA                 TO PARCELA-REC1
               MOVE DET-QTPARCE                 TO QT-PARCELA-REC1
               MOVE DET-TIPO(1:1)               TO TIPO-REC1
               MOVE DET-BANCO                   TO BANCO-REC1

               WRITE REG-RCD101 INVALID KEY
                     MOVE "Erro de Gravação...RCD101" TO MENSAGEM
                     MOVE "C" TO TIPO-MSG
                     PERFORM EXIBIR-MENSAGEM
               END-WRITE

               ADD 1 TO GS-LINHA
               MOVE SPACES TO GS-LINHA-DETALHE
               MOVE "LER-LB" TO DS-PROCEDURE
               PERFORM CALL-DIALOG-SYSTEM
           END-PERFORM.

       MARCAR-TITULOS SECTION.
           INITIALIZE WSITEM MENSAGEM

           INVOKE GS-LISTVIEW "Size" RETURNING WSSIZE
           PERFORM WSSIZE TIMES
              ADD 1                         TO WSITEM

              INVOKE GS-LISTVIEW "ITEMATINDEX"
                        USING WSITEM RETURNING UMITEM

              IF GS-MARCAR = 1
                 invoke umitem "selectcheckbox"
              ELSE
                 invoke umitem "unselectCheckBox"
              END-IF
           END-PERFORM.


       TRATAR-EVENTOS SECTION.
           evaluate gs-acp-evento
               when 34123  perform chamar-colunas-favopro
           end-evaluate.

       CRITICAR-CAMPO SECTION.
           evaluate gs-campo
               when "EF-DATA-MOVTO"  perform criticar-data-movto
               when "EF-VENDEDOR"    perform criticar-vendedor
               when "EF-DATA-VENDA"  perform criticar-data-venda
               when "EF-VENCTO-CH"   perform criticar-vencimento
               when "EF-VALOR-CH"    perform criticar-valor
               when "EF-BANCO-CH"    perform criticar-banco
               when "EF-PARCELA"     perform criticar-parcela
               when "EF-QT-PARCELA"  perform criticar-qt-parce
               when "SB-TIPO-CH"     perform criticar-tipo-ch
               when "INSERIR"        perform criticar-vencimento
                                        thru criticar-tipo-ch
                                     if gs-flag-critica = 0
                                        perform inserir-linha
                                        perform somar-total
                                     end-if
           end-evaluate.

       criticar-data-movto section.
           if mensagem equal spaces
              if gs-data-movto equal zeros
                 move function current-date to ws-data-sys
                 string ws-dia-cpu ws-mes-cpu ws-ano-cpu into
                        gs-data-movto
                 refresh-object principal
              else
                 call   "UTIVLDT" using gs-data-movto ws-ok
                 cancel "UTIVLDT"
                 if ws-ok equal "N"
                    move "Data de movimento inválida" to mensagem
                    move "C" to tipo-msg
                    perform exibir-mensagem.
       criticar-data-movto-fim.
           exit.

       criticar-vendedor section.
           if mensagem equal spaces
              if gs-vendedor equal zeros
                 move spaces to gs-nome-vendedor
                 refresh-object principal
              else
                 MOVE GS-VENDEDOR              TO CODIGO-CG01
                 READ CGD001 INVALID KEY
                      MOVE "Vendedor Inválido" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM EXIBIR-MENSAGEM
                      MOVE ZEROS  TO GS-VENDEDOR
                      MOVE SPACES TO GS-NOME-VENDEDOR
                  NOT INVALID KEY
                      IF T-VEND-CG01 = 1
                         MOVE NOME-CG01 TO GS-NOME-VENDEDOR
                         PERFORM SET-UP-FOR-REFRESH-SCREEN
                      ELSE
                         MOVE "Código Informado Incompatível com Vendedo
      -                       "r" TO MENSAGEM
                         MOVE "C" TO TIPO-MSG
                         PERFORM EXIBIR-MENSAGEM.
       criticar-vendedor-fim.
           exit.

       criticar-data-venda section.
           if mensagem equal spaces
              if gs-data-venda equal zeros
                 move function current-date to ws-data-sys
                 string ws-dia-cpu ws-mes-cpu ws-ano-cpu into
                        gs-data-venda
                 refresh-object principal
              else
                 call   "UTIVLDT" using gs-data-venda ws-ok
                 cancel "UTIVLDT"
                 if ws-ok equal "N"
                    move "Data da venda inválida" to mensagem
                    move "C" to tipo-msg
                    perform exibir-mensagem.
       criticar-data-venda-fim.
           exit.

       criticar-vencimento section.
           if mensagem equal spaces
              if gs-vencto-ch equal zeros
                 move function current-date to ws-data-sys
                 string ws-dia-cpu ws-mes-cpu ws-ano-cpu into
                        gs-vencto-ch
                 refresh-object principal
              else
                 call   "UTIVLDT" using gs-vencto-ch ws-ok
                 cancel "UTIVLDT"
                 if ws-ok equal "N"
                    move "Data de vencimento inválida" to mensagem
                    move "C" to tipo-msg
                    perform exibir-mensagem.
       criticar-vencimento-fim.
           exit.

       criticar-valor section.
           if mensagem equal spaces
              if gs-valor-ch equal zeros
                 move "Valor não informado" to mensagem
                 move "C" to tipo-msg
                 perform exibir-mensagem.
       criticar-valor-fim.
           exit.

       criticar-banco section.
           if mensagem equal spaces
              if gs-banco-ch > 0
                 move gs-banco-ch to codigo-cad30
                 read cad030 invalid key
                      move "Banco inválido" to mensagem
                      move "C" to tipo-msg
                      perform exibir-mensagem.
       criticar-banco-fim.
           exit.

       criticar-parcela section.
           if mensagem equal spaces
              if gs-parcela = 0
                 add 1 to gs-parcela
                 refresh-object principal.
       criticar-parcela-fim.
           exit.

       criticar-qt-parce section.
           if mensagem equal spaces
              if gs-qt-parcela = 0
                 move "Quantidade de parcela não informada" to mensagem
                 move "C" to tipo-msg
                 perform exibir-mensagem.
       criticar-qt-parce-fim.
           exit.

       criticar-tipo-ch section.
           if mensagem equal spaces
              if gs-tipo-ch equal spaces
                 move "Tipo não informado" to mensagem
                 move "C" to tipo-msg
                 perform exibir-mensagem.
       criticar-tipo-ch-fim.
           exit.

       inserir-linha section.
           move gs-vencto-ch    to det-vencimento
           move gs-valor-ch     to det-valor
           move gs-nr-cheque    to det-nr-docto
           move gs-banco-ch     to det-banco
           move gs-parcela      to det-parcela
           move gs-qt-parcela   to det-qtparce
           move gs-tipo-ch      to det-tipo

           move det-01          to gs-linha-detalhe
           MOVE "INSERIR-LB"    to ds-procedure
           perform call-dialog-system.
       inserir-linha-fim.
           exit.

       somar-total section.
           initialize gs-total-recebido

           move spaces to gs-linha-detalhe
           move 1 to gs-linha
           move "LER-LB" to ds-procedure
           perform call-dialog-system

           perform until gs-linha-detalhe = spaces
               move gs-linha-detalhe to det-01

               move det-valor        to aux-valor
               add aux-valor         to gs-total-recebido

               move spaces           to gs-linha-detalhe
               add  1                to gs-linha
               move "LER-LB"         to ds-procedure
               perform call-dialog-system
           end-perform
           refresh-object principal.
       somar-total-fim.
           exit.

       CARREGAR-DADOS SECTION.
           invoke gs-listview "DeleteAll"

           initialize reg-mtd019
           move gs-contrato        to contrato-mt19
           start mtd019 key is not less album-mt19 invalid key
                move "10" to st-mtd019.

           perform until st-mtd019 = "10"
                read mtd019 next at end
                     move "10" to st-mtd019
                not at end
                     if gs-contrato <> contrato-mt19
                        move "10" to st-mtd019
                     else
                        move albummt19 to album-mtg
                        read mtd020 not invalid key
                             if fogo-mtg <> 1 and 8
      *                         move albummt19 to album-rec
      *                         read rcd100 invalid key
                                     perform inserir-listview
      *                         end-read
                             end-if
                        end-read
                     end-if
                end-read
           end-perform

           perform mostrar-colunas-favoPro
           perform mostrar-fonte-favoPro
           perform zebrar-itensPro

           invoke gs-listview "Size" returning wsSize

           if wsSize = 0
              move "Nenhum Álbum encontrado" to mensagem
              move "C" to tipo-msg
              perform exibir-mensagem
           else
              invoke gs-listview "SetFocus".

       inserir-listview section.
           initialize indice
           invoke gs-listview "adicionarItem" returning wsItem

           add 1 to indice
           initialize wsTexto
           string contrato-mt19 X"00"  into wsTexto
           invoke gs-listview "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string seq-mt19 X"00"  into wsTexto
           invoke gs-listview "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto

           add 1 to indice
           initialize wsTexto
           string nome-form-mt19 x"00"  into wstexto
           invoke gs-listview "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto

           move cidade-mt19 to cidade
           read cad010 invalid key
                initialize reg-cad010
           end-read

           add 1 to indice
           initialize wsTexto
           string nome-compl-cid x"00"  into wstexto
           invoke gs-listview "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto.

           add 1 to indice
           initialize wsTexto
           string uf-cid x"00"  into wstexto
           invoke gs-listview "preencherColunaZ"
             using wsItem lnkcolunasPro(indice) wsTexto.

       CRIAR-LISTVIEW SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke gs-listview "adicionarColunaZ"
            using z"Contrato" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "centered"
          move indice to lnkcolunasPro(indice)
                         WSCOLUNACONTRATO
      *>---
      *>---

          add 1 to indice
          invoke gs-listview "adicionarColunaZ"
            using z"Álbum" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "centered"
          move indice to lnkcolunasPro(indice)
                         WSCOLUNAALBUM
      *>---
      *>---

          add 1 to indice
          invoke gs-listview "adicionarColunaZ"
            using z"Nome" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview "adicionarColunaZ"
            using z"Cidade" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview "adicionarColunaZ"
            using z"UF" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

          perform mostrar-fonte-favoPro
          perform mostrar-colunas-favoPro

          invoke gs-listview "gridLines"
          invoke gs-listview "checkboxes"
          invoke gs-listview "noBorder".

       mostrar-colunas-favoPro section.
          initialize wsTexto
          move "listview-bxrec" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-listview
                                  wsTexto
                                  lnktabelaPro.
       mostrar-colunas-favoPro-fim.
           exit.

       mostrar-fonte-favoPro section.
           move "listview-bxrec" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-listview wsTexto.
       mostrar-fonte-favoPro-fim.
           exit.

       EXPORTAR-PARA-EXCEL-PRO section.
           invoke aListview "ExportarParaOExcel"
                    using gs-listview lnkTabelaPro.
       EXPORTAR-PARA-EXCEL-PRO-fim.
           EXIT.


       zebrar-itensPro section.
           move "listview-bxrec" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-listview wsTexto
           invoke gs-listview "redrawallitems".
       zebrar-itensPro-fim.
           exit.

       chamar-colunas-favoPro section.
           move "listview-bxrec" to wsTexto
           call "COLFAV" using lnkusu
                               gs-listview
                               wsTexto
                               lnktabelaPro

           perform mostrar-colunas-favoPro
           perform mostrar-fonte-favoPro
           perform zebrar-itensPro.
       chamar-colunas-favoPro-fim.
           exit.


       VERIFICA-CONTRATO SECTION.
           MOVE GS-CONTRATO            TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE "Número do Contrato Não Cadastrado no (COP040)"
                  TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".


       POP-UP-VENDEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CGP001T"
           MOVE PASSAR-STRING-1(33: 6) TO GS-VENDEDOR
           MOVE GS-VENDEDOR TO CODIGO-CG01
           READ CGD001 INVALID KEY
                MOVE "Vendedor Inválido" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM
                MOVE ZEROS TO GS-VENDEDOR
                MOVE SPACES TO GS-NOME-VENDEDOR
            NOT INVALID KEY
             IF T-VEND-CG01 = 1
                MOVE NOME-CG01 TO GS-NOME-VENDEDOR
                PERFORM SET-UP-FOR-REFRESH-SCREEN
             ELSE
                MOVE "Código Informado Incompatível com Vendedor" TO
                MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM EXIBIR-MENSAGEM.

       POP-UP-BANCO SECTION.
           CALL   "CAP020T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "CAP020T"
           MOVE FUNCTION NUMVAL(PASSAR-STRING-1(59: 4)) TO GS-BANCO-CH.

       POP-UP-CONTRATO SECTION.
           CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "COP040T"
           MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO
           MOVE PASSAR-STRING-1(22: 11) TO GS-IDENTIFICACAO.

       LE-CONTRATO SECTION.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY MOVE "******" TO IDENTIFICACAO-CO40.
           MOVE IDENTIFICACAO-CO40 TO GS-IDENTIFICACAO.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-MTD019.
           INITIALIZE REG-MTD020.
           INITIALIZE REG-MTD023.
           MOVE GS-CONTRATO      TO CONTRATO-W
           MOVE GS-IDENTIFICACAO TO IDENTIFICACAO-W
           INITIALIZE GS-DATA-BLOCK
           MOVE CONTRATO-W       TO GS-CONTRATO.
           MOVE IDENTIFICACAO-W  TO GS-IDENTIFICACAO.
           MOVE "REFRESH-DATA"   TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.

           MOVE "APAGAR-LB"      TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.



       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1 to gs-flag-critica
           move spaces to mensagem.


       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "BXREC"     TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE CAD010 COD040 CGD001 MTD002 MTD019 MTD020 MTD023
                 CAD004 RCD100 RCD101 CAD030.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

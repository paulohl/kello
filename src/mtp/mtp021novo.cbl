       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTP021.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 04/07/2000.
      *FUNÇÃO: MOVTO P/ DEFINIÇÃO DE PRIORIDADE NA PRODUÇÃO

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window              is class "wclass"
           AListview           is class "alistview".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX004.
           COPY COPX004.
           COPY COPX040.
           COPY COPX045.
           COPY CGPX001.
           COPY CADPRO.SEL.
           COPY CADMOD.SEL.
           COPY MTPX021.
           COPY MTPX021D.
           COPY MTPX021P.
           COPY MTPX024.
           COPY MTPX019.
           COPY LBPX027.
           COPY LOGX002.
           COPY LOGACESS.SEL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW004.
           COPY COPW004.
           COPY COPW040.
           COPY COPW045.
           COPY CGPW001.
           COPY CADPRO.FD.
           COPY CADMOD.FD.
           COPY MTPW021.
           COPY MTPW021D.
           COPY MTPW021P.
           COPY MTPW024.
           COPY MTPW019.
           COPY LBPW027.
           COPY LOGW002.
           COPY LOGACESS.FD.

       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "MTP021.CPB".
           COPY "MTP021.CPY".
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
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-MTD021             PIC XX       VALUE SPACES.
           05  ST-MTD021D            PIC XX       VALUE SPACES.
           05  ST-MTD021P            PIC XX       VALUE SPACES.
           05  ST-MTD024             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-COD004             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD045             PIC XX       VALUE SPACES.
           05  ST-CADPRO             PIC XX       VALUE SPACES.
           05  ST-CADMOD             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-LBD027             PIC XX       VALUE SPACES.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  FS-LOGACESS           PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  WS-OK                 PIC X(01)    VALUE SPACES.
           05  GRAVAR-PRODUTO        PIC 9(001)   VALUE ZEROS.
           05  MASC-ORDEM            PIC ZZZ9     VALUE ZEROS.
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
           05  DATAMOV-W             PIC 9(8)     VALUE ZEROS.
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
           05  ALBUM-W               PIC 9(4)     VALUE ZEROS.
           05  SEQ-W                 PIC 9(2)     VALUE ZEROS.
           05  IDENTIFICACAO-W       PIC X(30)    VALUE SPACES.
           05  MESANO-W              PIC 9(6)     VALUE ZEROS.
           05  MESANO-E              PIC ZZ/ZZZZ.
           05  CAMPANHA-W            PIC 9(02)    VALUE ZEROS.

           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.

           05  AUX-DATA              PIC 9(08)    VALUE ZEROS.
           05  AUX-SEQUENCIA         PIC 9(03)    VALUE ZEROS.
           05  MASC-QTDE             PIC ZZZ.ZZ9  BLANK WHEN ZEROS.
           05  ACHEI                 PIC X(01)    VALUE SPACES.

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01 DET-ANOTACOES.
          05 DET-DIA                   PIC 99/.
          05 DET-MES                   PIC 99/.
          05 DET-ANO                   PIC 9999.
          05 FILLER                    PIC X(01).
          05 DET-SEQ                   PIC ZZ9.
          05 FILLER                    PIC X(01).
          05 DET-HORA                  PIC 9(02).
          05 FILLER                    PIC X(01) VALUE ":".
          05 DET-MINU                  PIC 9(02).
          05 FILLER                    PIC X(02).
          05 DET-USUARIO               PIC X(05).
          05 FILLER                    PIC X(01).
          05 DET-ANOTACAO              PIC X(76).

       01 DET-ANOTACOES2.
          05 FILLER                    PIC X(028).
          05 DET-ANOTACAO2             PIC X(76).

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(48)   VALUE
           "CONF.DEFINICAO DA PRIORIDADE PRODUCAO".
           05  FILLER              PIC X(12)   VALUE "DATA MOVTO: ".
           05  DATA-MOVTO-REL      PIC 99/99/9999 VALUE ZEROS.
           05  FILLER              PIC X(5)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)  VALUE
           "PRI   CONT   TAMANHO          OBSERVAÇÃO".

       01  LINDET.
           05  LINDET-REL          PIC X(80)  VALUE SPACES.

       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 resp-msg            pic x(01).

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

       01 indice                       pic 9(02).

       01 wssize                       pic 9(09) comp-5 value zeros.
       01 wsIndice                     pic 9(09) comp-5 value zeros.
       77 wsTexto                      pic x(255) value spaces.
       77 wsItem                       pic 9(009) comp-5 value zeros.
       77 UMITEM                       object reference.
       77 UMOBJETO                     object reference.

       01 lnktabelaPro.
          02 lnkobjetoscolPro  object reference occurs 99 times.
       01 lnktabelaColPro.
          02 lnkcolunasPro pic 9(09) comp-5 value zeros occurs 99 times.

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
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W DATA-MOVTO-REL.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO            TO DS-VERSION-NO
           MOVE EMPRESA-W                TO EMP-REC
           MOVE NOME-EMPRESA-W           TO EMPRESA-REL
           MOVE "\PROGRAMA\KELLO\*"      TO LNK-PATH-SIS
           MOVE EMPRESA-W                TO LNK-EMPRESA
           MOVE USUARIO-W                TO LNK-USUARIO
           MOVE "CAD004"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD004
           MOVE "CGD001"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001
           MOVE "COD004"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD004
           MOVE "COD040"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD045"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD045.
           MOVE "CADPRO"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CADPRO.
           MOVE "CADMOD"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CADMOD.
           MOVE "MTD019"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "LBD027"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD027.
           MOVE "MTD021"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD021.
           MOVE "MTD021D" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD021D.
           MOVE "MTD021P" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD021P.
           MOVE "MTD024"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD024.
           MOVE "LOG002"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "LOGACESS" TO ARQ-REC. MOVE EMPRESA-REF TO
                                                        ARQUIVO-LOGACESS
           OPEN I-O   MTD021  LOG002 MTD024 LBD027 CAD004 MTD021D
                              MTD021P CGD001 CADPRO CADMOD COD045
                              MTD019
           CLOSE              MTD021 MTD024 LBD027 CAD004 MTD021D
                              MTD021P CGD001 CADPRO CADMOD COD045
                              MTD019
           OPEN I-O   MTD024  MTD021D MTD021 MTD021P CGD001 CADPRO
                      CADMOD  COD045
           OPEN INPUT COD040  COD004  LBD027 CAD004 MTD019

           IF ST-CGD001 = "35"
              CLOSE CGD001      OPEN OUTPUT CGD001
              CLOSE CGD001      OPEN I-O    CGD001
           END-IF.
           IF ST-MTD021 = "35"
              CLOSE MTD021      OPEN OUTPUT MTD021
              CLOSE MTD021      OPEN I-O    MTD021
           END-IF.
           IF ST-MTD021D = "35"
              CLOSE MTD021D     OPEN OUTPUT MTD021D
              CLOSE MTD021D     OPEN I-O    MTD021D
           END-IF.
           IF ST-MTD021P = "35"
              CLOSE MTD021P     OPEN OUTPUT MTD021P
              CLOSE MTD021P     OPEN I-O    MTD021P
           END-IF.
           IF ST-MTD024 = "35"
              CLOSE MTD024      OPEN OUTPUT MTD024
              CLOSE MTD024      OPEN I-O    MTD024
           END-IF.
           IF ST-LOG002 = "35"
              CLOSE LOG002      OPEN OUTPUT LOG002
              CLOSE LOG002      OPEN I-O    LOG002
           END-IF.
           IF ST-LOG002 <> "00"
              MOVE "ERRO ABERTURA LOG002: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LOG002 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD001 <> "00"
              MOVE "ERRO ABERTURA CGD001: " TO GS-MENSAGEM-ERRO
              MOVE ST-CGD001 TO GS-MENSAGEM-ERRO(23:02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: " TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23:02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD004 <> "00"
              MOVE "ERRO ABERTURA COD004: " TO GS-MENSAGEM-ERRO
              MOVE ST-COD004 TO GS-MENSAGEM-ERRO(23:02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD040 <> "00"
              MOVE "ERRO ABERTURA COD040: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD040 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD027 <> "00"
              MOVE "ERRO ABERTURA LBD027: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD027 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD021 <> "00"
              MOVE "ERRO ABERTURA MTD021: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD021 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD021D <> "00"
              MOVE "ERRO ABERTURA MTD021D: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD021D TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD021P <> "00"
              MOVE "ERRO ABERTURA MTD021P: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD021P TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD024 <> "00"
              MOVE "ERRO ABERTURA MTD024: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD024 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.

           CLOSE      MTD021 LOG002 MTD021D MTD021P CGD001 CADPRO CADMOD
                      COD045
           OPEN INPUT MTD021 MTD021D CGD001 CADPRO CADMOD COD045
                      MTD021P

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP021"            to logacess-programa
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

           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
                   PERFORM CRIAR-LISTVIEW
                   PERFORM DESABILITAR-CAMPOS
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   IF GS-TIPO-GRAVACAO = 1
                      PERFORM REGRAVA-DADOS
                   ELSE
                      PERFORM GRAVA-DADOS
                      MOVE CAMPANHA-CO40 TO CAMPANHA-MT24
                      READ MTD024 INVALID KEY
                          MOVE GS-ORDEM TO PRIORIDADE-MT24
                          WRITE REG-MTD024
                          END-WRITE
                      NOT INVALID KEY
                          MOVE GS-ORDEM TO PRIORIDADE-MT24
                          REWRITE REG-MTD024
                          END-REWRITE
                      END-READ
                   END-IF
                   PERFORM LIMPAR-DADOS
                   PERFORM VERIFICAR-PERMISSOES
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
                   PERFORM VERIFICAR-PERMISSOES
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
                   PERFORM VERIFICAR-PERMISSOES
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
               WHEN GS-CARREGA-ANOTACAO-TRUE
                   PERFORM CARREGAR-ANOTACOES
               WHEN GS-SALVAR-ANOTACAO-TRUE
                   PERFORM SALVAR-ANOTACOES
                   PERFORM CARREGAR-ANOTACOES
               WHEN GS-VERIFICAR-EVENTO-TRUE
                   PERFORM VERIFICAR-EVENTO
                   PERFORM VERIFICAR-PERMISSOES
               WHEN GS-CRITICAR-FLG-TRUE
                   PERFORM CRITICAR
               WHEN GS-SUGESTAO-FLG-TRUE
                   PERFORM SUGESTAO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.



       CRITICAR SECTION.
           MOVE SPACES TO MENSAGEM
           EVALUATE GS-CAMPO-CRITICA
               WHEN "EF-CONTRATO"      PERFORM CRITICAR-CONTRATO
               WHEN "EF-ALBUM"         PERFORM CRITICAR-ALBUM
               WHEN "EF-SEQUENCIA"     PERFORM CRITICAR-SEQUENCIA
               WHEN "EF-ORDEM"         PERFORM CRITICAR-ORDEM
               WHEN "EF-OBSERVACAO"    PERFORM CRITICAR-OBSERVACAO
               WHEN "SB-PRIORIDADE"    PERFORM CRITICAR-PRIORIDADE
               WHEN "EF-PRODUTO"       PERFORM CRITICAR-PRODUTO
               WHEN "EF-MODELO"        PERFORM CRITICAR-MODELO
               WHEN "EF-QTDE-PLANILHA" PERFORM CRITICAR-QTDE-PLANILHA
               WHEN "EF-FORNECEDOR"    PERFORM CRITICAR-FORNECEDOR
               WHEN "SB-STATUS"        PERFORM CRITICAR-STATUS
               WHEN "EF-DTPREVISTA"    PERFORM CRITICAR-DTPREVISTA
               WHEN "EF-DTENTREGA"     PERFORM CRITICAR-DTENTREGA
               WHEN "MLE-OBSERVACAO"   PERFORM CRITICAR-OBS-PRODUTO

               WHEN "OK-PRODUTO"       PERFORM CRITICAR-PRODUTO
                                          THRU CRITICAR-OBS-PRODUTO
                                       IF GS-FLAG-CRITICA = 0
                                          PERFORM ATUALIZAR-MTD021P
                                       END-IF
                                       PERFORM LIMPAR-DADOS-PRODUTO
                                       PERFORM CARREGAR-MTD021P
               WHEN "NOVO-PRODUTO"     PERFORM LIMPAR-DADOS-PRODUTO
                                       PERFORM HABILITAR-CAMPOS
                                       SET-FOCUS EF-PRODUTO
               WHEN "ESTORNO-PRODUTO"  MOVE "Deseja Realmente Excluir o
      -                                     "Produto?" TO MENSAGEM
                                       MOVE "Q" TO TIPO-MSG
                                       PERFORM 140-EXIBIR-MENSAGEM
                                       IF RESP-MSG EQUAL "S"
                                          CLOSE      MTD021P
                                          OPEN I-O   MTD021P
                                          DELETE MTD021P INVALID KEY
                                              MOVE "Erro de Exclusão...M
      -                                            "TD021P" TO MENSAGEM
                                              MOVE "C" TO TIPO-MSG
                                              PERFORM
                                                     140-EXIBIR-MENSAGEM
                                          END-DELETE
                                          CLOSE      MTD021P
                                          OPEN INPUT MTD021P
                                       END-IF
                                       PERFORM LIMPAR-DADOS-PRODUTO
                                       PERFORM CARREGAR-MTD021P
               WHEN "LIMPAR-PRODUTO"   PERFORM LIMPAR-DADOS-PRODUTO
                                       PERFORM CARREGAR-MTD021P
           END-EVALUATE.

       CARREGAR-MTD021P SECTION.
           INVOKE GS-LISTVIEW-PRODUTOS "DeleteAll"

           INITIALIZE REG-MTD021P
           MOVE GS-CONTRATO            TO CONTRATO-MT21P
           MOVE GS-ALBUM               TO ALBUM-MT21P
           MOVE GS-SEQUENCIA           TO SEQ-MT21P
           START MTD021P KEY IS NOT LESS CHAVE-MT21P INVALID KEY
                 MOVE "10" TO ST-MTD021P.

           PERFORM UNTIL ST-MTD021P = "10"
                 READ MTD021P NEXT AT END
                      MOVE "10" TO ST-MTD021P
                 NOT AT END
                      IF GS-CONTRATO  <> CONTRATO-MT21P OR
                         GS-ALBUM     <> ALBUM-MT21P    OR
                         GS-SEQUENCIA <> SEQ-MT21P
                         MOVE "10" TO ST-MTD021P
                      ELSE
                         INITIALIZE INDICE
                         INVOKE GS-LISTVIEW-PRODUTOS
                                "ADICIONARITEM" RETURNING WSITEM

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         STRING CHAVE-MT21P X"00"  INTO WSTEXTO
                         INVOKE GS-LISTVIEW-PRODUTOS
                                "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNASPRO(INDICE)
                                WSTEXTO

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         MOVE PRODUTO-MT21P       TO CADPRO-CODIGO
                         MOVE MODELO-MT21P        TO CADPRO-MODELO
                         READ CADPRO INVALID KEY
                              MOVE "*********"    TO CADPRO-NOME
                         END-READ
                         STRING CADPRO-NOME X"00"  INTO WSTEXTO
                         INVOKE GS-LISTVIEW-PRODUTOS
                                "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNASPRO(INDICE)
                                WSTEXTO

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         MOVE MODELO-MT21P        TO CADMOD-CODIGO
                         READ CADMOD INVALID KEY
                              MOVE "*********"    TO CADMOD-NOME
                         END-READ
                         STRING CADMOD-NOME X"00"  INTO WSTEXTO
                         INVOKE GS-LISTVIEW-PRODUTOS
                                "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNASPRO(INDICE)
                                WSTEXTO

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         MOVE QTDE-CONTRATO-MT21P  TO MASC-QTDE
                         STRING MASC-QTDE X"00"  INTO WSTEXTO
                         INVOKE GS-LISTVIEW-PRODUTOS
                                "PREENCHERCOLUNAZ"
                          USING WSITEM LNKCOLUNASPRO(INDICE)
                                WSTEXTO

                         ADD 1 TO INDICE
                         INITIALIZE WSTEXTO
                         MOVE QTDE-PLANILHADA-MT21P TO MASC-QTDE
                         STRING MASC-QTDE X"00"   INTO WSTEXTO
                         INVOKE GS-LISTVIEW-PRODUTOS
                                "PREENCHERCOLUNAZ"
                           USING WSITEM LNKCOLUNASPRO(INDICE)
                                 WSTEXTO
                      END-IF
                 END-READ
           END-PERFORM

           perform mostrar-fonte-favoPro
           perform mostrar-colunas-favoPro
           perform zebrar-itenspro.

       ATUALIZAR-MTD021P SECTION.
           EVALUATE GRAVAR-PRODUTO
               WHEN 1 PERFORM GRAVACAO-PRODUTO
               WHEN 2 PERFORM REGRAVACAO-PRODUTO
               WHEN OTHER MOVE "Não Achei o que fazer" TO MENSAGEM
                          MOVE "C" TO TIPO-MSG
                          PERFORM 140-EXIBIR-MENSAGEM
           END-EVALUATE.

       GRAVACAO-PRODUTO SECTION.
           CLOSE      MTD021P
           OPEN I-O   MTD021P

           INITIALIZE REG-MTD021P

           MOVE GS-CONTRATO                    TO CONTRATO-MT21P
           MOVE GS-ALBUM                       TO ALBUM-MT21P
           MOVE GS-SEQUENCIA                   TO SEQ-MT21P
           MOVE GS-ACP-PRODUTO                 TO PRODUTO-MT21P
           MOVE GS-ACP-MODELO                  TO MODELO-MT21P

           MOVE FUNCTION NUMVAL(GS-STATUS-PRODUTO) TO STATUS-MT21P
           MOVE GS-ACP-FORNECEDOR                  TO FORNECEDOR-MT21P
           STRING GS-ACP-DTPREVISTA(5:4)
                  GS-ACP-DTPREVISTA(3:2)
                  GS-ACP-DTPREVISTA(1:2)
                                             INTO DATA-PREVISTA-MT21P
           STRING GS-ACP-DTENTREGA(5:4)
                  GS-ACP-DTENTREGA(3:2)
                  GS-ACP-DTENTREGA(1:2)
                                             INTO DATA-ENTREGA-MT21P
           MOVE   GS-ACP-OBSERVACAO            TO OBSERVACAO-MT21P
           MOVE   USUARIO-W                    TO USUARIO-MT21P
           MOVE   GS-ACP-QTDE-PLANILHA         TO QTDE-PLANILHADA-MT21P

           WRITE REG-MTD021P INVALID KEY
                 MOVE "Erro de Gravação...MTD021P" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM 140-EXIBIR-MENSAGEM
           END-WRITE

           CLOSE      MTD021P
           OPEN INPUT MTD021P.

       REGRAVACAO-PRODUTO SECTION.
           CLOSE      MTD021P
           OPEN I-O   MTD021P

           MOVE FUNCTION NUMVAL(GS-STATUS-PRODUTO) TO STATUS-MT21P
           MOVE GS-ACP-FORNECEDOR                  TO FORNECEDOR-MT21P
           STRING GS-ACP-DTPREVISTA(5:4)
                  GS-ACP-DTPREVISTA(3:2)
                  GS-ACP-DTPREVISTA(1:2)
                                             INTO DATA-PREVISTA-MT21P
           STRING GS-ACP-DTENTREGA(5:4)
                  GS-ACP-DTENTREGA(3:2)
                  GS-ACP-DTENTREGA(1:2)
                                             INTO DATA-ENTREGA-MT21P
           MOVE   GS-ACP-OBSERVACAO            TO OBSERVACAO-MT21P
           MOVE   USUARIO-W                    TO USUARIO-MT21P
           MOVE   GS-ACP-QTDE-PLANILHA         TO QTDE-PLANILHADA-MT21P

           REWRITE REG-MTD021P INVALID KEY
                 MOVE "Erro de Regravação...MTD021P" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM 140-EXIBIR-MENSAGEM
           END-REWRITE

           CLOSE      MTD021P
           OPEN INPUT MTD021P.


       LIMPAR-DADOS-PRODUTO SECTION.
           INITIALIZE GS-ACP-PRODUTO
                      GS-DESC-PRODUTO
                      GS-ACP-MODELO
                      GS-DESC-MODELO
                      GS-ACP-QTDE-PLANILHA
                      GS-STATUS-PRODUTO
                      GS-ACP-FORNECEDOR
                      GS-DESC-FORNECEDOR
                      GS-ACP-DTPREVISTA
                      GS-ACP-DTENTREGA
                      GS-USUARIO
                      GS-ACP-OBSERVACAO
           REFRESH-OBJECT PRINCIPAL
           MOVE 1  TO GRAVAR-PRODUTO.

       CRITICAR-CONTRATO SECTION.
           IF MENSAGEM EQUAL SPACES
              IF GS-CONTRATO EQUAL ZEROS
                 MOVE "Número do Contrato Não Informado" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM 140-EXIBIR-MENSAGEM
              ELSE
                 MOVE GS-CONTRATO        TO NR-CONTRATO-CO40
                 READ COD040 INVALID KEY
                      MOVE "******"      TO IDENTIFICACAO-CO40
                      MOVE ZEROS         TO CAMPANHA-CO40
                 END-READ
                 MOVE IDENTIFICACAO-CO40 TO GS-IDENTIFICACAO
                 MOVE CAMPANHA-CO40      TO CODIGO-CO04
                                            GS-CAMPANHA
                 READ COD004 INVALID KEY
                      MOVE ZEROS         TO GS-DTINI
                                            GS-DTFIM
                                            GS-CAMPANHA
                 NOT INVALID KEY
                      MOVE DATA-INI-CO04 TO DATA-INV
                      CALL "GRIDAT1" USING DATA-INV
                      MOVE DATA-INV      TO GS-DTINI

                      MOVE DATA-FIM-CO04 TO DATA-INV
                      CALL "GRIDAT1" USING DATA-INV
                      MOVE DATA-INV      TO GS-DTFIM
                 END-READ
                 REFRESH-OBJECT PRINCIPAL.

       CRITICAR-ALBUM SECTION.
           IF MENSAGEM EQUAL SPACES
              IF GS-ALBUM EQUAL ZEROS
                 MOVE 0 TO GS-SEQUENCIA
                 REFRESH-OBJECT PRINCIPAL
              ELSE
                 MOVE GS-CONTRATO   TO CONTRATO-MT19
                 MOVE GS-ALBUM      TO SEQ-MT19
                 READ MTD019 INVALID KEY
                      MOVE "Ficha de Identificação Não Encontrada" TO
                      MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                 NOT INVALID KEY
                      MOVE NOME-FORM-MT19 TO GS-IDENTIFICACAO
                      PERFORM PROCURAR-PROXIMA-SEQ
                      REFRESH-OBJECT PRINCIPAL.

       CRITICAR-SEQUENCIA SECTION.
           IF MENSAGEM EQUAL SPACES
              IF GS-ALBUM EQUAL ZEROS
                 MOVE 0 TO GS-SEQUENCIA
                 REFRESH-OBJECT PRINCIPAL.

           MOVE GS-CONTRATO        TO CONTRATO-MT21
           MOVE GS-ALBUM           TO ALBUM-MT21
           MOVE GS-SEQUENCIA       TO SEQ-MT21
           READ MTD021 INVALID KEY
                INITIALIZE REG-MTD021
                MOVE 0             TO GS-TIPO-GRAVACAO
                DISABLE-OBJECT PB2
                DISABLE-OBJECT PB3
           NOT INVALID KEY
                MOVE 1                  TO GS-TIPO-GRAVACAO
                MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
                MOVE "SENHA54"          TO PROGRAMA-CA004
                READ CAD004 NOT INVALID KEY
                     ENABLE-OBJECT PB3
                END-READ
                ENABLE-OBJECT PB2
           END-READ
           MOVE OBS-MT21           TO GS-OBS
           MOVE ORDEM-MT21         TO GS-ORDEM
           MOVE CAMPANHA-MT21      TO GS-CAMPANHA

           EVALUATE PRIORIDADE-MT21
               WHEN 0 MOVE "0-Não Recebido"  TO GS-PRIORIDADE
               WHEN 1 MOVE "1-Distribuição"  TO GS-PRIORIDADE
               WHEN 2 MOVE "2-Photoshop"     TO GS-PRIORIDADE
               WHEN 3 MOVE "3-Correção"      TO GS-PRIORIDADE
               WHEN 4 MOVE "4-Impressão"     TO GS-PRIORIDADE
               WHEN 5 MOVE "5-Serv. Externo" TO GS-PRIORIDADE
               WHEN 6 MOVE "6-Montagem"      TO GS-PRIORIDADE
               WHEN 7 MOVE "7-Finalizado"    TO GS-PRIORIDADE
               WHEN OTHER MOVE SPACES        TO GS-PRIORIDADE
           END-EVALUATE


           MOVE GS-CONTRATO        TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE "******"      TO IDENTIFICACAO-CO40
                MOVE ZEROS         TO CAMPANHA-CO40
           END-READ
           MOVE IDENTIFICACAO-CO40 TO GS-IDENTIFICACAO
           MOVE CAMPANHA-CO40      TO CODIGO-CO04
                                      GS-CAMPANHA
           READ COD004 INVALID KEY
                MOVE ZEROS         TO GS-DTINI
                                      GS-DTFIM
                                      GS-CAMPANHA
           NOT INVALID KEY
                MOVE DATA-INI-CO04 TO DATA-INV
                CALL "GRIDAT1" USING DATA-INV
                MOVE DATA-INV      TO GS-DTINI

                MOVE DATA-FIM-CO04 TO DATA-INV
                CALL "GRIDAT1" USING DATA-INV
                MOVE DATA-INV      TO GS-DTFIM
           END-READ

           IF GS-ORDEM = 0
              INITIALIZE REG-MTD024
              MOVE CAMPANHA-CO40        TO CAMPANHA-MT24
              READ MTD024 NOT INVALID KEY
                   MOVE PRIORIDADE-MT24 TO GS-ORDEM
              END-READ
              ADD 1                     TO GS-ORDEM
           END-IF

           MOVE "N"                    TO ACHEI
           INITIALIZE REG-MTD021P
           MOVE GS-CONTRATO            TO CONTRATO-MT21P
           MOVE GS-ALBUM               TO ALBUM-MT21P
           MOVE GS-SEQUENCIA           TO SEQ-MT21P
           START MTD021P KEY IS NOT LESS CHAVE-MT21P INVALID KEY
                 MOVE "10" TO ST-MTD021P
           END-START

           PERFORM UNTIL ST-MTD021P = "10"
                 READ MTD021P NEXT AT END
                      MOVE "10" TO ST-MTD021P
                 NOT AT END
                      IF GS-CONTRATO  <> CONTRATO-MT21P OR
                         GS-ALBUM     <> ALBUM-MT21P    OR
                         GS-SEQUENCIA <> SEQ-MT21P
                         MOVE "10" TO ST-MTD021P
                      ELSE
                         MOVE "S"  TO ACHEI
                         MOVE "10" TO ST-MTD021P
                      END-IF
                 END-READ
           END-PERFORM

           IF ACHEI = "N"
              CLOSE      MTD021P
              OPEN I-O   MTD021P
              INITIALIZE REG-COD045
              MOVE GS-CONTRATO            TO NR-CONTRATO-CO45
              START COD045 KEY IS NOT LESS CHAVE-CO45 INVALID KEY
                   MOVE "10" TO ST-COD045
              END-START
              PERFORM UNTIL ST-COD045 = "10"
                   READ COD045 NEXT AT END
                        MOVE "10" TO ST-COD045
                   NOT AT END
                        IF GS-CONTRATO <> NR-CONTRATO-CO45
                           MOVE "10" TO ST-COD045
                        ELSE
                           IF GS-ALBUM = 0
                              INITIALIZE REG-MTD021P
                              MOVE NR-CONTRATO-CO45 TO CONTRATO-MT21P
                              MOVE PRODUTO-CO45     TO PRODUTO-MT21P
                              MOVE MODELO-CO45      TO MODELO-MT21P
                              MOVE 9                TO STATUS-MT21P
                              MOVE USUARIO-W        TO USUARIO-MT21P
                              MOVE QTDE-PRODUTO-CO45 TO
                                   QTDE-CONTRATO-MT21P
                              WRITE REG-MTD021P INVALID KEY
                                   MOVE "Erro de Gravação...MTD021P"
                                            TO MENSAGEM
                                   MOVE "C" TO TIPO-MSG
                                   PERFORM 140-EXIBIR-MENSAGEM
                              END-WRITE
                           END-IF
                        END-IF
                   END-READ
              END-PERFORM
              CLOSE      MTD021P
              OPEN INPUT MTD021P
           END-IF

           PERFORM CARREGAR-MTD021P
           REFRESH-OBJECT PRINCIPAL.


       CRITICAR-ORDEM SECTION.

       CRITICAR-OBSERVACAO SECTION.

       CRITICAR-PRIORIDADE SECTION.

       CRITICAR-PRODUTO SECTION.
           IF MENSAGEM EQUAL SPACES
              IF GS-ACP-PRODUTO EQUAL ZEROS
                 MOVE "Produto Não Informado" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM 140-EXIBIR-MENSAGEM
              ELSE
                 MOVE GS-ACP-PRODUTO TO CADPRO-CODIGO
                 READ CADPRO INVALID KEY
                      MOVE "Produto Inválido" TO MENSAGEM
                      MOVE "C" TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                 NOT INVALID KEY
                      MOVE CADPRO-NOME     TO GS-DESC-PRODUTO
                      MOVE CADPRO-MODELO   TO GS-ACP-MODELO
                                              CADMOD-CODIGO
                      READ CADMOD INVALID KEY
                           MOVE SPACES     TO CADMOD-NOME
                      END-READ
                      MOVE CADMOD-NOME     TO GS-DESC-MODELO
                      REFRESH-OBJECT PRINCIPAL.

       CRITICAR-MODELO SECTION.
           IF MENSAGEM EQUAL SPACES
              IF GS-ACP-MODELO EQUAL ZEROS
                 MOVE SPACES TO GS-DESC-MODELO
              ELSE
                 MOVE GS-ACP-MODELO                 TO CADMOD-CODIGO
                 READ CADMOD INVALID KEY
                      MOVE "Modelo Não Encontrado" TO MENSAGEM
                      MOVE "C"                     TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                 NOT INVALID KEY
                      MOVE CADMOD-NOME              TO GS-DESC-MODELO
                 END-READ
             END-IF
             REFRESH-OBJECT PRINCIPAL.

       CRITICAR-QTDE-PLANILHA SECTION.

       CRITICAR-FORNECEDOR SECTION.
           IF MENSAGEM EQUAL SPACES
              IF GS-ACP-FORNECEDOR EQUAL ZEROS
                 MOVE "Fornecedor Não Informado" TO MENSAGEM
                 MOVE "C" TO TIPO-MSG
                 PERFORM 140-EXIBIR-MENSAGEM
              ELSE
                 MOVE GS-ACP-FORNECEDOR    TO CODIGO-CG01
                 READ CGD001 INVALID KEY
                      MOVE "Fornecedor Inválido" TO MENSAGEM
                      MOVE "C"                   TO TIPO-MSG
                      PERFORM 140-EXIBIR-MENSAGEM
                 NOT INVALID KEY
                      MOVE NOME-CG01             TO GS-DESC-FORNECEDOR
                      REFRESH-OBJECT PRINCIPAL.

       CRITICAR-STATUS SECTION.

       CRITICAR-DTPREVISTA SECTION.
           IF MENSAGEM EQUAL SPACES
              IF GS-ACP-DTPREVISTA > 0
                 CALL   "UTIVLDT" USING GS-ACP-DTPREVISTA WS-OK
                 CANCEL "UTIVLDT"
                 IF WS-OK EQUAL "N"
                    MOVE "Data Prevista Inválida" TO MENSAGEM
                    MOVE "C"                      TO TIPO-MSG
                    PERFORM 140-EXIBIR-MENSAGEM.

       CRITICAR-DTENTREGA SECTION.
           IF MENSAGEM EQUAL SPACES
              IF GS-ACP-DTENTREGA > 0
                 CALL   "UTIVLDT" USING GS-ACP-DTENTREGA WS-OK
                 CANCEL "UTIVLDT"
                 IF WS-OK EQUAL "N"
                    MOVE "Data Entrega Inválida"  TO MENSAGEM
                    MOVE "C"                      TO TIPO-MSG
                    PERFORM 140-EXIBIR-MENSAGEM.

       CRITICAR-OBS-PRODUTO SECTION.

       SUGESTAO SECTION.
           EVALUATE GS-CAMPO-CRITICA
               WHEN "EF-CONTRATO"      PERFORM SUGESTAO-CONTRATO
               WHEN "EF-ALBUM"         PERFORM SUGESTAO-ALBUM
               WHEN "EF-PRODUTO"       PERFORM SUGESTAO-PRODUTO
               WHEN "EF-MODELO"        PERFORM SUGESTAO-PRODUTO
               WHEN "EF-FORNECEDOR"    PERFORM SUGESTAO-FORNECEDOR
           END-EVALUATE.

       SUGESTAO-CONTRATO SECTION.
           CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "COP040T"
           MOVE PASSAR-STRING-1(52: 4)  TO GS-CONTRATO
           MOVE PASSAR-STRING-1(22: 11) TO GS-IDENTIFICACAO
           PERFORM CRITICAR-CONTRATO.

       SUGESTAO-ALBUM SECTION.
           MOVE GS-CONTRATO              TO PASSAR-PARAMETROS(40: 4).
           CALL   "MTP019T" USING PARAMETROS-W PASSAR-PARAMETROS
           CANCEL "MTP019T".
           MOVE PASSAR-PARAMETROS(1: 30) TO GS-IDENTIFICACAO
           MOVE PASSAR-PARAMETROS(45: 4) TO GS-ALBUM
           REFRESH-OBJECT PRINCIPAL.

       SUGESTAO-PRODUTO SECTION.
           CALL   "CONPRO" USING PARAMETROS-W
                                 GS-ACP-PRODUTO
           CANCEL "CONPRO"
           MOVE GS-ACP-PRODUTO     TO CADPRO-CODIGO
           READ CADPRO INVALID KEY
                MOVE "Produto Informado Inválido" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM 140-EXIBIR-MENSAGEM
           NOT INVALID KEY
                MOVE CADPRO-NOME   TO GS-DESC-PRODUTO
                MOVE CADPRO-MODELO TO GS-ACP-MODELO
                                      CADMOD-CODIGO
                READ CADMOD INVALID KEY
                     MOVE SPACES   TO CADMOD-NOME
                END-READ
                MOVE CADMOD-NOME   TO GS-DESC-MODELO
           END-READ
           REFRESH-OBJECT PRINCIPAL.

       SUGESTAO-FORNECEDOR SECTION.
           CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
           CANCEL "CGP001T"
           MOVE PASSAR-STRING-1(33: 6) TO GS-ACP-FORNECEDOR
                                          CODIGO-CG01
           READ CGD001 INVALID KEY
                INITIALIZE REG-CGD001
           END-READ
           MOVE CODIGO-CG01            TO GS-ACP-FORNECEDOR
           MOVE NOME-CG01              TO GS-DESC-FORNECEDOR
           REFRESH-OBJECT PRINCIPAL.

       VERIFICAR-EVENTO SECTION.
           EVALUATE GS-EVENTO
               WHEN 34123  PERFORM CHAMAR-COLUNAS-FAVOPRO
               WHEN 34591  PERFORM PRODUTO-SELECIONADO
                           PERFORM DESABILITAR-CAMPOS
               WHEN 34013  PERFORM PRODUTO-SELECIONADO
                           PERFORM HABILITAR-CAMPOS
                           SET-FOCUS EF-PRODUTO
               WHEN 34592  PERFORM PRODUTO-SELECIONADO
                           PERFORM HABILITAR-CAMPOS
                           SET-FOCUS EF-PRODUTO
               WHEN 34027  PERFORM DESABILITAR-CAMPOS
                           SET-FOCUS EF-CONTRATO
           END-EVALUATE.

       PRODUTO-SELECIONADO SECTION.
           INITIALIZE WSITEM
           INVOKE GS-LISTVIEW-PRODUTOS "NEXTSELECTEDITEM"
                  USING WSITEM RETURNING UMITEM

           IF UMITEM NOT EQUAL NULL
              INVOKE GS-LISTVIEW-PRODUTOS "indexOf" USING UMITEM
                                                RETURNING WSITEM

              INVOKE UMITEM "GETCOLUMNVALUE" USING LNKCOLUNASPRO(1)
                                         RETURNING UMOBJETO
              INITIALIZE WSTEXTO
              INVOKE UMOBJETO "GETVALUE" RETURNING WSTEXTO

              INITIALIZE REG-MTD021P
              MOVE WSTEXTO                      TO CHAVE-MT21P
              READ MTD021P INVALID KEY
                   INITIALIZE GS-STATUS-PRODUTO
                              GS-ACP-FORNECEDOR
                              GS-DESC-FORNECEDOR
                              GS-ACP-OBSERVACAO
                              GS-ACP-DTPREVISTA
                              GS-ACP-DTENTREGA
                              GS-USUARIO
                   MOVE   1                    TO GRAVAR-PRODUTO
              NOT INVALID KEY
                   MOVE PRODUTO-MT21P          TO CADPRO-CODIGO
                                                  GS-ACP-PRODUTO
                   MOVE MODELO-MT21P           TO CADPRO-MODELO
                                                  GS-ACP-MODELO
                                                  CADMOD-CODIGO
                   READ CADPRO INVALID KEY
                        MOVE "***********"     TO CADPRO-NOME
                   END-READ
                   MOVE CADPRO-NOME            TO GS-DESC-PRODUTO
                   READ CADMOD INVALID KEY
                        MOVE "***********"     TO CADMOD-NOME
                   END-READ
                   MOVE CADMOD-NOME            TO GS-DESC-MODELO

                   EVALUATE STATUS-MT21P
                       WHEN 1     MOVE "1 - Iniciado"   TO
                                       GS-STATUS-PRODUTO
                       WHEN 2     MOVE "2 - Andamento"  TO
                                       GS-STATUS-PRODUTO
                       WHEN 3     MOVE "3 - Cancelado"  TO
                                       GS-STATUS-PRODUTO
                       WHEN 4     MOVE "4 - Finalizado" TO
                                       GS-STATUS-PRODUTO
                       WHEN 5     MOVE "5 - Em Atraso"  TO
                                       GS-STATUS-PRODUTO
                       WHEN OTHER MOVE SPACES           TO
                                       GS-STATUS-PRODUTO
                   END-EVALUATE

                   MOVE FORNECEDOR-MT21P           TO CODIGO-CG01
                                                      GS-ACP-FORNECEDOR
                   READ CGD001 INVALID KEY
                        MOVE "******"              TO NOME-CG01
                   END-READ
                   MOVE NOME-CG01                  TO GS-DESC-FORNECEDOR
                   MOVE OBSERVACAO-MT21P           TO GS-ACP-OBSERVACAO

                   STRING DATA-PREVISTA-MT21P(7:2)
                          DATA-PREVISTA-MT21P(5:2)
                          DATA-PREVISTA-MT21P(1:4)
                                                 INTO GS-ACP-DTPREVISTA

                   STRING DATA-ENTREGA-MT21P(7:2)
                          DATA-ENTREGA-MT21P(5:2)
                          DATA-ENTREGA-MT21P(1:4)
                                                 INTO GS-ACP-DTENTREGA
                   MOVE QTDE-PLANILHADA-MT21P      TO
                        GS-ACP-QTDE-PLANILHA
                   MOVE OBSERVACAO-MT21P           TO GS-ACP-OBSERVACAO


                   MOVE   USUARIO-MT21P            TO GS-USUARIO
                   MOVE   2                        TO GRAVAR-PRODUTO
              END-READ
              REFRESH-OBJECT PRINCIPAL.

       CRIAR-LISTVIEW SECTION.
          initialize indice
      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
            using z"Identificação" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "centered"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
                 using z"Produto" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

      *>---
      *>---
          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
                using z"Modelo" returning lnkobjetoscolPro(indice)
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Qtde Contrato" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Qtde Planilhada" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Valor Custo" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

      *>---
      *>---

          add 1 to indice
          invoke gs-listview-produtos "adicionarColunaZ"
           using z"Valor Venda" returning lnkobjetoscolPro(indice)
          invoke lnkobjetoscolPro(indice) "RightJustified"
          move indice to lnkcolunasPro(indice)

          perform mostrar-fonte-favoPro
          perform mostrar-colunas-favoPro

          invoke gs-listview-produtos "gridLines"
          invoke gs-listview-produtos "noBorder".
       CRIAR-LISTVIEW-FIM.
           exit.

       SALVAR-ANOTACOES SECTION.
           CLOSE      MTD021D
           OPEN I-O   MTD021D

           INITIALIZE REG-MTD021D
                      AUX-SEQUENCIA
           MOVE GS-CONTRATO            TO CONTRATO-MT21D
           MOVE GS-ALBUM               TO ALBUM-MT21D
           MOVE GS-SEQUENCIA           TO SEQ-MT21D
           STRING GS-DATA-ANOT(5:4) GS-DATA-ANOT(3:2) GS-DATA-ANOT(1:2)
             INTO DATA-MT21D
           MOVE DATA-MT21D             TO AUX-DATA
           MOVE ALL "9"                TO SEQUENCIA-MT21D

           START MTD021D KEY IS LESS THAN CHAVE-MT21D NOT INVALID KEY
                 READ MTD021D PREVIOUS NOT AT END
                      IF GS-CONTRATO  = CONTRATO-MT21D AND
                         GS-ALBUM     = ALBUM-MT21D    AND
                         GS-SEQUENCIA = SEQ-MT21D      AND
                         AUX-DATA     = DATA-MT21D
                         MOVE SEQUENCIA-MT21D TO AUX-SEQUENCIA.

           ADD 1                       TO AUX-SEQUENCIA

           INITIALIZE REG-MTD021D
           MOVE GS-CONTRATO            TO CONTRATO-MT21D
           MOVE GS-ALBUM               TO ALBUM-MT21D
           MOVE GS-SEQUENCIA           TO SEQ-MT21D
           STRING GS-DATA-ANOT(5:4) GS-DATA-ANOT(3:2) GS-DATA-ANOT(1:2)
             INTO DATA-MT21D
           MOVE AUX-SEQUENCIA           TO SEQUENCIA-MT21D
           MOVE GS-ANOTACAO             TO ANOTACOES-MT21D
           MOVE USUARIO-W               TO USUARIO-MT21D
           ACCEPT WS-HORA-SYS FROM TIME
           STRING WS-HO-SYS WS-MI-SYS INTO HORA-MT21D

           WRITE REG-MTD021D INVALID KEY
                MOVE "Erro de Gravação...MTD021D" TO MENSAGEM
                MOVE "C" TO TIPO-MSG
                PERFORM 140-EXIBIR-MENSAGEM.

           CLOSE      MTD021D
           OPEN INPUT MTD021D.

       CARREGAR-ANOTACOES SECTION.

           CLEAR-OBJECT LB2

           INITIALIZE REG-MTD021D
                      GS-CONT
                      GS-ANOTACAO
                      GS-DATA-ANOT

           MOVE GS-CONTRATO             TO CONTRATO-MT21D
           MOVE GS-ALBUM                TO ALBUM-MT21D
           MOVE GS-SEQUENCIA            TO SEQ-MT21D
           START MTD021D KEY IS NOT LESS CHAVE-MT21D INVALID KEY
                MOVE "10" TO ST-MTD021D.

           PERFORM UNTIL ST-MTD021D = "10"
                READ MTD021D NEXT AT END
                     MOVE "10" TO ST-MTD021D
                NOT AT END
                     IF GS-CONTRATO  <> CONTRATO-MT21D OR
                        GS-ALBUM     <> ALBUM-MT21D    OR
                        GS-SEQUENCIA <> SEQ-MT21D
                        MOVE "10" TO ST-MTD021D
                     ELSE
                        ADD 1                       TO GS-CONT

                        MOVE DATA-MT21D(1:4)        TO DET-ANO
                        MOVE DATA-MT21D(5:2)        TO DET-MES
                        MOVE DATA-MT21D(7:2)        TO DET-DIA

                        MOVE SEQUENCIA-MT21D        TO DET-SEQ

                        MOVE HORA-MT21D(1:2)        TO DET-HORA
                        MOVE HORA-MT21D(3:2)        TO DET-MINU

                        MOVE USUARIO-MT21D          TO DET-USUARIO

                        MOVE ANOTACOES-MT21D(1:76)  TO DET-ANOTACAO
                        MOVE DET-ANOTACOES          TO GS-LINDET
                        MOVE "INSERIR-ANOT"         TO DS-PROCEDURE
                        PERFORM CALL-DIALOG-SYSTEM

                        IF ANOTACOES-MT21D(77:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(77:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(153:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(153:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(229:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(229:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(305:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(305:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(381:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(381:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(457:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(457:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF

                        IF ANOTACOES-MT21D(533:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(533:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF
                        IF ANOTACOES-MT21D(609:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(609:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF
                        IF ANOTACOES-MT21D(685:76) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(685:76)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF
                        IF ANOTACOES-MT21D(761:30) <> SPACES
                           ADD 1                    TO GS-CONT
                           MOVE ANOTACOES-MT21D(761:30)
                                                    TO DET-ANOTACAO2
                           MOVE DET-ANOTACOES2      TO GS-LINDET
                           MOVE "INSERIR-ANOT"      TO DS-PROCEDURE
                           PERFORM CALL-DIALOG-SYSTEM
                        END-IF
                     END-IF
                END-READ
           END-PERFORM

           move function current-date to ws-data-sys
           string ws-dia-cpu ws-mes-cpu ws-ano-cpu into gs-data-anot

           REFRESH-OBJECT WIN1.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

          move-object-handle win1 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 CALL   "COP040T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "COP040T"
                    MOVE PASSAR-STRING-1(52: 4) TO GS-CONTRATO
                    MOVE PASSAR-STRING-1(22: 11) TO GS-IDENTIFICACAO
           END-EVALUATE
           PERFORM LE-CONTRATO.
       LE-CONTRATO SECTION.

      *----------------------------------------------------------------
       EXCLUI SECTION.
           CLOSE    MTD021 MTD021D MTD021P
           OPEN I-O MTD021 MTD021D MTD021P LOG002
           DELETE MTD021 NOT INVALID KEY
                  MOVE USUARIO-W   TO LOG2-USUARIO
                  MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                  MOVE WS-DATA-CPU TO LOG2-DATA
                  ACCEPT WS-HORA-SYS FROM TIME
                  MOVE WS-HORA-SYS TO LOG2-HORAS
                  MOVE "E"         TO LOG2-OPERACAO
                  MOVE "MTD021"    TO LOG2-ARQUIVO
                  MOVE "MTP021"    TO LOG2-PROGRAMA
                  MOVE REG-MTD021  TO LOG2-REGISTRO
                  WRITE REG-LOG002
                  END-WRITE
           END-DELETE

           INITIALIZE REG-MTD021D
           MOVE GS-CONTRATO            TO CONTRATO-MT21D
           MOVE GS-ALBUM               TO ALBUM-MT21D
           MOVE GS-SEQUENCIA           TO SEQ-MT21D
           START MTD021D KEY IS NOT LESS CHAVE-MT21D INVALID KEY
                MOVE "10" TO ST-MTD021D.

           PERFORM UNTIL ST-MTD021D = "10"
                READ MTD021D NEXT AT END
                     MOVE "10" TO ST-MTD021D
                NOT AT END
                     IF GS-CONTRATO  <> CONTRATO-MT21D OR
                        GS-ALBUM     <> ALBUM-MT21D    OR
                        GS-SEQUENCIA <> SEQ-MT21D
                        MOVE "10" TO ST-MTD021D
                     ELSE
                        DELETE MTD021D INVALID KEY
                            MOVE "Erro de Exclusão...MTD021" TO MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM 140-EXIBIR-MENSAGEM
                        END-DELETE
                     END-IF
                END-READ
           END-PERFORM

           INITIALIZE REG-MTD021P
           MOVE GS-CONTRATO            TO CONTRATO-MT21P
           MOVE GS-ALBUM               TO ALBUM-MT21P
           MOVE GS-SEQUENCIA           TO SEQ-MT21P
           START MTD021P KEY IS NOT LESS CHAVE-MT21P INVALID KEY
                MOVE "10" TO ST-MTD021P
           END-START

           PERFORM UNTIL ST-MTD021P = "10"
                READ MTD021P NEXT AT END
                     MOVE "10" TO ST-MTD021P
                NOT AT END
                     IF GS-CONTRATO  <> CONTRATO-MT21P OR
                        GS-ALBUM     <> ALBUM-MT21P    OR
                        GS-SEQUENCIA <> SEQ-MT21P
                        MOVE "10" TO ST-MTD021P
                     ELSE
                        DELETE MTD021P INVALID KEY
                            MOVE "Erro de Exclusão...MTD021P" TO
                            MENSAGEM
                            MOVE "C" TO TIPO-MSG
                            PERFORM 140-EXIBIR-MENSAGEM
                        END-DELETE
                     END-IF
                END-READ
           END-PERFORM

           CLOSE      MTD021 MTD021D MTD021P LOG002
           OPEN INPUT MTD021 MTD021D MTD021P
           PERFORM LIMPAR-DADOS.

       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-MTD021
           MOVE GS-CONTRATO  TO CONTRATO-W
           MOVE GS-ALBUM     TO ALBUM-W
           MOVE GS-SEQUENCIA TO SEQ-W
           MOVE GS-CAMPANHA  TO CAMPANHA-W
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM
           MOVE CONTRATO-W TO GS-CONTRATO
           MOVE ALBUM-W    TO GS-ALBUM
           MOVE SEQ-W      TO GS-SEQUENCIA
           MOVE CAMPANHA-W TO GS-CAMPANHA
           REFRESH-OBJECT PRINCIPAL
           PERFORM DESABILITAR-CAMPOS.
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO           TO CONTRATO-MT21
           MOVE GS-ALBUM              TO ALBUM-MT21
           MOVE GS-SEQUENCIA          TO SEQ-MT21
           IF GS-PRIORIDADE <> SPACES
              MOVE FUNCTION NUMVAL(GS-PRIORIDADE(1:3))
                                      TO PRIORIDADE-MT21
           ELSE
              MOVE 9                  TO PRIORIDADE-MT21
           END-IF

           MOVE GS-OBS                TO OBS-MT21
           MOVE GS-ORDEM              TO ORDEM-MT21
           MOVE GS-CAMPANHA           TO CAMPANHA-MT21.

       GRAVA-DADOS SECTION.
           CLOSE    MTD021
           OPEN I-O MTD021 LOG002
           MOVE ZEROS TO ST-MTD021.
           WRITE REG-MTD021 INVALID KEY
                 MOVE "Erro-Contrato ou Prior.ja existente"
                     TO GS-MENSAGEM-ERRO
                 MOVE ST-MTD021 TO GS-MENSAGEM-ERRO(35: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG2-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG2-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG2-HORAS
                 MOVE "I"         TO LOG2-OPERACAO
                 MOVE "MTD021"    TO LOG2-ARQUIVO
                 MOVE "MTP021"    TO LOG2-PROGRAMA
                 MOVE REG-MTD021  TO LOG2-REGISTRO
                 WRITE REG-LOG002
                 END-WRITE
                 PERFORM MOVER-DADOS-LISTA
                 MOVE "INSERE-LIST" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.

           CLOSE      MTD021
           OPEN INPUT MTD021 LOG002.
       REGRAVA-DADOS SECTION.
           CLOSE    MTD021
           OPEN I-O MTD021 LOG002
           REWRITE REG-MTD021 INVALID KEY
                 MOVE "Erro Regravacao MTD021" TO GS-MENSAGEM-ERRO
                 MOVE ST-MTD021 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 MOVE USUARIO-W   TO LOG2-USUARIO
                 MOVE FUNCTION CURRENT-DATE TO WS-DATA-SYS
                 MOVE WS-DATA-CPU TO LOG2-DATA
                 ACCEPT WS-HORA-SYS FROM TIME
                 MOVE WS-HORA-SYS TO LOG2-HORAS
                 MOVE "A"         TO LOG2-OPERACAO
                 MOVE "MTD021"    TO LOG2-ARQUIVO
                 MOVE "MTP021"    TO LOG2-PROGRAMA
                 MOVE REG-MTD021  TO LOG2-REGISTRO
                 WRITE REG-LOG002
                 END-WRITE
                 PERFORM MOVER-DADOS-LISTA
                 PERFORM LIMPAR-DADOS.

           CLOSE      MTD021 LOG002
           OPEN INPUT MTD021 .

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

       VERIFICAR-PERMISSOES SECTION.
           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA40"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                DISABLE-OBJECT EF-ORDEM
           NOT INVALID KEY
                ENABLE-OBJECT EF-ORDEM
           END-READ

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA54"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                DISABLE-OBJECT PB3
           NOT INVALID KEY
                ENABLE-OBJECT PB3
           END-READ

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA41"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                DISABLE-OBJECT SB-PRIORIDADE
                DISABLE-OBJECT SB-STATUS
           NOT INVALID KEY
                ENABLE-OBJECT SB-PRIORIDADE
                ENABLE-OBJECT SB-STATUS
           END-READ

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA42"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                PERFORM DESABILITAR-CAMPOS
                DISABLE-OBJECT PB13
      *    NOT INVALID KEY
      *         IF GS-PRIORIDADE <> SPACES
      *            PERFORM DESABILITAR-CAMPOS
      *            DISABLE-OBJECT PB13
      *         END-IF
           END-READ

           MOVE COD-USUARIO-W      TO COD-USUARIO-CA004
           MOVE "SENHA43"          TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY
                DISABLE-OBJECT EF-OBSERVACAO
                DISABLE-OBJECT MLE-OBSERVACAO
           NOT INVALID KEY
                IF GS-PRIORIDADE = SPACES
                   ENABLE-OBJECT EF-OBSERVACAO
                   DISABLE-OBJECT MLE-OBSERVACAO
                ELSE
                   DISABLE-OBJECT EF-OBSERVACAO
                   DISABLE-OBJECT MLE-OBSERVACAO
                END-IF
           END-READ.

       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE ORDEM-MT21         TO MASC-ORDEM
           MOVE MASC-ORDEM         TO GS-LINDET(1:4)
           MOVE CONTRATO-MT21      TO GS-LINDET(6:4)
                                      NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                MOVE ZEROS         TO CAMPANHA-CO40.

           MOVE CAMPANHA-CO40      TO CODIGO-CO04
           READ COD004 INVALID KEY
                MOVE ZEROS         TO DATA-INI-CO04
                MOVE ZEROS         TO DATA-FIM-CO04.

           MOVE DATA-INI-CO04      TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO GS-DTINI

           MOVE DATA-FIM-CO04      TO DATA-INV
           CALL "GRIDAT1" USING DATA-INV
           MOVE DATA-INV           TO GS-DTFIM

           STRING CODIGO-CO04 " - " GS-DTINI(1:2) "/" GS-DTINI(3:2) "/"
                                    GS-DTINI(5:4) " ATÉ " GS-DTFIM(1:2)
                                    "/" GS-DTFIM(3:2) "/" GS-DTFIM(5:4)
                                 INTO GS-LINDET(48:30)

           EVALUATE PRIORIDADE-MT21
               WHEN 0 MOVE "0-Não Recebido"  TO GS-LINDET(11:16)
               WHEN 1 MOVE "1-Distribuição"  TO GS-LINDET(11:16)
               WHEN 2 MOVE "2-Photoshop"     TO GS-LINDET(11:16)
               WHEN 3 MOVE "3-Correção"      TO GS-LINDET(11:16)
               WHEN 4 MOVE "4-Impressão"     TO GS-LINDET(11:16)
               WHEN 5 MOVE "5-Serv. Externo" TO GS-LINDET(11:16)
               WHEN 6 MOVE "6-Montagem"      TO GS-LINDET(11:16)
               WHEN 7 MOVE "7-Finalizado"    TO GS-LINDET(11:16)
               WHEN OTHER MOVE SPACES        TO GS-LINDET(11:16)
           END-EVALUATE

           MOVE PRODUTO-MT21           TO CODIGO-LB27
           READ LBD027 INVALID KEY
                MOVE "********"        TO DESCRICAO-LB27
           END-READ
           MOVE DESCRICAO-LB27         TO GS-LINDET(31: 16)
           MOVE OBS-MT21               TO GS-LINDET(79: 30)

           MOVE CONTRATO-MT21          TO CONTRATO-MT21D
           READ MTD021D INVALID KEY
                INITIALIZE REG-MTD021D
           END-READ.

           MOVE ANOTACOES-MT21D        TO GS-LINDET(109:100).
      *--------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "MTP021" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *-------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           OPEN OUTPUT RELAT

           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM COND-HP BEFORE 0
           ELSE
              WRITE REG-RELAT FROM COND-EP BEFORE 0.

           MOVE ZEROS TO PAG-W.
           MOVE ZEROS          TO CONTRATO-MT21
           START MTD021 KEY IS NOT LESS CONTRATO-MT21 INVALID KEY
                 MOVE "10" TO ST-MTD021.
           MOVE ZEROS TO LIN.
           PERFORM CABECALHO.
           PERFORM UNTIL ST-MTD021 = "10"
                 READ MTD021 NEXT RECORD AT END
                      MOVE "10" TO ST-MTD021
                 NOT AT END
                      MOVE SPACES            TO LINDET-REL
                      PERFORM MOVER-DADOS-LISTA
                      MOVE GS-LINDET         TO LINDET-REL

                      WRITE REG-RELAT FROM LINDET
                      ADD 1 TO LIN
                      IF LIN > 56
                         PERFORM CABECALHO
                      END-IF
                 END-READ
           END-PERFORM.

           MOVE SPACES TO REG-RELAT.
           IF IMPRESSORA-W = 01
              WRITE REG-RELAT FROM DESCOND-HP BEFORE PAGE
           ELSE
              WRITE REG-RELAT FROM DESCOND-EP BEFORE PAGE.

           CLOSE RELAT.
       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM CAB03.
           WRITE REG-RELAT FROM CAB04.
           WRITE REG-RELAT FROM CAB03.
           MOVE 4 TO LIN.

       140-exibir-mensagem section.
           move 1         to gs-flag-critica
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.

       mostrar-colunas-favoPro section.
          initialize wsTexto
          move "listview-mtp020-produtos" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  gs-listview-produtos
                                  wsTexto
                                  lnktabelaPro.
       mostrar-colunas-favoPro-fim.
           exit.

       mostrar-fonte-favoPro section.
           move "listview-mtp020-produtos" to wsTexto
           invoke aListview "criarFonte"
                      using lnkusu gs-listview-produtos wsTexto.
       mostrar-fonte-favoPro-fim.
           exit.

       EXPORTAR-PARA-EXCEL-PRO section.
           invoke aListview "ExportarParaOExcel"
                    using gs-listview-produtos lnkTabelaPro.
       EXPORTAR-PARA-EXCEL-PRO-fim.
           EXIT.


       zebrar-itensPro section.
           move "listview-mtp020-produtos" to wsTexto
           invoke aListview "zebrarCor"
                  using lnkusu gs-listview-produtos wsTexto
           invoke gs-listview-produtos "redrawallitems".
       zebrar-itensPro-fim.
           exit.

       chamar-colunas-favoPro section.
           move "listview-mtp020-produtos" to wsTexto
           call "COLFAV" using lnkusu
                               gs-listview-produtos
                               wsTexto
                               lnktabelaPro

           perform mostrar-colunas-favoPro
           perform mostrar-fonte-favoPro
           perform zebrar-itensPro.
       chamar-colunas-favoPro-fim.
           exit.

       DESABILITAR-CAMPOS SECTION.
           DISABLE-OBJECT EF-PRODUTO
           DISABLE-OBJECT EF-MODELO
           DISABLE-OBJECT EF-QTDE-PLANILHA
           DISABLE-OBJECT SB-STATUS
           DISABLE-OBJECT EF-FORNECEDOR
           DISABLE-OBJECT EF10
           DISABLE-OBJECT EF-DTPREVISTA
           DISABLE-OBJECT EF-DTENTREGA
           DISABLE-OBJECT EF-USUARIO
           DISABLE-OBJECT MLE-OBSERVACAO
           DISABLE-OBJECT PB9
           DISABLE-OBJECT PB10
           DISABLE-OBJECT PB12.

       HABILITAR-CAMPOS SECTION.
           ENABLE-OBJECT EF-PRODUTO
           ENABLE-OBJECT EF-MODELO
           ENABLE-OBJECT EF-QTDE-PLANILHA
           ENABLE-OBJECT SB-STATUS
           ENABLE-OBJECT EF-FORNECEDOR
           ENABLE-OBJECT EF10
           ENABLE-OBJECT EF-DTPREVISTA
           ENABLE-OBJECT EF-DTENTREGA
           ENABLE-OBJECT EF-USUARIO
           ENABLE-OBJECT MLE-OBSERVACAO
           ENABLE-OBJECT PB9
           ENABLE-OBJECT PB10
           ENABLE-OBJECT PB12.

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 MTD021 MTD024 COD004 LBD027 CAD004 MTD021D
                 MTD021P CADPRO CADMOD COD045 MTD019.

           open i-o logacess

           move function current-date to ws-data-sys

           move usuario-w           to logacess-usuario
           move ws-data-cpu         to logacess-data
           accept ws-hora-sys from time
           move ws-hora-sys         to logacess-horas
           move 1                   to logacess-sequencia
           move "MTP021"            to logacess-programa
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
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

       PROCURAR-PROXIMA-SEQ SECTION.
           INITIALIZE REG-MTD021
                      GS-SEQUENCIA
           MOVE GS-CONTRATO        TO CONTRATO-MT21
           MOVE GS-ALBUM           TO ALBUM-MT21
           MOVE ALL "9"            TO SEQ-MT21

           START MTD021 KEY IS LESS THAN CHAVE-MT21 NOT INVALID KEY
                 READ MTD021 PREVIOUS NOT AT END
                      IF GS-CONTRATO = CONTRATO-MT21 AND
                         GS-ALBUM    = ALBUM-MT21
                         MOVE SEQ-MT21     TO GS-SEQUENCIA.

           ADD 1 TO GS-SEQUENCIA
           REFRESH-OBJECT PRINCIPAL.
       PROCURAR-PROXIMA-SEQ-FIM.
           EXIT.

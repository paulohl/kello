       copy dslang.cpy.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP041X.
       AUTHOR. MARELI AMANCIO VOLPATO
       DATE-WRITTEN. 04/08/1999.
      *FUNÇÃO: Cadastro de turmas/curso

      * - através da inclusão de um curso/turma deverá ser adicionado
      * no total de formandos atual e inicial do cadastro de
      * contrato(cod040) e também na quantidade de turmas.
      * - através da alteração modificar o total-formandos atual e
      *   quantidade de turmas em caso de exclusão.

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX004.
           COPY IEPX011.
           COPY CEAPX010.
           COPY CGPX010.
           COPY CGPX012.
           COPY COPX040.
           COPY COPX041.
           COPY COPX061.
           COPY MTPX019.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW004.
       COPY IEPW011.
       COPY COPW040.
       COPY COPW041.
       COPY COPW061.
       COPY CGPW012.
       COPY CGPW010.
       COPY CEAPW010.
       COPY MTPW019.
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "COP041X.CPB".
           COPY "COP041X.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD012             PIC XX       VALUE SPACES.
           05  ST-IED011             PIC XX       VALUE SPACES.
           05  ST-CEAD010            PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-COD041             PIC XX       VALUE SPACES.
           05  ST-COD061             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
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
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  CLIENTE-E             PIC ZZZZ.ZZZZ VALUE ZEROS.
           05  SEQ-E                 PIC ZZZZZ    VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999.
           05  AUX-CONTRATO          PIC 9(04).
           05  CONTRATO-W            PIC 9(4)     VALUE ZEROS.
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
           05  QTDE-FORM-W           PIC 9(4)     VALUE ZEROS.
           05  QTDE-TURMAS-W         PIC 99       VALUE ZEROS.
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.


       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(48)   VALUE
           "CONFERENCIA DO CADASTRO DE TURMAS/CURSOS".
           05  FILLER              PIC X(15)   VALUE SPACES.
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
       01  CAB03.
           05  FILLER              PIC X(80)  VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)  VALUE
           "CONT CUR TU FORM L FT MB CK COR-BAIXA  COR-CANUDO RESPON TIP
      -    "O-ALB".

       01  LINDET.
           05  LINDET-REL          PIC X(80)  VALUE SPACES.
       LINKAGE SECTION.

           COPY "PARAMETR".

       01  STRING-1          PIC X(65) VALUE SPACES.
       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.
       MAIN-PROCESS SECTION.
           MOVE STRING-1 TO PARAMETROS-W
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
      *    ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W.
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD004" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CGD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD012" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD012.
           MOVE "IED011" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "COD041" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD041.
           MOVE "COD061" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD061.
           MOVE "MTD019" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD019.
           OPEN I-O   COD041 COD040 COD061 CAD004
           CLOSE      CAD004
           OPEN INPUT IED011 CGD010 CEAD010 CGD012 CAD004 MTD019.

           IF ST-COD041 = "35"
              CLOSE COD041      OPEN OUTPUT COD041
              CLOSE COD041      OPEN I-O COD041
           END-IF.
           IF ST-COD061 = "35"
              CLOSE COD041      OPEN OUTPUT COD041
              CLOSE COD041      OPEN I-O COD041
           END-IF.
           IF ST-CAD004 <> "00"
              MOVE "ERRO ABERTURA CAD004: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CAD004 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-IED011 <> "00"
              MOVE "ERRO ABERTURA IED011: "  TO GS-MENSAGEM-ERRO
              MOVE ST-IED011 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD010 <> "00"
              MOVE "ERRO ABERTURA CGD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CGD012 <> "00"
              MOVE "ERRO ABERTURA CGD012: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CGD012 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-CEAD010 <> "00"
              MOVE "ERRO ABERTURA CEAD010: "  TO GS-MENSAGEM-ERRO
              MOVE ST-CEAD010 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD041 <> "00"
              MOVE "ERRO ABERTURA COD041: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD041 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-COD061 <> "00"
              MOVE "ERRO ABERTURA COD061: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD061 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-MTD019 <> "00"
              MOVE "ERRO ABERTURA MTD019: "  TO GS-MENSAGEM-ERRO
              MOVE ST-MTD019 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-SAVE-FLG-TRUE
                   PERFORM SALVAR-DADOS
                   IF GS-TIPO-GRAVACAO = 1
                      PERFORM REGRAVA-DADOS
                   ELSE
                      PERFORM GRAVA-DADOS
                   END-IF
                   PERFORM LIMPAR-DADOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-EXCLUI-FLG-TRUE
                   PERFORM EXCLUI
                   PERFORM LIMPAR-DADOS
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    COPY IMPRESSORA.CHAMA.
                    IF LNK-MAPEAMENTO <> SPACES
                       PERFORM IMPRIME-RELATORIO
                    END-IF
               WHEN GS-FINALIZA-INICIAL-TRUE
                    PERFORM FINALIZA-INICIAL
               WHEN GS-CARREGA-ULT-TRUE
                   PERFORM CARREGA-ULTIMOS
                   MOVE "SET-POSICAO-CURSOR1" TO DS-PROCEDURE
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE GS-LINDET(1: 4)  TO NR-CONTRATO-CO41
                   MOVE GS-LINDET(6: 3)  TO CURSO-CO41
                   MOVE GS-LINDET(10: 2) TO TURMA-CO41
                   PERFORM CARREGAR-DADOS
               WHEN GS-LE-CURSO-TRUE
                   PERFORM LE-CURSO
               WHEN GS-LE-TIPO-ALBUM-TRUE
                   PERFORM LE-TIPO-ALBUM
               WHEN GS-LE-RESPONSAVEL-TRUE
                   PERFORM LE-RESPONSAVEL
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
               WHEN GS-VERIFICAR-NR-FORM-TRUE
                    PERFORM VERIFICAR-NR-FORM
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".
       VERIFICAR-NR-FORM SECTION.
           MOVE COD-USUARIO-W TO COD-USUARIO-CA004
           MOVE "SENHA36"     TO PROGRAMA-CA004
           READ CAD004 INVALID KEY
                MOVE "N"      TO GS-LIBERADO
           NOT INVALID KEY
                MOVE "S"      TO GS-LIBERADO
           END-READ.
      *    INITIALIZE REG-MTD019
      *    MOVE GS-CONTRATO        TO CONTRATO-MT19
      *    START MTD019 KEY IS NOT LESS ALBUM-MT19 INVALID KEY
      *         MOVE "10" TO ST-MTD019.
      *
      *    PERFORM UNTIL ST-MTD019 = "10"
      *         READ MTD019 NEXT AT END
      *              MOVE "10" TO ST-MTD019
      *         NOT AT END
      *              IF GS-CONTRATO <> CONTRATO-MT19
      *                 MOVE "10" TO ST-MTD019
      *              ELSE
      *                 IF IDENTIFICADO-MT19 = 1
      *                    MOVE "N"  TO GS-LIBERADO
      *                    MOVE "10" TO ST-MTD019
      *                 END-IF
      *              END-IF
      *         END-READ
      *    END-PERFORM.

       FINALIZA-INICIAL SECTION.
           MOVE AUX-CONTRATO TO NR-CONTRATO-CO40
           READ COD040 NOT INVALID KEY
               MOVE "N" TO FLAG-GRAVA-CO40
               REWRITE REG-COD040.

       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 PERFORM CARREGA-POP-UP-CURSO
             WHEN 2 PERFORM CARREGA-POP-UP-RESPONSAVEL
             WHEN 3 PERFORM CARREGA-POP-UP-ALBUM
           END-EVALUATE.
       CARREGA-POP-UP-ALBUM SECTION.
           CALL   "CEAP010T" USING PARAMETROS-W PASSAR-STRING-1.
           CANCEL "CEAP010T".
           MOVE PASSAR-STRING-1(1: 5) TO GS-TIPO-ALBUM.
           MOVE PASSAR-STRING-1(7: 35) TO GS-DESC-ALBUM.
       CARREGA-POP-UP-RESPONSAVEL SECTION.
           MOVE "CLEAR-LIST-BOX1" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE GS-CONTRATO     TO CODIGO-CG12(1: 4).
           MOVE ZEROS           TO CODIGO-CG12(5: 4)
                                   GS-CONT-POPUP-FORNEC.
           START CGD012 KEY IS NOT < CODIGO-CG12 INVALID KEY
                 MOVE "10" TO ST-CGD012.
           PERFORM UNTIL ST-CGD012 = "10"
             READ CGD012 NEXT RECORD AT END MOVE "10" TO ST-CGD012
               NOT AT END
                 MOVE CODIGO-CG12(1: 4) TO CONTRATO-W
                 IF CONTRATO-W <> GS-CONTRATO
                    MOVE "10" TO ST-CGD012
                 ELSE
                   IF CARGO-COMISSAO-CG12 = 5 CONTINUE
                   ELSE
                     MOVE 0   TO CLASSIF-CG10
                     MOVE CODIGO-CG12 TO CODIGO-CG10
                     READ CGD010 INVALID KEY CONTINUE
                      NOT INVALID KEY
                       MOVE COMPRADOR-CG10     TO GS-LINDET1(1: 32)
                       MOVE CODIGO-CG10(5: 4)  TO GS-LINDET1(33: 04)
                       MOVE "INSERE-POP-REPRESENT" TO DS-PROCEDURE
                       PERFORM CALL-DIALOG-SYSTEM
                     END-READ
                   END-IF
                 END-IF
             END-READ
           END-PERFORM.
       CARREGA-POP-UP-CURSO SECTION.
           PERFORM INICIAL-A-PROCURAR.
           MOVE INICIAL-PROCURADA TO NOME-IE11
           START IED011 KEY IS NOT < NOME-IE11 INVALID KEY
                 MOVE "10" TO ST-IED011.
           PERFORM UNTIL ST-IED011 = "10"
              READ IED011 NEXT RECORD AT END MOVE "10" TO ST-IED011
               NOT AT END
                MOVE NOME-IE11(1: I)      TO INICIAL-A-COMPARAR
                IF INICIAL-A-COMPARAR <> INICIAL-PROCURADA
                  MOVE "10" TO ST-IED011
                ELSE
                  MOVE NOME-IE11       TO GS-LINDET1(1: 42)
                  MOVE CODIGO-IE11     TO GS-LINDET1(43: 03)
                  MOVE "INSERE-LISTA-POP-UP" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
              END-READ
           END-PERFORM.
       INICIAL-A-PROCURAR SECTION.
      *Rotina p/ identificar a inicial do nome solicitada a procurar
           MOVE ZEROS  TO SAIR-W.
           MOVE SPACES TO INICIAL-PROCURADA INICIAL-A-COMPARAR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6 OR SAIR-W = 1
               MOVE GS-LINDET1(I: 1) TO LETRA
               IF LETRA = SPACES MOVE 1 TO SAIR-W
                                 SUBTRACT 1 FROM I
               ELSE MOVE GS-LINDET1(I: 1) TO INICIAL-PROCURADA(I: 1)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM I.
       ITEM-SELECIONADO SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1
               MOVE GS-LINDET1(43: 3) TO GS-CURSO
               MOVE GS-LINDET1(1: 40) TO GS-NOME-CURSO
             WHEN 2
               MOVE GS-LINDET1(33: 4) TO GS-COD-RESPONSAVEL
               MOVE GS-LINDET1(1: 30) TO GS-NOME-RESPONSAVEL
           END-EVALUATE.
       EXCLUI SECTION.
           DELETE COD041.

      *   EXCLUI A RELAÇÃO DE EVENTO COM ESSA TURMA.
           MOVE NR-CONTRATO-CO41 TO NR-CONTRATO-CO61.
           MOVE CURSO-CO41       TO CURSO-CO61.
           MOVE TURMA-CO41       TO TURMA-CO61.
           MOVE ZEROS            TO ITEM-CO61.
           START COD061 KEY IS NOT < CHAVE-CO61 INVALID KEY
                        MOVE "10" TO ST-COD061.
           PERFORM UNTIL ST-COD061 = "10"
             READ COD061 NEXT RECORD AT END MOVE "10" TO ST-COD061
               NOT AT END
                  IF NR-CONTRATO-CO61 <> NR-CONTRATO-CO41
                     MOVE "10" TO ST-COD061
                  ELSE
                     IF CURSO-CO61 <> CURSO-CO41 OR
                        TURMA-CO61 <> TURMA-CO41 CONTINUE
                     ELSE
                        DELETE COD061
                        END-DELETE
                     END-IF
                  END-IF
             END-READ
           END-PERFORM.

      *    MOVE NR-CONTRATO-CO41 TO NR-CONTRATO-CO40.
      *    SUBTRAIR 1 DA QTDE DE TURMAS E TAMBEM O NR DE FORMANDOS.
      *    READ COD040 INVALID KEY CONTINUE
      *      NOT INVALID KEY
      *        SUBTRACT 1 FROM QTDE-TURMAS-CO40
      *        SUBTRACT NR-PREV-FORM-CO41 FROM QTDE-FORM-CO40
      *        se contrato nao autorizado fazer atualizacao na
      *        qtde-inicial de formando
      *        IF STATUS-CO40 = 01
      *           SUBTRACT NR-PREV-FORM-CO41 FROM QTDE-FORM-INI-CO40
      *        END-IF
      *        REWRITE REG-COD040
      *        END-REWRITE
      *    END-READ.

           PERFORM CALCULAR-QTDE-TURMAS-FORM
      *    le apartir do cod041 p/ recalcular a qtde-form e qtde-turmas
           MOVE GS-CONTRATO            TO NR-CONTRATO-CO40
           READ COD040 INVALID KEY
                 MOVE "Contrato não cadastrado" TO GS-MENSAGEM-ERRO
                 MOVE ST-COD040        TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO"  TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
               MOVE QTDE-FORM-W        TO QTDE-FORM-CO40
               MOVE QTDE-TURMAS-W      TO QTDE-TURMAS-CO40
      *        se contrato nao autorizado fazer atualizacao na
      *        qtde-inicial de formando
               IF STATUS-CO40 = 01
                  MOVE QTDE-FORM-W     TO QTDE-FORM-INI-CO40
               END-IF
               REWRITE REG-COD040 INVALID KEY
                  MOVE "Erro Regravacao COD040"
                                       TO GS-MENSAGEM-ERRO
                  MOVE ST-COD040       TO GS-MENSAGEM-ERRO(24: 5)
                  MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM.


           PERFORM LIMPAR-DADOS.
           PERFORM CARREGA-ULTIMOS.
       LE-RESPONSAVEL SECTION.
           MOVE 0                  TO CLASSIF-CG10.
           MOVE GS-CONTRATO        TO CODIGO-CG10(1: 4)
           MOVE GS-COD-RESPONSAVEL TO CODIGO-CG10(5: 4).
           READ CGD010 INVALID KEY MOVE "********" TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10          TO GS-NOME-RESPONSAVEL.
       LE-TIPO-ALBUM SECTION.
           MOVE GS-TIPO-ALBUM      TO PRODUTO.
           READ CEAD010 INVALID KEY MOVE "********" TO DESC-PROD.
           MOVE DESC-PROD          TO GS-DESC-ALBUM.
       LE-CURSO SECTION.
           MOVE GS-CURSO           TO CODIGO-IE11.
           READ IED011 INVALID KEY MOVE "******" TO NOME-IE11.
           MOVE NOME-IE11          TO GS-NOME-CURSO.
       CARREGAR-DADOS SECTION.
           START COD041 KEY IS = CHAVE-CO41 INVALID KEY CONTINUE.
           READ COD041 INVALID KEY INITIALIZE REG-COD041.
           MOVE NR-CONTRATO-CO41     TO GS-CONTRATO
           MOVE CURSO-CO41           TO GS-CURSO CODIGO-IE11
           READ IED011 INVALID KEY MOVE SPACES TO NOME-IE11.
           MOVE NOME-IE11            TO GS-NOME-CURSO
           MOVE TURMA-CO41           TO GS-TURMA
           MOVE TURNO-CO41           TO GS-TURNO
           MOVE NR-PREV-FORM-CO41    TO GS-QTDE-FORM
           EVALUATE LISTA-ALUNOS-CO41
             WHEN 0 MOVE "0-Não"     TO GS-LISTA-ALUNO
             WHEN 1 MOVE "1-Sim"     TO GS-LISTA-ALUNO
           END-EVALUATE.
           MOVE 0                    TO CLASSIF-CG10.
           MOVE NR-CONTRATO-CO41     TO CODIGO-CG10(1: 4).
           MOVE RESPONSAVEL-CO41     TO GS-COD-RESPONSAVEL
                                        CODIGO-CG10(5: 4)
           READ CGD010 INVALID KEY MOVE SPACES TO COMPRADOR-CG10.
           MOVE COMPRADOR-CG10       TO GS-NOME-RESPONSAVEL.
           EVALUATE FOTO-TURMA-CO41
             WHEN 0 MOVE "0-Não"     TO GS-FOTO-TURMA
             WHEN 1 MOVE "1-Sim"     TO GS-FOTO-TURMA
           END-EVALUATE.
           EVALUATE MEDIDA-BECA-CO41
             WHEN 0 MOVE "0-Não"     TO GS-MEDIDA-BECA
             WHEN 1 MOVE "1-Sim"     TO GS-MEDIDA-BECA
           END-EVALUATE.

           IF CHECK-CONVITE-CO41 IS NOT NUMERIC
              MOVE 0 TO CHECK-CONVITE-CO41
           END-IF
           EVALUATE CHECK-CONVITE-CO41
               WHEN 0 MOVE "0-Não"     TO GS-CHECK-CONVITE
               WHEN 1 MOVE "1-Sim"     TO GS-CHECK-CONVITE
           END-EVALUATE.

           MOVE COR-FAIXA-CO41       TO GS-COR-FAIXA
           MOVE COR-CANUDO-CO41      TO GS-COR-CANUDO
           MOVE TIPO-ALBUM-CO41      TO GS-TIPO-ALBUM PRODUTO
           READ CEAD010 INVALID KEY MOVE SPACES TO DESC-PROD.
           MOVE DESC-PROD            TO GS-DESC-ALBUM.
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           MOVE GS-AUX-CONTRATO TO AUX-CONTRATO
           INITIALIZE REG-COD041
           INITIALIZE GS-DATA-BLOCK
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
       SALVAR-DADOS SECTION.
           MOVE GS-CONTRATO             TO NR-CONTRATO-CO41
           MOVE GS-CURSO                TO CURSO-CO41
           MOVE GS-TURMA                TO TURMA-CO41
           MOVE GS-TURNO                TO TURNO-CO41
           MOVE GS-QTDE-FORM            TO NR-PREV-FORM-CO41
           MOVE GS-LISTA-ALUNO(1: 1)    TO LISTA-ALUNOS-CO41
           MOVE GS-FOTO-TURMA(1: 1)     TO FOTO-TURMA-CO41
           MOVE GS-MEDIDA-BECA(1: 1)    TO MEDIDA-BECA-CO41
           MOVE GS-CHECK-CONVITE(1:1)   TO CHECK-CONVITE-CO41
           MOVE GS-COD-RESPONSAVEL      TO RESPONSAVEL-CO41
           MOVE GS-COR-FAIXA            TO COR-FAIXA-CO41
           MOVE GS-COR-CANUDO           TO COR-CANUDO-CO41.
           MOVE GS-TIPO-ALBUM           TO TIPO-ALBUM-CO41.

       GRAVA-DADOS SECTION.
           WRITE REG-COD041 INVALID KEY
                 MOVE "Erro gravacao COD041" TO GS-MENSAGEM-ERRO
                 MOVE ST-COD041 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
      *     atualiza cadastro de contrato - COD040
            MOVE NR-CONTRATO-CO41 TO NR-CONTRATO-CO40.
            READ COD040 INVALID KEY
                  MOVE "Contrato não cadastrado" TO GS-MENSAGEM-ERRO
                  MOVE ST-COD040 TO GS-MENSAGEM-ERRO(24: 5)
                  MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                  PERFORM CALL-DIALOG-SYSTEM
               NOT INVALID KEY
      *          adiciona 1 p/ qtde de turmas e nr-prev-form-co41 na
      *          qtde-form-co40
      *          Se status = nao aprovado-01 adiciona qtde-form-ini-co40
                   IF FLAG-GRAVA-CO40 = "S"
                      ADD NR-PREV-FORM-CO41 TO QTDE-FORM-INI-CO40
                   END-IF
                   ADD NR-PREV-FORM-CO41 TO QTDE-FORM-CO40
                   ADD 1                 TO QTDE-TURMAS-CO40
                   REWRITE REG-COD040 INVALID KEY
                     MOVE "Erro Regravacao COD040" TO GS-MENSAGEM-ERRO
                     MOVE ST-COD040 TO GS-MENSAGEM-ERRO(24: 5)
                     MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM.

           PERFORM MOVER-DADOS-LISTA.
           MOVE "INSERE-LIST" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       REGRAVA-DADOS SECTION.
           REWRITE REG-COD041 INVALID KEY
                 MOVE "Erro Regravacao COD041" TO GS-MENSAGEM-ERRO
                 MOVE ST-COD041 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM
           NOT INVALID KEY
                 IF GS-LIBERADO = "S"
                    PERFORM CALCULAR-QTDE-TURMAS-FORM
      *     le apartir do cod041 p/ recalcular a qtde-form e qtde-turmas
                    MOVE GS-CONTRATO TO NR-CONTRATO-CO40
                    READ COD040 INVALID KEY
                          MOVE "Contrato não cadastrado"
                            TO GS-MENSAGEM-ERRO
                          MOVE ST-COD040 TO GS-MENSAGEM-ERRO(24: 5)
                          MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                          PERFORM CALL-DIALOG-SYSTEM
                      NOT INVALID KEY
                          MOVE QTDE-FORM-W    TO QTDE-FORM-CO40
                          MOVE QTDE-TURMAS-W  TO QTDE-TURMAS-CO40
      *                   se contrato nao autorizado fazer atualizacao
      *                   na qtde-inicial de formando
                          IF STATUS-CO40 = 01
                             MOVE QTDE-FORM-W TO QTDE-FORM-INI-CO40
                          END-IF
                          REWRITE REG-COD040 INVALID KEY
                             MOVE "Erro Regravacao COD040"
                               TO GS-MENSAGEM-ERRO
                             MOVE ST-COD040   TO GS-MENSAGEM-ERRO(24: 5)
                             MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                             PERFORM CALL-DIALOG-SYSTEM.
           PERFORM MOVER-DADOS-LISTA.
           MOVE "ATUALIZA-LISTA" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CALCULAR-QTDE-TURMAS-FORM SECTION.
      *    ATUALIZAR AS QUANTIDADES DE FORMANDOS E TURMAS DO COD040
           MOVE ZEROS TO QTDE-FORM-W QTDE-TURMAS-W.
           MOVE GS-CONTRATO TO NR-CONTRATO-CO41.
           MOVE ZEROS TO CURSO-CO41.
           MOVE SPACES TO TURMA-CO41.
           START COD041 KEY IS NOT < CHAVE-CO41 INVALID KEY
                 MOVE "10" TO ST-COD041.
           PERFORM UNTIL ST-COD041 = "10"
             READ COD041 NEXT RECORD AT END MOVE "10" TO ST-COD041
               NOT AT END
                 IF GS-CONTRATO <> NR-CONTRATO-CO41
                    MOVE "10" TO ST-COD041
                 ELSE
                  ADD NR-PREV-FORM-CO41 TO QTDE-FORM-W
                  ADD 1                 TO QTDE-TURMAS-W
                 END-IF
             END-READ
           END-PERFORM.
       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.
       CARREGA-ULTIMOS SECTION.
           MOVE "CLEAR-LIST-BOX" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM
           MOVE GS-CONTRATO  TO NR-CONTRATO-CO41.
           MOVE ZEROS TO CURSO-CO41.
           MOVE SPACES TO TURMA-CO41.
           START COD041 KEY IS NOT < CHAVE-CO41
                    INVALID KEY MOVE "10" TO ST-COD041.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-COD041 = "10"
              READ COD041 NEXT RECORD AT END MOVE "10" TO ST-COD041
              NOT AT END
                IF NR-CONTRATO-CO41 <> GS-CONTRATO
                   MOVE "10" TO ST-COD041
                ELSE
                   PERFORM MOVER-DADOS-LISTA
                   MOVE "INSERE-LIST" TO DS-PROCEDURE
                   PERFORM CALL-DIALOG-SYSTEM
                END-IF
              END-READ
           END-PERFORM.
       MOVER-DADOS-LISTA SECTION.
           MOVE SPACES             TO GS-LINDET
           MOVE NR-CONTRATO-CO41   TO GS-LINDET(1: 5)
           MOVE CURSO-CO41         TO GS-LINDET(6: 4)
           MOVE TURMA-CO41         TO GS-LINDET(10: 2)
           MOVE TURNO-CO41         TO GS-LINDET(13:10)
           MOVE NR-PREV-FORM-CO41  TO GS-LINDET(24: 5)
           MOVE LISTA-ALUNOS-CO41  TO GS-LINDET(29: 2)
           MOVE FOTO-TURMA-CO41    TO GS-LINDET(31: 3)
           MOVE MEDIDA-BECA-CO41   TO GS-LINDET(34: 3)
           MOVE CHECK-CONVITE-CO41 TO GS-LINDET(37: 3)
           MOVE COR-FAIXA-CO41     TO GS-LINDET(40: 11)
           MOVE COR-CANUDO-CO41    TO GS-LINDET(51: 11)
           MOVE RESPONSAVEL-CO41   TO GS-LINDET(62: 7)
           MOVE TIPO-ALBUM-CO41    TO GS-LINDET(69: 8).
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP041X" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.

           COPY CONDENSA.

           MOVE GS-CONTRATO    TO NR-CONTRATO-CO41.
           MOVE ZEROS          TO CURSO-CO41.
           MOVE SPACES         TO TURMA-CO41.
           START COD041 KEY IS = CHAVE-CO41 INVALID KEY
                 MOVE "10" TO ST-COD041.
           MOVE ZEROS TO LIN. PERFORM CABECALHO.
           PERFORM UNTIL ST-COD041 = "10"
             READ COD041 NEXT RECORD AT END MOVE "10" TO ST-COD041
              NOT AT END
                IF NR-CONTRATO-CO41 <> GS-CONTRATO
                         MOVE "10" TO ST-COD041
                ELSE
                  MOVE NR-CONTRATO-CO41   TO LINDET-REL(1: 5)
                  MOVE CURSO-CO41         TO LINDET-REL(6: 4)
                  MOVE TURMA-CO41         TO LINDET-REL(10: 3)
                  MOVE NR-PREV-FORM-CO41  TO LINDET-REL(13: 5)
                  MOVE LISTA-ALUNOS-CO41  TO LINDET-REL(18: 2)
                  MOVE FOTO-TURMA-CO41    TO LINDET-REL(20: 3)
                  MOVE MEDIDA-BECA-CO41   TO LINDET-REL(23: 3)
                  MOVE CHECK-CONVITE-CO41 TO LINDET-REL(26: 3)
                  MOVE COR-FAIXA-CO41     TO LINDET-REL(29: 11)
                  MOVE COR-CANUDO-CO41    TO LINDET-REL(40: 11)
                  MOVE RESPONSAVEL-CO41   TO LINDET-REL(51: 7)
                  MOVE TIPO-ALBUM-CO41    TO LINDET-REL(58: 8)
                  WRITE REG-RELAT FROM LINDET
                  ADD 1 TO LIN
                  IF LIN > 60 PERFORM CABECALHO
                  END-IF
                END-IF
             END-READ
           END-PERFORM.

           COPY DESCONDENSA.

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

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE IED011 COD040 COD041 COD061 CEAD010 CGD010 CGD012
                 CAD004 MTD019
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

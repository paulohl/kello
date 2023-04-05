       COPY DSLANG.CPY.
       identification division.
       program-id.                 cgp006t.
       author.                     ALFREDO SAVIOLLI.
       DATE-WRITTEN.               15-07-2005.

       environment division.
       special-names.              decimal-point  is  comma
                                   console        is  crt.

       class-control.
           Window             is class "wclass".

       input-output section.

       file-control.

           copy "cgpx006".

       data division.
       file section.

           copy  "cgpw006".

       working-storage section.
       78  dialog-system               VALUE "DSGRUN".
       01 Display-Error-No             PIC 9(4).
       01 Display-Details-1            PIC 9(4).
       01 Display-Details-2            PIC 9(4).

       COPY "DS-CNTRL.MF".

       copy "cgp006t.cpb".
       copy "cgp006t.cpy".

       77  TIPO-MSG                  PIC X(01).
       77  RESP-MSG                  PIC X(01).
       77  MENSAGEM                  PIC X(200).
       77  ORDEM-ACHADA              PIC 9(05) VALUE ZEROS.
       77  IND-PROCURA               PIC 9(03) VALUE ZEROS.
       77  IND-STR                   PIC 9(03) VALUE ZEROS.
       77  IND-TAB                   PIC 9(03) VALUE ZEROS.
       77  IND-AJUSTE                PIC 9(03) VALUE ZEROS.
       77  TOPO-TELA                 PIC 9(03) VALUE ZEROS.
           88 AJUSTAR-TABELA         VALUE 2 THRU 15.
       77  KEY-LIMITE                PIC 9(08) VALUE ZEROS.
       77  ABRIU-GS                  PIC X(01) VALUE "S".
       77  IND-P                     PIC 9(03) VALUE ZEROS.
       77  IND-A                     PIC 9(03) VALUE ZEROS.
       77  TAM-PROCURA               PIC 9(02) VALUE ZEROS.
       77  LIMITE-PROCURA            PIC 9(03) VALUE ZEROS.
       77  LIMPOU-TAB                PIC X(01) VALUE SPACES.
       77  LIN                       PIC 9(03).
       77  ST-CGD006                 pic x(02) value "00".
       77  ja                        pic x(01) value "N".
       01  EMP-REFERENCIA.
           05  FILLER                PIC X(15)
               VALUE "\PROGRAMA\KELLO".
           05  VAR1                  PIC X VALUE "\".
           05  EMP-REC               PIC XXX.
           05  VAR2                  PIC X VALUE "\".
           05  ARQ-REC               PIC X(10).
       01  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       LINKAGE SECTION.
       01  STRING-1               PIC X(65) VALUE SPACES.

           COPY "PARAMETR".

       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       010-raiz section.
           perform 020-rotina-inicial
           perform 030-rotina-principal until exit-flag equal 1
           perform 040-rotina-final.
       010-raiz-fim.
          exit.

       020-rotina-inicial section.
           initialize Ds-Control-Block
           initialize Data-block
           move Data-block-version-no
                                    to Ds-Data-Block-Version-No
           move Version-no to Ds-Version-No
           move "cgp006t - Consulta de Motivos" to Linha-label

           move Ds-Push-Set to Ds-Control
           move "cgp006t"    to Ds-Set-Name
           move spaces      to fun-cobol mensagem.
           initialize tab-90.

           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CGD006"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD006.
           OPEN INPUT CGD006.

           if ST-CGD006  not equal "00" and "05" and "35"
              move "Erro de Abertura...CGD006" to mensagem
              move "C" to tipo-msg
              perform exibir-erro
              move  1  to exit-flag
              move "N" to abriu-gs.
       020-rotina-inicial-fim.
          exit.

       030-rotina-principal section.
          perform Call-Dialog-System
          perform 050-processar-cobol.
       030-rotina-principal-fim.
          exit.

       040-rotina-final section.
           if abriu-gs equal "S"
              move ds-quit-set to ds-control
              perform Call-Dialog-System
           end-if
           close CGD006
           exit program.
           stop run.
       040-rotina-final-fim.
          exit.

       050-processar-cobol section.
           evaluate fun-cobol
                when "a" perform centralizar
                when "2" perform 060-pagina-posterior
                when "3" perform 070-cursor-acima
                when "4" perform 080-cursor-abaixo
                when "5" perform 100-tratar-captura
                when "A" perform exibir-mensagem
           end-evaluate.
           move spaces to fun-cobol.
       050-processar-cobol-fim.
           exit.

       CENTRALIZAR SECTION.
          move-object-handle win1 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       060-pagina-posterior section.
           unlock CGD006
           initialize reg-CGD006
           move "N" to exibe-pagina
           move string-1(1:6) to codigo-cg06
           start cgd006 key is not less chave-cg06
           if st-cgd006 = "00" or "02"
              move "N" to ja
              perform 090-ler-pagina-posterior
              move 1 to linha
           else
              if ja = "N"
                 perform mensagem-003.
       060-pagina-posterior-fim.
           exit.

       070-cursor-acima section.
           move "N" to exibe-pagina
           move string-1(1:6)   to codigo-cg06
           move tab-banco(1)    to banco-cg06
           move tab-agencia(1)  to agencia-cg06
           move tab-nr-conta(1) to nr-conta-cg06
           start cgd006 key is less than chave-cg06
           if st-cgd006 = "00" or "02"
              move "N" to ja
              perform 090-ler-pagina-posterior
              move 1 to linha
           else
              if ja = "N"
                 perform mensagem-002.
       070-cursor-acima-fim.
           exit.

       080-cursor-abaixo section.
           move "N" to exibe-pagina
           move string-1(1:6)   to codigo-cg06
           move tab-banco(1)    to banco-cg06
           move tab-agencia(1)  to agencia-cg06
           move tab-nr-conta(1) to nr-conta-cg06
           start CGD006 key is greater than chave-cg06
           if ST-CGD006 = "00" or "02"
              move "N" to ja
              perform 090-ler-pagina-posterior
              move 12 to linha
           else
              if ja = "N"
                 perform mensagem-003.
       080-cursor-abaixo-fim.
           exit.

       090-ler-pagina-posterior section.
           move "N" to limpou-tab
           move 0 to lin
           read CGD006 next
           if ST-CGD006 = "02" or "9a"
              move "00" to ST-CGD006
           end-if
           perform until ST-CGD006 <> "00" or lin = 12
              if limpou-tab equal "N"
                 initialize tab-90
                 move "S" to limpou-tab exibe-pagina
              end-if
              if string-1(1:6) <> codigo-cg06
                 move 12 to lin
              else
                 add 1   to lin
                 move banco-cg06           to tab-banco(lin)
                 move agencia-cg06         to tab-agencia(lin)
                 move nr-conta-cg06        to tab-nr-conta(lin)
                 move titular-conta-cg06   to tab-titular(lin)
                 evaluate tipo-de-conta-cg06
                   when 1 move "Conta Corrente" to tab-tipo-conta(lin)
                   when 2 move "Poupança"       to tab-tipo-conta(lin)
                 end-evaluate
              end-if
              read CGD006 next
              if ST-CGD006 = "02" or "9a"
                 move "00" to ST-CGD006.
       090-ler-pagina-posterior-fim.
           exit.

       100-tratar-captura section.
           if tab-banco(linha) not = zeros
              initialize string-1
              string tab-banco(linha) tab-agencia(linha)
                     tab-nr-conta(linha) into string-1.

      *    if tab-codigo(linha) not = zeros
      *       move tab-codigo(linha) to lnk-banco.
       100-tratar-captura-fim.
          exit.

       exibir-registro section.
           move "EXIBIR-TUDO" to ds-procedure
           perform call-dialog-system.
       exibir-registro-fim.
           exit.

      *----------------------------------------------------*
      * Mensagens Provenientes do Dialog System            *
      *----------------------------------------------------*
       exibir-mensagem section.
           if ds-msg = "001"
              move "S" to ja
              perform mensagem-003.
       exibir-mensagem-fim.
           exit.

       mensagem-002 section.
           move "S" to ja
           move "C" to tipo-msg
           move "Inicio do Arquivo!" to mensagem
           perform exibir-erro.
       mensagem-002-fim.
           exit.

       mensagem-003 section.
           move "S" to ja
           move "C" to tipo-msg
           move "Não Possui Contas Correntes Cadastradas!" to mensagem
           perform exibir-erro.
       mensagem-003-fim.
           exit.

       exibir-erro section.
           move spaces to resp-msg
           call   "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel "MENSAGEM".
       exibir-erro-fim.
           exit.

       Call-Dialog-System SECTION.
          CALL dialog-system USING Ds-Control-Block,
                                   Data-Block
          IF NOT Ds-No-Error
              MOVE Ds-Error-Code TO Display-error-no
              DISPLAY "DS ERROR NO:   "  Display-error-no
              DISPLAY "Error Details(1) :   "  Display-Details-1
              DISPLAY "Error Details(2) :   "  Display-Details-2
              PERFORM 040-ROTINA-FINAL
          END-IF.

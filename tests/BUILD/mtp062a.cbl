       COPY DSLANG.CPY.
       identification division.
       program-id.     mtp062a.
       author.         Alfredo Saviolli.
       date-written.   27-06-2004.

       security.       Impressão das Etiquetas dos Produtos.

       environment division.
       class-control.
           Window             is class "wclass".

       special-names.
           decimal-point is comma
           console is crt.
       file-control.

           copy capx010.

           copy mtpx019.

           copy mtpx020.

           copy iepx011.

           select relat     assign       to printer NOME-IMPRESSORA
                            file status  is ws-status.

           select   work    assign       to    arquivo-work
                            organization is         indexed
                            access mode  is         dynamic
                            record key   is        chave-wk
                            file status  is         st-work.

       data division.
       file section.

           copy capw010.

           copy mtpw019.

           copy mtpw020.

           copy iepw011.

       fd relat.
       01 regrel                  pic x(102).

       fd work.
       01 reg-work.
          05 chave-wk.
             10 folha-wk                       pic 9(05).
             10 numero-etiqueta-wk             pic 9(02).
             10 desc-etiqueta-wk               pic x(09).

       working-storage section.
       78 dialog-system       value "DSGRUN".
       01 display-error-no    pic 9(04).
       01 display-details-1   pic 9(04).
       01 display-details-2   pic 9(04).
       01 arq-help            pic x(11).
       01 abriu-gs            pic x(01) value "S".
       01 mensagem            pic x(200).
       01 tipo-msg            pic x(01).
       01 ws-ultimo           pic 9(06).
       01 resp-msg            pic x(01).
       01 st-cad001           pic x(02) value "00".
       01 st-cad010           pic x(02) value "00".
       01 st-mtd019           pic x(02) value "00".
       01 st-mtd020           pic x(02) value "00".
       01 st-ied011           pic x(02) value "00".
       01 st-work             pic x(02) value "00".
       01 fs-ordserv          pic x(02) value "00".
       01 multiplicador       pic 9(04)v99999.
       01 total-empresa       pic 9(03)v99.
       01 total-etiquetas     pic 9(05) value zeros.
       01 etiquetas-movidas   pic 9(05) value zeros.
       01 pagina1             pic 9(02) value zeros.
       01 numero-pagina       pic 9(05) value zeros.
       01 aux-numero-pagina   pic 9(05) value zeros.
       01 limpar-etiquetas    pic x(01) value spaces.
       01 ok                  pic x(01).
       01 pode                pic x(01).
       01 controle            pic 9(02) value zeros.
       01 masc-qtde           pic Z.ZZ9.
       01 ws-status           pic x(02) value "00".
       01 masc-vlr            pic Z.ZZ9,99 BLANK WHEN ZEROS.
       01 LNK-DETALHE         PIC X(200).
       01  VARIAVEIS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).

       01 LNK-DADOS.
          05 LNK-OPERACAO       PIC X(01).
          05 LNK-PROGRAMA       PIC X(10).
          05 LNK-ARQUIVO        pic x(10).
          05 LNK-CHAVE          PIC X(70).
          05 LNK-REGISTRO       PIC X(1500).
          05 LNK-TIPO           PIC 9(01).
          05 LNK-ERRO           PIC X(01).

       01 ws-data-sys.
          05 ws-data-cpu.
             10 ws-ano-cpu                 pic 9(04).
             10 ws-mes-cpu                 pic 9(02).
             10 ws-dia-cpu                 pic 9(02).
          05 filler                        pic x(13).

       01 lnk-cadpro.
          05 lnk-fornecedor                pic 9(06).
          05 lnk-grupo                     pic 9(06).
          05 lnk-subgrupo                  pic 9(06).
          05 lnk-codigo                    pic 9(06).

           COPY "PARAMETR".

           COPY IMPRESSORA.

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.


      *---------------- Impressao  -------------------------------------
       01  lh00.
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh01.
           03 filler          pic x(01)      value "Q".
           03 lh01-etiqueta   pic 9(03)      value 200.
           03 filler          pic x          value ",".
           03 lh01-gap        pic 9(02)      value 24.
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh02.
           03 filler          pic x(01)      value "q".
           03 lh01-papel      pic 9(03)      value 720.
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh03.
           03 filler          pic x(02)      value "rN".
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh04.
           03 filler          pic x(01)      value "S".
           03 lh04-velocidade pic 9          value 4.
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh05.
           03 filler          pic x          value "D".
           03 lh05-densidade  pic 9          value 4.
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh06.
           03 filler          pic xx         value "ZT".
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh07.
           03 filler          pic xx         value "JB".
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh08.
           03 filler          pic xxx        value "OD".
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh09.
           03 filler          pic x          value "R".
           03 filler          pic xxx        value "0,0".
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh10.
           03 filler          pic x(01)      value "N".
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh11.
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh12.
           03 filler          pic x          value "A".
           03 lh12-p1-x       pic 999        value 120.
           03 filler          pic x          value ",".
           03 lh12-p2-y       pic 9999       value 0005.
           03 filler          pic x          value ",".
           03 lh12-p3-rotacao pic 9          value 0.
           03 filler          pic x          value ",".
           03 lh12-p4-fonte   pic 9          value 4.
           03 filler          pic x          value ",".
           03 lh12-p5-MultX   pic 9          value 4.
           03 filler          pic x          value ",".
           03 lh12-p6-MultY   pic 9          value 4.
           03 filler          pic x          value ",".
           03 lh12-p7-inverso pic x          value "N".
           03 filler          pic x          value ",".
           03 filler          pic x          value '"'.
           03 lh12-p8-texto   pic x(09)      value spaces.
           03 filler          pic x          value '"'.
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh13.
           03 filler          pic x          value "A".
           03 lh13-p1-x       pic 999        value 80.
           03 filler          pic x          value ",".
           03 lh13-p2-y       pic 9999       value 0100.
           03 filler          pic x          value ",".
           03 lh13-p3-rotacao pic 9          value zeros.
           03 filler          pic x          value ",".
           03 lh13-p4-fonte   pic 9          value 4.
           03 filler          pic x          value ",".
           03 lh13-p5-MultX   pic 9          value 1.
           03 filler          pic x          value ",".
           03 lh13-p6-MultY   pic 9          value 1.
           03 filler          pic x          value ",".
           03 lh13-p7-inverso pic x          value "N".
           03 filler          pic x          value ",".
           03 filler          pic x          value '"'.
           03 lh13-p8-texto   pic x(35)      value spaces.
           03 lh13-p8-texto2  pic x(10)      value spaces.
           03 filler          pic x          value '"'.
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh14.
           03 filler          pic x          value "A".
           03 lh14-p1-x       pic 999        value 80.
           03 filler          pic x          value ",".
           03 lh14-p2-y       pic 9999       value 0125.
           03 filler          pic x          value ",".
           03 lh14-p3-rotacao pic 9          value zeros.
           03 filler          pic x          value ",".
           03 lh14-p4-fonte   pic 9          value 4.
           03 filler          pic x          value ",".
           03 lh14-p5-MultX   pic 9          value 1.
           03 filler          pic x          value ",".
           03 lh14-p6-MultY   pic 9          value 1.
           03 filler          pic x          value ",".
           03 lh14-p7-inverso pic x          value "N".
           03 filler          pic x          value ",".
           03 filler          pic x          value '"'.
           03 lh14-p8-texto   pic x(40)      value spaces.
           03 filler          pic x          value '"'.
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh15.
           03 filler          pic x          value "A".
           03 lh15-p1-x       pic 999        value 80.
           03 filler          pic x          value ",".
           03 lh15-p2-y       pic 9999       value 0150.
           03 filler          pic x          value ",".
           03 lh15-p3-rotacao pic 9          value zeros.
           03 filler          pic x          value ",".
           03 lh15-p4-fonte   pic 9          value 4.
           03 filler          pic x          value ",".
           03 lh15-p5-MultX   pic 9          value 1.
           03 filler          pic x          value ",".
           03 lh15-p6-MultY   pic 9          value 1.
           03 filler          pic x          value ",".
           03 lh15-p7-inverso pic x          value "N".
           03 filler          pic x          value ",".
           03 filler          pic x          value '"'.
           03 lh15-p8-texto   pic x(35)      value spaces.
           03 lh15-p8-texto2  pic x(10)      value spaces.
           03 filler          pic x          value '"'.
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       01  lh16.
           03 filler          pic xx         value "P1".
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

      *copy "etiqueta2.rel".

      *---------------- Impressao Numérica -----------------------------
       01  lh00-N.
           03 filler            pic x          value x"0a".
      *    03 filler            pic x          value spaces.

       01  lh01-N.
           03 filler            pic x(01)      value "Q".
           03 lh01-N-etiqueta   pic 9(03)      value 400.
           03 filler            pic x          value ",".
           03 lh01-N-gap        pic 9(02)      value 8.
           03 filler            pic x          value x"0a".
      *    03 filler            pic x          value spaces.

       01  lh02-N.
           03 filler            pic x(01)      value "q".
           03 lh01-N-papel      pic 9(03)      value 776.
           03 filler            pic x          value x"0a".
      *    03 filler            pic x          value spaces.

       01  lh03-N.
           03 filler            pic x(02)      value "rN".
           03 filler            pic x          value x"0a".
      *    03 filler            pic x          value spaces.

       01  lh04-N.
           03 filler            pic x(01)      value "S".
           03 lh04-N-velocidade pic 9          value 4.
           03 filler            pic x          value x"0a".
      *    03 filler            pic x          value spaces.

       01  lh05-N.
           03 filler            pic x          value "D".
           03 lh05-N-densidade  pic 9          value 5.
           03 filler            pic x          value x"0a".
      *    03 filler            pic x          value spaces.

       01  lh06-N.
           03 filler            pic xx         value "ZT".
           03 filler            pic x          value x"0a".
      *    03 filler            pic x          value spaces.

       01  lh07-N.
           03 filler            pic xx         value "JB".
           03 filler            pic x          value x"0a".
      *    03 filler            pic x          value spaces.

       01  lh08-N.
           03 filler            pic xxx        value "OD".
           03 filler            pic x          value x"0a".
      *    03 filler            pic x          value spaces.

       01  lh09-N.
           03 filler            pic x          value "R".
           03 filler            pic xxx        value "0,0".
           03 filler            pic x          value x"0a".
      *    03 filler            pic x          value spaces.

       01  lh10-N.
           03 filler            pic x(01)      value "N".
           03 filler            pic x          value x"0a".
      *    03 filler            pic x          value spaces.

       01  lh11-N.
           03 filler            pic x          value x"0a".
      *    03 filler            pic x          value spaces.

       01  lh12-N1.
           03 filler             pic x          value "A".
           03 lh12-N1-p1-x       pic 999        value 150.
           03 filler             pic x          value ",".
           03 lh12-N1-p2-y       pic 9999       value 0050.
           03 filler             pic x          value ",".
           03 lh12-N1-p3-rotacao pic 9          value 1.
           03 filler             pic x          value ",".
           03 lh12-N1-p4-fonte   pic 9          value 3.
           03 filler             pic x          value ",".
           03 lh12-N1-p5-MultX   pic 9          value 2.
           03 filler             pic x          value ",".
           03 lh12-N1-p6-MultY   pic 9          value 2.
           03 filler             pic x          value ",".
           03 lh12-N1-p7-inverso pic x          value "N".
           03 filler             pic x          value ",".
           03 filler             pic x          value '"'.
           03 lh12-N1-p8-texto   pic x(09)      value spaces.
           03 filler             pic x          value '"'.
           03 filler             pic x          value x"0a".
      *    03 filler             pic x          value spaces.

       01  lh12-N2.
           03 filler             pic x          value "A".
           03 lh12-N2-p1-x       pic 999        value 340.
           03 filler             pic x          value ",".
           03 lh12-N2-p2-y       pic 9999       value 0050.
           03 filler             pic x          value ",".
           03 lh12-N2-p3-rotacao pic 9          value 1.
           03 filler             pic x          value ",".
           03 lh12-N2-p4-fonte   pic 9          value 3.
           03 filler             pic x          value ",".
           03 lh12-N2-p5-MultX   pic 9          value 2.
           03 filler             pic x          value ",".
           03 lh12-N2-p6-MultY   pic 9          value 2.
           03 filler             pic x          value ",".
           03 lh12-N2-p7-inverso pic x          value "N".
           03 filler             pic x          value ",".
           03 filler             pic x          value '"'.
           03 lh12-N2-p8-texto   pic x(09)      value spaces.
           03 filler             pic x          value '"'.
           03 filler             pic x          value x"0a".
      *    03 filler             pic x          value spaces.

       01  lh12-N3.
           03 filler             pic x          value "A".
           03 lh12-N3-p1-x       pic 999        value 540.
           03 filler             pic x          value ",".
           03 lh12-N3-p2-y       pic 9999       value 0050.
           03 filler             pic x          value ",".
           03 lh12-N3-p3-rotacao pic 9          value 1.
           03 filler             pic x          value ",".
           03 lh12-N3-p4-fonte   pic 9          value 3.
           03 filler             pic x          value ",".
           03 lh12-N3-p5-MultX   pic 9          value 2.
           03 filler             pic x          value ",".
           03 lh12-N3-p6-MultY   pic 9          value 2.
           03 filler             pic x          value ",".
           03 lh12-N3-p7-inverso pic x          value "N".
           03 filler             pic x          value ",".
           03 filler             pic x          value '"'.
           03 lh12-N3-p8-texto   pic x(09)      value spaces.
           03 filler             pic x          value '"'.
           03 filler             pic x          value x"0a".
      *    03 filler             pic x          value spaces.

       01  lh12-N4.
           03 filler             pic x          value "A".
           03 lh12-N4-p1-x       pic 999        value 720.
           03 filler             pic x          value ",".
           03 lh12-N4-p2-y       pic 9999       value 0050.
           03 filler             pic x          value ",".
           03 lh12-N4-p3-rotacao pic 9          value 1.
           03 filler             pic x          value ",".
           03 lh12-N4-p4-fonte   pic 9          value 3.
           03 filler             pic x          value ",".
           03 lh12-N4-p5-MultX   pic 9          value 2.
           03 filler             pic x          value ",".
           03 lh12-N4-p6-MultY   pic 9          value 2.
           03 filler             pic x          value ",".
           03 lh12-N4-p7-inverso pic x          value "N".
           03 filler             pic x          value ",".
           03 filler             pic x          value '"'.
           03 lh12-N4-p8-texto   pic x(09)      value spaces.
           03 filler             pic x          value '"'.
           03 filler             pic x          value x"0a".
      *    03 filler             pic x          value spaces.

       01  lh16-N.
           03 filler          pic xx         value "P1".
           03 filler          pic x          value x"0a".
      *    03 filler          pic x          value spaces.

       copy "html.rel".

       copy "mtp062a.cpb".
       copy "mtp062a.cpy".

       copy "ds-cntrl.mf".


       procedure division.
       010-inicio section.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           perform 020-rotina-inicial.
           perform 030-rotina-principal until exit-flag-true.
           perform 040-rotina-final.
       010-inicio-fim.
           exit.

       020-rotina-inicial section.
           initialize  ds-control-block
           initialize  data-block

           move        data-block-version-no to ds-data-block-version-no
           move        version-no            to ds-version-no
           move        ds-push-set           to ds-control
           move        "MTP062A"             to ds-set-name
           move        spaces                to fun-cobol.

           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           OPEN INPUT MTD019 MTD020 IED011 CAD010
           IF ST-CAD010 <> "00"
              STRING "ERRO ABERTURA CAD010: " ST-CAD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM
              MOVE 1 TO EXIT-FLAG.
           IF ST-MTD019 <> "00"
              STRING "ERRO ABERTURA MTD019: " ST-MTD019 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM
              MOVE 1 TO EXIT-FLAG.
           IF ST-MTD020 <> "00"
              STRING "ERRO ABERTURA MTD020: " ST-MTD020 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM
              MOVE 1 TO EXIT-FLAG.
           IF ST-IED011 <> "00"
              STRING "ERRO ABERTURA IED011: " ST-IED011 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM
              MOVE 1 TO EXIT-FLAG.
           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM
              MOVE 1 TO EXIT-FLAG.

           COPY IMPRESSORA.CHAMA.
           IF LNK-MAPEAMENTO = SPACES
              MOVE "Selecionar uma Impressora" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM
              MOVE 1 TO EXIT-FLAG.

           move "c:\tempsis\etiqueta" to arquivo-work.

           move zeros to etiquetas-movidas
           initialize pagina1
           move 1 to numero-pagina
           move 1 to aux-numero-pagina

           open output work
           close       work.

       020-rotina-inicial-fim.
           exit.

       030-rotina-principal section.
           perform 050-call-dialog-system.
           perform 060-processar-cobol.
       030-rotina-principal-fim.
           exit.

       040-rotina-final section.
           if  abriu-gs = "S"
               move    ds-quit-set to ds-control
               perform 050-call-dialog-system.
           close mtd019 mtd020 ied011 cad010
           perform 045-fim-prog.
       040-rotina-final-fim.
           exit.

       045-fim-prog section.
           exit program.
           stop run.
       045-fim-prog-fim.
           exit.

       050-call-dialog-system section.
           call dialog-system using ds-control-block, data-block
           if  not ds-no-error
               move    ds-error-code   to  display-error-no
               display "ds error no:     " display-error-no
               display "error details(1):" display-details-1
               display "error details(2):" display-details-2
               perform 040-rotina-final.
       050-call-dialog-system-fim.
           exit.

       060-processar-cobol section.
           evaluate fun-cobol
               when "a" perform centralizar
               when "C" perform 110-criticar
               when "S" perform 200-sugestao
               when "N" perform 210-navegacao
               when "I" perform 500-incluir
               when "P" if tipo-etiqueta = 1
                           perform 800-etiqueta-numerica
                        else
                           perform 700-imprimir-etiqueta
                        end-if
               when "A" perform 061-anterior
               when "F" perform 062-proximo
               when "L" open output work
                        close       work
                        move 0 to etiquetas-movidas
                        move 1 to numero-pagina
                        move 1 to aux-numero-pagina
                        initialize pagina1
                        perform 063-carregar-pagina
           end-evaluate.
           move spaces to fun-cobol.
       060-processar-cobol-fim.
           exit.

       CENTRALIZAR SECTION.
          move-object-handle win1 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".



       061-anterior section.
           open input work
           initialize reg-work
           if aux-numero-pagina = 1
              move "Página Inicial" to mensagem
              move "C" to tipo-msg
              perform 140-exibir-mensagem
           else
              move spaces to desc-etiqueta1
                             desc-etiqueta2
                             desc-etiqueta3
                             desc-etiqueta4
                             desc-etiqueta5
                             desc-etiqueta6
                             desc-etiqueta7
                             desc-etiqueta8
                             desc-etiqueta9
                             desc-etiqueta10
                             desc-etiqueta11
                             desc-etiqueta12
                             desc-etiqueta13
                             desc-etiqueta14
                             desc-etiqueta15
                             desc-etiqueta16
                             desc-etiqueta17
                             desc-etiqueta18
                             desc-etiqueta19
                             desc-etiqueta20

              compute aux-numero-pagina = aux-numero-pagina - 1
              initialize reg-work
              move aux-numero-pagina to folha-wk
              start work key is not less chave-wk invalid key
                   move "10" to st-work
              end-start
              perform until st-work = "10"
                   read work next at end
                        move "10" to st-work
                   not at end
                        if aux-numero-pagina <> folha-wk
                           move "10" to st-work
                        else
                      evaluate numero-etiqueta-wk
                        when 1  move desc-etiqueta-wk to desc-etiqueta1
                        when 2  move desc-etiqueta-wk to desc-etiqueta2
                        when 3  move desc-etiqueta-wk to desc-etiqueta3
                        when 4  move desc-etiqueta-wk to desc-etiqueta4
                        when 5  move desc-etiqueta-wk to desc-etiqueta5
                        when 6  move desc-etiqueta-wk to desc-etiqueta6
                        when 7  move desc-etiqueta-wk to desc-etiqueta7
                        when 8  move desc-etiqueta-wk to desc-etiqueta8
                        when 9  move desc-etiqueta-wk to desc-etiqueta9
                        when 10 move desc-etiqueta-wk to desc-etiqueta10
                        when 11 move desc-etiqueta-wk to desc-etiqueta11
                        when 12 move desc-etiqueta-wk to desc-etiqueta12
                        when 13 move desc-etiqueta-wk to desc-etiqueta13
                        when 14 move desc-etiqueta-wk to desc-etiqueta14
                        when 15 move desc-etiqueta-wk to desc-etiqueta15
                        when 16 move desc-etiqueta-wk to desc-etiqueta16
                        when 17 move desc-etiqueta-wk to desc-etiqueta17
                        when 18 move desc-etiqueta-wk to desc-etiqueta18
                        when 19 move desc-etiqueta-wk to desc-etiqueta19
                        when 20 move desc-etiqueta-wk to desc-etiqueta20
                      end-evaluate
                        end-if
                   end-read
              end-perform
           end-if
           close work.
       061-anterior-fim.
           exit.

       062-proximo section.
           open input work
           initialize reg-work
           compute aux-numero-pagina = aux-numero-pagina + 1
           move spaces to desc-etiqueta1
                          desc-etiqueta2
                          desc-etiqueta3
                          desc-etiqueta4
                          desc-etiqueta5
                          desc-etiqueta6
                          desc-etiqueta7
                          desc-etiqueta8
                          desc-etiqueta9
                          desc-etiqueta10
                          desc-etiqueta11
                          desc-etiqueta12
                          desc-etiqueta13
                          desc-etiqueta14
                          desc-etiqueta15
                          desc-etiqueta16
                          desc-etiqueta17
                          desc-etiqueta18
                          desc-etiqueta19
                          desc-etiqueta20

           initialize reg-work
           move aux-numero-pagina to folha-wk
           start work key is not less chave-wk invalid key
                move "10" to st-work
                move "Página Final" to mensagem
                move "C" to tipo-msg
                perform 140-exibir-mensagem
                compute aux-numero-pagina = aux-numero-pagina - 1
           end-start
           perform until st-work = "10"
                read work next at end
                     move "10" to st-work
                not at end
                     if aux-numero-pagina <> folha-wk
                        move "10" to st-work
                     else
                   evaluate numero-etiqueta-wk
                     when 1  move desc-etiqueta-wk to desc-etiqueta1
                     when 2  move desc-etiqueta-wk to desc-etiqueta2
                     when 3  move desc-etiqueta-wk to desc-etiqueta3
                     when 4  move desc-etiqueta-wk to desc-etiqueta4
                     when 5  move desc-etiqueta-wk to desc-etiqueta5
                     when 6  move desc-etiqueta-wk to desc-etiqueta6
                     when 7  move desc-etiqueta-wk to desc-etiqueta7
                     when 8  move desc-etiqueta-wk to desc-etiqueta8
                     when 9  move desc-etiqueta-wk to desc-etiqueta9
                     when 10 move desc-etiqueta-wk to desc-etiqueta10
                     when 11 move desc-etiqueta-wk to desc-etiqueta11
                     when 12 move desc-etiqueta-wk to desc-etiqueta12
                     when 13 move desc-etiqueta-wk to desc-etiqueta13
                     when 14 move desc-etiqueta-wk to desc-etiqueta14
                     when 15 move desc-etiqueta-wk to desc-etiqueta15
                     when 16 move desc-etiqueta-wk to desc-etiqueta16
                     when 17 move desc-etiqueta-wk to desc-etiqueta17
                     when 18 move desc-etiqueta-wk to desc-etiqueta18
                     when 19 move desc-etiqueta-wk to desc-etiqueta19
                     when 20 move desc-etiqueta-wk to desc-etiqueta20
                   end-evaluate
                     end-if
                end-read
           end-perform
           close work.
       062-proximo-fim.
           exit.

       063-carregar-pagina section.
           open input work
           move spaces to desc-etiqueta1
                          desc-etiqueta2
                          desc-etiqueta3
                          desc-etiqueta4
                          desc-etiqueta5
                          desc-etiqueta6
                          desc-etiqueta7
                          desc-etiqueta8
                          desc-etiqueta9
                          desc-etiqueta10
                          desc-etiqueta11
                          desc-etiqueta12
                          desc-etiqueta13
                          desc-etiqueta14
                          desc-etiqueta15
                          desc-etiqueta16
                          desc-etiqueta17
                          desc-etiqueta18
                          desc-etiqueta19
                          desc-etiqueta20

           initialize reg-work
           move aux-numero-pagina to folha-wk
           start work key is not less chave-wk invalid key
                move "10" to st-work
           end-start
           perform until st-work = "10"
                read work next at end
                     move "10" to st-work
                not at end
                     if aux-numero-pagina <> folha-wk
                        move "10" to st-work
                     else
                   evaluate numero-etiqueta-wk
                     when 1  move desc-etiqueta-wk to desc-etiqueta1
                     when 2  move desc-etiqueta-wk to desc-etiqueta2
                     when 3  move desc-etiqueta-wk to desc-etiqueta3
                     when 4  move desc-etiqueta-wk to desc-etiqueta4
                     when 5  move desc-etiqueta-wk to desc-etiqueta5
                     when 6  move desc-etiqueta-wk to desc-etiqueta6
                     when 7  move desc-etiqueta-wk to desc-etiqueta7
                     when 8  move desc-etiqueta-wk to desc-etiqueta8
                     when 9  move desc-etiqueta-wk to desc-etiqueta9
                     when 10 move desc-etiqueta-wk to desc-etiqueta10
                     when 11 move desc-etiqueta-wk to desc-etiqueta11
                     when 12 move desc-etiqueta-wk to desc-etiqueta12
                     when 13 move desc-etiqueta-wk to desc-etiqueta13
                     when 14 move desc-etiqueta-wk to desc-etiqueta14
                     when 15 move desc-etiqueta-wk to desc-etiqueta15
                     when 16 move desc-etiqueta-wk to desc-etiqueta16
                     when 17 move desc-etiqueta-wk to desc-etiqueta17
                     when 18 move desc-etiqueta-wk to desc-etiqueta18
                     when 19 move desc-etiqueta-wk to desc-etiqueta19
                     when 20 move desc-etiqueta-wk to desc-etiqueta20
                   end-evaluate
                     end-if
                end-read
           end-perform
           close work.
       063-carregar-pagina-fim.
           exit.

       110-criticar section.
           move    spaces to mensagem.
           evaluate    campo-critica
               when "EF-CONTRATO"      perform 111-criticar-contrato
               when "EF-ALBUM-INI"     perform 112-criticar-album-ini
               when "EF-ALBUM-FIM"     perform 113-criticar-album-fim
               when "EF-QTDE-COPIAS"   perform 114-criticar-qtde-cop
               when "REGISTRO"         perform 111-criticar-contrato
                                          thru 114-criticar-qtde-cop
           end-evaluate.
       110-criticar-fim.
           exit.

       111-criticar-contrato section.
           if mensagem equal spaces
               if acp-contrato equal zeros
                   move "Contrato Não Informado" to mensagem
                   move "C" to tipo-msg
                   perform 140-exibir-mensagem
               else
                   initialize reg-mtd020
                   move acp-contrato to contrato-mtg
                   start mtd020 key is not less album-mtg invalid key
                        move "Contrato Não Encontrado no MTD020" to
                        mensagem
                        move "C" to tipo-msg
                        perform 140-exibir-mensagem
                   not invalid key
                        read mtd020 next at end
                             move "Contrato Não Encontrado no MTD020" to
                                   mensagem
                             move "C" to tipo-msg
                             perform 140-exibir-mensagem
                        not at end
                             if acp-contrato <> contrato-mtg
                                move "Contrato Não Encontrado no MTD020"
                                  to mensagem
                                move "C" to tipo-msg
                                perform 140-exibir-mensagem.
       111-criticar-contrato-fim.
           exit.

       112-criticar-album-ini section.
       112-criticar-album-ini-fim.
           exit.

       113-criticar-album-fim section.
       113-criticar-album-fim-fim.
           exit.

       114-criticar-qtde-cop section.
           if mensagem equal spaces
              if qtde-copias = 0
                 move 1 to qtde-copias
                 refresh-object win1.
       114-criticar-qtde-cop-fim.
           exit.

       140-exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move    1 to flag-critica.
       140-exibir-mensagem-fim.
           exit.

       200-sugestao section.
           move "Sugestão Inexistente!" to mensagem
           move "C" to tipo-msg
           perform 140-exibir-mensagem.
       200-sugestao-fim.
           exit.

       210-navegacao section.
           move "Navegacão Inexistente!" to mensagem
           move "C" TO TIPO-MSG
           perform 140-exibir-mensagem.
       210-navegacao-fim.
           exit.

       300-exibir-tudo section.
           move    "EXIBIR-TUDO" to ds-procedure.
           perform 050-call-dialog-system.
       300-exibir-tudo-fim.
           exit.

       500-incluir section.
      *    open output work
      *    close       work
           open i-o    work.
      *    contagem de contratos
           initialize reg-mtd019 total-etiquetas
           move acp-contrato  to contrato-mt19
           move acp-album-ini to seq-mt19
           start mtd019 key is not less album-mt19 invalid key
               move "10" to st-mtd019.
           perform until st-mtd019 = "10"
               read mtd019 next at end
                   move "10" to st-mtd019
               not at end
                   if acp-contrato <> contrato-mt19
                      move "10" to st-mtd019
                   else
                      if acp-album-fim > 0 and seq-mt19 >
                         acp-album-fim
                         move "10" to st-mtd019
                      else
                         move album-mt19 to album-mtg
                         read mtd020 not invalid key
                              if acp-opcao = 1 or (acp-opcao = 2 and
                                 nao-gerou-album-mtg <> 1)
                                 if fogo-mtg <> 1 and 8
                                    perform verificar-produtos
                                    if pode = "S"
                                       add 1    to total-etiquetas
                                    end-if
                                 end-if
                              end-if
                         end-read
                      end-if
                   end-if
               end-read
           end-perform

           if total-etiquetas = 0
              move "Não Possui nenhum Contrato/Álbum" to mensagem
              move "C" to tipo-msg
              perform 140-exibir-mensagem
           else
      * conta numero de etiquetas 1ª Folha
              initialize pagina1

              if selecao1 = "N"
                 add 1 to pagina1
              end-if

              if selecao2 = "N"
                 add 1 to pagina1
              end-if

              if selecao3 = "N"
                 add 1 to pagina1
              end-if

              if selecao4 = "N"
                 add 1 to pagina1
              end-if

              if selecao5 = "N"
                 add 1 to pagina1
              end-if

              if selecao6 = "N"
                 add 1 to pagina1
              end-if

              if selecao7 = "N"
                 add 1 to pagina1
              end-if

              if selecao8 = "N"
                 add 1 to pagina1
              end-if

              if selecao9 = "N"
                 add 1 to pagina1
              end-if

              if selecao10 = "N"
                 add 1 to pagina1
              end-if

              if selecao11 = "N"
                 add 1 to pagina1
              end-if

              if selecao12 = "N"
                 add 1 to pagina1
              end-if

              if selecao13 = "N"
                 add 1 to pagina1
              end-if

              if selecao14 = "N"
                 add 1 to pagina1
              end-if

              if selecao15 = "N"
                 add 1 to pagina1
              end-if

              if selecao16 = "N"
                 add 1 to pagina1
              end-if

              if selecao17 = "N"
                 add 1 to pagina1
              end-if

              if selecao18 = "N"
                 add 1 to pagina1
              end-if

              if selecao19 = "N"
                 add 1 to pagina1
              end-if

              if selecao20 = "N"
                 add 1 to pagina1
              end-if

      *---------------- joga folha a folha

              initialize reg-mtd019
              move 1 to aux-numero-pagina
              move acp-contrato  to contrato-mt19
              move acp-album-ini to seq-mt19
              start mtd019 key is not less album-mt19 invalid key
                  move "10" to st-mtd019
              end-start
              perform until st-mtd019 = "10"
                  read mtd019 next at end
                      move "10" to st-mtd019
                  not at end
                      if acp-contrato <> contrato-mt19
                         move "10" to st-mtd019
                      else
                         if acp-album-fim > 0 and seq-mt19 >
                            acp-album-fim
                            move "10" to st-mtd019
                         else
                            move album-mt19 to album-mtg
                            read mtd020 not invalid key
                                 if acp-opcao = 1 or (acp-opcao = 2 and
                                    nao-gerou-album-mtg <> 1)
                                    if fogo-mtg <> 1 and 8
                                       perform verificar-produtos
                                       if pode = "S"
                                          add 1 to etiquetas-movidas
                                          perform mover-dados
                                       end-if
                                    end-if
                                 end-if
                            end-read
                         end-if
                      end-if
                  end-read
              end-perform
           end-if


           move spaces to desc-etiqueta1
                          desc-etiqueta2
                          desc-etiqueta3
                          desc-etiqueta4
                          desc-etiqueta5
                          desc-etiqueta6
                          desc-etiqueta7
                          desc-etiqueta8
                          desc-etiqueta9
                          desc-etiqueta10
                          desc-etiqueta11
                          desc-etiqueta12
                          desc-etiqueta13
                          desc-etiqueta14
                          desc-etiqueta15
                          desc-etiqueta16
                          desc-etiqueta17
                          desc-etiqueta18
                          desc-etiqueta19
                          desc-etiqueta20
           close      work
           open input work
           initialize reg-work
           start work key is not less chave-wk invalid key
               move "N" to st-work
           end-start
           perform until st-work = "10"
               read work next at end
                   move "10" to st-work
               not at end
                   if folha-wk <> 1
                      move "10" to st-work
                   else
                      evaluate numero-etiqueta-wk
                        when 1  move desc-etiqueta-wk
                                  to desc-etiqueta1
                        when 2  move desc-etiqueta-wk
                                  to desc-etiqueta2
                        when 3  move desc-etiqueta-wk
                                  to desc-etiqueta3
                        when 4  move desc-etiqueta-wk
                                  to desc-etiqueta4
                        when 5  move desc-etiqueta-wk
                                  to desc-etiqueta5
                        when 6  move desc-etiqueta-wk
                                  to desc-etiqueta6
                        when 7  move desc-etiqueta-wk
                                  to desc-etiqueta7
                        when 8  move desc-etiqueta-wk
                                  to desc-etiqueta8
                        when 9  move desc-etiqueta-wk
                                  to desc-etiqueta9
                        when 10 move desc-etiqueta-wk
                                  to desc-etiqueta10
                        when 11 move desc-etiqueta-wk
                                  to desc-etiqueta11
                        when 12 move desc-etiqueta-wk
                                  to desc-etiqueta12
                        when 13 move desc-etiqueta-wk
                                  to desc-etiqueta13
                        when 14 move desc-etiqueta-wk
                                  to desc-etiqueta14
                        when 15 move desc-etiqueta-wk
                                  to desc-etiqueta15
                        when 16 move desc-etiqueta-wk
                                  to desc-etiqueta16
                        when 17 move desc-etiqueta-wk
                                  to desc-etiqueta17
                        when 18 move desc-etiqueta-wk
                                  to desc-etiqueta18
                        when 19 move desc-etiqueta-wk
                                  to desc-etiqueta19
                        when 20 move desc-etiqueta-wk
                                  to desc-etiqueta20
                      end-evaluate
                   end-if
               end-read
           end-perform
      *    move 1 to numero-pagina
           move 1 to aux-numero-pagina
           close work.
       500-incluir-fim.
           exit.

       verificar-produtos section.
           move "N" to pode

           if dvd = 0 and foto-cd = 0 and book = 0 and poster = 0 and
              estojo = 0
              move "S" to pode
           end-if

           if dvd = 1 and qt-dvd-mtg > 0
              move "S" to pode
           end-if

           if foto-cd = 1 and qt-foto-cd-mtg > 0
              move "S" to pode
           end-if

           if book    = 1 and qt-book-mtg > 0
              move "S" to pode
           end-if

           if poster  = 1 and qt-poster-mtg > 0
              move "S" to pode
           end-if

           if estojo = 1 and qt-estojo-mtg > 0
              move "S" to pode
           end-if.

       verificar-produtos-fim.
           exit.

       mover-dados section.
           perform qtde-copias times
                                 move "N" to ok
            if numero-pagina > 0 and limpar-etiquetas = "S"
      *     if etiquetas-movidas > pagina1
               move "N" to    selecao1
                              selecao2
                              selecao3
                              selecao4
                              selecao5
                              selecao6
                              selecao7
                              selecao8
                              selecao9
                              selecao10
                              selecao11
                              selecao12
                              selecao13
                              selecao14
                              selecao15
                              selecao16
                              selecao17
                              selecao18
                              selecao19
                              selecao20
               move spaces to desc-etiqueta1
                              desc-etiqueta2
                              desc-etiqueta3
                              desc-etiqueta4
                              desc-etiqueta5
                              desc-etiqueta6
                              desc-etiqueta7
                              desc-etiqueta8
                              desc-etiqueta9
                              desc-etiqueta10
                              desc-etiqueta11
                              desc-etiqueta12
                              desc-etiqueta13
                              desc-etiqueta14
                              desc-etiqueta15
                              desc-etiqueta16
                              desc-etiqueta17
                              desc-etiqueta18
                              desc-etiqueta19
                              desc-etiqueta20

            end-if

            if ok = "N" and desc-etiqueta1 = spaces and selecao1 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta1
               move numero-pagina  to folha-wk
               move 1              to numero-etiqueta-wk
               move desc-etiqueta1 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao1
            end-if
            if ok = "N" and desc-etiqueta2 = spaces and selecao2 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta2
               move numero-pagina  to folha-wk
               move 2              to numero-etiqueta-wk
               move desc-etiqueta2 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao2
            end-if
            if ok = "N" and desc-etiqueta3 = spaces and selecao3 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta3
               move numero-pagina  to folha-wk
               move 3              to numero-etiqueta-wk
               move desc-etiqueta3 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao3
            end-if
            if ok = "N" and desc-etiqueta4 = spaces and selecao4 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta4
               move numero-pagina  to folha-wk
               move 4              to numero-etiqueta-wk
               move desc-etiqueta4 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao4
            end-if
            if ok = "N" and desc-etiqueta5 = spaces and selecao5 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta5
               move numero-pagina  to folha-wk
               move 5              to numero-etiqueta-wk
               move desc-etiqueta5 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao5
            end-if
            if ok = "N" and desc-etiqueta6 = spaces and selecao6 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta6
               move numero-pagina  to folha-wk
               move 6              to numero-etiqueta-wk
               move desc-etiqueta6 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao6
            end-if
            if ok = "N" and desc-etiqueta7 = spaces and selecao7 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta7
               move numero-pagina  to folha-wk
               move 7              to numero-etiqueta-wk
               move desc-etiqueta7 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao7
            end-if
            if ok = "N" and desc-etiqueta8 = spaces and selecao8 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta8
               move numero-pagina  to folha-wk
               move 8              to numero-etiqueta-wk
               move desc-etiqueta8 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao8
            end-if
            if ok = "N" and desc-etiqueta9 = spaces and selecao9 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta9
               move numero-pagina  to folha-wk
               move 9              to numero-etiqueta-wk
               move desc-etiqueta9 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao9
            end-if
            if ok = "N" and desc-etiqueta10 = spaces and selecao10 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta10
               move numero-pagina   to folha-wk
               move 10              to numero-etiqueta-wk
               move desc-etiqueta10 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao10
            end-if
            if ok = "N" and desc-etiqueta11 = spaces and selecao11 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta11
               move numero-pagina   to folha-wk
               move 11              to numero-etiqueta-wk
               move desc-etiqueta11 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao11
            end-if
            if ok = "N" and desc-etiqueta12 = spaces and selecao12 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta12
               move numero-pagina   to folha-wk
               move 12              to numero-etiqueta-wk
               move desc-etiqueta12 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao12
            end-if
            if ok = "N" and desc-etiqueta13 = spaces and selecao13 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta13
               move numero-pagina   to folha-wk
               move 13              to numero-etiqueta-wk
               move desc-etiqueta13 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao13
            end-if
            if ok = "N" and desc-etiqueta14 = spaces and selecao14 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta14
               move numero-pagina   to folha-wk
               move 14              to numero-etiqueta-wk
               move desc-etiqueta14 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao14
            end-if
            if ok = "N" and desc-etiqueta15 = spaces and selecao15 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta15
               move numero-pagina   to folha-wk
               move 15              to numero-etiqueta-wk
               move desc-etiqueta15 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao15
            end-if
            if ok = "N" and desc-etiqueta16 = spaces and selecao16 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta16
               move numero-pagina   to folha-wk
               move 16              to numero-etiqueta-wk
               move desc-etiqueta16 to desc-etiqueta-wk
               move "S" to ok
               write reg-work
               move "N" to limpar-etiquetas
                           selecao16
            end-if
            if ok = "N" and desc-etiqueta17 = spaces and selecao17 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta17
               move numero-pagina   to folha-wk
               move 17              to numero-etiqueta-wk
               move desc-etiqueta17 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao17
            end-if
            if ok = "N" and desc-etiqueta18 = spaces and selecao18 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta18
               move numero-pagina   to folha-wk
               move 18              to numero-etiqueta-wk
               move desc-etiqueta18 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao18
            end-if
            if ok = "N" and desc-etiqueta19 = spaces and selecao19 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta19
               move numero-pagina   to folha-wk
               move 19              to numero-etiqueta-wk
               move desc-etiqueta19 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               move "N" to limpar-etiquetas
                           selecao19
            end-if
            if ok = "N" and desc-etiqueta20 = spaces and selecao20 = "N"
               string contrato-mt19 "/" seq-mt19 into desc-etiqueta20
               move numero-pagina   to folha-wk
               move 20              to numero-etiqueta-wk
               move desc-etiqueta20 to desc-etiqueta-wk
               write reg-work
               move "S" to ok
               add  1               to numero-pagina
               move "S" to limpar-etiquetas
                           selecao20
            end-if.
       500-incluir-fim.
           exit.

       700-imprimir-etiqueta section.
           perform 720-abrir-impressora
           if fs-ordserv <> "00"
               move "Erro ao Acessar...ETIQUETA.HTM" to mensagem
               move "C" to tipo-msg
               perform 140-exibir-mensagem
               go to 700-imprimir-etiqueta-fim.

           open input work

           initialize reg-work
           start work key is not less chave-wk invalid key
               move "10" to st-work.
           perform until st-work = "10"
               read work next at end
                   move "10" to st-work
               not at end
                   move lh10 to regrel
                   write regrel

                   move desc-etiqueta-wk      to lh12-p8-texto
                   move desc-etiqueta-wk(1:4) to contrato-mt19
                   move desc-etiqueta-wk(6:4) to seq-mt19
                   read mtd019 invalid key
                        string "Contrato Não Encontrado" X"0DA0"
                               desc-etiqueta1 into mensagem
                        move   "C" to tipo-msg
                        perform 140-exibir-mensagem
                   not invalid key
                        move curso-mt19     to codigo-ie11
                        read ied011 invalid key
                             move "-------" to nome-ie11
                        end-read
                        move nome-ie11(1:25)to lh13-p8-texto
                        move seq-mt19       to lh13-p8-texto2
                        move nome-form-mt19 to lh14-p8-texto
                        move cidade-mt19    to cidade
                        read cad010 invalid key
                             move "--------" to lh15-p8-texto
                             move "--"       to lh15-p8-texto2
                        not invalid key
                             move spaces to lh15-p8-texto
                             move nome-cid to lh15-p8-texto
                             move uf-cid   to lh15-p8-texto2
                        end-read
                   end-read

                   move lh12 to regrel
                   write regrel
                   move lh13 to regrel
                   write regrel
                   move lh14 to regrel
                   write regrel
                   move lh15 to regrel
                   write regrel

                   move lh16 to regrel
                   write regrel

               end-read
           end-perform

           close work

           perform 730-fechar-impressora.

       700-imprimir-etiqueta-fim.
           exit.

       720-abrir-impressora section.
           open output relat

      *    move lh00    to regrel
      *    write regrel
           move lh01    to regrel
           write regrel
           move lh02    to regrel
           write regrel
           move lh03    to regrel
           write regrel
           move lh04    to regrel
           write regrel
           move lh05    to regrel
           write regrel
           move lh06    to regrel
           write regrel
           move lh07    to regrel
           write regrel
           move lh08    to regrel
           write regrel
           move lh09    to regrel
           write regrel
           move lh10    to regrel
           write regrel
           move lh11    to regrel
           write regrel.
       720-abrir-impressora-fim.
           exit.

       730-fechar-impressora section.
           close relat.
       730-fechar-impressora-fim.
           exit.

       800-etiqueta-numerica section.
           open output relat

           move lh01-N    to regrel
           write regrel
           move lh02-N    to regrel
           write regrel
           move lh03-N    to regrel
           write regrel
           move lh04-N    to regrel
           write regrel
           move lh05-N    to regrel
           write regrel
           move lh06-N    to regrel
           write regrel
           move lh07-N    to regrel
           write regrel
           move lh08-N    to regrel
           write regrel
           move lh09-N    to regrel
           write regrel
           move lh10-N    to regrel
           write regrel
           move lh11-N    to regrel
           write regrel.

           open input work

           initialize reg-work
                      controle
           start work key is not less chave-wk invalid key
               move "10" to st-work.
           perform until st-work = "10"
               read work next at end
                   move "10" to st-work
               not at end
                   add 1 to controle

                   evaluate controle
                       when 1 move lh10-N           to regrel
                              write regrel
                              move desc-etiqueta-wk to lh12-n1-p8-texto
                              move "."         to lh12-n1-p8-texto(5:1)
                              move lh12-N1          to regrel
                              write regrel
                       when 2 move desc-etiqueta-wk to lh12-n2-p8-texto
                              move "."         to lh12-n2-p8-texto(5:1)
                              move lh12-N2          to regrel
                              write regrel
                       when 3 move desc-etiqueta-wk to lh12-n3-p8-texto
                              move "."         to lh12-n3-p8-texto(5:1)
                              move lh12-N3          to regrel
                              write regrel
                       when 4 move desc-etiqueta-wk to lh12-n4-p8-texto
                              move "."         to lh12-n4-p8-texto(5:1)
                              move lh12-N4          to regrel
                              write regrel
                              move lh16-N           to regrel
                              write regrel
                              move 0                to controle
                   end-evaluate
               end-read
           end-perform

           if controle <> 0
              move lh16-N                to regrel
              write regrel.

           close work

           close relat.
       800-etiqueta-numerica-fim.
           exit.

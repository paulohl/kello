       copy dslang.cpy.
       identification division.
       program-id.     LOG002.

       security.       Log002.

       environment division.
       class-control.
               AAnimacao           is class "aanimacao"
               Window              is class "wclass"
               FileINI             is class "fileini"
               Alistview           is class "alistview".

       special-names.
           decimal-point is comma
           console is crt.
       file-control.

           copy logx002.

           copy capx002.

           copy cgpx001.

           copy mtpx002.

           copy mtpx020.

           copy repx100.

           copy copx051.

           copy iepx011.

           copy copx002.

       data division.
       file section.

           copy logw002.

           copy capw002.

           copy cgpw001.

           copy mtpw002.

           copy mtpw020.

           copy repw100.

           copy copw051.

           copy iepw011.

           copy copw002.


       working-storage section.
       78 dialog-system        value "DSGRUN".
       01 display-error-no     pic 9(04).
       01 display-details-1    pic 9(04).
       01 display-details-2    pic 9(04).
       01 abriu-gs             pic x(01) value "S".
       01 mensagem             pic x(200).
       01 tipo-msg             pic x(01).
       01 resp-msg             pic x(01).

       01 fs-cad002            pic x(02) value "00".
       01 st-cgd001            pic x(02) value "00".
       01 st-mtd002            pic x(02) value "00".
       01 st-mtd020            pic x(02) value "00".
       01 st-red100            pic x(02) value "00".
       01 st-cod051            pic x(02) value "00".
       01 st-ied011            pic x(02) value "00".
       01 st-cod002            pic x(02) value "00".

       01 aux-reg-nnfentra     pic x(279) value spaces.
       01 aux-reg-nnfentra1    pic x(279) value spaces.

       01 wsTexto              pic x(255).
       01 wsTexto2             pic x(255).
       01 masc-valor           pic zzz.zzz.zz9,99.

       01 wsitemobjeto         object reference.
       01 wsobjetotexto        object reference.
       01 wsobjetotexto2       object reference.
       01 wsNumCol             pic 9(09) comp-5 value zeros.
       01 LINHA                PIC 9(09) VALUE ZEROS.

       01 masc-qtde                    pic zz.zzz.zz9.
       01 masc-qtde92                  pic zz9,99.
       01 auxiliar-data                pic 9(08).
       01 auxiliar-anomes              pic 9(06).

       01 cor.
          05 cor-red                   pic 9(03).
          05 cor-green                 pic 9(03).
          05 cor-blue                  pic 9(03).

       01 corRGB.
          05 corred                    pic 9(02) comp-5 value zeros.
          05 corgreen                  pic 9(02) comp-5 value zeros.
          05 corblue                   pic 9(02) comp-5 value zeros.

       01 AUX-DATA             PIC 9(08).
       01 FILLER REDEFINES AUX-DATA.
          05 AUX-DIA           PIC 9(02).
          05 AUX-MES           PIC 9(02).
          05 AUX-ANO           PIC 9(04).

       01 ws-area-call-ini.
          03 ws-arq-ini        pic x(256).
          03 ws-secao-ini      pic x(080).
          03 ws-chave-ini      pic x(080).
          03 ws-val-ini        pic x(080).
          03 filler            pic x(100).

       01 PRIMEIRO             PIC X(01) VALUE SPACES.
       01 lnktabela.
          02 lnkobjetoscoluna  object reference occurs 99 times.
       01 lnktabelaCol.
          02 lnkcolunas    pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnktabelaA.
          02 lnkobjetoscolunaA object reference occurs 99 times.
       01 lnktabelaColA.
          02 lnkcolunasA   pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnktabela1.
          02 lnkobjetoscoluna1 object reference occurs 99 times.
       01 lnktabelaCol1.
          02 lnkcolunas1   pic 9(09) comp-5 value zeros occurs 99 times.

       01 lnktabela2.
          02 lnkobjetoscoluna2 object reference occurs 99 times.
       01 lnktabelaCol2.
          02 lnkcolunas2   pic 9(09) comp-5 value zeros occurs 99 times.

       01 detRGBFundo.
          03 wsRedFundo    pic 9(03) comp-5 value zeros.
          03 wsGreenFundo  pic 9(03) comp-5 value zeros.
          03 wsBlueFundo   pic 9(03) comp-5 value zeros.

       01 detRGBFonte.
          03 wsRedFonte    pic 9(03) comp-5 value zeros.
          03 wsGreenFonte  pic 9(03) comp-5 value zeros.
          03 wsBlueFonte   pic 9(03) comp-5 value zeros.

       01 detRGBX.
          03 wsRedX        pic x(03).
          03 wsGreenX      pic x(03).
          03 wsBlueX       pic x(03).

       01 WSCOLUNARQUIVO   pic 9(09) comp-5 value zeros.

       01 aUtilitario              object reference value null.
       01 aFont                    object reference value null.
       01 aIcon                    object reference value null.
       01 aImageList2              object reference value null.
       01 wsColuna                 object reference value null.
       01 aObjeto                  object reference value null.
       01 aItem                    object reference value null.
       01 janelaPrincipal          object reference value null.
       01 janelaDetalhes           object reference value null.
       01 lnkColorFundo            object reference value null.
       01 lnkColorFonte            object reference value null.

       01 wsLargura                pic 9(09) comp-5 value zeros.
       01 wsIndice                 pic 9(09) comp-5 value zeros.
       01 wsItem                   pic 9(09) comp-5 value zeros.
       01 wsSize                   pic 9(09) comp-5 value zeros.
       01 handle8                  pic 9(08) comp-x value zeros.
       01 wHandle                  pic 9(09) comp-5 value zeros.
       01 handleIcone              pic 9(09) comp-5 value zeros.
       01 altura                   pic 9(09) comp-5 value zeros.
       01 largura                  pic 9(09) comp-5 value zeros.
       01 wsColunaIndice           pic 9(09) comp-5 value zeros.

       01 masc-numero              pic zzz.zzz.zz9.
       77 indice                   pic 9(09) value zeros.
       77 proximoIndice            pic 9(09) value zeros.
       77 qualColuna               pic 9(09) value zeros.
       77 indice2                  pic 9(09) value zeros.
       01 ws-ok                    pic x(01) value spaces.

       01 masc-parcela             pic z9.
       01 masc-sequencia           pic zzz.zzz.zzz.zzz.zzz.zz9.

       77 CHAVE-SG              PIC X(04).
       77 RETORNO-SG            PIC X(01).
       77 MODULO-SG             PIC X(07).
       01 TEXTO-90-SG           PIC X(90).
       01 EMP-CODIGO-SG         PIC X(04) REDEFINES TEXTO-90-SG.
       78 EMP-CODIGO-PIC        VALUE "#".
       01 texto-60-sg           pic x(60).
       01 usu-id-sg             pic x(15) redefines texto-60-sg.
       78 usu-id-pic            value "#".

       01 ws-fundo.
          02 ws-red-fundo             pic 9(02)  comp-5 value zeros.
          02 ws-blue-fundo            pic 9(02)  comp-5 value zeros.
          02 ws-green-fundo           pic 9(02)  comp-5 value zeros.

       01 ws-fonte.
          02 ws-red-fonte             pic 9(02)  comp-5 value zeros.
          02 ws-blue-fonte            pic 9(02)  comp-5 value zeros.
          02 ws-green-fonte           pic 9(02)  comp-5 value zeros.

       01 ws-data-sys.
          05 ws-data-cpu.
             10 ws-ano-cpu                 pic 9(04).
             10 ws-mes-cpu                 pic 9(02).
             10 ws-dia-cpu                 pic 9(02).
          05 filler                        pic x(13).

       01 ws-datafim           pic 9(08).
       01 filler redefines ws-datafim.
          05 ws-ano-fim        pic 9(04).
          05 ws-mes-fim        pic 9(02).
          05 ws-dia-fim        pic 9(02).


       01 ws-mascara-valor     pic zzz.zzz.zz9,99  blank when zeros.
       01 ws-mascara-sequencia pic zzz.zzz.zz9     blank when zeros.
       01 wsMascaraQuantidade  pic zz.zz9,999      blank when zeros.
       01 wsMascaraPercentual  pic zz9,99          blank when zeros.
       01 masc-margem          pic -zz9,999 value zeros.
       01 masc-desconto        pic zz9,999.

       *>........................................................

       01 ws-indice-coluna-movest.
          02 indice-coluna-movest   pic 9(09) comp-5 value zeros
                                    occurs 99 times.
       *>.............................

       01 ws-colunas-movest.
          02 obj-coluna-movest      object reference occurs 99 times.

       *>.............................

       01 ws-nome-coluna-movest.
          02 NomeColuna-movest      pic x(060) value spaces
                                    occurs 99 times.
       *>........................................................

       01  VARIAVEIS.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  ST-CAD002             PIC XX       VALUE SPACES.
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
           COPY "PARAMETR".

       copy "log002.cpb".
       copy "log002.cpy".
       copy "ds-cntrl.mf".

       01 lnkusu.
          copy usuario.cpy.
       procedure division.
       010-inicio section.
           accept parametros-w from command-line.
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
           move        "LOG002"              to ds-set-name
           move        spaces                to fun-cobol.

           move empresa-w                    to emp-rec
           move "\programa\kello\*"          to lnk-path-sis
           move empresa-w                    to lnk-empresa
           move usuario-w                    to lnk-usuario

           MOVE "LOG002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LOG002.
           MOVE "MTD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD002.
           MOVE "MTD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD020.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "RED100"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-RED100.
           MOVE "COD051"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD051.
           MOVE "IED011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-IED011.
           MOVE "COD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-COD002.

           OPEN INPUT CAD002 LOG002 MTD020 CGD001 MTD002 RED100 COD051
                      IED011 COD002

           IF ST-LOG002 <> "00"
              MOVE SPACES TO MENSAGEM
              STRING "ERRO ABERTURA LOG002: " ST-LOG002 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM.

           IF ST-CAD002 <> "00"
              MOVE SPACES TO MENSAGEM
              STRING "ERRO ABERTURA CAD002: " ST-CAD002 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM.

           IF ST-MTD002 <> "00"
              MOVE SPACES TO MENSAGEM
              STRING "ERRO ABERTURA MTD002: " ST-MTD002 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM.

           IF ST-MTD020 <> "00"
              MOVE SPACES TO MENSAGEM
              STRING "ERRO ABERTURA MTD020: " ST-MTD020 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM.

           IF ST-RED100 <> "00"
              MOVE SPACES TO MENSAGEM
              STRING "ERRO ABERTURA RED100: " ST-RED100 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM.

           IF ST-COD051 <> "00"
              MOVE SPACES TO MENSAGEM
              STRING "ERRO ABERTURA COD051: " ST-COD051 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM.

           IF ST-COD002 <> "00"
              MOVE SPACES TO MENSAGEM
              STRING "ERRO ABERTURA COD002: " ST-COD002 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM.


           IF ST-IED011 <> "00"
              MOVE SPACES TO MENSAGEM
              STRING "ERRO ABERTURA IED011: " ST-IED011 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM.

           IF ST-CGD001 <> "00"
              MOVE SPACES TO MENSAGEM
              STRING "ERRO ABERTURA CGD001: " ST-CGD001 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM.
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

           close log002
                 cad002
                 mtd002
                 mtd020
                 red100
                 cgd001
                 cod051
                 ied011
                 cod002

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
           move spaces to mensagem
           evaluate fun-cobol
              when "A" perform 070-instanciar-Janela
                       perform montar-listview

              when "C" perform 060a-criticar-campo
              when "S" perform sugestao
              when "L" perform carregar-consulta
              when "e" perform tratar-eventos
              when "+" perform evento-alterados

           end-evaluate.
           move spaces to fun-cobol.
       060-processar-cobol-fim.
           exit.

       060a-criticar-campo section.
           evaluate campo-critica
              when "EF-DATA-INI"   PERFORM 060A-CRITICAR-DTINI
              when "EF-DATA-FIM"   PERFORM 060A-CRITICAR-DTFIM
           end-evaluate.
       060a-criticar-campo-fim.
           exit.

       060a-criticar-dtini section.
           if mensagem equal spaces
              if acp-dataini equal zeros
                 move function current-date to ws-data-sys
                 string ws-dia-cpu ws-mes-cpu ws-ano-cpu
                   into acp-dataini
                 refresh-object win1
              else
                 call   "UTIVLDT" using acp-dataini ws-ok
                 cancel "UTIVLDT"
                 if ws-ok equal "N"
                    move "Data Inicial inválida" to mensagem
                    move "C" to tipo-msg
                    perform 140-exibir-mensagem.
       060a-criticar-dtini-fim.
           exit.

       060a-criticar-dtfim section.
           if mensagem equal spaces
              if acp-datafim equal zeros
                 move function current-date to ws-data-sys
                 string ws-dia-cpu ws-mes-cpu ws-ano-cpu
                   into acp-datafim
                 refresh-object win1
              else
                 call   "UTIVLDT" using acp-datafim ws-ok
                 cancel "UTIVLDT"
                 if ws-ok equal "N"
                    move "Data Final inválida" to mensagem
                    move "C" to tipo-msg
                    perform 140-exibir-mensagem.
       060a-criticar-dtfim-fim.
           exit.

       sugestao section.
           evaluate campo-critica
               when "EF-USUARIO"     perform sugestao-usuario
               when other move "Sugestão inexistente" to mensagem
                          move "C" to tipo-msg
                          perform 140-exibir-mensagem.
       sugestao-fim.
           exit.

       sugestao-usuario section.
       sugestao-usuario-fim.
           exit.

       carregar-consulta section.
           invoke acp-listview "DeleteAll"

           if acp-usuario <> spaces
              perform listar-por-usuario
           else
              if acp-arquivo <> spaces
                 perform listar-por-arquivo
              else
                 if acp-operacao <> spaces
                    perform listar-por-operacao
                 else
                    move "Informar pelo menos o nome de um usuário ou um
      -                  " arquivo ou uma operaçã" to mensagem
                    move "C" to tipo-msg
                    perform 140-exibir-mensagem
                 end-if
              end-if
           end-if

           perform mostrar-fonte-favorita
           perform mostrar-colunas-favoritas.
       carregar-consulta-fim.
           exit.

       listar-por-usuario section.
           move zeros to reg-log002
           initialize reg-log002
           move acp-dataini(1:2) to log2-dia
           move acp-dataini(3:2) to log2-mes
           move acp-dataini(5:4) to log2-ano

           move acp-datafim(1:2) to ws-dia-fim
           move acp-datafim(3:2) to ws-mes-fim
           move acp-datafim(5:4) to ws-ano-fim

           move acp-usuario      to log2-usuario
           start log002 key is not less log2-chave invalid key
                 move "10" to st-log002
           end-start
           perform until st-log002 = "10"
                 read log002 next at end
                      move "10" to st-log002
                 not at end
                      if log2-usuario <> acp-usuario or
                         log2-data     > ws-datafim
                         move "10" to st-log002
                      else
                         if acp-arquivo = spaces or log2-arquivo
                            if acp-operacao = spaces or
                               acp-operacao(1:1) = log2-operacao
                               perform mover-dados
                            end-if
                         end-if
                      end-if
                 end-read
           end-perform.
       listar-por-usuario-fim.
           exit.

       listar-por-arquivo section.
           move zeros to reg-log002
           initialize reg-log002
           move acp-dataini(1:2) to log2-dia
           move acp-dataini(3:2) to log2-mes
           move acp-dataini(5:4) to log2-ano

           move acp-datafim(1:2) to ws-dia-fim
           move acp-datafim(3:2) to ws-mes-fim
           move acp-datafim(5:4) to ws-ano-fim

           move acp-arquivo      to log2-arquivo
           start log002 key is not less log2-ch-arquivo invalid key
                 move "10" to st-log002
           end-start
           perform until st-log002 = "10"
                 read log002 next with ignore lock at end
                      move "10" to st-log002
                 not at end
                      if log2-arquivo <> acp-arquivo or
                         log2-data     > ws-datafim
                         move "10" to st-log002
                      else
                         if acp-operacao = spaces or
                            acp-operacao(1:1) = log2-operacao
                            perform mover-dados
                         end-if
                      end-if
                 end-read
           end-perform.
       listar-por-arquivo-fim.
           exit.

       listar-por-operacao section.
           move zeros to reg-log002
           initialize reg-log002
           move acp-dataini(1:2)  to log2-dia
           move acp-dataini(3:2)  to log2-mes
           move acp-dataini(5:4)  to log2-ano

           move acp-datafim(1:2)  to ws-dia-fim
           move acp-datafim(3:2)  to ws-mes-fim
           move acp-datafim(5:4)  to ws-ano-fim

           move acp-operacao(1:1) to log2-operacao
           start log002 key is not less log2-ch-operacao invalid key
                 move "10" to st-log002
           end-start
           perform until st-log002 = "10"
                 read log002 next at end
                      move "10" to st-log002
                 not at end
                      if log2-operacao <> acp-operacao(1:1) or
                         log2-data      > ws-datafim
                         move "10"     to st-log002
                      else
                         perform mover-dados
                      end-if
                 end-read
           end-perform.
       listar-por-operacao-fim.
           exit.

       mover-dados section.
           initialize indice
           invoke acp-listview "adicionarItem" returning wsItem

      *>>>
           add 1 to indice
           initialize wsTexto
           string log2-dia "/" log2-mes "/" log2-ano X"00"
             into wsTexto
           invoke acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

      *>>>
           add 1 to indice
           initialize wsTexto
           string log2-hora     ":"
                  log2-minu     ":"
                  log2-segu     ":"
                  log2-mile     x"00" into wsTexto
           invoke acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

      *>>>
           add 1 to indice
           initialize wsTexto
           string log2-programa x"00" into wsTexto
           invoke acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

      *>>>
           add 1 to indice
           initialize wsTexto
           string log2-arquivo x"00" into wsTexto
           invoke acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

      *>>>
           add 1 to indice
           initialize wsTexto
           evaluate log2-operacao
               when "I"   string "Inclusão"  x"00" into wsTexto
               when "A"   string "Alteração" x"00" into wsTexto
               when "E"   string "Exclusão"  x"00" into wsTexto
               when other move   x"00"               to wsTexto
           end-evaluate
           invoke acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

      *>>>
           add 1 to indice
           initialize wsTexto
           string log2-usuario x"00" into wsTexto
           invoke acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto

      *>>>
           add 1 to indice
           initialize wsTexto
           string log2-registro x"00" into wsTexto
           invoke acp-listview "preencherColunaZ"
                  using wsItem lnkcolunas(indice) wsTexto.
       mover-dados-fim.
           exit.

       evento-alterados section.
           evaluate acp-evento
               when 34123 perform chamar-colunas-favoA
           end-evaluate.
       evento-alterados-fim.
           exit.

       tratar-eventos section.
           evaluate acp-evento
               when 34592 perform item-aceito
               when 34013 perform item-aceito
               when 34123 perform chamar-colunas-favo
           end-evaluate.
       tratar-eventos-fim.
           exit.

       remover-colunas section.
           move proximoIndice to qualColuna
           compute qualColuna = qualColuna - 1

           initialize wsNumCol
           invoke acp-listview-arq "numberOfColumns" returning wsNumCol

           if wsNumCol > qualColuna
              perform until wsNumCol = qualColuna
                 invoke acp-listview-arq "removeColumn" using wsNumCol
                 subtract 1 from wsNumCol
              end-perform
           end-if.

       remover-colunas-fim.
           exit.

       adicionar-colunas section.
           evaluate acp-arquivo
               when "MTD020" perform adicionar-mtd020
               when "RED100" perform adicionar-red100
               when "COD051" perform adicionar-cod051
           end-evaluate
      *>---
      *>---
      *   perform mostrar-fonte-favoA
          perform mostrar-colunas-favoA

          invoke acp-listview-arq "gridLines"
          invoke acp-listview-arq "noBorder".
       adicionar-colunas-fim.
           exit.

       adicionar-cod051 section.
      *>>> Contrato
          move proximoIndice to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Contrato" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Item
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Item"  returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Código Brinde
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Cód Brinde"  returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Curso
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Curso"  returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Turma
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Turma"  returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qtde por Formandos
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
          using z"Qtde por formando" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qtde Formandos
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
          using z"Qtde formandos" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Custo unitário
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
          using z"Custo unitário" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Valor previsto
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
          using z"Valor previsto" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Data Vencimento
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
          using z"Data Vencto" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Data Solicitação
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
          using z"Data Solicitação" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Status
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
          using z"Status" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Valor pago
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
          using z"Valor pago" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Data pago
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
          using z"Data Pgto" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Realizado
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
          using z"Realizado" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Dias prazo
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
          using z"Prazo Médio" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Fornecedor
          add  1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
          using z"Fornecedor" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice).
       adicionar-cod051-fim.
           exit.

       adicionar-red100 section.
      *>>> Contrato
          move proximoIndice to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Documento" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Data Movto
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Data Movto" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> AnoMes
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Ano/Mês" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Lcto Conta
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Lcto C/C" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qtde pessoas
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Qtde pessoas" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice)
      *>>> Qtde veículo
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Qtde veículos" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice)
      *>>> Qtde dias
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Qtde dias" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice)
      *>>> Qtde Formandos
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Qtde formandos" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice)
      *>>> Valor combustível
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"Vlr Combustível" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice)
      *>>> Valor Hospedagem
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Vlr Hospedagem" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice)
      *>>> Valor Refeição
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Vlr Refeição" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice)
      *>>> Valor Passagem
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Vlr Passagem" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice)
      *>>> Valor Aluguel
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Vlr Aluguel" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice)
      *>>> Valor Material
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Vlr Material" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice)
      *>>> Valor Outros
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Vlr Outros" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice)
      *>>> Valor total Reportagem
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
           using z"Total Reportagem" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice)
      *>>> Total despesas
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Total Despesas" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice)
      *>>> Filme
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Filme" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice)
      *>>> Fita
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
             using z"Fita" returning lnkobjetoscolunaA(indice)
          invoke lnkobjetoscolunaA(indice) "RightJustified"
          move indice to lnkcolunasA(indice).
       adicionar-red100-fim.
           exit.

       adicionar-mtd020 section.
      *>>> Contrato
          move proximoIndice to indice
          invoke acp-listview-arq "adicionarColunaZ"
                 using z"Contrato" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Nr Álbum
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
                 using z"Nº Álbum" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Data movimento
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
                 using z"DT Movto" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qt Estojo
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
                 using z"QT Estojo" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qt Encadernação
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"QT Encadernação" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qt Folhas
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"QT Folhas" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qt Fotos
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"QT Fotos" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qt Fitas
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"QT Fitas" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qt Poster
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"QT Poster" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qt Porta Fita
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"QT Porta Fita" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qt Foto CD
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"QT Foto CD" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qt Moldura
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"QT Moldura" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qt Porta DVD
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"QT Porta DVD" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Fogo
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"Fogo" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Data Fogo
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"Data Fogo" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Ano/Mês
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"Ano/Mês" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Vista
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"QT Visita" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Posse
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"Posse" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Quem
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"Quem" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qt DVD
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"QT DVD" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Não Gerou Álbum
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"Ñ Gerou Álbum" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Data romaneio
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"Data Romaneio" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>>> Qt Book
          add 1  to indice
          invoke acp-listview-arq "adicionarColunaZ"
            using z"QT Book" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice).
       adicionar-mtd020-fim.
           exit.


       item-aceito section.
           if acp-arquivo <> spaces
              PERFORM REMOVER-COLUNAS
              PERFORM ADICIONAR-COLUNAS

              INVOKE ACP-LISTVIEW-ARQ   "DeleteAll"

              MOVE "S"                      TO PRIMEIRO
              INITIALIZE REG-LOG002
                         LINHA
              MOVE ACP-ARQUIVO              TO LOG2-ARQUIVO
              MOVE ACP-DATAINI(1:2)         TO LOG2-DIA
              MOVE ACP-DATAINI(3:2)         TO LOG2-MES
              MOVE ACP-DATAINI(5:4)         TO LOG2-ANO
              START LOG002 KEY IS NOT LESS LOG2-CH-ARQUIVO
                                                          INVALID KEY
                    MOVE "10" TO st-log002
              END-START
              PERFORM UNTIL st-log002 = "10"
                    READ LOG002 NEXT AT END
                         MOVE "10" TO st-log002
                    NOT AT END
                         IF ACP-ARQUIVO <> LOG2-ARQUIVO OR
                            LOG2-DATA    > WS-DATAFIM
                            MOVE "10" TO st-log002
                         ELSE
      *                  display "reg-log002 = " reg-log002
      *                  stop " "
                            ADD 1 TO LINHA
                            PERFORM MOVER-REGISTRO
                         END-IF
                    END-READ
              END-PERFORM

      *       perform mostrar-fonte-favoA
              perform mostrar-colunas-favoA

              show-window win2
              refresh-object win2
           ELSE
              MOVE "Essa função somente quando informado um arquivo"
                TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM 140-EXIBIR-MENSAGEM.
       item-aceito-fim.
           exit.

       MOVER-REGISTRO SECTION.
           initialize indice
           invoke acp-listview-arq "adicionarItem" returning wsItem

      *>>>
           add 1 to indice
           initialize wsTexto
           string linha X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto

      *>>> Data
           add 1 to indice
           initialize wsTexto
           string log2-dia "/" log2-mes "/" log2-ano X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto

      *>>> Horario
           add 1 to indice
           initialize wsTexto
           string log2-hora ":" log2-minu ":" log2-segu ":" log2-mile
           X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto

      *>>> Operação
           add 1 to indice
           initialize wsTexto
           evaluate log2-operacao
               when "I" string "Inclusão" x"00" into wsTexto
               when "A" string "Alteração" x"00" into wsTexto
               when "E" string "Exclusão" x"00" into wsTexto
               when other string x"00" into wsTexto
           end-evaluate
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto

      *>>> Usuário
           add 1 to indice
           initialize wsTexto
           string log2-usuario X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto

           EVALUATE ACP-ARQUIVO
               WHEN "MTD020" MOVE LOG2-REGISTRO TO REG-MTD020
                             PERFORM INCLUIR-MTD020
               WHEN "RED100" MOVE LOG2-REGISTRO TO REG-RED100
                             PERFORM INCLUIR-RED100
               WHEN "COD051" MOVE LOG2-REGISTRO TO REG-COD051
                             PERFORM INCLUIR-COD051
           END-EVALUATE.
       MOVER-REGISTRO-FIM.
           EXIT.

       INCLUIR-MTD020 SECTION.
      *>>> Contrato
           add 1 to indice
           initialize wsTexto
           string contrato-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Nr Álbum
           add 1 to indice
           initialize wsTexto
           string nralbum-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Data Movto
           add 1 to indice
           initialize wsTexto
           string datamov-mtg(7:2) "/"
                  datamov-mtg(5:2)"/"
                  datamov-mtg(1:4) X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Qtde Estojo
           add 1 to indice
           initialize wsTexto
           string qt-estojo-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Qtde Encadernação
           add 1 to indice
           initialize wsTexto
           string qt-encader-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Folhas montagem
           add 1 to indice
           initialize wsTexto
           string qt-folhas-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Fotos montagem
           add 1 to indice
           initialize wsTexto
           string qt-fotos-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Fitas montagem
           add 1 to indice
           initialize wsTexto
           string qt-fitas-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Postar montagem
           add 1 to indice
           initialize wsTexto
           string qt-poster-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Porta fita
           add 1 to indice
           initialize wsTexto
           string qt-porta-fita-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Foto CD
           add 1 to indice
           initialize wsTexto
           string qt-foto-cd-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Moldura
           add 1 to indice
           initialize wsTexto
           string qt-moldura-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Porta DVD
           add 1 to indice
           initialize wsTexto
           string qt-porta-dvd-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Fogo MTG
           add 1 to indice
           initialize wsTexto
           evaluate fogo-mtg
               when 0 string "0-Montagem"  x"00" into wsTexto
               when 1 string "1-Vendido"   x"00" into wsTexto
               when 8 string "8-Vend-Fogo" x"00" into wsTexto
               when 9 string "9-Fogo"      x"00" into wsTexto
               when other string fogo-mtg "-****" x"00" into wsTexto
           end-evaluate
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Data Fogo
           add 1 to indice
           initialize wsTexto
           if data-fogo-mtg > 0
              string data-fogo-mtg(7:2) "/"
                     data-fogo-mtg(5:2) "/"
                     data-fogo-mtg(1:4) x"00" into wsTexto
           else
              string X"00" into wsTexto
           end-if
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Ano/Mes visita
           add 1 to indice
           initialize wsTexto
           string anomes-visita-mtg(5:2) "/"
                  anomes-visita-mtg(1:4) X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> visita
           add 1 to indice
           initialize wsTexto
           string visita-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Posse MTG
           add 1 to indice
           initialize wsTexto
           evaluate posse-mtg
               when 1 string "1-Em estoque"   x"00" into wsTexto
               when 2 string "2-Com vendedor" x"00" into wsTexto
               when 3 string "3-Montagem"     x"00" into wsTexto
               when 4 string "4-Revendido"    x"00" into wsTexto
               when other string x"00" into wsTexto
           end-evaluate
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Posse MTG
           add 1 to indice
           initialize wsTexto
           evaluate posse-mtg
               when 1 move codigo-posse-mtg  to codigo-mt02
                      read mtd002 invalid key
                           move spaces       to nome-mt02
                      end-read
                      string nome-mt02 x"00" delimited by "   "
                        into wsTexto
               when 2 move codigo-posse-mtg           to codigo-cg01
                      read cgd001 invalid key
                           move spaces to nome-cg01
                      end-read
                      string nome-cg01 x"00" delimited by "   "
                        into wsTexto
               when 3 string x"00" into wsTexto
               when 4 string x"00" into wsTexto
               when other string x"00" into wsTexto
           end-evaluate
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Qtde DVD
           add 1 to indice
           initialize wsTexto
           string qt-dvd-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Não gerou album
           add 1 to indice
           initialize wsTexto
           string nao-gerou-album-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Data romaneio
           add 1 to indice
           initialize wsTexto
           string dataromaneio-mtg(7:2) "/"
                  dataromaneio-mtg(5:2) "/"
                  dataromaneio-mtg(1:4) X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Qtde book
           add 1 to indice
           initialize wsTexto
           string qt-book-mtg X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto.
       INCLUIR-MTD020-FIM.
           EXIT.

       INCLUIR-RED100 SECTION.
      *>>> número documento
           add 1 to indice
           initialize wsTexto
           string docto-r100 X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> data movto
           add 1 to indice
           initialize wsTexto
           move data-mov-r100      to auxiliar-data
           string auxiliar-data(7:2) "/"
                  auxiliar-data(5:2) "/"
                  auxiliar-data(1:4) X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Ano/mes
           add 1 to indice
           initialize wsTexto
           move anomes-r100          to auxiliar-anomes
           string auxiliar-anomes(1:4) "/"
                  auxiliar-anomes(5:2) X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Lcto conta corrente
           add 1 to indice
           initialize wsTexto
           evaluate lcto-cta-corr-r100
               when 1      string "Sim" X"00" into wsTexto
               when other  string "Não" X"00" into wsTexto
           end-evaluate
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Qtas pessoas
           add 1 to indice
           initialize wsTexto
           move qtde-pessoas-r100    to masc-qtde
           string masc-qtde X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Qtos veículos
           add 1 to indice
           initialize wsTexto
           move qtde-veiculos-r100  to masc-qtde
           string masc-qtde X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Qtas dias
           add 1 to indice
           initialize wsTexto
           move qtde-dias-r100       to masc-qtde92
           string masc-qtde92 X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Qtos formandos
           add 1 to indice
           initialize wsTexto
           move qtde-form-r100       to masc-qtde
           string masc-qtde X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Valor combustível
           add 1 to indice
           initialize wsTexto
           move vlr-comb-r100        to masc-valor
           string masc-valor X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Valor hospedagem
           add 1 to indice
           initialize wsTexto
           move vlr-hosp-r100        to masc-valor
           string masc-valor X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Valor refeição
           add 1 to indice
           initialize wsTexto
           move vlr-refeicao-r100    to masc-valor
           string masc-valor X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Valor passagem
           add 1 to indice
           initialize wsTexto
           move vlr-passagem-r100    to masc-valor
           string masc-valor X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Valor aluguel
           add 1 to indice
           initialize wsTexto
           move vlr-aluguel-r100     to masc-valor
           string masc-valor X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Valor material
           add 1 to indice
           initialize wsTexto
           move vlr-mat-r100         to masc-valor
           string masc-valor X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Valor outros
           add 1 to indice
           initialize wsTexto
           move vlr-outros-r100      to masc-valor
           string masc-valor X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Valor total reportagem
           add 1 to indice
           initialize wsTexto
           move vlr-tot-report-r100  to masc-valor
           string masc-valor X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Valor despesas reportagem
           add 1 to indice
           initialize wsTexto
           move vlr-despesa-report-r100 to masc-valor
           string masc-valor X"00"    into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Filme
           add 1 to indice
           initialize wsTexto
           move tot-filme-report-r100 to masc-qtde
           string masc-qtde X"00"   into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Fita
           add 1 to indice
           initialize wsTexto
           move tot-fita-report-r100 to masc-qtde
           string masc-qtde  X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto.
       INCLUIR-RED100-FIM.
           EXIT.

       INCLUIR-COD051 SECTION.
      *>>> Nº Contrato
           add 1 to indice
           initialize wsTexto
           string NR-CONTRATO-CO51  X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Item
           add 1 to indice
           initialize wsTexto
           string ITEM-CO51  X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Brinde
           add 1 to indice
           initialize wsTexto
           MOVE CODBRINDE-CO51 TO CODIGO-CO02
           READ COD002 INVALID KEY
                MOVE "*******" TO NOME-CO02
           END-READ
           string NOME-CO02 X"00" delimited by "  " into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto

      *>>> Curso
           add 1 to indice
           initialize wsTexto
           MOVE CURSO-CO51     TO CODIGO-IE11
           READ IED011 INVALID KEY
                MOVE "*******" TO NOME-IE11
           END-READ
           string NOME-IE11 X"00" delimited by "  " into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Turma
           add 1 to indice
           initialize wsTexto
           string TURMA-CO51 X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Qtde por formandos
           add 1 to indice
           initialize wsTexto
           MOVE QTDE-POR-FORM-CO51   to masc-qtde
           string masc-qtde X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Qtde de formandos
           add 1 to indice
           initialize wsTexto
           MOVE QTDE-FORM-CO51   to masc-qtde
           string masc-qtde X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Custo unitário
           add 1 to indice
           initialize wsTexto
           MOVE CUSTO-UNIT-CO51     to masc-valor
           string masc-valor X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Valor previsto
           add 1 to indice
           initialize wsTexto
           MOVE VALOR-PREVISTO-CO51 to masc-valor
           string masc-valor X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Data vencimento
           add 1 to indice
           initialize wsTexto
           string data-vencto-co51(7:2) "/"
                  data-vencto-co51(5:2) "/"
                  data-vencto-co51(1:4) X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Data solicitação
           add 1 to indice
           initialize wsTexto
           MOVE DATA-SOLICIT-CO51   to auxiliar-data
           string auxiliar-data(7:2) "/"
                  auxiliar-data(5:2) "/"
                  auxiliar-data(1:4) X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Status
           add 1 to indice
           initialize wsTexto
           evaluate susp-prev-def-co51
               when 0     string "Previsto"   x"00" into wsTexto
               when 1     string "Definitivo" x"00" into wsTexto
               when 2     string "Suspenso"   x"00" into wsTexto
               when other string x"00"              into wsTexto
           end-evaluate
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Valor pago
           add 1 to indice
           initialize wsTexto
           move valor-pago-co51      to masc-valor
           string masc-valor x"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Data Pagto
           add 1 to indice
           initialize wsTexto
           MOVE DATA-PAGTO-CO51      to auxiliar-data
           string auxiliar-data(7:2) "/"
                  auxiliar-data(5:2) "/"
                  auxiliar-data(1:4) X"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Realizado
           add 1 to indice
           initialize wsTexto
           evaluate realizado-co51
               when 0     string "Não"        x"00" into wsTexto
               when 1     string "Sim"        x"00" into wsTexto
               when other string x"00"              into wsTexto
           end-evaluate
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Prazo médio
           add 1 to indice
           initialize wsTexto
           move dias-prazo-co51      to masc-qtde
           string masc-qtde x"00" into wsTexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto
      *>>> Fornecedor
           add 1 to indice
           initialize wsTexto
           move cod-fornec-co51      to codigo-cg01
           read cgd001 invalid key
                initialize reg-cgd001
           end-read
           string  nome-cg01 x"00" delimited by "  " into wstexto
           invoke acp-listview-arq "preencherColunaZ"
                  using wsItem lnkcolunasA(indice) wsTexto.
       INCLUIR-COD051-FIM.
           EXIT.

       070-instanciar-Janela section.
           move-object-handle win1 handle8
           move handle8 to wHandle
           invoke Window "fromHandleWithClass" using wHandle Window
                  returning janelaPrincipal.
           invoke janelaPrincipal "centralizarNoDesktop".

           move-object-handle win2 handle8
           move handle8 to wHandle
           invoke Window "fromHandleWithClass" using wHandle Window
                  returning janelaPrincipal.
           invoke janelaPrincipal "centralizarNoDesktop".
       070-instanciar-Janela-fim.
           exit.

       montar-listview section.
           perform criar-listview-principal
           perform criar-listview-arq.
       montar-listview-fim.
           exit.

       criar-listview-principal section.
          initialize indice
          add 1 to indice
          invoke acp-listview "adicionarColunaZ"
                 using z"Data" returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "centered"
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke acp-listview "adicionarColunaZ"
                 using z"Horas"  returning lnkobjetoscoluna(indice)
          invoke lnkobjetoscoluna(indice) "centered"
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke acp-listview "adicionarColunaZ"
                 using z"Programa"  returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke acp-listview "adicionarColunaZ"
             using z"Arquivo"  returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke acp-listview "adicionarColunaZ"
                 using z"Operação"  returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke acp-listview "adicionarColunaZ"
                 using z"Usuário"  returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

      *>---
      *>---
          add 1 to indice
          invoke acp-listview "adicionarColunaZ"
                 using z"Registro"  returning lnkobjetoscoluna(indice)
          move indice to lnkcolunas(indice)

          perform mostrar-fonte-favorita
          perform mostrar-colunas-favoritas

          invoke acp-listview "gridLines"
          invoke acp-listview "noBorder".
       criar-listview-principal-fim.
           exit.

       criar-listview-arq section.
          initialize indice
          add 1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
                 using z"Linha" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)

      *>---
      *>---
          add 1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
                 using z"Data Log" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>---
      *>---
          add 1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
                 using z"Hora Log" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)

      *>---
      *>---
          add 1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
                 using z"Operação" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)
      *>---
      *>---
          add 1 to indice
          invoke acp-listview-arq "adicionarColunaZ"
                 using z"Usuário" returning lnkobjetoscolunaA(indice)
          move indice to lnkcolunasA(indice)

      *>---
      *>---

          add 1 to indice

          move indice to proximoIndice

          perform mostrar-fonte-favoA
          perform mostrar-colunas-favoA

          invoke acp-listview-arq "gridLines"
          invoke acp-listview-arq "noBorder".
       criar-listview-arq-fim.
           exit.

       140-exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
       140-exibir-mensagem-fim.
           exit.

       mostrar-colunas-favoritas section.
          initialize wsTexto
          move "listview-log002" to wsTexto
          invoke AListview "SetarTamanhoColunas"
                            using lnkusu
                                  acp-listview
                                  wsTexto
                                  lnktabela.
       mostrar-colunas-favoritas-fim.
           exit.

       mostrar-fonte-favorita section.
           move "listview-log002" to wsTexto
           invoke aListview "criarFonte"
                             using lnkusu acp-listview wsTexto.
       mostrar-fonte-favorita-fim.
           exit.

       zebrar-itens section.
           move "listview-log002" to wsTexto
           invoke aListview "zebrarCor"
                             using lnkusu acp-listview wsTexto
           invoke acp-listview "redrawallitems".
       zebrar-itens-fim.
           exit.

       chamar-colunas-favo section.
           move "listview-log002" to wsTexto
           call "COLFAV" using lnkusu
                               acp-listview
                               wsTexto
                               lnktabela

           perform mostrar-colunas-favoritas
           perform mostrar-fonte-favorita
           perform zebrar-itens.
       chamar-colunas-favo-fim.
           exit.

       mostrar-colunas-favoA section.
          initialize wsTexto
          move "listview-log002A" to wsTexto
          invoke AListview "SetarTamanhoTodasColunas"
                            using lnkusu
                                  acp-listview-arq
                                  wsTexto
                                  lnktabelaA.
       mostrar-colunas-favoA-fim.
           exit.

       mostrar-fonte-favoA section.
           move "listview-log002A" to wsTexto
           invoke aListview "criarFonte"
                             using lnkusu acp-listview-arq wstexto.
       mostrar-fonte-favoA-fim.
           exit.

       zebrar-itensA section.
           move "listview-log002A" to wsTexto
           invoke aListview "zebrarCor"
                             using lnkusu acp-listview-arq wsTexto
           invoke acp-listview-arq "redrawallitems".
       zebrar-itensA-fim.
           exit.

       chamar-colunas-favoA section.
           move "listview-log002A" to wsTexto
           call "COLFAV" using lnkusu
                               acp-listview-arq
                               wsTexto
                               lnktabelaA

           perform mostrar-colunas-favoA
           perform mostrar-fonte-favoA
           perform zebrar-itensA.
       chamar-colunas-favoA-fim.
           exit.


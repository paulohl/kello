       identification division.
       program-id.     mtp062.
       author.         Alfredo Saviolli.
       date-written.   27-06-2004.

       security.       Impressão das Etiquetas dos Produtos.

       environment division.
       special-names.
           decimal-point is comma
           console is crt.
       file-control.

           copy capx001.

           copy capx010.

           copy mtpx019.

           copy mtpx020.

           copy iepx011.

           select   ordserv assign       to arquivo-ordserv
                            organization is line sequential
                            access mode  is      sequential
                            file  status is      fs-ordserv.

           select   work    assign       to    arquivo-work
                            organization is         indexed
                            access mode  is         dynamic
                            record key   is        chave-wk
                            file status  is         st-work.

       data division.
       file section.

           copy capw001.

           copy capw010.

           copy mtpw019.

           copy mtpw020.

           copy iepw011.

       fd ordserv.
       01 reg-ordserv                          pic x(450).

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
       01 ok                  pic x(01).
       01 masc-qtde           pic Z.ZZ9.
       01 masc-vlr            pic Z.ZZ9,99 BLANK WHEN ZEROS.
       01 LNK-DETALHE         PIC X(200).
       01  VARIAVEIS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).

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

      *copy "etiqueta2.rel".

       copy "html.rel".

       copy "mtp062.cpb".

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
           move        "MTP062"              to ds-set-name
           move        spaces                to fun-cobol.

           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA               TO EMP-REC
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

           move "c:\etiqueta" to arquivo-work.

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
               when "C" perform 110-criticar
               when "S" perform 200-sugestao
               when "N" perform 210-navegacao
               when "I" perform 500-incluir
               when "P" perform 700-imprimir-etiqueta
                        move spaces to arq-explorer
                        STRING "C:\Arquiv~1\Intern~1\IEXPLORE.EXE "
                     "file:///C:/ETIQUETAS.HTM"
                     INTO ARQ-EXPLORER
                 move "CHAMAR-EXPLORER" to ds-procedure
                 perform 050-call-dialog-system
           end-evaluate.
           move spaces to fun-cobol.
       060-processar-cobol-fim.
           exit.

       110-criticar section.
           move    spaces to mensagem.
           evaluate    campo-critica
               when "EF-CONTRATO"      perform 111-criticar-contrato
               when "EF-ALBUM-INI"     perform 112-criticar-album-ini
               when "EF-ALBUM-FIM"     perform 113-criticar-album-fim
           end-evaluate.
       110-criticar-fim.
           exit.

       111-criticar-contrato section.
           if mensagem equal spaces
               if acp-contrato equal zeros
                   move "Contrato Não Informado" to mensagem
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
           open output work
           close       work
           open i-o    work
           initialize reg-mtd019
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
                         if acp-opcao = 1
                            move "N" to ok
                            perform mover-dados
                         else
                            move album-mt19 to album-mtg
                            read mtd020 not invalid key
                                 if fogo-mtg <> 1 and 8
                                    move "N" to ok
                                    perform mover-dados
                                 end-if
                            end-read
                         end-if
                      end-if
                   end-if
               end-read
           end-perform
           close work.
       500-incluir-fim.
           exit.

       mover-dados section.
           if ok = "N" and desc-etiqueta1 = spaces and selecao1 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta1
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta2 = spaces and selecao2 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta2
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta3 = spaces and selecao3 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta3
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta4 = spaces and selecao4 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta4
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta5 = spaces and selecao5 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta5
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta6 = spaces and selecao6 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta6
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta7 = spaces and selecao7 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta7
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta8 = spaces and selecao8 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta8
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta9 = spaces and selecao9 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta9
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta10 = spaces and selecao10 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta10
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta11 = spaces and selecao11 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta11
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta12 = spaces and selecao12 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta12
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta13 = spaces and selecao13 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta13
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta14 = spaces and selecao14 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta14
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta15 = spaces and selecao15 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta15
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta16 = spaces and selecao16 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta16
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta17 = spaces and selecao17 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta17
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta18 = spaces and selecao18 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta18
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta19 = spaces and selecao19 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta19
              move "S" to ok
           end-if
           if ok = "N" and desc-etiqueta20 = spaces and selecao20 = "N"
              string contrato-mt19 "/" seq-mt19 into desc-etiqueta20
              move "S" to ok
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

           initialize
                      campo-017   campo-018  campo-019
                      campo-020   campo-021  campo-022
                      campo-023   campo-024  campo-025
                      campo-026   campo-027  campo-028
                      campo-029   campo-030  campo-031
                      campo-032   campo-033  campo-034
                      campo-034   campo-035  campo-036
                      campo-037   campo-038  campo-039
                      campo-040   campo-041  campo-042
                      campo-043   campo-044  campo-045
                      campo-046   campo-047  campo-048
                      campo-049   campo-050  campo-051
                      campo-052   campo-053  campo-054
                      campo-055   campo-056  campo-057
                      campo-058   campo-059  campo-060
                      campo-061   campo-062  campo-063
                      campo-064   campo-065  campo-066
                      campo-067   campo-068  campo-069
                      campo-070   campo-071  campo-072
                      campo-073   campo-074  campo-075
                      campo-076   campo-077  campo-078
                      campo-079   campo-080  campo-081
                      campo-082   campo-083  campo-084
                      campo-085   campo-086  campo-087
                      campo-088   campo-089  campo-090
                      campo-091   campo-092  campo-093
                      campo-094   campo-095  campo-096
                      campo-097   campo-098  campo-099
                      campo-100   campo-101  campo-102
                      campo-103   campo-104  campo-105
                      campo-106   campo-107  campo-108
                      campo-109   campo-110  campo-111
                      campo-112   campo-113  campo-114
                      campo-115   campo-116  campo-117
                      campo-118   campo-119  campo-120
                      campo-121   campo-122  campo-123
                      campo-124   campo-125  campo-126
                      campo-127   campo-128  campo-129
                      campo-130   campo-131  campo-132
                      campo-133   campo-134  campo-135
                      campo-136   campo-137  campo-138
                      campo-139

      *-----------------------------------------------------------------
      *                           1ª LINHA
      *-----------------------------------------------------------------
           move desc-etiqueta1      to campo-017
           move desc-etiqueta1(1:4) to contrato-mt19
           move desc-etiqueta1(6:4) to seq-mt19
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
                move nome-ie11      to campo-019
                move nome-form-mt19 to campo-023
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-025
                     move "--"       to campo-026
                not invalid key
                     move nome-cid   to campo-025
                     move uf-cid     to campo-026
                end-read
           end-read
           MOVE DESC-ETIQUETA1(6:4) to campo-020

           move desc-etiqueta2      to campo-018
           move desc-etiqueta2(1:4) to contrato-mt19
           move desc-etiqueta2(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta2 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-021
                move nome-form-mt19 to campo-024
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-027
                     move "--"       to campo-028
                not invalid key
                     move nome-cid   to campo-027
                     move uf-cid     to campo-028
                end-read
           end-read
           MOVE DESC-ETIQUETA2(6:4) TO campo-022

      *-----------------------------------------------------------------
      *                           2ª LINHA
      *-----------------------------------------------------------------

           move desc-etiqueta3      to campo-030
           move desc-etiqueta3(1:4) to contrato-mt19
           move desc-etiqueta3(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta3 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-032
                move nome-form-mt19 to campo-036
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-038
                     move "--"       to campo-039
                not invalid key
                     move nome-cid   to campo-038
                     move uf-cid     to campo-039
                end-read
           end-read
           MOVE DESC-ETIQUETA3(6:4) TO campo-033

           move desc-etiqueta4      to campo-031
           move desc-etiqueta4(1:4) to contrato-mt19
           move desc-etiqueta4(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta4 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-034
                move nome-form-mt19 to campo-037
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-040
                     move "--"       to campo-041
                not invalid key
                     move nome-cid   to campo-040
                     move uf-cid     to campo-041
                end-read
           end-read
           MOVE DESC-ETIQUETA4(6:4) TO campo-035

      *-----------------------------------------------------------------
      *                           3ª LINHA
      *-----------------------------------------------------------------

           move desc-etiqueta5      to campo-043
           move desc-etiqueta5(1:4) to contrato-mt19
           move desc-etiqueta5(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta5 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-045
                move nome-form-mt19 to campo-049
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-051
                     move "--"       to campo-052
                not invalid key
                     move nome-cid   to campo-051
                     move uf-cid     to campo-052
                end-read
           end-read
           MOVE DESC-ETIQUETA5(6:4) TO campo-046

           move desc-etiqueta6      to campo-044
           move desc-etiqueta6(1:4) to contrato-mt19
           move desc-etiqueta6(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta6 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-047
                move nome-form-mt19 to campo-050
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-053
                     move "--"       to campo-054
                not invalid key
                     move nome-cid   to campo-053
                     move uf-cid     to campo-054
                end-read
           end-read
           MOVE DESC-ETIQUETA6(6:4) TO campo-048

      *-----------------------------------------------------------------
      *                           4ª LINHA
      *-----------------------------------------------------------------

           move desc-etiqueta7      to campo-056
           move desc-etiqueta7(1:4) to contrato-mt19
           move desc-etiqueta7(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta7 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-058
                move nome-form-mt19 to campo-062
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-064
                     move "--"       to campo-065
                not invalid key
                     move nome-cid   to campo-064
                     move uf-cid     to campo-065
                end-read
           end-read
           MOVE DESC-ETIQUETA7(6:4) TO campo-059

           move desc-etiqueta8      to campo-057
           move desc-etiqueta8(1:4) to contrato-mt19
           move desc-etiqueta8(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta8 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-060
                move nome-form-mt19 to campo-063
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-066
                     move "--"       to campo-067
                not invalid key
                     move nome-cid   to campo-066
                     move uf-cid     to campo-067
                end-read
           end-read
           MOVE DESC-ETIQUETA8(6:4) TO campo-061

      *-----------------------------------------------------------------
      *                           5ª LINHA
      *-----------------------------------------------------------------

           move desc-etiqueta9      to campo-068
           move desc-etiqueta9(1:4) to contrato-mt19
           move desc-etiqueta9(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta9 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-070
                move nome-form-mt19 to campo-074
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-076
                     move "--"       to campo-077
                not invalid key
                     move nome-cid   to campo-076
                     move uf-cid     to campo-077
                end-read
           end-read
           MOVE DESC-ETIQUETA9(6:4) TO campo-071

           move desc-etiqueta10      to campo-069
           move desc-etiqueta10(1:4) to contrato-mt19
           move desc-etiqueta10(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta10 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-072
                move nome-form-mt19 to campo-075
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-078
                     move "--"       to campo-079
                not invalid key
                     move nome-cid   to campo-078
                     move uf-cid     to campo-079
                end-read
           end-read
           MOVE DESC-ETIQUETA10(6:4) TO campo-073

      *-----------------------------------------------------------------
      *                           6ª LINHA
      *-----------------------------------------------------------------

           move desc-etiqueta11      to campo-080
           move desc-etiqueta11(1:4) to contrato-mt19
           move desc-etiqueta11(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta11 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-082
                move nome-form-mt19 to campo-086
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-088
                     move "--"       to campo-089
                not invalid key
                     move nome-cid   to campo-088
                     move uf-cid     to campo-089
                end-read
           end-read
           MOVE DESC-ETIQUETA11(6:4) TO campo-083

           move desc-etiqueta12      to campo-081
           move desc-etiqueta12(1:4) to contrato-mt19
           move desc-etiqueta12(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta12 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-084
                move nome-form-mt19 to campo-087
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-090
                     move "--"       to campo-091
                not invalid key
                     move nome-cid   to campo-090
                     move uf-cid     to campo-091
                end-read
           end-read
           MOVE DESC-ETIQUETA12(6:4) TO campo-085

      *-----------------------------------------------------------------
      *                           7ª LINHA
      *-----------------------------------------------------------------

           move desc-etiqueta13      to campo-092
           move desc-etiqueta13(1:4) to contrato-mt19
           move desc-etiqueta13(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta13 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-094
                move nome-form-mt19 to campo-098
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-100
                     move "--"       to campo-101
                not invalid key
                     move nome-cid   to campo-100
                     move uf-cid     to campo-101
                end-read
           end-read
           MOVE DESC-ETIQUETA13(6:4) TO campo-095

           move desc-etiqueta14      to campo-093
           move desc-etiqueta14(1:4) to contrato-mt19
           move desc-etiqueta14(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta14 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-096
                move nome-form-mt19 to campo-099
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-102
                     move "--"       to campo-103
                not invalid key
                     move nome-cid   to campo-102
                     move uf-cid     to campo-103
                end-read
           end-read
           MOVE DESC-ETIQUETA14(6:4) TO campo-097

      *-----------------------------------------------------------------
      *                           8ª LINHA
      *-----------------------------------------------------------------

           move desc-etiqueta15      to campo-104
           move desc-etiqueta15(1:4) to contrato-mt19
           move desc-etiqueta15(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta15 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-106
                move nome-form-mt19 to campo-110
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-112
                     move "--"       to campo-113
                not invalid key
                     move nome-cid   to campo-112
                     move uf-cid     to campo-113
                end-read
           end-read
           MOVE DESC-ETIQUETA15(6:4) TO campo-107

           move desc-etiqueta16      to campo-105
           move desc-etiqueta16(1:4) to contrato-mt19
           move desc-etiqueta16(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta16 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-108
                move nome-form-mt19 to campo-111
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-114
                     move "--"       to campo-115
                not invalid key
                     move nome-cid   to campo-114
                     move uf-cid     to campo-115
                end-read
           end-read
           MOVE DESC-ETIQUETA16(6:4) TO campo-109

      *-----------------------------------------------------------------
      *                           9ª LINHA
      *-----------------------------------------------------------------

           move desc-etiqueta17      to campo-116
           move desc-etiqueta17(1:4) to contrato-mt19
           move desc-etiqueta17(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta17 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-118
                move nome-form-mt19 to campo-122
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-124
                     move "--"       to campo-125
                not invalid key
                     move nome-cid   to campo-124
                     move uf-cid     to campo-125
                end-read
           end-read
           MOVE DESC-ETIQUETA17(6:4) TO campo-119

           move desc-etiqueta18      to campo-117
           move desc-etiqueta18(1:4) to contrato-mt19
           move desc-etiqueta18(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta18 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-120
                move nome-form-mt19 to campo-123
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-126
                     move "--"       to campo-127
                not invalid key
                     move nome-cid   to campo-126
                     move uf-cid     to campo-127
                end-read
           end-read
           MOVE DESC-ETIQUETA18(6:4) TO campo-121

      *-----------------------------------------------------------------
      *                          10ª LINHA
      *-----------------------------------------------------------------

           move desc-etiqueta19      to campo-128
           move desc-etiqueta19(1:4) to contrato-mt19
           move desc-etiqueta19(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta19 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-130
                move nome-form-mt19 to campo-134
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-136
                     move "--"       to campo-137
                not invalid key
                     move nome-cid   to campo-136
                     move uf-cid     to campo-137
                end-read
           end-read
           MOVE DESC-ETIQUETA19(6:4) TO campo-131

           move desc-etiqueta20      to campo-129
           move desc-etiqueta20(1:4) to contrato-mt19
           move desc-etiqueta20(6:4) to seq-mt19
           read mtd019 invalid key
                string "Contrato Não Encontrado" X"0DA0"
                       desc-etiqueta20 into mensagem
                move   "C" to tipo-msg
                perform 140-exibir-mensagem
           not invalid key
                move curso-mt19     to codigo-ie11
                read ied011 invalid key
                     move "-------" to nome-ie11
                end-read
                move nome-ie11      to campo-132
                move nome-form-mt19 to campo-135
                move cidade-mt19    to cidade
                read cad010 invalid key
                     move "--------" to campo-138
                     move "--"       to campo-139
                not invalid key
                     move nome-cid   to campo-138
                     move uf-cid     to campo-139
                end-read
           end-read
           MOVE DESC-ETIQUETA20(6:4) TO campo-133


           move det-001         to lnk-detalhe
           perform 790-imprimir-linha
           move det-002         to lnk-detalhe
           perform 790-imprimir-linha
           move det-003         to lnk-detalhe
           perform 790-imprimir-linha
           move det-004         to lnk-detalhe
           perform 790-imprimir-linha
           move det-005         to lnk-detalhe
           perform 790-imprimir-linha
           move det-006         to lnk-detalhe
           perform 790-imprimir-linha
           move det-009         to lnk-detalhe
           perform 790-imprimir-linha
           move det-010         to lnk-detalhe
           perform 790-imprimir-linha
           move det-011         to lnk-detalhe
           perform 790-imprimir-linha
           move det-012         to lnk-detalhe
           perform 790-imprimir-linha
           move det-013         to lnk-detalhe
           perform 790-imprimir-linha
           move det-014         to lnk-detalhe
           perform 790-imprimir-linha
           move det-015         to lnk-detalhe
           perform 790-imprimir-linha
           move det-016         to lnk-detalhe
           perform 790-imprimir-linha
           move det-017         to lnk-detalhe
           perform 790-imprimir-linha
           move det-018         to lnk-detalhe
           perform 790-imprimir-linha
           move det-019         to lnk-detalhe
           perform 790-imprimir-linha
           move det-020         to lnk-detalhe
           perform 790-imprimir-linha
           move det-021         to lnk-detalhe
           perform 790-imprimir-linha
           move det-022         to lnk-detalhe
           perform 790-imprimir-linha
           move det-025         to lnk-detalhe
           perform 790-imprimir-linha
           move det-026         to lnk-detalhe
           perform 790-imprimir-linha
           move det-028         to lnk-detalhe
           perform 790-imprimir-linha
           move det-029         to lnk-detalhe
           perform 790-imprimir-linha
           move det-030         to lnk-detalhe
           perform 790-imprimir-linha
           move det-031         to lnk-detalhe
           perform 790-imprimir-linha
           move det-032         to lnk-detalhe
           perform 790-imprimir-linha
           move det-033         to lnk-detalhe
           perform 790-imprimir-linha
           move det-034         to lnk-detalhe
           perform 790-imprimir-linha
           move det-035         to lnk-detalhe
           perform 790-imprimir-linha
           move det-036         to lnk-detalhe
           perform 790-imprimir-linha
           move det-037         to lnk-detalhe
           perform 790-imprimir-linha
           move det-038         to lnk-detalhe
           perform 790-imprimir-linha
           move det-039         to lnk-detalhe
           perform 790-imprimir-linha
           move det-040         to lnk-detalhe
           perform 790-imprimir-linha
           move det-041         to lnk-detalhe
           perform 790-imprimir-linha
           move det-042         to lnk-detalhe
           perform 790-imprimir-linha
           move det-043         to lnk-detalhe
           perform 790-imprimir-linha
           move det-044         to lnk-detalhe
           perform 790-imprimir-linha
           move det-045         to lnk-detalhe
           perform 790-imprimir-linha
           move det-046         to lnk-detalhe
           perform 790-imprimir-linha
           move det-047         to lnk-detalhe
           perform 790-imprimir-linha
           move det-048         to lnk-detalhe
           perform 790-imprimir-linha
           move det-049         to lnk-detalhe
           perform 790-imprimir-linha
           move det-050         to lnk-detalhe
           perform 790-imprimir-linha
           move det-051         to lnk-detalhe
           perform 790-imprimir-linha
           move det-052         to lnk-detalhe
           perform 790-imprimir-linha
           move det-053         to lnk-detalhe
           perform 790-imprimir-linha
           move det-054         to lnk-detalhe
           perform 790-imprimir-linha
           move det-055         to lnk-detalhe
           perform 790-imprimir-linha
           move det-056         to lnk-detalhe
           perform 790-imprimir-linha
           move det-057         to lnk-detalhe
           perform 790-imprimir-linha
           move det-058         to lnk-detalhe
           perform 790-imprimir-linha
           move det-059         to lnk-detalhe
           perform 790-imprimir-linha
           move det-060         to lnk-detalhe
           perform 790-imprimir-linha
           move det-061         to lnk-detalhe
           perform 790-imprimir-linha
           move det-062         to lnk-detalhe
           perform 790-imprimir-linha
           move det-063         to lnk-detalhe
           perform 790-imprimir-linha
           move det-064         to lnk-detalhe
           perform 790-imprimir-linha
           move det-065         to lnk-detalhe
           perform 790-imprimir-linha
           move det-066         to lnk-detalhe
           perform 790-imprimir-linha
           move det-067         to lnk-detalhe
           perform 790-imprimir-linha
           move det-068         to lnk-detalhe
           perform 790-imprimir-linha
           move det-069         to lnk-detalhe
           perform 790-imprimir-linha
           move det-070         to lnk-detalhe
           perform 790-imprimir-linha
           move det-071         to lnk-detalhe
           perform 790-imprimir-linha
           move det-072         to lnk-detalhe
           perform 790-imprimir-linha
           move det-073         to lnk-detalhe
           perform 790-imprimir-linha
           move det-074         to lnk-detalhe
           perform 790-imprimir-linha
           move det-075         to lnk-detalhe
           perform 790-imprimir-linha
           move det-076         to lnk-detalhe
           perform 790-imprimir-linha
           move det-077         to lnk-detalhe
           perform 790-imprimir-linha
           move det-078         to lnk-detalhe
           perform 790-imprimir-linha
           move det-079         to lnk-detalhe
           perform 790-imprimir-linha
           move det-080         to lnk-detalhe
           perform 790-imprimir-linha
           move det-081         to lnk-detalhe
           perform 790-imprimir-linha
           move det-082         to lnk-detalhe
           perform 790-imprimir-linha
           move det-083         to lnk-detalhe
           perform 790-imprimir-linha
           move det-084         to lnk-detalhe
           perform 790-imprimir-linha
           move det-085         to lnk-detalhe
           perform 790-imprimir-linha
           move det-086         to lnk-detalhe
           perform 790-imprimir-linha
           move det-087         to lnk-detalhe
           perform 790-imprimir-linha
           move det-088         to lnk-detalhe
           perform 790-imprimir-linha
           move det-089         to lnk-detalhe
           perform 790-imprimir-linha
           move det-090         to lnk-detalhe
           perform 790-imprimir-linha
           move det-091         to lnk-detalhe
           perform 790-imprimir-linha
           move det-092         to lnk-detalhe
           perform 790-imprimir-linha
           move det-093         to lnk-detalhe
           perform 790-imprimir-linha
           move det-094         to lnk-detalhe
           perform 790-imprimir-linha
           move det-095         to lnk-detalhe
           perform 790-imprimir-linha
           move det-096         to lnk-detalhe
           perform 790-imprimir-linha
           move det-097         to lnk-detalhe
           perform 790-imprimir-linha
           move det-098         to lnk-detalhe
           perform 790-imprimir-linha
           move det-099         to lnk-detalhe
           perform 790-imprimir-linha
           move det-100         to lnk-detalhe
           perform 790-imprimir-linha
           move det-101         to lnk-detalhe
           perform 790-imprimir-linha
           move det-102         to lnk-detalhe
           perform 790-imprimir-linha
           move det-103         to lnk-detalhe
           perform 790-imprimir-linha
           move det-104         to lnk-detalhe
           perform 790-imprimir-linha
           move det-105         to lnk-detalhe
           perform 790-imprimir-linha
           move det-106         to lnk-detalhe
           perform 790-imprimir-linha
           move det-107         to lnk-detalhe
           perform 790-imprimir-linha
           move det-108         to lnk-detalhe
           perform 790-imprimir-linha
           move det-109         to lnk-detalhe
           perform 790-imprimir-linha
           move det-110         to lnk-detalhe
           perform 790-imprimir-linha
           move det-111         to lnk-detalhe
           perform 790-imprimir-linha
           move det-112         to lnk-detalhe
           perform 790-imprimir-linha
           move det-113         to lnk-detalhe
           perform 790-imprimir-linha
           move det-114         to lnk-detalhe
           perform 790-imprimir-linha
           move det-115         to lnk-detalhe
           perform 790-imprimir-linha
           move det-116         to lnk-detalhe
           perform 790-imprimir-linha
           move det-117         to lnk-detalhe
           perform 790-imprimir-linha
           move det-118         to lnk-detalhe
           perform 790-imprimir-linha
           move det-119         to lnk-detalhe
           perform 790-imprimir-linha
           move det-120         to lnk-detalhe
           perform 790-imprimir-linha
           move det-121         to lnk-detalhe
           perform 790-imprimir-linha
           move det-122         to lnk-detalhe
           perform 790-imprimir-linha
           move det-123         to lnk-detalhe
           perform 790-imprimir-linha
           move det-124         to lnk-detalhe
           perform 790-imprimir-linha
           move det-125         to lnk-detalhe
           perform 790-imprimir-linha
           move det-126         to lnk-detalhe
           perform 790-imprimir-linha
           move det-127         to lnk-detalhe
           perform 790-imprimir-linha
           move det-128         to lnk-detalhe
           perform 790-imprimir-linha
           move det-129         to lnk-detalhe
           perform 790-imprimir-linha
           move det-130         to lnk-detalhe
           perform 790-imprimir-linha
           move det-131         to lnk-detalhe
           perform 790-imprimir-linha
           move det-132         to lnk-detalhe
           perform 790-imprimir-linha
           move det-133         to lnk-detalhe
           perform 790-imprimir-linha
           move det-134         to lnk-detalhe
           perform 790-imprimir-linha
           move det-135         to lnk-detalhe
           perform 790-imprimir-linha
           move det-136         to lnk-detalhe
           perform 790-imprimir-linha
           move det-137         to lnk-detalhe
           perform 790-imprimir-linha
           move det-138         to lnk-detalhe
           perform 790-imprimir-linha
           move det-139         to lnk-detalhe
           perform 790-imprimir-linha
           move det-140         to lnk-detalhe
           perform 790-imprimir-linha
           move det-141         to lnk-detalhe
           perform 790-imprimir-linha
           move det-142         to lnk-detalhe
           perform 790-imprimir-linha
           move det-143         to lnk-detalhe
           perform 790-imprimir-linha
           move det-144         to lnk-detalhe
           perform 790-imprimir-linha
           move det-145         to lnk-detalhe
           perform 790-imprimir-linha
           move det-146         to lnk-detalhe
           perform 790-imprimir-linha
           move det-147         to lnk-detalhe
           perform 790-imprimir-linha
           move det-148         to lnk-detalhe
           perform 790-imprimir-linha
           move det-149         to lnk-detalhe
           perform 790-imprimir-linha
           move det-150         to lnk-detalhe
           perform 790-imprimir-linha
           move det-151         to lnk-detalhe
           perform 790-imprimir-linha
           move det-152         to lnk-detalhe
           perform 790-imprimir-linha
           move det-153         to lnk-detalhe
           perform 790-imprimir-linha
           move det-154         to lnk-detalhe
           perform 790-imprimir-linha
           move det-155         to lnk-detalhe
           perform 790-imprimir-linha
           move det-156         to lnk-detalhe
           perform 790-imprimir-linha
           move det-157         to lnk-detalhe
           perform 790-imprimir-linha
           move det-158         to lnk-detalhe
           perform 790-imprimir-linha
           move det-159         to lnk-detalhe
           perform 790-imprimir-linha
           move det-160         to lnk-detalhe
           perform 790-imprimir-linha
           move det-161         to lnk-detalhe
           perform 790-imprimir-linha
           move det-162         to lnk-detalhe
           perform 790-imprimir-linha
           move det-163         to lnk-detalhe
           perform 790-imprimir-linha
           move det-164         to lnk-detalhe
           perform 790-imprimir-linha
           move det-165         to lnk-detalhe
           perform 790-imprimir-linha
           move det-166         to lnk-detalhe
           perform 790-imprimir-linha
           move det-167         to lnk-detalhe
           perform 790-imprimir-linha
           move det-168         to lnk-detalhe
           perform 790-imprimir-linha
           move det-169         to lnk-detalhe
           perform 790-imprimir-linha
           move det-170         to lnk-detalhe
           perform 790-imprimir-linha
           move det-171         to lnk-detalhe
           perform 790-imprimir-linha
           move det-172         to lnk-detalhe
           perform 790-imprimir-linha
           move det-173         to lnk-detalhe
           perform 790-imprimir-linha
           move det-174         to lnk-detalhe
           perform 790-imprimir-linha
           move det-175         to lnk-detalhe
           perform 790-imprimir-linha
           move det-176         to lnk-detalhe
           perform 790-imprimir-linha
           move det-177         to lnk-detalhe
           perform 790-imprimir-linha
           move det-178         to lnk-detalhe
           perform 790-imprimir-linha
           move det-179         to lnk-detalhe
           perform 790-imprimir-linha
           move det-180         to lnk-detalhe
           perform 790-imprimir-linha
           move det-181         to lnk-detalhe
           perform 790-imprimir-linha
           move det-182         to lnk-detalhe
           perform 790-imprimir-linha
           move det-183         to lnk-detalhe
           perform 790-imprimir-linha
           move det-184         to lnk-detalhe
           perform 790-imprimir-linha
           move det-185         to lnk-detalhe
           perform 790-imprimir-linha
           move det-186         to lnk-detalhe
           perform 790-imprimir-linha
           move det-187         to lnk-detalhe
           perform 790-imprimir-linha
           move det-188         to lnk-detalhe
           perform 790-imprimir-linha
           move det-189         to lnk-detalhe
           perform 790-imprimir-linha
           move det-190         to lnk-detalhe
           perform 790-imprimir-linha
           move det-191         to lnk-detalhe
           perform 790-imprimir-linha
           move det-192         to lnk-detalhe
           perform 790-imprimir-linha
           move det-193         to lnk-detalhe
           perform 790-imprimir-linha
           move det-194         to lnk-detalhe
           perform 790-imprimir-linha
           move det-195         to lnk-detalhe
           perform 790-imprimir-linha
           move det-196         to lnk-detalhe
           perform 790-imprimir-linha
           move det-197         to lnk-detalhe
           perform 790-imprimir-linha
           move det-198         to lnk-detalhe
           perform 790-imprimir-linha
           move det-199         to lnk-detalhe
           perform 790-imprimir-linha
           move det-200         to lnk-detalhe
           perform 790-imprimir-linha
           move det-201         to lnk-detalhe
           perform 790-imprimir-linha
           move det-202         to lnk-detalhe
           perform 790-imprimir-linha
           move det-203         to lnk-detalhe
           perform 790-imprimir-linha
           move det-204         to lnk-detalhe
           perform 790-imprimir-linha
           move det-205         to lnk-detalhe
           perform 790-imprimir-linha
           move det-206         to lnk-detalhe
           perform 790-imprimir-linha
           move det-207         to lnk-detalhe
           perform 790-imprimir-linha
           move det-208         to lnk-detalhe
           perform 790-imprimir-linha
           move det-209         to lnk-detalhe
           perform 790-imprimir-linha
           move det-210         to lnk-detalhe
           perform 790-imprimir-linha
           move det-211         to lnk-detalhe
           perform 790-imprimir-linha
           move det-212         to lnk-detalhe
           perform 790-imprimir-linha
           move det-213         to lnk-detalhe
           perform 790-imprimir-linha
           move det-214         to lnk-detalhe
           perform 790-imprimir-linha
           move det-215         to lnk-detalhe
           perform 790-imprimir-linha
           move det-216         to lnk-detalhe
           perform 790-imprimir-linha
           move det-217         to lnk-detalhe
           perform 790-imprimir-linha
           move det-218         to lnk-detalhe
           perform 790-imprimir-linha
           move det-219         to lnk-detalhe
           perform 790-imprimir-linha
           move det-220         to lnk-detalhe
           perform 790-imprimir-linha
           move det-221         to lnk-detalhe
           perform 790-imprimir-linha
           move det-222         to lnk-detalhe
           perform 790-imprimir-linha
           move det-223         to lnk-detalhe
           perform 790-imprimir-linha
           move det-224         to lnk-detalhe
           perform 790-imprimir-linha
           move det-225         to lnk-detalhe
           perform 790-imprimir-linha
           move det-226         to lnk-detalhe
           perform 790-imprimir-linha
           move det-227         to lnk-detalhe
           perform 790-imprimir-linha
           move det-228         to lnk-detalhe
           perform 790-imprimir-linha
           move det-229         to lnk-detalhe
           perform 790-imprimir-linha
           move det-230         to lnk-detalhe
           perform 790-imprimir-linha
           move det-231         to lnk-detalhe
           perform 790-imprimir-linha
           move det-232         to lnk-detalhe
           perform 790-imprimir-linha
           move det-233         to lnk-detalhe
           perform 790-imprimir-linha
           move det-234         to lnk-detalhe
           perform 790-imprimir-linha
           move det-235         to lnk-detalhe
           perform 790-imprimir-linha
           move det-236         to lnk-detalhe
           perform 790-imprimir-linha
           move det-237         to lnk-detalhe
           perform 790-imprimir-linha
           move det-238         to lnk-detalhe
           perform 790-imprimir-linha
           move det-239         to lnk-detalhe
           perform 790-imprimir-linha
           move det-240         to lnk-detalhe
           perform 790-imprimir-linha
           move det-241         to lnk-detalhe
           perform 790-imprimir-linha
           move det-242         to lnk-detalhe
           perform 790-imprimir-linha
           move det-243         to lnk-detalhe
           perform 790-imprimir-linha
           move det-244         to lnk-detalhe
           perform 790-imprimir-linha
           move det-245         to lnk-detalhe
           perform 790-imprimir-linha
           move det-246         to lnk-detalhe
           perform 790-imprimir-linha
           move det-247         to lnk-detalhe
           perform 790-imprimir-linha
           move det-248         to lnk-detalhe
           perform 790-imprimir-linha
           move det-249         to lnk-detalhe
           perform 790-imprimir-linha
           move det-250         to lnk-detalhe
           perform 790-imprimir-linha
           move det-251         to lnk-detalhe
           perform 790-imprimir-linha
           move det-252         to lnk-detalhe
           perform 790-imprimir-linha
           move det-253         to lnk-detalhe
           perform 790-imprimir-linha
           move det-254         to lnk-detalhe
           perform 790-imprimir-linha
           move det-255         to lnk-detalhe
           perform 790-imprimir-linha
           move det-256         to lnk-detalhe
           perform 790-imprimir-linha
           move det-257         to lnk-detalhe
           perform 790-imprimir-linha
           move det-258         to lnk-detalhe
           perform 790-imprimir-linha
           move det-259         to lnk-detalhe
           perform 790-imprimir-linha
           move det-260         to lnk-detalhe
           perform 790-imprimir-linha
           move det-261         to lnk-detalhe
           perform 790-imprimir-linha
           move det-262         to lnk-detalhe
           perform 790-imprimir-linha
           move det-263         to lnk-detalhe
           perform 790-imprimir-linha
           move det-264         to lnk-detalhe
           perform 790-imprimir-linha
           move det-265         to lnk-detalhe
           perform 790-imprimir-linha
           move det-266         to lnk-detalhe
           perform 790-imprimir-linha
           move det-267         to lnk-detalhe
           perform 790-imprimir-linha
           move det-268         to lnk-detalhe
           perform 790-imprimir-linha
           move det-269         to lnk-detalhe
           perform 790-imprimir-linha
           move det-270         to lnk-detalhe
           perform 790-imprimir-linha
           move det-271         to lnk-detalhe
           perform 790-imprimir-linha
           move det-272         to lnk-detalhe
           perform 790-imprimir-linha
           move det-273         to lnk-detalhe
           perform 790-imprimir-linha
           move det-274         to lnk-detalhe
           perform 790-imprimir-linha
           move det-275         to lnk-detalhe
           perform 790-imprimir-linha
           move det-276         to lnk-detalhe
           perform 790-imprimir-linha
           move det-277         to lnk-detalhe
           perform 790-imprimir-linha
           move det-278         to lnk-detalhe
           perform 790-imprimir-linha
           move det-279         to lnk-detalhe
           perform 790-imprimir-linha
           move det-280         to lnk-detalhe
           perform 790-imprimir-linha
           move det-281         to lnk-detalhe
           perform 790-imprimir-linha
           move det-282         to lnk-detalhe
           perform 790-imprimir-linha
           move det-283         to lnk-detalhe
           perform 790-imprimir-linha
           move det-284         to lnk-detalhe
           perform 790-imprimir-linha
           move det-285         to lnk-detalhe
           perform 790-imprimir-linha
           move det-286         to lnk-detalhe
           perform 790-imprimir-linha
           move det-287         to lnk-detalhe
           perform 790-imprimir-linha
           move det-288         to lnk-detalhe
           perform 790-imprimir-linha
           move det-289         to lnk-detalhe
           perform 790-imprimir-linha
           move det-290         to lnk-detalhe
           perform 790-imprimir-linha
           move det-291         to lnk-detalhe
           perform 790-imprimir-linha
           move det-292         to lnk-detalhe
           perform 790-imprimir-linha
           move det-293         to lnk-detalhe
           perform 790-imprimir-linha
           move det-294         to lnk-detalhe
           perform 790-imprimir-linha
           move det-295         to lnk-detalhe
           perform 790-imprimir-linha
           move det-296         to lnk-detalhe
           perform 790-imprimir-linha
           move det-297         to lnk-detalhe
           perform 790-imprimir-linha
           move det-298         to lnk-detalhe
           perform 790-imprimir-linha
           move det-299         to lnk-detalhe
           perform 790-imprimir-linha
           move det-300         to lnk-detalhe
           perform 790-imprimir-linha
           move det-301         to lnk-detalhe
           perform 790-imprimir-linha
           move det-302         to lnk-detalhe
           perform 790-imprimir-linha
           move det-303         to lnk-detalhe
           perform 790-imprimir-linha
           move det-304         to lnk-detalhe
           perform 790-imprimir-linha
           move det-305         to lnk-detalhe
           perform 790-imprimir-linha
           move det-306         to lnk-detalhe
           perform 790-imprimir-linha
           move det-307         to lnk-detalhe
           perform 790-imprimir-linha
           move det-308         to lnk-detalhe
           perform 790-imprimir-linha
           move det-309         to lnk-detalhe
           perform 790-imprimir-linha
           move det-310         to lnk-detalhe
           perform 790-imprimir-linha
           move det-311         to lnk-detalhe
           perform 790-imprimir-linha
           move det-312         to lnk-detalhe
           perform 790-imprimir-linha
           move det-313         to lnk-detalhe
           perform 790-imprimir-linha
           move det-314         to lnk-detalhe
           perform 790-imprimir-linha
           move det-315         to lnk-detalhe
           perform 790-imprimir-linha
           move det-316         to lnk-detalhe
           perform 790-imprimir-linha
           move det-317         to lnk-detalhe
           perform 790-imprimir-linha
           move det-318         to lnk-detalhe
           perform 790-imprimir-linha
           move det-319         to lnk-detalhe
           perform 790-imprimir-linha
           move det-320         to lnk-detalhe
           perform 790-imprimir-linha
           move det-321         to lnk-detalhe
           perform 790-imprimir-linha
           move det-322         to lnk-detalhe
           perform 790-imprimir-linha
           move det-323         to lnk-detalhe
           perform 790-imprimir-linha
           move det-324         to lnk-detalhe
           perform 790-imprimir-linha
           move det-325         to lnk-detalhe
           perform 790-imprimir-linha
           move det-326         to lnk-detalhe
           perform 790-imprimir-linha
           move det-327         to lnk-detalhe
           perform 790-imprimir-linha
           move det-328         to lnk-detalhe
           perform 790-imprimir-linha
           move det-329         to lnk-detalhe
           perform 790-imprimir-linha
           move det-330         to lnk-detalhe
           perform 790-imprimir-linha
           move det-331         to lnk-detalhe
           perform 790-imprimir-linha
           move det-332         to lnk-detalhe
           perform 790-imprimir-linha
           move det-333         to lnk-detalhe
           perform 790-imprimir-linha
           move det-334         to lnk-detalhe
           perform 790-imprimir-linha
           move det-335         to lnk-detalhe
           perform 790-imprimir-linha
           move det-336         to lnk-detalhe
           perform 790-imprimir-linha
           move det-337         to lnk-detalhe
           perform 790-imprimir-linha
           move det-338         to lnk-detalhe
           perform 790-imprimir-linha
           move det-339         to lnk-detalhe
           perform 790-imprimir-linha
           move det-340         to lnk-detalhe
           perform 790-imprimir-linha
           move det-341         to lnk-detalhe
           perform 790-imprimir-linha
           move det-342         to lnk-detalhe
           perform 790-imprimir-linha
           move det-343         to lnk-detalhe
           perform 790-imprimir-linha
           move det-344         to lnk-detalhe
           perform 790-imprimir-linha
           move det-345         to lnk-detalhe
           perform 790-imprimir-linha
           move det-346         to lnk-detalhe
           perform 790-imprimir-linha
           move det-347         to lnk-detalhe
           perform 790-imprimir-linha
           move det-348         to lnk-detalhe
           perform 790-imprimir-linha
           move det-349         to lnk-detalhe
           perform 790-imprimir-linha
           move det-350         to lnk-detalhe
           perform 790-imprimir-linha
           move det-351         to lnk-detalhe
           perform 790-imprimir-linha
           move det-352         to lnk-detalhe
           perform 790-imprimir-linha
           move det-353         to lnk-detalhe
           perform 790-imprimir-linha
           move det-354         to lnk-detalhe
           perform 790-imprimir-linha
           move det-355         to lnk-detalhe
           perform 790-imprimir-linha
           move det-356         to lnk-detalhe
           perform 790-imprimir-linha
           move det-357         to lnk-detalhe
           perform 790-imprimir-linha
           move det-358         to lnk-detalhe
           perform 790-imprimir-linha
           move det-359         to lnk-detalhe
           perform 790-imprimir-linha
           move det-360         to lnk-detalhe
           perform 790-imprimir-linha
           move det-361         to lnk-detalhe
           perform 790-imprimir-linha
           move det-362         to lnk-detalhe
           perform 790-imprimir-linha
           move det-363         to lnk-detalhe
           perform 790-imprimir-linha
           move det-364         to lnk-detalhe
           perform 790-imprimir-linha
           move det-365         to lnk-detalhe
           perform 790-imprimir-linha
           move det-366         to lnk-detalhe
           perform 790-imprimir-linha
           move det-367         to lnk-detalhe
           perform 790-imprimir-linha
           move det-368         to lnk-detalhe
           perform 790-imprimir-linha
           move det-369         to lnk-detalhe
           perform 790-imprimir-linha
           move det-370         to lnk-detalhe
           perform 790-imprimir-linha
           move det-371         to lnk-detalhe
           perform 790-imprimir-linha
           move det-372         to lnk-detalhe
           perform 790-imprimir-linha
           move det-373         to lnk-detalhe
           perform 790-imprimir-linha
           move det-374         to lnk-detalhe
           perform 790-imprimir-linha
           move det-375         to lnk-detalhe
           perform 790-imprimir-linha
           move det-376         to lnk-detalhe
           perform 790-imprimir-linha
           move det-377         to lnk-detalhe
           perform 790-imprimir-linha
           move det-378         to lnk-detalhe
           perform 790-imprimir-linha
           move det-379         to lnk-detalhe
           perform 790-imprimir-linha
           move det-380         to lnk-detalhe
           perform 790-imprimir-linha
           move det-381         to lnk-detalhe
           perform 790-imprimir-linha

           perform 730-fechar-impressora.

       700-imprimir-etiqueta-fim.
           exit.

       720-abrir-impressora section.
          MOVE "c:\ETIQUETAS.HTM" TO ARQUIVO-ORDSERV
          OPEN OUTPUT ORDSERV.
       720-abrir-impressora-fim.
           exit.

       730-fechar-impressora section.
           CLOSE ORDSERV.
       730-fechar-impressora-fim.
           exit.

       790-imprimir-linha section.
           move lnk-detalhe        to reg-ordserv
           write reg-ordserv.
       790-imprimir-linha-fim.
           exit.



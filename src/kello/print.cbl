       identification division.
       program-id.    PRINT2.
       author.        Alfredo Saviolli.
       date-written.  10-12-2003.
       security.      Modulo de Impressao de Linhas
                      Para Visualizacao na Tela ou para
                      Descarregar na Impressora.
       environment division.
       configuration section.
       input-output section.
       file-control.

           select arqimp assign to ws-arqimp
                  organization  is sequential
                  access mode   is sequential
                  lock mode     is exclusive
                  file status   is fs-print.

       data division.
       file section.

       fd  arqimp label record is omitted.
       01  reg-arqimp.
           03 filler             pic x(264).

       working-storage section.
       77 TIPO-MSG               PIC X(01).
       77 RESP-MSG               PIC X(01).
       77 MENSAGEM               PIC X(200).
       77 fs-print               pic x(02).
       77 ind-cab                pic 9(03).
       77 ind-timbrado           pic 9(03).

       01 status-code            pic x(02) comp-5.
       01 comando                pic x(04) comp-5.
       01 linha-detalhe          pic x(264).
       01 linha-detalhe-len      pic x(04) comp-5 value 264.
       01 font-size              pic x(04) comp-5.
       01 font-style             pic x(04) comp-5 value 0.

       01 PRT-INFO-1 is typedef.
          03 pi-struct-size      pic x(4) comp-5.
          03 hdc                 pic x(4) comp-5.
          03 hps                 pic x(4) comp-5.
          03 orientation         pic x(4) comp-5.
          03 rows                pic x(4) comp-5.
          03 cols                pic x(4) comp-5.
          03 rows-left           pic x(4) comp-5.
          03 max-horiz           pic x(4) comp-5.
          03 max-vert            pic x(4) comp-5.
          03 min-horiz           pic x(4) comp-5.
          03 min-vert            pic x(4) comp-5.
          03 curr-horiz          pic x(4) comp-5.
          03 curr-vert           pic x(4) comp-5.
          03 copies              pic 9(4) comp-5.
          03 quality             pic 9(4) comp-5.
          03 color               pic 99 comp-5.
          03 reserved1           pic x comp-5.
          03 driver-ver          pic 9(4) comp-5.
          03 pname.
             05 cbsize           pic x(4) comp-5.
             05 buffer           pointer.
          03 ptype.
             05 cbsize           pic x(4) comp-5.
             05 buffer           pointer.
          03 pdevice.
             05 cbsize           pic x(4) comp-5.
             05 buffer           pointer.
          03 plocation.
             05 cbsize           pic x(4) comp-5.
             05 buffer           pointer.
          03 pcomment.
             05 cbsize           pic x(4) comp-5.
             05 buffer           pointer.
          03 ppapersize.
             05 cbsize           pic x(4) comp-5.
             05 buffer           pointer.
       01.
          03 document-title.
             05 title-len        pic x(2) comp-5 value 50.
             05 title-text       pic x(50).
          03 font-family.
             05 font-family-namelen  pic x(2) comp-5 value 80.
             05 font-family-name     pic x(80).
          03 print-info          PRT-INFO-1.
          03 abort               pic x(4) comp-5 value 1.
          03 controle            pic x(4) comp-5 value 2.
          03 flags               pic x(4) comp-5 value 0.
          03 handle              pic x(4) comp-5.
       01 printer-name           pic x(255).
       01 printer-type           pic x(255).
       01 printer-device         pic x(255).
       01 printer-location       pic x(255).
       01 printer-comment        pic x(255).
       01 printer-papersize      pic x(255).
       01 extensao-arq           pic 9(03).

       01 ws-arqimp              pic x(50).

       linkage section.
       01 lnk-printer.
          03 lnk-num-cabs        pic 9(02).
          03 lnk-timbrado        pic x(01).
          03 lnk-cabecalhos      pic x(160) occurs 10 times.
          03 lnk-tamrel          pic 9(03).
          03 lnk-title-text      pic x(50).
          03 lnk-path-video      pic x(50).
          03 lnk-detalhe         pic x(264).
          03 lnk-funcao          pic x(04).
          03 lnk-video-impr      pic x(01).
          03 lnk-fs-print        pic x(02).
          03 lnk-linhas-pag      pic 9(03).                      
          03 lnk-colunas-pag     pic 9(03).
          03 lnk-linhas          pic 9(03).

       procedure division using lnk-printer.
       raiz section.
           evaluate lnk-funcao
              when "ABRE"
                   if lnk-video-impr = "V"
                      perform video-abre
                   else
                      perform printer-abre
                   end-if
              when "FECH"
                   if lnk-video-impr = "V"
                      perform video-fecha
                   else
                      perform printer-fecha
                   end-if
              when "IMPR"
                   if lnk-video-impr = "V"
                      perform video-linha
                   else
                      perform printer-linha
                   end-if
              when "IMPN"
                   if lnk-video-impr = "V"
                      perform video-negrito
                   else
                      perform printer-negrito
                   end-if
              when "IMPB"
                   if lnk-video-impr = "V"
                      perform video-cbarra
                   else
                      perform printer-cbarra
                   end-if
              when "CANC"
                   if lnk-video-impr = "V"
                      perform video-cancela
                   else
                      perform printer-cancela
                   end-if
              when "SALT"
                   if lnk-video-impr = "V"
                      perform video-salta-pagina
                   else
                      perform printer-salta-pagina
                   end-if
              when "INFO"
                   if lnk-video-impr = "V"
                      perform video-informacao
                   else
                      perform printer-informacao
                   end-if
              when "CABE"
                   if lnk-video-impr = "V"
                      perform video-cabecalho 
                   else
                      perform printer-cabecalho  
                   end-if
           end-evaluate.
       raiz-fim.
           exit program
           stop run.

      *----Rotinas para Impressao Grafica---(printer)-------------------

       printer-abre section.
           if lnk-tamrel > 160
              move 8 to flags
           else
              move 4 to flags
           end-if
           move lnk-title-text to title-text
           call "PC_PRINTER_OPEN" using by reference handle
                                        by reference document-title
                                        by value 1
                                        by value 0
                                  returning status-code
           end-call
           if status-code = zeros
              move "00" to lnk-fs-print
              perform printer-setfonte
              if lnk-fs-print = "00"
                 perform printer-informacao
              end-if
           else
              move "99" to lnk-fs-print
              perform mensagem-padrao
           end-if.
       printer-abre-fim.
           exit.

       printer-fecha section.
           call "PC_PRINTER_CLOSE" using by reference handle
                                            returning status-code
           end-call.
       printer-fecha-fim.
           exit.

       printer-cancela section.
           move 1  to comando
           perform printer-comando.
       printer-cancela-fim.
           exit.

       printer-comando section.
           call "PC_PRINTER_CONTROL" using by reference handle
                                               by value comando
                                              returning status-code
           end-call
           if status-code = zeros
              move "00" to lnk-fs-print
           else
              move "99" to lnk-fs-print
              perform mensagem-padrao
           end-if.
       printer-comando-fim.
           exit.

       printer-informacao section.
           move length of print-info to pi-struct-size
           set buffer of pname of print-info
               to address of printer-name
           move 255 to cbsize of pname of print-info
           set buffer of ptype of print-info
               to address of printer-type
           move 255 to cbsize of ptype of print-info
           set buffer of pdevice of print-info
                to address of printer-device
           move 255 to cbsize of pdevice of print-info
           set buffer of plocation of print-info
               to address of printer-location
           move 255 to cbsize of plocation of print-info
           set buffer of pcomment of print-info
               to address of printer-comment
           move 255 to cbsize of pcomment of print-info
           set buffer of ppapersize of print-info
               to address of printer-papersize
           move 255 to cbsize of ppapersize of print-info
           call "PC_PRINTER_INFO" using by reference handle
                                        by reference print-info
                                           returning status-code
           end-call.
           if status-code = zeros
              move "00" to lnk-fs-print
              move rows of print-info to lnk-linhas-pag
              move cols of print-info to lnk-colunas-pag
              subtract 4 from lnk-linhas-pag
           else
              move "99" to lnk-fs-print
              perform mensagem-padrao
           end-if.
       printer-informacao-fim.
           exit.

       printer-setfonte section.
           if lnk-tamrel not > 80
              move 11 to font-size
           else
              if lnk-tamrel not > 120
                 move 09 to font-size
              else
                 move 11 to font-size
              end-if
           end-if
           move "courier new" to font-family-name
           move 11            to font-family-namelen
           move zeros         to font-style
           call "PC_PRINTER_SET_FONT" using handle
                                            font-family
                                  by value  font-size
                                  by value  font-style
                                  returning status-code
           if status-code = zeros
              move "00" to lnk-fs-print
           else
              move "99" to lnk-fs-print
              perform mensagem-padrao
           end-if.
       printer-setfonte-fim.
           exit.

       printer-getfonte section.
           call "PC_PRINTER_GET_FONT" using handle
                                            font-family
                                            font-size
                                            font-style
                                  returning status-code
           if status-code = zeros
              move "00" to lnk-fs-print
           else
              move "99" to lnk-fs-print
              perform mensagem-padrao
           end-if.
           if font-size < 6
              move "99" to lnk-fs-print
              move "C"  to tipo-msg
              move "Escolha uma Fonte com Tamanho entre: (6 a 12)"
                to mensagem
              perform exibir-mensagem
           end-if
      *    if font-size < 6 or > 12
      *       move "99" to lnk-fs-print
      *       move "C"  to tipo-msg
      *       move "Escolha uma Fonte com Tamanho entre: (6 a 12)"
      *         to mensagem
      *       perform exibir-mensagem
      *    end-if
           if lnk-colunas-pag < lnk-tamrel
              move "99" to lnk-fs-print
              move "C"  to tipo-msg
              move "Tamanho do Fonte Insuficiente para o Relatório"
                to mensagem
              perform exibir-mensagem
           end-if.
       printer-getfonte-fim.
           exit.

       printer-linha section.
           move lnk-detalhe to linha-detalhe
           perform printer-write.
       printer-linha-fim.
           exit.

       printer-write section.
           call "PC_PRINTER_WRITE" using handle
                                         linha-detalhe
                                by value linha-detalhe-len
                               returning status-code.
           if status-code = zeros
              move "00" to lnk-fs-print
              if lnk-funcao = "IMPR" OR "IMPN" OR "IMPB" OR "CABE"
                 perform printer-avanca-linha
              end-if
           else
              move "99" to lnk-fs-print
              perform mensagem-padrao
           end-if.
           add 1 to lnk-linhas.
       printer-write-fim.
           exit.

       printer-negrito section.
           move 8 to font-style
           call "PC_PRINTER_SET_FONT" using handle
                                            font-family
                                  by value  font-size
                                  by value  font-style
                                  returning status-code.
           perform printer-linha.
           perform printer-setfonte.
       printer-negrito-fim.
           exit.

       printer-cbarra section.
           move "barcod39"    to font-family-name
           move 8             to font-family-namelen
           move 20            to font-size
           call "PC_PRINTER_SET_FONT" using handle
                                            font-family
                                  by value  font-size
                                  by value  font-style
                                  returning status-code.
           perform printer-linha.
           perform printer-setfonte.
       printer-cbarra-fim.
           exit.

       printer-avanca-linha SECTION.
           move 4 to comando
           perform printer-comando.
       printer-avanca-linha-fim.
           exit.

       printer-salta-pagina SECTION.
           move 2 to comando
           perform printer-comando.
       printer-salta-pagina-fim.
           exit.

       printer-cabecalho section.
           move zeros  to lnk-linhas.
           move spaces to linha-detalhe
           perform printer-write
           if lnk-timbrado equal "S"
              perform printer-timbrado.
           perform varying ind-cab from 1 by 1
                   until ind-cab > lnk-num-cabs or
                         lnk-fs-print not equal "00"
              move lnk-cabecalhos(ind-cab) to linha-detalhe
              perform printer-write
           end-perform.
       printer-cabecalho-fim.
           exit. 

       printer-timbrado section.
           if font-size = 6 or 7
              move 15 to ind-timbrado
           else
           if font-size = 8 or 9
              move 12 to ind-timbrado
           else
           if font-size = 10 or 11 or 12
              move 08 to ind-timbrado
           else
              move 10 to ind-timbrado.
           perform varying ind-cab from 1 by 1
                   until ind-cab > ind-timbrado or
                         lnk-fs-print not equal "00"
              move spaces to linha-detalhe
              perform printer-write
           end-perform.
       printer-timbrado-fim.
           exit.


      *----Rotinas para Visualizacao do Relatorio --(Video)-------------

       video-abre section.
           move "99"   to fs-print
           move zeros  to extensao-arq
           inspect lnk-path-video replacing all spaces by "*"
           perform until fs-print equal "00" or extensao-arq > 998
              move spaces to ws-arqimp
              string lnk-path-video extensao-arq
                     delimited by "*" into ws-arqimp
              open output arqimp
              add 1 to extensao-arq
           end-perform.
           move ws-arqimp to lnk-path-video.
           move fs-print  to lnk-fs-print.
           perform video-informacao.
       video-abre-fim.
           exit.

       video-fecha section.
           close arqimp.
       video-fecha-fim.
           exit.

       video-cancela section.
           perform video-fecha.
       video-cancela-fim.
           exit.

       video-comando section.
       video-comando-fim.
           exit.

       video-informacao section.
           move 60  to lnk-linhas-pag
           move 264 to lnk-colunas-pag.
       video-informacao-fim.
           exit.

       video-linha section.
           move lnk-detalhe to reg-arqimp
           perform video-write.
       video-linha-fim.
           exit.

       video-write section.
           write reg-arqimp after 1 
           if fs-print = zeros
              move "00" to lnk-fs-print
           else
              move "99" to lnk-fs-print
           end-if.
           add 1 to lnk-linhas.
       video-write-fim.
           exit.

       video-negrito section.
           perform video-linha.
       video-negrito-fim.
           exit.

       video-cbarra section.
           perform video-linha.
       video-cbarra-fim.
           exit.

       video-avanca-linha SECTION.
       video-avanca-linha-fim.
           exit.

       video-salta-pagina SECTION.
           move spaces to reg-arqimp
           write reg-arqimp after 1.
           add 1 to lnk-linhas.
       video-salta-pagina-fim.
           exit.

       video-cabecalho section.
           move zeros to lnk-linhas.
           perform video-salta-pagina.
           perform varying ind-cab from 1 by 1
                   until ind-cab > lnk-num-cabs or
                         lnk-fs-print not equal "00"
              move lnk-cabecalhos(ind-cab) to reg-arqimp
              perform video-write
           end-perform.
       video-cabecalho-fim.
           exit.

       mensagem-padrao section.
           move "C"  to tipo-msg
           move "Erro ao acessar a Impressora" to mensagem
           perform exibir-mensagem.
       mensagem-padrao-fim.
           exit.

       EXIBIR-MENSAGEM SECTION.
           MOVE SPACES TO RESP-MSG
           CALL   "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL "MENSAGEM".
       EXIBIR-MENSAGEM-FIM.
          EXIT.


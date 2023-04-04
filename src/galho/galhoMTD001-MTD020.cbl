       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOMTD001-MTD020.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 14-05-2010
      *DESCRIÇÃO: Conversão MTD001

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.
           COPY MTPX001.
           COPY MTPX020.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.
           COPY MTPW001.
           COPY MTPW020.

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD001             PIC XX       VALUE SPACES.
           05  ST-MTD001             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
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
           05  STATUS-CODE           PIC X(02) COMP-5.
           05  total-montadas        pic 9(09) value zeros.
           05  total-avulsas         pic 9(09) value zeros.
           05  total-clientes        pic 9(09) value zeros.
           05  masc-qtde             pic zzz.zz9.


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           open i-o   cad001
           close      cad001
           open input cad001

           if st-cad001 <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura...CAD001" x"0da0"
                     "Status . . . " st-cad001 into mensagem
              move "C" to tipo-msg
              perform exibir-mensagem.


           initialize reg-cad001
           start cad001 key is not less codigo-ca001 invalid key
                 move "10" to st-cad001.

           perform until st-cad001 = "10"
                 read cad001 next at end
                      move "10" to st-cad001
                 not at end
                      DISPLAY "CODIGO-CA001 = " CODIGO-CA001 STOP " "
                      perform abrir-arquivos
                      perform converter-arquivo
                      perform fechar-arquivos
                      display "ACABEI ESSA EMPRESA" STOP " "
                 end-read
           end-perform

           close cad001

           STOP RUN.

       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "MTD001"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-MTD001

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "MTD020"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-MTD020

           OPEN I-O                       MTD001 MTD020.
       abrir-arquivos-fim.
           exit.

       converter-arquivo section.
           initialize reg-mtd001
           start mtd001 key is not less contrato-mt01 invalid key
                 move "10" to st-mtd001.

           perform until st-mtd001 = "10"
                 read mtd001 next at end
                      move "10" to st-mtd001
                 not at end
                      perform calcular-mtd020

                      move total-montadas      to montada-mt01
                      move total-avulsas       to avulsa-mt01
                      move total-clientes      to clien-album-mt01

                      if total-montadas > 0 or total-avulsas > 0
                         compute produzida-mt01 = montada-mt01 +
                                                  perdida-mt01 +
                                                  avulsa-mt01
                      else
                         compute produzida-mt01 = montada-mt01 +
                                                  avulsa-mt01
                      end-if

      *              if contrato-mt01 = 63
      *                 display erase at 0101
      *                 display "Total Montadas = "  at 0101
      *                 move total-montadas to masc-qtde
      *                 display masc-qtde            at 0130
      *                 display "Total Avulsas  = "  at 0201
      *                 move total-avulsas to masc-qtde
      *                 display masc-qtde            at 0230
      *                 display "Total Clientes = "  at 0301
      *                 move total-clientes to masc-qtde
      *                 display masc-qtde            at 0330
      *                 display "Produzida-mt01 = "  at 0401
      *                 move produzida-mt01 to masc-qtde
      *                 display masc-qtde            at 0430
      *
      *                 stop " "
      *              end-if

                     if produzida-mt01 > 0
                        display "reg-mtd001 = " reg-mtd001
                        rewrite reg-mtd001 invalid key
                             move "Erro de Regravação...MTD001"
                               to mensagem
                             move "C" to tipo-msg
                             perform exibir-mensagem
                        end-rewrite
                     else
                        DISPLAY "VOU APAGAR O " contrato-mt01
                        delete mtd001 invalid key
                              move "Erro de Exclusão...MTD001" to
                              mensagem
                              move "C" to tipo-msg
                              perform exibir-mensagem
                        end-delete
                     end-if
                 end-read
           end-perform.
       converter-arquivo-fim.
           exit.

       calcular-mtd020 section.
           initialize total-montadas
                      total-avulsas
                      total-clientes
                      reg-mtd020

           move contrato-mt01 to contrato-mtg
           start mtd020 key is not less album-mtg invalid key
                move "10" to st-mtd020.

           perform until st-mtd020 = "10"
                read mtd020 next at end
                     move "10" to st-mtd020
                not at end
                     if contrato-mt01 <> contrato-mtg
                        move "10" to st-mtd020
                     else
                        if nralbum-mtg = 0
                           add qt-fotos-mtg to total-avulsas
                        else
                           add qt-fotos-mtg to total-montadas
                        end-if
                        if nao-gerou-album-mtg <> 1 and nralbum-mtg <> 0
                           add 1            to total-clientes
                        end-if
                     end-if
                end-read
           end-perform.
       calcular-mtd020-fim.
           exit.

       fechar-arquivos section.
           close mtd001 mtd020.
       fechar-arquivos-fim.
           exit.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.

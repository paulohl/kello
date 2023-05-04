       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCRD020Juntar.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 02-11-2010
      *DESCRIÇÃO: Juntar Arquivos

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CRPX020.

          SELECT CRD020a ASSIGN TO PATH-CRD020a
                  ORGANIZATION  IS   SEQUENTIAL
                  ACCESS MODE   IS   SEQUENTIAL
                  STATUS        IS   ST-CRD020a.

          SELECT CRD020b ASSIGN TO PATH-CRD020b
                  ORGANIZATION  IS   SEQUENTIAL
                  ACCESS MODE   IS   SEQUENTIAL
                  STATUS        IS   ST-CRD020b.


          SELECT ARQUIVO ASSIGN       TO CAMINHO-ARQUIVO
                         ORGANIZATION IS LINE SEQUENTIAL
                         ACCESS MODE  IS      SEQUENTIAL
                         STATUS       IS      ST-ARQUIVO.

       DATA DIVISION.
       FILE SECTION.

           COPY CRPW020.

       FD CRD020a.
       01 REG-CRD020A                PIC X(01).

       FD CRD020b.
       01 REG-CRD020b                PIC X(01).

       FD ARQUIVO.
       01 REG-ARQUIVO.
          05 ARQUIVO-LINHA           PIC ZZZ.ZZZ.ZZZ.ZZ9.
          05 FILLER                  PIC X(02).
          05 ARQUIVO-REGISTRO        PIC X(267).


       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  ST-CRD020A            PIC XX       VALUE SPACES.
           05  ST-CRD020B            PIC XX       VALUE SPACES.
           05  ST-ARQUIVO            PIC XX       VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  MENSAGEM              PIC X(200).
           05  FLAG-CRITICA          PIC 9(01) VALUE ZEROS.
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  STATUS-CODE           PIC X(02) COMP-5.
           05  ACP-CAMINHO1          PIC X(255) VALUE SPACES.
           05  ACP-CAMINHO2          PIC X(255) VALUE SPACES.
           05  ACP-CAMINHO3          PIC X(255) VALUE SPACES.
           05  ACP-DTINI             PIC 9(08)  VALUE ZEROS.
           05  ACP-DTFIM             PIC 9(08)  VALUE ZEROS.
           05  WS-OK                 PIC X(01)  VALUE SPACES.
           05  DATA-INI              PIC 9(08)  VALUE ZEROS.
           05  DATA-FIM              PIC 9(08)  VALUE ZEROS.
           05  RESP                  PIC X(01)  VALUE SPACES.
           05  IND                   PIC 9(03)  VALUE ZEROS.
           05  GRAVAR                PIC X(01)  VALUE SPACES.
           05  qtde-registro         pic 9(18)  value zeros.
           05  qtd-titulos           pic 9(18)  value zeros.
           05  qtd-titulos1          pic 9(18)  value zeros.
           05  qtd-titulos2          pic 9(18)  value zeros.
           05  qtde-erro1            pic 9(18)  value zeros.
           05  qtde-erro2            pic 9(18)  value zeros.
           05  masc-qtde             pic zzz.zzz.zzz.zzz.zzz.zz9.
           05  masc-qtde1            pic zzz.zzz.zzz.zzz.zzz.zz9.
           05  masc-qtde2            pic zzz.zzz.zzz.zzz.zzz.zz9.
           05  masc-qtde3            pic zzz.zzz.zzz.zzz.zzz.zz9.
           05  masc-qtde4            pic zzz.zzz.zzz.zzz.zzz.zz9.
           05  masc-qtde5            pic zzz.zzz.zzz.zzz.zzz.zz9.

       01 ws-data-sys.
          05 ws-data-cpu.
             10 ws-ano-cpu           pic 9(04).
             10 ws-mes-cpu           pic 9(02).
             10 ws-dia-cpu           pic 9(02).
          05 filler                  pic x(13).


       01 file-details.
          05 file-size               pic x(8) comp-x.
          05 file-date.
             10 dia                  pic x comp-x.
             10 month                pic x comp-x.
             10 year                 pic x(2) comp-x.
          05 file-time.
             10 hours                pic x comp-x.
             10 minutes              pic x comp-x.
             10 seconds              pic x comp-x.
             10 hundredths           pic x comp-x.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           display erase at 0101.

       001-caminho1.
           display "Informar o Caminho Arquivo Atual...: "
                at 0101
           accept acp-caminho1 at 0138

           if acp-caminho1 = spaces
              go to 001-caminho1
           else
              call "CBL_CHECK_FILE_EXIST"     using acp-caminho1
                                                    file-details
                                          returning status-code
              if status-code <> 0
                 display "Arquivo Nao Encontrado" at 1001
                    stop " "
                 display "                      " at 1001
                 go to 001-caminho1
              else
                 display "                      " at 1001.

       002-caminho2.
           display "Informar o Caminho Arquivo Backup..: "
                at 0201
           accept acp-caminho2 at 0238

           if acp-caminho2 = spaces
              go to 002-caminho2
           else
              call "CBL_CHECK_FILE_EXIST"     using acp-caminho2
                                                    file-details
                                          returning status-code
              if status-code <> 0
                 display "Arquivo de Backup Nao Encontrado" at 1001
                    stop " "
                 display "                                "  at 1001
                 go to 002-caminho2
              else
                 display "                                " at 1001.

       002-caminho3.
           display "Informar o Caminho Arquivo Novo....: "
                at 0301
           accept acp-caminho3 at 0338

           if acp-caminho3 = spaces
              go to 002-caminho3.


           move acp-caminho3                     to path-crd020
           open i-o crd020

           if st-crd020 <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura..." x"0da0"
                     acp-caminho3 into mensagem
              move   "C" to tipo-msg
              perform exibir-mensagem.

           move acp-caminho2                     to path-crd020a
           open i-o crd020a

           if st-crd020a <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura..." x"0da0"
                     acp-caminho2 into mensagem
              move   "C" to tipo-msg
              perform exibir-mensagem.

           move acp-caminho1                     to path-crd020b
           open i-o crd020b

           if st-crd020b <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura..." x"0da0"
                     acp-caminho1 into mensagem
              move   "C" to tipo-msg
              perform exibir-mensagem.

           display "Lendo o Arquivo " at 0501
           display PATH-CRD020a       at 0517

           initialize reg-crd020a
                      qtd-titulos
                      qtd-titulos1
                      qtd-titulos2

                      qtde-erro1
                      qtde-erro2

           move "\programa\kello\999\crd020a\arquivo.txt"
             to caminho-arquivo

           open output arquivo.

           perform until st-crd020a = "10"
                 read crd020a next at end
                      move "10" to st-crd020a
                 not at end
                      display reg-crd020a  at 1001
                      if reg-crd020a = "A"
                         read crd020a next at end
                              move "10" to st-crd020a
                         not at end
                              if reg-crd020a = " "
                                 perform ler-267
                              end-if
                         end-read
                      end-if
                 end-read
           end-perform

           close arquivo


           display "Lendo o Arquivo " at 1701
           display PATH-CRD020b       at 1717

           move "\programa\kello\999\crd020a\arquivo2.txt"
             to caminho-arquivo

           open output arquivo.

           initialize reg-crd020b
           perform until st-crd020b = "10"
                 read crd020b next at end
                      move "10" to st-crd020b
                 not at end
                      display reg-crd020b  at 1901
                      if reg-crd020b = "A"
                         read crd020b next at end
                              move "10" to st-crd020b
                         not at end
                              if reg-crd020b = " "
                                 perform ler-267b
                              end-if
                         end-read
                      end-if
                 end-read
           end-perform.


           close arquivo.
      *          crd020.
      *
      *    move path-crd020a       to path-crd020
      *
      *    move "\programa\kello\999\crd020a\arquivo3.txt"
      *      to caminho-arquivo
      *    open output arquivo.
      *
      *    initialize reg-arquivo
      *               qtde-registro
      *
      *    open input crd020
      *    perform until st-crd020 = "10"
      *         read crd020 next at end
      *              move "10" to st-crd020
      *         not at end
      *              display "reg-crd020 = " reg-crd020
      *              add 1              to qtde-registro
      *              move qtde-registro to arquivo-linha
      *              move reg-crd020    to arquivo-registro
      *              write reg-arquivo
      *         end-read
      *    end-perform
      *
      *    close arquivo.

       006-sair.
           add qtd-titulos1  to qtd-titulos
           add qtd-titulos2  to qtd-titulos

           move qtd-titulos  to masc-qtde
           move qtd-titulos1 to masc-qtde1
           move qtd-titulos2 to masc-qtde2
           move qtde-registro to masc-qtde3

           move qtde-erro1   to masc-qtde4
           move qtde-erro2   to masc-qtde5

           move spaces to mensagem
           string "Quantidade de Registros Atualizados " masc-qtde
           x"0da0"
                  "Registro Original = " masc-qtde2
           x"0da0"
                  "Registro Backup = " masc-qtde1
           x"0da0"
                  "Registro Original Normal = " masc-qtde3
           x"0da0"
                  "Registro Qtde Erro1      = " masc-qtde4
           x"0da0"
                  "Registro Qtde Erro2      = " masc-qtde5
             into mensagem
           move   "C" to tipo-msg
           perform exibir-mensagem

           close crd020 crd020a crd020b

           STOP RUN.

       ler-267 section.
           initialize reg-crd020

           move 0 to ind
           perform until ind = 267
               add 1 to ind
               read crd020a next at end
                    move 267 to ind
                    move "10" to st-crd020a
               not at end
                    move reg-crd020a to reg-crd020(ind:1)
               end-read
           end-perform
           if DATA-MOVTO-CR20 > 0
              display "reg-crd020 = " reg-crd020
              write reg-crd020 not invalid key
                    add 1 to qtd-titulos1
                    move qtd-titulos1 to arquivo-linha
                    move reg-crd020   to arquivo-registro
                    write reg-arquivo
              end-write
           else
              add 1 to qtde-erro1.
       ler-267-fim.
           exit.

       ler-267b section.
           initialize reg-crd020

           move 0 to ind
           perform until ind = 267
               add 1 to ind
               read crd020b next at end
                    move 267 to ind
                    move "10" to st-crd020b
               not at end
                    move reg-crd020b to reg-crd020(ind:1)
               end-read
           end-perform
           if DATA-MOVTO-CR20 > 0
              display "reg-crd020 = " reg-crd020
              write reg-crd020 not invalid key
                    add 1 to qtd-titulos2
                    move qtd-titulos2 to arquivo-linha
                    move reg-crd020   to arquivo-registro
                    write reg-arquivo
              end-write
           else
              add 1 to qtde-erro2.
       ler-267b-fim.
           exit.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem
           move 1      to flag-critica.

       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCRD201Juntar.
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

           COPY CRPX201.

          SELECT CRD201a ASSIGN TO PATH-CRD201a
                  ORGANIZATION  IS   SEQUENTIAL
                  ACCESS MODE   IS   SEQUENTIAL
                  STATUS        IS   ST-CRD201a.

       DATA DIVISION.
       FILE SECTION.

           COPY CRPW201.

       FD CRD201a.
       01 REG-CRD201A                PIC X(01).



       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CRD201             PIC XX       VALUE SPACES.
           05  ST-CRD201A            PIC XX       VALUE SPACES.
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
           display "Informar o Caminho Arquivo Novo....: "
                at 0101
           accept acp-caminho1 at 0138

           if acp-caminho1 = spaces
              go to 001-caminho1.

       002-caminho2.
           display "Informar o Caminho Arquivo Atual...: "
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

           move acp-caminho1                     to path-crd201
           open i-o crd201

           if st-crd201 <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura..." x"0da0"
                     acp-caminho1 into mensagem
              move   "C" to tipo-msg
              perform exibir-mensagem.

           move acp-caminho2                     to path-crd201a
           open i-o crd201a

           if st-crd201a <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura..." x"0da0"
                     acp-caminho2 into mensagem
              move   "C" to tipo-msg
              perform exibir-mensagem.



           initialize reg-crd201a
           perform until st-crd201a = "10"
                 read crd201a next at end
                      move "10" to st-crd201a
                 not at end
                      display reg-crd201a  at 1001
                      if reg-crd201a = "@"
                         read crd201a next at end
                              move "10" to st-crd201a
                         not at end
                              if reg-crd201a = "`"
                                 perform ler-96
                              end-if
                         end-read
                      end-if
                 end-read
           end-perform.

       006-sair.
           display "acabei " stop " "
           close crd201 crd201a

           STOP RUN.

       ler-96 section.
           initialize reg-crd201

           move 0 to ind
           perform until ind = 96
               add 1 to ind
               read crd201a next at end
                    move 96 to ind
                    move "10" to st-crd201a
               not at end
                    move reg-crd201a to reg-crd201(ind:1)
               end-read
           end-perform
           if COD-COMPL-CR201 > 0
              display "reg-crd201 = " reg-crd201
              write reg-crd201.
       ler-267-fim.
           exit.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem
           move 1      to flag-critica.

       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOCXD100.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *DATA: 23-08-2010
      *DESCRIÇÃO: Conversão GALHOCXD100

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Utilitario         is class "utilitario"
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CXPX100.

           SELECT CXD100A ASSIGN TO PATH-CXD100A
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  STATUS IS ST-CXD100A
                  LOCK MODE IS AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY IS CHAVE-CX100A = DATA-MOV-CX100A,
                                               SEQ-CX100A
                  ALTERNATE RECORD KEY IS ALT-CX100A =
                                          CONTAPART-CX100A,
                            DATA-MOV-CX100A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT-CX200A
                              = CONTA-REDUZ-CX100A,
                            DATA-MOV-CX100A WITH DUPLICATES
                  ALTERNATE RECORD KEY IS ALT-CX300A = DATA-MOV-CX100A
                            SEQ-DESM-CX100A WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.

           COPY CXPW100.

       FD  CXD100A.
       01  REG-CXD100A.
           05  SEQ-CX100A              PIC 9(04).
           05  DATA-MOV-CX100A         PIC 9(08).
           05  TIPO-LCTO-CX100A        PIC 99.
      * Tipo-lcto < 50 - saídas     e tipo-lcto >= 50 - entradas
           05  HISTORICO-CX100A        PIC X(30).
           05  DOCUMENTO-CX100A        PIC X(10).
           05  VALOR-CX100A            PIC 9(10)V99.
           05  CONTAPART-CX100A        PIC 9(06).
           05  CONTA-REDUZ-CX100A      PIC 9(05).
           05  CONTABIL-CX100A         PIC 9.
      * será usado no futuro - para informar se o lancto é contábil
           05  RESPONSAVEL-CX100A      PIC X(5).
           05  SEQ-DESM-CX100A         PIC 9(4).
      * seq.p/determinar qual lançamento de caixa que gerou esse desmem-
      * bramento. Ex. Pagto de uma conta desdobrada, vai existir um
      * lançamento principal com o total, e outros com o desdobramento
      * sendo a seq-desm a mesma seq. que o lançamento principal, outro
      * exemplo o pagto de uma conta com juro, multa


       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CXD100             PIC XX       VALUE SPACES.
           05  ST-CXD100a            PIC XX       VALUE SPACES.
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
           05  ACP-DTINI             PIC 9(08)  VALUE ZEROS.
           05  ACP-DTFIM             PIC 9(08)  VALUE ZEROS.
           05  WS-OK                 PIC X(01)  VALUE SPACES.
           05  DATA-INI              PIC 9(08)  VALUE ZEROS.
           05  DATA-FIM              PIC 9(08)  VALUE ZEROS.
           05  RESP                  PIC X(01)  VALUE SPACES.

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
           display "Informar o Caminho Arquivo Original: "
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

           move acp-caminho1                     to path-cxd100
           open i-o cxd100

           if st-cxd100 <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura..." x"0da0"
                     acp-caminho1 into mensagem
              move   "C" to tipo-msg
              perform exibir-mensagem.

           move acp-caminho2                     to path-cxd100a
           open i-o cxd100a

           if st-cxd100a <> "00" and "05" and "35"
              move spaces to mensagem
              string "Erro de Abertura..." x"0da0"
                     acp-caminho2 into mensagem
              move   "C" to tipo-msg
              perform exibir-mensagem.


       003-data-ini.
           move zeros                          to acp-dtini
           display "Informar a Data Inicial: " at 0401
           accept acp-dtini                    at 0426

           if acp-dtini equal zeros
              move function current-date to ws-data-sys
              string ws-dia-cpu ws-mes-cpu ws-ano-cpu into acp-dtini
           else
              call   "UTIVLDT" using acp-dtini ws-ok
              cancel "UTIVLDT"
              if ws-ok equal "N"
                 display "Data Inicial Invalida" at 1001 stop " "
                 display "                     " at 1001
                 go to 003-data-ini
              end-if
           end-if

           string acp-dtini(5:4) acp-dtini(3:2) acp-dtini(1:2)
             into data-ini.

       004-data-fim.
           move zeros                          to acp-dtfim
           display "Informar a Data Final..: " at 0501
           accept acp-dtfim                    at 0526

           if acp-dtfim equal zeros
              move function current-date to ws-data-sys
              string ws-dia-cpu ws-mes-cpu ws-ano-cpu into acp-dtfim
           else
              call   "UTIVLDT" using acp-dtfim ws-ok
              cancel "UTIVLDT"
              if ws-ok equal "N"
                 display "Data Final Invalida"   at 1001 stop " "
                 display "                   "   at 1001
                 go to 004-data-fim
              end-if
           end-if

           string acp-dtfim(5:4) acp-dtfim(3:2) acp-dtfim(1:2)
             into data-fim

           if data-fim < data-ini
              display "Data Final Menor que a Inicial" at 1001 stop " "
              display "                              " at 1001
              go to 004-data-fim.


           if flag-critica = 0
              initialize reg-cxd100a
              move data-ini to data-mov-cx100a
              start cxd100a key is not less chave-cx100a invalid key
                    move "10" to st-cxd100a
              end-start
              perform until st-cxd100a = "10"
                    read cxd100a next at end
                         move "10" to st-cxd100a
                    not at end
                         if data-mov-cx100a > data-fim
                            move "10" to st-cxd100a
                         else
                            display reg-cxd100  at 1001
                            move reg-cxd100a to reg-cxd100
                            write reg-cxd100
                         end-if
                    end-read
              end-perform
           end-if.

       005-resp.
           move spaces to resp
           display "Deseja Continuar? " at 0801
           accept resp                  at 0819

           if resp = "S" or "s"
              go to 003-data-ini
           else
              if resp = "N" or "n"
                 go to 006-sair
              else
                 go to 005-resp.

       006-sair.
           close cxd100 cxd100a

           STOP RUN.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem
           move 1      to flag-critica.

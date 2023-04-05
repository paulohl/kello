       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO75.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 13-02-2007.
      *FUNÇÃO: arruma o cpd020

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           SELECT CHD041 ASSIGN TO PATH-CHD041
                  ORGANIZATION IS INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS NOME-41
                  ALTERNATE RECORD KEY IS DATA-BASE-41
                  WITH DUPLICATES
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-CHD041.

           SELECT CHD041A ASSIGN TO PATH-CHD041A
                  ORGANIZATION IS INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS NOME-41A
                  ALTERNATE RECORD KEY IS DATA-BASE-41A
                  WITH DUPLICATES
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-CHD041A.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

       FD  CHD041.
       01  REG-CHD041.
           05  NOME-41               PIC X(16).
           05  CARTEIRA-41           PIC 9(2) OCCURS 6 TIMES.
           05  DATA-BASE-41          PIC 9(8).
           05  TAXA-JUROS-41         PIC 99V99.
           05  DATA-INI-41           PIC 9(8).
           05  DATA-FIM-41           PIC 9(8).
           05  DIAS-INI-41           PIC 9(3).
           05  DIAS-FIM-41           PIC 9(3).
           05  FERIADOS-41           PIC 9(6) OCCURS 10 TIMES.
           05  QTDE-CHEQUES-41       PIC 9(4).
           05  VLR-BRUTO-41          PIC 9(10)V99.
           05  PM-41                 PIC 9(3)V99.
           05  DIAS-30-41            PIC 9(3)V99.
           05  TAXA-30-41            PIC 9V999999.
           05  JURO-30-41            PIC 9(10)V99.
           05  SALDO-30-41           PIC 9(10)V99.
           05  DIAS-60-41            PIC 9(3)V99.
           05  TAXA-60-41            PIC 9V999999.
           05  JURO-60-41            PIC 9(10)V99.
           05  SALDO-60-41           PIC 9(10)V99.
           05  DIAS-90-41            PIC 9(3)V99.
           05  TAXA-90-41            PIC 9V999999.
           05  JURO-90-41            PIC 9(10)V99.
           05  SALDO-90-41           PIC 9(10)V99.
           05  DIAS-120-41           PIC 9(3)V99.
           05  TAXA-120-41           PIC 9V999999.
           05  JURO-120-41           PIC 9(10)V99.
           05  SALDO-120-41          PIC 9(10)V99.
           05  DIAS-150-41           PIC 9(3)V99.
           05  TAXA-150-41           PIC 9V999999.
           05  JURO-150-41           PIC 9(10)V99.
           05  SALDO-150-41          PIC 9(10)V99.
           05  DIAS-180-41           PIC 9(3)V99.
           05  TAXA-180-41           PIC 9V999999.
           05  JURO-180-41           PIC 9(10)V99.
           05  SALDO-180-41          PIC 9(10)V99.
           05  DIAS-210-41           PIC 9(3)V99.
           05  TAXA-210-41           PIC 9V999999.
           05  JURO-210-41           PIC 9(10)V99.
           05  SALDO-210-41          PIC 9(10)V99.
           05  DIAS-240-41           PIC 9(3)V99.
           05  TAXA-240-41           PIC 9V999999.
           05  JURO-240-41           PIC 9(10)V99.
           05  SALDO-240-41          PIC 9(10)V99.
           05  DIAS-270-41           PIC 9(3)V99.
           05  TAXA-270-41           PIC 9V999999.
           05  JURO-270-41           PIC 9(10)V99.
           05  SALDO-270-41          PIC 9(10)V99.
           05  DIAS-300-41           PIC 9(3)V99.
           05  TAXA-300-41           PIC 9V999999.
           05  JURO-300-41           PIC 9(10)V99.
           05  SALDO-300-41          PIC 9(10)V99.
           05  DIAS-330-41           PIC 9(3)V99.
           05  TAXA-330-41           PIC 9V999999.
           05  JURO-330-41           PIC 9(10)V99.
           05  SALDO-330-41          PIC 9(10)V99.
           05  DIAS-360-41           PIC 9(3)V99.
           05  TAXA-360-41           PIC 9V999999.
           05  JURO-360-41           PIC 9(10)V99.
           05  SALDO-360-41          PIC 9(10)V99.
           05  DIAS-390-41           PIC 9(3)V99.
           05  TAXA-390-41           PIC 9V999999.
           05  JURO-390-41           PIC 9(10)V99.
           05  SALDO-390-41          PIC 9(10)V99.
           05  DIAS-420-41           PIC 9(3)V99.
           05  TAXA-420-41           PIC 9V999999.
           05  JURO-420-41           PIC 9(10)V99.
           05  SALDO-420-41          PIC 9(10)V99.
           05  DIAS-450-41           PIC 9(3)V99.
           05  TAXA-450-41           PIC 9V999999.
           05  JURO-450-41           PIC 9(10)V99.
           05  SALDO-450-41          PIC 9(10)V99.
           05  DIAS-480-41           PIC 9(3)V99.
           05  TAXA-480-41           PIC 9V999999.
           05  JURO-480-41           PIC 9(10)V99.
           05  SALDO-480-41          PIC 9(10)V99.
           05  DIAS-510-41           PIC 9(3)V99.
           05  TAXA-510-41           PIC 9V999999.
           05  JURO-510-41           PIC 9(10)V99.
           05  SALDO-510-41          PIC 9(10)V99.
           05  DIAS-540-41           PIC 9(3)V99.
           05  TAXA-540-41           PIC 9V999999.
           05  JURO-540-41           PIC 9(10)V99.
           05  SALDO-540-41          PIC 9(10)V99.
           05  PORTADOR-DESTINO-41   PIC 9(03).

       FD  CHD041A.
       01  REG-CHD041A.
           05  NOME-41A              PIC X(16).
           05  CARTEIRA-41A          PIC 9(2) OCCURS 6 TIMES.
           05  DATA-BASE-41A         PIC 9(8).
           05  TAXA-JUROS-41A        PIC 99V99.
           05  DATA-INI-41A          PIC 9(8).
           05  DATA-FIM-41A          PIC 9(8).
           05  DIAS-INI-41A          PIC 9(3).
           05  DIAS-FIM-41A          PIC 9(3).
           05  FERIADOS-41A          PIC 9(6) OCCURS 10 TIMES.
           05  QTDE-CHEQUES-41A      PIC 9(4).
           05  VLR-BRUTO-41A         PIC 9(10)V99.
           05  PM-41A                PIC 9(3)V99.
           05  DIAS-30-41A           PIC 9(3)V99.
           05  TAXA-30-41A           PIC 9V999999.
           05  JURO-30-41A           PIC 9(10)V99.
           05  SALDO-30-41A          PIC 9(10)V99.
           05  DIAS-60-41A           PIC 9(3)V99.
           05  TAXA-60-41A           PIC 9V999999.
           05  JURO-60-41A           PIC 9(10)V99.
           05  SALDO-60-41A          PIC 9(10)V99.
           05  DIAS-90-41A           PIC 9(3)V99.
           05  TAXA-90-41A           PIC 9V999999.
           05  JURO-90-41A           PIC 9(10)V99.
           05  SALDO-90-41A          PIC 9(10)V99.
           05  DIAS-120-41A          PIC 9(3)V99.
           05  TAXA-120-41A          PIC 9V999999.
           05  JURO-120-41A          PIC 9(10)V99.
           05  SALDO-120-41A         PIC 9(10)V99.
           05  DIAS-150-41A          PIC 9(3)V99.
           05  TAXA-150-41A          PIC 9V999999.
           05  JURO-150-41A          PIC 9(10)V99.
           05  SALDO-150-41A         PIC 9(10)V99.
           05  DIAS-180-41A          PIC 9(3)V99.
           05  TAXA-180-41A          PIC 9V999999.
           05  JURO-180-41A          PIC 9(10)V99.
           05  SALDO-180-41A         PIC 9(10)V99.
           05  PORTADOR-DESTINO-41A PIC 9(03).

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CHD041             PIC XX       VALUE SPACES.
           05  ST-CHD041A            PIC XX       VALUE SPACES.
           05  ST-CAD001             PIC XX       VALUE SPACES.
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

           05  CUSTO-PREVISTO-W      PIC 9(8)V99  VALUE ZEROS.
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
           05  VLR-PARCELA           PIC 9(8)V99  VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  CLASSIF-W             PIC 9        VALUE ZEROS.
           05  COL1                  PIC 9(3)     VALUE ZEROS.
           05  ANOTACAO-W            PIC X(80)    VALUE SPACES.
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  AUX-ALBUM             PIC 9(08)    VALUE ZEROS.
           05  ACP-DATA-MOVTO        PIC 9(08)    VALUE ZEROS.
           05  ACP-SEQUENCIA         PIC 9(04)    VALUE ZEROS.
           05  ACP-CODRED            PIC 9(03)    VALUE ZEROS.
           05  CONTINUAR             PIC X(01)    VALUE SPACES.
           05  MASC-DATA             PIC 99/99/9999 VALUE ZEROS.
           05  MENSAGEM              PIC X(200).
           05  TIPO-MSG              PIC X(01).
           05  RESP-MSG              PIC X(01).
           05  STATUS-CODE           PIC X(02) COMP-5.
           COPY "PARAMETR".

       01  LINDET.
           05  LINDET-REL          PIC X(130)  VALUE SPACES.

       01  CAB04.
           05  FILLER              PIC X(130)  VALUE
           "CONTRATO    ITEM    CURSO    TURMA".


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           display erase at 0101.

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
                      perform atualizar-valores
                      perform fechar-arquivos

                      display "ACABEI ESSA EMPRESA" STOP " "
                 end-read
           end-perform

           close cad001

           DISPLAY "ACABOU" STOP "  ".

           STOP " "

           stop run.

       abrir-arquivos section.
           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CHD041"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CHD041

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CHD041A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CHD041A


           call "CBL_RENAME_FILE" using PATH-CHD041
                                        PATH-CHD041A
                              returning status-code

           STRING PATH-CHD041 ".idx" DELIMITED BY " " INTO PATH-CHD041

           STRING PATH-CHD041A ".idx" DELIMITED BY " " INTO PATH-CHD041A

           call "CBL_RENAME_FILE" using PATH-CHD041
                                        PATH-CHD041A
                              returning status-code.

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CHD041"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CHD041

           open i-o   chd041
           close      chd041
           open i-o   chd041

           MOVE CODIGO-CA001           TO EMP-REC
           MOVE "CHD041A"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CHD041A

           OPEN INPUT CHD041A.

       atualizar-valores section.
           initialize reg-chd041A
           start chd041A key is not less nome-41A invalid key
                 move "10" to st-chd041A.
           perform until st-chd041A = "10"
               read chd041A next at end
                    move "10" to st-chd041A
               not at end
                    initialize reg-chd041
                    move NOME-41A               to NOME-41
                    move CARTEIRA-41A(1)        to CARTEIRA-41(1)
                    move CARTEIRA-41A(2)        to CARTEIRA-41(2)
                    move CARTEIRA-41A(3)        to CARTEIRA-41(3)
                    move CARTEIRA-41A(4)        to CARTEIRA-41(4)
                    move CARTEIRA-41A(5)        to CARTEIRA-41(5)
                    move CARTEIRA-41A(6)        to CARTEIRA-41(6)
                    move DATA-BASE-41A          to DATA-BASE-41
                    move TAXA-JUROS-41A         to TAXA-JUROS-41
                    move DATA-INI-41A           to DATA-INI-41
                    move DATA-FIM-41A           to DATA-FIM-41
                    move DIAS-INI-41A           to DIAS-INI-41
                    move DIAS-FIM-41A           to DIAS-FIM-41
                    move FERIADOS-41A(1)        to FERIADOS-41(1)
                    move FERIADOS-41A(2)        to FERIADOS-41(2)
                    move FERIADOS-41A(3)        to FERIADOS-41(3)
                    move FERIADOS-41A(4)        to FERIADOS-41(4)
                    move FERIADOS-41A(5)        to FERIADOS-41(5)
                    move FERIADOS-41A(6)        to FERIADOS-41(6)
                    move QTDE-CHEQUES-41A       to QTDE-CHEQUES-41
                    move VLR-BRUTO-41A          to VLR-BRUTO-41
                    move PM-41A                 to PM-41
                    move DIAS-30-41A            to DIAS-30-41
                    move TAXA-30-41A            to TAXA-30-41
                    move JURO-30-41A            to JURO-30-41
                    move SALDO-30-41A           to SALDO-30-41
                    move DIAS-60-41A            to DIAS-60-41
                    move TAXA-60-41A            to TAXA-60-41
                    move JURO-60-41A            to JURO-60-41
                    move SALDO-60-41A           to SALDO-60-41
                    move DIAS-90-41A            to DIAS-90-41
                    move TAXA-90-41A            to TAXA-90-41
                    move JURO-90-41A            to JURO-90-41
                    move SALDO-90-41A           to SALDO-90-41
                    move DIAS-120-41A           to DIAS-120-41
                    move TAXA-120-41A           to TAXA-120-41
                    move JURO-120-41A           to JURO-120-41
                    move SALDO-120-41A          to SALDO-120-41
                    move DIAS-150-41A           to DIAS-150-41
                    move TAXA-150-41A           to TAXA-150-41
                    move JURO-150-41A           to JURO-150-41
                    move SALDO-150-41A          to SALDO-150-41
                    move DIAS-180-41A           to DIAS-180-41
                    move TAXA-180-41A           to TAXA-180-41
                    move JURO-180-41A           to JURO-180-41
                    move SALDO-180-41A          to SALDO-180-41
                    move PORTADOR-DESTINO-41A   to PORTADOR-DESTINO-41

                    display reg-chd041A
                    write reg-chd041 invalid key
                      display "Erro de Regravação...CHD041"
                      stop " "
                    end-write
               end-read
           end-perform.

       fechar-arquivos section.
           close chd041a chd041.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.


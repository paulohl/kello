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

           COPY CPPX020.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY CPPW020.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CPD020             PIC XX       VALUE SPACES.
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
           MOVE "CPD020"               TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CPD020

           open i-o   cpd020
           close      cpd020
           open i-o   cpd020.

       atualizar-valores section.
           initialize reg-cpd020
           move 2                          to situacao-cp20
           move 123                        to fornec-cp20
      *    move 20061201                   to data-vencto-cp20

           start cpd020 key is not less alt4-cp20 invalid key
               move "10" to st-cpd020.
           perform until st-cpd020 = "10"
               read cpd020 next at end
                   move "10" to st-cpd020
               not at end
                   if situacao-cp20   <> 2        or
                      fornec-cp20     <> 123
                      move "10" to st-cpd020
                   else
                      if data-pgto-cp20 > 0
      *                  move zeros to valor-tot-cp20
      *                                juro-pago-cp20
      *                                multa-paga-cp20
      *                                desconto-cp20
                         compute valor-liq-cp20 = valor-tot-cp20 +
                                                  juro-pago-cp20 +
                                                  multa-paga-cp20 -
                                                  desconto-cp20
                         display reg-cpd020
                         rewrite reg-cpd020 invalid key
                           display "Erro de Regravação...CPD020"
                           stop " "
                         end-rewrite
                      end-if
                   end-if
               end-read
           end-perform.

       fechar-arquivos section.
           close cpd020.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.


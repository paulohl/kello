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

           SELECT CHD041i ASSIGN TO PATH-CHD041i
                  ORGANIZATION IS INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS CHAVE-41i = NOME-41i
                                            PARCELA-41i
                  WITH DUPLICATES
                  LOCK MODE IS AUTOMATIC
                  WITH LOCK ON RECORD
                  STATUS IS ST-CHD041i.

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

       FD  CHD041i.
       01  REG-CHD041i.
           05  NOME-41i              PIC X(16).
           05  PARCELA-41i           PIC 9(03).
           05  DIAS-41i              PIC 9(3)V99.
           05  TAXA-41i              PIC 9V999999.
           05  JURO-41i              PIC 9(10)V99.
           05  SALDO-41i             PIC 9(10)V99.

       WORKING-STORAGE SECTION.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CHD041             PIC XX       VALUE SPACES.
           05  ST-CHD041i            PIC XX       VALUE SPACES.
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

       01  masc-valor                pic zz.zzz.zz9,99 value zeros.
       01  masc-juros                pic zz9,99 value zeros.
       01  masc-dias                 pic zz99.
       01  masc-dias2                pic zz9,999 value zeros.

       01  atualizados               pic 9(04) value zeros.
       01  masc-qtde                 pic z.zz9.


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
           MOVE "CHD041i"              TO ARQ-REC
           MOVE EMPRESA-REF            TO PATH-CHD041i

           open i-o   chd041 chd041i
           close      chd041 chd041i
           open i-o   chd041 chd041i.

       atualizar-valores section.
           initialize reg-chd041
                      atualizados
           start chd041 key is not less nome-41 invalid key
                 move "10" to st-chd041.

           perform until st-chd041 = "10"
                 read chd041 next at end
                      move "10" to st-chd041
                 not at end
                      display "reg-chd041 = " reg-chd041

                      if dias-30-41 is not numeric
                         move 0 to dias-30-41
                      end-if
                      if dias-30-41 > 0
                         move nome-41 to nome-41i
                         move 1       to parcela-41i
                         read chd041i invalid key
                              move dias-30-41   to dias-41i
                              move taxa-30-41   to taxa-41i
                              move juro-30-41   to juro-41i
                              move saldo-30-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-30-41 > 0
                                 if dias-30-41  <> dias-41i or
                                    taxa-30-41  <> taxa-41i or
                                    juro-30-41  <> juro-41i or
                                    saldo-30-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-30-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-30-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-30-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-30-41   to dias-41i
                                    move taxa-30-41   to taxa-41i
                                    move juro-30-41   to juro-41i
                                    move saldo-30-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-60-41 is not numeric
                         move 0 to dias-60-41
                      end-if
                      if dias-60-41 > 0
                         move nome-41 to nome-41i
                         move 2       to parcela-41i
                         read chd041i invalid key
                              move dias-60-41   to dias-41i
                              move taxa-60-41   to taxa-41i
                              move juro-60-41   to juro-41i
                              move saldo-60-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-60-41 > 0
                                 if dias-60-41  <> dias-41i or
                                    taxa-60-41  <> taxa-41i or
                                    juro-60-41  <> juro-41i or
                                    saldo-60-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-60-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-60-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-60-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-60-41   to dias-41i
                                    move taxa-60-41   to taxa-41i
                                    move juro-60-41   to juro-41i
                                    move saldo-60-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-90-41 is not numeric
                         move 0 to dias-90-41
                      end-if
                      if dias-90-41 > 0
                         move nome-41 to nome-41i
                         move 3       to parcela-41i
                         read chd041i invalid key
                              move dias-90-41   to dias-41i
                              move taxa-90-41   to taxa-41i
                              move juro-90-41   to juro-41i
                              move saldo-90-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-90-41 > 0
                                 if dias-90-41  <> dias-41i or
                                    taxa-90-41  <> taxa-41i or
                                    juro-90-41  <> juro-41i or
                                    saldo-90-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-90-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-90-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-90-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-90-41   to dias-41i
                                    move taxa-90-41   to taxa-41i
                                    move juro-90-41   to juro-41i
                                    move saldo-90-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-120-41 is not numeric
                         move 0 to dias-120-41
                      end-if
                      if dias-120-41 > 0
                         move nome-41 to nome-41i
                         move 4       to parcela-41i
                         read chd041i invalid key
                              move dias-120-41   to dias-41i
                              move taxa-120-41   to taxa-41i
                              move juro-120-41   to juro-41i
                              move saldo-120-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-120-41 > 0
                                 if dias-120-41  <> dias-41i or
                                    taxa-120-41  <> taxa-41i or
                                    juro-120-41  <> juro-41i or
                                    saldo-120-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-120-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-120-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-120-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-120-41   to dias-41i
                                    move taxa-120-41   to taxa-41i
                                    move juro-120-41   to juro-41i
                                    move saldo-120-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-150-41 is not numeric
                         move 0 to dias-150-41
                      end-if
                      if dias-150-41 > 0
                         move nome-41 to nome-41i
                         move 5       to parcela-41i
                         read chd041i invalid key
                              move dias-150-41   to dias-41i
                              move taxa-150-41   to taxa-41i
                              move juro-150-41   to juro-41i
                              move saldo-150-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-150-41 > 0
                                 if dias-150-41  <> dias-41i or
                                    taxa-150-41  <> taxa-41i or
                                    juro-150-41  <> juro-41i or
                                    saldo-150-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-150-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-150-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-150-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-150-41   to dias-41i
                                    move taxa-150-41   to taxa-41i
                                    move juro-150-41   to juro-41i
                                    move saldo-150-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-180-41 is not numeric
                         move 0 to dias-180-41
                      end-if
                      if dias-180-41 > 0
                         move nome-41 to nome-41i
                         move 6       to parcela-41i
                         read chd041i invalid key
                              move dias-180-41   to dias-41i
                              move taxa-180-41   to taxa-41i
                              move juro-180-41   to juro-41i
                              move saldo-180-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-180-41 > 0
                                 if dias-180-41  <> dias-41i or
                                    taxa-180-41  <> taxa-41i or
                                    juro-180-41  <> juro-41i or
                                    saldo-180-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-180-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-180-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-180-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-180-41   to dias-41i
                                    move taxa-180-41   to taxa-41i
                                    move juro-180-41   to juro-41i
                                    move saldo-180-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-210-41 is not numeric
                         move 0 to dias-210-41
                      end-if
                      if dias-210-41 > 0
                         move nome-41 to nome-41i
                         move 7       to parcela-41i
                         read chd041i invalid key
                              move dias-210-41   to dias-41i
                              move taxa-210-41   to taxa-41i
                              move juro-210-41   to juro-41i
      *>problema
      *                       move saldo-120-41  to saldo-41i
      *>estava movendo o saldo de 120 dias ao inves do 210
                              move saldo-210-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-210-41 > 0
                                 if dias-210-41  <> dias-41i or
                                    taxa-210-41  <> taxa-41i or
                                    juro-210-41  <> juro-41i or
                                    saldo-210-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-210-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-210-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-210-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-210-41   to dias-41i
                                    move taxa-210-41   to taxa-41i
                                    move juro-210-41   to juro-41i
      *>problema
      *                             move saldo-120-41  to saldo-41i
      *>estava movendo o saldoooo de 120 dias ao inves do 210
                                    move saldo-210-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-240-41 is not numeric
                         move 0 to dias-240-41
                      end-if
                      if dias-240-41 > 0
                         move nome-41 to nome-41i
                         move 8       to parcela-41i
                         read chd041i invalid key
                              move dias-240-41   to dias-41i
                              move taxa-240-41   to taxa-41i
                              move juro-240-41   to juro-41i
                              move saldo-240-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-240-41 > 0
                                 if dias-240-41  <> dias-41i or
                                    taxa-240-41  <> taxa-41i or
                                    juro-240-41  <> juro-41i or
                                    saldo-240-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-240-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-240-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-240-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-240-41   to dias-41i
                                    move taxa-240-41   to taxa-41i
                                    move juro-240-41   to juro-41i
                                    move saldo-240-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-270-41 is not numeric
                         move 0 to dias-270-41
                      end-if
                      if dias-270-41 > 0
                         move nome-41 to nome-41i
                         move 9       to parcela-41i
                         read chd041i invalid key
                              move dias-270-41   to dias-41i
                              move taxa-270-41   to taxa-41i
                              move juro-270-41   to juro-41i
                              move saldo-270-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-270-41 > 0
                                 if dias-270-41  <> dias-41i or
                                    taxa-270-41  <> taxa-41i or
                                    juro-270-41  <> juro-41i or
                                    saldo-270-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-270-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-270-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-270-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-270-41   to dias-41i
                                    move taxa-270-41   to taxa-41i
                                    move juro-270-41   to juro-41i
                                    move saldo-270-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-300-41 is not numeric
                         move 0 to dias-300-41
                      end-if
                      if dias-300-41 > 0
                         move nome-41 to nome-41i
                         move 10      to parcela-41i
                         read chd041i invalid key
                              move dias-300-41   to dias-41i
                              move taxa-300-41   to taxa-41i
                              move juro-300-41   to juro-41i
                              move saldo-300-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-300-41 > 0
                                 if dias-300-41  <> dias-41i or
                                    taxa-300-41  <> taxa-41i or
                                    juro-300-41  <> juro-41i or
                                    saldo-300-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-300-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-300-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-300-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-300-41   to dias-41i
                                    move taxa-300-41   to taxa-41i
                                    move juro-300-41   to juro-41i
                                    move saldo-300-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-330-41 is not numeric
                         move 0 to dias-330-41
                      end-if
                      if dias-330-41 > 0
                         move nome-41 to nome-41i
                         move 11      to parcela-41i
                         read chd041i invalid key
                              move dias-330-41   to dias-41i
                              move taxa-330-41   to taxa-41i
                              move juro-330-41   to juro-41i
                              move saldo-330-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-330-41 > 0
                                 if dias-330-41  <> dias-41i or
                                    taxa-330-41  <> taxa-41i or
                                    juro-330-41  <> juro-41i or
                                    saldo-330-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-330-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-330-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-330-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-330-41   to dias-41i
                                    move taxa-330-41   to taxa-41i
                                    move juro-330-41   to juro-41i
                                    move saldo-330-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-360-41 is not numeric
                         move 0 to dias-360-41
                      end-if
                      if dias-360-41 > 0
                         move nome-41 to nome-41i
                         move 12      to parcela-41i
                         read chd041i invalid key
                              move dias-360-41   to dias-41i
                              move taxa-360-41   to taxa-41i
                              move juro-360-41   to juro-41i
                              move saldo-360-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-360-41 > 0
                                 if dias-360-41  <> dias-41i or
                                    taxa-360-41  <> taxa-41i or
                                    juro-360-41  <> juro-41i or
                                    saldo-360-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-360-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-360-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-360-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-360-41   to dias-41i
                                    move taxa-360-41   to taxa-41i
                                    move juro-360-41   to juro-41i
                                    move saldo-360-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-390-41 is not numeric
                         move 0 to dias-390-41
                      end-if
                      if dias-390-41 > 0
                         move nome-41 to nome-41i
                         move 13      to parcela-41i
                         read chd041i invalid key
                              move dias-390-41   to dias-41i
                              move taxa-390-41   to taxa-41i
                              move juro-390-41   to juro-41i
                              move saldo-390-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-390-41 > 0
                                 if dias-390-41  <> dias-41i or
                                    taxa-390-41  <> taxa-41i or
                                    juro-390-41  <> juro-41i or
                                    saldo-390-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-390-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-390-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-390-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-390-41   to dias-41i
                                    move taxa-390-41   to taxa-41i
                                    move juro-390-41   to juro-41i
                                    move saldo-390-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-420-41 is not numeric
                         move 0 to dias-420-41
                      end-if
                      if dias-420-41 > 0
                         move nome-41 to nome-41i
                         move 14      to parcela-41i
                         read chd041i invalid key
                              move dias-420-41   to dias-41i
                              move taxa-420-41   to taxa-41i
                              move juro-420-41   to juro-41i
                              move saldo-420-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-420-41 > 0
                                 if dias-420-41  <> dias-41i or
                                    taxa-420-41  <> taxa-41i or
                                    juro-420-41  <> juro-41i or
                                    saldo-420-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-420-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-420-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-420-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-420-41   to dias-41i
                                    move taxa-420-41   to taxa-41i
                                    move juro-420-41   to juro-41i
                                    move saldo-420-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-450-41 is not numeric
                         move 0 to dias-450-41
                      end-if
                      if dias-450-41 > 0
                         move nome-41 to nome-41i
                         move 15      to parcela-41i
                         read chd041i invalid key
                              move dias-450-41   to dias-41i
                              move taxa-450-41   to taxa-41i
                              move juro-450-41   to juro-41i
                              move saldo-450-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-450-41 > 0
                                 if dias-450-41  <> dias-41i or
                                    taxa-450-41  <> taxa-41i or
                                    juro-450-41  <> juro-41i or
                                    saldo-450-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-450-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-450-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-450-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-450-41   to dias-41i
                                    move taxa-450-41   to taxa-41i
                                    move juro-450-41   to juro-41i
                                    move saldo-450-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-480-41 is not numeric
                         move 0 to dias-480-41
                      end-if
                      if dias-480-41 > 0
                         move nome-41 to nome-41i
                         move 16      to parcela-41i
                         read chd041i invalid key
                              move dias-480-41   to dias-41i
                              move taxa-480-41   to taxa-41i
                              move juro-480-41   to juro-41i
                              move saldo-480-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-480-41 > 0
                                 if dias-480-41  <> dias-41i or
                                    taxa-480-41  <> taxa-41i or
                                    juro-480-41  <> juro-41i or
                                    saldo-480-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-480-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-480-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-480-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-480-41   to dias-41i
                                    move taxa-480-41   to taxa-41i
                                    move juro-480-41   to juro-41i
                                    move saldo-480-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-510-41 is not numeric
                         move 0 to dias-510-41
                      end-if
                      if dias-510-41 > 0
                         move nome-41 to nome-41i
                         move 17      to parcela-41i
                         read chd041i invalid key
                              move dias-510-41   to dias-41i
                              move taxa-510-41   to taxa-41i
                              move juro-510-41   to juro-41i
                              move saldo-510-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-510-41 > 0
                                 if dias-510-41  <> dias-41i or
                                    taxa-510-41  <> taxa-41i or
                                    juro-510-41  <> juro-41i or
                                    saldo-510-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-510-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-510-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-510-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-510-41   to dias-41i
                                    move taxa-510-41   to taxa-41i
                                    move juro-510-41   to juro-41i
                                    move saldo-510-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if

                      if dias-540-41 is not numeric
                         move 0 to dias-540-41
                      end-if
                      if dias-540-41 > 0
                         move nome-41 to nome-41i
                         move 18      to parcela-41i
                         read chd041i invalid key
                              move dias-540-41   to dias-41i
                              move taxa-540-41   to taxa-41i
                              move juro-540-41   to juro-41i
                              move saldo-540-41  to saldo-41i
                              perform gravar-chd041i
                         not invalid key
                              if dias-540-41 > 0
                                 if dias-540-41  <> dias-41i or
                                    taxa-540-41  <> taxa-41i or
                                    juro-540-41  <> juro-41i or
                                    saldo-540-41 <> saldo-41i
                                    display erase at 0101

                                    compute masc-dias = parcela-41i * 30

                                    display masc-dias       at 0101
                                    display "Dias"          at 0105
                                    move dias-41i      to masc-dias2
                                    display masc-dias2      at 0115

                                    display "Taxa41....: "  at 0201
                                    move taxa-540-41    to masc-juros
                                    display masc-juros      at 0213
                                    display "Taxa41i...: "  at 0225
                                    move taxa-41i      to masc-juros
                                    display masc-juros      at 0237

                                    display "Juro41....: "  at 0301
                                    move juro-540-41    to masc-juros
                                    display masc-juros      at 0313
                                    display "Taxa41i...: "  at 0325
                                    move juro-41i      to masc-juros
                                    display masc-juros      at 0337

                                    display "Sald41....: "  at 0401
                                    move saldo-540-41    to masc-valor
                                    display masc-valor      at 0413
                                    display "Sald41i...: "  at 0425
                                    move saldo-41i      to masc-valor
                                    display masc-valor      at 0437

                                    move dias-540-41   to dias-41i
                                    move taxa-540-41   to taxa-41i
                                    move juro-540-41   to juro-41i
                                    move saldo-540-41  to saldo-41i
                                    perform regravar-chd041i
                                 end-if
                              end-if
                         end-read
                      end-if
                 end-read
           end-perform

           display erase at 0101
           move atualizados  to masc-qtde
           display masc-qtde at 0101
           display "Atualizados" at 0115

           stop  "  " .

       gravar-chd041i section.
           move nome-41 to nome-41i
           write reg-chd041i invalid key
               move "Erro de Gravacao...CHD041i" to mensagem
               move "C" to tipo-msg
               perform exibir-mensagem
           end-write.

       regravar-chd041i section.
           add 1 to atualizados
           display "Nome: " at 0801
           display nome-41  at 0810
           move nome-41 to nome-41i
           rewrite reg-chd041i invalid key
               move "Erro de Regravacao...CHD041i" to mensagem
               move "C" to tipo-msg
               perform exibir-mensagem
           end-rewrite.

       fechar-arquivos section.
           close chd041i chd041.

       EXIBIR-MENSAGEM SECTION.
           MOVE    SPACES TO RESP-MSG.
           CALL    "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL  "MENSAGEM".
           move spaces to mensagem.


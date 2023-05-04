       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO69.
      *DATA: 05-10-2006.
      *AUTOR: ALFREDO SAVIOLLI NETO
      *PROGRAMA: LE LOG DO MTD019
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX001.

           COPY LOGX002.

           COPY MTPX019.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY LOGW002.

           COPY MTPW019.

       WORKING-STORAGE SECTION.
           COPY "CBDATA.CPY".
           COPY "CPTIME.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBPRINT.CPY".

       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(60).
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-LOG002             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ACP-CONTRATO          PIC 9(04)    VALUE ZEROS.
           05  ACP-ALBUM             PIC 9(04)    VALUE ZEROS.
           05  DATA-LOG              PIC X(10)    VALUE SPACES.
           05  HORARIO               PIC X(08)    VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  LIN                   PIC 9(02)    VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  MESANOW.
               10  MES-WW            PIC 99.
               10  ANO-WW            PIC 9999.
           05  MESANO-W REDEFINES MESANOW PIC 9(6).
           05  MESANOI.
               10  ANO-WI            PIC 9999.
               10  MES-WI            PIC 99.
           05  MESANO-I REDEFINES MESANOI PIC 9(6).
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  CARACTERISTICA-W      PIC 9        VALUE ZEROS.
      *    P/ SABER QUAL O TIPO-LCTO SELECIONADO
           05  IMPRIME-W             PIC 9        VALUE ZEROS.
      *    FLAG P/ IDENTIFICAR QUAIS NOMES FAZEM PARTE DA CARACTERISTICA
      *    SELECIONADA
           05  GRAVA1-REGRAVA2       PIC 9        VALUE ZEROS.
           05  DATA-E                PIC 99/99/9999 BLANK WHEN ZEROS.
           05  MESANO-E              PIC 99/9999  BLANK WHEN ZEROS.
           05  TIPO-LCTO-W           PIC 9        VALUE ZEROS.
           05  VALOR-E               PIC ZZZZZ.ZZZ,ZZ BLANK WHEN ZEROS.
           05  TOTAL-W               PIC 9(8)V99  VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  VENCTO-INV            PIC 9(8)     VALUE ZEROS.
           05  OPERACAO              PIC X(01)    VALUE SPACES.
           COPY "PARAMETR".
       01  CAB01.
           05  EMPRESA-REL         PIC X(40)   VALUE SPACES.
           05  FILLER              PIC X(12)   VALUE "EMISSAO/HR: ".
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.
           05  FILLER              PIC X       VALUE SPACES.
           05  HORA-REL            PIC X(10)   VALUE "  :  ".
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PG-REL              PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(37)   VALUE
           "RELACAO DE CREDITOS DE REPORTAGEM".
           05  FILLER              PIC X(09)   VALUE "MES/ANO: ".
           05  MESANO-REL          PIC 99/9999.
           05  FILLER              PIC X(09)   VALUE SPACES.
           05  FILLER              PIC X(08)   VALUE 'VENCTO: '.
           05  VENCTO-REL          PIC 99/99/9999.
       01  CAB03.
           05  FILLER              PIC X(80)   VALUE ALL "=".
       01  CAB04.
           05  FILLER              PIC X(80)   VALUE
           "FUNCIONARIO                     TOTAL-CREDITOS".
       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           MOVE ZEROS TO PAG-W ERRO-W.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC
           MOVE "LOG002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-LOG002
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019

           OPEN I-O   LOG002 MTD019
           CLOSE             MTD019
           OPEN INPUT        MTD019

           CLOSE CONTROLE.
           IF ST-LOG002 = "35"
              CLOSE LOG002    OPEN OUTPUT LOG002
              CLOSE LOG002    OPEN I-O    LOG002
           END-IF.

           display erase at 0101

           display "Informar o Numero do Contrato: " at 0101
           accept acp-contrato                       at 0132

           display "Informar o Numero do Album...: " at 0201
           accept acp-album                          at 0232

           display "Informar a Operacao Desejada.: " at 0301
           accept operacao                           at 0332


           INITIALIZE REG-LOG002
           MOVE "MTD019" TO LOG2-ARQUIVO
           START LOG002 KEY IS NOT LESS LOG2-CH-ARQUIVO INVALID KEY
                 MOVE "10" TO ST-LOG002.

           PERFORM UNTIL ST-LOG002 = "10"
                 READ LOG002 NEXT RECORD AT END
                      MOVE "10" TO ST-LOG002
                 NOT AT END
                      IF LOG2-OPERACAO = OPERACAO
                         IF LOG2-PROGRAMA = "MTP019"
                            MOVE LOG2-REGISTRO TO REG-MTD019

                            IF CONTRATO-MT19 = ACP-CONTRATO
                               IF ACP-ALBUM = 0 OR SEQ-MT19

                                  display erase at 0601

                                  STRING LOG2-DIA "/"
                                         LOG2-MES "/"
                                         LOG2-ANO INTO DATA-LOG

                                  STRING LOG2-HORA ":"
                                         LOG2-MINU ":"
                                         LOG2-SEGU INTO HORARIO

                                  DISPLAY "Album...: "    AT 0601
                                  DISPLAY ALBUMMT19       AT 0611
                                  DISPLAY "Usuario.: "    AT 0701
                                  DISPLAY LOG2-USUARIO    AT 0711
                                  DISPLAY "Data....: "    AT 0801
                                  DISPLAY DATA-LOG        AT 0811
                                  DISPLAY "Hora....: "    AT 0901
                                  DISPLAY HORARIO         AT 0911

                                  STOP " "

                               END-IF
                            END-IF

                         END-IF
                      END-IF
                 END-READ
           END-PERFORM


           DISPLAY "ACABOU DE EXECUTAR O GALHO69" STOP " ".

           CLOSE LOG002 MTD019
           EXIT PROGRAM
           STOP RUN.


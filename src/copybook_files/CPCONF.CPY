
      ******************************************************************
      *          CONFIguracao do COBOLware 3.0                         *
      ******************************************************************

       FD  CONFIG
           LABEL RECORD IS STANDARD
           RECORDING MODE IS V
           RECORD CONTAINS 32 TO 1956 CHARACTERS
           VALUE OF FILE-ID IS LB-CONFIG.

       01  CONFIG-REG.
           05 CONFIG-CHAVE.
              10 CONFIG-TIPO                        PIC  X(002).
              10 CONFIG-ELEMENTO                    PIC  X(030).

       01  CONFIG-REG00.
           05 FILLER                                PIC  X(032).
           05 CONFIG-FATOR-00                COMP-X PIC  9(002).
           05 CONFIG-USERID.
              10 CONFIG-USUARIO                     PIC  X(030).
              10 CONFIG-SISTEMA                     PIC  X(030).
           05 REDEFINES CONFIG-USERID.
              10 CONFIG-BYTE-U     OCCURS 60 COMP-X PIC  9(002).
           05 CONFIG-QUADRO                         PIC  9(001).
           05 CONFIG-EJECT-MODE-OLD                 PIC  X(002).

       01  CONFIG-REG02.
           05 FILLER                                PIC  X(002).
           05 CONFIG-ARQUIVO                        PIC  X(030).
           05 CONFIG-LABEL.
              10 CONFIG-LABEL-BYTE OCCURS 50        PIC  X(001).
           05 CONFIG-EJECT-MODE                     PIC  X(002).
           05 CONFIG-ASCII                          PIC  X(001).
           05 CONFIG-CADEIA-ASCII-INICIAL.
              10 CONFIG-ASCII-I    OCCURS 50 COMP-X PIC  9(002).
           05 CONFIG-CADEIA-ASCII-FINAL.
              10 CONFIG-ASCII-F    OCCURS 50 COMP-X PIC  9(002).
      * Utlizado tambem com reg 03 (tabela de impressoras)

       01  CONFIG-REG92.
           05 FILLER                                PIC  X(032).
           05 CONFIG-BEEP                           PIC  X(001).
              88 CONFIG-BEEP-ON  VALUE "S" "s"
                                       "Y" "y".
           05 CONFIG-STOP                           PIC  X(001).
              88 CONFIG-STOP-ON  VALUE "S" "s"
                                       "Y" "y".
           05 CONFIG-ERASE                          PIC  X(001).
              88 CONFIG-ERASE-ON VALUE "S" "s"
                                       "Y" "y".
           05 CONFIG-TIMER                          PIC  9(002).

       01  CONFIG-REG94.
           05 FILLER                                PIC  X(002).
           05 CONFIG-RELATORIO                      PIC  X(030).
           05 REDEFINES CONFIG-RELATORIO.
              10 CONFIG-BYTE       OCCURS 30 COMP-X PIC  9(002).
           05 REDEFINES CONFIG-RELATORIO.
              10 FILLER                             PIC  X(007).
              10 CONFIG-NAME-REPORT                 PIC  X(023).
           05 CONFIG-PROGRAMA-PRINTER.
              10 CONFIG-PROGRAMA-ASCII1      COMP-X PIC  9(002).
              10 CONFIG-PROGRAMA-ASCII2      COMP-X PIC  9(002).
           05 CONFIG-DESPROGRAMA.
              10 CONFIG-DESPROGRAMA-ASCII1   COMP-X PIC  9(002).
              10 CONFIG-DESPROGRAMA-ASCII2   COMP-X PIC  9(002).
           05 CONFIG-TIPO-FORM                      PIC  X(020).
           05 CONFIG-SIZE-PAGE                      PIC  9(002).
           05 CONFIG-SAIDA                          PIC  X(015).
           05 REDEFINES CONFIG-SAIDA.
              10 CONFIG-BYTE-SAIDA OCCURS 15 COMP-X PIC  9(002).
           05 CONFIG-TITLE.
              10 CONFIG-TITLE-1                     PIC  X(043).
              10 CONFIG-TITLE-2                     PIC  X(043).
           05 CONFIG-SUB-TITLE.
              10 CONFIG-SUB-TITLE-1                 PIC  X(043).
              10 CONFIG-SUB-TITLE-2                 PIC  X(043).
           05 CONFIG-CAMPOS-AP.
              10 CONFIG-CAMPOS-APAGADOS OCCURS 6.
                 15 CONFIG-INICIO-AP                PIC  9(003).
                 15 CONFIG-FIM-AP                   PIC  9(003).
           05 CONFIG-CAMPOS-TB.
              10 CONFIG-CAMPOS-TABULADOS OCCURS 6.
                 15 CONFIG-INICIO-TB                PIC  9(003).
                 15 CONFIG-FIM-TB                   PIC  9(003).

           66 CONFIG-DADOS RENAMES CONFIG-PROGRAMA-ASCII1
                              THRU CONFIG-CAMPOS-TB.

       01  CONFIG-REG98.
           05 FILLER                                PIC  X(032).
           05 CONFIG-ESTADOS                        PIC  9(002).
           05 CONFIG-ARRAY-UF.
              10 CONFIG-ESTADO            OCCURS 50 PIC  X(002).

       01  CONFIG-REG99.
           05 FILLER                                PIC  X(002).
           05 CONFIG-PAGINA                         PIC  9(004).
           05 FILLER                                PIC  X(026).
           05 CONFIG-OPCOES.
              10 CONFIG-OPCAO OCCURS 26.
                 15 CONFIG-PROG                     PIC  X(008).
                 15 REDEFINES CONFIG-PROG.
                    20 PROG-X       OCCURS 8 COMP-X PIC  9(002).
                 15 CONFIG-NIVEL                    PIC  9(001).
                 15 CONFIG-CHECK                    PIC  X(001).
                 15 CONFIG-PASS                     PIC  X(006).
                 15 REDEFINES CONFIG-PASS.
                    20 PASS-X       OCCURS 6 COMP-X PIC  9(002).
                 15 CONFIG-HELP                     PIC  X(020).
                 15 CONFIG-FATOR             COMP-X PIC  9(002).
                 15                                 PIC  X(001).
                 15 CONFIG-NO-OPCAO                 PIC  9(002).
                 15 CONFIG-NM-OPCAO                 PIC  X(034).
                 15 REDEFINES CONFIG-NM-OPCAO.
                    20 CONFIG-BYTE-NM OCCURS 34 PIC X(001).

       01  CONFIG-REGAT.
           05 FILLER                                PIC  X(032).
           05 CONFIG-ATRIBUTOS.
              10 CONFIG-CORES OCCURS 11.
                 15 CONFIG-COR-TEXTO                PIC  9(002).
                 15 CONFIG-COR-FUNDO                PIC  9(002).

       01  CONFIG-REGJB.
           05 FILLER                                PIC  X(002).
           05 CONFIG-JOB                            PIC  X(030).
           05 CONFIG-JOB-MODULO                     PIC  X(050).
           05 CONFIG-JOB-TIPO                       PIC  9(001).
              88 CONFIG-JOB-BINARIO                           VALUE 1.
              88 CONFIG-JOB-COBOL                             VALUE 2.
           05 CONFIG-JOB-PARAMETRO                  PIC  X(050).
           05 CONFIG-JOB-TAMANHO                    PIC  9(002).
           05 CONFIG-JOB-PROXIMO-RC-OK              PIC  X(007).
           05 CONFIG-JOB-PROXIMO-NAO-OK             PIC  X(007).
           05 CONFIG-JOB-MENSAGEM                   PIC  X(079).

       01  CONFIG-REGLG.
           05 FILLER                                PIC  X(032).
           05 CONFIG-LOG                            PIC  X(001).

       01  CONFIG-REGPS.
           05 FILLER                                PIC  X(002).
           05 CONFIG-NOME                           PIC  X(030).
           05 CONFIG-SENHA                          PIC  X(006).
           05 REDEFINES CONFIG-SENHA.
              10 CONFIG-S           OCCURS 6 COMP-X PIC  9(002).
           05 CONFIG-FATOR-PS                COMP-X PIC  9(002).
           05 CONFIG-NIVEL-PS                COMP-X PIC  9(002).
           05 CONFIG-GRUPO                          PIC  X(022).
           05 CONFIG-MODO-MENU                      PIC  9(001).
           05 CONFIG-PATH-SPOOL                     PIC  X(030).
           05 CONFIG-PRINTER-DEFAULT                PIC  X(008).
           05 CONFIG-QUADRO-PS                      PIC  9(001).

       01  CONFIG-REGGU.
           05 FILLER                                PIC  X(002).
           05 CONFIG-NOME-GRUPO                     PIC  X(022).
           05 CONFIG-PROG-GRUPO                     PIC  X(008).
           05 CONFIG-ACESSO-GRUPO                   PIC  X(001).
           05 CONFIG-ALTERACAO-GRUPO                PIC  X(001).
           05 CONFIG-CONSULTA-GRUPO                 PIC  X(001).
           05 CONFIG-EXCLUSAO-GRUPO                 PIC  X(001).
           05 CONFIG-INCLUSAO-GRUPO                 PIC  X(001).

       01  CONFIG-REGRT.
           05 FILLER                                PIC  X(032).
           05 CONFIG-ROTINAS.
              10 CONFIG-ROTINA            OCCURS 84 PIC  X(008).

       01  CONFIG-REGRV.
           05 FILLER                                PIC  X(032).
           05 CONFIG-REVISAO                        PIC  99V99.

       01  CONFIG-REGVD.
           05 FILLER                                PIC  X(032).
           05 CONFIG-VALIDADE                       PIC  9(006) COMP-3.
           05 CONFIG-TRAVADO                        PIC  X(001).
           05 CONFIG-SENHA-ATIVACAO                 PIC  9(006) COMP-3.


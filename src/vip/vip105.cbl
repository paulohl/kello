       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIP105.
       AUTHOR. MARELI AMANCIO VOLPATO.
       DATE-WRITTEN. 03/08/2000.
      *FUNÇÃO: Movimento de PLANEJAMENTO E RELATORIO DE VIDEO DE EDIÇÃO

       ENVIRONMENT DIVISION.
       class-control.
           Window             is class "wclass".
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY CAPX010.
           COPY CAPX004.
           COPY COPX003.
           COPY COPX040.
           COPY CGPX001.
           COPY LBPX029.
           COPY VIPX100.
           COPY VIPX105.
           COPY VIPX106.
           COPY VIPX107.
           COPY VIPX108.
           COPY VIPX109.
           COPY VIPX110.
           COPY VIPX111.
           SELECT WORK ASSIGN TO VARIA-W
                  ACCESS MODE IS DYNAMIC
                  ORGANIZATION IS INDEXED
                  RECORD KEY IS CHAVE-WK = NR-MASTER-WK IDENTIFICADOR-WK
                  STATUS IS ST-WORK.
           SELECT WORK1 ASSIGN TO VARIA-W1
                  ACCESS MODE IS DYNAMIC
                  ORGANIZATION IS INDEXED
                  RECORD KEY IS CHAVE-WK1 = TIPO-ATIV-WK
                     TIPO-INTERR-WK
                  STATUS IS ST-WORK1.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.

       DATA DIVISION.
       FILE SECTION.
       COPY CAPW010.
       COPY CAPW004.
       COPY COPW003.
       COPY COPW040.
       COPY CGPW001.
       COPY LBPW029.
       COPY VIPW100.
       COPY VIPW105.
       COPY VIPW106.
       COPY VIPW107.
       COPY VIPW108.
       COPY VIPW109.
       COPY VIPW110.
       COPY VIPW111.
       FD  WORK.
       01  REG-WORK.
           05  NR-MASTER-WK        PIC 99.
           05  IDENTIFICADOR-WK    PIC 9(9).
           05  NR-FITAS-WK.
               10 CONTRATO-WK      PIC 9(4).
               10  NR-FITA-WK      PIC 9(5).
           05  PERSONALIZADA-WK    PIC 9.
           05  TEMPO-WK            PIC 9(4).
       FD  WORK1.
       01  REG-WORK1.
           05  TIPO-ATIV-WK        PIC 9.
           05  TIPO-INTERR-WK      PIC 9(2).
           05  HR-INTERR-WK        PIC 9(4).
           05  FUNCIONARIO-WK      PIC 9(6).
       FD  RELAT
           LABEL RECORD IS OMITTED.
       01  REG-RELAT.
           05  FILLER              PIC X(130).
       WORKING-STORAGE SECTION.
           COPY "VIP105.CPB".
           COPY "VIP105.CPY".
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
           COPY "CPADAY1.CPY".
           COPY "CPTIME.CPY".
           COPY "CPDCIC.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(65).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-CAD004             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-COD003             PIC XX       VALUE SPACES.
           05  ST-COD040             PIC XX       VALUE SPACES.
           05  ST-LBD029             PIC XX       VALUE SPACES.
           05  ST-VID100             PIC XX       VALUE SPACES.
           05  ST-VID105             PIC XX       VALUE SPACES.
           05  ST-VID106             PIC XX       VALUE SPACES.
           05  ST-VID107             PIC XX       VALUE SPACES.
           05  ST-VID108             PIC XX       VALUE SPACES.
           05  ST-VID109             PIC XX       VALUE SPACES.
           05  ST-VID110             PIC XX       VALUE SPACES.
           05  ST-VID111             PIC XX       VALUE SPACES.
           05  ST-WORK               PIC XX       VALUE SPACES.
           05  ST-WORK1              PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *    ERRO-W - flag que controla se houve erro de abertura arquivo
           05  ULT-SEQ               PIC 9(3)     VALUE ZEROS.
           05  VARIA-W               PIC 9(8)     VALUE ZEROS.
           05  VARIA-W1              PIC 9(8)     VALUE ZEROS.
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  LIN                   PIC 9(2)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  DATA-MOVTO-W          PIC 9(8)     VALUE ZEROS.
           05  DATA-MOVTO-I          PIC 9(8)     VALUE ZEROS.
           05  DATA-E                PIC ZZ/ZZ/ZZZZ.
           05  HORA-E                PIC 99B99    VALUE ZEROS.
           05  AVALIACAO-W           PIC X        VALUE SPACES.
           05  ULT-MASTER            PIC 99       VALUE ZEROS.
           05  TIPO-GRAVA-MASTER     PIC 9        VALUE ZEROS.
           05  TIPO-GRAVA-INTERR     PIC 9        VALUE ZEROS.
           05  OBS-W                 PIC X(100)   VALUE SPACES.
      *    CONTADORES P/ OBS
           05  I                     PIC 9(3)     VALUE ZEROS.
           05  J                     PIC 9(3)     VALUE ZEROS.
           05  K                     PIC 9(3)     VALUE ZEROS.
           05  DATA-DIA-I            PIC 9(8)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).

           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

           copy impressora.

       01  CAB01.
           05  EMPRESA-REL         PIC X(60)   VALUE SPACES.
           05  FILLER              PIC X(13)   VALUE SPACES.
           05  FILLER              PIC X(5)    VALUE "PAG: ".
           05  PAG-REL             PIC Z9      VALUE ZEROS.
       01  CAB02.
           05  FILLER              PIC X(63)   VALUE
           "PLANEJAMENTO E RELATORIO DE VIDEO EDICAO".
           05  HORA-REL            PIC X(5)    VALUE "  :  ".
           05  FILLER              PIC XX      VALUE SPACES.
           05  EMISSAO-REL         PIC 99/99/9999 BLANK WHEN ZEROS.

       01 mensagem                 pic x(200).
       01 tipo-msg                 pic x(01).
       01 resp-msg                 pic x(01).

       01  LINDET.
           05  LINDET-REL          PIC X(80)   VALUE SPACES.

       02  LINHA-01                          PIC  X(080) VALUE ALL "_".
       02  LINHA-02.
           05 FILLER                         PIC  X(014) VALUE
              "|Nr.Contrato: ".
           05 NR-CONTRATO-REL                PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(009) VALUE
              "   Item: ".
           05 ITEM-REL                       PIC  Z(002) VALUE ZEROS.
           05 FILLER                         PIC  X(004) VALUE SPACES.
           05 CURSO-REL                      PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 CIDADE-REL                     PIC  X(018) VALUE SPACES.
           05 FILLER                         PIC  X(006) VALUE
              "     |".
       02  LINHA-03.
           05 FILLER                         PIC  X(059) VALUE
          "|__________________________________________________________".
           05 FILLER                         PIC  X(021) VALUE
              "____________________|".
       02  LINHA-03A.
           05 FILLER                         PIC  X(059) VALUE
          "|                   RELACAO DE FITAS BRUTAS DESTE CONTRATO ".
           05 FILLER                         PIC  X(021) VALUE
              "                    |".
       02  LINHA-04.
           05 FILLER                         PIC  X(006) VALUE "|    N".
           05 FILLER                         PIC  X(001) VALUE "R".
           05 FILLER                         PIC  X(055) VALUE
              ".FIT|DATA EVENTO|EVENTO               |LOCAL|AVALIACAO ".
           05 FILLER                         PIC  X(018) VALUE
              "|CINEGRAFISTA    |".
       02  LINHA-05.
           05 FILLER                         PIC  X(059) VALUE
          "|__________|___________|_____________________|_____|_______".
           05 FILLER                         PIC  X(021) VALUE
              "___|________________|".
       02  LINHA-06.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 NR-FITA-REL                    PIC  ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(002) VALUE "| ".
           05 DATA-EVE-REL                   PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(002) VALUE "| ".
           05 EVENTO-REL                     PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 LOCAL-REL                      PIC  X(005) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 AVAL-REL                       PIC  X(010) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 CINEGRAFISTA-REL               PIC  X(016) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-07.
           05 FILLER                         PIC  X(057) VALUE
              "|                              INSTRUCOES DE EDICAO    ".
           05 FILLER                         PIC  X(023) VALUE
              "                      |".
       02  LINHA-08.
           05 FILLER                         PIC  X(009) VALUE
              "|EDITOR: ".
           05 EDITOR-ED-REL                  PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(015) VALUE
              "      REVISOR: ".
           05 REVISOR-ED-REL                 PIC  X(025) VALUE SPACES.
           05 FILLER                         PIC  X(005) VALUE "    |".
       02  LINHA-09.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 OBS-ED-REL                     PIC  X(082) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-10.
           05 FILLER                         PIC  X(029) VALUE
              "| Em que Data Inic.a Edicao| ".
           05 DATA-INI-ED-REL                PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(030) VALUE
              "|Em que Data Terminou a Edic.|".
           05 DATA-FIM-ED-REL                PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-11.
           05 FILLER                         PIC  X(034) VALUE
              "|       A que horas iniciou|      ".
           05 HORA-INI-ED-REL                PIC  ZZBZZ.
           05 FILLER                         PIC  X(035) VALUE
              "|        A que Horas Terminou|     ".
           05 HORA-FIM-ED-REL                PIC  ZZBZZ.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-12.
           05 FILLER                         PIC  X(033) VALUE
              "|      Quantas fitas brutas|     ".
           05 FITA-BR-ED-REL                 PIC  ZZ.ZZZ.
           05 FILLER                         PIC  X(034) VALUE
              "|        Quantas fitas master|    ".
           05 FITA-MAS-ED-REL                PIC  ZZ.ZZZ.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-13.
           05 FILLER                         PIC  X(032) VALUE
              "|    Quantas horas gravadas|    ".
           05 HORAS-GRAV-ED-REL              PIC  ZZZZBZZ.
           05 FILLER                         PIC  X(033) VALUE
              "|Quantas horas editad.em mast|   ".
           05 HORAS-ED-MASTER-ED-REL         PIC  ZZZZBZZ.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-14.
           05 FILLER                         PIC  X(035) VALUE
              "|     Quantos cursos/turmas|       ".
           05 CURSO-ED-REL                   PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(036) VALUE
              "|           Quantos formandos|      ".
           05 FORM-ED-REL                    PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-16.
           05 FILLER                         PIC  X(032) VALUE
              "|Quantas horas em serv.Edic|    ".
           05 HORA-SERV-ED-REL               PIC  ZZZZBZZ.
           05 FILLER                         PIC  X(030) VALUE
              "|             Avaliacao Geral|".
           05 AVAL-ED-REL                    PIC  X(010) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-17.
           05 FILLER                         PIC  X(057) VALUE
          "|                      RELACAO DE FITAS MASTER DESTE ".
           05 FILLER                         PIC  X(023) VALUE
              "CONTRATO              |".
       02  LINHA-18.
           05 FILLER                         PIC  X(016) VALUE
              "| MASTER |     N".
           05 FILLER                         PIC  X(001) VALUE "R".
           05 FILLER                         PIC  X(055) VALUE
              ".FITA | EVENTO               | PERS. | OBSERVACAO      ".
           05 FILLER                         PIC  X(008) VALUE
              "       |".
       02  LINHA-18A.
           05 FILLER                         PIC  X(017) VALUE
              "|____________|___".
           05 FILLER                         PIC  X(055) VALUE
              "______|______________________|_______|_________________".
           05 FILLER                         PIC  X(008) VALUE
              "_______|".
       02  LINHA-19.
           05 FILLER                         PIC  X(004) VALUE "|   ".
           05 NR-MASTER-ED-REL               PIC  Z(002) VALUE ZERO.
           05 FILLER                         PIC  X(006) VALUE
              "   |  ".
           05 NR-FITA-ED-REL                 PIC  ZZ.ZZZ.ZZZ.
           05 FILLER                         PIC  X(003) VALUE " | ".
           05 EVENTO-ED-REL                  PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE " | ".
           05 PERS-ED-REL                    PIC  X(003) VALUE SPACES.
           05 FILLER                         PIC  X(005) VALUE "   | ".
           05 TEMPO-MASTER-REL               PIC  99B99  VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE
              "  |".
       02  LINHA-20.
           05 FILLER                         PIC  X(057) VALUE
          "|                            INSTRUCOES PARA COPIA     ".
           05 FILLER                         PIC  X(023) VALUE
              "                      |".
       02  LINHA-21.
           05 FILLER                         PIC  X(014) VALUE
              "|FUNCIONARIO: ".
           05 FUNCIO-COP-REL                 PIC  X(026) VALUE SPACES.
           05 FILLER                         PIC  X(012) VALUE
              "   REVISOR: ".
           05 REVISOR-COP-REL                PIC  X(025) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE "  |".
       02  LINHA-22.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 OBS-COP-REL                    PIC  X(078) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-23.
           05 FILLER                         PIC  X(029) VALUE
              "|       Em que Data Iniciou| ".
           05 DATA-INI-COP-REL               PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(030) VALUE
              "|        Em que Data Terminou|".
           05 DATA-FIM-COP-REL               PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-24.
           05 FILLER                         PIC  X(034) VALUE
              "|       A que horas iniciou|      ".
           05 HORA-INI-COP-REL               PIC  ZZBZZ.
           05 FILLER                         PIC  X(035) VALUE
              "|        A que Horas Terminou|     ".
           05 HORA-FIM-COP-REL               PIC  ZZBZZ.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-26.
           05 FILLER                         PIC  X(032) VALUE
              "|Quantas horas em serv.Cop.|    ".
           05 HORA-SERV-COP-REL              PIC  ZZZZBZZ.
           05 FILLER                         PIC  X(030) VALUE
              "|             Avaliacao Geral|".
           05 AVAL-COP-REL                   PIC  X(010) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-26A.
           05 FILLER                         PIC  X(039) VALUE
              "|__________________________|___________".
           05 FILLER                         PIC  X(040) VALUE
              "|____________________________|__________".
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-27.
           05 FILLER                         PIC  X(057) VALUE
          "|                            INSTRUCOES PARA INSERT    ".
           05 FILLER                         PIC  X(023) VALUE
              "                      |".
       02  LINHA-28.
           05 FILLER                         PIC  X(014) VALUE
              "|FUNCIONARIO: ".
           05 FUNCION-INS-REL                PIC  X(026) VALUE SPACES.
           05 FILLER                         PIC  X(012) VALUE
              "   REVISOR: ".
           05 REVISOR-INS-REL                PIC  X(025) VALUE SPACES.
           05 FILLER                         PIC  X(003) VALUE "  |".
       02  LINHA-29.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 OBS-INS-REL                    PIC  X(078) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-30.
           05 FILLER                         PIC  X(029) VALUE
              "|       Em que Data Iniciou| ".
           05 DATA-INI-INS-REL               PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(030) VALUE
              "|        Em que Data Terminou|".
           05 DATA-FIM-INS-REL               PIC  ZZ/ZZ/ZZZZ.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-31.
           05 FILLER                         PIC  X(034) VALUE
              "|       A que horas iniciou|      ".
           05 HORA-INI-INS-REL               PIC  ZZBZZ.
           05 FILLER                         PIC  X(035) VALUE
              "|        A que Horas Terminou|     ".
           05 HORA-FIM-INS-REL               PIC  ZZBZZ.
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-33.
           05 FILLER                         PIC  X(032) VALUE
              "|Quantas horas em serv.Ins.|    ".
           05 HORA-SERV-INS-REL              PIC  ZZZZBZZ.
           05 FILLER                         PIC  X(030) VALUE
              "|             Avaliacao Geral|".
           05 AVAL-INS-REL                   PIC  X(010) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".

       02  LINHA-34.
           05 FILLER                         PIC  X(057) VALUE
          "|                              INTERRUPCOES            ".
           05 FILLER                         PIC  X(023) VALUE
              "                      |".
       02  LINHA-35.
           05 FILLER                         PIC  X(078) VALUE
           "|TIPO-ATIVID TIPO-INTERRUPCAO        HR.INTERR FUNCIONARIO".
           05 FILLER                         PIC  X(002) VALUE " |".
       02  LINHA-36.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 FILLER                         PIC  X(078) VALUE
           "=========== ======================= ========= =============
      -    "==============".
           05 FILLER                         PIC  X(001) VALUE "|".
       02  LINHA-37.
           05 FILLER                         PIC  X(001) VALUE "|".
           05 INTERRUPCAO-REL                PIC  X(078) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "|".

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.
       INICIALIZA-PROGRAMA SECTION.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           COPY "CBDATA1.CPY".
           MOVE DATA-INV TO DATA-MOVTO-W
           CALL "GRIDAT2"  USING DATA-INV.
           MOVE DATA-INV       TO DATA-DIA-I.
           MOVE ZEROS TO PAG-W ERRO-W.
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO TO DS-DATA-BLOCK-VERSION-NO.
           MOVE GS-VERSION-NO  TO DS-VERSION-NO
           MOVE EMPRESA-W          TO EMP-REC
           MOVE NOME-EMPRESA-W     TO EMPRESA-REL
           MOVE "CAD004"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD004.
           MOVE "CAD010" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "CGD001" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "COD003" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD003.
           MOVE "COD040" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-COD040.
           MOVE "LBD029" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-LBD029.
           MOVE "VID100" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID100.
           MOVE "VID105" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID105.
           MOVE "VID106" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID106.
           MOVE "VID107" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID107.
           MOVE "VID108" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID108.
           MOVE "VID109" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID109.
           MOVE "VID110" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID110.
           MOVE "VID111" TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-VID111.

           OPEN I-O   VID105 VID106 VID107 VID108 VID109 VID110 VID111
           CLOSE      VID105 VID106 VID107 VID108 VID109 VID110 VID111
           OPEN INPUT VID105 VID106 VID107 VID108 VID109 VID110 VID111

           OPEN INPUT COD003 LBD029 VID100.
           ACCEPT VARIA-W FROM TIME.
           OPEN OUTPUT WORK.
           CLOSE WORK.  OPEN I-O WORK.
           ACCEPT VARIA-W1 FROM TIME.
           ADD 10 TO VARIA-W1
           OPEN OUTPUT WORK1.
           CLOSE WORK1.  OPEN I-O WORK1.
           IF ST-VID105 = "35"
              CLOSE VID105      OPEN OUTPUT VID105
              CLOSE VID105      OPEN I-O VID105
           END-IF.
           IF ST-VID106 = "35"
              CLOSE VID106      OPEN OUTPUT VID106
              CLOSE VID106      OPEN I-O VID106
           END-IF.
           IF ST-VID107 = "35"
              CLOSE VID107      OPEN OUTPUT VID107
              CLOSE VID107      OPEN I-O VID107
           END-IF.
           IF ST-VID108 = "35"
              CLOSE VID108      OPEN OUTPUT VID108
              CLOSE VID108      OPEN I-O VID108
           END-IF.
           IF ST-VID109 = "35"
              CLOSE VID109      OPEN OUTPUT VID109
              CLOSE VID109      OPEN I-O VID109
           END-IF.
           IF ST-VID110 = "35"
              CLOSE VID110      OPEN OUTPUT VID110
              CLOSE VID110      OPEN I-O VID110
           END-IF.
           IF ST-VID111 = "35"
              CLOSE VID111      OPEN OUTPUT VID111
              CLOSE VID111      OPEN I-O VID111
           END-IF.
           IF ST-COD003 <> "00"
              MOVE "ERRO ABERTURA COD003: "  TO GS-MENSAGEM-ERRO
              MOVE ST-COD003 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-LBD029 <> "00"
              MOVE "ERRO ABERTURA LBD029: "  TO GS-MENSAGEM-ERRO
              MOVE ST-LBD029 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID100 <> "00"
              MOVE "ERRO ABERTURA VID100: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID100 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID105 <> "00"
              MOVE "ERRO ABERTURA VID105: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID105 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID106 <> "00"
              MOVE "ERRO ABERTURA VID106: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID106 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID107 <> "00"
              MOVE "ERRO ABERTURA VID107: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID107 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID108 <> "00"
              MOVE "ERRO ABERTURA VID108: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID108 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID109 <> "00"
              MOVE "ERRO ABERTURA VID109: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID109 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID110 <> "00"
              MOVE "ERRO ABERTURA VID110: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID110 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ST-VID111 <> "00"
              MOVE "ERRO ABERTURA VID111: "  TO GS-MENSAGEM-ERRO
              MOVE ST-VID111 TO GS-MENSAGEM-ERRO(23: 02)
              PERFORM CARREGA-MENSAGEM-ERRO.

           IF COD-USUARIO-W NOT NUMERIC
              MOVE "Executar pelo MENU" TO GS-MENSAGEM-ERRO
              PERFORM CARREGA-MENSAGEM-ERRO.
           IF ERRO-W = 0 PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                   PERFORM CENTRALIZAR
               WHEN GS-VERIFICA-ACESSO-TRUE
                    PERFORM VERIFICA-ACESSO
               WHEN GS-SAVE-FLG-TRUE
                    CLOSE      VID105 VID106 VID107 VID108 VID109 VID110
                               VID111
                    OPEN I-O   VID105 VID106 VID107 VID108 VID109 VID110
                               VID111
                    PERFORM SALVAR-DADOS
                    PERFORM LIMPAR-DADOS
                    CLOSE      VID105 VID106 VID107 VID108 VID109 VID110
                               VID111
                    OPEN INPUT VID105 VID106 VID107 VID108 VID109 VID110
                               VID111
               WHEN GS-SAVE-MASTER-TRUE
                    PERFORM INCLUI-MASTER
               WHEN GS-CARREGA-MASTER-TRUE
                    PERFORM CARREGA-MASTER
               WHEN GS-EXCLUI-MASTER-TRUE
                    CLOSE      VID105 VID106 VID107 VID108 VID109 VID110
                               VID111
                    OPEN I-O   VID105 VID106 VID107 VID108 VID109 VID110
                               VID111
                    PERFORM EXCLUI-MASTER
                    CLOSE      VID105 VID106 VID107 VID108 VID109 VID110
                               VID111
                    OPEN INPUT VID105 VID106 VID107 VID108 VID109 VID110
                               VID111
               WHEN GS-CLR-MASTER-TRUE
                    PERFORM LIMPAR-MASTER
               WHEN GS-SAVE-INTERRUP-TRUE
                    PERFORM INCLUI-INTERRUPCAO
               WHEN GS-CARREGA-INTERR-TRUE
                    PERFORM CARREGA-INTERRUPCAO
               WHEN GS-EXCLUI-INTERRUP-TRUE
                    PERFORM EXCLUI-INTERRUPCAO
               WHEN GS-CLR-FLG-TRUE
                   PERFORM LIMPAR-DADOS
               WHEN GS-PRINTER-FLG-TRUE
                    copy impressora.chama.
                    if lnk-mapeamento <> spaces
                       PERFORM IMPRIME-RELATORIO
                    end-if
               WHEN GS-CARREGA-LIST-BOX-TRUE
                   MOVE DATA-MOVTO-I     TO DATA-MOVTO-V100
                   MOVE GS-LINDET(1: 3)  TO SEQ-V100
                   PERFORM CARREGAR-DADOS
               WHEN GS-VERIFICA-CONTRATO-TRUE
                   PERFORM VERIFICA-CONTRATO
               WHEN GS-LE-CONTRATO-TRUE
                   PERFORM LE-CONTRATO
               WHEN GS-LE-GERAL-TRUE
                   PERFORM LE-GERAL
               WHEN GS-LE-TIPO-INTERR-TRUE
                   PERFORM LE-TIPO-INTERRUPCAO
               WHEN GS-CHAMAR-POP-UP-TRUE
                   PERFORM CHAMAR-POP-UP
               WHEN GS-VERIFICA-FITA-TRUE
                   PERFORM VERIFICA-FITA
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       VERIFICA-FITA SECTION.
           INITIALIZE REG-VID100
                      GS-NR-FITA
           MOVE GS-CONTRATO            TO CONTRATO-V100
           START VID100 KEY IS NOT LESS ALT1-V100 INVALID KEY
                MOVE "10" TO ST-VID100.

           PERFORM UNTIL ST-VID100 = "10"
                READ VID100 NEXT AT END
                     MOVE "10" TO ST-VID100
                NOT AT END
                     IF GS-CONTRATO <> CONTRATO-V100
                        MOVE "10" TO ST-VID100
                     ELSE
                        IF GS-IDENTIFICADOR = IDENTIFICADOR-V100
                           MOVE NR-FITA-V100 TO GS-NR-FITA
                           MOVE "10"         TO ST-VID100
                        END-IF
                     END-IF
                END-READ
           END-PERFORM

           IF GS-NR-FITA = ZEROS
              MOVE "Número da Fita Não Encontrada" TO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM.

       exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move 1 to gs-flag-critica
           move spaces to mensagem.

       VERIFICA-ACESSO SECTION.
           OPEN INPUT CAD004.
      *    ACESSO A EDICAO
           MOVE COD-USUARIO-W   TO COD-USUARIO-CA004.
           MOVE "SENHA07"       TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY CONTINUE
             NOT INVALID KEY MOVE "HABILITA-EDICAO" TO DS-PROCEDURE
                             PERFORM CALL-DIALOG-SYSTEM.
      *    ACESSO A COPIA
           MOVE COD-USUARIO-W   TO COD-USUARIO-CA004.
           MOVE "SENHA08"       TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY CONTINUE
             NOT INVALID KEY MOVE "HABILITA-COPIA" TO DS-PROCEDURE
                             PERFORM CALL-DIALOG-SYSTEM.
      *    ACESSO A INSERT
           MOVE COD-USUARIO-W   TO COD-USUARIO-CA004.
           MOVE "SENHA09"       TO PROGRAMA-CA004.
           READ CAD004 INVALID KEY CONTINUE
             NOT INVALID KEY MOVE "HABILITA-INSERT" TO DS-PROCEDURE
                             PERFORM CALL-DIALOG-SYSTEM.
           CLOSE CAD004.
       CHAMAR-POP-UP SECTION.
           EVALUATE GS-OPCAO-POP-UP
             WHEN 1 CALL   "CGP001T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "CGP001T"
                    EVALUATE GS-TIPO-GERAL
                     WHEN 1
                      MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-EDITOR
                      MOVE PASSAR-STRING-1(33: 6) TO GS-EDITOR
                     WHEN 2
                      MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-REVISOR
                      MOVE PASSAR-STRING-1(33: 6) TO GS-REVISOR
                     WHEN 3
                      MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-FUNCION-COP
                      MOVE PASSAR-STRING-1(33: 6) TO GS-FUNCION-COP
                     WHEN 4
                      MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-REVISOR-COP
                      MOVE PASSAR-STRING-1(33: 6) TO GS-REVISOR-COP
                     WHEN 5
                      MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-FUNCION-INS
                      MOVE PASSAR-STRING-1(33: 6) TO GS-FUNCION-INS
                     WHEN 6
                      MOVE PASSAR-STRING-1(1: 30) TO GS-NOME-REVISOR-INS
                      MOVE PASSAR-STRING-1(33: 6) TO GS-REVISOR-INS
                     WHEN 7
                      MOVE PASSAR-STRING-1(1: 30)
                           TO GS-NOME-FUNCION-INTERR
                      MOVE PASSAR-STRING-1(33: 6)
                           TO GS-FUNCION-INTERR
                    END-EVALUATE
             WHEN 2 CALL   "LBP029T" USING PARAMETROS-W PASSAR-STRING-1
                    CANCEL "LBP029T"
                    MOVE PASSAR-STRING-1(1: 20) TO GS-DESC-INTERRUPCAO
                    MOVE PASSAR-STRING-1(33: 2) TO GS-TIPO-INTERRUPCAO
             WHEN OTHER CONTINUE
           END-EVALUATE.
      *----------------------------------------------------------------
       LE-GERAL SECTION.
           OPEN INPUT CGD001.
           EVALUATE GS-TIPO-GERAL
             WHEN 1 MOVE GS-EDITOR      TO CODIGO-CG01
             WHEN 2 MOVE GS-REVISOR     TO CODIGO-CG01
             WHEN 3 MOVE GS-FUNCION-COP  TO CODIGO-CG01
             WHEN 4 MOVE GS-REVISOR-COP TO CODIGO-CG01
             WHEN 5 MOVE GS-FUNCION-INS TO CODIGO-CG01
             WHEN 6 MOVE GS-REVISOR-INS TO CODIGO-CG01
             WHEN 7 MOVE GS-FUNCION-INTERR TO CODIGO-CG01
             WHEN OTHER MOVE ZEROS TO CODIGO-CG01
           END-EVALUATE.
           READ CGD001 INVALID KEY MOVE "********" TO NOME-CG01.
           EVALUATE GS-TIPO-GERAL
             WHEN 1 MOVE NOME-CG01 TO GS-NOME-EDITOR
             WHEN 2 MOVE NOME-CG01 TO GS-NOME-REVISOR
             WHEN 3 MOVE NOME-CG01 TO GS-NOME-FUNCION-COP
             WHEN 4 MOVE NOME-CG01 TO GS-NOME-REVISOR-COP
             WHEN 5 MOVE NOME-CG01 TO GS-NOME-FUNCION-INS
             WHEN 6 MOVE NOME-CG01 TO GS-NOME-REVISOR-INS
             WHEN 7 MOVE NOME-CG01 TO GS-NOME-FUNCION-INTERR
           END-EVALUATE.
           CLOSE CGD001.
       LE-CONTRATO SECTION.
           OPEN INPUT COD040 CAD010.
           MOVE GS-CONTRATO        TO NR-CONTRATO-CO40.
           READ COD040 INVALID KEY INITIALIZE REG-COD040.
           MOVE IDENTIFICACAO-CO40 TO GS-CURSO.
           MOVE CIDADE-CO40        TO CIDADE
           READ CAD010 INVALID KEY MOVE SPACES TO NOME-CID.
           MOVE NOME-CID           TO GS-CIDADE.
           CLOSE COD040 CAD010.
       LE-TIPO-INTERRUPCAO SECTION.
           MOVE GS-TIPO-INTERRUPCAO TO CODIGO-LB29
           READ LBD029 INVALID KEY MOVE "********" TO DESCRICAO-LB29.
           MOVE DESCRICAO-LB29 TO GS-DESC-INTERRUPCAO.
      *------------------------------------------------------
      *CONTROLE MASTER
       INCLUI-MASTER SECTION.
           MOVE GS-MASTER          TO NR-MASTER-WK
           MOVE GS-CONTRATO        TO CONTRATO-WK
           MOVE GS-NR-FITA         TO NR-FITA-WK
           MOVE GS-IDENTIFICADOR   TO IDENTIFICADOR-WK
           READ WORK INVALID KEY
                MOVE GS-PERSONALIZADO(1: 1) TO PERSONALIZADA-WK
                MOVE GS-TEMPO-MASTER       TO TEMPO-WK
                WRITE REG-WORK
                END-WRITE
                MOVE 1 TO TIPO-GRAVA-MASTER
              NOT INVALID KEY
                MOVE GS-PERSONALIZADO(1: 1) TO PERSONALIZADA-WK
                MOVE GS-TEMPO-MASTER       TO TEMPO-WK
                REWRITE REG-WORK
                END-REWRITE
                MOVE 2 TO TIPO-GRAVA-MASTER
           END-READ.
           MOVE GS-MASTER      TO GS-LINDET1(1: 10)
           MOVE GS-CONTRATO    TO CONTRATO-V100
                                  GS-LINDET(4:4)
           MOVE GS-NR-FITA     TO NR-FITA-V100
           MOVE ZEROS          TO DATA-EVENTO-V100
           START VID100 KEY IS NOT < ALT-V100 INVALID KEY
                 MOVE "10" TO ST-VID100.
           PERFORM UNTIL ST-VID100 = "10"
             READ VID100 NEXT RECORD AT END
                MOVE "10" TO ST-VID100
              NOT AT END
                IF CONTRATO-V100 <> CONTRATO-WK OR
                   NR-FITA-V100  <> NR-FITA-WK
                   MOVE ZEROS TO EVENTO-V100
                ELSE
                  CONTINUE
                END-IF
                MOVE "10" TO ST-VID100
             END-READ
           END-PERFORM.
           MOVE GS-IDENTIFICADOR  TO GS-LINDET1(11: 10)
           MOVE EVENTO-V100       TO CODIGO-CO03
           READ COD003 INVALID KEY
                MOVE SPACES TO NOME-CO03
           END-READ
           MOVE NOME-CO03         TO GS-LINDET1(21: 31)
           EVALUATE PERSONALIZADA-WK
             WHEN 1 MOVE "1-Sim"  TO GS-LINDET1(52: 10)
             WHEN 2 MOVE "2-Não"  TO GS-LINDET1(52: 10)
           END-EVALUATE
           MOVE GS-TEMPO-MASTER   TO HORA-E
           MOVE HORA-E            TO GS-LINDET1(62: 20)
           IF TIPO-GRAVA-MASTER = 1
              MOVE "INSERE-LIST2" TO DS-PROCEDURE
              ADD 1 TO ULT-MASTER
              MOVE ULT-MASTER     TO GS-MASTER
           ELSE
              MOVE "ATUALIZA-LISTA2" TO DS-PROCEDURE
              MOVE ULT-MASTER   TO GS-MASTER.
           PERFORM CALL-DIALOG-SYSTEM.
       EXCLUI-MASTER SECTION.
           MOVE GS-MASTER      TO NR-MASTER-WK
           MOVE GS-CONTRATO    TO CONTRATO-WK
           MOVE GS-NR-FITA     TO NR-FITA-WK
           READ WORK INVALID KEY
                CONTINUE
           NOT INVALID KEY
               DELETE WORK
           END-READ.
           MOVE ULT-MASTER     TO GS-MASTER.
       LIMPAR-MASTER SECTION.
           MOVE ULT-MASTER     TO GS-MASTER
           MOVE "CLEAR-MASTER" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-MASTER SECTION.
           MOVE GS-LINDET1(1: 2)   TO NR-MASTER-WK GS-MASTER.
           MOVE FUNCTION NUMVAL(GS-LINDET1(11: 10)) TO GS-IDENTIFICADOR
                                                       IDENTIFICADOR-WK
           START WORK KEY IS = CHAVE-WK INVALID KEY CONTINUE
             NOT INVALID KEY
              READ WORK NEXT RECORD
              END-READ
              EVALUATE PERSONALIZADA-WK
               WHEN 1 MOVE "1-Sim"  TO GS-PERSONALIZADO
               WHEN 2 MOVE "2-Não"  TO GS-PERSONALIZADO
              END-EVALUATE
              MOVE TEMPO-WK         TO GS-TEMPO-MASTER
              MOVE CONTRATO-WK      TO GS-CONTRATO
              MOVE NR-FITA-WK       TO GS-NR-FITA
           END-START.
      *------------------------------------------------------
      *CONTROLE INTERRUPÇÃO
       INCLUI-INTERRUPCAO SECTION.
           MOVE GS-TIPO-ATIV-INTERR(1: 1) TO TIPO-ATIV-WK
           MOVE GS-TIPO-INTERRUPCAO      TO TIPO-INTERR-WK
           READ WORK1 INVALID KEY
                MOVE GS-HR-INTERRUPCAO   TO HR-INTERR-WK
                MOVE GS-FUNCION-INTERR   TO FUNCIONARIO-WK
                WRITE REG-WORK1
                END-WRITE
                MOVE 1 TO TIPO-GRAVA-INTERR
              NOT INVALID KEY
                MOVE GS-HR-INTERRUPCAO   TO HR-INTERR-WK
                MOVE GS-FUNCION-INTERR   TO FUNCIONARIO-WK
                REWRITE REG-WORK1
                END-REWRITE
                MOVE 2 TO TIPO-GRAVA-INTERR
           END-READ.
           MOVE GS-TIPO-ATIV-INTERR TO GS-LINDET2(1: 12)
           MOVE GS-TIPO-INTERRUPCAO TO GS-LINDET2(13: 2)
           MOVE "-"                 TO GS-LINDET2(15: 1)
           MOVE GS-DESC-INTERRUPCAO TO GS-LINDET2(16: 21)
           MOVE GS-HR-INTERRUPCAO   TO GS-LINDET2(37: 10)
           MOVE GS-FUNCION-INTERR   TO GS-LINDET2(47: 6)
           MOVE "-"                 TO GS-LINDET2(53: 1)
           MOVE GS-NOME-FUNCION-INTERR TO GS-LINDET2(54: 20)
           IF TIPO-GRAVA-INTERR = 1
              MOVE "INSERE-LIST3" TO DS-PROCEDURE
           ELSE MOVE "ATUALIZA-LISTA3" TO DS-PROCEDURE.
           PERFORM CALL-DIALOG-SYSTEM.
       EXCLUI-INTERRUPCAO SECTION.
           MOVE GS-TIPO-ATIV-INTERR(1: 1) TO TIPO-ATIV-WK
           MOVE GS-TIPO-INTERRUPCAO TO TIPO-INTERR-WK
           READ WORK1 INVALID KEY CONTINUE
             NOT INVALID KEY
               DELETE WORK1
           END-READ.
       CARREGA-INTERRUPCAO SECTION.
           OPEN INPUT CGD001.
           MOVE GS-LINDET2(1: 1)   TO TIPO-ATIV-WK
           MOVE GS-LINDET2(1: 9)   TO GS-TIPO-ATIV-INTERR
           MOVE GS-LINDET2(13: 2)  TO TIPO-INTERR-WK
                                      GS-TIPO-INTERRUPCAO
           MOVE GS-LINDET2(16: 20) TO GS-DESC-INTERRUPCAO
           START WORK1 KEY IS = CHAVE-WK1 INVALID KEY CONTINUE
             NOT INVALID KEY
              READ WORK1 NEXT RECORD
              END-READ
              MOVE HR-INTERR-WK    TO GS-HR-INTERRUPCAO
              MOVE FUNCIONARIO-WK  TO GS-FUNCION-INTERR CODIGO-CG01
              READ CGD001 INVALID KEY MOVE SPACES TO NOME-CG01
              END-READ
              MOVE NOME-CG01       TO GS-NOME-FUNCION-INTERR
           END-START.
           CLOSE CGD001.
      *--------------------------------------------------------------
       VERIFICA-CONTRATO SECTION.
           CLOSE WORK.  OPEN OUTPUT WORK.
           CLOSE WORK.  OPEN I-O WORK.
           CLOSE WORK1.  OPEN OUTPUT WORK1.
           CLOSE WORK1.  OPEN I-O WORK1.
           MOVE GS-CONTRATO       TO CONTRATO-V105
           MOVE GS-ITEM           TO ITEM-V105
           READ VID105 INVALID KEY
                PERFORM CARREGAR-FITAS-BRUTAS
           NOT INVALID KEY
                PERFORM CARREGAR-DADOS
           END-READ.
       CARREGAR-FITAS-BRUTAS SECTION.
           OPEN INPUT CGD001.
           MOVE GS-CONTRATO TO CONTRATO-V100
           MOVE ZEROS       TO DATA-EVENTO-V100 GS-CONT.
           START VID100 KEY IS NOT < ALT1-V100 INVALID KEY
                 MOVE "10" TO ST-VID100.
           PERFORM UNTIL ST-VID100 = "10"
                 READ VID100 NEXT RECORD AT END
                      MOVE "10" TO ST-VID100
                 NOT AT END
                      IF CONTRATO-V100 <> GS-CONTRATO
                         MOVE "10" TO ST-VID100
                      ELSE
                         MOVE IDENTIFICADOR-V100  TO GS-LINDET(1:10)
                         MOVE DATA-EVENTO-V100    TO DATA-INV
                         CALL "GRIDAT1" USING DATA-INV
                         MOVE DATA-INV            TO DATA-E
                         MOVE DATA-E              TO GS-LINDET(11: 11)
                         MOVE EVENTO-V100         TO CODIGO-CO03
                         READ COD003 INVALID KEY
                              MOVE SPACES TO NOME-CO03
                         END-READ
                         MOVE NOME-CO03           TO GS-LINDET(24: 31)
                         MOVE LOCALIZACAO-V100    TO GS-LINDET(54: 6)
                         MOVE CINEGRAFISTA-V100   TO CODIGO-CG01
                         READ CGD001 INVALID KEY
                              MOVE SPACES TO NOME-CG01
                         END-READ
                         MOVE NOME-CG01           TO GS-LINDET(60: 20)
                         MOVE "INSERE-LIST"       TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                      END-IF
                 END-READ
           END-PERFORM.
           CLOSE CGD001.
       CARREGAR-DADOS SECTION.
           PERFORM CARREGAR-FITAS-BRUTAS.
           PERFORM CARREGAR-INSTRUCAO-EDICAO
           PERFORM CARREGAR-INSTRUCAO-COPIA
           PERFORM CARREGAR-INSTRUCAO-INSERT.
           PERFORM CARREGAR-OBS.
           PERFORM CARREGAR-INSTRUCAO-INTERRUPCAO.
       CARREGAR-INSTRUCAO-EDICAO SECTION.
           MOVE GS-CONTRATO            TO CONTRATO-V105.
           MOVE GS-ITEM                TO ITEM-V105.
           READ VID105 INVALID KEY
                INITIALIZE REG-VID105.
           MOVE DATA-INIC-V105         TO GS-DATA-INI-EDI
           MOVE HORA-INIC-V105         TO GS-HORA-INI-EDI
           MOVE DATA-FIM-V105          TO GS-DATA-FIM-EDI
           MOVE HORA-FIM-V105          TO GS-HORA-FIM-EDI
           MOVE QT-FITA-BR-V105        TO GS-QT-FITA-BR
           MOVE QT-FITA-MASTER-V105    TO GS-QT-FITA-MASTER
           MOVE QT-HORA-GRAV-V105      TO GS-QT-HORA-GRAV
           MOVE QT-HORA-ED-MASTER-V105 TO GS-TOT-HORA-EDIT
           MOVE QT-CURSO-V105          TO GS-QT-CURSO
           MOVE QT-FORMANDO-V105       TO GS-QT-FORMANDO
           MOVE QT-HORA-SERV-ED-V105   TO GS-HR-SERV-ED
           EVALUATE AVALIACAO-GERAL-V105
             WHEN 1 MOVE "1-Péssima"   TO GS-AVAL-GERAL
             WHEN 2 MOVE "2-Ruim   "   TO GS-AVAL-GERAL
             WHEN 3 MOVE "3-Regular"   TO GS-AVAL-GERAL
             WHEN 4 MOVE "4-Boa    "   TO GS-AVAL-GERAL
             WHEN 5 MOVE "5-Ótima  "   TO GS-AVAL-GERAL
           END-EVALUATE.

           CLOSE VID106.  OPEN I-O VID106.
           MOVE GS-CONTRATO            TO CONTRATO-V106.
           MOVE GS-ITEM                TO ITEM-V106.
           MOVE ZEROS                  TO NR-MASTER-V106 GS-CONT1
                                          ULT-MASTER.
           START VID106 KEY IS NOT < CHAVE-V106 INVALID KEY
                 MOVE "10" TO ST-VID106.
           PERFORM UNTIL ST-VID106 = "10"
             READ VID106 NEXT RECORD AT END
                  MOVE "10" TO ST-VID106
              NOT AT END
                  IF CONTRATO-V106 <> GS-CONTRATO OR
                     ITEM-V106 <> GS-ITEM
                     MOVE "10" TO ST-VID106
                  ELSE
                     MOVE NR-MASTER-V106    TO NR-MASTER-WK
                                               GS-LINDET1(1: 10)
                     MOVE NR-FITA-V106      TO NR-FITA-WK
                                               GS-LINDET1(11: 10)
                     MOVE CONTRATO-V106     TO CONTRATO-V100
                     MOVE NR-FITA-V106      TO NR-FITA-V100
                     MOVE ZEROS             TO DATA-EVENTO-V100
                     START VID100 KEY IS NOT < ALT-V100 INVALID KEY
                           MOVE "10" TO ST-VID100
                     END-START
                     PERFORM UNTIL ST-VID100 = "10"
                           READ VID100 NEXT RECORD AT END
                                MOVE "10" TO ST-VID100
                            NOT AT END
                                IF CONTRATO-V100 <> CONTRATO-V106 OR
                                   NR-FITA-V100  <> NR-FITA-V106
                                   MOVE ZEROS    TO EVENTO-V100
                                   MOVE ZEROS    TO IDENTIFICADOR-V100
                                ELSE
                                  CONTINUE
                                END-IF
                                MOVE "10" TO ST-VID100
                           END-READ
                     END-PERFORM
                     MOVE IDENTIFICADOR-V100 TO GS-LINDET1(11:10)
                                                IDENTIFICADOR-WK
                     MOVE EVENTO-V100       TO CODIGO-CO03
                     READ COD003 INVALID KEY
                          MOVE SPACES TO NOME-CO03
                     END-READ
                     MOVE NOME-CO03         TO GS-LINDET1(21: 31)
                     MOVE PERSONALIZAR-V106 TO PERSONALIZADA-WK
                     EVALUATE PERSONALIZAR-V106
                       WHEN 1 MOVE "1-Sim"  TO GS-LINDET1(52: 10)
                       WHEN 2 MOVE "2-Não"  TO GS-LINDET1(52: 10)
                     END-EVALUATE
                     MOVE TEMPO-V106        TO TEMPO-WK HORA-E
                     MOVE HORA-E            TO GS-LINDET1(62: 5)
                     WRITE REG-WORK
                     END-WRITE
                     MOVE NR-MASTER-V106    TO ULT-MASTER
                     MOVE "INSERE-LIST2" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  END-IF
             END-READ
           END-PERFORM.
           ADD 1 TO ULT-MASTER
           MOVE ULT-MASTER               TO GS-MASTER.
       CARREGAR-INSTRUCAO-COPIA SECTION.
           MOVE GS-CONTRATO           TO CONTRATO-V107
           MOVE GS-ITEM               TO ITEM-V107.
           READ VID107 INVALID KEY
                INITIALIZE REG-VID107.
           MOVE DATA-INIC-V107        TO GS-DATA-INI-COP
           MOVE HORA-INIC-V107        TO GS-HORA-INI-COP
           MOVE DATA-FIM-V107         TO GS-DATA-FIM-COP
           MOVE HORA-FIM-V107         TO GS-HORA-FIM-COP
           MOVE QT-HORA-SERV-COP-V107 TO GS-TEMPO-SERV-COP
           EVALUATE AVALIACAO-GERAL-V107
             WHEN 1 MOVE "1-Péssima"  TO GS-AVAL-GER-COP
             WHEN 2 MOVE "2-Ruim   "  TO GS-AVAL-GER-COP
             WHEN 3 MOVE "3-Regular"  TO GS-AVAL-GER-COP
             WHEN 4 MOVE "4-Boa    "  TO GS-AVAL-GER-COP
             WHEN 5 MOVE "5-Ótima  "  TO GS-AVAL-GER-COP
           END-EVALUATE.

       CARREGAR-INSTRUCAO-INSERT SECTION.
           MOVE GS-CONTRATO           TO CONTRATO-V108
           MOVE GS-ITEM               TO ITEM-V108.
           READ VID108 INVALID KEY
                INITIALIZE REG-VID108.
           MOVE DATA-INIC-V108        TO GS-DATA-INI-INS
           MOVE HORA-INIC-V108        TO GS-HORA-INI-INS
           MOVE DATA-FIM-V108         TO GS-DATA-FIM-INS
           MOVE HORA-FIM-V108         TO GS-HORA-FIM-INS
           MOVE QT-HORA-SERV-INS-V108 TO GS-TEMPO-SERV-INS
           EVALUATE AVALIACAO-GERAL-V108
             WHEN 1 MOVE "1-Péssima"  TO GS-AVAL-GER-INS
             WHEN 2 MOVE "2-Ruim   "  TO GS-AVAL-GER-INS
             WHEN 3 MOVE "3-Regular"  TO GS-AVAL-GER-INS
             WHEN 4 MOVE "4-Boa    "  TO GS-AVAL-GER-INS
             WHEN 5 MOVE "5-Ótima  "  TO GS-AVAL-GER-INS
           END-EVALUATE.
       CARREGAR-INSTRUCAO-INTERRUPCAO SECTION.
           CLOSE VID109.  OPEN I-O VID109.
           OPEN INPUT CGD001.
           MOVE GS-CONTRATO            TO CONTRATO-V109.
           MOVE GS-ITEM                TO ITEM-V109.
           MOVE ZEROS                  TO TIPO-ATIV-V109
                                        TIPO-INTERRUP-V109 GS-CONT2.
           START VID109 KEY IS NOT < CHAVE-V109 INVALID KEY
                 MOVE "10" TO ST-VID109.
           PERFORM UNTIL ST-VID109 = "10"
             READ VID109 NEXT RECORD AT END
                  MOVE "10" TO ST-VID109
             NOT AT END
                  IF CONTRATO-V109 <> GS-CONTRATO OR
                     ITEM-V109 <> GS-ITEM
                     MOVE "10" TO ST-VID109
                  ELSE
                     MOVE TIPO-ATIV-V109    TO TIPO-ATIV-WK
                     EVALUATE TIPO-ATIV-V109
                       WHEN 1 MOVE "1-Edição"  TO GS-LINDET2(1: 12)
                       WHEN 2 MOVE "2-Cópia "  TO GS-LINDET2(1: 12)
                       WHEN 3 MOVE "3-Insert"  TO GS-LINDET2(1: 12)
                     END-EVALUATE
                     MOVE TIPO-INTERRUP-V109   TO GS-LINDET2(13: 2)
                                                  CODIGO-LB29
                                                  TIPO-INTERR-WK
                     READ LBD029 INVALID KEY
                          MOVE SPACES TO DESCRICAO-LB29
                     END-READ
                     MOVE "-"                  TO GS-LINDET2(15: 1)
                     MOVE DESCRICAO-LB29       TO GS-LINDET2(16: 21)
                     MOVE TEMPO-INTERRUP-V109  TO GS-LINDET2(37: 10)
                                                  HR-INTERR-WK
                     MOVE FUNCIONARIO-V109     TO GS-LINDET2(47: 6)
                                                  CODIGO-CG01
                                                  FUNCIONARIO-WK
                     READ CGD001 INVALID KEY
                          MOVE SPACES TO NOME-CG01
                     END-READ
                     MOVE NOME-CG01            TO GS-LINDET2(54: 20)
                     MOVE "-"                  TO GS-LINDET2(53: 1)
                     WRITE REG-WORK1
                     END-WRITE
                     MOVE "INSERE-LIST3" TO DS-PROCEDURE
                     PERFORM CALL-DIALOG-SYSTEM
                  END-IF
             END-READ
           END-PERFORM.
           CLOSE CGD001.

       CARREGAR-OBS SECTION.
           MOVE GS-CONTRATO           TO CONTRATO-V110
           MOVE GS-ITEM               TO ITEM-V110
           MOVE ZEROS                 TO TIPO-V110
           START VID110 KEY IS NOT < CHAVE-V110 INVALID KEY
              MOVE "10" TO ST-VID110.
           PERFORM UNTIL ST-VID110 = "10"
             READ VID110 NEXT RECORD AT END
                  MOVE "10" TO ST-VID110
             NOT AT END
                  IF CONTRATO-V110 <> GS-CONTRATO OR
                     ITEM-V110 <> GS-ITEM
                     MOVE "10" TO ST-VID110
                  ELSE
                     EVALUATE TIPO-V110
                       WHEN 1 MOVE FUNCIONARIO-V110 TO GS-EDITOR
                              MOVE 1                TO GS-TIPO-GERAL
                              PERFORM LE-GERAL
                              MOVE REVISOR-V110     TO GS-REVISOR
                              MOVE 2                TO GS-TIPO-GERAL
                              PERFORM LE-GERAL
                       WHEN 2 MOVE FUNCIONARIO-V110 TO GS-FUNCION-COP
                              MOVE 3                TO GS-TIPO-GERAL
                              PERFORM LE-GERAL
                              MOVE REVISOR-V110     TO GS-REVISOR-COP
                              MOVE 4                TO GS-TIPO-GERAL
                              PERFORM LE-GERAL
                       WHEN 3 MOVE FUNCIONARIO-V110 TO GS-FUNCION-INS
                              MOVE 5                TO GS-TIPO-GERAL
                              PERFORM LE-GERAL
                              MOVE REVISOR-V110     TO GS-REVISOR-INS
                              MOVE 6                TO GS-TIPO-GERAL
                              PERFORM LE-GERAL
                     END-EVALUATE
                     MOVE CONTRATO-V110  TO CONTRATO-V111
                     MOVE ITEM-V110      TO ITEM-V111
                     MOVE TIPO-V110      TO TIPO-V111
                     MOVE ZEROS          TO SEQ-V111
                     START VID111 KEY IS NOT < CHAVE-V111 INVALID KEY
                           MOVE "10" TO ST-VID111
                     END-START
                     MOVE 1 TO I J K
                     PERFORM UNTIL ST-VID111 = "10"
                           READ VID111 NEXT RECORD AT END
                                MOVE "10" TO ST-VID111
                           NOT AT END
                               IF CONTRATO-V111 <> GS-CONTRATO OR
                                  ITEM-V111 <> GS-ITEM
                                  MOVE "10" TO ST-VID111
                               ELSE
                                  EVALUATE TIPO-V111
                                  WHEN 1
                                     MOVE OBS-V111 TO GS-OBS(I: 100)
                                     ADD 100 TO I
                                  WHEN 2
                                     MOVE OBS-V111 TO GS-OBS-COP(J: 100)
                                     ADD 100 TO J
                                  WHEN 3
                                     MOVE OBS-V111 TO GS-OBS-INS(K: 100)
                                     ADD 100 TO K
                                END-EVALUATE
                               END-IF
                           END-READ
                     END-PERFORM
                  END-IF
             END-READ
           END-PERFORM.
      *----------------------------------------------------------
       CARREGA-MENSAGEM-ERRO SECTION.
           PERFORM LOAD-SCREENSET
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
           MOVE 1 TO ERRO-W.
       LIMPAR-DADOS SECTION.
           INITIALIZE REG-VID105 REG-VID106 REG-VID107 REG-VID108
                      REG-VID109 REG-VID110 REG-VID111.
           INITIALIZE GS-DATA-BLOCK
           MOVE ULT-SEQ TO GS-SEQ
           MOVE DATA-MOVTO-W TO GS-DATA-MOVTO.
           PERFORM SET-UP-FOR-REFRESH-SCREEN.
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
      *SALVAR, EXCLUIR CONTRATO

       SALVAR-DADOS SECTION.
           PERFORM EXCLUIR-DADOS-CONTRATO.

      *    GRAVA EDIÇÃO
           MOVE GS-CONTRATO         TO CONTRATO-V105
           MOVE GS-ITEM             TO ITEM-V105
           MOVE GS-DATA-INI-EDI     TO DATA-INIC-V105
           MOVE GS-HORA-INI-EDI     TO HORA-INIC-V105
           MOVE GS-DATA-FIM-EDI     TO DATA-FIM-V105
           MOVE GS-HORA-FIM-EDI     TO HORA-FIM-V105
           MOVE GS-QT-FITA-BR       TO QT-FITA-BR-V105
           MOVE GS-QT-FITA-MASTER   TO QT-FITA-MASTER-V105
           MOVE GS-QT-HORA-GRAV     TO QT-HORA-GRAV-V105
           MOVE GS-TOT-HORA-EDIT    TO QT-HORA-ED-MASTER-V105
           MOVE GS-QT-CURSO         TO QT-CURSO-V105
           MOVE GS-QT-FORMANDO      TO QT-FORMANDO-V105
           MOVE GS-HR-SERV-ED       TO QT-HORA-SERV-ED-V105
           MOVE GS-AVAL-GERAL(1: 1) TO AVALIACAO-W
           IF AVALIACAO-W = SPACES
              MOVE ZEROS            TO AVALIACAO-GERAL-V105
           ELSE
              MOVE AVALIACAO-W      TO AVALIACAO-GERAL-V105.

           MOVE USUARIO-W           TO USUARIO-V105.

           WRITE REG-VID105 INVALID KEY
                 MOVE "Erro gravacao VID105" TO GS-MENSAGEM-ERRO
                 MOVE ST-VID105 TO GS-MENSAGEM-ERRO(24: 5)
                 MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                 PERFORM CALL-DIALOG-SYSTEM.


      *    gravar master
           MOVE ZEROS       TO NR-MASTER-WK NR-FITA-WK CONTRATO-WK
                               IDENTIFICADOR-WK
           START WORK KEY IS NOT < CHAVE-WK INVALID KEY
                 MOVE "10" TO ST-WORK.
           PERFORM UNTIL ST-WORK = "10"
                 READ WORK NEXT RECORD AT END
                      MOVE "10" TO ST-WORK
                 NOT AT END
                     MOVE GS-CONTRATO            TO CONTRATO-V106
                     MOVE GS-ITEM                TO ITEM-V106
                     MOVE NR-FITA-WK             TO NR-FITA-V106
                     MOVE NR-MASTER-WK           TO NR-MASTER-V106
                     MOVE PERSONALIZADA-WK(1: 1) TO PERSONALIZAR-V106
                     MOVE TEMPO-WK               TO TEMPO-V106
                     WRITE REG-VID106 INVALID KEY
                         MOVE "Erro gravacao VID106" TO GS-MENSAGEM-ERRO
                         MOVE ST-VID106 TO GS-MENSAGEM-ERRO(24: 5)
                         MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                     END-WRITE
                 END-READ
           END-PERFORM.
      *    gravar copia
           MOVE GS-CONTRATO           TO CONTRATO-V107
           MOVE GS-ITEM               TO ITEM-V107
           MOVE GS-DATA-INI-COP       TO DATA-INIC-V107
           MOVE GS-HORA-INI-COP       TO HORA-INIC-V107
           MOVE GS-DATA-FIM-COP       TO DATA-FIM-V107
           MOVE GS-HORA-FIM-COP       TO HORA-FIM-V107
           MOVE GS-TEMPO-SERV-COP     TO QT-HORA-SERV-COP-V107
           MOVE GS-AVAL-GER-COP(1: 1) TO AVALIACAO-W
           IF AVALIACAO-W = SPACES
              MOVE ZEROS              TO AVALIACAO-GERAL-V107
           ELSE
              MOVE AVALIACAO-W        TO AVALIACAO-GERAL-V107.

           WRITE REG-VID107 INVALID KEY
              MOVE "Erro gravacao VID107" TO GS-MENSAGEM-ERRO
              MOVE ST-VID107 TO GS-MENSAGEM-ERRO(24: 5)
              MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

      *    grava insert
           MOVE GS-CONTRATO           TO CONTRATO-V108
           MOVE GS-ITEM               TO ITEM-V108
           MOVE GS-DATA-INI-INS       TO DATA-INIC-V108
           MOVE GS-HORA-INI-INS       TO HORA-INIC-V108
           MOVE GS-DATA-FIM-INS       TO DATA-FIM-V108
           MOVE GS-HORA-FIM-INS       TO HORA-FIM-V108
           MOVE GS-TEMPO-SERV-INS     TO QT-HORA-SERV-INS-V108
           MOVE GS-AVAL-GER-INS(1: 1) TO AVALIACAO-W
           IF AVALIACAO-W = SPACES
              MOVE ZEROS TO AVALIACAO-GERAL-V108
           ELSE
              MOVE AVALIACAO-W TO AVALIACAO-GERAL-V108.

           WRITE REG-VID108 INVALID KEY
              MOVE "Erro gravacao VID108" TO GS-MENSAGEM-ERRO
              MOVE ST-VID108 TO GS-MENSAGEM-ERRO(24: 5)
              MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
              PERFORM CALL-DIALOG-SYSTEM.

      *    gravar interrupcao
           CLOSE WORK1
           OPEN I-O WORK1
           MOVE ZEROS       TO TIPO-ATIV-WK TIPO-INTERR-WK
           START WORK1 KEY IS NOT < CHAVE-WK1 INVALID KEY
                MOVE "10" TO ST-WORK1.
           PERFORM UNTIL ST-WORK1 = "10"
                READ WORK1 NEXT RECORD AT END
                     MOVE "10" TO ST-WORK1
                NOT AT END
                    MOVE GS-CONTRATO            TO CONTRATO-V109
                    MOVE GS-ITEM                TO ITEM-V109
                    MOVE TIPO-ATIV-WK           TO TIPO-ATIV-V109
                    MOVE TIPO-INTERR-WK         TO TIPO-INTERRUP-V109
                    MOVE FUNCIONARIO-WK         TO FUNCIONARIO-V109
                    MOVE HR-INTERR-WK           TO TEMPO-INTERRUP-V109
                    WRITE REG-VID109 INVALID KEY
                         MOVE "Erro gravacao VID109" TO GS-MENSAGEM-ERRO
                         MOVE ST-VID109 TO GS-MENSAGEM-ERRO(24: 5)
                         MOVE "ERRO-GRAVACAO" TO DS-PROCEDURE
                         PERFORM CALL-DIALOG-SYSTEM
                    END-WRITE
                END-READ
           END-PERFORM.

      *   GRAVAR OBS E INSTRUÇÕES - CABECALHO - EDICAO
           MOVE GS-CONTRATO        TO CONTRATO-V110
           MOVE GS-ITEM            TO ITEM-V110
           MOVE 1                  TO TIPO-V110
           MOVE GS-EDITOR          TO FUNCIONARIO-V110
           MOVE GS-REVISOR         TO REVISOR-V110
           MOVE DATA-MOVTO-W       TO DATA-V110
           ACCEPT HORARIO-V110 FROM TIME.
           MOVE USUARIO-W          TO USUARIO-V110
           WRITE REG-VID110.
      *   GRAVAR OBS E INSTRUÇÕES -EDICAO

           MOVE GS-CONTRATO        TO CONTRATO-V111
           MOVE GS-ITEM            TO ITEM-V111
           MOVE 1                  TO TIPO-V111
           MOVE ZEROS TO SEQ-V111
           PERFORM VARYING I FROM 1 BY 100 UNTIL I > 500
             MOVE GS-OBS(I: 100) TO OBS-W
             IF OBS-W = SPACES MOVE 500 TO I
             ELSE MOVE OBS-W       TO OBS-V111
                  ADD 1            TO SEQ-V111
                  WRITE REG-VID111
                  END-WRITE
             END-IF
           END-PERFORM.
      *   GRAVAR OBS E INSTRUÇÕES - CABECALHO - COPIA
           MOVE GS-CONTRATO        TO CONTRATO-V110
           MOVE GS-ITEM            TO ITEM-V110
           MOVE 2                  TO TIPO-V110
           MOVE GS-FUNCION-COP     TO FUNCIONARIO-V110
           MOVE GS-REVISOR-COP     TO REVISOR-V110
           MOVE DATA-MOVTO-W         TO DATA-V110
           ACCEPT HORARIO-V110 FROM TIME.
           MOVE USUARIO-W          TO USUARIO-V110
           WRITE REG-VID110.
      *   GRAVAR OBS E INSTRUÇÕES -COPIA

           MOVE GS-CONTRATO        TO CONTRATO-V111
           MOVE GS-ITEM            TO ITEM-V111
           MOVE 2                  TO TIPO-V111
           MOVE ZEROS TO SEQ-V111
           PERFORM VARYING J FROM 1 BY 100 UNTIL J > 500
             MOVE GS-OBS-COP(J: 100) TO OBS-W
             IF OBS-W = SPACES MOVE 500 TO J
             ELSE MOVE OBS-W       TO OBS-V111
                  ADD 1            TO SEQ-V111
                  WRITE REG-VID111
                  END-WRITE
             END-IF
           END-PERFORM.

      *   GRAVAR OBS E INSTRUÇÕES - CABECALHO - INSERT
           MOVE GS-CONTRATO        TO CONTRATO-V110
           MOVE GS-ITEM            TO ITEM-V110
           MOVE 3                  TO TIPO-V110
           MOVE GS-FUNCION-INS     TO FUNCIONARIO-V110
           MOVE GS-REVISOR-INS     TO REVISOR-V110
           MOVE DATA-MOVTO-W         TO DATA-V110
           ACCEPT HORARIO-V110 FROM TIME.
           MOVE USUARIO-W          TO USUARIO-V110
           WRITE REG-VID110.
      *   GRAVAR OBS E INSTRUÇÕES -COPIA

           MOVE GS-CONTRATO        TO CONTRATO-V111
           MOVE GS-ITEM            TO ITEM-V111
           MOVE 3                  TO TIPO-V111
           MOVE ZEROS TO SEQ-V111
           PERFORM VARYING K FROM 1 BY 100 UNTIL K > 500
             MOVE GS-OBS-INS(K: 100) TO OBS-W
             IF OBS-W = SPACES MOVE 500 TO K
             ELSE MOVE OBS-W       TO OBS-V111
                  ADD 1            TO SEQ-V111
                  WRITE REG-VID111
                  END-WRITE
             END-IF
           END-PERFORM.

       EXCLUIR-DADOS-CONTRATO SECTION.
           MOVE GS-CONTRATO   TO CONTRATO-V105
           MOVE GS-ITEM       TO ITEM-V105
           READ VID105 INVALID KEY CONTINUE
             NOT INVALID KEY DELETE VID105.

           MOVE GS-CONTRATO   TO CONTRATO-V106
           MOVE GS-ITEM       TO ITEM-V106
           MOVE ZEROS         TO NR-MASTER-V106.
           START VID106 KEY IS NOT < CHAVE-V106 INVALID KEY
                 MOVE "10" TO ST-VID106.
           PERFORM UNTIL ST-VID106 = "10"
            READ VID106 NEXT RECORD AT END MOVE "10" TO ST-VID106
              NOT AT END
                IF CONTRATO-V106 <> GS-CONTRATO OR
                   ITEM-V106 <> GS-ITEM
                     MOVE "10" TO ST-VID106
                ELSE
                  DELETE VID106
                END-IF
            END-READ
           END-PERFORM.

           MOVE GS-CONTRATO   TO CONTRATO-V107
           MOVE GS-ITEM       TO ITEM-V107
           READ VID107 INVALID KEY CONTINUE
             NOT INVALID KEY DELETE VID107.

           MOVE GS-CONTRATO   TO CONTRATO-V108
           MOVE GS-ITEM       TO ITEM-V108
           READ VID108 INVALID KEY CONTINUE
             NOT INVALID KEY DELETE VID108.

           MOVE GS-CONTRATO   TO CONTRATO-V109
           MOVE GS-ITEM       TO ITEM-V109
           MOVE ZEROS         TO TIPO-ATIV-V109 TIPO-INTERRUP-V109
           START VID109 KEY IS NOT < CHAVE-V109 INVALID KEY
                 MOVE "10" TO ST-VID109.
           PERFORM UNTIL ST-VID109 = "10"
            READ VID109 NEXT RECORD AT END MOVE "10" TO ST-VID109
              NOT AT END
                IF CONTRATO-V109 <> GS-CONTRATO OR
                   ITEM-V109 <> GS-ITEM
                     MOVE "10" TO ST-VID109
                ELSE
                  DELETE VID109
                END-IF
            END-READ
           END-PERFORM.

           MOVE GS-CONTRATO   TO CONTRATO-V110
           MOVE GS-ITEM       TO ITEM-V110
           MOVE ZEROS         TO TIPO-V110.
           START VID110 KEY IS NOT < CHAVE-V110 INVALID KEY
              MOVE "10" TO ST-VID110.
           PERFORM UNTIL ST-VID110 = "10"
             READ VID110 NEXT RECORD AT END MOVE "10" TO ST-VID110
               NOT AT END
                 IF CONTRATO-V110 <> GS-CONTRATO OR
                    ITEM-V110 <> GS-ITEM MOVE "10" TO ST-VID110
                 ELSE
                   DELETE VID110
                 END-IF
             END-READ
           END-PERFORM.

           MOVE GS-CONTRATO   TO CONTRATO-V111
           MOVE GS-ITEM       TO ITEM-V111
           MOVE ZEROS         TO TIPO-V111 SEQ-V111.
           START VID111 KEY IS NOT < CHAVE-V111 INVALID KEY
              MOVE "10" TO ST-VID111.
           PERFORM UNTIL ST-VID111 = "10"
             READ VID111 NEXT RECORD AT END MOVE "10" TO ST-VID111
               NOT AT END
                 IF CONTRATO-V111 <> GS-CONTRATO OR
                    ITEM-V111 <> GS-ITEM MOVE "10" TO ST-VID111
                 ELSE
                   DELETE VID111
                 END-IF
             END-READ
           END-PERFORM.

       ERRO-GRAVACAO SECTION.
           MOVE "ERRO GRAVAÇÃO" TO GS-MENSAGEM-ERRO
           PERFORM LOAD-SCREENSET
           PERFORM CARREGA-MENSAGEM-ERRO.

      *------------------------------------------------------------
       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "VIP105" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.
      *------------------------------------------------------------
       IMPRIME-RELATORIO SECTION.
           MOVE ZEROS TO PAG-W.
           OPEN OUTPUT RELAT.
           PERFORM CABECALHO.
           PERFORM MOVER-DADOS-REL.
           MOVE SPACES TO REG-RELAT.
           WRITE REG-RELAT AFTER PAGE.
           CLOSE RELAT.

       MOVER-DADOS-REL SECTION.
           MOVE GS-CONTRATO        TO NR-CONTRATO-REL
           MOVE GS-ITEM            TO ITEM-REL
           MOVE GS-CURSO           TO CURSO-REL
           MOVE GS-CIDADE          TO CIDADE-REL
           WRITE REG-RELAT FROM LINHA-02
           WRITE REG-RELAT FROM LINHA-03.
           WRITE REG-RELAT FROM LINHA-03A.
           WRITE REG-RELAT FROM LINHA-03.
           WRITE REG-RELAT FROM LINHA-04.
           WRITE REG-RELAT FROM LINHA-05.
           ADD 6 TO LIN.

           OPEN INPUT CGD001.
           MOVE GS-CONTRATO TO CONTRATO-V100
           MOVE ZEROS       TO DATA-EVENTO-V100 GS-CONT.
           START VID100 KEY IS NOT < ALT1-V100 INVALID KEY
                 MOVE "10" TO ST-VID100.
           PERFORM UNTIL ST-VID100 = "10"
                 READ VID100 NEXT RECORD AT END
                      MOVE "10" TO ST-VID100
                 NOT AT END
                      IF CONTRATO-V100 <> GS-CONTRATO
                         MOVE "10" TO ST-VID100
                      ELSE
                         MOVE IDENTIFICADOR-V100 TO NR-FITA-REL
                         MOVE DATA-EVENTO-V100   TO DATA-INV
                         CALL "GRIDAT1" USING DATA-INV
                         MOVE DATA-INV         TO DATA-EVE-REL
                         MOVE EVENTO-V100      TO CODIGO-CO03
                         READ COD003 INVALID KEY
                              MOVE SPACES TO NOME-CO03
                         END-READ
                         MOVE NOME-CO03         TO EVENTO-REL
                         MOVE LOCALIZACAO-V100  TO LOCAL-REL
                         MOVE SPACES            TO AVAL-REL
                         MOVE CINEGRAFISTA-V100 TO CODIGO-CG01
                         READ CGD001 INVALID KEY
                              MOVE SPACES TO NOME-CG01
                         END-READ
                         MOVE NOME-CG01         TO CINEGRAFISTA-REL
                         WRITE REG-RELAT FROM LINHA-06
                         ADD 1 TO LIN
                         IF LIN > 56
                            PERFORM CABECALHO
                         END-IF
                      END-IF
                 END-READ
           END-PERFORM.
           WRITE REG-RELAT FROM LINHA-05.
           ADD 1 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.
           CLOSE CGD001.

           IF GS-EDICAO = 1
              PERFORM IMPRIMIR-EDICAO.

           IF GS-FITAS = 1
              PERFORM IMPRIMIR-FITA-MASTER.

           IF GS-COPIAS = 1
              PERFORM IMPRIMIR-COPIA.

           IF GS-INSERT = 1
              PERFORM IMPRIMIR-INSERT.

           IF GS-INTERRUPCOES = 1
              PERFORM IMPRIMIR-INTERRUPCOES.

       IMPRIMIR-EDICAO SECTION.
      *    INSTRUCOES DE EDICAO
           WRITE REG-RELAT FROM LINHA-07.
           WRITE REG-RELAT FROM LINHA-03
           MOVE GS-NOME-EDITOR   TO EDITOR-ED-REL
           MOVE GS-NOME-REVISOR  TO REVISOR-ED-REL
           WRITE REG-RELAT FROM LINHA-08
           WRITE REG-RELAT FROM LINHA-03.
           ADD 4 TO LIN.
           IF LIN > 56 PERFORM CABECALHO.

           MOVE SPACES TO OBS-W.
           PERFORM VARYING I FROM 1 BY 78 UNTIL I > 500
             MOVE GS-OBS(I: 78)      TO OBS-ED-REL OBS-W
             IF OBS-W = SPACES MOVE 500 TO I
             ELSE WRITE REG-RELAT FROM LINHA-09
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
             END-IF
           END-PERFORM.
           WRITE REG-RELAT FROM LINHA-03
           ADD 1 TO LIN.
           MOVE GS-DATA-INI-EDI      TO DATA-INI-ED-REL
           MOVE GS-HORA-INI-EDI      TO HORA-INI-ED-REL
           MOVE GS-DATA-FIM-EDI      TO DATA-FIM-ED-REL
           MOVE GS-HORA-FIM-EDI      TO HORA-FIM-ED-REL
           MOVE GS-QT-FITA-BR        TO FITA-BR-ED-REL
           MOVE GS-QT-FITA-MASTER    TO FITA-MAS-ED-REL
           MOVE GS-QT-HORA-GRAV      TO HORAS-GRAV-ED-REL
           MOVE GS-TOT-HORA-EDIT     TO HORAS-ED-MASTER-ED-REL
           MOVE GS-QT-CURSO          TO CURSO-ED-REL
           MOVE GS-QT-FORMANDO       TO FORM-ED-REL
           MOVE GS-HR-SERV-ED        TO HORA-SERV-ED-REL
           MOVE GS-AVAL-GERAL        TO AVAL-ED-REL
           IF LIN > 52 PERFORM CABECALHO.
           WRITE REG-RELAT FROM LINHA-10.
           WRITE REG-RELAT FROM LINHA-11.
           WRITE REG-RELAT FROM LINHA-12.
           WRITE REG-RELAT FROM LINHA-13.
           WRITE REG-RELAT FROM LINHA-14.
           WRITE REG-RELAT FROM LINHA-16.
           WRITE REG-RELAT FROM LINHA-03.
           ADD 9 TO LIN.
       IMPRIMIR-EDICAO-FIM.
           EXIT.

       IMPRIMIR-FITA-MASTER SECTION.
      *    FITAS MASTER
           WRITE REG-RELAT FROM LINHA-17.
           WRITE REG-RELAT FROM LINHA-03.
           WRITE REG-RELAT FROM LINHA-18.
           WRITE REG-RELAT FROM LINHA-18A.

           MOVE GS-CONTRATO            TO CONTRATO-V106.
           MOVE GS-ITEM                TO ITEM-V106.
           MOVE ZEROS                  TO NR-MASTER-V106
                                          GS-CONT1.
           START VID106 KEY IS NOT < CHAVE-V106 INVALID KEY
                 MOVE "10" TO ST-VID106.
           PERFORM UNTIL ST-VID106 = "10"
                 READ VID106 NEXT RECORD AT END
                     MOVE "10" TO ST-VID106
                 NOT AT END
                     IF CONTRATO-V106 <> GS-CONTRATO OR
                        ITEM-V106     <> GS-ITEM
                        MOVE "10" TO ST-VID106
                     ELSE
                        MOVE NR-MASTER-V106    TO NR-MASTER-ED-REL
                        INITIALIZE REG-VID100
                        MOVE CONTRATO-V106     TO CONTRATO-V100
                        MOVE NR-FITA-V106      TO NR-FITA-V100
                        START VID100 KEY IS NOT < ALT-V100 INVALID KEY
                             MOVE ZEROS        TO EVENTO-V100
                             MOVE ZEROS        TO IDENTIFICADOR-V100
                        NOT INVALID KEY
                             READ VID100 NEXT AT END
                                  MOVE ZEROS   TO EVENTO-V100
                                  MOVE ZEROS   TO IDENTIFICADOR-V100
                             NOT AT END
                                  IF CONTRATO-V106 <> CONTRATO-V100 OR
                                     NR-FITA-V106  <> NR-FITA-V100
                                     MOVE ZEROS TO EVENTO-V100
                                     MOVE ZEROS TO IDENTIFICADOR-V100
                                  END-IF
                             END-READ
                        END-START
                        MOVE IDENTIFICADOR-V100 TO NR-FITA-ED-REL
                        MOVE EVENTO-V100        TO CODIGO-CO03
                        READ COD003 INVALID KEY
                             MOVE SPACES        TO NOME-CO03
                        END-READ
                        MOVE NOME-CO03          TO EVENTO-ED-REL
                        EVALUATE PERSONALIZAR-V106
                          WHEN 1 MOVE "Sim"     TO PERS-ED-REL
                          WHEN 2 MOVE "Nao"     TO PERS-ED-REL
                        END-EVALUATE
                        MOVE TEMPO-V106         TO TEMPO-MASTER-REL
                        WRITE REG-RELAT FROM LINHA-19
                        ADD 1 TO LIN
                        IF LIN > 56
                           PERFORM CABECALHO
                        END-IF
                     END-IF
                 END-READ
           END-PERFORM.
           WRITE REG-RELAT FROM LINHA-18A.
           ADD 1 TO LIN.
       IMPRIMIR-FITA-MASTER-FIM.
           EXIT.

       IMPRIMIR-COPIA SECTION.
      *    INSTRUCOES P/ COPIA
           PERFORM CABECALHO.
           MOVE GS-NOME-FUNCION-COP     TO FUNCIO-COP-REL
           MOVE GS-NOME-REVISOR-COP     TO REVISOR-COP-REL.
           WRITE REG-RELAT FROM LINHA-20.
           WRITE REG-RELAT FROM LINHA-03.
           WRITE REG-RELAT FROM LINHA-21.
           WRITE REG-RELAT FROM LINHA-03.
           ADD 4 TO LIN.
           MOVE SPACES TO OBS-W.
           PERFORM VARYING I FROM 1 BY 78 UNTIL I > 500
               MOVE GS-OBS-COP(I: 78)      TO OBS-COP-REL OBS-W
               IF OBS-W = SPACES
                  MOVE 500 TO I
               ELSE
                  WRITE REG-RELAT FROM LINHA-22
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
               END-IF
           END-PERFORM.
           WRITE REG-RELAT FROM LINHA-03
           ADD 1 TO LIN.

           MOVE GS-DATA-INI-COP       TO DATA-INI-COP-REL
           MOVE GS-DATA-FIM-COP       TO DATA-FIM-COP-REL
           MOVE GS-HORA-INI-COP       TO HORA-INI-COP-REL
           MOVE GS-HORA-FIM-COP       TO HORA-FIM-COP-REL
           MOVE GS-TEMPO-SERV-COP     TO HORA-SERV-COP-REL
           MOVE GS-AVAL-GER-COP       TO AVAL-COP-REL
           IF LIN > 56 PERFORM CABECALHO.
           WRITE REG-RELAT FROM LINHA-23
           WRITE REG-RELAT FROM LINHA-24
           WRITE REG-RELAT FROM LINHA-26
           WRITE REG-RELAT FROM LINHA-26A
           ADD 5 TO LIN.
       IMPRIMIR-COPIA-FIM.
           EXIT.

       IMPRIMIR-INSERT SECTION.
      *    INSTRUCOES P/ INSERT
           PERFORM CABECALHO.
           MOVE GS-NOME-FUNCION-INS     TO FUNCION-INS-REL
           MOVE GS-NOME-REVISOR-INS     TO REVISOR-INS-REL.
           WRITE REG-RELAT FROM LINHA-27.
           WRITE REG-RELAT FROM LINHA-03.
           WRITE REG-RELAT FROM LINHA-28.
           WRITE REG-RELAT FROM LINHA-03.
           ADD 4 TO LIN.
           MOVE SPACES TO OBS-W.
           PERFORM VARYING I FROM 1 BY 78 UNTIL I > 500
             MOVE GS-OBS-INS(I: 78)      TO OBS-INS-REL OBS-W
             IF OBS-W = SPACES MOVE 500 TO I
             ELSE WRITE REG-RELAT FROM LINHA-29
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
             END-IF
           END-PERFORM.
           WRITE REG-RELAT FROM LINHA-03
           ADD 1 TO LIN.

           MOVE GS-DATA-INI-INS       TO DATA-INI-INS-REL
           MOVE GS-DATA-FIM-INS       TO DATA-FIM-INS-REL
           MOVE GS-HORA-INI-INS       TO HORA-INI-INS-REL
           MOVE GS-HORA-FIM-INS       TO HORA-FIM-INS-REL
           MOVE GS-TEMPO-SERV-INS     TO HORA-SERV-INS-REL
           MOVE GS-AVAL-GER-INS       TO AVAL-INS-REL
           IF LIN > 56 PERFORM CABECALHO.
           WRITE REG-RELAT FROM LINHA-30
           WRITE REG-RELAT FROM LINHA-31
           WRITE REG-RELAT FROM LINHA-33
           WRITE REG-RELAT FROM LINHA-26A
           WRITE REG-RELAT FROM LINHA-03.
           ADD 5 TO LIN.
       IMPRIMIR-INSERT-FIM.
           EXIT.

       IMPRIMIR-INTERRUPCOES SECTION.
      *    INTERRUPCOES
           PERFORM CABECALHO.
           WRITE REG-RELAT FROM LINHA-34.
           MOVE SPACES TO INTERRUPCAO-REL.
           WRITE REG-RELAT FROM LINHA-37.
           WRITE REG-RELAT FROM LINHA-35.
           WRITE REG-RELAT FROM LINHA-36.
           ADD 4 TO LIN.
           MOVE GS-CONTRATO            TO CONTRATO-V109.
           MOVE GS-ITEM                TO ITEM-V109.
           MOVE ZEROS                  TO TIPO-ATIV-V109
                                        TIPO-INTERRUP-V109 GS-CONT2.
           START VID109 KEY IS NOT < CHAVE-V109 INVALID KEY
                 MOVE "10" TO ST-VID109.
           OPEN INPUT CGD001.
           PERFORM UNTIL ST-VID109 = "10"
             READ VID109 NEXT RECORD AT END
                  MOVE "10" TO ST-VID109
              NOT AT END
               IF CONTRATO-V109 <> GS-CONTRATO OR
                  ITEM-V109 <> GS-ITEM
                  MOVE "10" TO ST-VID109
               ELSE
                  EVALUATE TIPO-ATIV-V109
                    WHEN 1 MOVE "1-Edição"  TO INTERRUPCAO-REL(1: 12)
                    WHEN 2 MOVE "2-Cópia "  TO INTERRUPCAO-REL(1: 12)
                    WHEN 3 MOVE "3-Insert"  TO INTERRUPCAO-REL(1: 12)
                  END-EVALUATE
                  MOVE TIPO-INTERRUP-V109   TO INTERRUPCAO-REL(13: 2)
                                               CODIGO-LB29
                  READ LBD029 INVALID KEY
                       MOVE SPACES TO DESCRICAO-LB29
                  END-READ
                  MOVE "-"                  TO INTERRUPCAO-REL(15: 1)
                  MOVE DESCRICAO-LB29       TO INTERRUPCAO-REL(16: 21)
                  MOVE TEMPO-INTERRUP-V109  TO HORA-E
                  MOVE HORA-E               TO INTERRUPCAO-REL(37: 10)
                  MOVE FUNCIONARIO-V109     TO INTERRUPCAO-REL(47: 6)
                                               CODIGO-CG01
                  READ CGD001 INVALID KEY
                       MOVE SPACES TO NOME-CG01
                  END-READ
                  MOVE "-"                  TO INTERRUPCAO-REL(53: 1)
                  MOVE NOME-CG01            TO INTERRUPCAO-REL(54: 20)
                  WRITE REG-RELAT FROM LINHA-37
                  ADD 1 TO LIN
                  IF LIN > 56 PERFORM CABECALHO
                  END-IF
               END-IF
             END-READ
           END-PERFORM.
           CLOSE CGD001.
           WRITE REG-RELAT FROM LINHA-03.
       IMPRIMIR-INTERRUPCOES-FIM.
           EXIT.

       CABECALHO SECTION.
           ADD 1 TO PAG-W.  MOVE PAG-W TO PAG-REL.
           IF PAG-W = 1
              WRITE REG-RELAT FROM CAB01
           ELSE WRITE REG-RELAT FROM CAB01 AFTER PAGE.
           WRITE REG-RELAT FROM CAB02 AFTER 2.
           WRITE REG-RELAT FROM LINHA-01.
           MOVE 4 TO LIN.
      *-------------------------------------------------------------
       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD003 LBD029 VID100 VID105 VID106
                 VID107 VID108 VID109 VID110 VID111 WORK WORK1.
           DELETE FILE WORK WORK1.
           MOVE DS-QUIT-SET TO DS-CONTROL.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

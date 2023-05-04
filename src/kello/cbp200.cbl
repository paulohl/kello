       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBP200.
      *DATA: 20/10/1998
      *AUTORA: MARELI AMÂNCIO VOLPATO
      *PROGRAMA: Emite cheque com layout do Banestado
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RELAT ASSIGN TO PRINTER NOME-IMPRESSORA.
       DATA DIVISION.
       FILE SECTION.
       FD  RELAT.
       01  REG-REL.
           05  FILLER          PIC X(80).
       WORKING-STORAGE SECTION.
           COPY IMPRESSORA.
           COPY "CBDATA.CPY".
           COPY "CBPRINT.CPY".
       01  TABELA-DATA.
           05  FILLER         PIC X(10) VALUE "  JANEIRO ".
           05  FILLER         PIC X(10) VALUE " FEVEREIRO".
           05  FILLER         PIC X(10) VALUE "   MARCO  ".
           05  FILLER         PIC X(10) VALUE "   ABRIL  ".
           05  FILLER         PIC X(10) VALUE "   MAIO   ".
           05  FILLER         PIC X(10) VALUE "   JUNHO  ".
           05  FILLER         PIC X(10) VALUE "   JULHO  ".
           05  FILLER         PIC X(10) VALUE "  AGOSTO  ".
           05  FILLER         PIC X(10) VALUE " SETEMBRO ".
           05  FILLER         PIC X(10) VALUE "  OUTUBRO ".
           05  FILLER         PIC X(10) VALUE " NOVEMBRO ".
           05  FILLER         PIC X(10) VALUE " DEZEMBRO ".
       01  TAB-DAT REDEFINES TABELA-DATA.
           05  TABMES OCCURS 12 TIMES PIC X(10).
       01  VARIAVEIS.
           05  DDMMAAAA1.
               10  DD         PIC 99.
               10  MM         PIC 99.
               10  AAAA       PIC 9999.
           05  DDMMAAAA REDEFINES DDMMAAAA1 PIC 9(8).
           05  WS-DATA.
               10  DIA        PIC 99  VALUE ZEROS.
               10  MES        PIC 99  VALUE ZEROS.
               10  ANO        PIC 99  VALUE ZEROS.
               10  ANO1       PIC 99  VALUE ZEROS.
           05  WSDATA REDEFINES WS-DATA PIC 9(06).
           05  DATA-EMIS.
               10  DIA-E      PIC 99  VALUE ZEROS.
               10  MES-E      PIC 99  VALUE ZEROS.
               10  ANO-E      PIC 99   VALUE ZEROS.
               10  ANO-E1     PIC 99   VALUE ZEROS.

       01  VARIAVEIS.
           05  ERRO-W                PIC 9        VALUE ZEROS.
           05  PAG-W                 PIC 99       VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).
           05  VENCTOW.
               10  ANO-VENC          PIC 9(4).
               10  MES-VENC          PIC 9(2).
               10  DIA-VENC          PIC 9(2).
           05  VENCTO-W REDEFINES VENCTOW PIC 9(8).
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  CONTDUP               PIC 9   VALUE ZEROS.
           05  S                     PIC 99       VALUE ZEROS.
           05  PASSAR-PARAMETRO1.
               10  VALOR-REC         PIC 9(8)V99  VALUE ZEROS.
               10  NOME-REC          PIC X(30)    VALUE SPACES.
           05  PASSAR-PARAMETRO REDEFINES PASSAR-PARAMETRO1 PIC X(40).

       01  UNIDADE-DEZENA.
           05  FILLER    PIC X(16)   VALUE 'UM              '.
           05  FILLER    PIC X(16)   VALUE 'DOIS            '.
           05  FILLER    PIC X(16)   VALUE 'TRES            '.
           05  FILLER    PIC X(16)   VALUE 'QUA-TRO         '.
           05  FILLER    PIC X(16)   VALUE 'CIN-CO          '.
           05  FILLER    PIC X(16)   VALUE 'SEIS            '.
           05  FILLER    PIC X(16)   VALUE 'SE-TE           '.
           05  FILLER    PIC X(16)   VALUE 'OI-TO           '.
           05  FILLER    PIC X(16)   VALUE 'NO-VE           '.
           05  FILLER    PIC X(16)   VALUE 'DEZ             '.
           05  FILLER    PIC X(16)   VALUE 'ON-ZE           '.
           05  FILLER    PIC X(16)   VALUE 'DO-ZE           '.
           05  FILLER    PIC X(16)   VALUE 'TRE-ZE          '.
           05  FILLER    PIC X(16)   VALUE 'QUA-TOR-ZE      '.
           05  FILLER    PIC X(16)   VALUE 'QUIN-ZE         '.
           05  FILLER    PIC X(16)   VALUE 'DE-ZES-SEIS     '.
           05  FILLER    PIC X(16)   VALUE 'DE-ZES-SE-TE    '.
           05  FILLER    PIC X(16)   VALUE 'DE-ZOI-TO       '.
           05  FILLER    PIC X(16)   VALUE 'DE-ZE-NO-VE     '.
       01  TABELA-UNI-DEZ REDEFINES UNIDADE-DEZENA.
           05  UNID-DEZ OCCURS 19 TIMES PIC X(16).

       01  DEZENA-1.
           05  FILLER    PIC X(16)   VALUE 'DEZ             '.
           05  FILLER    PIC X(16)   VALUE 'VIN-TE          '.
           05  FILLER    PIC X(16)   VALUE 'TRIN-TA         '.
           05  FILLER    PIC X(16)   VALUE 'QUA-REN-TA      '.
           05  FILLER    PIC X(16)   VALUE 'CIN-QUEN-TA     '.
           05  FILLER    PIC X(16)   VALUE 'SES-SEN-TA      '.
           05  FILLER    PIC X(16)   VALUE 'SE-TEN-TA       '.
           05  FILLER    PIC X(16)   VALUE 'OI-TEN-TA       '.
           05  FILLER    PIC X(16)   VALUE 'NO-VEN-TA       '.
       01  TABELA-DEZENA REDEFINES DEZENA-1.
           05  DEZENA OCCURS 9 TIMES PIC X(16).

       01  CENTENA-1.
           05  FILLER    PIC X(16)   VALUE 'CEM             '.
           05  FILLER    PIC X(16)   VALUE 'DU-ZEN-TOS      '.
           05  FILLER    PIC X(16)   VALUE 'TRE-ZEN-TOS     '.
           05  FILLER    PIC X(16)   VALUE 'QUA-TRO-CEN-TOS '.
           05  FILLER    PIC X(16)   VALUE 'QUI-NHEN-TOS    '.
           05  FILLER    PIC X(16)   VALUE 'SEIS-CEN-TOS    '.
           05  FILLER    PIC X(16)   VALUE 'SE-TE-CEN-TOS   '.
           05  FILLER    PIC X(16)   VALUE 'OI-TO-CEN-TOS   '.
           05  FILLER    PIC X(16)   VALUE 'NO-VE-CEN-TOS   '.
       01  TABELA-CENTENA REDEFINES CENTENA-1.
           05  CENTENA OCCURS 9 TIMES PIC X(16).

       01  NUM-EXT.
           05  EXT OCCURS 16 TIMES PIC X.


      *    --- A tabela abaixo deve ter tantas ocorrˆncias quantos
      *        forem o n£mero de caracteres da linha detalhe.

       01  TABELA-S.
           05  TAB-S OCCURS 65 TIMES PIC X.

       01  NUM-3DIG.
           05  D1                        PIC 9 VALUE ZEROS.
           05  NUM-2DIG.
               10  D2                    PIC 9 VALUE ZEROS.
               10  D3                    PIC 9 VALUE ZEROS.
           05  NUM-2D REDEFINES NUM-2DIG PIC 99.
       01  NUM-3D REDEFINES NUM-3DIG     PIC 999.

       01  NUMERO.
           05  N1       PIC 99    VALUE ZEROS.
           05  N2       PIC 999   VALUE ZEROS.
           05  N3       PIC 999   VALUE ZEROS.
           05  N4       PIC 999   VALUE ZEROS.
           05  N5       PIC 99    VALUE ZEROS.
       01  NUM REDEFINES NUMERO PIC 9(11)V99.

       01  AUXILIARES.
           05  CONT     PIC 9     VALUE ZEROS.
           05  I        PIC 99    VALUE ZEROS.
           05  J        PIC 99    VALUE ZEROS.
           05  K        PIC 99    VALUE ZEROS.
           05  M        PIC 99    VALUE ZEROS.
           05  P        PIC 99    VALUE ZEROS.

       01  LINHA-1.
           05  FILLER             PIC X(50)  VALUE SPACES.
           05  VALOR-REL          PIC *.***.***.*99,99-.
       01  LINHA-2.
      *     05  FILLER             PIC X(10)  VALUE SPACES.
           05  NOME-REL           PIC X(45)  VALUE SPACES.
           05  FILLER             PIC X(04)  VALUE SPACES.
       01  LINHA-3.
           05  FILLER             PIC X(29)  VALUE SPACES.
           05  CIDADE-REL         PIC X(13)  VALUE SPACES.
           05  DIA-REL            PIC Z99    VALUE ZEROS.
           05  FILLER             PIC XXX    VALUE SPACES.
           05  MES-REL            PIC X(10)  VALUE SPACES.
           05  FILLER             PIC X(6)   VALUE SPACES.
           05  ANO-REL            PIC 99     VALUE ZEROS.
       01  LINHA-EXT.
      *     05  FILLER             PIC X(09)  VALUE SPACES.
           05  EXTENSO            PIC X(65)  VALUE SPACES.


       LINKAGE SECTION.
           COPY "PARAMETR".

       01  STRING-1                  PIC X(40).
       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       INICIALIZA-PROGRAMA SECTION.
           ACCEPT DATA6-W FROM DATE.
           ACCEPT HORA-BRA FROM TIME.
           MOVE DATA6-W TO DATA-INV(3: 6).
           MOVE DATA6-W(1: 2) TO ANO-V.
           IF ANO-V > 80 MOVE "19" TO DATA-INV(1: 2)
           ELSE MOVE "20" TO DATA-INV(1: 2).
           CALL "GRIDAT1" USING DATA-INV.

           MOVE DATA-INV TO DATA-EMIS.

           copy impressora.chama.

           OPEN OUTPUT RELAT.

       DESENVOLVE.

           MOVE STRING-1          TO PASSAR-PARAMETRO.
           MOVE NOME-REC          TO NOME-REL.
           MOVE "SANTA FE"        TO CIDADE-REL.
           MOVE TABMES (MES-E) TO MES-REL.
           MOVE VALOR-REC         TO VALOR-REL.
           WRITE REG-REL FROM LINHA-1 AFTER 1.
           MOVE SPACES TO REG-REL.
           WRITE REG-REL.
           MOVE SPACES TO TABELA-S.
           MOVE ZEROS TO I, J, CONTDUP.
           MOVE 10 TO K.
           MOVE VALOR-REC TO NUM.

       GRAVA-CHEQUE.
      *
      *
      *           ........   rotina de extenso   ..........
      *
      *
      *
           IF N1 = 1
              MOVE 'UM              ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT
              MOVE 'BI-LHAO         ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT.
           IF N1 = 0 OR = 1
              GO TO ESCR-MILHAO.
           MOVE N1 TO NUM-3D.
           PERFORM ESCR-3DIG THRU FIM-3DIG.
           MOVE 'BI-LHOES        ' TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.

       ESCR-MILHAO.
           IF N2 = 1
              MOVE 'UM               ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT
              MOVE 'MI-LHAO          ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT.
           IF N2 = 0 OR = 1
              GO TO ESCR-MIL.
           MOVE N2 TO NUM-3D.
           PERFORM ESCR-3DIG THRU FIM-3DIG.
           MOVE 'MI-LHOES        ' TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.

       ESCR-MIL.
           IF N3 = 0
              GO TO ESCR-UNID.
           MOVE N3 TO NUM-3D.
           PERFORM ESCR-3DIG THRU FIM-3DIG.
           MOVE 'MIL             ' TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.

       ESCR-UNID.
           IF N4 = 0
              GO TO ESCR-CRUZADO.
           MOVE N4 TO NUM-3D.
           IF D1 = 0 AND (N1 NOT = 0 OR N2 NOT = 0 OR N3 NOT = 0)
              MOVE 'E               ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT.
           IF D1 NOT = 0 AND D2 = 0 AND D3 = 0
              IF N1 NOT = 0 OR N2 NOT = 0 OR N3 NOT = 0
                 MOVE 'E               ' TO NUM-EXT
                 PERFORM ESCR-EXT THRU FIM-EXT.
           PERFORM ESCR-3DIG THRU FIM-3DIG.

       ESCR-CRUZADO.
           IF N3 = 0 AND N4 = 0 AND (N1 NOT = 0 OR N2 NOT = 0)
              MOVE 'DE              ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT.
           IF N1 = 0 AND N2 = 0 AND N3 = 0 AND N4 = 1
              MOVE 'RE-AL           ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT
      *       MOVE 'RE-AL           ' TO NUM-EXT
      *       PERFORM ESCR-EXT THRU FIM-EXT
           ELSE IF N1 NOT = 0 OR N2 NOT = 0 OR N3 NOT = 0 OR N4 NOT = 0
                MOVE 'RE-AIS          ' TO NUM-EXT
                PERFORM ESCR-EXT THRU FIM-EXT.
      *         MOVE 'RE-AIS          ' TO NUM-EXT
      *         PERFORM ESCR-EXT THRU FIM-EXT.

       ESCR-CENTAVO.
           MOVE ZEROS TO NUM-3D.
           MOVE N5 TO NUM-3D.
           IF N5 = 0
              GO MOVE-ASTER.
           PERFORM ESCR-3DIG THRU FIM-3DIG.
           IF N5 = 1
              MOVE 'CEN-TA-VO       ' TO NUM-EXT
           ELSE MOVE 'CEN-TA-VOS      ' TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.


      *    --- na compara‡„o abaixo   IF K < 65
      *        substituir o valor 65 pelo n£mero de caracteres
      *        existentes na linha detalhe.


       MOVE-ASTER.
           MOVE '*' TO TAB-S (K).
           ADD 1 TO K.
           IF K < 66
              GO TO MOVE-ASTER.
           PERFORM AJUSTE-DIR THRU FIM-AJUSTE.
           MOVE TABELA-S TO EXTENSO.
           WRITE REG-REL FROM LINHA-EXT.
           IF CONTDUP NOT = 1
              MOVE ALL '*' TO EXTENSO
              WRITE REG-REL FROM LINHA-EXT AFTER 2.

           GO TO TERMINO.

      *    ---  comando:
      *         GO TO  nome-par grafo.
      *    ---  colocar o nome do par grafo para onde deve prosseguir
      *         o programa ap•s a impress„o do n£mero em extenso.


      *    --- os par grafos abaixo s„o executados
      *        pelo comando PERFORM e devem ser colocados
      *        no fim do programa.

       ESCR-3DIG.
           IF D1 = 0 GO TO ESCR-2DIG.
           IF NUM-3D = 100
              MOVE 'CEM             ' TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT
              GO TO FIM-3DIG.
           IF D1 = 1
              MOVE 'CEN-TO          ' TO NUM-EXT
           ELSE MOVE CENTENA (D1) TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.
           IF NUM-2D = 0 GO FIM-3DIG.
           MOVE 'E               ' TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.

       ESCR-2DIG.
           IF D2 = 0 OR D2 = 1
              MOVE UNID-DEZ (NUM-2D) TO NUM-EXT
              PERFORM ESCR-EXT THRU FIM-EXT
              GO FIM-3DIG.
           MOVE DEZENA (D2) TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.
           IF D3 = 0 GO FIM-3DIG.
           MOVE 'E               ' TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.
           MOVE UNID-DEZ (D3) TO NUM-EXT.
           PERFORM ESCR-EXT THRU FIM-EXT.
       FIM-3DIG. EXIT.

       ESCR-EXT.
           MOVE 1 TO CONT.
           ADD 1 TO I.
           MOVE I TO J.


      *    --- na compara‡„o abaixo  IF K + CONT < 65
      *        substituir o valor 65 pelo n£mero de caracteres
      *        definidos na tabela TABELA-S + 1.

       CONTA-CARAC.
           IF EXT (J) NOT = '-' AND NOT = ' '
              ADD 1 TO CONT, J
              GO CONTA-CARAC.
           IF K + CONT < 66
              GO MOVE-SILABA.
           COMPUTE S = I - 1.
           IF S < 1 MOVE 1 TO S.
           IF EXT (S) = '-'
              MOVE '-' TO TAB-S (K).
           PERFORM AJUSTE-DIR THRU FIM-AJUSTE.

           MOVE TABELA-S TO EXTENSO.
           WRITE REG-REL FROM LINHA-EXT.
           MOVE SPACES TO TABELA-S  REG-REL.
           WRITE REG-REL.
           MOVE 1 TO K  CONTDUP.

       MOVE-SILABA.
           MOVE EXT (I) TO TAB-S (K).
           ADD 1 TO K, I.
           IF EXT (I) NOT = '-' AND NOT = ' '
              GO MOVE-SILABA.
           IF EXT (I) NOT = ' '
              GO ESCR-EXT.
           MOVE ZEROS TO I.
           ADD 1 TO K.
       FIM-EXT. EXIT.

       AJUSTE-DIR.
           MOVE 65 TO J, M.
           MOVE 1     TO CONT.

       CONTA-BRANCOS.
           IF TAB-S (J) = ' '
              ADD 1 TO CONT
              SUBTRACT 1 FROM J
              GO CONTA-BRANCOS
           ELSE SUBTRACT 1 FROM CONT.

       AJUSTA.
           SUBTRACT CONT FROM M GIVING P.
           IF TAB-S (P) NOT = ' '
              MOVE TAB-S (P) TO TAB-S (M)
              SUBTRACT 1 FROM M
              GO AJUSTA.
           MOVE TAB-S (P) TO TAB-S (M).
           IF CONT NOT = 0
              SUBTRACT 1 FROM CONT
              SUBTRACT 1 FROM M
              SUBTRACT CONT FROM M GIVING P
              MOVE TAB-S (P) TO TAB-S (M)
              SUBTRACT 1 FROM M
              GO TO AJUSTA.
       FIM-AJUSTE. EXIT.

       TERMINO.
           MOVE DIA-E TO DIA-REL.
           MOVE ANO-E1 TO ANO-REL.
           WRITE REG-REL FROM LINHA-2 AFTER 2.
           WRITE REG-REL FROM LINHA-3 AFTER 2.
           MOVE SPACES TO REG-REL.
           WRITE REG-REL AFTER 9.

       FINALIZAR-PROGRAMA SECTION.
           EXIT PROGRAM.

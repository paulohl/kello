       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRISAM.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  05/06/88.
       SECURITY.      *************************************************
                      *                                               *
                      *   Tratamento de file status                   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT USELOG ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-USELOG.

       DATA DIVISION.
       FILE SECTION.

       FD  USELOG
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-USELOG.

       01  USELOG-REG.
           05 USELOG-DATA-DE-HOJE       PIC  X(009).
           05 USELOG-HORA               PIC  X(009).
           05 USELOG-PROGRAMA           PIC  X(008)B.
           05 USELOG-PARTE.
              10 USELOG-FUNCAO-PROGRAMA PIC  X(035).
              10 USELOG-OPERADOR        PIC  X(030).
           05 FILLER REDEFINES USELOG-PARTE.
              10 USELOG-FS              PIC  X(003).
              10 USELOG-MENSAGEM        PIC  X(046).
           05 USELOG-TASK               PIC  9(006).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 TEMPO-3.
              10 HH-3                  PIC  9(002).
              10 MM-3                  PIC  9(002).
              10 SS-3                  PIC  9(002).
              10 CC-3                  PIC  9(002).
           05 SEGUNDOS-1               PIC  9(008) VALUE 0.
           05 SEGUNDOS-2               PIC  9(008) VALUE 0.
           05 SEGUNDOS                 PIC  9(002) VALUE 0.
           05 CURSOR-POSITION.
              10                       PIC  9(004) COMP-X VALUE 00.
              10                       PIC  9(004) COMP-X VALUE 00.
           05 GRMENU                   PIC  X(001) VALUE SPACE.
           05 OPERADOR                 PIC  X(030) VALUE SPACES.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 HOJE.
              10 AA-H-1                PIC  9(002) VALUE ZEROS.
              10 MM-H-1                PIC  9(002) VALUE ZEROS.
              10 DD-H-1                PIC  9(002) VALUE ZEROS.
           05 DATA-DE-HOJE.
              10 DD-H-2                PIC  9(002) VALUE ZEROS.
              10 FILLER                PIC  X(001) VALUE "/".
              10 MM-H-2                PIC  9(002) VALUE ZEROS.
              10 FILLER                PIC  X(001) VALUE "/".
              10 AA-H-2                PIC  9(002) VALUE ZEROS.
           05 TEMPO.
              10 HH-1                  PIC  9(002) VALUE ZEROS.
              10 MM-1                  PIC  9(002) VALUE ZEROS.
              10 SS-1                  PIC  9(002) VALUE ZEROS.
              10 DD-1                  PIC  9(002) VALUE ZEROS.
           05 HORA.
              10 HH-2                  PIC  9(002) VALUE ZEROS.
              10 FILLER                PIC  X(001) VALUE ":".
              10 MM-2                  PIC  9(002) VALUE ZEROS.
              10 FILLER                PIC  X(001) VALUE ":".
              10 SS-2                  PIC  9(002) VALUE ZEROS.
           05 I                        PIC  9(002) VALUE ZERO.
           05 Y                        PIC  9(002) VALUE ZERO.
           05 CX                       PIC  9(002) VALUE ZERO.
           05 NADA                     PIC  X(001) VALUE SPACE.
           05 IGNORA                   PIC  X(065) VALUE "Ignora...".
           05 RESTAURA                 PIC  X(033) VALUE
              "Aguarde a restaura‡…o do arquivo ".
           05 ER-USELOG.
              10 FS-USELOG             PIC  X(002) VALUE "00".
              10 LB-USELOG.
                 15 FILLER             PIC  X(002) VALUE "GR".
                 15 ANO-USELOG         PIC  9(002) VALUE ZERO.
                 15 MES-USELOG         PIC  9(002) VALUE ZERO.
                 15 FILLER             PIC  X(004) VALUE ".LOG".
                 15 FILLER             PIC  X(040) VALUE SPACES.
           05 MENSAGEM VALUE SPACES.
              10 BYTE-M OCCURS 46      PIC  X(001).
           05 MENSAGENS-DE-ERRO.
              10 F PIC X(30) VALUE "Novo arquivo".
              10 F PIC X(30) VALUE "Erro de estrutura no".
              10 F PIC X(30) VALUE "Arquivo apagado".
              10 F PIC X(30) VALUE "Chave duplicada no".
              10 F PIC X(30) VALUE "Registro n„o encontrado no".
              10 F PIC X(30) VALUE "Disco cheio p/grava‡„o do".
              10 F PIC X(30) VALUE "Arquivo n„o encontrado".
              10 F PIC X(30) VALUE "Estrutura alterada no".
              10 F PIC X(30) VALUE "Open redundante:".
              10 F PIC X(30) VALUE "Close p/arquivo n„o aberto:".
              10 F PIC X(30) VALUE "Opara‡„o mal sucedida:".
              10 F PIC X(30) VALUE "Read ap¢s fim do".
              10 F PIC X(30) VALUE "I/O p/arquivo fechado:".
              10 F PIC X(30) VALUE "Write c/Open n„o ok:".
              10 F PIC X(30) VALUE "Delete/Rewrite s/Open I-O:".
              10 F PIC X(30) VALUE "Nome de arquivo inv lido:".
              10 F PIC X(30) VALUE "Registro bloqueado no arquivo:".
              10 F PIC X(30) VALUE "Indices corruptos no arquivo:".
              10 Q PIC X(30) VALUE "Erro de I/O RT000 no arquivo:".
              10 REDEFINES Q.
                 15 PIC X(14).
                 15 ABRE-RT-2 PIC 9(003).
                 15 PIC X(13).
              10 F PIC X(30) VALUE "Indices indispon¡veis,arquivo:".
              10 W PIC X(30) VALUE "Erro I/O RT000 sortwork:".
              10 REDEFINES W.
                 15 PIC X(11).
                 15 ABRE-RT-1 PIC 9(003).
                 15 PIC X(18).
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 21 PIC X(30).

       COPY CPREFE.
       COPY CPSEND.

       LINKAGE SECTION.

       01  ER-ARQUIVO.
           05 FS-ARQUIVO               PIC  X(002).
           05 REDEFINES FS-ARQUIVO.
              10 RT-NUMBER-1     COMP-X PIC  9(002).
              10 RT-NUMBER-2     COMP-X PIC  9(002).
           05 LB-ARQUIVO                PIC  X(050).

       PROCEDURE DIVISION USING ER-ARQUIVO.

       000-INICIO.

           DISPLAY (23, 3) SPACES WITH SIZE 70

           IF   FS-ARQUIVO = "10"
                STRING "Fim da leitura de " DELIMITED SIZE
                       LB-ARQUIVO           DELIMITED SIZE
                       INTO GRSEND-MSG
                CALL "GRSEND" USING PARAMETROS-GRSEND.

           IF   FS-ARQUIVO = "00" OR "10"
                EXIT PROGRAM.

           MOVE "Erro de I/O no " TO MENSAGEM
           MOVE 0                 TO ABRE-RT-2

           IF   FS-ARQUIVO = "05"
                MOVE MSG (1) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
                MOVE "00" TO FS-ARQUIVO
           ELSE
           IF   FS-ARQUIVO = "21"
                MOVE MSG (2) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "22"
                MOVE MSG (4) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "23"
                MOVE MSG (5) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "24"
           OR   FS-ARQUIVO = "34"
                MOVE MSG (6) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "30"
           OR   FS-ARQUIVO = "35"
                MOVE MSG (7) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "39"
                MOVE MSG (8) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "41"
                MOVE MSG (9) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "42"
                MOVE MSG (10) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "43"
                MOVE MSG (11) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "46"
                MOVE MSG (12) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "47"
                MOVE MSG (13) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "48"
                MOVE MSG (14) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "49"
                MOVE MSG (15) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "9D"
                MOVE MSG (17) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "9"
                MOVE MSG (16) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "9)"
                MOVE MSG (18) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   FS-ARQUIVO = "9+"
                MOVE MSG (20) TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
                CALL "GRFINA"
           ELSE
           IF   RT-NUMBER-1 = 57
                MOVE RT-NUMBER-2 TO ABRE-RT-2
                MOVE MSG (19)  TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
           ELSE
           IF   RT-NUMBER-1 < 48
           OR   RT-NUMBER-1 > 57
                MOVE RT-NUMBER-1 TO ABRE-RT-1
                MOVE MSG (21)  TO MENSAGEM
                PERFORM 010-MSG THRU 010-99-FIM
                CALL "GRFINA"
           ELSE
                PERFORM 010-MSG THRU 010-99-FIM.

       000-FIM. EXIT PROGRAM.

       010-MSG.

           PERFORM VARYING I FROM 46 BY -1
                   UNTIL BYTE-M (I) NOT = SPACE
                      OR I = 0
           END-PERFORM

           ADD  2           TO I
           MOVE LB-ARQUIVO  TO MENSAGEM (I: )

           PERFORM VARYING I FROM 46 BY -1
                   UNTIL BYTE-M (I) NOT = SPACE
                      OR I = 0
           END-PERFORM

           ADD 1 TO I

           IF  FS-ARQUIVO NOT = "9D"
               IF  FS-ARQUIVO NOT = "05"
                   MOVE MENSAGEM  TO GRSEND-MSG
                   CALL "GRSEND" USING PARAMETROS-GRSEND
               ELSE
                   DISPLAY (23, 3) MENSAGEM WITH SIZE I
               END-IF
               ACCEPT  HOJE    FROM DATE
               ACCEPT  TEMPO   FROM TIME
               MOVE DD-H-1       TO DD-H-2
               MOVE MM-H-1       TO MM-H-2
                                    MES-USELOG
               MOVE AA-H-1       TO AA-H-2
                                    ANO-USELOG
               MOVE HH-1         TO HH-2
               MOVE MM-1         TO MM-2
               MOVE SS-1         TO SS-2
               OPEN EXTEND USELOG
               IF   FS-USELOG = "00"
                    MOVE  SPACES        TO USELOG-REG
                    MOVE  DATA-DE-HOJE  TO USELOG-DATA-DE-HOJE
                    MOVE  HORA          TO USELOG-HORA
                    IF   RT-NUMBER-1 < 48
                    OR   RT-NUMBER-1 > 56
                         MOVE "RT" TO FS-ARQUIVO
                    END-IF
                    MOVE  FS-ARQUIVO    TO USELOG-FS
                    MOVE "GRISAM"       TO USELOG-PROGRAMA
                    MOVE  MENSAGEM      TO USELOG-MENSAGEM
                    MOVE "?"            TO GRMENU
                    CALL "GRFINA" USING OPERADOR USELOG-TASK
                                        PROGRAMA GRMENU
                    WRITE USELOG-REG
                    CLOSE USELOG
              END-IF
           ELSE
               DISPLAY (23, 3) MENSAGEM WITH SIZE I
               PERFORM 110-ESPERA-5 THRU 110-99-FIM
               DISPLAY (23, 3) SPACES WITH SIZE 70
           END-IF.

       010-99-FIM. EXIT.

       110-ESPERA-5.

           ACCEPT TEMPO FROM TIME
           MOVE   0       TO SEGUNDOS
           MOVE   TEMPO   TO TEMPO-3
           COMPUTE SEGUNDOS-1 =  HH-1 * 60
           COMPUTE SEGUNDOS-1 = (SEGUNDOS-1 + MM-1) * 60
           COMPUTE SEGUNDOS-1 =  SEGUNDOS-1 + SS-1
           PERFORM UNTIL (SEGUNDOS NOT < 5)
                      OR (TEMPO > TEMPO-3)
               CALL "GRHORA"
               ACCEPT TEMPO-3 FROM TIME
               COMPUTE SEGUNDOS-2 =  HH-3 * 60
               COMPUTE SEGUNDOS-2 = (SEGUNDOS-2 + MM-3) * 60
               COMPUTE SEGUNDOS-2 =  SEGUNDOS-2 + SS-3
               COMPUTE SEGUNDOS   =  SEGUNDOS-2 - SEGUNDOS-1
           END-PERFORM.

       110-99-FIM. EXIT.

       END PROGRAM GRISAM.

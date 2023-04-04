       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHO88.
      *AUTORA: ALFREDO SAVIOLLI NETO
      *DATA: 27-08-2007
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAPX001.

           COPY RCPX001.

           COPY RCPX002.

           COPY RCPX100.

           COPY RCPX101.

           COPY MTPX020.

       DATA DIVISION.
       FILE SECTION.

           COPY CAPW001.

           COPY RCPW001.

           COPY RCPW002.

           COPY RCPW100.

           COPY RCPW101.

           COPY MTPW020.

       WORKING-STORAGE SECTION.
           COPY "DS-CNTRL.MF".
           COPY "CBDATA.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  PASSAR-PARAMETROS.
           05  PASSAR-STRING-1       PIC X(55).
       01  VARIAVEIS.
           05  ST-RCD001             PIC XX       VALUE SPACES.
           05  ST-RCD002             PIC XX       VALUE SPACES.
           05  ST-RCD100             PIC XX       VALUE SPACES.
           05  ST-RCD101             PIC XX       VALUE SPACES.
           05  ST-MTD020             PIC XX       VALUE SPACES.
           05  ERRO-W                PIC 9        VALUE ZEROS.
      *  ERRO-W - flag que controla se houve erro abertura nos arquivos
           05  HORA-W                PIC 9(8)     VALUE ZEROS.
           05  PAG-W                 PIC 9(2)     VALUE ZEROS.
           05  EMP-REFERENCIA.
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(7).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(12).

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  I                     PIC 9        VALUE ZEROS.
           05  LETRA                 PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

           COPY "PARAMETR".

           COPY "LDIFDIAS".


       PROCEDURE DIVISION.

       MAIN-PROCESS SECTION.
           OPEN INPUT CONTROLE
           READ CONTROLE
           MOVE EMPRESA            TO EMP-REC

           MOVE "RCD001"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD001
           MOVE "RCD002"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD002
           MOVE "RCD100"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD100
           MOVE "RCD101"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-RCD101
           MOVE "MTD020"  TO ARQ-REC.  MOVE EMPRESA-REF TO PATH-MTD020


           MOVE ZEROS     TO ERRO-W.
           OPEN I-O RCD001 RCD002 RCD100 RCD101 MTD020

           CLOSE CONTROLE.

           DISPLAY "VOU COMECAR A CORRECAO DO RCD101" STOP " ".

           INITIALIZE REG-RCD100
           MOVE 20070726    TO DATA-MOVTO-REC

           START RCD100 KEY IS NOT LESS ALT-REC INVALID KEY
               MOVE "10" TO ST-RCD100.

           PERFORM UNTIL ST-RCD100 = "10"
               READ RCD100 NEXT AT END
                   MOVE "10" TO ST-RCD100
               NOT AT END
                   INITIALIZE REG-RCD101
                   MOVE ALBUM-REC            TO ALBUM-REC1
                   START RCD101 KEY IS NOT LESS CHAVE-REC1 INVALID KEY
                       MOVE "10" TO ST-RCD101
                   END-START
                   PERFORM UNTIL ST-RCD101 = "10"
                       READ RCD101 NEXT AT END
                           MOVE "10" TO ST-RCD101
                       NOT AT END
                           IF ALBUM-REC <> ALBUM-REC1
                              MOVE "10" TO ST-RCD101
                           ELSE
                             IF TIPO-REC1 = 3
                                move zeros to dta-baixa-rec1
                                display "RCD101 =>  " reg-rcd101
                                perform ver-qtde-dias
                                display "COMIS-PARC-REC1 = "
                                COMIS-PARC-REC1
                                rewrite reg-rcd101 invalid key
                                   display "Erro de Regravacao...RCD101"
                                      stop " "
                                end-rewrite
                             END-IF
                           END-IF
                       END-READ
                   END-PERFORM
               END-READ
           END-PERFORM

           CLOSE RCD100 RCD101

           DISPLAY "ACABEI" STOP "  "
           STOP RUN.

       VER-QTDE-DIAS SECTION.
           MOVE ALBUM-REC        TO ALBUM-MTG.
           READ MTD020 INVALID KEY
               INITIALIZE REG-MTD020
           END-READ

           MOVE VISITA-MTG            TO CODIGO-COMIS-RC01
           READ RCD001 INVALID KEY
               INITIALIZE REG-RCD001
           END-READ
           MOVE COMIS-ANTECIPADA-RC01 TO COMIS-PARC-REC1


           STRING DATAVEN-REC(7:2) DATAVEN-REC(5:2) DATAVEN-REC(1:4)
             INTO LINK-DATABA

           STRING VENCTO-REC1(7:2) VENCTO-REC1(5:2) VENCTO-REC1(1:4)
             INTO LINK-DATAPE

           CALL "DIFDIAS" USING LINKA-DIFDIAS
           CANCEL "DIFDIAS"

           MOVE VISITA-MTG               TO CODIGO-COMIS-RC02
           MOVE "Antecipada"             TO TIPO-RC02
           READ RCD002 NOT INVALID KEY
              MOVE COMIS-0-30-DIAS       TO COMIS-PARC-REC1
              IF LINK-DIASCA > 30
                 MOVE COMIS-31-120-DIAS  TO COMIS-PARC-REC1
              END-IF
              IF LINK-DIASCA > 120
                 MOVE COMIS-121-240-DIAS TO COMIS-PARC-REC1
              END-IF
              IF LINK-DIASCA > 240
                 MOVE COMIS-241-000-DIAS TO COMIS-PARC-REC1
              END-IF
           END-READ.

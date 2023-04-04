       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GALHOMTP020.
       AUTHOR. ALFREDO SAVIOLLI.
       DATE-WRITTEN. 11-02-2011.
      *FUNÇÃO: Altera status do Album

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA
         PRINTER IS LPRINTER.

       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY MTPX020.

       DATA DIVISION.
       FILE SECTION.

           COPY MTPW020.

       WORKING-STORAGE SECTION.
       01  VARIAVEIS.
           05  ST-MTD020             PIC XX       VALUE SPACES.
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
           05  AUX-ALBUM             PIC 9(08) VALUE ZEROS.
           05  RESPOSTA              PIC X(01) VALUE SPACES.


       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           DISPLAY "Informar o Codigo da Empresa: " AT 0101
           ACCEPT EMP-REC                           AT 0131

           MOVE "MTD020"                            TO ARQ-REC
           MOVE EMPRESA-REF                         TO PATH-MTD020

           OPEN I-O   MTD020

           IF ST-MTD020 = "35"
              CLOSE MTD020             OPEN OUTPUT MTD020
              CLOSE MTD020             OPEN I-O    MTD020
           END-IF

           IF ST-MTD020 <> "00"
              DISPLAY "ERRO DE ABERTURA MTD020"     AT 1001
              DISPLAY "STATUS => "                  AT 1101
              DISPLAY ST-MTD020                     AT 1111
              STOP " "
              CLOSE MTD020
              STOP RUN
           ELSE
              PERFORM NOVO-ALBUM.


       NOVO-ALBUM SECTION.
           MOVE ZEROS                              TO AUX-ALBUM
           DISPLAY "Informar o Album + Contrato: " AT 1001
           ACCEPT AUX-ALBUM                        AT 1030

           MOVE AUX-ALBUM                          TO ALBUM-MTG
           READ MTD020 INVALID KEY
                DISPLAY "Album Nao Encontrado"     AT 1501
                STOP " "
           NOT INVALID KEY
                MOVE SPACES TO RESPOSTA
                DISPLAY "Vou Liberar o Album, Confirma? " AT 1101
                ACCEPT RESPOSTA                           AT 1132
                IF RESPOSTA = "S" OR "s"
                   MOVE 0 TO FOGO-MTG
                   REWRITE REG-MTD020 INVALID KEY
                        DISPLAY "Erro de Regravacao" AT 1501
                        STOP " "
                        CLOSE MTD020
                        STOP RUN
                   NOT INVALID KEY
                        MOVE SPACES TO RESPOSTA
                        DISPLAY "Deseja Alterar um Novo Album? "
                             AT 1201
                        ACCEPT RESPOSTA                   AT 1231
                        IF RESPOSTA = "S"
                           DISPLAY ERASE                  AT 0101
                           GO TO NOVO-ALBUM
                        ELSE
                           CLOSE MTD020
                           STOP RUN.

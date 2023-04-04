       COPY DSLANG.CPY.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COP041T.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       class-control.
           Window             is class "wclass".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY COPX040.
           COPY COPX041.
           COPY IEPX011.

       DATA DIVISION.
       FILE SECTION.
           COPY COPW040.
           COPY COPW041.
           COPY IEPW011.

       WORKING-STORAGE SECTION.
           COPY "COP041T.CPB".
           COPY "COP041T.CPY".
           COPY "DS-CNTRL.MF".
           COPY "DSSYSINF.CPY".
       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-COD040              PIC XX       VALUE SPACES.
           05  ST-COD041              PIC XX       VALUE SPACES.
           05  ST-IED011              PIC XX       VALUE SPACES.
           05  LIN-DETALHE-W          PIC X(30)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER             PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1               PIC X VALUE "\".
               10  EMP-REC            PIC XXX.
               10  VAR2               PIC X VALUE "\".
               10  ARQ-REC            PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  CODIGO-W               PIC X(8)   VALUE SPACES.
           05  CLASSIF-W              PIC X      VALUE SPACES.
           05  MESANO-E               PIC 99/9999 VALUE ZEROS.
           05  INICIAL-PROCURADA      PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR     PIC X(6)     VALUE SPACES.
           05  I                      PIC 9        VALUE ZEROS.
           05  LETRA                  PIC X        VALUE SPACES.
           05  SAIR-W                 PIC 9        VALUE ZEROS.
           05  MENSAGEM               PIC X(200).
           05  TIPO-MSG               PIC X(01).
           05  RESP-MSG               PIC X(01).
           05  AUX-CURSO              PIC 9(03)    VALUE ZEROS.
           05  AUX-TURMA              PIC X(02)    VALUE SPACES.
           05  AUX-CONTRATO           PIC 9(04)    VALUE ZEROS.
      *    variáveis p/ listar os nomes com iniciais solicitadas

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.


       LINKAGE SECTION.
           COPY "PARAMETR".

       01  STRING-1               PIC X(120) VALUE SPACES.

       PROCEDURE DIVISION USING PARAMETROS-W STRING-1.

       MAIN-PROCESS SECTION.
           MOVE EMPRESA-W          TO EMP-REC
           PERFORM INICIALIZA-PROGRAMA.
           PERFORM CORPO-PROGRAMA UNTIL GS-EXIT-FLG-TRUE.
           GO FINALIZAR-PROGRAMA.

       INICIALIZA-PROGRAMA SECTION.
      *    MOVE "0000" TO STRING-1(52: 4).
           INITIALIZE GS-DATA-BLOCK
           INITIALIZE DS-CONTROL-BLOCK
           MOVE GS-DATA-BLOCK-VERSION-NO
                                   TO DS-DATA-BLOCK-VERSION-NO
           MOVE GS-VERSION-NO      TO DS-VERSION-NO
           MOVE "COD040"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-COD040.
           MOVE "COD041"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-COD041.
           MOVE "IED011"           TO ARQ-REC.
           MOVE EMPRESA-REF        TO PATH-IED011.
           OPEN I-O   COD040 COD041 IED011
           CLOSE      COD040 COD041 IED011
           OPEN INPUT COD040 COD041 IED011
           IF ST-COD040 <> "00" OR ST-COD041 <> "00" OR
              ST-IED011 <> "00"
              STRING "ERRO ABERTURA COD040: " ST-COD040 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-MENSAGEM
           ELSE
              PERFORM LOAD-SCREENSET.

       CORPO-PROGRAMA SECTION.
           EVALUATE TRUE
               WHEN GS-CENTRALIZA-TRUE
                    PERFORM CENTRALIZAR
                    MOVE FUNCTION NUMVAL(STRING-1) TO AUX-CONTRATO
                    IF AUX-CONTRATO > ZEROS
                       PERFORM CARREGAR-TURMAS
                    END-IF
               WHEN GS-CARREGA-LIST-BOX-TRUE
                    PERFORM CARREGA-ULTIMOS
               WHEN GS-ITEM-SELECIONADO-TRUE
                    PERFORM ITEM-SELECIONADO
           END-EVALUATE.
           PERFORM CLEAR-FLAGS.
           PERFORM CALL-DIALOG-SYSTEM.

       CARREGAR-TURMAS SECTION.
           INITIALIZE REG-COD040

           MOVE ZEROS        TO GS-CONT
           MOVE AUX-CONTRATO TO NR-CONTRATO-CO40
           START COD040 KEY IS NOT < NR-CONTRATO-CO40 INVALID KEY
                 MOVE "10"   TO ST-COD040.

           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-COD040 = "10"
              READ COD040 NEXT RECORD AT END
                   MOVE "10" TO ST-COD040
              NOT AT END
                   IF NR-CONTRATO-CO40 <> AUX-CONTRATO
                      MOVE "10" TO ST-COD040
                   ELSE
                      MOVE NR-CONTRATO-CO40    TO GS-LINDET(1: 4)

                      INITIALIZE REG-COD041
                                 AUX-CURSO
                                 AUX-TURMA
                      MOVE NR-CONTRATO-CO40  TO NR-CONTRATO-CO41
                      MOVE ZEROS             TO CURSO-CO41
                      MOVE SPACES            TO TURMA-CO41
                      START COD041 KEY IS NOT < CHAVE-CO41 INVALID KEY
                            MOVE "10" TO ST-COD041
                      END-START
                      PERFORM UNTIL ST-COD041 = "10"
                            READ COD041 NEXT RECORD AT END
                                 MOVE "10" TO ST-COD041
                            NOT AT END
                                 IF NR-CONTRATO-CO41 <>
                                    NR-CONTRATO-CO40
                                    MOVE "10" TO ST-COD041
                                 ELSE
                                    IF AUX-CURSO <> CURSO-CO41 OR
                                       AUX-TURMA <> TURMA-CO41

                                       MOVE CURSO-CO41 TO AUX-CURSO
                                       MOVE TURMA-CO41 TO AUX-TURMA

                                       MOVE CURSO-CO41   TO CODIGO-IE11
                                       READ IED011 INVALID KEY
                                            MOVE SPACES  TO NOME-IE11
                                       END-READ
                                       MOVE CURSO-CO41   TO
                                            GS-LINDET(6:3)
                                       MOVE NOME-IE11
                                         TO GS-LINDET(10:25)
                                       MOVE TURMA-CO41
                                         TO GS-LINDET(37:6)
                                       MOVE TURNO-CO41
                                         TO GS-LINDET(44:10)

                                       ADD 1   TO GS-CONT
                                       MOVE "INSERE-LIST"
                                                         TO DS-PROCEDURE
                                       PERFORM CALL-DIALOG-SYSTEM
                                    END-IF
                                 END-IF
                            END-READ
                      END-PERFORM
                  END-IF
              END-READ
           END-PERFORM.

       CENTRALIZAR SECTION.
          move-object-handle principal handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       CARREGA-MENSAGEM-ERRO SECTION.
           MOVE "EXIBE-ERRO" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       CARREGA-ULTIMOS SECTION.
           INITIALIZE REG-COD040
           MOVE ZEROS TO GS-CONT.
           MOVE AUX-CONTRATO TO NR-CONTRATO-CO40
           START COD040 KEY IS NOT < NR-CONTRATO-CO40 INVALID KEY
                 MOVE "10" TO ST-COD040.
           MOVE SPACES TO GS-LINDET.
           PERFORM UNTIL ST-COD040 = "10"
              READ COD040 NEXT RECORD AT END
                   MOVE "10" TO ST-COD040
              NOT AT END
                   IF AUX-CONTRATO <> NR-CONTRATO-CO40
                      MOVE "10" TO ST-COD040
                   ELSE
                      MOVE NR-CONTRATO-CO40    TO GS-LINDET(1: 4)

                      INITIALIZE REG-COD041
                                 AUX-CURSO
                                 AUX-TURMA
                      MOVE NR-CONTRATO-CO40  TO NR-CONTRATO-CO41
                      MOVE ZEROS             TO CURSO-CO41
                      MOVE SPACES            TO TURMA-CO41
                      START COD041 KEY IS NOT < CHAVE-CO41 INVALID KEY
                            MOVE "10" TO ST-COD041
                      END-START
                      PERFORM UNTIL ST-COD041 = "10"
                            READ COD041 NEXT RECORD AT END
                                 MOVE "10" TO ST-COD041
                            NOT AT END
                                 IF NR-CONTRATO-CO41 <>
                                    NR-CONTRATO-CO40
                                    MOVE "10" TO ST-COD041
                                 ELSE
                                    IF AUX-CURSO <> CURSO-CO41 OR
                                       AUX-TURMA <> TURMA-CO41

                                       MOVE CURSO-CO41 TO AUX-CURSO
                                       MOVE TURMA-CO41 TO AUX-TURMA

                                       MOVE CURSO-CO41   TO CODIGO-IE11
                                       READ IED011 INVALID KEY
                                            MOVE SPACES  TO NOME-IE11
                                       END-READ
                                       MOVE CURSO-CO41   TO
                                            GS-LINDET(6:3)
                                       MOVE NOME-IE11
                                         TO GS-LINDET(10:25)
                                       MOVE TURMA-CO41
                                         TO GS-LINDET(37:6)
                                       MOVE TURNO-CO41
                                         TO GS-LINDET(44:10)

                                       ADD 1   TO GS-CONT
                                       MOVE "INSERE-LIST"
                                                         TO DS-PROCEDURE
                                       PERFORM CALL-DIALOG-SYSTEM
                                    END-IF
                                 END-IF
                            END-READ
                      END-PERFORM
                  END-IF
              END-READ
           END-PERFORM.
       INICIAL-A-PROCURAR SECTION.
      *Rotina p/ identificar a inicial do nome solicitada a procurar
           MOVE ZEROS  TO SAIR-W.
           MOVE SPACES TO INICIAL-PROCURADA INICIAL-A-COMPARAR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6 OR SAIR-W = 1
               MOVE GS-LINDET(I: 1) TO LETRA
               IF LETRA = SPACES
                  MOVE 1 TO SAIR-W
                  SUBTRACT 1 FROM I
               ELSE
                  MOVE GS-LINDET(I: 1) TO INICIAL-PROCURADA(I: 1)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM I.

       ITEM-SELECIONADO SECTION.
           MOVE GS-LINDET(1:120) TO STRING-1.
           MOVE STRING-1(52:04)  TO CODIGO-W.

           IF CODIGO-W = SPACES
              MOVE ZEROS         TO STRING-1(52:04).

       CLEAR-FLAGS SECTION.
           INITIALIZE GS-FLAG-GROUP.

       SET-UP-FOR-REFRESH-SCREEN SECTION.
           MOVE "REFRESH-DATA" TO DS-PROCEDURE.

       LOAD-SCREENSET SECTION.
           MOVE DS-PUSH-SET TO DS-CONTROL
           MOVE "COP041T" TO DS-SET-NAME
           PERFORM CALL-DIALOG-SYSTEM.

       EXIBIR-MENSAGEM SECTION.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".

       CALL-DIALOG-SYSTEM SECTION.
           CALL "DSRUN" USING DS-CONTROL-BLOCK, GS-DATA-BLOCK.
           IF NOT DS-NO-ERROR
              MOVE DS-ERROR-CODE TO DISPLAY-ERROR-NO
              DISPLAY "DS ERROR NO:  " DISPLAY-ERROR-NO
              GO FINALIZAR-PROGRAMA
           END-IF.
       FINALIZAR-PROGRAMA SECTION.
           CLOSE COD040 COD041 IED011.
           move ds-quit-set to ds-control.
           PERFORM CALL-DIALOG-SYSTEM.
           EXIT PROGRAM.

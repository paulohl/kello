      $SET DEFFILE
      $SET DEFFILETYPE"WIN"

       0010-INICIO SECTION.
      *------------
           PERFORM 0020-INICIALIZAR
           IF (LK-I-STATUS <> "00")
              DISPLAY "ERRO DE INICIALIZACAO: " LK-I-STATUS  STOP " "
           ELSE
              PERFORM 9999-ADICIONAR-DADOS
              IF (LK-A-STATUS <> "00")
                 IF (LK-A-STATUS = "05")
                    NEXT SENTENCE
                 ELSE
                    DISPLAY "ERRO DE ADICAO DE DADOS: " LK-A-STATUS
                    STOP " "
                 END-IF
              ELSE
                 PERFORM 9999-VISUALIZAR-RELATORIO
              END-IF
           END-IF

           CALL "CBL_DELETE_FILE" USING WS-LABEL-DEF
           CALL "CBL_DELETE_FILE" USING WS-LABEL-VAR
           CALL "CBL_DELETE_FILE" USING WS-LABEL-GRF.

           MOVE "FOCAR-CHAVE" TO DS-PROCEDURE
           PERFORM CALL-DIALOG-SYSTEM.
       0010-INICIO-FIM.
           EXIT.

       0020-INICIALIZAR SECTION.
      *-----------------
           PERFORM 9999-VERSAO-DOS-ARQUIVOS
           OPEN OUTPUT ATDEF
           PERFORM 0030-CRIAR-ARQ-DEFINICAO
           CLOSE ATDEF

           SET BIBLIOTECA TO ENTRY "CobRel.DLL"
           MOVE WS-LABEL-DEF           TO LK-I-ARQUIVO
           IF COBREL-BARRA-STATUS = "S"
              IF CONTA-PAGINA = 0
                 MOVE 1 TO LK-I-QTDE
              ELSE
                 MOVE CONTA-PAGINA TO LK-I-QTDE
             END-IF
           ELSE
              MOVE 0                      TO LK-I-QTDE
           END-IF
           CALL DSDLL "Init" USING BY REFERENCE LK-INIT
           CANCEL "Init".
       0020-INICIALIZAR-FIM.
           EXIT.

       CABECALHO-PRINCIPAL SECTION.
           IF COBREL-ORIENTACAO-PAGINA = "R" OR "r"
              MOVE 1     TO COBREL-VAR-NEGRITO
              MOVE 90    TO COBREL-QTD-LINHAS
      *       MOVE 50    TO COBREL-QTD-LINHAS
              MOVE 15,0  TO COBREL-SHAPE-ALTURA
              MOVE 190,0 TO COBREL-SHAPE-LARGURA
              PERFORM 0050-SHAPE-0001
              MOVE 75,0 TO COBREL-SHAPE-LARGURA
      *       PERFORM 0050-SHAPE-0001
              MOVE 190,0 TO COBREL-SHAPE-LARGURA
              PERFORM 0130-IM-FIXA-0001
              MOVE 190 TO COBREL-TAMANHO-VARIAVEL
              PERFORM 0120-TEXTO-VAR-0001
              MOVE 4,5  TO WS-TOPO
              PERFORM 0120-TEXTO-VAR-0001
              MOVE 7,5  TO WS-TOPO
              PERFORM 0120-TEXTO-VAR-0001
           ELSE
              MOVE 1     TO COBREL-VAR-NEGRITO
      *       MOVE 30    TO COBREL-QTD-LINHAS
              MOVE 60    TO COBREL-QTD-LINHAS
              MOVE 15,0  TO COBREL-SHAPE-ALTURA
              MOVE 280,0 TO COBREL-SHAPE-LARGURA
              PERFORM 0050-SHAPE-0001
              MOVE 50,0  TO COBREL-SHAPE-LARGURA
      *       PERFORM 0050-SHAPE-0001
              MOVE 280,0 TO COBREL-SHAPE-LARGURA
              PERFORM 0130-IM-FIXA-0001
              MOVE 350 TO COBREL-TAMANHO-VARIAVEL
              PERFORM 0120-TEXTO-VAR-0001
              MOVE 4,5  TO WS-TOPO
              PERFORM 0120-TEXTO-VAR-0001
              MOVE 7,5  TO WS-TOPO
              PERFORM 0120-TEXTO-VAR-0001
          END-IF.
       CABECALHO-PRINCIPAL-FIM.
           EXIT.

       DEFINICAO SECTION.
      *--------------------------------- CABECALHO PADRAO 1
          INITIALIZE WS-TOPO

          PERFORM CABECALHO-PRINCIPAL
      *--------------------------------------------------------- FIM
      *--------------------------------- CABECALHO 2
          IF COBREL-QTD-LINHA-CAB2 > 0
             IF COBREL-QTD-LINHA-CAB2 > 6
                 COMPUTE COBREL-SHAPE-ALTURA =
                         COBREL-QTD-LINHA-CAB2 * 3,1 + 15
             ELSE
                 COMPUTE COBREL-SHAPE-ALTURA =
                         COBREL-QTD-LINHA-CAB2 * 3,5 + 15
             END-IF
             PERFORM 0060-SHAPE-0002
             INITIALIZE CONTA-LINHA WS-TOPO
             MOVE 13,0 TO WS-TOPO
             IF COBREL-QTD-LINHA-CAB2 > 6
                PERFORM UNTIL CONTA-LINHA = COBREL-QTD-LINHA-CAB2
                   PERFORM 0120-TEXTO-VAR-0001
                   ADD 2,2 TO WS-TOPO
                   ADD 1 TO CONTA-LINHA
                END-PERFORM
             ELSE
                PERFORM UNTIL CONTA-LINHA = COBREL-QTD-LINHA-CAB2
                   PERFORM 0120-TEXTO-VAR-0001
                   ADD 2,5 TO WS-TOPO
                   ADD 1 TO CONTA-LINHA
                END-PERFORM
             END-IF
             MOVE 0  TO COBREL-VAR-NEGRITO
             INITIALIZE CONTA-LINHA
             IF COBREL-QTD-LINHA-CAB3 = 0
                ADD 2 TO WS-TOPO
                COMPUTE COBREL-QTD-LINHAS = COBREL-QTD-LINHAS -
                                            COBREL-QTD-LINHA-CAB2
                PERFORM UNTIL CONTA-LINHA = COBREL-QTD-LINHAS
                   PERFORM 0120-TEXTO-VAR-0001
                   ADD 3,0 TO WS-TOPO
                   ADD 1   TO CONTA-LINHA
                END-PERFORM
             END-IF
          END-IF
      *--------------------------------------------------------- FIM
      *--------------------------------- CABECALHO 3
          IF COBREL-QTD-LINHA-CAB3 > 0
             IF COBREL-QTD-LINHA-CAB3 > 6
                 COMPUTE COBREL-SHAPE-ALTURA = COBREL-SHAPE-ALTURA +
                         (COBREL-QTD-LINHA-CAB3 * 3,1)
             ELSE
                 COMPUTE COBREL-SHAPE-ALTURA = COBREL-SHAPE-ALTURA +
                        (COBREL-QTD-LINHA-CAB3  * 4,0)
             END-IF
             PERFORM 0070-SHAPE-0003
             INITIALIZE CONTA-LINHA
             MOVE 1   TO COBREL-VAR-NEGRITO
             IF COBREL-QTD-LINHA-CAB2 < 6
                ADD 1,5 TO WS-TOPO
             ELSE
                ADD 2,0  TO WS-TOPO
             END-IF
             IF COBREL-QTD-LINHA-CAB3 > 6
                PERFORM UNTIL CONTA-LINHA = COBREL-QTD-LINHA-CAB3
                   PERFORM 0120-TEXTO-VAR-0001
                   ADD 2,2 TO WS-TOPO
                   ADD 1 TO CONTA-LINHA
                END-PERFORM
             ELSE
                PERFORM UNTIL CONTA-LINHA = COBREL-QTD-LINHA-CAB3
                   PERFORM 0120-TEXTO-VAR-0001
                   ADD 2,5 TO WS-TOPO
                   ADD 1 TO CONTA-LINHA
                END-PERFORM
             END-IF
             MOVE 0   TO COBREL-VAR-NEGRITO
             INITIALIZE CONTA-LINHA
             IF COBREL-QTD-LINHA-CAB4 = 0
                COMPUTE COBREL-QTD-LINHA-CAB3 = COBREL-QTD-LINHA-CAB2 +
                        COBREL-QTD-LINHA-CAB3 + 2
                COMPUTE COBREL-QTD-LINHAS = COBREL-QTD-LINHAS -
                                            COBREL-QTD-LINHA-CAB3
                ADD 1 TO WS-TOPO
      *********************************

                PERFORM UNTIL CONTA-LINHA = COBREL-QTD-LINHAS
                   PERFORM  0120-TEXTO-VAR-0001
                   ADD 3,0 TO WS-TOPO
                   ADD 1   TO CONTA-LINHA
                END-PERFORM
             END-IF
          END-IF
      *--------------------------------------------------------- FIM
      *--------------------------------- CABECALHO 4
          IF COBREL-QTD-LINHA-CAB4 > 0
             IF COBREL-QTD-LINHA-CAB4 > 6
                 COMPUTE COBREL-SHAPE-ALTURA = COBREL-SHAPE-ALTURA +
                         (COBREL-QTD-LINHA-CAB4 * 3,7)
             ELSE
                 COMPUTE COBREL-SHAPE-ALTURA = COBREL-SHAPE-ALTURA +
                        (COBREL-QTD-LINHA-CAB4  * 3,5)
             END-IF
             PERFORM 0080-SHAPE-0004
             INITIALIZE CONTA-LINHA
             MOVE 1 TO COBREL-VAR-NEGRITO
             IF COBREL-QTD-LINHA-CAB3 < 6
                ADD 0,1 TO WS-TOPO
             ELSE
                ADD 1,6  TO WS-TOPO
             END-IF
             IF COBREL-QTD-LINHA-CAB4 > 6
                PERFORM UNTIL CONTA-LINHA = COBREL-QTD-LINHA-CAB4
                   PERFORM 0120-TEXTO-VAR-0001
                   ADD 3,5 TO WS-TOPO
                   ADD 1 TO CONTA-LINHA
                END-PERFORM
             ELSE
                PERFORM UNTIL CONTA-LINHA = COBREL-QTD-LINHA-CAB4
                   PERFORM 0120-TEXTO-VAR-0001
                   ADD 3,5 TO WS-TOPO
                   ADD 1 TO CONTA-LINHA
                END-PERFORM
             END-IF
             MOVE 0   TO COBREL-VAR-NEGRITO
             INITIALIZE CONTA-LINHA
             IF COBREL-QTD-LINHA-CAB5 = 0
                COMPUTE COBREL-QTD-LINHA-CAB4 = COBREL-QTD-LINHA-CAB2 +
                        COBREL-QTD-LINHA-CAB3 + COBREL-QTD-LINHA-CAB4 +
                        2

                COMPUTE COBREL-QTD-LINHAS = COBREL-QTD-LINHAS -
                                            COBREL-QTD-LINHA-CAB4
                ADD 1 TO WS-TOPO
                PERFORM UNTIL CONTA-LINHA = COBREL-QTD-LINHAS
                  PERFORM  0120-TEXTO-VAR-0001
                  ADD 3,5 TO WS-TOPO
                  ADD 1   TO CONTA-LINHA
                END-PERFORM
             END-IF
          END-IF
      *--------------------------------------------------------- FIM
      *--------------------------------- CABECALHO 5
          IF COBREL-QTD-LINHA-CAB5 > 0
             IF COBREL-QTD-LINHA-CAB5 > 6
                 COMPUTE COBREL-SHAPE-ALTURA = COBREL-SHAPE-ALTURA +
                         (COBREL-QTD-LINHA-CAB5 * 3,7)
             ELSE
                 COMPUTE COBREL-SHAPE-ALTURA = COBREL-SHAPE-ALTURA +
                        (COBREL-QTD-LINHA-CAB5  * 3,5)
             END-IF
             PERFORM 0090-SHAPE-0005
             INITIALIZE CONTA-LINHA
             MOVE 1 TO COBREL-VAR-NEGRITO
             IF COBREL-QTD-LINHA-CAB4 < 6
                ADD 0,0 TO WS-TOPO
             ELSE
                ADD 1,7  TO WS-TOPO
             END-IF
             IF COBREL-QTD-LINHA-CAB5 > 6
                PERFORM UNTIL CONTA-LINHA = COBREL-QTD-LINHA-CAB5
                   PERFORM 0120-TEXTO-VAR-0001
                   ADD 3,5 TO WS-TOPO
                   ADD 1 TO CONTA-LINHA
                END-PERFORM
             ELSE
                PERFORM UNTIL CONTA-LINHA = COBREL-QTD-LINHA-CAB5
                   PERFORM 0120-TEXTO-VAR-0001
                   ADD 3,5 TO WS-TOPO
                   ADD 1 TO CONTA-LINHA
                END-PERFORM
             END-IF
             MOVE 0   TO COBREL-VAR-NEGRITO
             INITIALIZE CONTA-LINHA
             COMPUTE COBREL-QTD-LINHA-CAB5 = COBREL-QTD-LINHA-CAB2 +
                     COBREL-QTD-LINHA-CAB3 + COBREL-QTD-LINHA-CAB4 +
                     COBREL-QTD-LINHA-CAB5 + 2

             COMPUTE COBREL-QTD-LINHAS = COBREL-QTD-LINHAS -
                                         COBREL-QTD-LINHA-CAB5
             ADD 2 TO WS-TOPO
             PERFORM UNTIL CONTA-LINHA = COBREL-QTD-LINHAS
                PERFORM  0120-TEXTO-VAR-0001
                ADD 3,5 TO WS-TOPO
                ADD 1 TO CONTA-LINHA
             END-PERFORM
          END-IF
      *--------------------------------------------------------- FIM
      *--------------------------------- MARGEM TOTAL DA PAGINA
          IF COBREL-ORIENTACAO-PAGINA = "R" OR "r"
             MOVE 285, TO COBREL-SHAPE-ALTURA
             PERFORM 0100-SHAPE-0006
          ELSE
             MOVE 200, TO COBREL-SHAPE-ALTURA
             PERFORM 0100-SHAPE-0006
          END-IF
      *--------------------------------------------------------- FIM
      *--------------------------------- LINHAS PARA 1 CABECALHO

          IF COBREL-QTD-LINHA-CAB2 = 0
            IF COBREL-QTD-LINHA-CAB3 = 0
               IF COBREL-QTD-LINHA-CAB4 = 0
                  IF COBREL-QTD-LINHA-CAB5 = 0
      *              MOVE 78 TO COBREL-QTD-LINHAS
                     MOVE 15 TO WS-TOPO
                     INITIALIZE CONTA-LINHA
                     PERFORM UNTIL CONTA-LINHA = COBREL-QTD-LINHAS
                        PERFORM 0120-TEXTO-VAR-0001
                        ADD 1   TO CONTA-LINHA
                        ADD 2,0 TO WS-TOPO
                     END-PERFORM
                  END-IF
               END-IF
            END-IF
          END-IF.
      *------------------------------------------------------------- FIM
       DEFINICAO-FIM.
           EXIT.

       0030-CRIAR-ARQ-DEFINICAO SECTION.
           MOVE ZEROS                  TO WS-TOPO WS-ESQ
           PERFORM PARAMETROS-IMPRESSAO
           PERFORM 0040-PAGINA
              PERFORM DEFINICAO.
       0030-CRIAR-ARQ-DEFINICAO-FIM.
           EXIT.

       0040-PAGINA SECTION.
      *------------
           MOVE 1                        TO CRP-TIPO
           MOVE 1                        TO CRP-MEDIDA
           MOVE 0                        TO CRP-MARGEM-SUP
           MOVE 0                        TO CRP-MARGEM-ESQ
           MOVE 0                        TO CRP-MARGEM-INF
           MOVE 0                        TO CRP-MARGEM-DIR
           IF COBREL-ORIENTACAO-PAGINA = "R" OR "r"
              MOVE "R"                   TO CRP-ORIENTACAO
           ELSE
              MOVE "P"                   TO CRP-ORIENTACAO
           END-IF
           MOVE 10                       TO CRP-PAPEL
           MOVE 0                        TO CRP-ALTURA
           MOVE 0                        TO CRP-LARGURA
           MOVE COBREL-TITULO-RELATORIO  TO CRP-DOCUMENTO
           MOVE "C:\"                    TO CRP-DIRETORIO
           MOVE " "                      TO CRP-EMAIL
           WRITE ATDEF-R1 FROM CR-PAGINA.
       0040-PAGINA-FIM.
           EXIT.

       0050-SHAPE-0001 SECTION.
      *----------------
           MOVE 3                      TO CRS-TIPO
           MOVE 1                      TO CRS-SHAPE
           COMPUTE CRS-POS-SUP = 0,0 + 5,0
           COMPUTE CRS-POS-ESQ = WS-ESQ + 8,0
           MOVE COBREL-SHAPE-ALTURA    TO CRS-POS-ALT
           MOVE COBREL-SHAPE-LARGURA   TO CRS-POS-LARG
           MOVE 255                    TO CRS-SHAPE-R
           MOVE 255                    TO CRS-SHAPE-G
           MOVE 255                    TO CRS-SHAPE-B
           MOVE 2                      TO CRS-INTERIOR
           MOVE 0                      TO CRS-TRACO-R
           MOVE 0                      TO CRS-TRACO-G
           MOVE 0                      TO CRS-TRACO-B
           MOVE 3                      TO CRS-PONTOS
           WRITE ATDEF-R1 FROM CR-SHAPE.
       0050-SHAPE-0001-FIM.
           EXIT.

       0060-SHAPE-0002 SECTION.
      *----------------
           MOVE 3                      TO CRS-TIPO
           MOVE 1                      TO CRS-SHAPE
           COMPUTE CRS-POS-SUP = 0,0 + 5,0
           COMPUTE CRS-POS-ESQ = WS-ESQ + 8,0
           MOVE COBREL-SHAPE-ALTURA    TO CRS-POS-ALT
           MOVE COBREL-SHAPE-LARGURA   TO CRS-POS-LARG
           MOVE 255                    TO CRS-SHAPE-R
           MOVE 255                    TO CRS-SHAPE-G
           MOVE 255                    TO CRS-SHAPE-B
           MOVE 2                      TO CRS-INTERIOR
           MOVE 0                      TO CRS-TRACO-R
           MOVE 0                      TO CRS-TRACO-G
           MOVE 0                      TO CRS-TRACO-B
           MOVE 3                      TO CRS-PONTOS
           WRITE ATDEF-R1 FROM CR-SHAPE.
       0060-SHAPE-0002-FIM.
           EXIT.

       0070-SHAPE-0003 SECTION.
      *----------------
           MOVE 3                      TO CRS-TIPO
           MOVE 1                      TO CRS-SHAPE
           COMPUTE CRS-POS-SUP = 0,0 + 5,0
           COMPUTE CRS-POS-ESQ = WS-ESQ + 8,0
           MOVE COBREL-SHAPE-ALTURA    TO CRS-POS-ALT
           MOVE COBREL-SHAPE-LARGURA   tO CRS-POS-LARG
           MOVE 255                    TO CRS-SHAPE-R
           MOVE 255                    TO CRS-SHAPE-G
           MOVE 255                    TO CRS-SHAPE-B
           MOVE 2                      TO CRS-INTERIOR
           MOVE 0                      TO CRS-TRACO-R
           MOVE 0                      TO CRS-TRACO-G
           MOVE 0                      TO CRS-TRACO-B
           MOVE 3                      TO CRS-PONTOS
           WRITE ATDEF-R1 FROM CR-SHAPE.
       0070-SHAPE-0003-FIM.
           EXIT.

       0080-SHAPE-0004 SECTION.
      *----------------
           MOVE 3                      TO CRS-TIPO
           MOVE 1                      TO CRS-SHAPE
           COMPUTE CRS-POS-SUP = 0,0 + 5,0
           COMPUTE CRS-POS-ESQ = WS-ESQ + 8,0
           MOVE COBREL-SHAPE-ALTURA    TO CRS-POS-ALT
           MOVE COBREL-SHAPE-LARGURA   TO CRS-POS-LARG
           MOVE 255                    TO CRS-SHAPE-R
           MOVE 255                    TO CRS-SHAPE-G
           MOVE 255                    TO CRS-SHAPE-B
           MOVE 2                      TO CRS-INTERIOR
           MOVE 0                      TO CRS-TRACO-R
           MOVE 0                      TO CRS-TRACO-G
           MOVE 0                      TO CRS-TRACO-B
           MOVE 3                      TO CRS-PONTOS
           WRITE ATDEF-R1 FROM CR-SHAPE.
       0080-SHAPE-0004-FIM.
           EXIT.

       0090-SHAPE-0005 SECTION.
      *----------------
           MOVE 3                      TO CRS-TIPO
           MOVE 1                      TO CRS-SHAPE
           COMPUTE CRS-POS-SUP = 0,0 + 5,0
           COMPUTE CRS-POS-ESQ = WS-ESQ + 8,0
           MOVE COBREL-SHAPE-ALTURA    TO CRS-POS-ALT
           MOVE COBREL-SHAPE-LARGURA   TO CRS-POS-LARG
           MOVE 255                    TO CRS-SHAPE-R
           MOVE 255                    TO CRS-SHAPE-G
           MOVE 255                    TO CRS-SHAPE-B
           MOVE 2                      TO CRS-INTERIOR
           MOVE 0                      TO CRS-TRACO-R
           MOVE 0                      TO CRS-TRACO-G
           MOVE 0                      TO CRS-TRACO-B
           MOVE 3                      TO CRS-PONTOS
           WRITE ATDEF-R1 FROM CR-SHAPE.
       0090-SHAPE-0005-FIM.
           EXIT.

       0100-SHAPE-0006 SECTION.
      *----------------
           MOVE 3                      TO CRS-TIPO
           MOVE 1                      TO CRS-SHAPE
           COMPUTE CRS-POS-SUP = 0,0 + 5,0
           COMPUTE CRS-POS-ESQ = WS-ESQ + 8,0
           MOVE COBREL-SHAPE-ALTURA    TO CRS-POS-ALT
           MOVE COBREL-SHAPE-LARGURA   TO CRS-POS-LARG
           MOVE 255                    TO CRS-SHAPE-R
           MOVE 255                    TO CRS-SHAPE-G
           MOVE 255                    TO CRS-SHAPE-B
           MOVE 2                      TO CRS-INTERIOR
           MOVE 0                      TO CRS-TRACO-R
           MOVE 0                      TO CRS-TRACO-G
           MOVE 0                      TO CRS-TRACO-B
           MOVE 3                      TO CRS-PONTOS
           WRITE ATDEF-R1 FROM CR-SHAPE.
       0100-SHAPE-0006-FIM.
           EXIT.

       0130-IM-FIXA-0001 SECTION.
      *------------------
      *    MOVE 4                      TO CRF2-TIPO
      *    MOVE SPACES                 TO CRF2-PATH
      *    STRING LNK-PATH-SIS "IMAGENS\LOGOCOBREL.JPG"
      *             DELIMITED BY "*" INTO CRF2-PATH
      *    COMPUTE CRF2-POS-SUP = 0,0 + 6,0
      *    COMPUTE CRF2-POS-ESQ = WS-ESQ + 10,0
      *    MOVE 13,0                   TO CRF2-POS-ALT
      *    MOVE 45,0                   TO CRF2-POS-LARG
      *    MOVE 0                      TO CRF2-AUTO
      *    MOVE 1                      TO CRF2-AJUSTADO
      *    WRITE ATDEF-R1 FROM CR-FIGURA2.
       0130-IM-FIXA-0001-FIM.
           EXIT.


       0120-TEXTO-VAR-0001 SECTION.
      *--------------------
           MOVE 5                          TO CRV-TIPO
           MOVE 1                          TO CRV-VARIAVEL
           MOVE COBREL-TAMANHO-VARIAVEL    TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 7,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 10,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 7                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           IF COBREL-VAR-NEGRITO = 1
              MOVE 1                   TO CRCL-NEGRITO
              MOVE 0                   TO CRCL-SUBLINHADO
              MOVE 0                   TO CRCL-R
           ELSE
              MOVE 0                   TO CRCL-NEGRITO
              MOVE 1                   TO CRCL-SUBLINHADO
              MOVE 0                   TO CRCL-R
           END-IF
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 14,0                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0120-TEXTO-VAR-0001-FIM.
           EXIT.

       9999-ADICIONAR-DADOS SECTION.
            PERFORM ADICIONAR-CABECALHO
            PERFORM IMPRIMIR-RELATORIO
            PERFORM VERIFICAR-FINAL.
       9999-ADICIONAR-DADOS-FIM.
           EXIT.

       9999-VISUALIZAR-RELATORIO SECTION.
      *--------------------------
           SET BIBLIOTECA TO ENTRY "CobRel.DLL"
           MOVE COBREL-VISUALIZACAO  TO LK-P-VISUAL
           CALL DSDLL "Print" USING BY REFERENCE LK-PRINT
           CANCEL "Print".
       9999-VISUALIZAR-RELATORIO-FIM.
           EXIT.

       9999-VERSAO-DOS-ARQUIVOS SECTION.
      *-------------------------
           MOVE ZEROS                  TO WS-LD-NUMERO
                                          WS-RESULTADO-CALL
           PERFORM 9999-VERIFICA-SE-EXISTE UNTIL
                                    (WS-RESULTADO-CALL <> ZEROS)
           MOVE WS-LD-NUMERO           TO WS-LV-NUMERO
                                          WS-LG-NUMERO.
       9999-VERSAO-DOS-ARQUIVOS-FIM.
           EXIT.

       9999-VERIFICA-SE-EXISTE SECTION.
      *------------------------
           ADD 1                       TO WS-LD-NUMERO
           MOVE ZEROS                  TO WS-TM-ARQ
           CALL "CBL_CHECK_FILE_EXIST" USING WS-LABEL-DEF
                                             WS-RETORNO
                                             WS-RESULTADO-CALL
           IF (WS-TM-ARQ = 0) AND (WS-RESULTADO-CALL = 0)
              MOVE 1                   TO WS-RESULTADO-CALL
           ELSE
           IF (WS-TM-ARQ > 0) AND (WS-RESULTADO-CALL = 0) AND
              (WS-LD-NUMERO = 999)
              MOVE 1                   TO WS-RESULTADO-CALL
                                          WS-LD-NUMERO.
       9999-VERIFICA-SE-EXISTE-FIM.
           EXIT.
      *----------------------------------------------- REGRAS
      *----------------------------EXPLICAÇÃO
      * ESTA ROTINA IRÁ DESEMPENHAR O PAPEL DE ADICIONAR UMA NOVA PAGINA
      * NO DOCUMENTO.
      *--------------------------------------------------------------FIM
       ADICIONAR-PAGINA SECTION.
           CLOSE ATVAR
           SET BIBLIOTECA TO ENTRY "CobRel.DLL"
           MOVE WS-LABEL-VAR           TO LK-A-ARQUIVO
           CALL DSDLL "Add" USING BY REFERENCE LK-ADD
           CANCEL "Add".
       ADICIONAR-PAGINA-FIM.
           EXIT.

      *----------------------------EXPLICAÇÃO
      * ESTA ROTINA IRÁ SE ENCONTRAR ABAIXO DE TODOS OS WRITE ATVAR DO
      * RELATÓRIO, POIS ASSIM ELE SABERÁ QUANDO É A HORA DE ADICIONAR
      * MAIS UMA PAGINA NO RELATÓRIO.
      *--------------------------------------------------------------FIM
       VERIFICAR-LINHA SECTION.
           ADD 1 TO CONTA-LINHA
           IF CONTA-LINHA = COBREL-QTD-LINHAS
              PERFORM ADICIONAR-PAGINA
              PERFORM ADICIONAR-CABECALHO
           END-IF.
       VERIFICAR-LINHA-FIM.
           EXIT.
      *----------------------------EXPLICAÇÃO
      * ESTA ROTINA SE TORNA NECESSÁRIA QUANDO  O RELATÓRIO CHEGA AO FIM
      * E A QUANTIDADE DE LINHAS EXISTENTE NÃO DA O COMANDO PARA QUE
      * SEJA ADICIONADA UMA NOVA PAGINA, SENDO ASSIM, SEM A MESMA
      * FICARIA FALTANDO OS ULTIMOS DADOS NO RELATORIO.
      *--------------------------------------------------------------FIM
       VERIFICAR-FINAL SECTION.
           ADD 1 TO CONTA-LINHA
           IF CONTA-LINHA <> 0
              PERFORM ADICIONAR-PAGINA
           END-IF.
       VERIFICAR-FINAL-FIM.
           EXIT.


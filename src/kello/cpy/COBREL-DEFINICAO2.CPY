       0030-CRIAR-ARQ-DEFINICAO SECTION.
           PERFORM 0040-PAGINA
           PERFORM 0050-IM-FIXA-0001
           PERFORM 0470-TEXTO-VAR-0018
           PERFORM 0450-TEXTO-VAR-0016
      *    PERFORM 0460-TEXTO-VAR-0017
           PERFORM 0060-TEXTO-VAR-0001
           PERFORM 0070-TEXTO-VAR-0002
           PERFORM 0080-SHAPE-0001
           PERFORM 0090-TEXTO-FIXO-0001
           PERFORM 0100-TEXTO-VAR-0003
           PERFORM 0110-TEXTO-FIXO-0002
           PERFORM 0120-TEXTO-VAR-0004
           PERFORM 0130-TEXTO-FIXO-0003
           PERFORM 0140-TEXTO-VAR-0005
           PERFORM 0150-TEXTO-FIXO-0004
           PERFORM 0160-TEXTO-VAR-0006
           PERFORM 0170-TEXTO-FIXO-0005
           PERFORM 0180-TEXTO-VAR-0007
           PERFORM 0190-TEXTO-FIXO-0006
           PERFORM 0200-TEXTO-VAR-0008
           PERFORM 0210-TEXTO-FIXO-0007
           PERFORM 0220-TEXTO-VAR-0009
           PERFORM 0230-TEXTO-FIXO-0008
           PERFORM 0240-TEXTO-VAR-0010
           PERFORM 0250-SHAPE-0002
           PERFORM 0260-TEXTO-FIXO-0009
           PERFORM 0270-TEXTO-FIXO-0010
           PERFORM 0280-TEXTO-FIXO-0011
           PERFORM 0290-TEXTO-FIXO-0012
           PERFORM 0300-TEXTO-FIXO-0013
           PERFORM 0310-SHAPE-0003
           INITIALIZE CONTADOR
           PERFORM UNTIL CONTADOR > CONTA-LINHA
               PERFORM 0320-TEXTO-VAR-0011
               ADD 1 TO CONTADOR
               ADD 4 TO WS-TOPO
           END-PERFORM

           IF WS-CONT = 1 OR QTD-VIAS = 1
              MOVE 0 TO WS-TOPO
           ELSE
              MOVE 150 TO WS-TOPO
           END-IF

           IF CONTA-LINHA > 19
              COMPUTE WS-TOPO = (CONTA-LINHA - 19) * 4
              ADD 5,0 TO WS-TOPO
              PERFORM 0330-SHAPE-0004
              PERFORM 0340-SHAPE-0005
              PERFORM 0350-SHAPE-0006
              PERFORM 0360-TEXTO-FIXO-0014
              PERFORM 0410-TEXTO-FIXO-0017
              PERFORM 0420-TEXTO-FIXO-0018
              PERFORM 0430-TEXTO-VAR-0014
              PERFORM 0440-TEXTO-VAR-0015
           END-IF
           IF CONTA-LINHA <= 19
           COMPUTE WS-TOPO = WS-TOPO - 3,0
              PERFORM 0330-SHAPE-0004
              PERFORM 0340-SHAPE-0005
              PERFORM 0350-SHAPE-0006
              PERFORM 0360-TEXTO-FIXO-0014
              PERFORM 0410-TEXTO-FIXO-0017
              PERFORM 0420-TEXTO-FIXO-0018
              PERFORM 0430-TEXTO-VAR-0014
              PERFORM 0440-TEXTO-VAR-0015
           END-IF.
       0030-CRIAR-ARQ-DEFINICAO-FIM.
           EXIT.

       0040-PAGINA SECTION.
           MOVE 1                      TO CRP-TIPO
           MOVE 1                      TO CRP-MEDIDA
           MOVE 0                      TO CRP-MARGEM-SUP
           MOVE 0                      TO CRP-MARGEM-ESQ
           MOVE 0                      TO CRP-MARGEM-INF
           MOVE 0                      TO CRP-MARGEM-DIR
           MOVE "R"                    TO CRP-ORIENTACAO
           MOVE 10                     TO CRP-PAPEL
           MOVE 0                      TO CRP-ALTURA
           MOVE 0                      TO CRP-LARGURA
           MOVE "Documento do CobRel"
                                       TO CRP-DOCUMENTO
           MOVE "C:\"
                                       TO CRP-DIRETORIO
           MOVE " "
                                       TO CRP-EMAIL
           WRITE ATDEF-R1 FROM CR-PAGINA.
       0040-PAGINA-FIM.
             EXIT.

       0470-TEXTO-VAR-0018 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 70                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 7,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 80,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 9                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 1                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 14,0                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0470-TEXTO-VAR-0018-FIM.
           EXIT.


       0450-TEXTO-VAR-0016 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 70                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 12,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 80,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 9                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 1                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 14,0                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0450-TEXTO-VAR-0016-FIM.
           EXIT.

       0460-TEXTO-VAR-0017 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 10                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 16,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 40,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 9                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 1                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 0                      TO CRCL-TRANSPARENTE
           MOVE 14,0                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0460-TEXTO-VAR-0017-FIM.
           EXIT.

       0050-IM-FIXA-0001 SECTION.
           MOVE 4                      TO CRF-TIPO
           MOVE "C:\programa\info-sol\imagens\logocobrel_ped.jpg"
                                       TO CRF-PATH
           COMPUTE CRF-POS-SUP = WS-TOPO + 4,0
           COMPUTE CRF-POS-ESQ = WS-ESQ + 10,0
           MOVE 18,0                   TO CRF-POS-ALT
           MOVE 25,0                   TO CRF-POS-LARG
           MOVE 0                      TO CRF-AUTO
           MOVE 1                      TO CRF-AJUSTADO
           WRITE ATDEF-R1 FROM CR-FIGURA.
       0050-IM-FIXA-0001-FIM.
           EXIT.


       0060-TEXTO-VAR-0001 SECTION.
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 30                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 15,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 75,0
           MOVE "MS Sans Serif"
                                       TO CRCL-FONTE
           MOVE 14                     TO CRCL-TAM-FONTE
           MOVE 255                    TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 1                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 0                      TO CRCL-TRANSPARENTE
           MOVE 66,2                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0060-TEXTO-VAR-0001-FIM.
           EXIT.

       0070-TEXTO-VAR-0002 SECTION.
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 10                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 19,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 178,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 8                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 1                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 0                      TO CRCL-TRANSPARENTE
           MOVE 14,0                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0070-TEXTO-VAR-0002-FIM.
           EXIT.

       0080-SHAPE-0001 SECTION.
           MOVE 3                      TO CRS-TIPO
           MOVE 5                      TO CRS-SHAPE
           COMPUTE CRS-POS-SUP = WS-TOPO + 23,0
           COMPUTE CRS-POS-ESQ = WS-ESQ + 0,0
           MOVE 0,4                    TO CRS-POS-ALT
           MOVE 210,0                  TO CRS-POS-LARG
           MOVE 255                    TO CRS-SHAPE-R
           MOVE 255                    TO CRS-SHAPE-G
           MOVE 255                    TO CRS-SHAPE-B
           MOVE 1                      TO CRS-INTERIOR
           MOVE 0                      TO CRS-TRACO-R
           MOVE 0                      TO CRS-TRACO-G
           MOVE 0                      TO CRS-TRACO-B
           MOVE 2                      TO CRS-PONTOS
           WRITE ATDEF-R1 FROM CR-SHAPE.
       0080-SHAPE-0001-FIM.
           EXIT.

       0090-TEXTO-FIXO-0001 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "CLIENTE ....:"        TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 26,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 10,0
           MOVE "MS Sans Serif"        TO CRL-FONTE
           MOVE 8                      TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 1                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 15,4                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0090-TEXTO-FIXO-0001-FIM.
           EXIT.

       0100-TEXTO-VAR-0003 SECTION.
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 70                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 26,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 29,0
           MOVE "Courier New"          TO CRCL-FONTE
           MOVE 8                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 0                      TO CRCL-TRANSPARENTE
           MOVE 98,0                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0100-TEXTO-VAR-0003-FIM.
           EXIT.

       0110-TEXTO-FIXO-0002 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "DATA..........:"      TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 26,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 160,0
           MOVE "MS Sans Serif"        TO CRL-FONTE
           MOVE 8                      TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 1                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 15,6                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0110-TEXTO-FIXO-0002-FIM.
           EXIT.

       0120-TEXTO-VAR-0004 SECTION.
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 15                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 26,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 180,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 8                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 0                      TO CRCL-TRANSPARENTE
           MOVE 21,0                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0120-TEXTO-VAR-0004-FIM.
           EXIT.

       0130-TEXTO-FIXO-0003 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "CPF / CNPJ.:"
                                       TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 31,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 10,0
           MOVE "MS Sans Serif"
                                       TO CRL-FONTE
           MOVE 8                      TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 1                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 15,2                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0130-TEXTO-FIXO-0003-FIM.
           EXIT.

       0140-TEXTO-VAR-0005 SECTION.
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 20                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 31,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 29,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 8                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 0                      TO CRCL-TRANSPARENTE
           MOVE 28,0                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0140-TEXTO-VAR-0005-FIM.
           EXIT.


       0150-TEXTO-FIXO-0004 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "RG / INSC..:"
                                       TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 31,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 100,0
           MOVE "MS Sans Serif"
                                       TO CRL-FONTE
           MOVE 8                      TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 1                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 11,0                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0150-TEXTO-FIXO-0004-FIM.
           EXIT.

       0160-TEXTO-VAR-0006 SECTION.
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 20                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 31,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 117,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 8                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 0                      TO CRCL-TRANSPARENTE
           MOVE 28,0                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0160-TEXTO-VAR-0006-FIM.
           EXIT.

       0170-TEXTO-FIXO-0005 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "SEQUÊNCIA.:"
                                       TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 31,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 160,0
           MOVE "MS Sans Serif"
                                       TO CRL-FONTE
           MOVE 8                      TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 1                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 16,0                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0170-TEXTO-FIXO-0005-FIM.
           EXIT.

       0180-TEXTO-VAR-0007.
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 10                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 31,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 180,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 8                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 0                      TO CRCL-TRANSPARENTE
           MOVE 14,0                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0180-TEXTO-VAR-0007-FIM.
           EXIT.

       0190-TEXTO-FIXO-0006 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "ENDEREÇO.:"
                                       TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 36,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 10,0
           MOVE "MS Sans Serif"
                                       TO CRL-FONTE
           MOVE 8                      TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 1                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 15,4                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0190-TEXTO-FIXO-0006-FIM.
           EXIT.

       0200-TEXTO-VAR-0008 SECTION.
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 40                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 36,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 29,0
           MOVE "Courier New"          TO CRCL-FONTE
           MOVE 8                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 0                      TO CRCL-TRANSPARENTE
           MOVE 63,0                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0200-TEXTO-VAR-0008-FIM.
           EXIT.

       0210-TEXTO-FIXO-0007 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "FONE ........:"
                                       TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 36,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 100,0
           MOVE "MS Sans Serif"
                                       TO CRL-FONTE
           MOVE 8                      TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 1                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 10,8                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0210-TEXTO-FIXO-0007-FIM.
           EXIT.

       0220-TEXTO-VAR-0009 SECTION.
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 20                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 36,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 117,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 8                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 0                      TO CRCL-TRANSPARENTE
           MOVE 28,0                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0220-TEXTO-VAR-0009-FIM.
           EXIT.

       0230-TEXTO-FIXO-0008 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "PAGINA........:"
                                       TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 36,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 160,0
           MOVE "MS Sans Serif"
                                       TO CRL-FONTE
           MOVE 8                      TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 1                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 16,6                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0230-TEXTO-FIXO-0008-FIM.
           EXIT.

       0240-TEXTO-VAR-0010 SECTION.
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 10                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 36,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 180,0
           MOVE "Courier New"          TO CRCL-FONTE
           MOVE 8                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 0                      TO CRCL-TRANSPARENTE
           MOVE 14,0                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0240-TEXTO-VAR-0010-FIM.
           EXIT.

       0250-SHAPE-0002 SECTION.
           MOVE 3                      TO CRS-TIPO
           MOVE 5                      TO CRS-SHAPE
           COMPUTE CRS-POS-SUP = WS-TOPO + 42,0
           COMPUTE CRS-POS-ESQ = WS-ESQ + -0,2
           MOVE 0,4                    TO CRS-POS-ALT
           MOVE 210,0                  TO CRS-POS-LARG
           MOVE 255                    TO CRS-SHAPE-R
           MOVE 255                    TO CRS-SHAPE-G
           MOVE 255                    TO CRS-SHAPE-B
           MOVE 1                      TO CRS-INTERIOR
           MOVE 0                      TO CRS-TRACO-R
           MOVE 0                      TO CRS-TRACO-G
           MOVE 0                      TO CRS-TRACO-B
           MOVE 2                      TO CRS-PONTOS
           WRITE ATDEF-R1 FROM CR-SHAPE.
       0250-SHAPE-0002-FIM.
           EXIT.

       0260-TEXTO-FIXO-0009 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "QTD"
                                       TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 44,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 10,0
           MOVE "MS Sans Serif"
                                       TO CRL-FONTE
           MOVE 10                     TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 1                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 20,0                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0260-TEXTO-FIXO-0009-FIM.
           EXIT.

       0270-TEXTO-FIXO-0010 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "UNID."
                                       TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 44,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 23,0
           MOVE "MS Sans Serif"
                                       TO CRL-FONTE
           MOVE 10                     TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 1                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 8,4                    TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0270-TEXTO-FIXO-0010-FIM.
           EXIT.

       0280-TEXTO-FIXO-0011 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "DESCRIÇÃO"
                                       TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 44,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 38,3
           MOVE "MS Sans Serif"
                                       TO CRL-FONTE
           MOVE 10                     TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 1                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 17,6                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0280-TEXTO-FIXO-0011-FIM.
           EXIT.

       0290-TEXTO-FIXO-0012 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "VL. UNIT."
                                       TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 44,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 150,0
           MOVE "MS Sans Serif"
                                       TO CRL-FONTE
           MOVE 10                     TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 1                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 13,4                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0290-TEXTO-FIXO-0012-FIM.
           EXIT.

       0300-TEXTO-FIXO-0013 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "VL. TOTAL"
                                       TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 44,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 175,0
           MOVE "MS Sans Serif"
                                       TO CRL-FONTE
           MOVE 10                     TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 1                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 15,2                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0290-TEXTO-FIXO-0012-FIM.
           EXIT.

       0310-SHAPE-0003 SECTION.
           MOVE 3                      TO CRS-TIPO
           MOVE 5                      TO CRS-SHAPE
           COMPUTE CRS-POS-SUP = WS-TOPO + 48,5
           COMPUTE CRS-POS-ESQ = WS-ESQ + 0,0
           MOVE 0,4                    TO CRS-POS-ALT
           MOVE 210,0                  TO CRS-POS-LARG
           MOVE 255                    TO CRS-SHAPE-R
           MOVE 255                    TO CRS-SHAPE-G
           MOVE 255                    TO CRS-SHAPE-B
           MOVE 1                      TO CRS-INTERIOR
           MOVE 0                      TO CRS-TRACO-R
           MOVE 0                      TO CRS-TRACO-G
           MOVE 0                      TO CRS-TRACO-B
           MOVE 2                      TO CRS-PONTOS
           WRITE ATDEF-R1 FROM CR-SHAPE.
       0310-SHAPE-0003-FIM.
           EXIT.

       0320-TEXTO-VAR-0011 SECTION.
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 137                    TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 51,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 0,6
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 8                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 0                      TO CRCL-TRANSPARENTE
           MOVE 191,8                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0320-TEXTO-VAR-0011-FIM.
           EXIT.

       0330-SHAPE-0004 SECTION.
           MOVE 3                      TO CRS-TIPO
           MOVE 5                      TO CRS-SHAPE
           COMPUTE CRS-POS-SUP = WS-TOPO + 130,0
           COMPUTE CRS-POS-ESQ = WS-ESQ + 0,0
           MOVE 0,4                    TO CRS-POS-ALT
           MOVE 210,0                  TO CRS-POS-LARG
           MOVE 255                    TO CRS-SHAPE-R
           MOVE 255                    TO CRS-SHAPE-G
           MOVE 255                    TO CRS-SHAPE-B
           MOVE 1                      TO CRS-INTERIOR
           MOVE 0                      TO CRS-TRACO-R
           MOVE 0                      TO CRS-TRACO-G
           MOVE 0                      TO CRS-TRACO-B
           MOVE 2                      TO CRS-PONTOS
           WRITE ATDEF-R1 FROM CR-SHAPE.
       0330-SHAPE-0004-FIM.
           EXIT.

       0340-SHAPE-0005 SECTION.
           MOVE 3                      TO CRS-TIPO
           MOVE 1                      TO CRS-SHAPE
           COMPUTE CRS-POS-SUP = WS-TOPO + 130,0
           COMPUTE CRS-POS-ESQ = WS-ESQ + 155,0
           MOVE 8,0                    TO CRS-POS-ALT
           MOVE 20,0                   TO CRS-POS-LARG
           MOVE 255                    TO CRS-SHAPE-R
           MOVE 255                    TO CRS-SHAPE-G
           MOVE 255                    TO CRS-SHAPE-B
           MOVE 1                      TO CRS-INTERIOR
           MOVE 0                      TO CRS-TRACO-R
           MOVE 0                      TO CRS-TRACO-G
           MOVE 0                      TO CRS-TRACO-B
           MOVE 3                      TO CRS-PONTOS
           WRITE ATDEF-R1 FROM CR-SHAPE.
       0340-SHAPE-0005-FIM.
           EXIT.

       0350-SHAPE-0006 SECTION.
           MOVE 3                      TO CRS-TIPO
           MOVE 1                      TO CRS-SHAPE
           COMPUTE CRS-POS-SUP = WS-TOPO + 130,0
           COMPUTE CRS-POS-ESQ = WS-ESQ + 174,0
           MOVE 8,0                    TO CRS-POS-ALT
           MOVE 30,0                   TO CRS-POS-LARG
           MOVE 255                    TO CRS-SHAPE-R
           MOVE 255                    TO CRS-SHAPE-G
           MOVE 255                    TO CRS-SHAPE-B
           MOVE 1                      TO CRS-INTERIOR
           MOVE 0                      TO CRS-TRACO-R
           MOVE 0                      TO CRS-TRACO-G
           MOVE 0                      TO CRS-TRACO-B
           MOVE 3                      TO CRS-PONTOS
           WRITE ATDEF-R1 FROM CR-SHAPE.
       0350-SHAPE-0006-FIM.
           EXIT.

       0360-TEXTO-FIXO-0014 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "TOTAL"                TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 132,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 160,0
           MOVE "MS Sans Serif"
                                       TO CRL-FONTE
           MOVE 10                     TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 1                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 10,0                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0350-SHAPE-0006-FIM.
           EXIT.

       0440-TEXTO-VAR-0015 SECTION.
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 15                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 132,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 175,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 10                     TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 0                      TO CRCL-TRANSPARENTE
           MOVE 24,0                   TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0440-TEXTO-VAR-0015-FIM.
           EXIT.


       0410-TEXTO-FIXO-0017 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "___________________________________"
                                       TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 138,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 87,0
           MOVE "MS Sans Serif"
                                       TO CRL-FONTE
           MOVE 8                      TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 0                      TO CRL-NEGRITO
           MOVE 0                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 54,0                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0410-TEXTO-FIXO-0017-FIM.
           EXIT.

       0420-TEXTO-FIXO-0018 SECTION.
           MOVE 2                      TO CRL-TIPO
           MOVE "ASSINATURA DO CLIENTE"
                                       TO CRL-TEXTO
           COMPUTE CRL-SUPERIOR = WS-TOPO + 142,0
           COMPUTE CRL-ESQUERDA = WS-ESQ + 96,0
           MOVE "MS Sans Serif"
                                       TO CRL-FONTE
           MOVE 8                      TO CRL-TAM-FONTE
           MOVE 0                      TO CRL-R
           MOVE 0                      TO CRL-G
           MOVE 0                      TO CRL-B
           MOVE 0                      TO CRL-NEGRITO
           MOVE 1                      TO CRL-ITALICO
           MOVE 0                      TO CRL-SUBLINHADO
           MOVE 0                      TO CRL-RISCADO
           MOVE 1                      TO CRL-TRANSPARENTE
           MOVE 28,4                   TO CRL-LARGURA
           MOVE 0                      TO CRL-ALINHAMENTO
           WRITE ATDEF-R1 FROM CR-LABEL.
       0420-TEXTO-FIXO-0018-FIM.
           EXIT.

       0430-TEXTO-VAR-0014 SECTION.
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 150                    TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 146,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + -0,2
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 8                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 0                      TO CRCL-TRANSPARENTE
           MOVE 210,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0430-TEXTO-VAR-0014-FIM.
           EXIT.



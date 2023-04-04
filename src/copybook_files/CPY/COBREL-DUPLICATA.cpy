       0010-INICIO SECTION.
      *------------
           PERFORM 0020-INICIALIZAR
           IF (LK-I-STATUS <> "00")
              DISPLAY "ERRO DE INICIALIZACAO: " LK-I-STATUS
           ELSE
              PERFORM 9999-ADICIONAR-DADOS
           IF (LK-A-STATUS <> "00")
              IF (LK-A-STATUS = "05")
                 NEXT SENTENCE
              ELSE
                 DISPLAY "ERRO DE ADICAO DE DADOS: " LK-A-STATUS
           ELSE
              PERFORM 9999-VISUALIZAR-RELATORIO

           CALL "CBL_DELETE_FILE" USING WS-LABEL-DEF
           CALL "CBL_DELETE_FILE" USING WS-LABEL-VAR
           CALL "CBL_DELETE_FILE" USING WS-LABEL-GRF

           MOVE "MOSTRAR-TELA" TO DS-PROCEDURE
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
           MOVE 0                      TO LK-I-QTDE
           CALL DSDLL "Init" USING BY REFERENCE LK-INIT
           CANCEL "Init".
       0020-INICIALIZAR-FIM.
           EXIT.

       0030-CRIAR-ARQ-DEFINICAO SECTION.
      *-------------------------
           MOVE ZEROS                  TO WS-TOPO WS-ESQ

           PERFORM 0040-PAGINA
           PERFORM 0050-SHAPE-0001
           PERFORM 0070-IM-FIXA-0002
           PERFORM 0060-IM-FIXA-0001
           PERFORM 0080-TEXTO-VAR-0001
           PERFORM 0090-TEXTO-VAR-0002
           PERFORM 0100-TEXTO-VAR-0003
           PERFORM 0110-TEXTO-VAR-0004
           PERFORM 0120-TEXTO-VAR-0005
           PERFORM 0140-TEXTO-VAR-0007
           PERFORM 0150-TEXTO-VAR-0008
           PERFORM 0160-TEXTO-VAR-0009
           PERFORM 0170-TEXTO-VAR-0010
           PERFORM 0130-TEXTO-VAR-0006
           PERFORM 0180-TEXTO-VAR-0011
           PERFORM 0190-IM-FIXA-0003
           PERFORM 0200-TEXTO-VAR-0012
           PERFORM 0210-TEXTO-VAR-0013
           PERFORM 0220-IM-FIXA-0004
           PERFORM 0230-TEXTO-VAR-0014
           PERFORM 0240-TEXTO-VAR-0015
           PERFORM 0250-TEXTO-VAR-0016
           PERFORM 0260-TEXTO-VAR-0017
           PERFORM 0270-TEXTO-VAR-0018
           PERFORM 0280-IM-FIXA-0005
           PERFORM 0290-TEXTO-VAR-0019
           PERFORM 0300-TEXTO-VAR-0020
           PERFORM 0310-IM-FIXA-0006
           PERFORM 0320-TEXTO-VAR-0021
           PERFORM 0330-TEXTO-VAR-0022
           PERFORM 0340-TEXTO-VAR-0023
           PERFORM 0350-TEXTO-VAR-0024
           PERFORM 0360-TEXTO-VAR-0025.
       0030-CRIAR-ARQ-DEFINICAO-FIM.
           EXIT.

       0040-PAGINA SECTION.
      *------------
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
           MOVE "DUPLICATA"
                                       TO CRP-DOCUMENTO
           MOVE "C:\"
                                       TO CRP-DIRETORIO
           MOVE " "
                                       TO CRP-EMAIL
           WRITE ATDEF-R1 FROM CR-PAGINA.
       0040-PAGINA-FIM.
           EXIT.

       0050-SHAPE-0001 SECTION.
      *----------------
           MOVE 3                      TO CRS-TIPO
           MOVE 1                      TO CRS-SHAPE
           COMPUTE CRS-POS-SUP = WS-TOPO + 6,0
           COMPUTE CRS-POS-ESQ = WS-ESQ + 5,0
           MOVE 139,0                  TO CRS-POS-ALT
           MOVE 200,0                  TO CRS-POS-LARG
           MOVE 255                    TO CRS-SHAPE-R
           MOVE 255                    TO CRS-SHAPE-G
           MOVE 255                    TO CRS-SHAPE-B
           MOVE 1                      TO CRS-INTERIOR
           MOVE 0                      TO CRS-TRACO-R
           MOVE 0                      TO CRS-TRACO-G
           MOVE 0                      TO CRS-TRACO-B
           MOVE 3                      TO CRS-PONTOS
           WRITE ATDEF-R1 FROM CR-SHAPE.
       0050-SHAPE-0001-FIM.
           EXIT.

       0060-IM-FIXA-0001 SECTION.
      *------------------
           MOVE 4                      TO CRF-TIPO
           MOVE
           "C:\programa\info-sol\imagens\cobrel_cabecalhoDuplicata.jpg"
                                       TO CRF-PATH
           COMPUTE CRF-POS-SUP = WS-TOPO + 7,0
           COMPUTE CRF-POS-ESQ = WS-ESQ + 7,0
           MOVE 32,8                   TO CRF-POS-ALT
           MOVE 107,6                  TO CRF-POS-LARG
           MOVE 0                      TO CRF-AUTO
           MOVE 1                      TO CRF-AJUSTADO
           WRITE ATDEF-R1 FROM CR-FIGURA.
       0060-IM-FIXA-0001-FIM.
           EXIT.

       0070-IM-FIXA-0002 SECTION.
      *------------------
           MOVE 4                      TO CRF-TIPO
           MOVE
           "C:\programa\info-sol\imagens\cobrel_esqueletoDuplicata.jpg"
                                       TO CRF-PATH
           COMPUTE CRF-POS-SUP = WS-TOPO + 8,0
           COMPUTE CRF-POS-ESQ = WS-ESQ + 7,0
           MOVE 135,0                  TO CRF-POS-ALT
           MOVE 196,0                  TO CRF-POS-LARG
           MOVE 0                      TO CRF-AUTO
           MOVE 1                      TO CRF-AJUSTADO
           WRITE ATDEF-R1 FROM CR-FIGURA.
       0070-IM-FIXA-0002-FIM.
           EXIT.

       0080-TEXTO-VAR-0001 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 50                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 10,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 117,0
           MOVE "Arial Black"
                                       TO CRCL-FONTE
           MOVE 7                     TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 81,0                   TO CRCL-LARGURA
           MOVE 3                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0080-TEXTO-VAR-0001-FIM.
           EXIT.

       0090-TEXTO-VAR-0002 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 30                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 14,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 128,0
           MOVE "Arial Black"
                                       TO CRCL-FONTE
           MOVE 7                     TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 54,0                   TO CRCL-LARGURA
           MOVE 3                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0090-TEXTO-VAR-0002-FIM.
           EXIT.

       0100-TEXTO-VAR-0003 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 30                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 23,5
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 140,0
           MOVE "Arial Black"
                                       TO CRCL-FONTE
           MOVE 7                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 42,0                   TO CRCL-LARGURA
           MOVE 3                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0100-TEXTO-VAR-00030-FIM.
           EXIT.

       0110-TEXTO-VAR-0004 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 30                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 27,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 140,0
           MOVE "Arial Black"
                                       TO CRCL-FONTE
           MOVE 7                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 42,0                   TO CRCL-LARGURA
           MOVE 3                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0110-TEXTO-VAR-0004-FIM.
           EXIT.

       0120-TEXTO-VAR-0005 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 31                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 33,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 128,0
           MOVE "Arial Black"
                                       TO CRCL-FONTE
           MOVE 8                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 63,0                   TO CRCL-LARGURA
           MOVE 3                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0120-TEXTO-VAR-0005-FIM.
           EXIT.


       0130-TEXTO-VAR-0006 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 10                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 49,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 140,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 13                     TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 1                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 24,0                   TO CRCL-LARGURA
           MOVE 3                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0130-TEXTO-VAR-0006-FIM.
           EXIT.

       0140-TEXTO-VAR-0007 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 12                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 51,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 18,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 11                     TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 1                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 21,6                   TO CRCL-LARGURA
           MOVE 3                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0140-TEXTO-VAR-0007-FIM.
           EXIT.

       0150-TEXTO-VAR-0008 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 10                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 51,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 51,5
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 11                     TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 18,0                   TO CRCL-LARGURA
           MOVE 3                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0150-TEXTO-VAR-0008-FIM.
           EXIT.

       0160-TEXTO-VAR-0009 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 12                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 51,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 81,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 11                     TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 1                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 21,6                   TO CRCL-LARGURA
           MOVE 3                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0160-TEXTO-VAR-0009-FIM.
           EXIT.

       0170-TEXTO-VAR-0010 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 10                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 51,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 115,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 11                     TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 18,0                   TO CRCL-LARGURA
           MOVE 3                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0170-TEXTO-VAR-0010-FIM.
           EXIT.

       0180-TEXTO-VAR-0011 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 59                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 59,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 53,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 9                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 112,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0180-TEXTO-VAR-0011-FIM.
           EXIT.

       0190-IM-FIXA-0003 SECTION.
      *------------------
           MOVE 4                      TO CRF-TIPO
           MOVE
        "C:\programa\info-sol\imagens\cobrel_textoverticalDuplicata.jpg"
                                       TO CRF-PATH
           COMPUTE CRF-POS-SUP = WS-TOPO + 60,0
           COMPUTE CRF-POS-ESQ = WS-ESQ + 44,0
           MOVE 79,0                   TO CRF-POS-ALT
           MOVE 4,0                    TO CRF-POS-LARG
           MOVE 0                      TO CRF-AUTO
           MOVE 1                      TO CRF-AJUSTADO
           WRITE ATDEF-R1 FROM CR-FIGURA.
       0190-IM-FIXA-0003-FIM.
           EXIT.

       0200-TEXTO-VAR-0012 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 59                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 63,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 53,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 9                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 112,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0200-TEXTO-VAR-0012-FIM.
           EXIT.

       0210-TEXTO-VAR-0013 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 59                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 67,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 53,0
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
           MOVE 112,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0210-TEXTO-VAR-0013-FIM.
           EXIT.

       0220-IM-FIXA-0004 SECTION.
      *------------------
           MOVE 4                      TO CRF-TIPO
           MOVE "C:\programa\info-sol\imagens\cobrel_meioDuplicata.jpg"
                                       TO CRF-PATH
           COMPUTE CRF-POS-SUP = WS-TOPO + 71,0
           COMPUTE CRF-POS-ESQ = WS-ESQ + 52,0
           MOVE 30,0                   TO CRF-POS-ALT
           MOVE 151,0                  TO CRF-POS-LARG
           MOVE 0                      TO CRF-AUTO
           MOVE 1                      TO CRF-AJUSTADO
           WRITE ATDEF-R1 FROM CR-FIGURA.
       0220-IM-FIXA-0004-FIM.
           EXIT.

       0230-TEXTO-VAR-0014 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 69                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 74,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 54,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 10                     TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 1                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 147,2                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0230-TEXTO-VAR-0014-FIM.
           EXIT.

       0240-TEXTO-VAR-0015 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 76                    TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 80,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 54,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 9                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 147,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0240-TEXTO-VAR-0015-FIM.
           EXIT.

       0250-TEXTO-VAR-0016 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 76                    TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 85,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 54,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 9                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 147,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0250-TEXTO-VAR-0016-FIM.
           EXIT.

       0260-TEXTO-VAR-0017 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 76                    TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 90,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 54,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 9                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 147,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0260-TEXTO-VAR-0017-FIM.
           EXIT.

       0270-TEXTO-VAR-0018 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 76                    TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 95,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 54,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 9                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 147,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0270-TEXTO-VAR-0018-FIM.
           EXIT.

       0280-IM-FIXA-0005 SECTION.
      *------------------
           MOVE 4                      TO CRF-TIPO
           MOVE
           "C:\programa\info-sol\imagens\cobrel_extensoDuplicata.jpg"
                                       TO CRF-PATH
           COMPUTE CRF-POS-SUP = WS-TOPO + 102,0
           COMPUTE CRF-POS-ESQ = WS-ESQ + 52,0
           MOVE 16,0                   TO CRF-POS-ALT
           MOVE 151,0                  TO CRF-POS-LARG
           MOVE 0                      TO CRF-AUTO
           MOVE 1                      TO CRF-AJUSTADO
           WRITE ATDEF-R1 FROM CR-FIGURA.
       0280-IM-FIXA-0005-FIM.
           EXIT.

       0290-TEXTO-VAR-0019 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 74                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 105,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 74,0
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
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 126,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0290-TEXTO-VAR-0019-FIM.
           EXIT.

       0300-TEXTO-VAR-0020 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 74                     TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 112,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 74,0
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
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 126,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0300-TEXTO-VAR-0020-FIM.
           EXIT.

       0310-IM-FIXA-0006 SECTION.
      *------------------
           MOVE 4                      TO CRF-TIPO
           MOVE "C:\programa\info-sol\imagens\cobrel_meioDuplicata.jpg"
                                       TO CRF-PATH
           COMPUTE CRF-POS-SUP = WS-TOPO + 119,0
           COMPUTE CRF-POS-ESQ = WS-ESQ + 52,0
           MOVE 24,0                   TO CRF-POS-ALT
           MOVE 151,0                  TO CRF-POS-LARG
           MOVE 0                      TO CRF-AUTO
           MOVE 1                      TO CRF-AJUSTADO
           WRITE ATDEF-R1 FROM CR-FIGURA.
       0310-IM-FIXA-0006-FIM.
           EXIT.

       0320-TEXTO-VAR-0021 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 76                    TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 121,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 54,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 9                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 147,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0320-TEXTO-VAR-0021-FIM.
           EXIT.

       0330-TEXTO-VAR-0022 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 76                    TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 124,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 54,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 9                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 147,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0330-TEXTO-VAR-0022-FIM.
           EXIT.

       0340-TEXTO-VAR-0023 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 76                    TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 127,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 54,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 9                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 147,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0340-TEXTO-VAR-0023-FIM.
           EXIT.

       0350-TEXTO-VAR-0024 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 76                    TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 135,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 54,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 9                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 147,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0350-TEXTO-VAR-0024-FIM.
           EXIT.

       0360-TEXTO-VAR-0025 SECTION.
      *--------------------
           MOVE 5                      TO CRV-TIPO
           MOVE 1                      TO CRV-VARIAVEL
           MOVE 76                    TO CRCL-TAMANHO
           COMPUTE CRCL-SUPERIOR = WS-TOPO + 138,0
           COMPUTE CRCL-ESQUERDA = WS-ESQ + 54,0
           MOVE "Courier New"
                                       TO CRCL-FONTE
           MOVE 9                      TO CRCL-TAM-FONTE
           MOVE 0                      TO CRCL-R
           MOVE 0                      TO CRCL-G
           MOVE 0                      TO CRCL-B
           MOVE 0                      TO CRCL-NEGRITO
           MOVE 0                      TO CRCL-ITALICO
           MOVE 0                      TO CRCL-SUBLINHADO
           MOVE 0                      TO CRCL-RISCADO
           MOVE 1                      TO CRCL-TRANSPARENTE
           MOVE 147,0                  TO CRCL-LARGURA
           MOVE 1                      TO CRCL-ALINHAMENTO
           MOVE CR-COMPLEMENTO-LABEL   TO CRV-COMPLEMENTO
           WRITE ATDEF-R1 FROM CR-VARIAVEL.
       0360-TEXTO-VAR-0025-FIM.
           EXIT.

       9999-ADICIONAR-DADOS SECTION.
       9999-ADICIONAR-DADOS-FIM.
           EXIT.

       9999-VISUALIZAR-RELATORIO SECTION.
      *--------------------------
           SET BIBLIOTECA TO ENTRY "CobRel.DLL"
           MOVE 2                      TO LK-P-VISUAL
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

      *-----------------------------------------------------------------
      * REGRAS
      *-----------------------------------------------------------------
       ADICIONAR-PAGINA SECTION.
           CLOSE ATVAR
           SET BIBLIOTECA TO ENTRY "CobRel.DLL"
           MOVE WS-LABEL-VAR           TO LK-A-ARQUIVO
           CALL DSDLL "Add" USING BY REFERENCE LK-ADD
           CANCEL "Add".
       ADICIONAR-PAGINA-FIM.
           EXIT.

       ADICIONAR-CABECALHO SECTION.
       ADICIONAR-CABECALHO-FIM.
           EXIT.

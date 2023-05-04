       COPY DSLANG.CPY.
       identification division.
       program-id.                 concad.
       author.                     ALFREDO SAVIOLLI.
       DATE-WRITTEN.               13-06-2005.
       SECURITY.      CONSULTA DOS CADASTRO MTD019,CGD001 E CGD010



       environment division.
       special-names.              decimal-point  is  comma
                                   console        is  crt.
       class-control.
           Window             is class "wclass".
       input-output section.

       file-control.

           copy   "mtpx019".

           copy   "cgpx001".

           copy   "cgpx002".

           copy   "cgpx003".

           copy   "capx010".

           copy   "cgpx010".

           copy   "cgpx011".

           copy   "crpx020".

       data division.
       file section.

           copy   "mtpw019".

           copy   "capw010".

           copy   "cgpw001".

           copy   "cgpw002".

           copy   "cgpw003".

           copy   "cgpw010".

           copy   "cgpw011".

           copy   "crpw020".

       working-storage section.
       78  dialog-system               VALUE "DSGRUN".
       01 Display-Details-1            PIC 9(4).
       01 Display-Details-2            PIC 9(4).

       COPY "DS-CNTRL.MF".
       COPY "DSSYSINF.CPY".

       COPY "concad.CPB".
       copy "concad.cpy".

       78  REFRESH-TEXT-AND-DATA-PROC VALUE 255.
       77  DISPLAY-ERROR-NO          PIC 9(4).
       01  VARIAVEIS.
           05  ST-CAD010             PIC XX       VALUE SPACES.
           05  ST-MTD019             PIC XX       VALUE SPACES.
           05  ST-CGD001             PIC XX       VALUE SPACES.
           05  ST-CGD002             PIC XX       VALUE SPACES.
           05  ST-CGD003             PIC XX       VALUE SPACES.
           05  ST-CGD010             PIC XX       VALUE SPACES.
           05  ST-CGD011             PIC XX       VALUE SPACES.
           05  ST-CRD020             PIC XX       VALUE SPACES.
           05  LIN-DETALHE-W         PIC X(30)    VALUE SPACES.
           05  EMP-REFERENCIA.
               10  FILLER            PIC X(15)
                   VALUE "\PROGRAMA\KELLO".
               10  VAR1              PIC X VALUE "\".
               10  EMP-REC           PIC XXX.
               10  VAR2              PIC X VALUE "\".
               10  ARQ-REC           PIC X(10).
           05  EMPRESA-REF REDEFINES EMP-REFERENCIA PIC X(30).
           05  CODIGO-W              PIC X(8)   VALUE SPACES.
           05  CLASSIF-W             PIC X      VALUE SPACES.

           05  INICIAL-PROCURADA     PIC X(6)     VALUE SPACES.
           05  INICIAL-A-COMPARAR    PIC X(6)     VALUE SPACES.
           05  I                     PIC 9        VALUE ZEROS.
           05  LETRA                 PIC X        VALUE SPACES.
           05  SAIR-W                PIC 9        VALUE ZEROS.
           05  masc-cpf              pic 99999999999.
           05  masc-cgc              pic 99999999999999.
           05  masc-cgc2             pic 9999999999999999.
      *    variáveis p/ listar os nomes com iniciais solicitadas
           COPY "PARAMETR".

       77 janelaPrincipal              object reference.
       77 handle8                      pic 9(08) comp-x value zeros.
       77 wHandle                      pic 9(09) comp-5 value zeros.

       01  TIPO-MSG                  PIC X(01).
       01  RESP-MSG                  PIC X(01).
       01  MENSAGEM                  PIC X(200).
       01  ORDEM-ACHADA              PIC 9(05) VALUE ZEROS.
       01  IND-PROCURA               PIC 9(03) VALUE ZEROS.
       01  IND-STR                   PIC 9(03) VALUE ZEROS.
       01  IND-TAB                   PIC 9(03) VALUE ZEROS.
       01  IND-AJUSTE                PIC 9(03) VALUE ZEROS.
       77  TOPO-TELA                 PIC 9(03) VALUE ZEROS.
           88 AJUSTAR-TABELA         VALUE 2 THRU 15.
       01  KEY-LIMITE                PIC 9(08) VALUE ZEROS.
       01  ABRIU-GS                  PIC X(01) VALUE "S".
       01  IND-P                     PIC 9(03) VALUE ZEROS.
       01  IND-A                     PIC 9(03) VALUE ZEROS.
       01  TAM-PROCURA               PIC 9(02) VALUE ZEROS.
       01  LIMITE-PROCURA            PIC 9(03) VALUE ZEROS.
       01  LIMPOU-TAB                PIC X(01) VALUE SPACES.
       01  LIN                       PIC 9(03).
       01  ja                        pic x(01) value "N".
       01  ind2                      pic 9(02).

       01  TAB-CPF                   PIC 9(11).
       01  TAB-CPF-R REDEFINES TAB-CPF OCCURS 11 TIMES.
           05 TAB-N-CPF              PIC 9(01).

       01  TAB-CGC                   PIC 9(14).
       01  TAB-CGC-R REDEFINES TAB-CGC OCCURS 14 TIMES.
           05 TAB-N-CGC              PIC 9(01).

       01  TAB-CGC2                  PIC 9(16).
       01  TAB-CGC2-R REDEFINES TAB-CGC2 OCCURS 16 TIMES.
           05 TAB-N-CGC2             PIC 9(01).

       PROCEDURE DIVISION.

       010-raiz section.
           perform 020-rotina-inicial
           perform 030-rotina-principal until exit-flag equal 1
           perform 040-rotina-final.
       010-raiz-fim.
          exit.

       020-rotina-inicial section.
           ACCEPT PARAMETROS-W FROM COMMAND-LINE.
           initialize Ds-Control-Block
           initialize Data-block
           move Data-block-version-no
                                    to Ds-Data-Block-Version-No
           move Version-no to Ds-Version-No
           move Ds-Push-Set to Ds-Control
           move "CONCAD"     to Ds-Set-Name
           move spaces      to fun-cobol mensagem.
           initialize tab-90.

           MOVE EMPRESA-W          TO EMP-REC
           MOVE "CAD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CAD010.
           MOVE "MTD019"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-MTD019.
           MOVE "CGD001"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD001.
           MOVE "CGD002"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD002.
           MOVE "CGD003"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD003.
           MOVE "CGD010"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD010.
           MOVE "CGD011"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CGD011.
           MOVE "CRD020"  TO ARQ-REC. MOVE EMPRESA-REF TO PATH-CRD020.
           OPEN INPUT CAD010.
           IF ST-CAD010 <> "00"
              STRING "ERRO ABERTURA CAD010: " ST-CAD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-ERRO
              MOVE 1 TO EXIT-FLAG
              MOVE "N" TO ABRIU-GS.
           OPEN INPUT MTD019.
           IF ST-MTD019 <> "00"
              STRING "ERRO ABERTURA MTD019: " ST-MTD019 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-ERRO
              MOVE 1 TO EXIT-FLAG
              MOVE "N" TO ABRIU-GS.
           OPEN INPUT CGD001.
           IF ST-CGD001 <> "00"
              STRING "ERRO ABERTURA CGD001: " ST-CGD001 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-ERRO
              MOVE 1 TO EXIT-FLAG
              MOVE "N" TO ABRIU-GS.
           OPEN INPUT CGD002.
           IF ST-CGD002 <> "00"
              STRING "ERRO ABERTURA CGD002: " ST-CGD002 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-ERRO
              MOVE 1 TO EXIT-FLAG
              MOVE "N" TO ABRIU-GS.
           OPEN INPUT CGD003.
           IF ST-CGD003 <> "00"
              STRING "ERRO ABERTURA CGD003: " ST-CGD003 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-ERRO
              MOVE 1 TO EXIT-FLAG
              MOVE "N" TO ABRIU-GS.
           OPEN INPUT CGD010.
           IF ST-CGD010 <> "00"
              STRING "ERRO ABERTURA CGD010: " ST-CGD010 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-ERRO
              MOVE 1 TO EXIT-FLAG
              MOVE "N" TO ABRIU-GS.
           OPEN INPUT CGD011.
           IF ST-CGD011 <> "00"
              STRING "ERRO ABERTURA CGD011: " ST-CGD011 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-ERRO
              MOVE 1 TO EXIT-FLAG
              MOVE "N" TO ABRIU-GS.
           OPEN INPUT CRD020.
           IF ST-CRD020 <> "00"
              STRING "ERRO ABERTURA CRD020: " ST-CGD011 INTO MENSAGEM
              MOVE "C" TO TIPO-MSG
              PERFORM EXIBIR-ERRO
              MOVE 1 TO EXIT-FLAG
              MOVE "N" TO ABRIU-GS.
       020-rotina-inicial-fim.
          exit.

       030-rotina-principal section.
          perform Call-Dialog-System
          perform 050-processar-cobol.
       030-rotina-principal-fim.
          exit.

       040-rotina-final section.
           if abriu-gs equal "S"
              move ds-quit-set to ds-control
              perform Call-Dialog-System
           end-if
           close mtd019 cgd001 cgd010 cad010 cgd002 cgd003 cgd011 crd020
           exit program.
           stop run.
       040-rotina-final-fim.
          exit.

       050-processar-cobol section.
           evaluate fun-cobol
                    when "a" perform centralizar
                    when "2" perform 060-pagina-posterior
                    when "3" perform 070-cursor-acima
                    when "4" perform 080-cursor-abaixo
                    when "S" perform 130-sugestao
                    when "A" perform exibir-mensagem
           end-evaluate.
           move spaces to fun-cobol.
       050-processar-cobol-fim.
           exit.

       CENTRALIZAR SECTION.
          move-object-handle win1 handle8
          move handle8 to wHandle
          invoke Window "fromHandleWithClass" using wHandle Window
                 returning janelaPrincipal

          invoke janelaPrincipal "CentralizarNoDesktop".

       060-pagina-posterior section.
           move "N" to exibe-pagina
           evaluate opcao
               when 1 unlock cgd010
                      initialize reg-cgd010
                      move tab-procura to comprador-cg10
                      start cgd010 key is not less comprador-cg10
                      if st-cgd010 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior1
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-003
                         end-if
                      end-if
               when 2 unlock mtd019
                      initialize reg-mtd019
                      move tab-procura to nome-form-mt19
                      start mtd019 key is not less alt2-mt19
                      if st-mtd019 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior2
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-003
                         end-if
                      end-if
               when 3 unlock cgd001
                      initialize reg-cgd001
                      move tab-procura to nome-cg01
                      start cgd001 key is not less nome-cg01
                      if st-cgd001 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior3
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-003
                         end-if
                      end-if
               when 4 unlock crd020
                      initialize reg-crd020
                      move tab-procura to nr-docto-cr20
                      start crd020 key is not less alt7-cr20
                      if st-crd020 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior4
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-003
                         end-if
                      end-if
               when 5 unlock crd020
                      initialize reg-crd020
                      move tab-procura to outro-docto-cr20
                      start crd020 key is not less alt8-cr20
                      if st-crd020 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior5
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-003
                         end-if
                      end-if
               when 6 unlock cgd003
                      initialize reg-cgd003
                      perform montar-cpf
                      move tab-cpf     to cpf-cg03
                      start cgd003 key is not less alt2-cg03
                      if st-cgd003 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior6
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-003
                         end-if
                      end-if
               when 7 unlock cgd002
                      initialize reg-cgd002
                      perform montar-cgc
                      move tab-cgc     to cgc-cg02
                      start cgd002 key is not less alt2-cg02
                      if st-cgd002 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior7
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-003
                         end-if
                      end-if
               when 8 unlock cgd011
                      initialize reg-cgd011
                      perform montar-cgc2
                      move tab-cgc2    to cpf1-cg11
                      start cgd011 key is not less alt2-cg11
                      if st-cgd011 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior8
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-003
                         end-if
                      end-if.
       060-pagina-posterior-fim.
           exit.

       montar-cpf section.
           move zeros to ind2
           perform until ind2 = 11
               add 1 to ind2
               move zeros to tab-n-cpf(ind2)
           end-perform

           if ind = 2
              move tab-pos(1) to tab-n-cpf(1).

           if ind = 3
              move tab-pos(1) to tab-n-cpf(1)
              move tab-pos(2) to tab-n-cpf(2).

           if ind = 4
              move tab-pos(1) to tab-n-cpf(1)
              move tab-pos(2) to tab-n-cpf(2)
              move tab-pos(3) to tab-n-cpf(3).

           if ind = 5
              move tab-pos(1) to tab-n-cpf(1)
              move tab-pos(2) to tab-n-cpf(2)
              move tab-pos(3) to tab-n-cpf(3)
              move tab-pos(4) to tab-n-cpf(4).

           if ind = 6
              move tab-pos(1) to tab-n-cpf(1)
              move tab-pos(2) to tab-n-cpf(2)
              move tab-pos(3) to tab-n-cpf(3)
              move tab-pos(4) to tab-n-cpf(4)
              move tab-pos(5) to tab-n-cpf(5).

           if ind = 7
              move tab-pos(1) to tab-n-cpf(1)
              move tab-pos(2) to tab-n-cpf(2)
              move tab-pos(3) to tab-n-cpf(3)
              move tab-pos(4) to tab-n-cpf(4)
              move tab-pos(5) to tab-n-cpf(5)
              move tab-pos(6) to tab-n-cpf(6).

           if ind = 8
              move tab-pos(1) to tab-n-cpf(1)
              move tab-pos(2) to tab-n-cpf(2)
              move tab-pos(3) to tab-n-cpf(3)
              move tab-pos(4) to tab-n-cpf(4)
              move tab-pos(5) to tab-n-cpf(5)
              move tab-pos(6) to tab-n-cpf(6)
              move tab-pos(7) to tab-n-cpf(7).

           if ind = 9
              move tab-pos(1) to tab-n-cpf(1)
              move tab-pos(2) to tab-n-cpf(2)
              move tab-pos(3) to tab-n-cpf(3)
              move tab-pos(4) to tab-n-cpf(4)
              move tab-pos(5) to tab-n-cpf(5)
              move tab-pos(6) to tab-n-cpf(6)
              move tab-pos(7) to tab-n-cpf(7)
              move tab-pos(8) to tab-n-cpf(8).

           if ind = 10
              move tab-pos(1) to tab-n-cpf(1)
              move tab-pos(2) to tab-n-cpf(2)
              move tab-pos(3) to tab-n-cpf(3)
              move tab-pos(4) to tab-n-cpf(4)
              move tab-pos(5) to tab-n-cpf(5)
              move tab-pos(6) to tab-n-cpf(6)
              move tab-pos(7) to tab-n-cpf(7)
              move tab-pos(8) to tab-n-cpf(8)
              move tab-pos(9) to tab-n-cpf(9).

           if ind = 11
              move tab-pos(1) to tab-n-cpf(1)
              move tab-pos(2) to tab-n-cpf(2)
              move tab-pos(3) to tab-n-cpf(3)
              move tab-pos(4) to tab-n-cpf(4)
              move tab-pos(5) to tab-n-cpf(5)
              move tab-pos(6) to tab-n-cpf(6)
              move tab-pos(7) to tab-n-cpf(7)
              move tab-pos(8) to tab-n-cpf(8)
              move tab-pos(9) to tab-n-cpf(9)
              move tab-pos(10) to tab-n-cpf(10).

           if ind = 12
              move tab-pos(1) to tab-n-cpf(1)
              move tab-pos(2) to tab-n-cpf(2)
              move tab-pos(3) to tab-n-cpf(3)
              move tab-pos(4) to tab-n-cpf(4)
              move tab-pos(5) to tab-n-cpf(5)
              move tab-pos(6) to tab-n-cpf(6)
              move tab-pos(7) to tab-n-cpf(7)
              move tab-pos(8) to tab-n-cpf(8)
              move tab-pos(9) to tab-n-cpf(9)
              move tab-pos(10) to tab-n-cpf(10)
              move tab-pos(11) to tab-n-cpf(11).

       montar-cgc section.
           move zeros to ind2
           perform until ind2 = 14
               add 1 to ind2
               move zeros to tab-n-cgc(ind2)
           end-perform

           if ind = 2
              move tab-pos(1) to tab-n-cgc(1).

           if ind = 3
              move tab-pos(1) to tab-n-cgc(1)
              move tab-pos(2) to tab-n-cgc(2).

           if ind = 4
              move tab-pos(1) to tab-n-cgc(1)
              move tab-pos(2) to tab-n-cgc(2)
              move tab-pos(3) to tab-n-cgc(3).

           if ind = 5
              move tab-pos(1) to tab-n-cgc(1)
              move tab-pos(2) to tab-n-cgc(2)
              move tab-pos(3) to tab-n-cgc(3)
              move tab-pos(4) to tab-n-cgc(4).

           if ind = 6
              move tab-pos(1) to tab-n-cgc(1)
              move tab-pos(2) to tab-n-cgc(2)
              move tab-pos(3) to tab-n-cgc(3)
              move tab-pos(4) to tab-n-cgc(4)
              move tab-pos(5) to tab-n-cgc(5).

           if ind = 7
              move tab-pos(1) to tab-n-cgc(1)
              move tab-pos(2) to tab-n-cgc(2)
              move tab-pos(3) to tab-n-cgc(3)
              move tab-pos(4) to tab-n-cgc(4)
              move tab-pos(5) to tab-n-cgc(5)
              move tab-pos(6) to tab-n-cgc(6).

           if ind = 8
              move tab-pos(1) to tab-n-cgc(1)
              move tab-pos(2) to tab-n-cgc(2)
              move tab-pos(3) to tab-n-cgc(3)
              move tab-pos(4) to tab-n-cgc(4)
              move tab-pos(5) to tab-n-cgc(5)
              move tab-pos(6) to tab-n-cgc(6)
              move tab-pos(7) to tab-n-cgc(7).

           if ind = 9
              move tab-pos(1) to tab-n-cgc(1)
              move tab-pos(2) to tab-n-cgc(2)
              move tab-pos(3) to tab-n-cgc(3)
              move tab-pos(4) to tab-n-cgc(4)
              move tab-pos(5) to tab-n-cgc(5)
              move tab-pos(6) to tab-n-cgc(6)
              move tab-pos(7) to tab-n-cgc(7)
              move tab-pos(8) to tab-n-cgc(8).

           if ind = 10
              move tab-pos(1) to tab-n-cgc(1)
              move tab-pos(2) to tab-n-cgc(2)
              move tab-pos(3) to tab-n-cgc(3)
              move tab-pos(4) to tab-n-cgc(4)
              move tab-pos(5) to tab-n-cgc(5)
              move tab-pos(6) to tab-n-cgc(6)
              move tab-pos(7) to tab-n-cgc(7)
              move tab-pos(8) to tab-n-cgc(8)
              move tab-pos(9) to tab-n-cgc(9).

           if ind = 11
              move tab-pos(1) to tab-n-cgc(1)
              move tab-pos(2) to tab-n-cgc(2)
              move tab-pos(3) to tab-n-cgc(3)
              move tab-pos(4) to tab-n-cgc(4)
              move tab-pos(5) to tab-n-cgc(5)
              move tab-pos(6) to tab-n-cgc(6)
              move tab-pos(7) to tab-n-cgc(7)
              move tab-pos(8) to tab-n-cgc(8)
              move tab-pos(9) to tab-n-cgc(9)
              move tab-pos(10) to tab-n-cgc(10).

           if ind = 12
              move tab-pos(1) to tab-n-cgc(1)
              move tab-pos(2) to tab-n-cgc(2)
              move tab-pos(3) to tab-n-cgc(3)
              move tab-pos(4) to tab-n-cgc(4)
              move tab-pos(5) to tab-n-cgc(5)
              move tab-pos(6) to tab-n-cgc(6)
              move tab-pos(7) to tab-n-cgc(7)
              move tab-pos(8) to tab-n-cgc(8)
              move tab-pos(9) to tab-n-cgc(9)
              move tab-pos(10) to tab-n-cgc(10)
              move tab-pos(11) to tab-n-cgc(11).

           if ind = 13
              move tab-pos(1) to tab-n-cgc(1)
              move tab-pos(2) to tab-n-cgc(2)
              move tab-pos(3) to tab-n-cgc(3)
              move tab-pos(4) to tab-n-cgc(4)
              move tab-pos(5) to tab-n-cgc(5)
              move tab-pos(6) to tab-n-cgc(6)
              move tab-pos(7) to tab-n-cgc(7)
              move tab-pos(8) to tab-n-cgc(8)
              move tab-pos(9) to tab-n-cgc(9)
              move tab-pos(10) to tab-n-cgc(10)
              move tab-pos(11) to tab-n-cgc(11)
              move tab-pos(12) to tab-n-cgc(12).

           if ind = 14
              move tab-pos(1) to tab-n-cgc(1)
              move tab-pos(2) to tab-n-cgc(2)
              move tab-pos(3) to tab-n-cgc(3)
              move tab-pos(4) to tab-n-cgc(4)
              move tab-pos(5) to tab-n-cgc(5)
              move tab-pos(6) to tab-n-cgc(6)
              move tab-pos(7) to tab-n-cgc(7)
              move tab-pos(8) to tab-n-cgc(8)
              move tab-pos(9) to tab-n-cgc(9)
              move tab-pos(10) to tab-n-cgc(10)
              move tab-pos(11) to tab-n-cgc(11)
              move tab-pos(12) to tab-n-cgc(12)
              move tab-pos(13) to tab-n-cgc(13).

           if ind = 15
              move tab-pos(1) to tab-n-cgc(1)
              move tab-pos(2) to tab-n-cgc(2)
              move tab-pos(3) to tab-n-cgc(3)
              move tab-pos(4) to tab-n-cgc(4)
              move tab-pos(5) to tab-n-cgc(5)
              move tab-pos(6) to tab-n-cgc(6)
              move tab-pos(7) to tab-n-cgc(7)
              move tab-pos(8) to tab-n-cgc(8)
              move tab-pos(9) to tab-n-cgc(9)
              move tab-pos(10) to tab-n-cgc(10)
              move tab-pos(11) to tab-n-cgc(11)
              move tab-pos(12) to tab-n-cgc(12)
              move tab-pos(13) to tab-n-cgc(13)
              move tab-pos(14) to tab-n-cgc(14).

       montar-cgc2 section.
           move zeros to ind2
           perform until ind2 = 16
               add 1 to ind2
               move zeros to tab-n-cgc2(ind2)
           end-perform

           if ind = 2
              move tab-pos(1) to tab-n-cgc2(1).

           if ind = 3
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2).

           if ind = 4
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2)
              move tab-pos(3) to tab-n-cgc2(3).

           if ind = 5
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2)
              move tab-pos(3) to tab-n-cgc2(3)
              move tab-pos(4) to tab-n-cgc2(4).

           if ind = 6
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2)
              move tab-pos(3) to tab-n-cgc2(3)
              move tab-pos(4) to tab-n-cgc2(4)
              move tab-pos(5) to tab-n-cgc2(5).

           if ind = 7
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2)
              move tab-pos(3) to tab-n-cgc2(3)
              move tab-pos(4) to tab-n-cgc2(4)
              move tab-pos(5) to tab-n-cgc2(5)
              move tab-pos(6) to tab-n-cgc2(6).

           if ind = 8
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2)
              move tab-pos(3) to tab-n-cgc2(3)
              move tab-pos(4) to tab-n-cgc2(4)
              move tab-pos(5) to tab-n-cgc2(5)
              move tab-pos(6) to tab-n-cgc2(6)
              move tab-pos(7) to tab-n-cgc2(7).

           if ind = 9
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2)
              move tab-pos(3) to tab-n-cgc2(3)
              move tab-pos(4) to tab-n-cgc2(4)
              move tab-pos(5) to tab-n-cgc2(5)
              move tab-pos(6) to tab-n-cgc2(6)
              move tab-pos(7) to tab-n-cgc2(7)
              move tab-pos(8) to tab-n-cgc2(8).

           if ind = 10
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2)
              move tab-pos(3) to tab-n-cgc2(3)
              move tab-pos(4) to tab-n-cgc2(4)
              move tab-pos(5) to tab-n-cgc2(5)
              move tab-pos(6) to tab-n-cgc2(6)
              move tab-pos(7) to tab-n-cgc2(7)
              move tab-pos(8) to tab-n-cgc2(8)
              move tab-pos(9) to tab-n-cgc2(9).

           if ind = 11
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2)
              move tab-pos(3) to tab-n-cgc2(3)
              move tab-pos(4) to tab-n-cgc2(4)
              move tab-pos(5) to tab-n-cgc2(5)
              move tab-pos(6) to tab-n-cgc2(6)
              move tab-pos(7) to tab-n-cgc2(7)
              move tab-pos(8) to tab-n-cgc2(8)
              move tab-pos(9) to tab-n-cgc2(9)
              move tab-pos(10) to tab-n-cgc2(10).

           if ind = 12
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2)
              move tab-pos(3) to tab-n-cgc2(3)
              move tab-pos(4) to tab-n-cgc2(4)
              move tab-pos(5) to tab-n-cgc2(5)
              move tab-pos(6) to tab-n-cgc2(6)
              move tab-pos(7) to tab-n-cgc2(7)
              move tab-pos(8) to tab-n-cgc2(8)
              move tab-pos(9) to tab-n-cgc2(9)
              move tab-pos(10) to tab-n-cgc2(10)
              move tab-pos(11) to tab-n-cgc2(11).

           if ind = 13
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2)
              move tab-pos(3) to tab-n-cgc2(3)
              move tab-pos(4) to tab-n-cgc2(4)
              move tab-pos(5) to tab-n-cgc2(5)
              move tab-pos(6) to tab-n-cgc2(6)
              move tab-pos(7) to tab-n-cgc2(7)
              move tab-pos(8) to tab-n-cgc2(8)
              move tab-pos(9) to tab-n-cgc2(9)
              move tab-pos(10) to tab-n-cgc2(10)
              move tab-pos(11) to tab-n-cgc2(11)
              move tab-pos(12) to tab-n-cgc2(12).

           if ind = 14
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2)
              move tab-pos(3) to tab-n-cgc2(3)
              move tab-pos(4) to tab-n-cgc2(4)
              move tab-pos(5) to tab-n-cgc2(5)
              move tab-pos(6) to tab-n-cgc2(6)
              move tab-pos(7) to tab-n-cgc2(7)
              move tab-pos(8) to tab-n-cgc2(8)
              move tab-pos(9) to tab-n-cgc2(9)
              move tab-pos(10) to tab-n-cgc2(10)
              move tab-pos(11) to tab-n-cgc2(11)
              move tab-pos(12) to tab-n-cgc2(12)
              move tab-pos(13) to tab-n-cgc2(13).

           if ind = 15
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2)
              move tab-pos(3) to tab-n-cgc2(3)
              move tab-pos(4) to tab-n-cgc2(4)
              move tab-pos(5) to tab-n-cgc2(5)
              move tab-pos(6) to tab-n-cgc2(6)
              move tab-pos(7) to tab-n-cgc2(7)
              move tab-pos(8) to tab-n-cgc2(8)
              move tab-pos(9) to tab-n-cgc2(9)
              move tab-pos(10) to tab-n-cgc2(10)
              move tab-pos(11) to tab-n-cgc2(11)
              move tab-pos(12) to tab-n-cgc2(12)
              move tab-pos(13) to tab-n-cgc2(13)
              move tab-pos(14) to tab-n-cgc2(14).

           if ind = 16
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2)
              move tab-pos(3) to tab-n-cgc2(3)
              move tab-pos(4) to tab-n-cgc2(4)
              move tab-pos(5) to tab-n-cgc2(5)
              move tab-pos(6) to tab-n-cgc2(6)
              move tab-pos(7) to tab-n-cgc2(7)
              move tab-pos(8) to tab-n-cgc2(8)
              move tab-pos(9) to tab-n-cgc2(9)
              move tab-pos(10) to tab-n-cgc2(10)
              move tab-pos(11) to tab-n-cgc2(11)
              move tab-pos(12) to tab-n-cgc2(12)
              move tab-pos(13) to tab-n-cgc2(13)
              move tab-pos(14) to tab-n-cgc2(14)
              move tab-pos(15) to tab-n-cgc2(15).

           if ind = 17
              move tab-pos(1) to tab-n-cgc2(1)
              move tab-pos(2) to tab-n-cgc2(2)
              move tab-pos(3) to tab-n-cgc2(3)
              move tab-pos(4) to tab-n-cgc2(4)
              move tab-pos(5) to tab-n-cgc2(5)
              move tab-pos(6) to tab-n-cgc2(6)
              move tab-pos(7) to tab-n-cgc2(7)
              move tab-pos(8) to tab-n-cgc2(8)
              move tab-pos(9) to tab-n-cgc2(9)
              move tab-pos(10) to tab-n-cgc2(10)
              move tab-pos(11) to tab-n-cgc2(11)
              move tab-pos(12) to tab-n-cgc2(12)
              move tab-pos(13) to tab-n-cgc2(13)
              move tab-pos(14) to tab-n-cgc2(14)
              move tab-pos(15) to tab-n-cgc2(15)
              move tab-pos(16) to tab-n-cgc2(16).

       070-cursor-acima section.
           move "N" to exibe-pagina
           evaluate opcao
               when 1 move tab-nome(1) to comprador-cg10
                      start cgd010 key is less than comprador-cg10
                      if st-cgd010 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior1
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-002
                         end-if
                      end-if
               when 2 move tab-nome(1) to nome-form-mt19
                      move tab-contrato(1) to contrato-mt19
                      move tab-curso(1)    to curso-mt19
                      start mtd019 key is less than alt2-mt19
                      if st-mtd019 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior2
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-002
                         end-if
                      end-if
               when 3 move tab-nome(1) to nome-cg01
                      start cgd001 key is less than nome-cg01
                      if st-cgd001 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior3
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-002
                         end-if
                      end-if
               when 4 move tab-documento(1) to nr-docto-cr20
                      start crd020 key is less than alt7-cr20
                      if st-crd020 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior4
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-002
                         end-if
                      end-if
               when 5 move tab-documento(1) to outro-docto-cr20
                      start crd020 key is less than alt8-cr20
                      if st-crd020 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior5
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-002
                         end-if
                      end-if
               when 6 initialize reg-cgd003
                      move tab-documento(1)(1:11) to cpf-cg03
                      move tab-codigo(1)    to codigo-cg03
                      start cgd003 key is less than alt2-cg03
                      if st-cgd003 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior6
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-002
                         end-if
                      end-if
               when 7 initialize reg-cgd002
                      move tab-documento(1)(1:14) to cgc-cg02
                      move tab-codigo(1)    to codigo-cg02
                      start cgd002 key is less than alt2-cg02
                      if st-cgd002 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior7
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-002
                         end-if
                      end-if
               when 8 initialize reg-cgd011
                      move tab-documento(1)(1:16) to cpf1-cg11
                      move tab-codigo(1)    to codigo-cg11
                      start cgd011 key is less than alt2-cg11
                      if st-cgd011 = "00" or "02"
                         move "N" to ja
                         perform 090-ler-pagina-posterior8
                         move 1 to linha
                      else
                         if ja = "N"
                            perform mensagem-002.
       070-cursor-acima-fim.
           exit.

       080-cursor-abaixo section.
           move "N" to exibe-pagina
           evaluate opcao
              when 1 move tab-nome(1) to comprador-cg10
                    start cgd010 key is greater than comprador-cg10
                    if st-cgd010 = "00" or "02"
                       move "N" to ja
                       perform 090-ler-pagina-posterior1
                       move 12 to linha
                    else
                       if ja = "N"
                          perform mensagem-003
                       end-if
                    end-if
              when 2 move tab-nome(1) to nome-form-mt19
                     move tab-contrato(1) to contrato-mt19
                     move tab-curso(1)    to curso-mt19
                     start mtd019 key is greater than alt2-mt19
                     if st-mtd019 = "00" or "02"
                        move "N" to ja
                        perform 090-ler-pagina-posterior2
                        move 12 to linha
                     else
                        if ja = "N"
                           perform mensagem-003
                        end-if
                     end-if
              when 3 move tab-nome(1) to nome-cg01
                     start cgd001 key is greater than nome-cg01
                     if st-cgd001 = "00" or "02"
                        move "N" to ja
                        perform 090-ler-pagina-posterior3
                        move 12 to linha
                     else
                        if ja = "N"
                           perform mensagem-003
                        end-if
                     end-if
              when 4 move tab-documento(1) to nr-docto-cr20
                     start crd020 key is greater than alt7-cr20
                     if st-crd020 = "00" or "02"
                        move "N" to ja
                        perform 090-ler-pagina-posterior4
                        move 12 to linha
                     else
                        if ja = "N"
                           perform mensagem-003
                        end-if
                     end-if
              when 5 move tab-documento(1) to outro-docto-cr20
                     start crd020 key is greater than alt8-cr20
                     if st-crd020 = "00" or "02"
                        move "N" to ja
                        perform 090-ler-pagina-posterior5
                        move 12 to linha
                     else
                        if ja = "N"
                           perform mensagem-003
                        end-if
                     end-if
              when 6 move tab-documento(1)(1:11) to cpf-cg03
                     move tab-codigo(1)          to codigo-cg03
                     start cgd003 key is greater than alt2-cg03
                     if st-cgd003 = "00" or "02"
                        move "N" to ja
                        perform 090-ler-pagina-posterior6
                        move 12 to linha
                     else
                        if ja = "N"
                           perform mensagem-003
                        end-if
                     end-if
              when 7 move tab-documento(1)(1:14) to cgc-cg02
                     move tab-codigo(1)          to codigo-cg02
                     start cgd002 key is greater than alt2-cg02
                     if st-cgd002 = "00" or "02"
                        move "N" to ja
                        perform 090-ler-pagina-posterior7
                        move 12 to linha
                     else
                        if ja = "N"
                           perform mensagem-003
                        end-if
                     end-if
              when 8 move tab-documento(1)(1:16) to cpf1-cg11
                     move tab-codigo(1)          to codigo-cg11
                     start cgd011 key is greater than alt2-cg11
                     if st-cgd011 = "00" or "02"
                        move "N" to ja
                        perform 090-ler-pagina-posterior8
                        move 12 to linha
                     else
                        if ja = "N"
                           perform mensagem-003.

       080-cursor-abaixo-fim.
           exit.

       090-ler-pagina-posterior1 section.
           move "N" to limpou-tab
           move 0 to lin
           read cgd010 next
           if st-cgd010 = "02" or "9a"
              move "00" to st-cgd010
           end-if
           perform until st-cgd010 <> "00" or lin = 12
              if limpou-tab equal "N"
                 initialize tab-90
                 move "S" to limpou-tab exibe-pagina
              end-if
              add  1                            to lin
              move spaces                       to tab-documento(lin)
              move comprador-cg10               to tab-nome(lin)
              move cod-compl-cg10               to tab-codigo(lin)
              move zeros                        to tab-contrato(lin)
                                                   tab-curso(lin)
              move cod-compl-cg10               to cod-compl-cg11
              read cgd011 invalid key
                   move spaces                  to tab-cidade(lin)
                   move spaces                  to tab-estado(lin)
              not invalid key
                   move cidade1-cg11            to cidade
                   read cad010 invalid key
                       move spaces              to nome-cid
                       move spaces              to uf-cid
                   end-read
                   move nome-cid                to tab-cidade(lin)
                   move uf-cid                  to tab-estado(lin)
              end-read
              read cgd010 next
              if st-cgd010 = "02" or "9a"
                 move "00" to st-cgd010.
       090-ler-pagina-posterior1-fim.
           exit.

       090-ler-pagina-posterior2 section.
           move "N" to limpou-tab
           move 0 to lin
           read mtd019 next
           if st-mtd019 = "02" or "9a"
              move "00" to st-mtd019
           end-if
           perform until st-mtd019 <> "00" or lin = 12
              if limpou-tab equal "N"
                 initialize tab-90
                 move "S" to limpou-tab exibe-pagina
              end-if
              add  1                            to lin
              move spaces                       to tab-documento(lin)
              move nome-form-mt19               to tab-nome(lin)
              move album-mt19                   to tab-codigo(lin)
              move contrato-mt19                to tab-contrato(lin)
              move curso-mt19                   to tab-curso(lin)
              move cidade-mt19                  to cidade
              read cad010 invalid key
                   move spaces                  to nome-cid
                   move spaces                  to uf-cid
              end-read
              move nome-cid                     to tab-cidade(lin)
              move uf-cid                       to tab-estado(lin)
              read mtd019 next
              if st-mtd019 = "02" or "9a"
                 move "00" to st-mtd019.
       090-ler-pagina-posterior2-fim.
           exit.

       090-ler-pagina-posterior3 section.
           move "N" to limpou-tab
           move 0 to lin
           read cgd001 next
           if st-cgd001 = "02" or "9a"
              move "00" to st-cgd001
           end-if
           perform until st-cgd001 <> "00" or lin = 12
              if limpou-tab equal "N"
                 initialize tab-90
                 move "S" to limpou-tab exibe-pagina
              end-if
              add  1                            to lin
              move spaces                       to tab-documento(lin)
              move nome-cg01                    to tab-nome(lin)
              move codigo-cg01                  to tab-codigo(lin)
              move zeros                        to tab-contrato(lin)
                                                   tab-curso(lin)
              if t-pesfis-cg01 = 1
                 move codigo-cg01               to codigo-cg03
                 read cgd003 invalid key
                      move spaces               to tab-cidade(lin)
                      move spaces               to tab-estado(lin)
                 not invalid key
                      move cidade-res-cg03      to cidade
                      read cad010 invalid key
                           move spaces          to nome-cid
                           move spaces          to uf-cid
                      end-read
                      move nome-cid             to tab-cidade(lin)
                      move uf-cid               to tab-estado(lin)
                 end-read
              else
                 move codigo-cg01               to codigo-cg02
                 read cgd002 invalid key
                      move spaces               to tab-cidade(lin)
                      move spaces               to tab-estado(lin)
                 not invalid key
                      move cidade1-cg02         to cidade
                      read cad010 invalid key
                           move spaces          to nome-cid
                           move spaces          to uf-cid
                      end-read
                      move nome-cid             to tab-cidade(lin)
                      move uf-cid               to tab-estado(lin)
                 end-read
              end-if
              read cgd001 next
              if st-cgd001 = "02" or "9a"
                 move "00" to st-cgd001.
       090-ler-pagina-posterior3-fim.
           exit.

       090-ler-pagina-posterior4 section.
           move "N" to limpou-tab
           move 0 to lin
           read crd020 next
           if st-crd020 = "02" or "9a"
              move "00" to st-crd020
           end-if
           perform until st-crd020 <> "00" or lin = 12
              if limpou-tab equal "N"
                 initialize tab-90
                 move "S" to limpou-tab exibe-pagina
              end-if
              add  1                            to lin
              move nr-docto-cr20                to tab-documento(lin)
              move cod-compl-cr20               to cod-compl-cg10
              move codigo-cg10                  to tab-codigo(lin)
              read cgd010 invalid key
                   move spaces                  to tab-nome(lin)
                   move zeros                   to tab-contrato(lin)
                                                   tab-curso(lin)
                   move spaces                  to tab-cidade(lin)
                                                   tab-estado(lin)
              not invalid key
                   move comprador-cg10           to tab-nome(lin)
                   move cod-compl-cg10           to tab-codigo(lin)
                   move zeros                    to tab-contrato(lin)
                                                    tab-curso(lin)
                   move cod-compl-cg10           to cod-compl-cg11
                   read cgd011 invalid key
                        move spaces              to tab-cidade(lin)
                        move spaces              to tab-estado(lin)
                   not invalid key
                        move cidade1-cg11        to cidade
                        read cad010 invalid key
                            move spaces          to nome-cid
                            move spaces          to uf-cid
                        end-read
                        move nome-cid            to tab-cidade(lin)
                        move uf-cid              to tab-estado(lin)
                   end-read
              end-read
              read crd020 next
              if st-crd020 = "02" or "9a"
                 move "00" to st-crd020.
       090-ler-pagina-posterior4-fim.
           exit.

       090-ler-pagina-posterior5 section.
           move "N" to limpou-tab
           move 0 to lin
           read crd020 next
           if st-crd020 = "02" or "9a"
              move "00" to st-crd020
           end-if
           perform until st-crd020 <> "00" or lin = 12
              if limpou-tab equal "N"
                 initialize tab-90
                 move "S" to limpou-tab exibe-pagina
              end-if
              add  1                            to lin
              move outro-docto-cr20             to tab-documento(lin)
              move cod-compl-cr20               to cod-compl-cg10
              move codigo-cg10                  to tab-codigo(lin)
              read cgd010 invalid key
                   move spaces                  to tab-nome(lin)
                   move zeros                   to tab-contrato(lin)
                                                   tab-curso(lin)
                   move spaces                  to tab-cidade(lin)
                                                   tab-estado(lin)
              not invalid key
                   move comprador-cg10           to tab-nome(lin)
                   move cod-compl-cg10           to tab-codigo(lin)
                   move zeros                    to tab-contrato(lin)
                                                    tab-curso(lin)
                   move cod-compl-cg10           to cod-compl-cg11
                   read cgd011 invalid key
                        move spaces              to tab-cidade(lin)
                        move spaces              to tab-estado(lin)
                   not invalid key
                        move cidade1-cg11        to cidade
                        read cad010 invalid key
                            move spaces          to nome-cid
                            move spaces          to uf-cid
                        end-read
                        move nome-cid            to tab-cidade(lin)
                        move uf-cid              to tab-estado(lin)
                   end-read
              end-read
              read crd020 next
              if st-crd020 = "02" or "9a"
                 move "00" to st-crd020.
       090-ler-pagina-posterior5-fim.
           exit.

       090-ler-pagina-posterior6 section.
           move "N" to limpou-tab
           move 0 to lin
           read cgd003 next
           if st-cgd003 = "02" or "9a"
              move "00" to st-cgd003
           end-if
           perform until st-cgd003 <> "00" or lin = 12
              if limpou-tab equal "N"
                 initialize tab-90
                 move "S" to limpou-tab exibe-pagina
              end-if
              add  1                            to lin
              move cpf-cg03                     to masc-cpf
              move masc-cpf                     to tab-documento(lin)
              move codigo-cg03                  to codigo-cg01
              read cgd001 invalid key
                   move spaces                  to nome-cg01
              end-read
              move nome-cg01                    to tab-nome(lin)
              move cidade-res-cg03      to cidade
              read cad010 invalid key
                   move spaces          to nome-cid
                   move spaces          to uf-cid
              end-read
              move nome-cid             to tab-cidade(lin)
              move uf-cid               to tab-estado(lin)
              move codigo-cg03          to tab-codigo(lin)
              move zeros                to tab-contrato(lin)
                                           tab-curso(lin)
              read cgd003 next
              if st-cgd003 = "02" or "9a"
                 move "00" to st-cgd003.
       090-ler-pagina-posterior6-fim.
           exit.

       090-ler-pagina-posterior7 section.
           move "N" to limpou-tab
           move 0 to lin
           read cgd002 next
           if st-cgd002 = "02" or "9a"
              move "00" to st-cgd002
           end-if
           perform until st-cgd002 <> "00" or lin = 12
              if limpou-tab equal "N"
                 initialize tab-90
                 move "S" to limpou-tab exibe-pagina
              end-if
              add  1                            to lin
              move cgc-cg02                     to masc-cgc
              move masc-cgc                     to tab-documento(lin)
              move codigo-cg02                  to codigo-cg01
              read cgd001 invalid key
                   move spaces                  to nome-cg01
              end-read
              move nome-cg01                    to tab-nome(lin)
              move cidade1-cg02                 to cidade
              read cad010 invalid key
                   move spaces          to nome-cid
                   move spaces          to uf-cid
              end-read
              move nome-cid             to tab-cidade(lin)
              move uf-cid               to tab-estado(lin)
              move codigo-cg02          to tab-codigo(lin)
              move zeros                to tab-contrato(lin)
                                           tab-curso(lin)
              read cgd002 next
              if st-cgd002 = "02" or "9a"
                 move "00" to st-cgd002.
       090-ler-pagina-posterior7-fim.
           exit.

       090-ler-pagina-posterior8 section.
           move "N" to limpou-tab
           move 0 to lin
           read cgd011 next
           if st-cgd011 = "02" or "9a"
              move "00" to st-cgd011
           end-if
           perform until st-cgd011 <> "00" or lin = 12
              if limpou-tab equal "N"
                 initialize tab-90
                 move "S" to limpou-tab exibe-pagina
              end-if
              add  1                            to lin
              move cpf1-cg11                    to masc-cgc2
              move masc-cgc2                    to tab-documento(lin)
              move cod-compl-cg11               to cod-compl-cg10
              read cgd010 invalid key
                   move spaces                  to nome-cg01
              end-read
              move cod-compl-cg10               to tab-codigo(lin)
              move zeros                        to tab-contrato(lin)
                                                   tab-curso(lin)
              move comprador-cg10               to tab-nome(lin)
              move cidade1-cg11                 to cidade
              read cad010 invalid key
                   move spaces          to nome-cid
                   move spaces          to uf-cid
              end-read
              move nome-cid             to tab-cidade(lin)
              move uf-cid               to tab-estado(lin)
              read cgd011 next
              if st-cgd011 = "02" or "9a"
                 move "00" to st-cgd011.
       090-ler-pagina-posterior8-fim.
           exit.

       130-sugestao section.
           move "Sugestão Inexistente" to mensagem
           move "C" to tipo-msg
           perform exibir-erro.
       130-sugestao-fim.
           exit.

       exibir-registro section.
           move "EXIBIR-TUDO" to ds-procedure
           perform call-dialog-system.
       exibir-registro-fim.
           exit.

      *----------------------------------------------------*
      * Mensagens Provenientes do Dialog System            *
      *----------------------------------------------------*
       exibir-mensagem section.
           if ds-msg = "001"
              move "S" to ja
              perform mensagem-003.
       exibir-mensagem-fim.
           exit.

       mensagem-002 section.
           move "S" to ja
           move "C" to tipo-msg
           move "Inicio do Arquivo!" to mensagem
           perform exibir-erro.
       mensagem-002-fim.
           exit.

       mensagem-003 section.
           move "S" to ja
           move "C" to tipo-msg
           move "Fim do Arquivo!" to mensagem
           perform exibir-erro.
       mensagem-003-fim.
           exit.

       exibir-erro section.
           move 1 to flag-critica
           move spaces to resp-msg
           call   "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel "MENSAGEM".
       exibir-erro-fim.
           exit.

       Call-Dialog-System SECTION.
          CALL dialog-system USING Ds-Control-Block,
                                   Data-Block
          IF NOT Ds-No-Error
              MOVE Ds-Error-Code TO Display-error-no
              DISPLAY "DS ERROR NO:   "  Display-error-no
              DISPLAY "Error Details(1) :   "  Display-Details-1
              DISPLAY "Error Details(2) :   "  Display-Details-2
              PERFORM 040-ROTINA-FINAL
          END-IF.

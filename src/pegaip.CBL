       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEGAIP.
       AUTHOR. ALFREDO SAVIOLLI NETO.
       DATE-WRITTEN. 11-06-2009.
      *FUNÇÃO: PROGRAMA PRA PEGAR IP, MAC ADDRESS E TIPO DE IP

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
         DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           select ip assign to "ip.txt"
                     organization is line sequential
                     access mode is sequential
                     file status is fs-ip.

       DATA DIVISION.
       FILE SECTION.

       fd ip.
       01 reg-ip                   pic x(200).

       WORKING-STORAGE SECTION.
       01 fs-ip                    pic x(02).
       01 mensagem                 pic x(200).
       01 tipo-msg                 pic x.
       01 resp-msg                 pic x.

       01 W1-COMANDO.
          05 WS1-COMANDO PIC X(100).

       01 WPAR-CALLEXT.
          05 WRESUL            PIC 9(02) COMP-X.
          05 WFUNC             PIC 9(02) COMP-X VALUE 35.
          05 W-PARAM.
             10 W-PARACESSO    PIC 9(02) COMP-X VALUE ZEROS.

       LINKAGE SECTION.
       01 lnkIP                pic x(20).
       01 lnkMac               pic x(20).
       01 lnkTipoIp            pic x(20).
       01 lnkHost              pic x(20).

       PROCEDURE DIVISION USING lnkIp lnkMac lnkTipoIp lnkHost.
       MAIN-PROCESS SECTION.
           MOVE SPACES TO WS1-COMANDO
           STRING "IPCONFIG /ALL >IP.TXT" INTO WS1-COMANDO
           DISPLAY WS1-COMANDO UPON COMMAND-LINE
           CALL X"91" USING WRESUL WFUNC W-PARAM.

           initialize lnkIp lnkMac lnkTipoIP lnkHost

           open input ip

           if fs-ip <> "00"
              move "Não achei o IP" to mensagem
              move "C" to tipo-msg
              perform 140-exibir-mensagem
           else
              perform until fs-ip = "10"
                   read ip next at end
                        move "10" to fs-ip
                   not at end
                        evaluate reg-ip(9:37)
                          when "Nome do host . . . . . . . . . . . : "
                               move reg-ip(46:12) to lnkHost
                          when "Endereço físico . . . . . . . . . . :"
                               if lnkIp = spaces
                                  move reg-ip(47:17) to lnkMac
                               end-if
                          when "Endereço IP . . . . . . . . . . . . :"
                               move reg-ip(47:15)    to lnkIp
                          when "DHCP ativado. . . . . . . . . . . . :"
                               evaluate reg-ip(47:3)
                                   when "Sim"  move "Dinamico" to
                                                    lnkTipoIp
                                   when other  move "Estatico" to
                                                    lnkTipoIp
                               end-evaluate
                        end-evaluate
                   end-read
              end-perform
           end-if

           close ip
           EXIT PROGRAM.


       140-exibir-mensagem section.
           move    spaces to resp-msg.
           call    "MENSAGEM" using tipo-msg resp-msg mensagem
           cancel  "MENSAGEM".
           move spaces to mensagem.


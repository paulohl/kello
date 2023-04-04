       identification division.
       program-id.                 extenso.
       author.                     Isaias.

       date-written.               29-Nov-92.
       date-compiled.
       security.                   *---------------------------------*
                                   * Programa que busca o extenso de *
                                   * um Determinado Valor            *   
                                   *---------------------------------*
       environment division.
       configuration section.
       special-names.              decimal-point is       comma
                                   console       is         crt.
       input-output section.
       file-control.

           select   arqext         assign        to  arquivo-arqext
                                   organization  is     indexed
                                   access  mode  is      random
                                   record   key  is   ext-chave 
                                   lock    mode  is   automatic
                                   with    lock  on      record
                                   file  status  is fs-arqext.
      /
       data division.
       file section.

      *----------------------------------------------------------------*
      *      FD DO ARQUIVO DE EXTENSO                                  *
      *----------------------------------------------------------------*
       fd  arqext.

       01  reg-ext.
           05  ext-chave.
               10  ext-numer       pic 9(03).
           05  ext-descricao       pic x(35).
           05  ext-descricao-r     redefines    ext-descricao.
               10  ext-descr       pic x(01)    occurs 35 times.
           05  ext-num-car         pic 9(02).

       working-storage section.
       77  w-aaa                   pic 9(03) value zeros.
       77  w-cru                   pic 9(03) value zeros.
       77  w-xxx                   pic 9(02) value zeros.
       77  w-cp                    pic 9(03) value zeros.
       77  w-ct                    pic 9(02) value zeros.
       77  w-ca                    pic 9(02) value zeros.
       77  w-numer                 pic 9(03) value zeros.
       77  mensagem                pic x(200) value spaces.
       77  tipo-msg                pic x(001) value spaces.
       77  resp-msg                pic x(001) value spaces.
       77  fs-arqext               pic 9(02) value zeros.
       77  arquivo-arqext          pic x(30) value
           "arqext".
       01  eof                     pic 9(01) value zeros.

      *    copy   "work-pad.men".

       01  reg-aux.
           05  w-aux               pic x(001) occurs 50 times.
     


       linkage section.

       01  link-extenso.
           05  link-descricao.
               10  link-descr      pic x(01)         occurs 200  times.
           05  link-valor          pic 9(12)v99.
           05  link-valor-r        redefines         link-valor.
               10  link-vlr-cruz   pic 9(03)         occurs   4  times.
               10  link-vlr-cent   pic 9(02).
      /
       procedure division   using link-extenso.
      *----------------------------------------------------------------*
      *    CONTROLE DO PROGRAMA                                        *
      *----------------------------------------------------------------*
       0010-controle.

           perform  0020-inicio   thru   0020-inicio-exit.
           perform  0030-meio     thru   0030-meio-exit until eof = 1.
           perform  0040-fim      thru   0040-fim-exit.

       0010-controle-exit.
           exit   program.
      /
      *----------------------------------------------------------------*
      *    ROTINA DE INICIO                                            *
      *----------------------------------------------------------------*
       0020-inicio.  

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
           open    input     arqext.

           IF FS-ARQEXT NOT EQUAL "00" AND "05" AND "35"
                   move "Erro De Abertura...ARQEXT" to mensagem
                   move "C"  to  tipo-msg
                   perform  exibir-mensagem
                   move     1               to          eof.

       0020-inicio-exit.
           exit.

       EXIBIR-MENSAGEM SECTION.
           MOVE SPACES TO RESP-MSG
           CALL   "MENSAGEM" USING TIPO-MSG RESP-MSG MENSAGEM
           CANCEL "MENSAGEM".
       EXIBIR-MENSAGEM-FIM.
           EXIT.

      /
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0030-meio.

           move     all "="            to     link-descricao.
           move     1                  to                eof.

           if       link-valor         =                   0
                    go                 to     0030-meio-exit.

           if       link-valor         =                1,00
                    perform 0050-hum-cruzeiro thru 0050-exit
                    go                 to     0030-meio-exit.

           if       link-valor         =                0,01
                    perform  0060-hum-centavo thru 0060-exit
                    go                 to     0030-meio-exit.

           move     zeros              to              w-xxx.
 
           perform  0070-rotina        thru        0070-exit
                    until              w-aaa       =       4.

           if       link-vlr-cruz (03)     =    zeros    and
                    link-vlr-cruz (04)     =           zeros
               if   link-vlr-cruz (01)     =    zeros    and
                    link-vlr-cruz (02)     =           zeros 
                    next                            sentence
              else
                    perform  0080-letra-de thru    0080-exit.

           if       link-vlr-cruz (01)     =    zeros    and
                    link-vlr-cruz (02)     =    zeros    and
                    link-vlr-cruz (03)     =    zeros    and
                    link-vlr-cruz (04)     =           zeros
                    next                            sentence
           else
                    perform  0090-cruzeiros thru   0090-exit.
       
           if       link-vlr-cent          =           zeros
                    go                     to 0030-meio-exit.

           if       w-cp      not          =           zeros
                    perform   0100-letra-e thru    0100-exit.

           move     zeros                  to        w-numer.
           move     link-vlr-cent          to        w-numer.
           move     w-numer                to      ext-numer.

           perform  0250-leitura-arqext    thru    0250-exit.
           move     zeros                  to           w-ct.

           perform  0110-mover             thru    0110-exit
                    until      w-ct        =     ext-num-car.

           perform  0120-centavos          thru    0120-exit.

       0030-meio-exit.
           exit.
      /
      *----------------------------------------------------------------*
      *    ROTINA FINAL                                                *
      *----------------------------------------------------------------*
       0040-fim.

           close    arqext.

       0040-fim-exit.
           exit.
      /
      *----------------------------------------------------------------*
      *    Move Extenso quando for (Um Cruzeiro)                       *
      *----------------------------------------------------------------*
       0050-hum-cruzeiro.

           move     "HUM REAL"         to            reg-aux.
           move     zeros              to               w-ct.

           perform  0130-especial      thru        0130-exit
                    until              w-ct        =       8.
       
       0050-exit.
           exit.
      *----------------------------------------------------------------*
      *    Move Extenso quando for (Um Sentavo)                        *
      *----------------------------------------------------------------*
       0060-hum-centavo. 

           move     "HUM CENTAVO"      to            reg-aux.
           move     zeros              to               w-ct.

           perform  0130-especial      thru        0130-exit
                    until    w-ct      =                  11.
      
       0060-exit.
           exit.
      /
      *----------------------------------------------------------------*
      *    Rotina para Buscar Extenso de um determinado Valor          *
      *----------------------------------------------------------------*
       0070-rotina.

           add      1                  to  w-ca w-aaa  w-xxx.

           if       link-vlr-cruz (w-ca)       =       zeros
                    go                 to          0070-exit.

           if       w-ca        not    =               1 and
                    link-vlr-cruz (w-ca)     not = zeros and
                    w-cp                     not = zeros
                    perform  0140-testes     thru  0140-exit.
        
           move     link-vlr-cruz (w-ca)     to    ext-numer.
           perform  0250-leitura-arqext      thru  0250-exit.

           move     zeros              to               w-ct.

           perform  0110-mover         thru        0110-exit
                    until       w-ct   =         ext-num-car.

           if       w-ca               =                   1
               if   link-vlr-cruz (w-ca)         =         1
                    perform   0150-bilhao   thru   0150-exit
               else
                    perform   0160-bilhoes  thru   0160-exit.

           if       w-ca               =                   2
               if   link-vlr-cruz (w-ca)         =         1
                    perform   0170-milhao   thru   0170-exit
               else
                    perform   0180-milhoes  thru   0180-exit.
       
           if       w-ca               =                   3
                    perform   0190-mil      thru   0190-exit.

       0070-exit.
           exit.
      /
      *----------------------------------------------------------------*
      *    Move a Letra (DE) para o Extenso                            *
      *----------------------------------------------------------------*
       0080-letra-de.

           move     " DE"              to            reg-aux.
           move     zeros              to               w-ct.

           perform  0130-especial      thru        0130-exit
                    until              w-ct         =      3.

       0080-exit.
           exit.
      /
      *----------------------------------------------------------------*
      *    Move (CRUZEIROS) para o Extenso                             *
      *----------------------------------------------------------------*
       0090-cruzeiros.

           move     " REAIS"           to            reg-aux.
           move     zeros              to               w-ct.

           perform  0130-especial      thru        0130-exit
                    until    w-ct      =                   6.

       0090-exit.
           exit.
      *----------------------------------------------------------------*
      *    Move (E) para o Extenso                                     *
      *----------------------------------------------------------------*
       0100-letra-e.  
     
           move     " E"               to            reg-aux.
           move     zeros              to               w-ct.

           perform  0130-especial      thru        0130-exit
                    until              w-ct        =       3.

       0100-exit.
           exit.
      *----------------------------------------------------------------*
      *    Move (E) para o Extenso                                     *
      *----------------------------------------------------------------*
       0110-mover.    

           add      1                  to    w-cp       w-ct.

           move     ext-descr (w-ct)   to    link-descr(w-cp).

       0110-exit.
           exit.
      *----------------------------------------------------------------*
      *    Move (CENTAVOS) para o Extenso                              *
      *----------------------------------------------------------------*
       0120-centavos. 
      
           move     " CENTAVOS"        to            reg-aux.
           move     zeros              to               w-ct.

           perform  0130-especial      thru        0130-exit
                    until              w-ct        =       9.

       0120-exit.
           exit.
      *----------------------------------------------------------------*
      *    Move Descricao da Work para Link.                           *
      *----------------------------------------------------------------*
       0130-especial.

           add      1                  to       w-ct    w-cp.

           move     w-aux (w-ct)       to  link-descr (w-cp).

       0130-exit.
           exit.
      *----------------------------------------------------------------*
      *    Se for final de palavra move a Letra "E" ou uma ","         *
      *----------------------------------------------------------------*
       0140-testes.

           if       w-ca               =                   4
               if   link-vlr-cruz (01) =           zeros and
                    link-vlr-cruz (02) =           zeros and
                    link-vlr-cruz (03) =               zeros
                    next                            sentence
              else
                    perform  0100-letra-e  thru    0100-exit
           else
                    perform  0200-virgula  thru    0200-exit.

       0140-exit.
           exit.
      *----------------------------------------------------------------*
      *    Move (BILHAO) para a Link.                                  *
      *----------------------------------------------------------------*
       0150-bilhao.

           move     " BILHAO"          to            reg-aux.
           move     zeros              to               w-ct.

           perform  0130-especial      thru        0130-exit
                    until              w-ct        =       7.

       0150-exit.
           exit.
      *----------------------------------------------------------------*
      *    Move (BILHOES) para a Link.                                 *
      *----------------------------------------------------------------*
       0160-bilhoes.

           move     " BILHOES"         to            reg-aux.
           move     zeros              to               w-ct.

           perform  0130-especial      thru        0130-exit
                    until              w-ct        =       8.

       0160-exit.
           exit.
      *----------------------------------------------------------------*
      *    Move (MILHAO)  para a Link.                                 *
      *----------------------------------------------------------------*
       0170-milhao. 

           move     " MILHAO"          to            reg-aux.
           move     zeros              to               w-ct.

           perform  0130-especial      thru        0130-exit
                    until              w-ct        =       7.

       0170-exit.
           exit.
      *----------------------------------------------------------------*
      *    Move (MILHOES) para a Link.                                 *
      *----------------------------------------------------------------*
       0180-milhoes.

           move     " MILHOES"         to            reg-aux.
           move     zeros              to               w-ct.

           perform  0130-especial      thru        0130-exit
                    until              w-ct        =       8.

       0180-exit.
           exit.
      *----------------------------------------------------------------*
      *    Move (MIL) para a Link.                                     *
      *----------------------------------------------------------------*
       0190-mil. 

           move     " MIL"             to            reg-aux.
           move     zeros              to               w-ct.

           perform  0130-especial      thru        0130-exit
                    until              w-ct        =       4.

       0190-exit.
           exit.
      *----------------------------------------------------------------*
      *    Move (MIL) para a Link.                                     *
      *----------------------------------------------------------------*
       0200-virgula.

           move     ", "               to            reg-aux.
           move     zeros              to               w-ct.

           perform  0130-especial      thru        0130-exit
                    until              w-ct        =       2.

       0200-exit.
           exit.
      /
      *----------------------------------------------------------------*
      *    Le o arquivo de Extenso                                     *
      *----------------------------------------------------------------*
       0250-leitura-arqext.

           read     arqext                 invalid      key
                    display "Existe VALOR SEM EXTENSO, RETIRE DO MOVTO"
                    stop " "
                    move all "CANCELADO "  to link-descricao
                    move      187          to           w-cp
                    go                     to      0250-exit.

           if fs-arqext not equal "00"
                    move  "arqext"      to mensagem
                    move  "C"           to tipo-msg
                    perform   exibir-mensagem
                    go       to          0250-leitura-arqext.

       0250-exit.
           exit.
      /


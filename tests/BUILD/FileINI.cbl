      $set mfoo
       class-id. FileINI inherits from base.

       special-names.
          call-convention 66 is winapi.


       object section.
       class-control.
           color is class "color".

       class-object.
       object-storage section.
       77 osArquivo    pic x(255) value spaces.
       77 osSecao      pic x(255) value spaces.
       77 osChave      pic x(255) value spaces.
       77 osTamanho    pic 9(09) comp-5 value zeros.
       77 osConteudo   pic x(255) value spaces.

       01  DLL-WINAPI USAGE IS PROCEDURE-POINTER.

      *---------------------------------------------------------------
       method-id. "LerINI".
       local-storage section.
       77 lsRetorno pic 9(09) comp-5.
       linkage section.
       77 lnkArquivo pic x(255).
       77 lnkSecao pic x(255).
       77 lnkChave pic x(255).
       77 lnkConteudo pic x(255).
       procedure division using lnkArquivo lnkSecao
                                lnkChave returning lnkConteudo.

           set dll-winapi to entry "kernel32.dll"

           string lnkSecao delimited by  "           "
                  x"00" delimited by size
                  into osSecao
           end-string
           string lnkChave delimited by "            "
                  x"00" delimited by size
                  into osChave
           end-string
           string lnkArquivo delimited by "                            "
                  x"00" delimited by size
                  into osArquivo
           end-string
           move 255 to osTamanho
           call winapi "GetPrivateProfileStringA" using
                       by reference osSecao
                       by reference osChave
                       by reference x"00"
                       by reference osConteudo
                       by value osTamanho
                       by reference osArquivo
                       returning lsRetorno
           end-call.
           if  lsRetorno not equal 0
               unstring osConteudo delimited by x"00"
                        into lnkConteudo
               end-unstring
           end-if.

       exit method
       end method "LerINI".

      *---------------------------------------------------------------
       method-id. "GravarINI".
       local-storage section.
       77 lsRetorno pic 9(09) comp-5.

       01 ws-hora-ini                  pic 9(08).
       01 filler redefines ws-hora-ini.
          03 ws-ho-ini                 pic 9(02).
          03 ws-mi-ini                 pic 9(02).
          03 ws-se-ini                 pic 9(02).
          03 ws-ms-ini                 pic 9(02).

       01 ws-hora-fim                  pic 9(08).
       01 filler redefines ws-hora-fim.
          03 ws-ho-fim                 pic 9(02).
          03 ws-mi-fim                 pic 9(02).
          03 ws-se-fim                 pic 9(02).
          03 ws-ms-fim                 pic 9(02).

       01 mascara-hora                 pic ZZbZZbZZbZZ.

       linkage section.
       77 lnkArquivo pic x(255).
       77 lnkSecao pic x(255).
       77 lnkChave pic x(255).
       77 lnkConteudo pic x(255).
       procedure division using lnkArquivo lnkSecao
                                lnkChave lnkConteudo.

           set dll-winapi  to entry    "kernel32.dll"

           string lnkSecao delimited by "    "
                  x"00" delimited by size
                  into osSecao
           end-string

           string lnkChave delimited by "    "
                  x"00" delimited by size
                  into osChave
           end-string

           string lnkConteudo x"00" delimited by size
                  into osConteudo
           end-string

           string lnkArquivo delimited by "                        "
                  x"00" delimited by size
                  into osArquivo
           end-string


           move 255 to osTamanho
           call winapi "WritePrivateProfileStringA" using
                       by reference osSecao
                       by reference osChave
                       by reference osConteudo
                       by reference osArquivo returning lsRetorno
           end-call


       exit method
       end method "GravarINI".

      *---------------------------------------------------------------
       method-id. "SetarCaminho".
       linkage section.
       77 lnkArquivo pic x(255).

       procedure division using lnkArquivo.

           move spaces to osArquivo
           string lnkArquivo delimited by "     "
                  x"00" delimited by size into osArquivo

       exit method
       end method "SetarCaminho".

      *---------------------------------------------------------------
       method-id. "Ler".
       local-storage section.
       77 lsRetorno pic 9(09) comp-5.
       linkage section.
       77 lnkConteudo pic x(255).
       procedure division returning lnkConteudo.

           set dll-winapi to entry "kernel32.dll"

           move 255 to osTamanho
           call winapi "GetPrivateProfileStringA" using
                       by reference osSecao
                       by reference osChave
                       by reference x"00"
                       by reference osConteudo
                       by value osTamanho
                       by reference osArquivo
                       returning lsRetorno
           end-call.
           if lsRetorno not equal 0
              unstring osConteudo delimited by x"00" into lnkConteudo
              end-unstring
           end-if.

       exit method
       end method "Ler".

      *---------------------------------------------------------------
       method-id. "mensagem".
       local-storage section.
       77 lsRetorno pic 9(09) comp-5.
       linkage section.
       77 lnkArquivo pic x(255).
       77 lnkSecao pic x(255).
       77 lnkChave pic x(255).
       77 lnkConteudo pic x(255).

       procedure division using lnkArquivo lnkSecao lnkChave
                                returning lnkConteudo.

           initialize osSecao osChave osArquivo
           move lnkSecao to osSecao
           string lnkChave delimited by "    "
                  x"00" delimited by size into osChave
           string lnkArquivo delimited by "     "
                  x"00" delimited by size into osArquivo

           invoke self "Ler" returning lnkConteudo

       exit method
       end method "mensagem".

      *---------------------------------------------------------------
       method-id. "cor".
       local-storage section.
       01 lsRGB.
          02 lsR pic 99 comp-5.
          02 lsG pic 99 comp-5.
          02 lsB pic 99 comp-5.

       01 lsXRGB.
          02 lsXR pic x(03).
          02 filler pic x(01).
          02 lsXG pic x(03).
          02 filler pic x(01).
          02 lsXB pic x(03).

       77 lsConteudo pic x(255).

       linkage section.
       77 lnkSecao pic x(255).
       77 lnkChave pic x(255).
       77 lnkCor object reference.

       procedure division using lnkSecao lnkChave lnkCor.

           initialize osSecao osChave lsConteudo
           move lnkSecao to osSecao
           string lnkChave delimited by "    "
                  x"00" delimited by size into osChave

           invoke self "Ler" returning lsConteudo

           move lsConteudo to lsXRGB
           if lsConteudo = spaces
              if lnkChave = "FOREGROUND"
                 move 0 to lsR lsG
                 move 1 to lsB
              else
                 move 255 to lsR lsG lsB
              end-if
              invoke color "RGB" using lsR lsG lsB returning lnkCor
           else
              move function numval(lsXR) to lsR
              move function numval(lsXG) to lsG
              move function numval(lsXB) to lsB
              invoke color "RGB" using lsR lsG lsB returning lnkCor
           end-if

       exit method
       end method "cor".

       end class-object.


      *---------------------------------------------------------------

       method-id. "GravarCorVencidos".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkConteudo lnkusu.

           initialize lsArquivo lsSecao lsChave
           string lnk-path-sis delimited by "*" lnk-usuario
                  delimited by "  " "-resumo.INI"  X"00" into
                  lsArquivo

           string "CORES"  X"00" into lsSecao
           string "VENCIDOS" X"00" into lsChave
           string lnkConteudo X"00" into lnkConteudo

           invoke self "GravarINI" using lsArquivo lsSecao lsChave
                                         lnkConteudo

       exit method
       end method "GravarCorVencidos".

       method-id. "LerCorVencidos".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkusu returning lnkConteudo.
           initialize lsArquivo lsSecao lsChave
           string lnk-path-sis delimited by "*" lnk-usuario
                  delimited by "  " "-resumo.INI" X"00" into
                  lsArquivo
           string "CORES"  X"00" into lsSecao
           string "VENCIDOS" X"00" into lsChave

           invoke self "LerINI" using lsArquivo lsSecao lsChave
                                returning  lnkConteudo

       exit method
       end method "LerCorVencidos".

      *---------------------------------------------------------------

       method-id. "GravarCorAVencer".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkConteudo lnkusu.
           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis delimited by "*" lnk-usuario
                  delimited by "  " "-resumo.INI" X"00" into lsArquivo

           string "CORES"  X"00" into lsSecao
           string "AVENCER" X"00" into lsChave
           string lnkConteudo X"00" into lnkConteudo

           invoke self "GravarINI" using lsArquivo lsSecao lsChave
                                         lnkConteudo

       exit method
       end method "GravarCorAVencer".

       method-id. "LerCorAVencer".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkusu returning lnkConteudo.
           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis delimited by "*" lnk-usuario
                  delimited by "  " "-resumo.INI" X"00" into lsArquivo

           string "CORES"  X"00" into lsSecao
           string "AVENCER" X"00" into lsChave

           invoke self "LerINI" using lsArquivo lsSecao lsChave
                                returning  lnkConteudo

       exit method
       end method "LerCorAVencer".

      *---------------------------------------------------------------

       method-id. "GravarCorFonteVencidos".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkConteudo lnkusu.

           initialize lsArquivo lsSecao lsChave
           string lnk-path-sis delimited by "*" lnk-usuario
                  delimited by "  " "-resumo.INI"  X"00" into
                  lsArquivo

           string "CORES-FONTE"  X"00" into lsSecao
           string "VENCIDOS" X"00" into lsChave
           string lnkConteudo X"00" into lnkConteudo

           invoke self "GravarINI" using lsArquivo lsSecao lsChave
                                         lnkConteudo

       exit method
       end method "GravarCorFonteVencidos".

       method-id. "LerCorFonteVencidos".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkusu returning lnkConteudo.
           initialize lsArquivo lsSecao lsChave
           string lnk-path-sis delimited by "*" lnk-usuario
                  delimited by "  " "-resumo.INI" X"00" into
                  lsArquivo
           string "CORES-FONTE"  X"00" into lsSecao
           string "VENCIDOS" X"00" into lsChave

           invoke self "LerINI" using lsArquivo lsSecao lsChave
                                returning  lnkConteudo

       exit method
       end method "LerCorFonteVencidos".

      *---------------------------------------------------------------

       method-id. "GravarCorFonteAVencer".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkConteudo lnkusu.
           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis delimited by "*" lnk-usuario
                  delimited by "  " "-resumo.INI" X"00" into lsArquivo

           string "CORES-FONTE"  X"00" into lsSecao
           string "AVENCER" X"00" into lsChave
           string lnkConteudo X"00" into lnkConteudo

           invoke self "GravarINI" using lsArquivo lsSecao lsChave
                                         lnkConteudo

       exit method
       end method "GravarCorFonteAVencer".

       method-id. "LerCorFonteAVencer".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkusu returning lnkConteudo.
           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis delimited by "*" lnk-usuario
                  delimited by "  " "-resumo.INI" X"00" into lsArquivo

           string "CORES-FONTE"  X"00" into lsSecao
           string "AVENCER" X"00" into lsChave

           invoke self "LerINI" using lsArquivo lsSecao lsChave
                                returning  lnkConteudo

       exit method
       end method "LerCorFonteAVencer".

      *---------------------------------------------------------------
       method-id. "CaminhoEmpresa".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).

       linkage section.
       77 lnkCaminho pic x(255).
       procedure division returning lnkCaminho.

          initialize ls-area-call-ini

          move "\programa\caminhoEmpresa.ini"  to ls-arq-ini
          move "Empresa"              to ls-secao-ini
          move "Caminho"              to ls-chave-ini
          invoke self "LerINI"
                          using ls-Arq-ini ls-secao-ini ls-Chave-ini
                          returning lnkCaminho

       exit method
       end method "CaminhoEmpresa".
      *---------------------------------------------------------------
       method-id. "nomeEmpresa".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).

       linkage section.
       77 lnkNome pic x(20).
       procedure division returning lnkNome.

          initialize ls-area-call-ini

          move "\programa\caminhoEmpresa.ini" to ls-arq-ini
          move "Empresa"              to ls-secao-ini
          move "Nome"                 to ls-chave-ini
          invoke self "LerINI"
                          using ls-Arq-ini ls-secao-ini ls-Chave-ini
                          returning lnkNome

       exit method
       end method "nomeEmpresa".

      *---------------------------------------------------------------
      *---------------------------------------------------------------
       method-id. "gravarCaminhoImagem".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).
       linkage section.
       77 lnkChaveListView pic x(255).
       77 lnkSecao pic x(255).
       77 lnkCaminho pic x(255).
       01 lnkusu.
          copy "usuario.cpy".
       procedure division using lnkChaveListView lnkSecao lnkCaminho
                                lnkusu.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\caminhoImagem.ini"
            delimited by "*" into ls-arq-ini
          string lnkSecao  X"00"  delimited by "  "   into ls-secao-ini
          string lnkChaveListView delimited by "  " into ls-chave-ini
      *   string "EMPRESA" X"00" into ls-chave-ini

          invoke self "GravarINI"
                          using ls-Arq-ini ls-secao-ini ls-chave-ini
                                lnkCaminho

       exit method
       end method "gravarCaminhoImagem".
      *---------------------------------------------------------------
       method-id. "caminhoImagem".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).
       linkage section.
       77 lnkChaveListView pic x(255).
       77 lnkSecao pic x(255).
       77 lnkCaminho pic x(255).
       01 lnkusu.
          copy "usuario.cpy".
       procedure division using lnkChaveListView lnkSecao lnkusu
                          returning lnkCaminho.

          initialize  ls-area-call-ini

          string lnk-path-sis"\arquivosINI\caminhoImagem.ini"
            delimited by "*" into ls-arq-ini
          string lnkSecao  X"00"  delimited by "  "   into ls-secao-ini
          string lnkChaveListView delimited by "  " into ls-chave-ini
      *   string "EMPRESA" X"00" into ls-chave-ini

          invoke self "lerINI"
                          using ls-Arq-ini ls-secao-ini ls-chave-ini
                      returning lnkCaminho

       exit method
       end method "caminhoImagem".
      *---------------------------------------------------------------
       method-id. "gravarAnimacao".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).
       linkage section.
       01 lnkusu.
          copy "usuario.cpy".
       77 lnkChaveListView pic x(255).
       77 lnkValor pic x(255).
       procedure division using lnkusu lnkChaveListView lnkValor.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\animacaoTela.ini"
              delimited by "*" into ls-arq-ini
      *   string "EMPRESA" X"00"   into ls-secao-ini
          string lnk-usuario      delimited by "  " into ls-secao-ini
          string lnkChaveListView delimited by "  " into ls-chave-ini

          invoke self "GravarINI"
                          using ls-Arq-ini ls-secao-ini lnkChaveListView
                                lnkValor

       exit method
       end method "gravarAnimacao".
      *---------------------------------------------------------------
       method-id. "Animacao".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).
       linkage section.
       01 lnkusu.
          copy "usuario.cpy".
       77 lnkChaveListView pic x(255).
       77 lnkValor pic x(255).
       procedure division using lnkusu lnkChaveListView
                      returning lnkValor.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\animacaoTela.ini"
              delimited by "*" into ls-arq-ini
      *   string "EMPRESA" X"00"   into ls-secao-ini
          string lnk-usuario delimited by "  "  into ls-secao-ini
          string lnkChaveListView delimited by "  " into ls-chave-ini
          invoke self "lerINI"
                          using ls-Arq-ini ls-secao-ini lnkChaveListView
                      returning lnkValor


       exit method
       end method "Animacao".
      *---------------------------------------------------------------
      *---------------------------------------------------------------

       method-id. "GravarUltimoUsuario".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       77 lsConteudo pic x(255).
       linkage section.
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkusu.
           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis "\arquivosINI\" delimited by "*"
                  "ultimoUsuario.INI" X"00"
             into lsArquivo

           string "USUARIO"   X"00" into lsSecao
           string "ID"        X"00" into lsChave
           string lnk-usuario X"00" into lsConteudo

           invoke self "GravarINI" using lsArquivo lsSecao lsChave
                                         lsConteudo

       exit method
       end method "GravarUltimoUsuario".

       method-id. "LerUltimoUsuario".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkusu returning lnkConteudo.
           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis "\arquivosINI\" delimited by "*"
                  "ultimoUsuario.INI" X"00"
             into lsArquivo
           string "USUARIO"   X"00" into lsSecao
           string "ID"        X"00" into lsChave

           invoke self "LerINI" using lsArquivo lsSecao lsChave
                                returning  lnkConteudo

       exit method
       end method "LerUltimoUsuario".

      *---------------------------------------------------------------

       method-id. "GravarParametrosCompra".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkConteudo lnkusu.

           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis "\arquivosINI\"  delimited by "*"
                  "parametrosCompra.INI" X"00" into lsArquivo

           string "PARAMETROS"  X"00" into lsSecao
           string "GERAL" X"00" into lsChave
           string lnkConteudo X"00" into lnkConteudo

           invoke self "GravarINI" using lsArquivo lsSecao lsChave
                                         lnkConteudo

       exit method
       end method "GravarParametrosCompra".


       method-id. "LerParametrosCompra".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkusu returning lnkConteudo.
           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis "\arquivosINI\"  delimited by "*"
                  "parametrosCompra.INI" X"00" into lsArquivo

           string "PARAMETROS"  X"00" into lsSecao
           string "GERAL" X"00" into lsChave

           invoke self "LerINI" using lsArquivo lsSecao lsChave
                                returning  lnkConteudo

       exit method
       end method "LerParametrosCompra".

      *---------------------------------------------------------------
      *---------------------------------------------------------------

       method-id. "GravarParametrosCompra2".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkConteudo lnkusu.

           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis "\arquivosINI\"  delimited by "*"
                  "parametrosCompra2.INI" X"00" into lsArquivo

           string "PARAMETROS"  X"00" into lsSecao
           string "GERAL" X"00" into lsChave
           string lnkConteudo X"00" into lnkConteudo

           invoke self "GravarINI" using lsArquivo lsSecao lsChave
                                         lnkConteudo

       exit method
       end method "GravarParametrosCompra2".


       method-id. "LerParametrosCompra2".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkusu returning lnkConteudo.
           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis "\arquivosINI\"  delimited by "*"
                  "parametrosCompra2.INI" X"00" into lsArquivo

           string "PARAMETROS"  X"00" into lsSecao
           string "GERAL" X"00" into lsChave

           invoke self "LerINI" using lsArquivo lsSecao lsChave
                                returning  lnkConteudo

       exit method
       end method "LerParametrosCompra2".


      ******************************************************************
      ******************************************************************

       method-id. "GravarForeground".
       77 lSArquivo  pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       01 lnkusu.
          copy usuario.cpy.
       77 lnkConteudo pic x(255).
       77 lnkSecao    pic x(255).

       procedure division using lnkusu lnkConteudo lnkSecao.

           initialize lsArquivo lsChave

           string lnk-path-sis delimited by "*" lnk-usuario
              delimited by "  " "-ParametrosCor.INI" X"00"
                                           into lsArquivo
             string lnkSecao        X"00"  into lnkSecao
             string "COR"           X"00"  into lsChave
             string lnkConteudo     X"00"  into lnkConteudo

           invoke self "GravarINI" using lsArquivo
                                         lnkSecao
                                         lsChave
                                         lnkConteudo

       exit method
       end method "GravarForeground".

      ******************************************************************

       method-id. "LerForeground".
       77 lSArquivo  pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       01 lnkusu.
          copy usuario.cpy.
       77 lnkSecao    pic x(255).
       77 lnkConteudo pic x(255).
       procedure division using lnkusu lnkSecao returning lnkConteudo.

           initialize lsArquivo lsChave
           string lnk-path-sis delimited by "*" lnk-usuario
              delimited by "  " "-ParametrosCor.INI" X"00"
                                           into lsArquivo

             string lnkSecao        X"00"  into lnkSecao
             string "COR"           X"00"  into lsChave
             string lnkConteudo     X"00"  into lnkConteudo

             invoke self "LerINI" using lsArquivo
                                        lnkSecao
                                        lsChave
                              returning lnkConteudo

       exit method
       end method "LerForeground".

      ******************************************************************
      ******************************************************************

       method-id. "GravarBackground".
       77 lSArquivo  pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       01 lnkusu.
          copy usuario.cpy.
       77 lnkConteudo pic x(255).
       77 lnkSecao pic x(255).
       procedure division using lnkusu lnkConteudo lnkSecao.

           initialize lsArquivo lsChave

           string lnk-path-sis delimited by "*" lnk-usuario
              delimited by "  " "-ParametrosCor.INI" X"00"
                                           into lsArquivo
             string lnkSecao        X"00"  into lnkSecao
             string "COR"           X"00"  into lsChave
             string lnkConteudo     X"00"  into lnkConteudo

           invoke self "GravarINI" using lsArquivo
                                         lnkSecao
                                         lsChave
                                         lnkConteudo

       exit method
       end method "GravarBackground".

      ******************************************************************

       method-id. "LerBackground".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       77 lnkSecao pic x(255).
       procedure division using lnkusu lnkSecao returning lnkConteudo.

           initialize lsArquivo lsChave
           string lnk-path-sis delimited by "*" lnk-usuario
              delimited by "  " "-ParametrosCor.INI" X"00"
                                           into lsArquivo

             string lnkSecao        X"00"  into lnkSecao
             string "COR"           X"00"  into lsChave
             string lnkConteudo     X"00"  into lnkConteudo

             invoke self "LerINI" using lsArquivo
                                        lnkSecao
                                        lsChave
                              returning lnkConteudo

       exit method
       end method "LerBackground".

      ******************************************************************
      ******************************************************************

       method-id. "GravarFontePedidon".
       77 lSArquivo  pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       01 lnkusu.
          copy usuario.cpy.
       77 lnkConteudo pic x(255).
       77 lnkSecao pic x(255).
       procedure division using lnkusu lnkConteudo lnkSecao.

           initialize lsArquivo lsChave

           string lnk-path-sis delimited by "*" lnk-usuario
              delimited by "  " "-ParametrosCor.INI" X"00"
                                           into lsArquivo
             string lnkSecao        X"00"  into lnkSecao
             string "VALOR"         X"00"  into lsChave
             string lnkConteudo     X"00"  into lnkConteudo

           invoke self "GravarINI" using lsArquivo
                                         lnkSecao
                                         lsChave
                                         lnkConteudo

       exit method
       end method "GravarFontePedidon".

      ******************************************************************

       method-id. "LerFontePedidon".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       77 lnkSecao pic x(255).
       procedure division using lnkusu lnkSecao returning lnkConteudo.

           initialize lsArquivo lsChave
           string lnk-path-sis delimited by "*" lnk-usuario
              delimited by "  " "-ParametrosCor.INI" X"00"
                                           into lsArquivo

             string lnkSecao        X"00"  into lnkSecao
             string "VALOR"         X"00"  into lsChave
             string lnkConteudo     X"00"  into lnkConteudo

             invoke self "LerINI" using lsArquivo
                                        lnkSecao
                                        lsChave
                              returning lnkConteudo

       exit method
       end method "LerFontePedidon".

      ******************************************************************
      *---------------------------------------------------------------
       method-id. "gravarColunas".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).
       linkage section.
       01 lnkusu.
          copy "usuario.cpy".
       77 lnkChaveListView     pic x(255).
       77 lnkValor             pic x(255).
       procedure division using lnkusu lnkChaveListView lnkValor.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\Colunas.ini"
              delimited by "*" into ls-arq-ini
          string lnk-usuario      delimited by "  " into ls-secao-ini
          string lnkChaveListView delimited by "  " into ls-chave-ini

          invoke self "GravarINI"
                          using ls-Arq-ini ls-secao-ini lnkChaveListView
                                lnkValor

       exit method
       end method "gravarColunas".
      *---------------------------------------------------------------
       method-id. "Colunas".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).
       linkage section.
       01 lnkusu.
          copy "usuario.cpy".
       77 lnkChaveListView pic x(255).
       77 lnkValor pic x(255).
       procedure division using lnkusu lnkChaveListView
                      returning lnkValor.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\Colunas.ini"
              delimited by "*" into ls-arq-ini
          string lnk-usuario delimited by "  "  into ls-secao-ini
          string lnkChaveListView delimited by "  " into ls-chave-ini
          invoke self "lerINI"
                          using ls-Arq-ini ls-secao-ini lnkChaveListView
                      returning lnkValor


       exit method
       end method "Colunas".
      *---------------------------------------------------------------
      *---------------------------------------------------------------
       method-id. "GravarTaxa".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).
       linkage section.
       01 lnkusu.
          copy "usuario.cpy".
       77 lnkValor pic 9(09)V99.
       procedure division using lnkusu lnkValor.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\TaxaEntrega.ini"
              delimited by "*" into ls-arq-ini

          MOVE "EMPRESA"           to ls-secao-ini
          MOVE "TAXA DE ENTREGA"   to ls-chave-ini

          invoke self "GravarINI" using ls-Arq-ini
                                        ls-secao-ini
                                        ls-chave-ini
                                        lnkValor
       exit method
       end method "GravarTaxa".
      *---------------------------------------------------------------
      *---------------------------------------------------------------
       method-id. "TaxaDeEntrega".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).
       linkage section.
       01 lnkusu.
          copy "usuario.cpy".
       77 lnkValor pic 9(09)V99.
       procedure division using lnkusu returning lnkValor.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\TaxaEntrega.ini"
              delimited by "*" into ls-arq-ini

          MOVE "EMPRESA"           to ls-secao-ini
          MOVE "TAXA DE ENTREGA"   to ls-chave-ini

          invoke self "lerINI" using ls-Arq-ini
                                     ls-secao-ini
                                     ls-chave-ini
                                     returning ls-val-ini
          string ls-val-ini(1:09) ls-val-ini(10:2)
          into lnkvalor

       exit method
       end method "TaxaDeEntrega".

      ******************************************************************
      ******************************************************************

       method-id. "GravarColunasFavoritas".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).

       linkage section.
       01 lnkusu.
          copy usuario.cpy.
       01 LNKSECAO     PIC X(255).
       01 LSNOMECOLUNA PIC X(255).
       01 LSVALOR      PIC 99.

       PROCEDURE DIVISION USING LNKUSU
                                LNKSECAO
                                LSNOMECOLUNA
                                LSVALOR.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\" delimited by "*"
                 lnk-usuario "-ColunasFavoritas.ini" delimited by "  "
            into ls-arq-ini
          string lnksecao x"00"     delimited by "  " into ls-secao-ini
          string lsnomecoluna x"00" delimited by "  " into ls-chave-ini
          string lsvalor x"00"                        into ls-val-ini

          invoke self "GravarINI"
                          using ls-Arq-ini
                                ls-secao-ini
                                ls-chave-ini
                                ls-val-ini



       exit method
       end method "GravarColunasFavoritas".


       method-id. "RetornarColunasFavoritas".
       LOCAL-STORAGE SECTION.
       01  LS-AREA-CALL-INI.
           03 LS-ARQ-INI              PIC X(255).
           03 LS-SECAO-INI            PIC X(255).
           03 LS-CHAVE-INI            PIC X(255).
           03 LS-VAL-INI              PIC X(255).
           03 FILLER                  PIC X(100).

       LINKAGE SECTION.
       01 LNKUSU.
          COPY USUARIO.CPY.
       01 LNKSECAO     PIC X(255).
       01 LSNOMECOLUNA PIC X(255).
       01 LSVALOR      PIC 99.

       PROCEDURE DIVISION USING LNKUSU
                                LNKSECAO
                                LSNOMECOLUNA RETURNING LSVALOR.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\" delimited by "*"
                 lnk-usuario "-ColunasFavoritas.ini" delimited by "  "
            into ls-arq-ini
          string lnksecao x"00"     delimited by "  " into ls-secao-ini
          string lsnomecoluna x"00" delimited by "  " into ls-chave-ini


          invoke self "LerINI" using   ls-Arq-ini
                                       ls-secao-ini
                                       ls-chave-ini returning ls-val-ini

          move function numval(ls-val-ini) to lsvalor


       exit method
       end method "RetornarColunasFavoritas".

      ******************************************************************
      ******************************************************************

       method-id. "GravarCorFavoritaZebrar".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).

       linkage section.
       01 lnkusu.
          copy usuario.cpy.
       01 LNKSECAO     PIC X(255).
       01 LNKCHAVE     PIC X(255).
       01 LSVALOR      PIC X(009).

       PROCEDURE DIVISION USING LNKUSU
                                LNKSECAO
                                LNKCHAVE
                                LSVALOR.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\" delimited by "*"
                 lnk-usuario "-CorFavoritaZebrar.ini" delimited by "  "
            into ls-arq-ini
          string lnksecao x"00"     delimited by "  "  into ls-secao-ini
          string lnkchave x"00"     delimited by "  "  into ls-chave-ini
          string lsvalor x"00"                         into ls-val-ini

          invoke self "GravarINI"
                          using ls-Arq-ini
                                ls-secao-ini
                                ls-chave-ini
                                ls-val-ini



       exit method
       end method "GravarCorFavoritaZebrar".


       method-id. "RetornarCorFavoritaZebrar".
       LOCAL-STORAGE SECTION.
       01  LS-AREA-CALL-INI.
           03 LS-ARQ-INI              PIC X(255).
           03 LS-SECAO-INI            PIC X(255).
           03 LS-CHAVE-INI            PIC X(255).
           03 LS-VAL-INI              PIC X(255).
           03 FILLER                  PIC X(100).

       LINKAGE SECTION.
       01 LNKUSU.
          COPY USUARIO.CPY.
       01 LNKSECAO     PIC X(255).
       01 LNKCHAVE     PIC X(255).
       01 LSVALOR      PIC 9(009).

       PROCEDURE DIVISION USING LNKUSU
                                LNKSECAO
                                LNKCHAVE
                                RETURNING LSVALOR.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\" delimited by "*"
                 lnk-usuario "-CorFavoritaZebrar.ini" delimited by "  "
            into ls-arq-ini
          string lnksecao x"00"     delimited by "  "  into ls-secao-ini
          string lnkchave x"00"     delimited by "  "  into ls-chave-ini

          invoke self "LerINI" using   ls-Arq-ini
                                       ls-secao-ini
                                       ls-chave-ini returning ls-val-ini

          move function numval(ls-val-ini) to lsvalor


       exit method
       end method "RetornarCorFavoritaZebrar".
      ******************************************************************
      ******************************************************************


       method-id. "GravarFonteFavorita".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).

       linkage section.
       01 lnkusu.
          copy usuario.cpy.
       01 LNKSECAO     PIC X(255).
       01 LNKCHAVE     PIC X(255).
       01 LSVALOR      PIC X(090).

       PROCEDURE DIVISION USING LNKUSU
                                LNKSECAO
                                LNKCHAVE
                                LSVALOR.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\" delimited by "*"
                 lnk-usuario "-FonteFavorita.ini" delimited by "  "
            into ls-arq-ini
          string lnksecao x"00"     delimited by "  "  into ls-secao-ini
          string lnkchave x"00"     delimited by "  "  into ls-chave-ini
          string lsvalor x"00"                         into ls-val-ini

          invoke self "GravarINI"
                          using ls-Arq-ini
                                ls-secao-ini
                                ls-chave-ini
                                ls-val-ini



       exit method
       end method "GravarFonteFavorita".


       method-id. "trazerFonteFavorita".
       LOCAL-STORAGE SECTION.
       01  LS-AREA-CALL-INI.
           03 LS-ARQ-INI              PIC X(255).
           03 LS-SECAO-INI            PIC X(255).
           03 LS-CHAVE-INI            PIC X(255).
           03 LS-VAL-INI              PIC X(255).
           03 FILLER                  PIC X(100).

       LINKAGE SECTION.
       01 LNKUSU.
          COPY USUARIO.CPY.
       01 LNKSECAO     PIC X(255).
       01 LNKCHAVE     PIC X(255).
       01 LSVALOR      PIC X(090).

       PROCEDURE DIVISION USING LNKUSU
                                LNKSECAO
                                LNKCHAVE
                                RETURNING LSVALOR.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\" delimited by "*"
                 lnk-usuario "-FonteFavorita.ini" delimited by "  "
            into ls-arq-ini
          string lnksecao x"00"     delimited by "  "  into ls-secao-ini
          string lnkchave x"00"     delimited by "  "  into ls-chave-ini

          invoke self "LerINI" using   ls-Arq-ini
                                       ls-secao-ini
                                       ls-chave-ini returning ls-val-ini

          move ls-val-ini(1:90) to lsvalor


       exit method
       end method "trazerFonteFavorita".
      ******************************************************************
      ******************************************************************

       method-id. "GravarTamanhoColunasFavoritas".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).

       linkage section.
       01 lnkusu.
          copy usuario.cpy.
       01 LNKSECAO     PIC X(255).
       01 LSNOMECOLUNA PIC X(255).
       01 LSVALOR      PIC 9(09).

       PROCEDURE DIVISION USING LNKUSU
                                LNKSECAO
                                LSNOMECOLUNA
                                LSVALOR.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\" delimited by "*"
                 lnk-usuario "-TamanhoColunasFavoritas.ini"
                 delimited by "  " into ls-arq-ini
          string lnksecao x"00"     delimited by "  " into ls-secao-ini
          string lsnomecoluna x"00" delimited by "  " into ls-chave-ini
          string lsvalor x"00"                        into ls-val-ini

          invoke self "GravarINI"
                          using ls-Arq-ini
                                ls-secao-ini
                                ls-chave-ini
                                ls-val-ini



       exit method
       end method "GravarTamanhoColunasFavoritas".


       method-id. "RetornarTamanhoColunasFavoritas".
       LOCAL-STORAGE SECTION.
       01  LS-AREA-CALL-INI.
           03 LS-ARQ-INI              PIC X(255).
           03 LS-SECAO-INI            PIC X(255).
           03 LS-CHAVE-INI            PIC X(255).
           03 LS-VAL-INI              PIC X(255).
           03 FILLER                  PIC X(100).

       LINKAGE SECTION.
       01 LNKUSU.
          COPY USUARIO.CPY.
       01 LNKSECAO     PIC X(255).
       01 LSNOMECOLUNA PIC X(255).
       01 LSVALOR      PIC 9(009).

       PROCEDURE DIVISION USING LNKUSU
                                LNKSECAO
                                LSNOMECOLUNA RETURNING LSVALOR.

          initialize ls-area-call-ini

          string lnk-path-sis"\arquivosINI\" delimited by "*"
                 lnk-usuario "-TamanhoColunasFavoritas.ini"
                 delimited by "  " into ls-arq-ini
          string lnksecao x"00"     delimited by "  " into ls-secao-ini
          string lsnomecoluna x"00" delimited by "  " into ls-chave-ini


          invoke self "LerINI" using   ls-Arq-ini
                                       ls-secao-ini
                                       ls-chave-ini returning ls-val-ini

          move function numval(ls-val-ini) to lsvalor


       exit method
       end method "RetornarTamanhoColunasFavoritas".

      ******************************************************************
      ******************************************************************
      *---------------------------------------------------------------

       method-id. "GravarCorConpro".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       77 lnkChave    pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkConteudo lnkChave lnkusu.
           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis "\arquivosINI\" delimited by "*"
                  lnk-usuario delimited by "  " "-conpro.INI" X"00"
             into lsArquivo

           string "COR"  X"00" into lsSecao
           string lnkChave X"00" delimited by "  " into lsChave
           string lnkConteudo X"00" into lnkConteudo

           invoke self "GravarINI" using lsArquivo lsSecao lsChave
                                         lnkConteudo

       exit method
       end method "GravarCorConpro".

       method-id. "LerCorConpro".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       77 lnkChave    pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkusu lnkChave returning lnkConteudo.
           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis "\arquivosINI\" delimited by "*"
                  lnk-usuario delimited by "  " "-conpro.INI" X"00"
             into lsArquivo

           string "COR"  X"00" into lsSecao
           string lnkChave X"00" delimited by "  " into lsChave

           invoke self "LerINI" using lsArquivo lsSecao lsChave
                                returning  lnkConteudo

       exit method
       end method "LerCorConpro".

      *---------------------------------------------------------------
      *---------------------------------------------------------------

       method-id. "GravarCorFonteConpro".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       77 lnkChave    pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkConteudo lnkChave lnkusu.
           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis "\arquivosINI\" delimited by "*"
                  lnk-usuario delimited by "  " "-conpro.INI" X"00"
             into lsArquivo

           string "FONTE"  X"00" into lsSecao
           string lnkChave X"00" delimited by "  " into lsChave
           string lnkConteudo X"00" into lnkConteudo

           invoke self "GravarINI" using lsArquivo lsSecao lsChave
                                         lnkConteudo

       exit method
       end method "GravarCorFonteConpro".

      *---------------------------------------------------------------
      *---------------------------------------------------------------

       method-id. "LerCorFonteConpro".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       77 lnkChave    pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkusu lnkChave returning lnkConteudo.
           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis "\arquivosINI\" delimited by "*"
                  lnk-usuario delimited by "  " "-conpro.INI" X"00"
             into lsArquivo

           string "FONTE"  X"00" into lsSecao
           string lnkChave X"00" delimited by "  " into lsChave

           invoke self "LerINI" using lsArquivo lsSecao lsChave
                                returning  lnkConteudo

       exit method
       end method "LerCorFonteConpro".

      *---------------------------------------------------------------
      *---------------------------------------------------------------
       method-id. "gravarOrdemColunaListView".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).
       linkage section.
       77 lnkChaveListView pic x(255).
       77 lnkOrdem pic x(255).
       77 lnkSecao pic x(255).
       01 lnk-dados.
          copy "usuario.cpy".
       procedure division using lnkChaveListView lnkOrdem lnkSecao
                                lnk-dados.

          initialize ls-area-call-ini

          string lnk-path-sis"arquivosINI\" lnk-usuario
             "-ordemColunaListView.ini" delimited by "*" into ls-arq-ini
          string lnkSecao delimited by "  "  into ls-secao-ini
          string lnkChaveListView delimited by "  " into ls-chave-ini

          invoke self "GravarINI"
                          using ls-Arq-ini ls-secao-ini lnkChaveListView
                                lnkOrdem

       exit method
       end method "gravarOrdemColunaListView".

      *---------------------------------------------------------------
      *---------------------------------------------------------------

       method-id. "ordemColunaListView".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).
       linkage section.
       77 lnkChaveListView pic x(255).
       77 lnkSecao pic x(255).
       77 lnkOrdem pic x(255).
       01 lnk-dados.
          copy "usuario.cpy".
       procedure division using lnkChaveListView lnkSecao lnk-dados
                      returning lnkOrdem.

          initialize ls-area-call-ini

          string lnk-path-sis"arquivosINI\" lnk-usuario
             "-ordemColunaListView.ini" delimited by "*" into ls-arq-ini
          string lnkSecao delimited by "  " into ls-secao-ini
          string lnkChaveListView delimited by "  " into ls-chave-ini
          invoke self "LerINI"
                          using ls-Arq-ini ls-secao-ini lnkChaveListView
                          returning lnkOrdem

       exit method
       end method "ordemColunaListView".

      *---------------------------------------------------------------
      *---------------------------------------------------------------
      *---------------------------------------------------------------
      *---------------------------------------------------------------
       method-id. "gravarIndiceColunaListView".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).
       linkage section.
       77 lnkChaveListView pic x(255).
       77 lnkOrdem pic x(255).
       77 lnkSecao pic x(255).
       01 lnk-dados.
          copy "usuario.cpy".
       procedure division using lnkChaveListView lnkOrdem lnkSecao
                                lnk-dados.

          initialize ls-area-call-ini

          string lnk-path-sis"arquivosINI\" lnk-usuario
             "-indiceColunaListView.ini"
             delimited by "*" into ls-arq-ini

          string lnkSecao delimited by "  "  into ls-secao-ini
          string lnkChaveListView delimited by "  " into ls-chave-ini

          invoke self "GravarINI"
                          using ls-Arq-ini ls-secao-ini lnkOrdem
                                lnkChaveListView


       exit method
       end method "gravarIndiceColunaListView".

      *---------------------------------------------------------------
      *---------------------------------------------------------------

       method-id. "IndiceColunaListView".
       local-storage section.
       01  ls-area-call-ini.
           03  ls-arq-ini              pic x(255).
           03  ls-secao-ini            pic x(255).
           03  ls-chave-ini            pic x(255).
           03  ls-val-ini              pic x(255).
           03  filler                  pic x(100).
       linkage section.
       77 lnkChaveListView pic x(255).
       77 lnkSecao pic x(255).
       77 lnkOrdem pic x(255).
       01 lnk-dados.
          copy "usuario.cpy".
       procedure division using lnkChaveListView lnkSecao lnk-dados
                          returning lnkOrdem.

          initialize ls-area-call-ini

          string lnk-path-sis"arquivosINI\" lnk-usuario
             "-indiceColunaListView.ini"
             delimited by "*" into ls-arq-ini
          string lnkSecao delimited by "  " into ls-secao-ini
          string lnkChaveListView delimited by "  " into ls-chave-ini
          invoke self "LerINI"
                          using ls-Arq-ini ls-secao-ini lnkChaveListView
                          returning lnkOrdem

       exit method
       end method "IndiceColunaListView".

      *---------------------------------------------------------------
      *---------------------------------------------------------------

      *---------------------------------------------------------------
      *---------------------------------------------------------------

       method-id. "GravarUltimaTelaPedido".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       77 lsConteudo pic x(255).
       linkage section.
       01 lnkusu.
          copy usuario.cpy.
       01 lnk-texto pic x(255).
       procedure division using lnkusu lnk-texto.
           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis "\arquivosINI\" delimited by "*"
                  "ultimaTela.INI" X"00"
             into lsArquivo

           string "PEDIDO"           X"00" into lsSecao
           string "TIPOVENDA"        X"00" into lsChave
           string lnk-texto          X"00" into lsConteudo

           invoke self "GravarINI" using lsArquivo lsSecao lsChave
                                         lsConteudo

       exit method
       end method "GravarUltimaTelaPedido".

       method-id. "UltimaTelaPedido".
       77 lSArquivo  pic x(255).
       77 lsSecao    pic x(255).
       77 lsChave    pic x(255).
       linkage section.
       77 lnkConteudo pic x(255).
       01 lnkusu.
          copy usuario.cpy.
       procedure division using lnkusu returning lnkConteudo.
           initialize lsArquivo lsSecao lsChave

           string lnk-path-sis "\arquivosINI\" delimited by "*"
                  "ultimaTela.INI" X"00"
             into lsArquivo
           string "PEDIDO"           X"00" into lsSecao
           string "TIPOVENDA"        X"00" into lsChave

           invoke self "LerINI" using lsArquivo lsSecao lsChave
                                returning  lnkConteudo

       exit method
       end method "UltimaTelaPedido".


      ******************************************************************

       object.
       end object.
       end class FileINI.

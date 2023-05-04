           SELECT CAD001 ASSIGN "\PROGRAMA\KELLO\PADRAO\CAD001"
                         ORGANIZATION IS          INDEXED
                         ACCESS MODE  IS          DYNAMIC
                         LOCK MODE    IS        AUTOMATIC
                         WITH LOCK    ON           RECORD
                         STATUS       IS        ST-CAD001
                         RECORD KEY   IS     CODIGO-CA001
                         ALTERNATE                 RECORD
                         KEY          IS   NOME-EMP-CA001.

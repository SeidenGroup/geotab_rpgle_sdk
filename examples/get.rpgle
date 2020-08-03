**FREE
Ctl-Opt DFTACTGRP(*No) BNDDIR('GEOTAB');

/copy ./headers/geotab.rpgle_h

dcl-pr getenv pointer extproc('getenv');
  *n pointer value options(*string:*trim)  ;
end-pr;

dcl-pr putenv int(10) extproc('putenv');
  *n pointer value options(*string:*trim) ;
end-pr;

Dcl-S authinfo Like(Geotab_Token);
Dcl-s data Pointer;
dcl-s currentElement pointer;

Dcl-S someValue Varchar(52);

dcl-s authpointer pointer;
dcl-s index    int(5);
dcl-s length   int(5);

authpointer = getenv('GEOTAB_TOKEN');
If (authpointer <> *NULL);
  authinfo = %Str(authpointer);
Else;
  authinfo = Geotab_Auth('':'':'');
  putenv('GEOTAB_TOKEN=' + %TrimR(authinfo));
Endif;

data = Geotab_Get(authinfo:'Rule');

If (Geotab_Successful(data));

  length = Geotab_GetCount(data);

  For index = 1 to length;
    currentElement = Geotab_ElementAt(data:index-1);
    someValue = Geotab_StringAt(currentElement:'baseType');

    Dsply someValue;
  endfor;

Else;

  Dsply 'It went horribly wrong';

Endif;

Geotab_Close(data);

Return;
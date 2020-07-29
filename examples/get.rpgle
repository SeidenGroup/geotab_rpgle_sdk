**FREE
Ctl-Opt DFTACTGRP(*No) BNDDIR('GEOTAB');

/copy ./headers/geotab.rpgle_h

Dcl-S authinfo Pointer;
Dcl-s data Pointer;
dcl-s currentElement pointer;

Dcl-S someValue Varchar(52);

dcl-s index    int(5);
dcl-s length   int(5);

authinfo = Geotab_Auth('':'':'');

data = Geotab_Get(authinfo:'Rule');

length = Geotab_GetCount(data);

For index = 1 to length;
  currentElement = Geotab_ElementAt(data:index-1);
  someValue = Geotab_StringAt(currentElement:'baseType');

  Dsply someValue;
endfor;

Geotab_Close(data);

Return;
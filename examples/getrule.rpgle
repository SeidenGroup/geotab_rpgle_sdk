**FREE
//compile:crtbndrpg
Ctl-Opt DFTACTGRP(*No) BNDDIR('GEOTAB');

/copy ./headers/geotab.rpgle_h

Dcl-S authinfo Pointer;
Dcl-s data Pointer;
dcl-s currentElement pointer;

dcl-s search Pointer;

Dcl-S someValue Varchar(52);

dcl-s index    int(5);
dcl-s length   int(5);

authinfo = Geotab_Auth('':'':'');

search = Geotab_NewObject();
Geotab_SetStr(search:'id':'auCTYUcfKx02zLHcsxaBTXA');

data = Geotab_Get(authinfo:'Rule':100:search);

length = Geotab_GetCount(data);

For index = 0 to length-1;
  currentElement = Geotab_ElementAt(data:index);
  someValue = Geotab_StringAt(currentElement:'name');

  Dsply someValue;
endfor;

Geotab_Close(data);

Return;
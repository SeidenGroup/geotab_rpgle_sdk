**FREE
Ctl-Opt DFTACTGRP(*No) BNDDIR('GEOTAB');

/copy ./headers/geotab.rpgle_h

Dcl-S authinfo Like(Geotab_Token);
Dcl-s data Pointer;
dcl-s currentElement pointer;

dcl-s fromDate date;
Dcl-S speed packed(8:3);

dcl-s index    int(5);
dcl-s length   int(5);

authinfo = Geotab_Auth('':'':'');

fromDate = %date - %days(1);
data = Geotab_GetRoadMaxSpeeds(authinfo:'b8F0':%char(fromDate:*iso));

length = Geotab_GetCount(data);

For index = 1 to length;
  currentElement = Geotab_ElementAt(data:index-1);
  speed = Geotab_NumberAt(currentElement:'v');

  Dsply ('Speed: ' + %Char(speed));
endfor;

Geotab_Close(data);

Return;
**FREE
//compile:crtbndrpg
Ctl-Opt DFTACTGRP(*No) BNDDIR('GEOTAB');

/copy ./headers/geotab.rpgle_h

Dcl-Pi SETDEVICE;
  pDeviceID Char(4);
  pNewName  Char(30);
End-Pi;

Dcl-S authinfo Pointer;
Dcl-s data Pointer;
dcl-s subObject pointer;
dcl-s array pointer;

authinfo = Geotab_Auth('':'':'');

data = Geotab_NewObject();
Geotab_SetStr(data:'id':%TrimR(pDeviceID));
Geotab_SetStr(data:'name':%TrimR(pNewName));

Geotab_Update(authinfo:'Device':data);

Geotab_Close(data);

Return;
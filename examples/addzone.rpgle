**FREE
//compile:crtbndrpg
Ctl-Opt DFTACTGRP(*No) BNDDIR('GEOTAB');

/copy ./headers/geotab.rpgle_h

Dcl-S authinfo Pointer;
Dcl-s data Pointer;
dcl-s subObject pointer;
dcl-s array pointer;
dcl-s index int(3);

Dcl-S activeFrom Date Inz(d'1986-01-01');
Dcl-S activeTo Date Inz(d'2050-01-01');

Dcl-Ds points qualified dim(2);
  x Packed(9:6);
  y Packed(9:6);
End-Ds;

points(1).x = -79.712318;
points(1).y = 43.438266;
points(2).x = -79.711181;
points(2).y = 43.437461;

authinfo = Geotab_Auth('':'':'');

data = Geotab_NewObject();

Geotab_SetStr(data:'name':'Example Zone some date');
Geotab_SetBool(data:'mustIdentifyStops':*On);
Geotab_SetBool(data:'displayed':*On);

Geotab_SetStr(data:'activeFrom':%Char(activeFrom:*ISO));
Geotab_SetStr(data:'activeTo':%Char(activeTo:*ISO));

array = Geotab_NewArray();
Geotab_ArrayPushString(array:'ZoneTypeOfficeId');
Geotab_SetArray(data:'zoneTypes':array);

subObject = Geotab_NewObject();
Geotab_SetNum(subObject:'r':255);
Geotab_SetNum(subObject:'g':165);
Geotab_SetNum(subObject:'b':0);
Geotab_SetNum(subObject:'a':191);
Geotab_SetObject(data:'fillColor':subObject);

array = Geotab_NewArray();

For index = 1 to %Elem(points);
  subObject = Geotab_NewObject();
  Geotab_SetNum(subObject:'x':points(index).x);
  Geotab_SetNum(subObject:'y':points(index).y);
  Geotab_ArrayPush(array:subObject);
Endfor;

Geotab_SetArray(data:'points':array);

array = Geotab_NewArray();
subObject = Geotab_NewObject();
Geotab_SetStr(subObject:'id':'GroupCompanyId');
Geotab_ArrayPush(array:subObject);
Geotab_SetArray(data:'groups':array);

Geotab_Add(authinfo:'Zone':data);

Geotab_Close(data);

Return;
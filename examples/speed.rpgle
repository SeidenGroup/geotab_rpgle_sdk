**FREE
//compile:crtbndrpg
Ctl-Opt DFTACTGRP(*No) BNDDIR('GEOTAB');

/copy ./headers/geotab.rpgle_h

Dcl-S authinfo Pointer;
Dcl-s data Pointer;
dcl-s search pointer;
dcl-s currentElement pointer;

Dcl-S toVersion Varchar(32);
Dcl-S currentRule Varchar(32);
dcl-s driverID varchar(20);

dcl-s index    int(5);
dcl-s length   int(5);

Dcl-s date date;

//**********************************

//Authenticate to Geotab APIs. Must be called first
authinfo = Geotab_Auth('':'':'');

toVersion = '';
length = 1; //To get into the DOW loop

//We need to create a new search objec to pass a date in
search = Geotab_NewObject();

//We create a date using the %CHAR function
date = %date;
Geotab_SetStr(search:'fromDate':%char(date:*iso));

//We keep calling GetFeed until we have no more data to fetch.
Dow (length > 0);
  Geotab_Close(data);
  data = Geotab_GetFeed(authinfo:'ExceptionEvent':500:toVersion:search);
  //We store the next version to go from so we can stream the results
  toVersion = Geotab_StringAt(data:'result.toVersion');

  //Amount of elements returned
  length = Geotab_GetCount(data);

  If (length > 0);

    Dsply ('Count: ' + %char(length) + '-' + %trimr(toVersion));
    //Handle array of data here
    For index = 0 to length-1;
      currentElement = Geotab_ElementAt(data:index);
      currentRule = Geotab_StringAt(currentElement:'rule.id');

      if (currentRule = 'auCTYUcfKx02zLHcsxaBTXA');
        driverID = Geotab_StringAt(currentElement:'driver.id');
        Dsply ('Driver ' + %TrimR(driverID) + ' broke the rule.');
      Endif;
    Endfor;

  Else;
    Geotab_Close(data);
    Leave;
  Endif;
Enddo;

Dsply 'End';

Return;
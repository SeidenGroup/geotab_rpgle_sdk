**FREE
//compile:CRTSQLRPGI_MOD

ctl-opt nomain;
ctl-opt debug(*yes);

//**************************************

/include ./headers/JSONPARSER.rpgle
/include ./headers/geotab.rpgle_h

//**************************************

//@ Used to call the Geotab API
Dcl-Proc Geotab_Call;
  Dcl-Pi Geotab_Call Pointer;
    //@ Pointer which comes from noxDB
    pBody Pointer;
    //@ Optional auth token which comes from Geotab
    pToken Like(Geotab_Token) Options(*NoPass);
  End-Pi;
  
  Dcl-Ds request Qualified;
    URL    Varchar(64);
    Header Varchar(1024);
    Body   Varchar(2048);
  End-Ds;

  Dcl-S AuthJSON Pointer;
  Dcl-S Response SQLTYPE(CLOB:1000000);

  If (%Parms >= 2);
    AuthJSON = json_ParseString(pToken);
    request.URL = 'https://'
                + json_GetStr(AuthJSON:'result.path')
                + '/apiv1/';
    json_SetValue(pBody
                 :'params.credentials'
                 :json_Locate(AuthJSON:'result.credentials')
                 :json_OBJECT);
  Else;
    If (json_Has(pBody:'initURL'));
      request.URL = json_GetStr(pBody:'initURL');
    Else;
      request.URL = 'https://my.geotab.com/apiv1';
    Endif;
  Endif;

  request.Header 
        = '{'
        + '"header": "Content-Type,application/json",'
        + '}';

  request.Body = json_AsText(pBody);

  EXEC SQL
    SET :Response = 
    QSYS2.HTTP_POST(
      :request.URL,
      :request.Body,
      :request.Header
    );

  json_Close(pBody);

  Return json_ParseString(Response_Data);
End-Proc;

//**************************************

Dcl-Proc Geotab_Successful Export;
  Dcl-Pi Geotab_Successful Ind;
    pResult Pointer Options(*NoPass);
  End-Pi;

  If (sqlstate <> '00000');
    Return *Off;
  Endif;

  If (%Parms >= 1);
    If (json_Error(pResult));
      Return *Off;
    Endif;

    If (json_Has(pResult:'error'));
      Return *Off;
    Endif;
  Endif;

  Return *On;
End-Proc;

//**************************************

Dcl-Proc Geotab_Auth Export;
  Dcl-Pi Geotab_Auth Like(Geotab_Token);
    pURL      Pointer Value Options(*String);
    pDatabase Pointer Value Options(*String);
    pUsername Pointer Value Options(*String);
    pPassword Pointer Value Options(*String);
  End-Pi;

  Dcl-S json Pointer;
  Dcl-S authData Like(Geotab_Token);

  json = json_NewObject();

  json_SetStr(json:'initURL':pURL);
  json_SetStr(json:'method':'Authenticate');
  json_SetStr(json:'params.database':pDatabase);
  json_SetStr(json:'params.userName':pUsername);
  json_SetStr(json:'params.password':pPassword);

  json = Geotab_Call(json);
  authData = json_AsText(json);

  json_Close(json);

  Return authData;
End-Proc;

//**************************************

Dcl-Proc Geotab_Close Export;
  Dcl-Pi Geotab_Close;
    pJSON Pointer;
  End-Pi;

  json_Close(pJSON);
End-Proc;

//**************************************

Dcl-Proc Geotab_Get Export;
  Dcl-Pi Geotab_Get Pointer;
    pSession Like(Geotab_Token);
    pType    Pointer Value Options(*String);
    pLimit   Int(5) Const Options(*NoPass);
    pSearch  Pointer Options(*NoPass);
  End-Pi;

  Dcl-S json Pointer;
  Dcl-S limit Like(pLimit) Inz(100);

  json = json_NewObject();

  If (%Parms >= 3);
    limit = pLimit;
  Endif;

  If (%Parms >= 4);
    json_SetValue(json
                 :'params.search'
                 :pSearch
                 :json_OBJECT);
  Endif;

  json_SetStr(json:'method':'Get');
  json_SetStr(json:'params.typeName':pType);
  json_SetNum(json:'params.resultsLimit':limit);
  
  json = Geotab_Call(json:pSession);

  Return json;
End-Proc;

//**************************************

Dcl-Proc Geotab_GetFeed Export;
  Dcl-Pi Geotab_GetFeed Pointer;
    pSession     Like(Geotab_Token);
    pType        Pointer Value Options(*String);
    pLimit       Int(5) Const Options(*NoPass);
    pFromVersion Pointer Value Options(*String:*NoPass);
    pSearch      Pointer Options(*NoPass);
  End-Pi;

  Dcl-S json Pointer;
  Dcl-S limit Like(pLimit) Inz(100);
  Dcl-S fromversion varchar(32);

  Dcl-s message varchar(128);

  json = json_NewObject();

  If (%Parms >= 3);
    limit = pLimit;
  Endif;

  If (%Parms >= 4);
    fromversion = %Str(pFromVersion);
  Else;
    fromversion = '';
  Endif;

  If (%Parms >= 5);
    json_SetValue(json
                 :'params.search'
                 :pSearch
                 :json_OBJECT);
  Endif;

  If (fromversion <> *BLANK);
    json_SetStr(json:'params.fromVersion':fromversion);
  Endif;

  json_SetStr(json:'method':'GetFeed');
  json_SetStr(json:'params.typeName':pType);
  json_SetNum(json:'params.resultsLimit':limit);
  
  json = Geotab_Call(json:pSession);

  If (json_Error(json));
    message = json_Message(json);
  Endif;

  Return json;
End-Proc;

//**************************************

Dcl-Proc Geotab_GetRoadMaxSpeeds Export;
  Dcl-Pi Geotab_GetRoadMaxSpeeds Pointer;
    pSession  Like(Geotab_Token);
    pDeviceID Pointer Value Options(*String:*NoPass);
    pFromDate Pointer Value Options(*String:*NoPass);
    pToDate   Pointer Value Options(*String:*NoPass);
  End-Pi;

  Dcl-S json Pointer;
  Dcl-S string varchar(24);

  json = json_NewObject();

  json_SetStr(json:'method':'GetRoadMaxSpeeds');

  If (%Parms >= 2);
    string = %Str(pDeviceID);
    If (string <> *BLANK);
      json_SetStr(json:'params.deviceSearch.id':string);
    Endif;
  Endif;

  If (%Parms >= 3);
    string = %Str(pFromDate);
    If (string <> *BLANK);
      json_SetStr(json:'params.fromDate':string);
    Endif;
  Endif;

  If (%Parms >= 4);
    string = %Str(pToDate);
    If (string <> *BLANK);
      json_SetStr(json:'params.toDate':string);
    Endif;
  Endif;
  
  json = Geotab_Call(json:pSession);

  return json;
End-Proc;

//**************************************

Dcl-Proc Geotab_GetAddresses Export;
  Dcl-Pi Geotab_GetAddresses Pointer;
    pSession         Like(Geotab_Token);
    pCoordinates     Pointer Value;
    pHosAddresses    Ind Const Options(*NoPass);
    pMovingAddresses Ind Const Options(*NoPass);
  End-Pi;

  Dcl-S json Pointer;
  Dcl-S HosAddresses Ind Inz(*Off);
  Dcl-S MovingAddresses Ind Inz(*Off);

  If (%Parms >= 3); 
    HosAddresses = pHosAddresses;
  Endif;

  If (%Parms >= 4); 
    MovingAddresses = pMovingAddresses;
  Endif;

  json = json_NewObject();

  json_SetStr(json:'method':'GetAddresses');
  json_SetValue(json:'params.coordinates':pCoordinates:json_ARRAY);
  json_SetBool(json:'params.hosAddresses':HosAddresses);
  json_SetBool(json:'params.movingAddresses':MovingAddresses);

  json = Geotab_Call(json:pSession);

  return json;
End-Proc;

//**************************************

Dcl-Proc Geotab_Add Export;
  Dcl-Pi Geotab_Add Pointer;
    pSession          Like(Geotab_Token);
    pEntityType       Pointer Value Options(*String);
    pEntityProperties Pointer;
  End-Pi;

  Dcl-S json Pointer;

  json = json_NewObject();

  json_SetStr(json:'method':'Add');
  json_SetStr(json:'params.typeName':pEntityType);
  json_SetValue(json:'params.entity':pEntityProperties:json_OBJECT);

  json = Geotab_Call(json:pSession);
  
  Return json;

End-Proc;

//**************************************

Dcl-Proc Geotab_Update Export;
  Dcl-Pi Geotab_Update Pointer;
    pSession          Like(Geotab_Token);
    pEntityType       Pointer Value Options(*String);
    pEntityProperties Pointer;
  End-Pi;

  Dcl-S json Pointer;

  json = json_NewObject();

  json_SetStr(json:'method':'Set');
  json_SetStr(json:'params.typeName':pEntityType);
  json_SetValue(json:'params.entity':pEntityProperties:json_OBJECT);

  json = Geotab_Call(json:pSession);

  Return json;

End-Proc;

//**************************************

Dcl-Proc Geotab_Remove Export;
  Dcl-Pi Geotab_Remove;
    pSession    Like(Geotab_Token);
    pEntityType Pointer Value Options(*String);
    pEntityID   Pointer Value Options(*String);
  End-Pi;

  Dcl-S json Pointer;

  json = json_NewObject();

  json_SetStr(json:'method':'Set');
  json_SetStr(json:'params.typeName':pEntityType);
  json_SetStr(json:'params.entity.id':pEntityID);

  json = Geotab_Call(json:pSession);
  json_Close(json);

End-Proc;

//**************************************

Dcl-Proc Geotab_GetCoordinates Export;
  Dcl-Pi Geotab_GetCoordinates Pointer;
    pSession          Like(Geotab_Token);
    pAddressesArray   Pointer;
  End-Pi;

  Dcl-S json Pointer;

  json = json_NewObject();

  json_SetStr(json:'method':'GetCoordinates');
  json_SetValue(json:'params.addresses':pAddressesArray:json_ARRAY);

  json = Geotab_Call(json:pSession);

  return json;
End-Proc;

//**************************************

Dcl-Proc Geotab_GetDirections Export;
  Dcl-Pi Geotab_GetDirections Pointer;
    pSession          Like(Geotab_Token);
    pWaypointsArray   Pointer;
  End-Pi;

  Dcl-S json Pointer;

  json = json_NewObject();

  json_SetStr(json:'method':'GetDirections');
  json_SetValue(json:'params.waypoints':pWaypointsArray:json_ARRAY);

  json = Geotab_Call(json:pSession);

  return json;
End-Proc;

//**************************************

Dcl-Proc Geotab_OptimizeWaypoints Export;
  Dcl-Pi Geotab_OptimizeWaypoints Pointer;
    pSession          Like(Geotab_Token);
    pWaypointsArray   Pointer;
  End-Pi;

  Dcl-S json Pointer;

  json = json_NewObject();

  json_SetStr(json:'method':'OptimizeWaypoints');
  json_SetValue(json:'params.waypoints':pWaypointsArray:json_ARRAY);

  json = Geotab_Call(json:pSession);

  return json;
End-Proc;

//**************************************

Dcl-Proc Geotab_NewWaypoint Export;
  Dcl-Pi Geotab_NewWaypoint Pointer;
    pCoordX      Packed(30:15) Const;
    pCoordY      Packed(30:15) Const;
    pDescription Pointer Value Options(*String);
    pSequence    Packed(30:15) Const;
  End-Pi;

  Dcl-S waypoint Pointer;

  waypoint = json_NewObject();
  
  json_SetStr(waypoint:'description':pDescription);
  json_SetNum(waypoint:'sequence':pSequence);
  json_SetNum(waypoint:'coordinate.x':pCoordX);
  json_SetNum(waypoint:'coordinate.y':pCoordY);

  Return waypoint;
End-Proc;

//**************************************

Dcl-Proc Geotab_NewObject Export;
  Dcl-Pi Geotab_NewObject Pointer End-Pi;

  Return json_NewObject();
End-Proc;

//**************************************

Dcl-Proc Geotab_NewArray Export;
  Dcl-Pi Geotab_NewArray Pointer End-Pi;

  Return json_NewArray();
End-Proc;

//**************************************

Dcl-Proc Geotab_ArrayPush Export;
  Dcl-Pi Geotab_ArrayPush;
    pArray Pointer;
    pValue Pointer;
  End-Pi;

  json_ArrayPush(pArray:pValue);
End-Proc;

//**************************************

Dcl-Proc Geotab_ArrayPushString Export;
  Dcl-Pi Geotab_ArrayPushString;
    pArray Pointer;
    pValue Pointer Value Options(*String);
  End-Pi;

  json_ArrayPush(pArray:pValue);
End-Proc;

//**************************************

Dcl-Proc Geotab_SetStr Export;
  Dcl-Pi Geotab_SetStr;
    pObject   Pointer;
    pProperty Pointer Value Options(*String);
    pValue    Pointer Value Options(*String);
  End-Pi;

  json_SetStr(pObject:pProperty:pValue);
End-Proc;

//**************************************

Dcl-Proc Geotab_SetNum Export;
  Dcl-Pi Geotab_SetNum;
    pObject   Pointer;
    pProperty Pointer Value Options(*String);
    pValue    Packed(30:15) Const;
  End-Pi;

  json_SetNum(pObject:pProperty:pValue);
End-Proc;

//**************************************

Dcl-Proc Geotab_SetBool Export;
  Dcl-Pi Geotab_SetBool;
    pObject   Pointer;
    pProperty Pointer Value Options(*String);
    pValue    Ind Const;
  End-Pi;

  json_SetBool(pObject:pProperty:pValue);
End-Proc;

//**************************************

Dcl-Proc Geotab_SetArray Export;
  Dcl-Pi Geotab_SetArray;
    pObject   Pointer;
    pProperty Pointer Value Options(*String);
    pArray    Pointer;
  End-Pi;

  json_SetValue(pObject:pProperty:pArray:json_ARRAY);
End-Proc;

//**************************************

Dcl-Proc Geotab_SetObject Export;
  Dcl-Pi Geotab_SetObject;
    pObject    Pointer;
    pProperty  Pointer Value Options(*String);
    pNewObject Pointer;
  End-Pi;

  json_SetValue(pObject:pProperty:pNewObject:json_OBJECT);
End-Proc;

//**************************************

Dcl-Proc Geotab_ObjectAt Export;
  Dcl-Pi Geotab_ObjectAt Pointer;
    pObject    Pointer;
    pProperty  Pointer Value Options(*String);
  End-Pi;

  Return json_Locate(pObject:pProperty);
End-proc;

//**************************************

// This API should be used to get the length of
// An array from the Geotab_Get API
Dcl-Proc Geotab_GetCount Export;
  Dcl-Pi Geotab_GetCount Int(5);
    pResult Pointer;
  End-Pi;

  Dcl-S Array Pointer;

  Select;
    When (json_Has(pResult:'result.data'));
      //Usually indicates GetFeed
      Array = json_Locate(pResult:'result.data');
    When (json_Has(pResult:'result'));
      Array = json_Locate(pResult:'result');
    Other;
      Array = pResult;
  Endsl;

  If (Array = *NULL);
    Array = pResult;
  Endif;

  Return json_GetLength(Array);
End-Proc;

//**************************************

Dcl-Proc Geotab_ElementAt Export;
  Dcl-Pi Geotab_ElementAt Pointer;
    pObject Pointer;
    pIndex  Int(5) Const;
  End-Pi;

  Select;
    When (json_Has(pObject:'result.data'));
      Return json_Locate(pObject:'result.data[' + %Char(pIndex) + ']');
    When (json_Has(pObject:'result'));
      Return json_Locate(pObject:'result[' + %Char(pIndex) + ']');
    Other;
      Return json_Locate(pObject:'[' + %Char(pIndex) + ']');
  Endsl;

  Return *Null;

End-Proc;

//**************************************

Dcl-Proc Geotab_StringAt Export;
  Dcl-Pi Geotab_StringAt Varchar(128);
    pObject   Pointer;
    pProperty Pointer Value Options(*String);
  End-Pi;

  Return json_GetStr(pObject:pProperty);
End-Proc;

//**************************************

Dcl-Proc Geotab_NumberAt Export;
  Dcl-Pi Geotab_NumberAt Packed(30:15);
    pObject   Pointer;
    pProperty Pointer Value Options(*String);
  End-Pi;

  Return json_GetNum(pObject:pProperty);
End-Proc;

//**************************************

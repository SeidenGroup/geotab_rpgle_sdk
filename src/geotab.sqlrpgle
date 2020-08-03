**FREE
//compile :CRTSQLRPGI_MOD

ctl-opt nomain;
ctl-opt debug(*yes);

//**************************************

/include ./headers/JSONPARSER.rpgle
/include ./headers/geotab.rpgle_h

//**************************************

Dcl-Proc Geotab_Call;
  Dcl-Pi Geotab_Call Pointer;
    pBody Pointer;
    pAuth Pointer Options(*NoPass);
  End-Pi;

  Dcl-Ds request Qualified;
    URL    Varchar(64);
    Header Varchar(1024);
    Body   Varchar(1024);
  End-Ds;

  Dcl-S Response SQLTYPE(CLOB:262144);

  If (%Parms >= 2);
    request.URL = 'https://'
                + JSON_GetStr(pAuth:'result.path')
                + '/apiv1/';
    JSON_SetValue(pBody
                 :'params.credentials'
                 :JSON_Locate(pAuth:'result.credentials')
                 :JSON_OBJECT);
  Else;
    request.URL = 'https://my.geotab.com/apiv1';
  Endif;

  request.Header 
         = '<httpHeader>'
         + '<header name="Content-Type" value="application/json" />'
         + '<header name="Accept-Encoding" value="gzip, deflate" />'
         + '</httpHeader>';

  request.Body = JSON_AsText(pBody);

  EXEC SQL
    SET :Response = 
    SYSTOOLS.HTTPPOSTCLOB(
      :request.URL,
      :request.Header,
      :request.Body
    );

  JSON_Close(pBody);

  Return JSON_ParseString(Response_Data);
End-Proc;

//**************************************

Dcl-Proc Geotab_Auth Export;
  Dcl-Pi Geotab_Auth Pointer;
    pDatabase Pointer Value Options(*String);
    pUsername Pointer Value Options(*String);
    pPassword Pointer Value Options(*String);
  End-Pi;

  Dcl-S json Pointer;

  json = JSON_NewObject();

  JSON_SetStr(json:'method':'Authenticate');
  JSON_SetStr(json:'params.database':pDatabase);
  JSON_SetStr(json:'params.userName':pUsername);
  JSON_SetStr(json:'params.password':pPassword);

  json = Geotab_Call(json);

  Return json;
End-Proc;

//**************************************

Dcl-Proc Geotab_Close Export;
  Dcl-Pi Geotab_Close;
    pJSON Pointer;
  End-Pi;

  JSON_Close(pJSON);
End-Proc;

//**************************************

Dcl-Proc Geotab_Get Export;
  Dcl-Pi Geotab_Get Pointer;
    pSession Pointer;
    pType    Pointer Value Options(*String);
    pLimit   Int(5) Const Options(*NoPass);
    pSearch  Pointer Options(*NoPass);
  End-Pi;

  Dcl-S json Pointer;
  Dcl-S limit Like(pLimit) Inz(100);

  json = JSON_NewObject();

  If (%Parms >= 3);
    limit = pLimit;
  Endif;

  If (%Parms >= 4);
    JSON_SetValue(json
                 :'params.search'
                 :pSearch
                 :JSON_OBJECT);
  Endif;

  JSON_SetStr(json:'method':'Get');
  JSON_SetStr(json:'params.typeName':pType);
  JSON_SetNum(json:'params.resultsLimit':limit);
  
  json = Geotab_Call(json:pSession);

  Return json;
End-Proc;

//**************************************

Dcl-Proc Geotab_GetFeed Export;
  Dcl-Pi Geotab_GetFeed Pointer;
    pSession     Pointer;
    pType        Pointer Value Options(*String);
    pLimit       Int(5) Const Options(*NoPass);
    pFromVersion Pointer Value Options(*String:*NoPass);
    pSearch      Pointer Options(*NoPass);
  End-Pi;

  Dcl-S json Pointer;
  Dcl-S limit Like(pLimit) Inz(100);
  Dcl-S fromversion varchar(32);

  Dcl-s message varchar(128);

  json = JSON_NewObject();

  If (%Parms >= 3);
    limit = pLimit;
  Endif;

  If (%Parms >= 4);
    fromversion = %Str(pFromVersion);
  Else;
    fromversion = '';
  Endif;

  If (%Parms >= 5);
    JSON_SetValue(json
                 :'params.search'
                 :pSearch
                 :JSON_OBJECT);
  Endif;

  If (fromversion <> *BLANK);
    JSON_SetStr(json:'params.fromVersion':fromversion);
  Endif;

  JSON_SetStr(json:'method':'GetFeed');
  JSON_SetStr(json:'params.typeName':pType);
  JSON_SetNum(json:'params.resultsLimit':limit);
  
  json = Geotab_Call(json:pSession);

  If (json_Error(json));
    message = json_Message(json);
  Endif;

  Return json;
End-Proc;

//**************************************

Dcl-Proc Geotab_GetRoadMaxSpeeds Export;
  Dcl-Pi Geotab_GetRoadMaxSpeeds Pointer;
    pSession  Pointer;
    pDeviceID Pointer Value Options(*String:*NoPass);
    pFromDate Pointer Value Options(*String:*NoPass);
    pToDate   Pointer Value Options(*String:*NoPass);
  End-Pi;

  Dcl-S json Pointer;
  Dcl-S string varchar(24);

  json = JSON_NewObject();

  JSON_SetStr(json:'method':'GetRoadMaxSpeeds');

  If (%Parms >= 2);
    string = %Str(pDeviceID);
    If (string <> *BLANK);
      JSON_SetStr(json:'params.deviceSearch.id':string);
    Endif;
  Endif;

  If (%Parms >= 3);
    string = %Str(pFromDate);
    If (string <> *BLANK);
      JSON_SetStr(json:'params.fromDate':string);
    Endif;
  Endif;

  If (%Parms >= 4);
    string = %Str(pToDate);
    If (string <> *BLANK);
      JSON_SetStr(json:'params.toDate':string);
    Endif;
  Endif;
  
  json = Geotab_Call(json:pSession);

  return json;
End-Proc;

//**************************************

Dcl-Proc Geotab_GetAddresses Export;
  Dcl-Pi Geotab_GetAddresses Pointer;
    pSession         Pointer;
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

  json = JSON_NewObject();

  JSON_SetStr(json:'method':'GetAddresses');
  JSON_SetValue(json:'params.coordinates':pCoordinates:JSON_ARRAY);
  JSON_SetBool(json:'params.hosAddresses':HosAddresses);
  JSON_SetBool(json:'params.movingAddresses':MovingAddresses);

  json = Geotab_Call(json:pSession);

  return json;
End-Proc;

//**************************************

Dcl-Proc Geotab_Add Export;
  Dcl-Pi Geotab_Add;
    pSession          Pointer;
    pEntityType       Pointer Value Options(*String);
    pEntityProperties Pointer;
  End-Pi;

  Dcl-S json Pointer;

  json = JSON_NewObject();

  JSON_SetStr(json:'method':'Add');
  JSON_SetStr(json:'params.typeName':pEntityType);
  JSON_SetValue(json:'params.entity':pEntityProperties:JSON_OBJECT);

  json = Geotab_Call(json:pSession);

End-Proc;

//**************************************

Dcl-Proc Geotab_NewObject Export;
  Dcl-Pi Geotab_NewObject Pointer End-Pi;

  Return JSON_NewObject();
End-Proc;

//**************************************

Dcl-Proc Geotab_NewArray Export;
  Dcl-Pi Geotab_NewArray Pointer End-Pi;

  Return JSON_NewArray();
End-Proc;

//**************************************

Dcl-Proc Geotab_ArrayPush Export;
  Dcl-Pi Geotab_ArrayPush;
    pArray Pointer;
    pValue Pointer;
  End-Pi;

  JSON_ArrayPush(pArray:pValue);
End-Proc;

//**************************************

Dcl-Proc Geotab_ArrayPushString Export;
  Dcl-Pi Geotab_ArrayPushString;
    pArray Pointer;
    pValue Pointer Value Options(*String);
  End-Pi;

  JSON_ArrayPush(pArray:pValue);
End-Proc;

//**************************************

Dcl-Proc Geotab_SetStr Export;
  Dcl-Pi Geotab_SetStr;
    pObject   Pointer;
    pProperty Pointer Value Options(*String);
    pValue    Pointer Value Options(*String);
  End-Pi;

  JSON_SetStr(pObject:pProperty:pValue);
End-Proc;

//**************************************

Dcl-Proc Geotab_SetNum Export;
  Dcl-Pi Geotab_SetNum;
    pObject   Pointer;
    pProperty Pointer Value Options(*String);
    pValue    Packed(30:15) Const;
  End-Pi;

  JSON_SetNum(pObject:pProperty:pValue);
End-Proc;

//**************************************

Dcl-Proc Geotab_SetBool Export;
  Dcl-Pi Geotab_SetBool;
    pObject   Pointer;
    pProperty Pointer Value Options(*String);
    pValue    Ind Const;
  End-Pi;

  JSON_SetBool(pObject:pProperty:pValue);
End-Proc;

//**************************************

Dcl-Proc Geotab_SetArray Export;
  Dcl-Pi Geotab_SetArray;
    pObject   Pointer;
    pProperty Pointer Value Options(*String);
    pArray    Pointer;
  End-Pi;

  JSON_SetValue(pObject:pProperty:pArray:JSON_ARRAY);
End-Proc;

//**************************************

Dcl-Proc Geotab_SetObject Export;
  Dcl-Pi Geotab_SetObject;
    pObject    Pointer;
    pProperty  Pointer Value Options(*String);
    pNewObject Pointer;
  End-Pi;

  JSON_SetValue(pObject:pProperty:pNewObject:JSON_OBJECT);
End-Proc;

//**************************************

// This API should be used to get the length of
// An array from the Geotab_Get API
Dcl-Proc Geotab_GetCount Export;
  Dcl-Pi Geotab_GetCount Int(5);
    pResult Pointer;
  End-Pi;

  Dcl-S Array Pointer;

  Select;
    When (JSON_Has(pResult:'result.data'));
      //Usually indicates GetFeed
      Array = JSON_Locate(pResult:'result.data');
    When (JSON_Has(pResult:'result'));
      Array = JSON_Locate(pResult:'result');
  Endsl;

  Return JSON_GetLength(Array);
End-Proc;

//**************************************

Dcl-Proc Geotab_ElementAt Export;
  Dcl-Pi Geotab_ElementAt Pointer;
    pObject Pointer;
    pIndex  Int(5) Const;
  End-Pi;

  Select;
    When (JSON_Has(pObject:'result.data'));
      Return JSON_Locate(pObject:'result.data[' + %Char(pIndex) + ']');
    When (JSON_Has(pObject:'result'));
      Return JSON_Locate(pObject:'result[' + %Char(pIndex) + ']');
  Endsl;

  Return *Null;

End-Proc;

//**************************************

Dcl-Proc Geotab_StringAt Export;
  Dcl-Pi Geotab_StringAt Varchar(128);
    pObject   Pointer;
    pProperty Pointer Value Options(*String);
  End-Pi;

  Return JSON_GetStr(pObject:pProperty);
End-Proc;

//**************************************

Dcl-Proc Geotab_NumberAt Export;
  Dcl-Pi Geotab_NumberAt Packed(30:15);
    pObject   Pointer;
    pProperty Pointer Value Options(*String);
  End-Pi;

  Return JSON_GetNum(pObject:pProperty);
End-Proc;

//**************************************
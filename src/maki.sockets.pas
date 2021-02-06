unit maki.sockets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TNodeSocketInt }

  TNodeSocketInt = class ( gNodeSocketCustom < Integer >)
    protected
      procedure InternalJSONRead( AValue: TJSONData ); override;
      function InternalJSONWrite: TJSONData; override;

    public
      class function GetTypeName: String; override;
  end;

  { TNodeSocketFloat }

  TNodeSocketFloat = class ( gNodeSocketCustom < Single >)
    protected
      procedure InternalJSONRead( AValue: TJSONData ); override;
      function InternalJSONWrite: TJSONData; override;

    public
      class function GetTypeName: String; override;
  end;

  { TNodeSocketBoolean }

  TNodeSocketBoolean = class ( gNodeSocketCustom < Boolean >)
    protected
      procedure InternalJSONRead( AValue: TJSONData ); override;
      function InternalJSONWrite: TJSONData; override;

    public
      class function GetTypeName: String; override;
  end;

  { TNodeSocketString }

  TNodeSocketString = class ( gNodeSocketCustom < String >)
    protected
      procedure InternalJSONRead( AValue: TJSONData ); override;
      function InternalJSONWrite: TJSONData; override;

    public
      class function GetTypeName: String; override;
  end;


implementation

procedure TNodeSocketString.InternalJSONRead(AValue: TJSONData);
begin
  if ( AValue.JSONType = jtString ) then
    AsValue:= AValue.AsString
  else
    raise Exception.CreateFmt( ST_ERROR_JSON_WRONG_TYPE, [ 'jtString', JSONTypeName( AValue.JSONType )]);;
end;

function TNodeSocketString.InternalJSONWrite: TJSONData;
begin
  Result:= TJSONString.Create( AsValue );
end;

class function TNodeSocketString.GetTypeName: String;
begin
  Result:= 'text';
end;

procedure TNodeSocketBoolean.InternalJSONRead(AValue: TJSONData);
begin
  if ( AValue.JSONType = jtBoolean ) then
    AsValue:= AValue.AsBoolean
  else
    raise Exception.CreateFmt( ST_ERROR_JSON_WRONG_TYPE, [ 'jtBoolean', JSONTypeName( AValue.JSONType )]);;
end;

function TNodeSocketBoolean.InternalJSONWrite: TJSONData;
begin
  Result:= TJSONBoolean.Create( AsValue );
end;

class function TNodeSocketBoolean.GetTypeName: String;
begin
  Result:= 'bool';
end;

procedure TNodeSocketFloat.InternalJSONRead(AValue: TJSONData);
begin
  if (( AValue.JSONType = jtNumber ) and ( TJSONNumber( AValue ).NumberType = ntFloat )) then
    AsValue:= AValue.AsFloat
  else
    raise Exception.CreateFmt( ST_ERROR_JSON_WRONG_TYPE, [ 'jtNumber, ntFloat', JSONTypeName( AValue.JSONType )]);;
end;

function TNodeSocketFloat.InternalJSONWrite: TJSONData;
begin
  Result:= TJSONFloatNumber.Create( AsValue );
end;

class function TNodeSocketFloat.GetTypeName: String;
begin
  Result:= 'float';
end;

procedure TNodeSocketInt.InternalJSONRead(AValue: TJSONData);
begin
  if (( AValue.JSONType = jtNumber ) and ( TJSONNumber( AValue ).NumberType = ntInteger )) then
    AsValue:= AValue.AsInteger
  else
    raise Exception.CreateFmt( ST_ERROR_JSON_WRONG_TYPE, [ 'jtNumber, ntInteger', JSONTypeName( AValue.JSONType )]);;
end;

function TNodeSocketInt.InternalJSONWrite: TJSONData;
begin
  Result:= TJSONIntegerNumber.Create( AsValue );
end;

class function TNodeSocketInt.GetTypeName: String;
begin
  Result:= 'int';
end;


end.


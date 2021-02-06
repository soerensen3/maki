unit maki.node;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  fpjson,

  st.storage,
  st.properties;

type

  TNodeSocket = class;
  TNodeSocketClass = class of TNodeSocket;
  TNode = class;

  { TNodeSocket }

  TNodeSocket = class ( TStorageTreeNode )
    protected
      FNode: TNode;
      FName: String;

      function GetName: String; override;
      procedure SetName(AValue: String); override;

    public
      constructor Create( AName: String; const AOwner: TStorageTreeCollection=nil); virtual; reintroduce;

      function GetData( ANode: TNode; ADefines: TStringList ): TJSONData; virtual; abstract;

      class function GetTypeName: String; virtual; abstract;

      property Node: TNode read FNode;
  end;

  { TNodeSocketOutput }

  TNodeSocketOutput = class abstract ( TNodeSocket )
    public
      //function GetData(ANode: TNode; ADefines: TStringList): TJSONData; override;

      class function GetTypeName: String; override;
  end;

  { TNodeSocketInput }

  TNodeSocketInput = class abstract ( TNodeSocket )
    private
      FConnected: TNodeSocketOutput;
      function GetIsConnected: Boolean;
      procedure SetConnected(AValue: TNodeSocketOutput);

    published
      property IsConnected: Boolean read GetIsConnected;
      property Connected: TNodeSocketOutput read FConnected write SetConnected;
  end;

  { TNode }

  TNode = class ( TStorageTreeObject )
    protected
      FInputs: gStorageTreeObjectDict < TNodeSocketInput >;
      FOutputs: gStorageTreeObjectDict < TNodeSocketOutput >;

      procedure CreateProperties; override;

    public
      property Inputs: gStorageTreeObjectDict < TNodeSocketInput > read FInputs;
      property Outputs: gStorageTreeObjectDict < TNodeSocketOutput > read FOutputs;
  end;


  { gNodeSocketCustom }

  gNodeSocketCustom < T > = class ( TNodeSocketInput )
    protected
      FAsValue: T;

      function NeedsStoring: Boolean; override;

    public
      function GetData(ANode: TNode; ADefines: TStringList): TJSONData; override;
      function SaveToJSON: TJSONData; override;

      property AsValue: T read FAsValue write FAsValue;
  end;

implementation

function gNodeSocketCustom<T>.SaveToJSON: TJSONData;
begin
  case StoreMode of
    smText: Result:= AsJSON;
    else
      Result:= nil;
  end;
end;

function gNodeSocketCustom<T>.NeedsStoring: Boolean;
begin
  Result:= True;
end;

function gNodeSocketCustom<T>.GetData(ANode: TNode; ADefines: TStringList): TJSONData;
begin
  if ( IsConnected ) then
    Result:= Connected.GetData( Node, ADefines )
  else
    Result:= SaveToJSON;
end;


class function TNodeSocketOutput.GetTypeName: String;
begin
  Result:= 'output';
end;

{ TNodeSocketInput }

function TNodeSocketInput.GetIsConnected: Boolean;
begin
  Result:= Assigned( Connected );
end;

procedure TNodeSocketInput.SetConnected(AValue: TNodeSocketOutput);
begin
  if FConnected=AValue then Exit;
  FConnected:=AValue;
end;

{ TNode }

procedure TNode.CreateProperties;
begin
  inherited CreateProperties;
  FInputs:= gStorageTreeObjectDict < TNodeSocketInput >.Create( 'Inputs', Self );
  FOutputs:= gStorageTreeObjectDict < TNodeSocketOutput >.Create( 'Outputs', Self );
end;



function TNodeSocket.GetName: String;
begin
  Result:= FName;
end;

procedure TNodeSocket.SetName(AValue: String);
begin
  inherited SetName(AValue);
  FName:= AValue;
end;

constructor TNodeSocket.Create(AName: String;
  const AOwner: TStorageTreeCollection);
begin
  FName:= AName;
  Owner:= AOwner;
  FStoreMode:= smText;
  inherited Create;
end;


end.


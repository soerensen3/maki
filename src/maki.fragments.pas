unit maki.fragments;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  fpjson,

  st.storage,
  st.properties,

  maki.node,
  maki.text;

type
  { TNodeTextFragmentSelector }

  TNodeTextFragmentSelector = class( TNodeTextFragment )
    private
      FDefines: TStringList;
      FInputName: String;
      FOutputOverride: String;
      FWildCard: Boolean;

      procedure SetInputName(AValue: String);

    public
      constructor Create;
      destructor Destroy; override;

      function InternalGetStringOutput( ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer ): String; override;
      function GetData( ANodeInstance: TNodeInstance; ADefines: TStringList ): TJSONData; override;

      property InputName: String read FInputName write SetInputName;
      property OutputOverride: String read FOutputOverride write FOutputOverride;
      property Defines: TStringList read FDefines;
  end;

  { TNodeTextFragmentInline }

  TNodeTextFragmentInline = class( TNodeTextFragment )
    private
      FText: String;

    public
      function InternalGetStringOutput( ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer ): String; override;
      function GetData(ANodeInstance: TNodeInstance; ADefines: TStringList): TJSONData; override;

      property Text: String read FText write FText;
  end;

  { TNodeTextFragmentWithChildren }

  TNodeTextFragmentWithChildren = class( TNodeTextFragment )
    private
      FFragments: TNodeTextFragmentList;

      function InternalGetStringOutput(ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer): String; override;
      function GetData(ANodeInstance: TNodeInstance; ADefines: TStringList): TJSONData; override;

    public
      constructor Create;
      destructor Destroy; override;

      property Fragments: TNodeTextFragmentList read FFragments write FFragments;
  end;

  { TNodeTextFragmentIfDef }

  TNodeTextFragmentIfDef = class( TNodeTextFragmentWithChildren )
    private
      FCheckDefs: TStringList;
      FInvert: Boolean;

    public
      constructor Create( ACheckDefs: String; AInvert: Boolean );
      destructor Destroy; override;

      function GetStringOutput( ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer ): String; override;
      function GetData(ANodeInstance: TNodeInstance; ADefines: TStringList): TJSONData; override;

      property CheckDefs: TStringList read FCheckDefs;
      property Invert: Boolean read FInvert write FInvert;
  end;

  { TNodeTextFragmentIfConn }

  TNodeTextFragmentIfConn = class( TNodeTextFragmentWithChildren )
    private
      FCheckDefs: TStringList;
      FInputName: String;
      FInvert: Boolean;

    public
      constructor Create( AInputName: String; AInvert: Boolean ); reintroduce;

      function GetStringOutput( ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer ): String; override;
      property InputName: String read FInputName;
      property Invert: Boolean read FInvert;
  end;

implementation

{ TNodeTextFragmentInline }

function TNodeTextFragmentInline.InternalGetStringOutput(
  ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer): String;
begin
  Result:= //'/*' + ASocketInstance.Node.CloneOf.Name + '.' + ASocketInstance.CloneOf.Name + '-->*/' +
           Text
           //+ '/*<--' + ASocketInstance.Node.CloneOf.Name + '.' + ASocketInstance.CloneOf.Name + '*/'
end;

function TNodeTextFragmentInline.GetData(ANodeInstance: TNodeInstance; ADefines: TStringList): TJSONData;
begin
  Result:= TJSONString.Create( Text );
end;

{ TNodeTextFragmentWithChildren }

function TNodeTextFragmentWithChildren.InternalGetStringOutput(ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer): String;
begin
  Result:= Fragments.GetStringOutput( ASocketInstance, ATextBuffer );
end;

function TNodeTextFragmentWithChildren.GetData(ANodeInstance: TNodeInstance; ADefines: TStringList): TJSONData;
begin
  Result:= Fragments.GetData( ANodeInstance, ADefines );
end;

constructor TNodeTextFragmentWithChildren.Create;
begin
  inherited;
  Fragments:= TNodeTextFragmentList.Create;
end;

destructor TNodeTextFragmentWithChildren.Destroy;
begin
  Fragments.Clear;
  Fragments.Free;
  inherited Destroy;
end;


{ TNodeTextFragmentIfDef }

constructor TNodeTextFragmentIfDef.Create(ACheckDefs: String; AInvert: Boolean);
begin
  inherited Create;
  FCheckDefs:= TStringList.Create;
  FCheckDefs.Text:= ACheckDefs;
  FInvert:= AInvert;
end;

destructor TNodeTextFragmentIfDef.Destroy;
begin
  FCheckDefs.Free;
  inherited Destroy;
end;

function TNodeTextFragmentIfDef.GetStringOutput(ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer ): String;

  function FindAllDefines: Boolean;
  var
    S: String;
  begin
    Result:= True;
    for S in CheckDefs do
      if ( ATextBuffer.Defines.IndexOf( S ) < 0 ) then begin
        //log_info( 'ifdef failed: ' + S + ' was not defined!' );
        Result:= False;
        Break;
      end;
  end;

  function FindAllDefinesInverted: Boolean;
  var
    S: String;
  begin
    Result:= True;
    for S in CheckDefs do
      if ( ATextBuffer.Defines.IndexOf( S ) > 0 ) then begin
        //log_info( 'ifndef failed: ' + S + ' was defined!' );
        Result:= False;
        Break;
      end;
  end;

var
  condition: Boolean;
begin
  //WriteLn( 'IFDEF ', Caption, ' defines=', Buffer.Defines.CommaText, ' defined?=', Buffer.Defines.IndexOf( Caption ) > -1 );
  if ( Invert ) then
    condition:= FindAllDefinesInverted
  else
    condition:= FindAllDefines;
  if ( condition ) then
    Result:= InternalGetStringOutput( ASocketInstance, ATextBuffer )
  else
    Result:= '';

  {else
    WriteLn( 'ifdef failed: ', Caption, ' was not defined!' );}
end;

function TNodeTextFragmentIfDef.GetData(ANodeInstance: TNodeInstance; ADefines: TStringList): TJSONData;
function FindAllDefines: Boolean;
var
  S: String;
begin
  Result:= True;
  for S in CheckDefs do
    if ( ADefines.IndexOf( S ) < 0 ) then begin
      Result:= False;
      Break;
    end;
end;

function FindAllDefinesInverted: Boolean;
var
  S: String;
begin
  Result:= True;
  for S in CheckDefs do
    if ( ADefines.IndexOf( S ) > 0 ) then begin
      Result:= False;
      Break;
    end;
end;

var
condition: Boolean;
begin
  //WriteLn( 'IFDEF ', Caption, ' defines=', Buffer.Defines.CommaText, ' defined?=', Buffer.Defines.IndexOf( Caption ) > -1 );
  if ( Invert ) then
    condition:= FindAllDefinesInverted
  else
    condition:= FindAllDefines;

  if ( condition ) then
    Result:= inherited GetData( ANodeInstance, ADefines )
  else
    Result:= nil;
end;

{ TNodeTextFragmentIfConn }

constructor TNodeTextFragmentIfConn.Create(AInputName: String; AInvert: Boolean );
begin
  inherited Create;
  FInputName:= AInputName;
  FInvert:= AInvert;
end;

function TNodeTextFragmentIfConn.GetStringOutput(ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer): String;
var
  inp: TNodeSocketInstance;
  condition: Boolean;
begin
  {Result:= '';
  inp:= ASocketInstance.Node.Outputs[ InputName ];
  if ( not Assigned( inp )) then
    raise Exception.Create( 'Input ' + InputName + ' not found in ' + ASocketInstance.Node.NodeType.Name );
  condition:= inp is TStorageTreeReference;

  if ( Invert ) then
    condition:= not condition;

  if ( condition ) then
    Result:= inherited GetStringOutput( ASocketInstance, ATextBuffer );}
end;

{ TNodeTextFragmentSelector }

procedure TNodeTextFragmentSelector.SetInputName(AValue: String);
begin
  if FInputName=AValue then Exit;
  FInputName:=AValue;
  FWildCard:= FInputName.Contains( '*' );
end;

constructor TNodeTextFragmentSelector.Create;
begin
  inherited Create;
  FDefines:= TStringList.Create;
end;

destructor TNodeTextFragmentSelector.Destroy;
begin
  FDefines.Free;
  inherited Destroy;
end;

function TNodeTextFragmentSelector.InternalGetStringOutput(ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer): String;
var
  i: Integer;
  Defs: String;
  Socket: TNodeSocketInstanceInput;
  Socket2: TNodeSocketInstance;
  Inputs, Types, Libs: gStorageTreeObjectDict < TStorageTreeReference >;
  JSON: TJSONData;
//  T: TP3DShaderNodeType;

begin
  Defs:= ATextBuffer.Defines.Text;
  ATextBuffer.Defines.AddStrings( Defines );
  Result:= '';
  if ( FWildCard ) then
    (*try
      Inputs:= TP3DNodeSocketClonePointerList.Create( 'Inputs' );
      Inputs.Root:= ASocketInstance.Root;
      Inputs.Context:= ASocketInstance;
      Types:= TP3DNodeSocketClonePointerList.Create( 'Types' );
      Types.Root:= ASocketInstance.Root;
      Types.Context:= ASocketInstance;
      Libs:= TP3DNodeSocketPointerList.Create( 'Libs' );
      Libs.Root:= ASocketInstance.Root;
      Libs.Context:= ASocketInstance;
      for Socket in ASocketInstance.Node.Inputs.EnumerateWildcard( InputName ) do
        TP3DShaderNodeSocketClone( Socket ).GetAllInputs( Buffer, OutputOverride, Inputs, Types, Libs );
      for Socket in Types.Reversed do
        begin
          T:= P3DShaderNodeLib.FindType( TP3DShaderNodeSocket( Socket.CloneOf ).SocketType );
          if ( Assigned( T )) then
            Result+= T.GetOutputAsType( Buffer, Socket, OutputOverride );
        end;
      //WriteLn( 'Inputs: ', Inputs.Count, ' Types: ', Types.Count );
      for Socket in Inputs.Reversed do
        Result+= TP3DShaderNodeSocket( Socket.CloneOf ).GetOutput( Buffer, Socket );
      for Socket2 in Libs{.Reversed} do
        Result+= TP3DShaderNodeSocket( Socket2 ).Fragments.GetStringOutput( Socket, Buffer );

    finally
      Types.Free;
      Inputs.Free;
      Libs.Free;
    end*)
    Result:= ':not implemented:'
  else if ( InputName = 'inject' ) then begin
    (*if ( Assigned( ASocketInstance.Connected )) then
      Result:= TP3DShaderNodeSocketClone( ASocketInstance.Connected ).GetOutput( Buffer )
    else
      Result:= ASocketInstance.ValueProp.AsString;*)
    Result:= ':not implemented:'
  end else begin
      if ( ASocketInstance.Node.Inputs.TryGet( InputName, Socket )) then begin
        JSON:= Socket.AsJSON;
        Result:= JSON.AsString;
        FreeAndNil( JSON );
      end else
        raise Exception.Create( 'Can not get output of fragment. Socket name "' + InputName + '" was not found in ' + ASocketInstance.Node.NodeType.Name + '!' );
    end;
  ATextBuffer.Defines.Text:= Defs;
end;

function TNodeTextFragmentSelector.GetData(ANodeInstance: TNodeInstance; ADefines: TStringList): TJSONData;
var
  ANewDefs: TStringList;
  Socket: TNodeSocketInstanceInput;
begin
  ANewDefs:= TStringList.Create;
  ANewDefs.AddStrings( ADefines );
  Result:= nil;
  if ( FWildCard ) then
    Result:= TJSONString.Create( ':not implemented:' )
  else if ( InputName = 'inject' ) then begin
    Result:= TJSONString.Create( ':not implemented:' )
  end else begin
    if ( ANodeInstance.Inputs.TryGet( InputName, Socket )) then
      Result:= Socket.GetData( ANodeInstance, ANewDefs )
    else
      raise Exception.Create( 'Can not get output of fragment. Socket name "' + InputName + '" was not found in ' + ANodeInstance.NodeType.Name + '!' );
    end;
  ANewDefs.Free;
end;

end.


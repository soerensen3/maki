unit maki.text;

{$mode delphi}

interface

uses
  Classes, SysUtils,

  fpjson,

  generics.collections,

  st.storage,
  st.properties,

  maki.node;

type

  { TNodeTextBuffer }{%region /fold}{ <-- Unfold for reference

  Used for the final text output. There is one buffer created for every output
  socket of the root node when compiled.

  }{%endregion}

  TNodeTextBuffer = class
    private
      FCode: String;
      FDefines: TStringList;
      FName: String;

    public
      constructor Create( AName: String );
      destructor Destroy; override;

    published
      property Code: String read FCode write FCode;
      property Name: String read FName write FName;
      property Defines: TStringList read FDefines write FDefines;
  end;

  {TNodeTextFragment}{%region /fold}{ <-- Unfold for reference

  Abstract class for code fragments of a node markdown file. These are stored
  as children in a node output socket and can should not exist outside of
  them.

  }{%endregion}

  TNodeTextFragment = class abstract
    public

      {TNodeTextFragment.GetStringOutput}{%region /fold}{ <-- Unfold for reference

      Parse the node and return string output, calls InternalGetStringOutput.
      Can be overriden to define a condition in an if statement.

      }{%endregion}
      function GetStringOutput( ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer ): String; virtual;
      function GetData( ANodeInstance: TNodeInstance; ADefines: TStringList ): TJSONData; virtual; abstract;

    protected

      {TNodeTextFragment.InternalGetStringOutput}{%region}{ <-- Unfold for reference

       Actual string output. Output of nested fragments or inline text goes here

      }{%endregion}
      function InternalGetStringOutput( ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer ): String; virtual; abstract;
  end;

  { TNodeTextFragmentList }

  TNodeTextFragmentList = class( TObjectList < TNodeTextFragment >)
    function GetStringOutput( ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer ): String; virtual;
    function GetData( ANodeInstance: TNodeInstance; ADefines: TStringList ): TJSONArray;
  end;

  { TNodeTextSocketType }

  TNodeTextSocketType = class ( TNodeSocketType )
    protected
      FFragments: TNodeTextFragmentList;

    public
      constructor Create(AName: String; ASocketType: TNodeSocketInstanceClass; const ADefaultValue: TJSONData; const AOwner: TStorageTreeCollection=nil); override;
      destructor Destroy; override;

      function GetData( ANodeInstance: TNodeInstance; ADefines: TStringList ): TJSONData; override;

      property Fragments: TNodeTextFragmentList read FFragments write FFragments;
  end;


implementation

{ TNodeTextSocketType }

constructor TNodeTextSocketType.Create(AName: String;
  ASocketType: TNodeSocketInstanceClass; const ADefaultValue: TJSONData;
  const AOwner: TStorageTreeCollection);
begin
  inherited Create(AName, ASocketType, ADefaultValue, AOwner);
  FFragments:= TNodeTextFragmentList.Create();
end;

destructor TNodeTextSocketType.Destroy;
begin
  FreeAndNil( FFragments );
  inherited Destroy;
end;

function TNodeTextSocketType.GetData(ANodeInstance: TNodeInstance; ADefines: TStringList): TJSONData;
begin
  Result:= Fragments.GetData( ANodeInstance, ADefines );
end;

{ TNodeTextBuffer }

constructor TNodeTextBuffer.Create(AName: String);
begin
  inherited Create;

  FName:= AName;

  Defines:= TStringList.Create;
  Defines.Sorted:= True;
  Defines.Duplicates:= dupIgnore;
end;

destructor TNodeTextBuffer.Destroy;
begin
  FreeAndNil( FDefines );

  inherited Destroy;
end;

function TNodeTextFragment.GetStringOutput(
  ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer): String;
begin
  Result:= InternalGetStringOutput( ASocketInstance, ATextBuffer );
end;

{ TNodeTextFragmentList }

function TNodeTextFragmentList.GetStringOutput(ASocketInstance: TNodeSocketInstance; ATextBuffer: TNodeTextBuffer): String;
var
  Fragment: TNodeTextFragment;
begin
  Result:= '';
  for Fragment in Self do
    Result+= Fragment.GetStringOutput( ASocketInstance, ATextBuffer );
end;

function TNodeTextFragmentList.GetData(ANodeInstance: TNodeInstance; ADefines: TStringList): TJSONArray;
var
  Fragment: TNodeTextFragment;
  Data: TJSONData;
begin
  Result:= TJSONArray.Create;
  for Fragment in Self do begin
    Data:= Fragment.GetData( ANodeInstance, ADefines );
    if ( Assigned( Data )) then
      Result.Add( Data );
  end;
end;


end.


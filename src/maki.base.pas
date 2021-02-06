unit maki.base;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,

  maki.lists,
  maki.module;

type
  { TMakiError }

  TMakiError = record
    StringPos: Integer;
    CaretPos: TPoint;
    Len: Integer;
    Message: ShortString;
  end;

  TMakiErrorList = maki.lists.TList < TMakiError >;
  TMakiErrorType = ( metTokenizer, metParser );

const
  SkippingChars = [ ' ', #9  ];
  Bracket = ['(',')','[',']','{','}'];
  Symbol = [ ',','.', ':', '=', '+', '-', '*' ] + Bracket;
  Letter = [ 'a'..'z','A'..'Z', '_' ];
  Number = [ '1'..'9','0' ];
  HexNumber = Number + [ 'A'..'F' ];
  NumberScientific = 'e';
  NumberDecimalDelim = '.';
  Alphanumeric = Letter + Number;
  StringDelim = '''';
  StartCommentDelim = '#';

type

  { TScanner }

  TScanner = class
    protected
      FBufferName: String;
      FCursor: Integer;
      function GetCount: Integer; virtual; abstract;

    public
      function PeekChars( const ACharCount: Integer = 1 ): AnsiString; virtual; abstract;
      function PopCharsUntil( AChars: TSysCharSet; const AStopOnEOL: Boolean = False ): AnsiString; virtual; abstract;
      function PopCharsWhile( AChars: TSysCharSet; const AStopOnEOL: Boolean = False ): AnsiString; virtual; abstract;
      function PopChars( const ACharCount: Integer = 1 ): AnsiString; virtual;
      function PopUntil( AString: AnsiString; const AStopOnEOL: Boolean = False ): AnsiString; virtual; abstract;

      procedure SkipChars( const ACharCount: Integer = 1 ); virtual; abstract;
      procedure SkipCharsWhile(AChars: TSysCharSet; const AStopOnEOL: Boolean = False; const ASkipToEOLOnComment: Boolean = True ); virtual; abstract;
      procedure SkipCharsToEOL(); virtual; abstract;
      procedure SkipUntil( AString: AnsiString; const AStopOnEOL: Boolean = False ); virtual; abstract;

      function GetCaretPos( const AStringPos: Integer = 0 ): TPoint; virtual; abstract;
      procedure Reset; virtual; abstract;
      function EOL: Boolean;
      function EOF: Boolean;

      property Cursor: Integer read FCursor;
      property Count: Integer read GetCount;
      property BufferName: String read FBufferName;
  end;


  TTokenKind = ( tkIdentifier, tkNumber, tkString, tkSymbol, tkIllegal, tkComment );

  { TToken }

  TToken = record
    Kind: TTokenKind;
    StartPos: Integer;
    EndPos: Integer;
    Text: ShortString;
  end;

  PToken = ^TToken;

  TTokenList = maki.lists.TList < TToken >;

  TMakiCompilerResult = class;

  { TTokenStream }

  TTokenStream = class( TTokenList )
    protected
      FCompilerResult: TMakiCompilerResult;

    public
      constructor Create( ACompilerResult: TMakiCompilerResult );

      procedure Error( AStringPos: Integer; AMessage: String );

      property CompilerResult: TMakiCompilerResult read FCompilerResult;
  end;

  TMakiCompilerErrorList = array [ TMakiErrorType ] of TMakiErrorList;

  { TMakiCompilerResult }

  TMakiCompilerResult = class
    private
      FScanner: TScanner;
      FErrors: TMakiCompilerErrorList;
      FModule: TMakiModule;
      FScannerOwned: Boolean;
      FTokens: TTokenStream;

    public
      constructor CreateFromScanner( AScanner: TScanner; const AScannerOwned: Boolean = True );
      destructor Destroy; override;

      property Scanner: TScanner read FScanner;
      property Tokens: TTokenStream read FTokens;
      property Module: TMakiModule read FModule;
      property Errors: TMakiCompilerErrorList read FErrors;
      property ScannerOwned: Boolean read FScannerOwned write FScannerOwned;
  end;

// Errors

resourcestring
  // Tokenizer
  MEUnknownStringLiteral = 'Unknown String literal';

  // Parser
  MEUnknownSocketType = 'Unknown type %s for socket %s';
  MEInputRedefined = 'An input with the name %s is already defined for the same node.';
  MEDuplicateReference = 'A library with the name %s is already referenced in the same node.';
  MeKeywordExpected = 'Keyword expected but %s found!';
  MEUnterminatedNode = 'Unterminated node!';
  MEKeywordOrEndnodeExpected = 'Keyword or endnode expected but %s found!';
  MEChunkWithoutNode = 'Cannot define chunk. You have to define a node first!';
  MEChunkWithoutOutput = 'Cannot define chunk. You have to define an output first!';
  MEOutputWithoutNode = 'Cannot define output before defining a node first!';
  MEMissingEndIf = 'Missing EndIf: level = %d';
  MEOutputRedefined = 'An output with the name %s is already defined for the same node';
  MESelectWithoutNode = 'Cannot use select before defining a node first!';
  MESelectWithoutOutput = 'Cannot use select before defining a node''s output first!';
  MESelectWildcardWithoutSocketName = '"." and socket name expected after wildcard!';
  MESocketNameExpected = 'Socket name or *. and socket name expected but symbol %s found!';
  MESelectSocketOrWildcardExpected = 'Identifier or wildcard expected after select keyword!';
  MESelectInputUnknown = 'Select with non existing input "%s" found in "%s"';
  MEIfdefWithoutNode = 'Cannot use ifdef before defining a node first!';
  MEIfdefWithoutOutput = 'Cannot use ifdef before defining a node''s output first!';
  MEElseWithoutIf = 'Else without matching if!';
  MEOutputEndWithoutNode = 'Endoutput found but no node is active!';
  MEOutputEndWithoutOutput = 'Endoutput found but no output is active!';

implementation

{ TScanner }

function TScanner.PopChars(const ACharCount: Integer): AnsiString;
begin
  Result:= PeekChars( ACharCount );
  SkipChars( ACharCount );
end;

function TScanner.EOL: Boolean;
begin
  Result:= PeekChars( Length( LineEnding )) = LineEnding;
end;

function TScanner.EOF: Boolean;
begin
  Result:= Cursor > Count;
end;


constructor TTokenStream.Create(ACompilerResult: TMakiCompilerResult);
begin
  inherited Create;
  FCompilerResult:= ACompilerResult;
end;

procedure TTokenStream.Error( AStringPos: Integer; AMessage: String );
var
  RecError: TMakiError;
begin
  RecError.StringPos:= AStringPos;
  RecError.CaretPos:= CompilerResult.Scanner.GetCaretPos( AStringPos );
  RecError.Message:= AMessage;
  CompilerResult.Errors[ metTokenizer ].Add( RecError );
end;

{ TMakiCompilerResult }

constructor TMakiCompilerResult.CreateFromScanner(AScanner: TScanner; const AScannerOwned: Boolean);
begin
  FScannerOwned:= AScannerOwned;
  FScanner:= AScanner;
  FErrors[ metTokenizer ]:= TMakiErrorList.Create;
  FErrors[ metParser ]:= TMakiErrorList.Create;
  FTokens:= TTokenStream.Create( Self );
  FModule:= TMakiModule.Create( AScanner.BufferName );
end;

destructor TMakiCompilerResult.Destroy;
begin
  if ( ScannerOwned ) then
    FreeAndNil( FScanner );
  FreeAndNil( FTokens );
  FreeAndNil( FErrors[ metTokenizer ]);
  FreeAndNil( FErrors[ metParser ]);

  inherited Destroy;
end;

end.


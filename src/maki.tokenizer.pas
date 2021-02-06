unit maki.tokenizer;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  StrUtils,

  maki.lists,

  st.storage,

  maki.base,
  maki.module,
  maki.node,
  maki.fragments,
  maki.text;

type
  ETokenizerException = class ( Exception );


  TTokenizerDebugCallback = procedure ( AToken: TToken ) of object;

  { TTokenizer }

  TTokenizer = class
    protected
      FActiveResult: TMakiCompilerResult;
      FDebugCallBack: TTokenizerDebugCallback;

      procedure Debug( AToken: TToken );

    public
      procedure Tokenize( ACompilerResult: TMakiCompilerResult );

      function ParseIdentifier: AnsiString;
      function ParseSymbol: AnsiString;
      function ParseString: AnsiString;
      function ParseNumber: AnsiString;
      function ParseIllegal: AnsiString;

      procedure SkipComment;
      procedure SkipSpaces( const ASkipEOL: Boolean = True );

      procedure Error(Msg: String);

    public
      property DebugCallBack: TTokenizerDebugCallback read FDebugCallBack write FDebugCallBack;
      property ActiveResult: TMakiCompilerResult read FActiveResult;
  end;

  function TokenKindToDisplayName( ATokenKind: TTokenKind ): String;

implementation

function TokenKindToDisplayName(ATokenKind: TTokenKind): String;
begin
  case ATokenKind of
    tkIdentifier: Result:= 'identifier';
    tkNumber: Result:= 'number';
    tkString: Result:= 'string';
    tkSymbol: Result:= 'symbol';
    tkIllegal: Result:= 'illegal character';
    tkComment: Result:= 'comment';
  end;
end;

procedure TTokenizer.SkipComment;
begin
  ActiveResult.Scanner.SkipCharsToEOL();
end;

procedure TTokenizer.Debug(AToken: TToken);
begin
  if ( Assigned( DebugCallBack )) then
    DebugCallBack( AToken );
end;


procedure TTokenizer.Tokenize(ACompilerResult: TMakiCompilerResult);
var
  Ch: String;
  StartPos: Integer;
  P: TPoint;

  procedure CreateToken(AStartPos, AEndPos: Integer; AKind: TTokenKind;
    AText: String);
  var
    Token: PToken;
  begin
    Token:= ActiveResult.Tokens.PtrTo( ActiveResult.Tokens.Add( default( TToken )));
    Token^.StartPos:= AStartPos;
    Token^.EndPos:= AEndPos;
    Token^.Kind:= AKind;
    Token^.Text:= AText;
    Debug( Token^ );
  end;

begin
  FActiveResult:= ACompilerResult;
  with ( ActiveResult ) do begin
    Scanner.Reset;

    SkipSpaces();
    while ( not Scanner.EOF ) do try
      StartPos:= Scanner.Cursor;
      Ch:= Scanner.PeekChars();
      if ( PosSet( Number, Ch ) > 0 ) then
        CreateToken( StartPos, Scanner.Cursor, tkNumber, ParseNumber )
      else if ( PosSet( Letter, Ch ) > 0 ) then
        CreateToken( StartPos, Scanner.Cursor, tkIdentifier, ParseIdentifier )
      else if ( StringDelim = Ch ) then
        CreateToken( StartPos, Scanner.Cursor, tkString, ParseString )
      else if ( StartCommentDelim = Ch ) then begin
        SkipComment;
        CreateToken( StartPos, Scanner.Cursor, tkComment, 'Comment' )
      end else if ( PosSet( Symbol, Ch ) > 0 ) then
        CreateToken( StartPos, Scanner.Cursor, tkSymbol, ParseSymbol )
      else begin
        CreateToken( StartPos, Scanner.Cursor, tkIllegal, ParseIllegal );
          Error( MEUnknownStringLiteral );
      end;
      SkipSpaces();
    except
      on E: Exception do begin // When using the Error function, an exception is raised so we end up here
        Tokens.Error( StartPos, E.Message );
        SkipSpaces();
      end;
    end;
  end;
end;

procedure TTokenizer.Error( Msg: String );
begin
  raise ETokenizerException.Create( Msg );
end;

procedure TTokenizer.SkipSpaces(const ASkipEOL: Boolean);
begin
  ActiveResult.Scanner.SkipCharsWhile( SkippingChars, not ASkipEOL );
end;

function TTokenizer.ParseIdentifier: AnsiString;
begin
  Result:= ActiveResult.Scanner.PopCharsWhile( Alphanumeric, True );
end;

function TTokenizer.ParseSymbol: AnsiString;
begin
  Result:= ActiveResult.Scanner.PopCharsWhile( Symbol, True );
end;

function TTokenizer.ParseString: AnsiString;
begin
  ActiveResult.Scanner.SkipChars(); // Skip first String Delim
  Result:= ActiveResult.Scanner.PopCharsUntil([ StringDelim ]{, True });
  ActiveResult.Scanner.SkipChars(); // Skip final String Delim
end;

function TTokenizer.ParseNumber: AnsiString;
begin
  if ( ActiveResult.Scanner.PeekChars( 2 ) = '0x' ) then begin
    Result:= ActiveResult.Scanner.PopChars( 2 ) +
             ActiveResult.Scanner.PopCharsWhile( HexNumber, True );
  end else
    Result:= ActiveResult.Scanner.PopCharsWhile( Number + [ NumberScientific, NumberDecimalDelim ], True );
end;

function TTokenizer.ParseIllegal: AnsiString;
begin
  Result:= ActiveResult.Scanner.PopCharsUntil( Alphanumeric + Symbol + SkippingChars + [ StringDelim, StartCommentDelim ], True );
end;


end.


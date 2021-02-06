unit maki.scanners;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  StrUtils,

  maki.base;

type

  { TScannerString }

  TScannerString = class ( TScanner )
    protected
      FSource: String;

      property Source: String read FSource;
      function GetCount: Integer; override;

    public
      constructor Create( S: String; ABufferName: String );

      function PeekChars( const ACharCount: Integer = 1 ): AnsiString; override;
      function PopCharsUntil( AChars: TSysCharSet; const AStopOnEOL: Boolean = False ): AnsiString; override;
      function PopCharsWhile( AChars: TSysCharSet; const AStopOnEOL: Boolean = False ): AnsiString; override;
      function PopUntil(AString: AnsiString; const AStopOnEOL: Boolean=False): AnsiString; override;
      procedure SkipChars(const ACharCount: Integer=1); override;
      procedure SkipCharsWhile(AChars: TSysCharSet; const AStopOnEOL: Boolean = False; const ASkipToEOLOnComment: Boolean = True ); override;
      procedure SkipCharsToEOL(); override;
      procedure SkipUntil(AString: AnsiString; const AStopOnEOL: Boolean=False); override;

      function GetCaretPos( const AStringPos: Integer = 0 ): TPoint; override;
      procedure Reset; override;
  end;

implementation


function SetOfCharsToString( AChars: TSysCharSet ): String;
var
  C: AnsiChar;
begin
  Result:= '';
  for C in AChars do
    if ( Result > '' ) then
      Result:= Result + ', ' + C
    else
      Result:= C;
end;

{ TScannerString }

function TScannerString.GetCount: Integer;
begin
  Result:= Length( Source );
end;

constructor TScannerString.Create(S: String; ABufferName: String);
begin
  FSource:= S;
  FCursor:= 1;
  FBufferName:= ABufferName;
end;

function TScannerString.PeekChars(const ACharCount: Integer): AnsiString;
begin
  Result:= Copy( Source, Cursor, ACharCount );

  //WriteLn( 'PeekChars = ', Result );
end;

function TScannerString.PopCharsUntil(AChars: TSysCharSet;
  const AStopOnEOL: Boolean): AnsiString;

  function TokenNotInChars: Boolean;
  begin
    Result:= PosSet( AChars, PeekChars()) = 0;
  end;

var
  EndRepeat: Boolean = False;
begin
  Result:= '';
  repeat
    //WriteLn( PeekChars( 10 ));
    if ( EOF ) then
      EndRepeat:= True
    else if ( AStopOnEOL and EOL ) then
      EndRepeat:= True
    else if TokenNotInChars then
      Result += PopChars()
    else
      EndRepeat:= True;
  until EndRepeat;
  //WriteLn( 'PopCharsUntil(', SetOfCharsToString( AChars ), ') = ', Result );
end;

function TScannerString.PopCharsWhile(AChars: TSysCharSet;
  const AStopOnEOL: Boolean): AnsiString;

  function TokenInChars: Boolean;
  begin
    Result:= EOL or ( PosSet( AChars, PeekChars()) > 0 );
  end;

var
  EndRepeat: Boolean = False;
begin
  Result:= '';
  repeat
    if ( EOF ) then
      EndRepeat:= True
    else if ( AStopOnEOL and EOL ) then
      EndRepeat:= True
    else if TokenInChars then // Token in Chars
      Result += PopChars()
    else
      EndRepeat:= True;
  until EndRepeat;
  //WriteLn( 'PopCharsWhile(', SetOfCharsToString( AChars ), ') = ', Result );
end;

function TScannerString.PopUntil( AString: AnsiString; const AStopOnEOL: Boolean ): AnsiString;
  function StringFound: Boolean;
  begin
    Result:= ( PeekChars( Length( AString )) = AString );
  end;

var
  EndRepeat: Boolean = False;
begin
  Result:= '';
  repeat
    if ( EOF ) then
      EndRepeat:= True
    else if ( AStopOnEOL and EOL ) then
      EndRepeat:= True
    else if ( not StringFound ) then
      Result += PopChars()
    else
      EndRepeat:= True;
  until EndRepeat;
  //WriteLn( 'PopUntil(', AString, ') = ', Result );
end;

procedure TScannerString.SkipChars(const ACharCount: Integer);
begin
  //WriteLn( 'SkipChars(', ACharCount, ') = ', PeekChars( ACharCount ));
  Inc( FCursor, ACharCount );
end;

procedure TScannerString.SkipCharsWhile(AChars: TSysCharSet;
  const AStopOnEOL: Boolean; const ASkipToEOLOnComment: Boolean);

  function TokenInChars( const AToken: Char ): Boolean;
  begin
    Result:= EOL or ( PosSet( AChars, AToken ) > 0 );
  end;

var
  EndRepeat: Boolean = False;
  Token: Char;
  TmpC: Integer;
begin
  TmpC:= Cursor;
  if ( EOF ) then
    exit;
  repeat
    Token:= PeekChars()[ 1 ];
//    if ( EOF ) then
//      EndRepeat:= True
    //else
    if ( AStopOnEOL and EOL ) then
      EndRepeat:= True
    {else if ( ASkipToEOLOnComment and ( Token = StartCommentDelim )) then
      SkipCharsToEOL()}
    else if TokenInChars( Token ) then // Token in Chars
      SkipChars()
    else
      EndRepeat:= True;
  until EndRepeat or EOF;
  //WriteLn( 'PopCharsWhile( ', SetOfCharsToString( AChars ), ') = ', Copy( Source, TmpC, Cursor - TmpC ));
end;

procedure TScannerString.SkipCharsToEOL();
begin
  while ( not ( EOL or EOF )) do
    SkipChars();
end;

procedure TScannerString.SkipUntil(AString: AnsiString;
  const AStopOnEOL: Boolean);
  function StringFound: Boolean;
  begin
    Result:= ( PosSet( AString[ 1 ], PeekChars()) = 0 ) and
             ( PeekChars( Length( AString )) = AString );
  end;

var
  EndRepeat: Boolean = False;
begin
  repeat
    if ( EOF ) then
      EndRepeat:= True
    else if ( AStopOnEOL and EOL ) then
      EndRepeat:= True
    else if ( not StringFound ) then
      SkipChars()
    else
      EndRepeat:= True;
  until EndRepeat;
end;

function TScannerString.GetCaretPos(const AStringPos: Integer): TPoint;
var
  n, i, StringPos: Integer;
begin
  i:= 0;
  n:= 0;

  if ( AStringPos < 1 ) then
    StringPos:= Cursor
  else
    StringPos:= AStringPos;
  repeat
    Result.y:= n;
    n:= PosEx( LineEnding, Source, n + 1 );
    Inc( i );
  until ( n = 0 ) or ( n > StringPos );
  Result:= Point( StringPos - Result.y, i );
end;

procedure TScannerString.Reset;
begin
  FCursor:= 1;
end;

end.


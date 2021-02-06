unit maki.parser;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  StrUtils,
  Math,

  fpjson,

  st.storage,

  maki.base,
  maki.lists,
  maki.module,
  maki.node,
  maki.fragments,
  maki.text,
  maki.tokenizer;

type
  TParserDebugToken = ( pdtNode, pdtInput, pdtOutput, pdtChunk, pdtSelector );
  TParserDebugCallback = procedure ( AStartPos, AEndPos: Integer; AToken: TParserDebugToken ) of object;

  EParserException = class ( Exception );

  TIfStackEntry = record
    TokenPos: Integer;
    IfFragment: TNodeTextFragmentWithChildren;
  end;

  { TParser }

  TParser = class
    protected
      FActiveTokenStartPos: Integer;
      FActiveNode: TNodeType;
      FActiveSocket: TNodeTextSocketType;
      FActiveModule: TMakiModule;
      FActiveFragmentRoot: TNodeTextFragmentList;
      FActiveResult: TMakiCompilerResult;
      FDebugCallBack: TParserDebugCallback;
      FCursor: Cardinal;
      FCompilerResult: TMakiCompilerResult;
      IfStack: array of TIfStackEntry;

      procedure Debug( AStartPos, AEndPos: Integer; AToken: TParserDebugToken );

      property ActiveNode: TNodeType read FActiveNode write FActiveNode;
      property ActiveSocket: TNodeTextSocketType read FActiveSocket write FActiveSocket;
      property ActiveResult: TMakiCompilerResult read FActiveResult write FActiveResult;
      property ActiveFragmentRoot: TNodeTextFragmentList read FActiveFragmentRoot write FActiveFragmentRoot;
      property ActiveTokenStartPos: Integer read FActiveTokenStartPos write FActiveTokenStartPos;

    public
      function GetToken: PToken; inline;
      function GetTokenKind: TTokenKind; inline;
      function EOF: Boolean; inline;
      procedure NextToken; inline;
      procedure Reset; inline;

      procedure ParseChunk;
      procedure ParseElse;
      procedure ParseEndIf;
      procedure ParseIfConn(Negate: Boolean);
      procedure ParseIfDef(Negate: Boolean);
      procedure ParseNode;
      procedure ParseEndOutput;
      procedure ParseOutput;
      procedure ParseSelector;
      function ParseData: TJSONData;
      procedure ExpectEmptyIfStack;

      function Expect( ATokenKind: TTokenKind ): Boolean; overload;
      function Expect( ATokenKind: TTokenKind; AText: String ): Boolean; overload;
      function TryExpect( ATokenKind: TTokenKind ): Boolean; overload;
      function TryExpect( ATokenKind: TTokenKind; AText: String ): Boolean; overload;

      function ParseString( ARaiseException: Boolean = False ): AnsiString;
      function ParseIntNumber( ARaiseException: Boolean = False ): Integer;
      function ParseFloatNumber( ARaiseException: Boolean = False ): Single;
      function ParseBoolean( ARaiseException: Boolean = False ): Boolean;

      function GetSocketTypeByName( ASocketType: String ): TNodeSocketInstanceClass;

      procedure Parse( ACompilerResult: TMakiCompilerResult );
      procedure Error( AMsg: String; const ACursor: Integer = -1 );

    public
      property DebugCallBack: TParserDebugCallback read FDebugCallBack write FDebugCallBack;
      property Cursor: Cardinal read FCursor write FCursor;
  end;

implementation

{ TParserResult }

{procedure TParser.ParseComment;
begin
  Scanner.SkipCharsToEOL();
end;
}

procedure TParser.ParseNode;
var
  breakat: SizeInt;
  Tk: TToken;

  procedure ParseInput;
  var
    SocketName: String = '';
    SocketType: String = '';
    SocketData: TJSONData = nil;
    SocketVisible: Boolean = True;
    symb: String;
    Tp: TNodeSocketInstanceClass;
    NamePos: Integer;
  begin
    NextToken;

    NamePos:= GetToken^.StartPos;

    if ( Expect( tkIdentifier )) then
      SocketType:= GetToken^.Text;
    NextToken;

    if ( Expect( tkIdentifier )) then
      SocketName:= GetToken^.Text;

    NextToken;

    if ( TryExpect( tkSymbol, '=' )) then begin
      NextToken;
      SocketData:= ParseData;

//      if ( Assigned( SocketData )) then
//        WriteLn( SocketName, ' = ', SocketData.FormatJSON());
    end;

    SocketVisible:= SocketName[ 1..2 ] = '__';

    Tp:= GetSocketTypeByName( SocketType );

    if ( not Assigned( Tp )) then begin
      Cursor:= Cursor - 1;
      Error( Format( MEUnknownSocketType, [ SocketType, SocketName ]), Cursor - 1 );
    end;
    if ( Assigned( ActiveNode.Inputs.GetNodeByName( SocketName ))) then begin
      Cursor:= Cursor - 1;
      Error( Format( MEInputRedefined, [ SocketName ]));
    end;

    TNodeSocketType.Create( SocketName, Tp, SocketData, ActiveNode.Inputs ).SourcePos:= ActiveResult.Scanner.GetCaretPos( NamePos );
    //CreateSocketFromType( GlobalActiveNode, nsdInput, SocketType, SocketName, False, SocketVisible, SocketDefault );
  end;

  procedure ParseLib;
  var
    SocketName: String = '';
    NamePos: Integer;
  begin
    NextToken;

    NamePos:= GetToken^.StartPos;

    if ( Expect( tkIdentifier )) then
      SocketName:= GetToken^.Text;
    NextToken;

    if ( Assigned( ActiveNode.Libs.GetNodeByName( SocketName ))) then begin
      Cursor:= Cursor - 1;
      Error( Format( MEDuplicateReference, [ SocketName ]));
    end;
    TNodeSocketType.Create( SocketName, GetSocketTypeByName( 'text' ), nil, ActiveNode.Libs ).SourcePos:= ActiveResult.Scanner.GetCaretPos( NamePos );
    //CreateSocketFromType( GlobalActiveNode, nsdLib, 'text', SocketName, False, False );
  end;

  {procedure ParseType;
  var
    name: String;
  begin
    name:= ParseWord( True );

    if ( GlobalLibrary.Types.FindByName( name ) > -1 ) then
      Error( 'A type with the name ' + name + ' already exists!' );
    GlobalActiveNode:= TP3DShaderNodeType.Create( GlobalLibrary.Types );
    GlobalMarkdown.Types.Add( GlobalActiveNode );
    GlobalActiveNode.Name:= name;
    GlobalActiveOutput:= nil;
    CreateSocketFromType( GlobalActiveNode, nsdInput, 'text', 'inject', False, False, '' );
  end;}

  procedure NodeCase( Name: String );
  {        tkIdentifier:
            case ( idx( IndexStr( Tk.Text, idx_str ))) of
              idx_output: ParseOutput;
              idx_select: ParseSelector;
              idx_ifdef: ParseIfDef( False );
              idx_ifndef: ParseIfDef( True );
              idx_ifconn: ParseIfConn( False );
              idx_ifnconn: ParseIfConn( True );
              idx_else: ParseElse;
              idx_endif: ParseEndIf;

              idx_illegal:
                Error( 'Keyword or node expected but ' + Tk.Text + ' found!' );
            end;

          tkString:
            ParseChunk;}
  type
    idx = (
      idx_illegal =     -1,
      idx_input =        0,
      idx_lib =          1,
      idx_type =         2,
      idx_output =       3,
      idx_endoutput =    4,
      idx_select =       5,
      idx_ifdef =        6,
      idx_ifndef =       7,
      idx_ifconn =       8,
      idx_ifnconn =      9,
      idx_else =         10,
      idx_endif =        11
    );
  const
    idx_str: array[ idx_input..idx_endif ] of String = (
      'input',
      'lib',
      'type',
      'output',
      'endoutput',
      'select',
      'ifdef',
      'ifndef',
      'ifconn',
      'ifnconn',
      'else',
      'endif'
     );

  begin
    case ( idx( IndexStr( Name, idx_str ))) of
      idx_input:
        ParseInput;

      idx_lib:
        ParseLib;

      idx_type:
        ;
        //ParseType;
      idx_output:
        ParseOutput;

      idx_endoutput:
        ParseEndOutput;

      idx_select:
        ParseSelector;

      idx_ifdef:
        ParseIfDef( False );

      idx_ifndef:
        ParseIfDef( True );

      idx_ifconn:
        ParseIfConn( False );

      idx_ifnconn:
        ParseIfConn( True );

      idx_else:
        ParseElse;

      idx_endif:
        ParseEndIf;


      idx_illegal:
        Error( Format( MeKeywordExpected, [ TokenKindToDisplayName( Tk.Kind )]));
    end;
  end;

  {function NodeDebugOutput: String;
  var
    socket: TNodeSocketType;
  begin
    if ( not Assigned( ActiveNode )) then begin
      Result:= 'Error: Node not Assigned';
      exit;
    end;
    Result:= 'Node ' + ActiveNode.Name + LineEnding;
    for socket in ActiveNode.Inputs do
      Result+= 'input: ' + socket.Name + ' : ' + socket.SocketType.ClassName + LineEnding;
  end; }

var
  GoOn: Boolean = True;
  Name: String;
  NamePos: Integer;

begin
  ExpectEmptyIfStack;
  NextToken;

  NamePos:= GetToken^.StartPos;

  if ( Expect( tkIdentifier )) then begin
    Tk:= GetToken^; // Can not be nil because else EOF would be True
    Name:= Tk.Text;
  end;

  NextToken;

  ActiveNode:= TNodeType.Create( Name, ActiveResult.Module.Types );
  ActiveNode.SourcePos:= ActiveResult.Scanner.GetCaretPos( NamePos );
  ActiveSocket:= nil;


  while ( GoOn ) do begin
    try
      ActiveTokenStartPos:= Cursor;

      if ( EOF ) then begin
        GoOn:= False;
        Error( MEUnterminatedNode )

      end else begin

        Tk:= GetToken^; // Can not be nil because else EOF would be True

        case Tk.Kind of

          tkIdentifier:
            if ( Tk.Text = 'endnode' ) then
              GoOn:= False
            else
              NodeCase( Tk.Text );

          tkSymbol, tkIllegal, tkNumber:
            Error( Format( MEKeywordOrEndnodeExpected, [ TokenKindToDisplayName( Tk.Kind )]));

          tkString:
            ParseChunk;

          tkComment:
            NextToken; // is not added to Tokens so this can be considered unreachable code

        end;
      end;
    except On E: Exception do
      NextToken;
    end;
  end;

  NextToken;
end;


procedure TParser.ParseChunk;
    procedure AddInline( S: String; Trim: Boolean );
    var
      chunk: TNodeTextFragmentInline;
    begin
      Assert( Assigned( ActiveFragmentRoot ), 'Internal error 201111: ActiveFragmentRoot not assigned!');

      chunk:= TNodeTextFragmentInline.Create;
      ActiveFragmentRoot.Add( chunk );

      if ( Trim ) then
        if ( RightStr( S, Length( LineEnding )) = LineEnding ) then
          SetLength( S, Length( S ) - Length( LineEnding ));
      //Delete one trailing line-ending if present
      //so the chunk terminator does not have to be in the same line
      //which is unintuive

      chunk.Text:= S;
    end;

var
  InlineText: String;
begin
  if ( Expect( tkString )) then begin
    InlineText:= GetToken^.Text;
  end;

  NextToken;

  if ( not Assigned( ActiveNode )) then
    Error( MEChunkWithoutNode );

  if ( not Assigned( ActiveSocket )) then
    Error( MEChunkWithoutOutput );

  //WriteLn( InlineText );

  //Debug( ActiveTokenStartPos, Scanner.Cursor, pdtChunk );

  AddInline( InlineText, True );
end;

procedure TParser.ParseOutput;
var
  SocketName: String = '';
  SocketType: String = '';
  SocketVisible: Boolean = True;
  Tp: TNodeSocketInstanceClass;
  NamePos: Integer;
begin
  ExpectEmptyIfStack;

  NextToken; // Skip output keyword

  if ( not Assigned( ActiveNode )) then
    Error( MEOutputWithoutNode );

  NamePos:= GetToken^.StartPos;

  if ( Expect( tkIdentifier )) then
    SocketType:= GetToken^.Text;

  NextToken;

  if ( Expect( tkIdentifier )) then
    SocketName:= GetToken^.Text;

  NextToken;

  if ( length( SocketName ) > 2 ) then
    SocketVisible:= SocketName[ 1..2 ] = '__';

  Tp:= TNodeSocketInstanceOutput; //GetSocketTypeByName( SocketType );
  if ( not Assigned( Tp )) then begin
    Cursor:= Cursor - 1; // One Token is skipped after an error is raised
    Error( Format( MEUnknownSocketType, [ SocketType, SocketName ]), Cursor - 1 );
  end;

  if ( Assigned( ActiveNode.Outputs.GetNodeByName( SocketName ))) then begin
    Cursor:= Cursor - 1; // One Token is skipped after an error is raised
    Error( Format( MEOutputRedefined, [ SocketName ]));
  end;

  ActiveSocket:= TNodeTextSocketType.Create( SocketName, Tp, nil, ActiveNode.Outputs );
  ActiveSocket.SourcePos:= ActiveResult.Scanner.GetCaretPos( NamePos );
  ActiveFragmentRoot:= ActiveSocket.Fragments;
  //Debug( ActiveTokenStartPos, Scanner.Cursor, pdtOutput );
end;

procedure TParser.ParseSelector;
var
  SocketName: String = '';
  OutputOverride: String = '';
  chunk: TNodeTextFragmentSelector;
begin
  NextToken; // skip select keyword

  if ( not Assigned( ActiveNode )) then
    Error( MESelectWithoutNode );

  if ( not Assigned( ActiveFragmentRoot )) then
    Error( MESelectWithoutOutput );

  if ( not EOF ) then begin
    if ( GetTokenKind = tkSymbol ) then begin
      SocketName:= GetToken^.Text;
      if ( SocketName = '*' ) then
        Error( MESelectWildcardWithoutSocketName )

      else if ( SocketName = '*.' ) then begin
        NextToken;

        if ( Expect( tkIdentifier )) then
          OutputOverride:= GetToken^.Text;

      end else
        Error( Format( MESocketNameExpected, [ SocketName ]));

      NextToken;

    end else if ( Expect( tkIdentifier )) then begin
      SocketName:= GetToken^.Text;

      NextToken;
      if ( TryExpect( tkSymbol, '.' )) then begin
        NextToken;
        if ( Expect( tkIdentifier )) then
          OutputOverride:= GetToken^.Text;
        NextToken;
      end
    end else
      Error( MESelectSocketOrWildcardExpected );

    chunk:= TNodeTextFragmentSelector.Create;
    ActiveFragmentRoot.Add( chunk );
    chunk.InputName:= SocketName;
    chunk.OutputOverride:= OutputOverride;

    if ( TryExpect( tkSymbol, ':' )) then begin
      NextToken;

      if ( Expect( tkIdentifier )) then
        chunk.Defines.Add( GetToken^.Text );

      NextToken;

      if ( TryExpect( tkSymbol, ',' )) then
        while ( TryExpect( tkSymbol, ',' )) do begin
          NextToken;

          if ( Expect( tkIdentifier )) then
            chunk.Defines.Add( GetToken^.Text );

          NextToken;
        end;
    end;

    if (( SocketName <> '*.' ) and
        ( not Assigned( ActiveNode.Inputs.GetNodeByName( SocketName )))) then
      Error( Format( MESelectInputUnknown, [ SocketName, ActiveNode.Name ]), Cursor - 1 );
//    Debug( ActiveTokenStartPos, Scanner.Cursor, pdtSelector );
  end;
end;

function TParser.ParseData: TJSONData;
  function ParseArray: TJSONArray;
  var
    GoOn: Boolean = True;
    Data: TJSONData;
  begin
    Result:= TJSONArray.Create;

    while GoOn do begin
      NextToken;

      Data:= ParseData;
      if ( Assigned( Data )) then
        TJSONArray( Result ).Add( Data );

      if ( TryExpect( tkSymbol, ']' )) then
        GoOn:= False
      else
        Expect( tkSymbol, ',' );
    end;
  end;

begin
  Result:= nil;
  try
    if ( TryExpect( tkSymbol, '[' )) then begin
      Result:= ParseArray;
    end else if ( TryExpect( tkNumber )) then begin
      if ( Pos( '.', GetToken^.Text ) > 0 ) then
        Result:= TJSONFloatNumber.Create( ParseFloatNumber( True ))
      else
        Result:= TJSONIntegerNumber.Create( ParseIntNumber( True ));
    end else if ( TryExpect( tkIdentifier )) then begin
      Result:= TJSONBoolean.Create( ParseBoolean( True ))
    end else if ( TryExpect( tkString )) then begin
      Result:= TJSONString.Create( ParseString( True ))
    end;
  except On E: Exception do
    Error( 'Parsing of data failed: ' + E.Message );
  end;
  NextToken;
end;

procedure TParser.ExpectEmptyIfStack;
var
  i: Integer;
begin
  try
    if ( Length( IfStack ) > 0 ) then begin
      for i:= 0 to High( IfStack ) do try
        Error( Format( MEMissingEndIf, [ IntToStr( Length( IfStack ))]));
      except
        on E: Exception do;
      end;

      SetLength( IfStack, 0 );
    end;

  except
    on E: Exception do;
  end;
end;

function TParser.Expect(ATokenKind: TTokenKind): Boolean;
begin
  Result:= TryExpect( ATokenKind );
  if ( not Result ) then
    Error( Format( '%s expected but %s %s found!', [ TokenKindToDisplayName( ATokenKind ), TokenKindToDisplayName( ActiveResult.Tokens[ Cursor ].Kind ), ActiveResult.Tokens[ Cursor ].Text ]));
end;

function TParser.Expect(ATokenKind: TTokenKind; AText: String): Boolean;
begin
  Result:= TryExpect( ATokenKind, AText );
  if ( not Result ) then
    Error( Format( '%s expected but %s found!', [ AText, ActiveResult.Tokens[ Cursor ].Text ]));
end;

function TParser.TryExpect(ATokenKind: TTokenKind): Boolean;
begin
  Assert( Assigned( ActiveResult.Tokens ));
  if ( EOF ) then begin
    Error( Format( '%s expected but end of file found!', [ TokenKindToDisplayName( ATokenKind )]));
    Exit( False );
  end;
  Result:= ActiveResult.Tokens[ Cursor ].Kind = ATokenKind;
end;

function TParser.TryExpect(ATokenKind: TTokenKind; AText: String): Boolean;
begin
  Assert( Assigned( ActiveResult.Tokens ));

  if ( EOF ) then begin
    Error( Format( '%s expected but end of file found!', [ TokenKindToDisplayName( ATokenKind )]));
    Exit( False );
  end;

  Result:= ( ActiveResult.Tokens[ Cursor ].Kind = ATokenKind ) and ( ActiveResult.Tokens[ Cursor ].Text = AText );
end;

function TParser.GetToken: PToken;
begin
  if ( EOF ) then
    Result:= nil
  else
    Result:= ActiveResult.Tokens.PtrTo( Cursor );
end;

function TParser.GetTokenKind: TTokenKind;
begin
  if ( EOF ) then
    Result:= tkIllegal
  else
    Result:= ActiveResult.Tokens[ Cursor ].Kind;
end;

function TParser.EOF: Boolean;
begin
  Result:= Cursor >= ActiveResult.Tokens.Count;
end;

procedure TParser.NextToken;
begin
  Inc( FCursor );
end;

procedure TParser.Reset;
begin
  Cursor:= 0;
  ActiveNode:= nil;
  ActiveSocket:= nil;
  ActiveResult:= nil;
  ActiveFragmentRoot:= nil;
  ActiveTokenStartPos:= 0;
end;

procedure TParser.ParseIfDef( Negate: Boolean );
var
  IfDefs: String;
  IfDef: TNodeTextFragmentIfDef;
  GoOn: Boolean = True;
  IfStart: Cardinal;
begin
  IfStart:= Cursor;
  NextToken;

  if ( not Assigned( ActiveNode )) then
    Error( MEIfdefWithoutNode );

  if ( not Assigned( ActiveFragmentRoot )) then
    Error( MEIfdefWithoutOutput );

  Expect( tkSymbol, '(' );

  while GoOn do begin
    NextToken;

    if ( Expect( tkIdentifier )) then begin
      IfDefS:= GetToken^.Text;
      NextToken;
    end;

    if ( TryExpect( tkSymbol, ')' )) then
      GoOn:= False
    else
      Expect( tkSymbol, ',' );
  end;

  NextToken;

  IfDef:= TNodeTextFragmentIfDef.Create( IfDefs, Negate );
//  IfDef.CheckDefs.Text:= ReplaceStr( IfDefs, ',', LineEnding );
//  IfDef.Invert:= Negate;

  SetLength( IfStack, Length( IfStack ) + 1 );
  IfStack[ high( IfStack )].TokenPos:= IfStart;
  IfStack[ high( IfStack )].IfFragment:= IfDef;

  ActiveFragmentRoot.Add( IfDef );
  ActiveFragmentRoot:= IfDef.Fragments;
end;

procedure TParser.ParseIfConn( Negate: Boolean );
var
  IsString: Boolean;
  IfConnS: String;
  IfConn: TNodeTextFragmentIfConn;
  IfStart: Cardinal;
begin
  IfStart:= Cursor;
  NextToken;

  if ( not Assigned( ActiveNode )) then
    Error( MEIfdefWithoutNode );

  if ( not Assigned( ActiveFragmentRoot )) then
    Error( MEIfdefWithoutOutput );

  if ( Expect( tkSymbol, '(' )) then
    NextToken;

  if ( Expect( tkIdentifier )) then begin
    IfConnS:= GetToken^.Text;
    NextToken;
  end;

  Expect( tkSymbol, ')' );
  NextToken;

  IfConn:= TNodeTextFragmentIfConn.Create( IfConnS, Negate );

  SetLength( IfStack, Length( IfStack ) + 1 );
  IfStack[ high( IfStack )].TokenPos:= IfStart;
  IfStack[ high( IfStack )].IfFragment:= IfConn;

  ActiveFragmentRoot.Add( IfConn );
  ActiveFragmentRoot:= IfConn.Fragments;
end;

procedure TParser.ParseElse;
  function CopyInverted( Node: TNodeTextFragmentWithChildren ): TNodeTextFragmentWithChildren;
  begin
    if ( Node is TNodeTextFragmentIfConn ) then
      Result:= TNodeTextFragmentIfConn.Create( TNodeTextFragmentIfConn( Node ).InputName, TNodeTextFragmentIfConn( Node ).Invert )
    else if ( Node is TNodeTextFragmentIfDef ) then
      Result:= TNodeTextFragmentIfDef.Create( TNodeTextFragmentIfDef( Node ).CheckDefs.Text, TNodeTextFragmentIfDef( Node ).Invert )
    else
      Error( 'Unknown if type on top of IfStack: ' + Node.ClassName );
  end;

var
  l: Integer;
begin
  NextToken;
  l:= Length( IfStack ) - 1;
  if ( l < 0 ) then
    Error( MEElseWithoutIf )
  else begin
    if ( l >= 1 ) then
      ActiveFragmentRoot:= IfStack[ l - 1 ].IfFragment.Fragments
    else
      ActiveFragmentRoot:= ActiveSocket.Fragments;
    IfStack[ l ].IfFragment:= CopyInverted( IfStack[ l ].IfFragment );
    IfStack[ l ].TokenPos:= ActiveTokenStartPos;
    ActiveFragmentRoot.Add( IfStack[ l ].IfFragment );
    ActiveFragmentRoot:= IfStack[ l ].IfFragment.Fragments
  end;
end;

procedure TParser.ParseEndOutput;
var
  l: Integer;
begin
  NextToken;
  if ( not Assigned( ActiveNode )) then
    Error( MEOutputEndWithoutNode );

  if ( not Assigned( ActiveFragmentRoot )) then
    Error( MEOutputEndWithoutOutput )

  else begin
    ActiveSocket:= nil;
    ActiveFragmentRoot:= nil;
  end;
end;


procedure TParser.ParseEndIf;
var
  l: Integer;
begin
  NextToken;
  if ( not Assigned( ActiveNode )) then
    Error( 'Cannot use endif before defining a node first!' );

  if ( not Assigned( ActiveFragmentRoot )) then
    Error( 'Cannot use endif before defining a node''s output first!' );

  l:= Length( IfStack ) - 1;

  if ( l < 0 ) then
    Error( 'Endif without matching if!' )

  else begin
    SetLength( IfStack, l );
    Dec( l );
    if ( l >= 0 ) then
      ActiveFragmentRoot:= IfStack[ l ].IfFragment.Fragments
    else
      ActiveFragmentRoot:= ActiveSocket.Fragments;
  end;
end;

procedure TParser.Debug(AStartPos, AEndPos: Integer; AToken: TParserDebugToken);
begin
  if ( Assigned( DebugCallBack )) then
    DebugCallBack( AStartPos, AEndPos, AToken );
end;

procedure TParser.Parse( ACompilerResult: TMakiCompilerResult );
var
  Tk: TToken;

type
  idx = (
    idx_illegal = -1,
    idx_output = 0,
    idx_select = 1,
    idx_ifdef = 2,
    idx_ifndef = 3,
    idx_ifconn = 4,
    idx_ifnconn = 5,
    idx_else = 6,
    idx_endif = 7
  );
const
  idx_str: array[ idx_output..idx_endif ] of String = (
    'output',
    'select',
    'ifdef',
    'ifndef',
    'ifconn',
    'ifnconn',
    'else',
    'endif'
   );

begin
  Reset;

  ActiveResult:= ACompilerResult;
  Cursor:= 0;
  //GlobalActiveNode:= nil;

  SetLength( IfStack, 0 );

  while ( not EOF ) do begin
    ActiveTokenStartPos:= Cursor;

    Tk:= GetToken^; // Can not be nil because else EOF would be True

    try
      case Tk.Kind of

{        tkIdentifier:
          case ( idx( IndexStr( Tk.Text, idx_str ))) of
            idx_output: ParseOutput;
            idx_select: ParseSelector;
            idx_ifdef: ParseIfDef( False );
            idx_ifndef: ParseIfDef( True );
            idx_ifconn: ParseIfConn( False );
            idx_ifnconn: ParseIfConn( True );
            idx_else: ParseElse;
            idx_endif: ParseEndIf;

            idx_illegal:
              Error( 'Keyword or node expected but ' + Tk.Text + ' found!' );
          end;

        tkString:
          ParseChunk;}

        tkIdentifier:
          if ( Tk.Text = 'node' ) then
            ParseNode
          else
            Error( 'Node expected but ' + Tk.Text + ' found!' );

        tkSymbol:
            Error( 'Node expected but symbol found!' );

        tkNumber, tkIllegal, tkString:
          Error( 'Node expected but ' + TokenKindToDisplayName( Tk.Kind ) + ' found!' );

        tkComment:
          NextToken;

      end;

    except On E: Exception do
      NextToken;
    end;
  end;
  ExpectEmptyIfStack;
end;


procedure TParser.Error(AMsg: String; const ACursor: Integer);
var
  E: TMakiError;
  _Cursor: Cardinal;
begin
  E.Message:= AMsg;

  if ( ACursor < 0 ) then begin
    if ( not EOF ) then
      _Cursor:= Cursor
    else
      _Cursor:= ActiveResult.Tokens.Count - 1;
  end else
    _Cursor:= ACursor;

  E.StringPos:= ActiveResult.Tokens[ _Cursor ].StartPos;

  E.CaretPos:= ActiveResult.Scanner.GetCaretPos( E.StringPos );

  ActiveResult.Errors[ metParser ].Add( E );

  raise EParserException.Create( E.Message );
end;

function TParser.ParseString(ARaiseException: Boolean): AnsiString;
begin
  Result:= GetToken^.Text;
end;

function TParser.ParseIntNumber(ARaiseException: Boolean): Integer;
var
  NumTxt: String;
begin
  NumTxt:= GetToken^.Text;

  if ( not Integer.TryParse( NumTxt, Result )) then begin
    Result:= 0;
    if ( ARaiseException ) then
      Error( 'Integer number expected!' );
  end;
end;

function TParser.ParseFloatNumber(ARaiseException: Boolean): Single;
var
  NumTxt: String;
begin
  NumTxt:= GetToken^.Text;

  if ( not Single.TryParse( NumTxt, Result )) then begin
    Result:= 0;
    if ( ARaiseException ) then
      Error( 'Floating point number expected!' );
  end;
end;

function TParser.ParseBoolean(ARaiseException: Boolean): Boolean;
var
  S: String;
begin
  S:= GetToken^.Text;

  if ( S = 'true' ) then
    Result:= True
  else if ( S = 'false' ) then
    Result:= False
  else begin
    Result:= False;
    if ( ARaiseException ) then
      Error( Format( 'Boolean with either true or false expected but "%s" found.', [ S ]));
  end;
end;


function TParser.GetSocketTypeByName(ASocketType: String): TNodeSocketInstanceClass;
begin
  if ( ASocketType = 'text' ) then
    Result:= TNodeSocketInstanceString
  else if ( ASocketType = 'int' ) then
    Result:= TNodeSocketInstanceInt
  else if ( ASocketType = 'float' ) then
    Result:= TNodeSocketInstanceFloat
  else if ( ASocketType = 'bool' ) then
    Result:= TNodeSocketInstanceBoolean
  else if ( ASocketType = 'vec2' ) then  // For now
    Result:= TNodeSocketInstanceFloat
  else if ( ASocketType = 'vec3' ) then
    Result:= TNodeSocketInstanceFloat
  else if ( ASocketType = 'vec4' ) then
    Result:= TNodeSocketInstanceFloat
  else if ( ASocketType = 'color3' ) then
    Result:= TNodeSocketInstanceFloat
  else if ( ASocketType = 'color4' ) then
    Result:= TNodeSocketInstanceFloat
  else
    Result:= nil;
end;

end.


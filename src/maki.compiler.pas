unit maki.compiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  maki.base,
  maki.scanners,
  maki.tokenizer,
  maki.parser;

type
  { TMakiCompiler }

  TMakiCompiler = class
    private
      FParser: TParser;
      FTokenizer: TTokenizer;

    protected
      property Tokenizer: TTokenizer read FTokenizer;
      property Parser: TParser read FParser;

    public
      constructor Create;
      destructor Destroy; override;

      function CompileFromString( AText, ABufferName: String ): TMakiCompilerResult;
      function CompileFromFile( AFileName: String ): TMakiCompilerResult;
  end;

implementation

{ TMakiCompiler }

constructor TMakiCompiler.Create;
begin
  FTokenizer:= TTokenizer.Create;
  FParser:= TParser.Create;
end;

destructor TMakiCompiler.Destroy;
begin
  FreeAndNil( FTokenizer );
  FreeAndNil( FParser );
  inherited Destroy;
end;

function TMakiCompiler.CompileFromString(AText, ABufferName: String): TMakiCompilerResult;
var
  Scanner: TScannerString;
begin
  Scanner:= TScannerString.Create( AText, ABufferName );

  Result:= TMakiCompilerResult.CreateFromScanner( Scanner );

  Tokenizer.Tokenize( Result );
  Parser.Parse( Result );
end;

function TMakiCompiler.CompileFromFile(AFileName: String): TMakiCompilerResult;
var
  S: TStringList;
  Scanner: TScannerString;
begin
  S:= TStringList.Create;
  S.LoadFromFile( AFileName );
  Scanner:= TScannerString.Create( S.Text, AFileName );
  FreeAndNil( S );

  Result:= TMakiCompilerResult.CreateFromScanner( Scanner );

  Tokenizer.Tokenize( Result );
  Parser.Parse( Result);
end;






end.


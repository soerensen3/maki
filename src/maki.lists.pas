{
The MIT License (MIT)

Copyright (c) [2015] [Johannes Rosleff Soerensen]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
}

unit maki.lists;

{$mode delphi}

interface

uses
  Classes, SysUtils, Math;

type
    { TListEnumerator }

    TListEnumerator < T > = class
      private type
        TMoveNext = function ( var AIndex: Integer; out AItem: T ): Boolean of object;
      private
        FCurrent: T;
        FCurrentIdx: Integer;
        FMoveNext: TMoveNext;

      public
        constructor Create( AStartIndex: Integer; AMoveNext: TMoveNext );
        function MoveNext: Boolean;
        property Current: T read FCurrent;
        property CurrentIdx: Integer read FCurrentIdx;
    end;

    { TList }
    TItemListOnChangeAction = ( actAdd, actDelete, actClear, actAssignBefore, actAssignAfter, actSet );
    TItemListOnChangeEvent = procedure ( Sender: TPersistent; ItemIndex: Integer; Action: TItemListOnChangeAction ) of object;

    TList < T > = class
      type
        TItemList = TList < T >;
        TItemListOnChangeEvent = procedure ( Sender: TItemList; ItemIndex: Integer; Action: TItemListOnChangeAction ) of object;
        TItemListOnSetEvent = function ( Sender: TObject; ItemIndex: Integer; AValue: T ): Boolean of object;

        TItemArray = array [ 0..( MAXINT shr 8 )] of T;
        pItemArray = ^TItemArray;

      private
        FItems: pItemArray;
        FCount: Integer;
        FCapacity: Integer;
        FGrowth: Integer;
        FOnChange: TItemListOnChangeEvent;
        FOnSet: TItemListOnSetEvent;
        FSizeLimit: Integer;

        function GetItem( Index: Integer ): T;
        procedure SetCapacity( const Value: Integer );
        procedure SetCount( AValue: Integer );
        procedure SetGrowth( const Value: Integer );
        procedure SetItem( Index: Integer; const AValue: T );
        procedure Grow;
        procedure Shrink;
        function MoveNext( var AIndex: Integer; out AItem: T ): Boolean;

      protected
        function GetItemSize: Integer;

      public
        constructor Create; virtual;
        destructor Destroy; override;

        function Add( Item: T ): Integer; overload;
        function AddArray( Items: array of T ): Integer; overload;
        procedure Delete( Index: Integer ); overload;
        procedure Clear;
        function PtrTo( Index: Integer ): Pointer;
        function Ptr: Pointer;
        procedure AssignTo( Dest: TItemList ); reintroduce;
        function GetEnumerator(): TListEnumerator < T >;
        //function GetAsString( const MaxItems: Integer = 100 ): String;

        property Items[ Index: Integer ]: T read GetItem write SetItem; default;
        property Count: Integer read FCount write SetCount;
        property Capacity: Integer read FCapacity write SetCapacity;
        property Growth: Integer read FGrowth write SetGrowth;
        property SizeLimit: Integer read FSizeLimit;
        property OnChange: TItemListOnChangeEvent read FOnChange write FOnChange;
        property OnSet: TItemListOnSetEvent read FOnSet write FOnSet;
    end;

implementation

function TList < T >.Add( Item: T ): Integer;
begin
  if ( FCount = FCapacity ) then
    Grow;

  FItems^[ FCount ]:= Item;
  Result:= FCount;

  Inc( FCount );
  if ( Assigned( OnChange )) then
    OnChange( Self, Result, actAdd );
end;

function TList < T >.AddArray( Items: array of T ): Integer;
var
  i: Integer;
begin
  Result:= Count;
  for i:= Low( Items ) to High( Items ) do
    Add( Items[ I ]);
end;

procedure TList < T >.AssignTo( Dest: TList < T > );
begin
  if ( Assigned( Dest.OnChange )) then
    Dest.OnChange( Self, -1, actAssignBefore );
  Dest.Count:= Count;
  Move( FItems^, Dest.FItems^, SizeOf( T ) * Count );
  if ( Assigned( Dest.OnChange )) then
    Dest.OnChange( Self, -1, actAssignAfter );
end;

function TList < T >.GetEnumerator(): TListEnumerator < T >;
begin
  Result:= TListEnumerator < T >.Create( -1, MoveNext );
end;

procedure TList < T >.Clear;
begin
  if ( Assigned( OnChange )) then
    OnChange( Self, -1, actClear );

  FreeMem( FItems );
  FCount:= 0;
  FCapacity:= 0;
  FItems:= nil;
end;

constructor TList < T >.Create;
begin
  inherited Create;
  FItems:= nil;
  FCount:= 0;
  FCapacity:= 0;
  FGrowth:= 256;
  FSizeLimit:= SizeOf( TItemArray ) div SizeOf( T );
end;

procedure TList < T >.Delete( Index: Integer );
var
  i: Integer;
begin
  if ( Assigned( OnChange )) then
    OnChange( Self, Index, actDelete );

  Move( FItems^[ Index + 1 ], FItems^[ Index ], SizeOf( T ) * ( Count - 1 - Index )); //Move does a check for overlapping regions

  //for i:= Index to Count - 2 do
  //  FItems[ i ]:= FItems[ i + 1 ];
  FCount:= FCount - 1;
end;

destructor TList < T >.Destroy;
begin
  FreeMem( FItems );
  inherited;
end;

function TList < T >.GetItem( Index: Integer ): T;
begin
  if (( Index >= 0 ) and ( Index < FCount )) then
    Result:= FItems^[ Index ];
end;

procedure TList < T >.Grow;
begin
  FCapacity:= FCapacity + FGrowth;
  ReallocMem( FItems, FCapacity * SizeOf( T ));
end;

function TList < T >.PtrTo(Index: Integer): Pointer;
begin
  if ( Count > Index ) then
    Result:= @FItems[ Index ]
  else
    Result:= nil;
end;

procedure TList < T >.SetCount(AValue: Integer);
begin
  FCount:= AValue;
  while ( FCapacity < FCount ) do
    Grow;
  while ( FCapacity > FCount + FGrowth ) do
    Shrink;
end;

procedure TList < T >.SetCapacity( const Value: Integer );
begin
  FCapacity:= Value;
  if ( FCapacity < FCount ) then
    FCapacity:= FCount;
  ReallocMem( FItems, FCapacity * SizeOf( T ));
end;

procedure TList < T >.SetGrowth(const Value: Integer);
begin
  FGrowth:= Math.Max( 16, Value ); // Minimum Value 16
end;

procedure TList < T >.SetItem( Index: Integer; const AValue: T );
begin
  if ( Assigned( OnSet ) and ( not OnSet( Self, Index, AValue ))) then
    exit;

  if ( Assigned( OnChange )) then
    OnChange( Self, Index, actSet );

  FItems^[ Index ]:= AValue;
end;

procedure TList < T >.Shrink;
begin
  FCapacity:= Math.Max( 0, FCapacity - FGrowth );
  ReallocMem( FItems, FCapacity * SizeOf( T ));
end;

function TList < T >.Ptr: Pointer;
begin
  Result:= FItems;
end;

function TList < T >.GetItemSize: Integer;
begin
  Result:= SizeOf( T );
end;

function TList < T >.MoveNext( var AIndex: Integer; out AItem: T ): Boolean;
begin
  Inc( AIndex );
  Result:= AIndex < Count;
  if ( Result ) then
    AItem:= Items[ AIndex ]
  else
    AItem:= default( T );
end;


{ TList }

constructor TListEnumerator < T >.Create( AStartIndex: Integer; AMoveNext: TMoveNext );
begin
  inherited Create;
  FillByte( FCurrent, SizeOf( FCurrent ), 0 );
  FCurrentIdx:= AStartIndex;
  FMoveNext:= AMoveNext;
end;

function TListEnumerator < T >.MoveNext: Boolean;
begin
  Result:= FMoveNext( FCurrentIdx, FCurrent );
end;

end.

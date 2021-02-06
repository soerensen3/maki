unit maki.module;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  st.storage,

  maki.lists,

  maki.node;

type
  { TMakiModule }

  TMakiModule = class ( TStorageTreeObject )
    protected
      FTypes: gStorageTreeObjectDict<TNodeType>;

      procedure CreateProperties; override;

    published
      property Types: gStorageTreeObjectDict < TNodeType > read FTypes;
  end;

implementation



{ TModule }

procedure TMakiModule.CreateProperties;
begin
  inherited CreateProperties;
  FTypes:= gStorageTreeObjectDict < TNodeType >.Create( 'Types', Self );
end;

end.


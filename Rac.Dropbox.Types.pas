unit Rac.Dropbox.Types;

interface

uses
  System.Generics.Collections, System.SysUtils;

type TDropboxItem = class(TObject)
  private
    FName: string;
    FId: string;
    FPath: string;
    FPathLower: string;
    FSharedLink: string;
  protected
  public
    function IsFile: Boolean; virtual; abstract;
    function IsFolder: Boolean; virtual; abstract;
    property Name: string read FName write FName;
    property ID: string read FId write FId;
    property Path: string read FPath write FPath;
    property PathLower: string read FPathLower write FPathLower;
    property SharedLink: string read FSharedLink write FSharedLink;
end;

type TDropboxItems = class(TObjectList<TDropboxItem>)
  private
    FCursor: string;
    FHasMore: Boolean;
  public
    property Cursor: string read FCursor write FCursor;
    property HasMore: Boolean read FHasMore write FHasMore;
    function GetByID(ID: string): TDropboxItem; // return nil if not found

end;

type TDropboxFolder = class(TDropboxItem)
  protected
  protected
  public
    function IsFile: Boolean; override;
    function IsFolder: Boolean; override;
end;

type TDropboxFile = class(TDropboxItem)
  private
    FRev: string;
    FSize: UInt64;
    FModifiedServer: TDateTime;
    FModifiedClient: TDateTime;
    FIsDownloadable: Boolean;
  protected
  public
    function IsFile: Boolean; override;
    function IsFolder: Boolean; override;
    property Rev: string read FRev write FRev;
    property ModifiedClient: TDateTime read FModifiedClient write FModifiedClient;
    property ModifiedServer: TDateTime read FModifiedServer write FModifiedServer;
    property Size: UInt64 read FSize write FSize;
    property IsDownloadable: Boolean read FIsDownloadable write FIsDownloadable;
end;

implementation


{ TDropboxFile }

function TDropboxFile.IsFile: Boolean;
begin
  Result := True;
end;

function TDropboxFile.IsFolder: Boolean;
begin
  Result := False;
end;

{ TDropboxFolder }

function TDropboxFolder.IsFile: Boolean;
begin
  Result := False;
end;

function TDropboxFolder.IsFolder: Boolean;
begin
  Result := True;
end;

{ TDropboxItems }

function TDropboxItems.GetByID(ID: string): TDropboxItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].ID.Equals(ID) then
    begin
      Result := Items[i];
      Break;
    end;
end;

end.

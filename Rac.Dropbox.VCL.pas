unit Rac.Dropbox.VCL;

interface

uses
  Rac.Dropbox, System.SysUtils, Vcl.Edge;

const
  AUTHORIZE_URL = '%s/oauth2/authorize?client_id=%s&response_type=token&redirect_uri=http://localhost&state=%s';
  STATE_LENGTH = 64;

type TDropbox = class(TDropboxBase)
  private
    FState: string;
    procedure _OnTitleChange(Sender: TCustomEdgeBrowser; const ADocumentTitle: string);
  public
    procedure Authorize(AWebBrowser: TEdgeBrowser); reintroduce;
end;

implementation

{ TDropbox }

procedure TDropbox.Authorize(AWebBrowser: TEdgeBrowser);
var
  url: string;
begin
  if Assigned(AWebBrowser) then
  begin
    FState := GetState(STATE_LENGTH);
    url := Format(AUTHORIZE_URL, [DROPBOX_AUTHORIZE_URL, AppKey, FState]);
    AWebBrowser.OnDocumentTitleChanged := _OnTitleChange;
    AWebBrowser.Navigate(url);
  end;
end;

procedure TDropbox._OnTitleChange(Sender: TCustomEdgeBrowser; const ADocumentTitle: string);
var
  url: string;
begin
  url := Sender.LocationURL;
  if url.StartsWith(DROPBOX_RETURN_URL, true) then
  begin
    Sender.Stop;
    if IsReturnURLCorrect(FState, url) and Assigned(OnAuthorize) then
      if ParseReturnURL(url) then
        OnAuthorize(Self, true)
      else
        OnAuthorize(Self, false);
  end;
end;

end.

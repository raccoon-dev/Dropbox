unit Rac.Dropbox.FMX;

interface

uses
  Rac.Dropbox, FMX.WebBrowser, System.SysUtils;

const
  AUTHORIZE_URL = '%s/oauth2/authorize?client_id=%s&response_type=token&redirect_uri=http://localhost&state=%s';
  STATE_LENGTH = 64;

type TDropbox = class(TDropboxBase)
  private
    FState: string;
    procedure _OnDidFinishLoad(ASender: TObject);
  public
    procedure Authorize(AWebBrowser: TCustomWebBrowser); reintroduce;
end;


implementation

{ TDropbox }

procedure TDropbox.Authorize(AWebBrowser: TCustomWebBrowser);
var
  url: string;
begin
  if Assigned(AWebBrowser) then
  begin
    FState := GetState(STATE_LENGTH);
    url := Format(AUTHORIZE_URL, [DROPBOX_AUTHORIZE_URL, AppKey, FState]);
    AWebBrowser.OnDidFinishLoad := _OnDidFinishLoad;
    AWebBrowser.Navigate(url);
  end;
end;

procedure TDropbox._OnDidFinishLoad(ASender: TObject);
var
  url: string;
begin
  if (ASender is TCustomWebBrowser) then
  begin
    url := (ASender as TCustomWebBrowser).URL;
    if url.StartsWith(DROPBOX_RETURN_URL, true) then
    begin
      (ASender as TCustomWebBrowser).Stop;
      if IsReturnURLCorrect(FState, url) and Assigned(OnAuthorize) then
        if ParseReturnURL(url) then
          OnAuthorize(Self, true)
        else
          OnAuthorize(Self, false);
    end;
  end;
end;

end.

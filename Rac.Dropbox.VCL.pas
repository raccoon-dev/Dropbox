unit Rac.Dropbox.VCL;

interface

uses
  Rac.Dropbox, SHDocVw, System.SysUtils;

const
  AUTHORIZE_URL = '%s/oauth2/authorize?client_id=%s&response_type=token&redirect_uri=http://localhost&state=%s';
  STATE_LENGTH = 64;

type TDropbox = class(TDropboxBase)
  private
    FState: string;
    procedure _OnTitleChange(ASender: TObject; const Text: WideString);
  public
    procedure Authorize(AWebBrowser: TWebBrowser); reintroduce;
end;

implementation

{ TDropbox }

procedure TDropbox.Authorize(AWebBrowser: TWebBrowser);
var
  url: string;
begin
  if Assigned(AWebBrowser) then
  begin
    FState := GetState(STATE_LENGTH);
    url := Format(AUTHORIZE_URL, [DROPBOX_AUTHORIZE_URL, AppKey, FState]);
    AWebBrowser.Silent := True;
    AWebBrowser.OnTitleChange := _OnTitleChange;
    AWebBrowser.Navigate(url);
  end;
end;

procedure TDropbox._OnTitleChange(ASender: TObject; const Text: WideString);
var
  url: string;
begin
  if (ASender is TWebBrowser) then
  begin
    url := (ASender as TWebBrowser).LocationURL;
    if url.StartsWith(DROPBOX_RETURN_URL, true) then
    begin
      (ASender as TWebBrowser).Stop;
      if IsReturnURLCorrect(FState, url) and Assigned(OnAuthorize) then
        if ParseReturnURL(url) then
          OnAuthorize(Self, true)
        else
          OnAuthorize(Self, false);
    end;
  end;
end;

end.

unit Rac.Dropbox;

interface

uses
  System.Classes, System.SysUtils, REST.Types, REST.Client, System.JSON.Types,
  System.JSON.Readers, REST.Authenticator.OAuth, System.Net.HttpClient,
  Rac.Dropbox.Types;

const
  DROPBOX_RETURN_URL       = 'http://localhost';
  DROPBOX_AUTHORIZE_URL    = 'https://www.dropbox.com';
  DROPBOX_API_BASE_URL     = 'https://api.dropbox.com';
  DROPBOX_CONTENT_BASE_URL = 'https://content.dropboxapi.com';
  MAX_STATE_LENGTH         = 500;

{$REGION 'Classes'}

type TOnAuthorize = procedure(Sender: TObject; const IsAuthorized: Boolean) of object;

type TDropboxBase = class(TObject)
  private
    FClient: TRESTClient;
    FRequest: TRESTRequest;
    FResponse: TRESTResponse;
    FOAuth2: TOAuth2Authenticator;
    FOnauthorize: TOnAuthorize;
    FAppKey: string;
    function GetToken: string;
    procedure JsonToListFolder(Reader: TJsonReader; AList: TDropboxItems);
  protected
    function GetState(const ALength: Cardinal = MAX_STATE_LENGTH): string;
    function ParseReturnURL(Url: string): Boolean;
    function IsReturnURLCorrect(AState, AUrl: string): Boolean;
    function JsonBool(Value: Boolean): string;
    procedure ClearRequest(BaseURL{, Mime}: string; Method: TRESTRequestMethod; Command: string);
  public
    /// <summary>
    ///  AppKey is necessary to authorize - to retrieve Token from dropbox.
    /// </summary>
    constructor Create(const AAppKey, AToken: string);
    destructor  Destroy; override;

    // Commands

    /// <summary>
    ///  Retrieve Token from dropbox. It is abstract procedure. If you want use
    ///  authorization, use TDropbox from Rac.Dropbox.VCL or Rac.Dropbox.FMX.
    /// </summary>
    procedure Authorize(AWebBrowser: TPersistent); virtual; abstract;
    /// <summary>
    ///  Create new folder.
    /// </summary>
    function CreateFolder(ANewPath: string): Boolean;
    /// <summary>
    ///  Delete file or folder.
    /// </summary>
    function Delete(APath: string): Boolean; // delete file or folder
    /// <summary>
    ///  Download file from Dropbox and save it as ALocalPath.
    /// </summary>
    function Download(ADropboxPath: string; ALocalPath: string): Boolean;
    /// <summary>
    ///  Download all folder as zip archive and save it as ALocalPath.
    /// </summary>
    function DownloadZip(ADropboxPath: string; ALocalPath: string): Boolean;
    /// <summary>
    ///  Retrieve list of files and folders.
    /// </summary>
    function ListFolder(ADropboxPath: string; AList: TDropboxItems; ARecursive: Boolean = True): Boolean;
    /// <summary>
    ///  Upload ALocalPAth file to ADropboxPath folder. File size limit is 150[MB].
    /// </summary>
    function Upload(ALocalPath: string; ADropboxPath: string): Boolean;
    /// <summary>
    ///  Move file or folder. If ADropboxFromPath is folder, all folder content
    ///  will be moved as well.
    /// </summary>
    function Move(ADropboxFromPath: string; ADropboxToPath: string): Boolean;

    // Properties
    property Token: string read GetToken;
    property AppKey: string read FAppKey;

    // Events
    property OnAuthorize: TOnAuthorize read FOnauthorize write FOnAuthorize;
end;

{$ENDREGION} // Classes

implementation

const
  USER_AGENT = 'Raccoon Dropbox Client/1.0';
  MIME_JSON  = 'application/json';
  MIME_BINARY = 'application/octet-stream';

  CMD_CREATE_FOLDER = '/2/files/create_folder_v2';
  CMD_DELETE        = '/2/files/delete_v2';
  CMD_DOWNLOAD      = '/2/files/download';
  CMD_DOWNLOAD_ZIP  = '/2/files/download_zip';
  CMD_LIST_FOLDER   = '/2/files/list_folder';
  CMD_MOVE          = '/2/files/move_v2';
  CMD_UPLOAD        = '/2/files/upload';

{ TDropboxBase }

{$REGION 'Other functions'}

constructor TDropboxBase.Create(const AAppKey, AToken: string);
begin
  FAppKey := AAppKey;

  FClient   := TRESTClient.Create(DROPBOX_API_BASE_URL);
  FRequest  := TRESTRequest.Create(nil);
  FResponse := TRESTResponse.Create(nil);
  FOAuth2   := TOAuth2Authenticator.Create(nil);

  FOAuth2.AccessToken := AToken;
  FOAuth2.TokenType   := TOAuth2TokenType.ttBEARER;

  FClient.AcceptEncoding  := 'UTF-8';
  FClient.Authenticator   := FOAuth2;
  FClient.ContentType     := MIME_JSON;
  FClient.SecureProtocols := [THTTPSecureProtocol.TLS12, THTTPSecureProtocol.TLS11];
  FClient.UserAgent       := USER_AGENT;

{$IF Defined(DEBUG)}
//////////////////////////////////////////////
////////////// For debug only ////////////////
//////////////////////////////////////////////
////  FClient.ProxyServer := '127.0.0.1'; ////
////  FClient.ProxyPort   := 8080;        ////
//////////////////////////////////////////////
{$ENDIF}

  FRequest.Client   := FClient;
  FRequest.Response := FResponse;
end;

destructor TDropboxBase.Destroy;
begin
  FClient.Free;
  FRequest.Free;
  FResponse.Free;
  FOAuth2.Free;
  inherited;
end;

function TDropboxBase.GetState(const ALength: Cardinal): string;
const
  DICT = 'ABCDEFGHIJKLMNOPQRSTUWVXYZabcdefghijklmnopqrstuwvxyz1234567890';
var
  i: Integer;
  sb: TStringBuilder;
begin
  if ALength > MAX_STATE_LENGTH then
    raise Exception.Create(Format('Maximum state length is %d characters', [MAX_STATE_LENGTH]));

  sb := TStringBuilder.Create;
  try
    for i := 1 to ALength do
      sb.Append( DICT[ Random(High(DICT)) + Low(string) ] );
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TDropboxBase.GetToken: string;
begin
  Result := FOAuth2.AccessToken;
end;

function TDropboxBase.IsReturnURLCorrect(AState, AUrl: string): Boolean;
begin
  Result := AUrl.StartsWith(DROPBOX_RETURN_URL) and AUrl.Contains('state=' + AState);
end;

function TDropboxBase.JsonBool(Value: Boolean): string;
begin
  if Value then
    Result := 'true'
  else
    Result := 'false';
end;

procedure TDropboxBase.JsonToListFolder(Reader: TJsonReader;
  AList: TDropboxItems);
var
  Item: TDropboxItem;
  Prop: string;
  sVal: string;
begin
  Item := nil; // Remove warnig W1036 Variable 'Item' might not have been initialized
  while Reader.Read do
    if Reader.TokenType = TJsonToken.PropertyName then
    begin
      Prop := Reader.Value.AsString.ToLower;
      if Assigned(Item) then
      begin
        if Prop.Equals('name') then
          Item.Name := Reader.ReadAsString else
        if Prop.Equals('id') then
          Item.ID := Reader.ReadAsString else
        if Prop.Equals('path_display') then
          Item.Path := Reader.ReadAsString else
        if Prop.Equals('path_lower') then
          Item.PathLower := Reader.ReadAsString else
        if Item is TDropboxFile then
        begin
          if Prop.Equals('rev') then
            (Item as TDropboxFile).Rev := Reader.ReadAsString else
          if Prop.Equals('client_modified') then
            (Item as TDropboxFile).ModifiedClient := Reader.ReadAsDateTime else
          if Prop.Equals('server_modified') then
            (Item as TDropboxFile).ModifiedServer := Reader.ReadAsDateTime else
          if Prop.Equals('size') then
            (Item as TDropboxFile).Size := UInt64(Reader.ReadAsInt64) else
          if Prop.Equals('is_downloadable') and Reader.Read then
            (Item as TDropboxFile).IsDownloadable := Reader.Value.AsBoolean;
        end;
      end else
      begin
        if Prop.Equals('.tag') then
        begin
          sVal := Reader.ReadAsString.ToLower;
          if sVal.Equals('file') then
            Item := TDropboxFile.Create else
          if sVal.Equals('folder') then
            Item := TDropboxFolder.Create;
        end;
      end;
    end else
    if Reader.TokenType = TJsonToken.EndArray then
      Break else
    if Reader.TokenType = TJsonToken.StartObject then
      Item := nil else
    if Reader.TokenType = TJsonToken.EndObject then
    begin
      if Assigned(Item) then
        AList.Add(Item);
    end;
end;

function TDropboxBase.ParseReturnURL(Url: string): Boolean;
var
  i: Integer;
  sl: TStringList;
begin
  Result := False;
  i := Url.IndexOf('#');
  if i >= 0 then
  begin
    sl := TStringList.Create;
    try
      sl.Delimiter := '&';
      sl.DelimitedText := Url.Remove(0, i + 1);
      FOAuth2.AccessToken := sl.Values['access_token'];
      Result := not string.IsNullOrEmpty(Token);
    finally
      sl.Free;
    end;
  end;
end;

procedure TDropboxBase.ClearRequest(BaseURL: string;
  Method: TRESTRequestMethod; Command: string);
begin
  FClient.BaseURL := BaseURL;
  FRequest.Params.ClearAndResetID;
  FRequest.ClearBody;
  FRequest.Method := Method;
  FRequest.Resource := Command;
end;

{$ENDREGION} // Other functions

{$REGION 'Dropbox Commands'}

function TDropboxBase.CreateFolder(ANewPath: string): Boolean;
begin
  Result := False;
  ClearRequest(DROPBOX_API_BASE_URL,
               TRESTRequestMethod.rmPOST,
               CMD_CREATE_FOLDER);
  FRequest.Body.Add(Format('{"path": "/%s", "autorename": false}', [ANewPath]), TRESTContentType.ctAPPLICATION_JSON);
  FRequest.Execute;

  if FResponse.StatusCode = 200 then
  begin
    while FResponse.JSONReader.Read do
    begin
      if FResponse.JSONReader.TokenType = TJsonToken.PropertyName then
      begin
        Result := FResponse.JSONReader.Value.AsString.Equals('metadata');
        Break;
      end;
    end;
  end;
end;

function TDropboxBase.Delete(APath: string): Boolean;
begin
  Result := False;
  ClearRequest(DROPBOX_API_BASE_URL,
               TRESTRequestMethod.rmPOST,
               CMD_DELETE);
  FRequest.Body.Add(Format('{"path": "%s"}', [APath]), TRESTContentType.ctAPPLICATION_JSON);
  FRequest.Execute;

  if FResponse.StatusCode = 200 then
  begin
    while FResponse.JSONReader.Read do
    begin
      if FResponse.JSONReader.TokenType = TJsonToken.PropertyName then
      begin
        Result := FResponse.JSONReader.Value.AsString.Equals('metadata');
        Break;
      end;
    end;
  end;
end;

function TDropboxBase.DownloadZip(ADropboxPath, ALocalPath: string): Boolean;
var
  fileLocal: TFileStream;
  bytesStream: TBytesStream;
  param: TRESTRequestParameter;
begin
  ClearRequest(DROPBOX_CONTENT_BASE_URL,
               TRESTRequestMethod.rmPOST,
               CMD_DOWNLOAD_ZIP);
  param := FRequest.Params.AddHeader('Dropbox-API-Arg', Format('{"path": "%s"}', [ADropboxPath]));
  param.Options := param.Options + [TRESTRequestParameterOption.poDoNotEncode];
  FRequest.AddBody(string.Empty, TRESTContentType.ctAPPLICATION_OCTET_STREAM);

  FRequest.Execute;

  Result := FResponse.StatusCode = 200;

  if Result then
  begin
    fileLocal := TFileStream.Create(ALocalPath, fmCreate + fmShareExclusive);
    bytesStream := TBytesStream.Create(FResponse.RawBytes);
    try
      bytesStream.Seek(0, soFromBeginning);
      fileLocal.CopyFrom(bytesStream, bytesStream.Size);
    finally
      bytesStream.Free;
      fileLocal.Free;
    end;
  end;
end;

function TDropboxBase.Download(ADropboxPath: string; ALocalPath: string): Boolean;
var
  fileLocal: TFileStream;
  bytesStream: TBytesStream;
  param: TRESTRequestParameter;
begin
  ClearRequest(DROPBOX_CONTENT_BASE_URL,
               TRESTRequestMethod.rmPOST,
               CMD_DOWNLOAD);
  param := FRequest.Params.AddHeader('Dropbox-API-Arg', Format('{"path": "%s"}', [ADropboxPath]));
  param.Options := param.Options + [TRESTRequestParameterOption.poDoNotEncode];
  FRequest.AddBody(string.Empty, TRESTContentType.ctAPPLICATION_OCTET_STREAM);

  FRequest.Execute;

  Result := FResponse.StatusCode = 200;
  if Result then
  begin
    fileLocal := TFileStream.Create(ALocalPath, fmCreate + fmShareExclusive);
    bytesStream := TBytesStream.Create(FResponse.RawBytes);
    try
      bytesStream.Seek(0, soFromBeginning);
      fileLocal.CopyFrom(bytesStream, bytesStream.Size);
    finally
      bytesStream.Free;
      fileLocal.Free;
    end;
  end;
end;

function TDropboxBase.ListFolder(ADropboxPath: string; AList: TDropboxItems;
  ARecursive: Boolean): Boolean;
var
  sb: TStringBuilder;
  Prop: string;
begin
  if not Assigned(AList) then
    raise EArgumentNilException.Create('Empty destination list parameter');

  Result := False;
  AList.Clear;
  sb := TStringBuilder.Create;
  try
    ClearRequest(DROPBOX_API_BASE_URL,
                 TRESTRequestMethod.rmPOST,
                 CMD_LIST_FOLDER);
    sb.Append('{')
      .Append('"path": "').Append(ADropboxPath).Append('",')
      .Append('"recursive": ').Append(JsonBool(ARecursive)).Append(',')
      .Append('"include_deleted": ').Append(JsonBool(false)).Append(',')                     // Default value = false
      .Append('"include_has_explicit_shared_members": ').Append(JsonBool(false)).Append(',') // Default value = false
      .Append('"include_mounted_folders": ').Append(JsonBool(True)).Append(',')              // Default value = true
      .Append('"include_non_downloadable_files": ').Append(JsonBool(False))                  // Default value = true
      .Append('}');
    FRequest.Body.Add(sb.ToString, TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;

    if FResponse.StatusCode = 200 then
      while FResponse.JSONReader.Read do
        if FResponse.JSONReader.TokenType = TJsonToken.PropertyName then
        begin
          Prop := FResponse.JSONReader.Value.AsString;
          if Prop.Equals('entries') then
          begin
            Result := True;
            while FResponse.JSONReader.Read do
              if FResponse.JSONReader.TokenType = TJsonToken.StartArray then
              begin
                JsonToListFolder(FResponse.JSONReader, AList);
                Break;
              end;
          end else
          if Prop.Equals('cursor') then
            AList.Cursor := FResponse.JSONReader.ReadAsString else
          if Prop.Equals('has_more') and FResponse.JSONReader.Read then
            AList.HasMore := FResponse.JSONReader.Value.AsBoolean;
        end else
        if FResponse.JSONReader.TokenType = TJsonToken.EndObject then
          Break;
  finally
    sb.Free;
  end;
end;

function TDropboxBase.Move(ADropboxFromPath, ADropboxToPath: string): Boolean;
var
  sb: TStringBuilder;
begin
  Result := False;
  sb := TStringBuilder.Create;
  try
    ClearRequest(DROPBOX_API_BASE_URL,
                 TRESTRequestMethod.rmPOST,
                 CMD_MOVE);
    sb.Append('{')
      .Append('"from_path": "').Append(ADropboxFromPath).Append('",')
      .Append('"to_path": "').Append(ADropboxToPath).Append('",')
      .Append('"allow_shared_folder": ').Append(JsonBool(false)).Append(',') // Default value = false
      .Append('"autorename": ').Append(JsonBool(false)).Append(',')          // Default value = false
      .Append('"allow_ownership_transfer": ').Append(JsonBool(False))        // Default value = false
      .Append('}');
    FRequest.Body.Add(sb.ToString, TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;

    if FResponse.StatusCode = 200 then
    begin
      while FResponse.JSONReader.Read do
      begin
        if FResponse.JSONReader.TokenType = TJsonToken.PropertyName then
        begin
          Result := not FResponse.JSONReader.Value.AsString.StartsWith('error');
          Break;
        end;
      end;
    end;
  finally
    sb.Free;
  end;
end;

function TDropboxBase.Upload(ALocalPath, ADropboxPath: string): Boolean;
var
  fileLocal: TFileStream;
  param: TRESTRequestParameter;
begin
  Result := False;
  ClearRequest(DROPBOX_CONTENT_BASE_URL,
               TRESTRequestMethod.rmPOST,
               CMD_UPLOAD);
  param := FRequest.Params.AddHeader('Dropbox-API-Arg', Format('{"path": "%s"}', [ADropboxPath]));
  param.ContentType := TRESTContentType.ctAPPLICATION_JSON;
  param.Options := param.Options + [TRESTRequestParameterOption.poDoNotEncode];

  fileLocal := TFileStream.Create(ALocalPath, fmOpenRead + fmShareDenyWrite);
  try
    fileLocal.Seek(0, soFromBeginning);
    FRequest.AddBody(fileLocal, TRESTContentType.ctAPPLICATION_OCTET_STREAM);
    FRequest.Execute;
  finally
    fileLocal.Free;
  end;

  if FResponse.StatusCode = 200 then
  begin
    while FResponse.JSONReader.Read do
    begin
      if FResponse.JSONReader.TokenType = TJsonToken.PropertyName then
      begin
        Result := not FResponse.JSONReader.Value.AsString.StartsWith('error');
        Break;
      end;
    end;
  end;
end;

{$ENDREGION} // Dropbox Commands

initialization
  Randomize;

end.


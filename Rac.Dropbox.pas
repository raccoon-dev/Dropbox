unit Rac.Dropbox;

interface

{.$DEFINE DUMP_RESULTS}

uses
  System.Classes, System.SysUtils, REST.Types, REST.Client, System.JSON.Types,
  System.JSON.Readers, REST.Authenticator.OAuth, System.Net.HttpClient,
  Rac.Dropbox.Types
  {$IF Defined(DEBUG) AND Defined(DUMP_RESULTS)}, System.IOUtils{$ENDIF};

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
{$IF Defined(DEBUG) AND Defined(DUMP_RESULTS)}
    procedure DumpResults(const Description: string; Response: TRESTResponse);
{$ENDIF}
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
    function ListFolder(ADropboxPath: string; AList: TDropboxItems; ARecursive: Boolean = True; GetSharedLinks: Boolean = True): Boolean;
    /// <summary>
    ///  Upload ALocalPAth file to ADropboxPath folder. File size limit is 150[MB].
    /// </summary>
    function Upload(ALocalPath: string; ADropboxPath: string): Boolean;
    /// <summary>
    ///  Move file or folder. If ADropboxFromPath is folder, all folder content
    ///  will be moved as well.
    /// </summary>
    function Move(ADropboxFromPath: string; ADropboxToPath: string): Boolean;
    /// <summary>
    ///  Retrieve list of shared links in format ID=URL
    /// </summary>
    function ListSharedLinks(SharedLinks: TStrings): Boolean;
    /// <summary>
    ///  Revoke shared link by URL of shared link
    /// </summary>
    function RevokeSharedLink(SharedLink: String): Boolean;
    /// <summary>
    ///  Create shared link from Dropbox file or folder.
    ///  Return shared link or empty string if error.
    /// </summary>
    function CreateSharedLink(DropboxPath: String): String;

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

  CMD_CREATE_FOLDER      = '/2/files/create_folder_v2';
  CMD_DELETE             = '/2/files/delete_v2';
  CMD_DOWNLOAD           = '/2/files/download';
  CMD_DOWNLOAD_ZIP       = '/2/files/download_zip';
  CMD_LIST_FOLDER        = '/2/files/list_folder';
  CMD_MOVE               = '/2/files/move_v2';
  CMD_UPLOAD             = '/2/files/upload';
  CMD_GET_SHARED_LINKS   = '/2/sharing/list_shared_links';
  CMD_REVOKE_SHARED_LINK = '/2/sharing/revoke_shared_link';
  CMD_CREATE_SHARED_LINK = '/2/sharing/create_shared_link_with_settings';

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

{$IF Defined(DEBUG) AND Defined(DUMP_RESULTS)}
procedure TDropboxBase.DumpResults(const Description: string; Response: TRESTResponse);
const
  DUMP_FILE_NAME = 'results.dump.txt';
  DELIMITER = '----------------------------------------';
var
  sw: TStreamWriter;
begin
  if TFile.Exists(DUMP_FILE_NAME) then
    sw := TFile.AppendText(DUMP_FILE_NAME)
  else
    sw := TFile.CreateText(DUMP_FILE_NAME);
  try
    sw.Write(DELIMITER); sw.WriteLine;
    sw.Write(Description); sw.WriteLine;
    sw.Write(DateTimeToStr(Now)); sw.WriteLine;
    sw.Write('Status Code: '); sw.Write(Response.StatusCode); sw.WriteLine;
    sw.Write('Status Text: '); sw.Write(Response.StatusText); sw.WriteLine;
    sw.Write('Content Type: '); sw.Write(Response.ContentType); sw.WriteLine;
    sw.Write('Content Length: '); sw.Write(Response.ContentLength); sw.WriteLine;
    sw.Write('Content Encoding: '); sw.Write(Response.ContentEncoding); sw.WriteLine;
    sw.WriteLine;
    if Response.ContentLength > 0 then
      if Response.ContentType.ToLower.Equals('application/json') then
        sw.Write(Response.JSONValue.Format)
      else
        sw.Write(Response.Content);
    sw.WriteLine;
  finally
    sw.Free;
  end;
end;
{$ENDIF}

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
{$IF Defined(DEBUG) AND Defined(DUMP_RESULTS)}
  DumpResults('CreateFolder', FResponse);
{$ENDIF}

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

function TDropboxBase.CreateSharedLink(DropboxPath: String): String;
begin
  Result := string.Empty;
  ClearRequest(DROPBOX_API_BASE_URL,
               TRESTRequestMethod.rmPOST,
               CMD_CREATE_SHARED_LINK);
  FRequest.Body.Add(Format('{"path": "%s"}', [DropboxPath]), TRESTContentType.ctAPPLICATION_JSON);
  FRequest.Execute;
{$IF Defined(DEBUG) AND Defined(DUMP_RESULTS)}
  DumpResults('CreateSharedLink', FResponse);
{$ENDIF}

  if FResponse.StatusCode = 200 then
    while FResponse.JSONReader.Read do
      if FResponse.JSONReader.TokenType = TJsonToken.PropertyName then
        if FResponse.JSONReader.Value.AsString.ToLower.Equals('url') then
        begin
        Result := FResponse.JSONReader.ReadAsString;
        Break;
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
{$IF Defined(DEBUG) AND Defined(DUMP_RESULTS)}
  DumpResults('Delete', FResponse);
{$ENDIF}

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
{$IF Defined(DEBUG) AND Defined(DUMP_RESULTS)}
  DumpResults('DownloadZip', FResponse);
{$ENDIF}

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
{$IF Defined(DEBUG) AND Defined(DUMP_RESULTS)}
  DumpResults('Download', FResponse);
{$ENDIF}

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
  ARecursive: Boolean; GetSharedLinks: Boolean): Boolean;
var
  sb: TStringBuilder;
  Prop: string;
  sl: TStringList;
  Itm: TDropboxItem;
  i: Integer;
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
{$IF Defined(DEBUG) AND Defined(DUMP_RESULTS)}
    DumpResults('ListFolder', FResponse);
{$ENDIF}

    if FResponse.StatusCode = 200 then
    begin
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

      if GetSharedLinks then
      begin
        sl := TStringList.Create;
        try
          if ListSharedLinks(sl) then
            for i := 0 to sl.Count - 1 do
            begin
              Itm := AList.GetByID(sl.KeyNames[i]);
              if Assigned(Itm) then
                Itm.SharedLink := sl.ValueFromIndex[i];
            end;
        finally
          sl.Free;
        end;
      end;

    end;
  finally
    sb.Free;
  end;
end;

function TDropboxBase.ListSharedLinks(SharedLinks: TStrings): Boolean;
var
  AID, AURL: string;
begin
  Result := False;
  ClearRequest(DROPBOX_API_BASE_URL,
               TRESTRequestMethod.rmPOST,
               CMD_GET_SHARED_LINKS);
  FRequest.Body.Add('{}', TRESTContentType.ctAPPLICATION_JSON);
  FRequest.Execute;
{$IF Defined(DEBUG) AND Defined(DUMP_RESULTS)}
  DumpResults('ListSharedLinks', FResponse);
{$ENDIF}

  if FResponse.StatusCode = 200 then
  begin
    AID := string.Empty;
    AURL := string.Empty;
    while FResponse.JSONReader.Read do
      if FResponse.JSONReader.TokenType = TJsonToken.PropertyName then
      begin
        if FResponse.JSONReader.Value.AsString.Equals('url') then
        begin
          AURL := FResponse.JSONReader.ReadAsString;
          if not string.IsNullOrEmpty(AID) then
          begin
            SharedLinks.AddPair(AID, AURL);
            AID := string.Empty;
            AURL := string.Empty;
          end;
        end else
        if FResponse.JSONReader.Value.AsString.Equals('id') then
        begin
          AID := FResponse.JSONReader.ReadAsString;
          if not string.IsNullOrEmpty(AURL) then
          begin
            SharedLinks.AddPair(AID, AURL);
            AID := string.Empty;
            AURL := string.Empty;
          end;
        end;
    end;
    Result := True; // delete me
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
{$IF Defined(DEBUG) AND Defined(DUMP_RESULTS)}
    DumpResults('Move', FResponse);
{$ENDIF}

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


function TDropboxBase.RevokeSharedLink(SharedLink: String): Boolean;
begin
  Result := False;
  if not string.IsNullOrEmpty(SharedLink) then
  begin
    ClearRequest(DROPBOX_API_BASE_URL,
                 TRESTRequestMethod.rmPOST,
                 CMD_REVOKE_SHARED_LINK);
    FRequest.Body.Add(Format('{"url": "%s"}', [SharedLink]), TRESTContentType.ctAPPLICATION_JSON);
    FRequest.Execute;
{$IF Defined(DEBUG) AND Defined(DUMP_RESULTS)}
    DumpResults('RevokeSharedLink', FResponse);
{$ENDIF}

    Result := (FResponse.StatusCode = 200);
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
{$IF Defined(DEBUG) AND Defined(DUMP_RESULTS)}
    DumpResults('Upload', FResponse);
{$ENDIF}
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


unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Rac.Dropbox.VCL, Rac.Dropbox.Types, Vcl.StdCtrls,
  Vcl.OleCtrls, SHDocVw, Vcl.ExtCtrls, System.IOUtils, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, REST.Authenticator.OAuth,
  Vcl.ComCtrls, System.ImageList, Vcl.ImgList, Vcl.Buttons, System.UITypes,
  System.Actions, Vcl.ActnList, ShellApi;

const
  APP_KEY = 'CHANGE_ME';

type
  TfrmMain = class(TForm)
    pnlLeft: TPanel;
    wbWww: TWebBrowser;
    btnAuthorize: TButton;
    pnlMain: TPanel;
    restClient: TRESTClient;
    restRequest: TRESTRequest;
    restResponse: TRESTResponse;
    restOauth2: TOAuth2Authenticator;
    btnCreateFolder: TButton;
    lvList: TListView;
    ilList: TImageList;
    pnlRight: TPanel;
    btnUp: TSpeedButton;
    btnRefresh: TSpeedButton;
    btnDelete: TButton;
    btnDownload: TButton;
    btnUpload: TButton;
    btnRename: TButton;
    dlgOpenUpload: TOpenDialog;
    alMain: TActionList;
    actGotoUp: TAction;
    actRefreshFolder: TAction;
    actAuthorize: TAction;
    actCreateFolder: TAction;
    actDelete: TAction;
    actDownload: TAction;
    actUpload: TAction;
    actRename: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvListDblClick(Sender: TObject);
    procedure actGotoUpExecute(Sender: TObject);
    procedure actRefreshFolderExecute(Sender: TObject);
    procedure actAuthorizeExecute(Sender: TObject);
    procedure actCreateFolderExecute(Sender: TObject);
    procedure actUploadExecute(Sender: TObject);
    procedure actRenameExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDownloadExecute(Sender: TObject);
  private
    { Private declarations }
    FCurrentPath: string;
    procedure _OnAuthorize(Sender: TObject; const IsAuthorized: Boolean);
    procedure SetButtons(Isauthorized: Boolean);
    function  ReadToken: string;
    procedure SaveToken(AToken: string);
    function  GetDataDir: string;
    procedure GotoPath(APath: string);
    function PreparePath(APath: string): string;
    procedure ListFolder(APath: string);
    function GetTempFile(AFileExt: string): string;
    procedure OpenLocalFile(AFilePath: string);
  public
    { Public declarations }
    Dropbox: TDropbox;
    procedure WaitCursorOn;
    procedure WaitCursorOff;
  end;

var
  frmMain: TfrmMain;

implementation

const
  DATA_DIR = 'Raccoon';
  TOKEN_FILE_NAME = 'token.txt';
  ICON_FOLDER = 0;
  ICON_FILE = 1;

{$R *.dfm}

procedure TfrmMain.actAuthorizeExecute(Sender: TObject);
begin
  wbWww.Visible := True;
  pnlMain.Visible := False;

  Dropbox.Authorize(wbWww);
end;

procedure TfrmMain.actCreateFolderExecute(Sender: TObject);
var
  Value: string;
begin
  if InputQuery('Create new folder', 'New folder name...', Value) then
  begin
    WaitCursorOn;
    Dropbox.CreateFolder(Value);
    WaitCursorOff;
    ListFolder(FCurrentPath);
  end;
end;

procedure TfrmMain.actDeleteExecute(Sender: TObject);
var
  AName, AType: string;
begin
  if not Assigned(lvList.Selected) then
  begin
    MessageDlg('Select item to delete.', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
  end else
  begin
    AName := lvList.Selected.Caption;
    if lvList.Selected.ImageIndex = ICON_FOLDER then
      AType := 'folder'
    else
      AType := 'file';
    if MessageDlg(Format('Are you sure, you want delete "%s" %s?', [AName, AType]), TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      WaitCursorOn;
      Dropbox.Delete(PreparePath(FCurrentPath + AName));
      WaitCursorOff;
      ListFolder(FCurrentPath);
    end;
  end;
end;

procedure TfrmMain.actDownloadExecute(Sender: TObject);
var
  AName: string;
  bIsFolder: Boolean;
  ANewPath: string;
begin
  if not Assigned(lvList.Selected) then
  begin
    MessageDlg('Select item to download.', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
  end else
  begin
    AName     := lvList.Selected.Caption;
    bIsFolder := (lvList.Selected.ImageIndex = ICON_FOLDER);
    WaitCursorOn;
    if bIsFolder then
    begin
      ANewPath  := GetTempFile('.zip');
      Dropbox.DownloadZip(PreparePath(FCurrentPath + AName), ANewPath);
      if MessageDlg(Format('Folder "%s" saved as'#13#10'%s'#13#10#13#10'Do you want open this file?', [AName, ANewPath]), TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
        OpenLocalFile(ANewPath);
    end else
    begin
      ANewPath  := GetTempFile(TPath.GetExtension(AName));
      Dropbox.Download(PreparePath(FCurrentPath + AName), ANewPath);
      if MessageDlg(Format('File "%s" saved as'#13#10'%s'#13#10#13#10'Do you want open this file?', [AName, ANewPath]), TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
        OpenLocalFile(ANewPath);
    end;
    WaitCursorOff;
  end;
end;

procedure TfrmMain.actGotoUpExecute(Sender: TObject);
begin
  GotoPath('..');
end;

procedure TfrmMain.actRefreshFolderExecute(Sender: TObject);
begin
  ListFolder(FCurrentPath);
end;

procedure TfrmMain.actRenameExecute(Sender: TObject);
var
  AName, ANewName, AType: string;
begin
  if not Assigned(lvList.Selected) then
  begin
    MessageDlg('Select item to change name.', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
  end else
  begin
    AName := lvList.Selected.Caption;
    ANewName := AName;
    if lvList.Selected.ImageIndex = ICON_FOLDER then
      AType := 'folder'
    else
      AType := 'file';
    if InputQuery(Format('Change "%s" %s name...', [AName, AType]), 'New name...', ANewName) then
      if not AName.Equals(ANewName) then
      begin
        WaitCursorOn;
        Dropbox.Move(PreparePath(FCurrentPath + AName), PreparePath(FCurrentPath + ANewName));
        WaitCursorOff;
        ListFolder(FCurrentPath);
      end else
        MessageDlg(Format('Change %s name from "%s" to "%s" doesn''t make sense.', [AType, AName, ANewName]), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmMain.actUploadExecute(Sender: TObject);
begin
  if dlgOpenUpload.Execute(self.Handle) then
  begin
    WaitCursorOn;
    Dropbox.Upload(dlgOpenUpload.FileName, FCurrentPath + TPath.GetFileName(dlgOpenUpload.FileName));
    WaitCursorOff;
    ListFolder(FCurrentPath);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FCurrentPath := '/';
  Dropbox := TDropbox.Create(APP_KEY, ReadToken);
  Dropbox.OnAuthorize := _OnAuthorize;

  SetButtons(not string.IsNullOrEmpty(Dropbox.Token));
  if not string.IsNullOrEmpty(Dropbox.Token) then
    actRefreshFolder.Execute;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Dropbox.Free;
end;

function TfrmMain.GetDataDir: string;
begin
  Result := TPath.Combine(TPath.GetHomePath, DATA_DIR);
  if not TDirectory.Exists(Result) then
    TDirectory.CreateDirectory(Result);
end;

function TfrmMain.GetTempFile(AFileExt: string): string;
begin
  repeat
    Result := TPath.Combine(TPath.GetTempPath, TPath.GetTempFileName + AFileExt);
  until not TFile.Exists(Result);
end;

procedure TfrmMain.GotoPath(APath: string);
var
  p: Integer;
begin
  if APath.Equals('..') then
  begin
    if FCurrentPath.Length > 1 then
    begin
      FCurrentPath := FCurrentPath.Substring(0, FCurrentPath.Length - 1);
      p := FCurrentPath.LastIndexOf('/');
      if p >= 0 then
        FCurrentPath := FCurrentPath.Remove(p + 1);
    end;
  end else
  if APath.Length > 0 then
  begin
    if APath = '/' then
      FCurrentPath := '/'
    else
      FCurrentPath := FCurrentPath + APath + '/';
  end;
  ListFolder(FCurrentPath);
end;

procedure TfrmMain.ListFolder(APath: string);
var
  List: TDropboxItems;
  i: Integer;
  Item: TListItem;
begin
  List := TDropboxItems.Create;
  lvList.Items.BeginUpdate;
  try
    lvList.Clear;
    WaitCursorOn;
    if Dropbox.ListFolder(PreparePath(FCurrentPath), List, False) then
    begin
      for i := 0 to List.Count - 1 do
      begin
        Item := lvList.Items.Add;
        Item.Caption := List[i].Name;
        if List[i].IsFolder then
          Item.ImageIndex := ICON_FOLDER
        else
          Item.ImageIndex := ICON_FILE;
      end;
    end;
  finally
    lvList.Items.EndUpdate;
    List.Free;
    WaitCursorOff;
  end;
end;

procedure TfrmMain.lvListDblClick(Sender: TObject);
begin
  if Assigned(lvList.Selected) then
    if lvList.Selected.ImageIndex = ICON_FOLDER then
      GotoPath(lvList.Selected.Caption)
    else
      actDownload.Execute;
end;

procedure TfrmMain.OpenLocalFile(AFilePath: string);
begin
  ShellExecute(Handle, 'open', PChar(AFilePath), nil, nil, SW_SHOWNORMAL) ;
end;

function TfrmMain.PreparePath(APath: string): string;
begin
  if APath.EndsWith('/') then
    Result := APath.Substring(0, APath.Length - 1)
  else
    Result := APath;
end;

function TfrmMain.ReadToken: string;
var
  sl: TStringList;
  FilePath: string;
begin
  FilePath := TPath.Combine(GetDataDir, TOKEN_FILE_NAME);
  if TFile.Exists(FilePath) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(FilePath);
      Result := sl.Text.Trim;
    finally
      sl.Free;
    end;
  end else
    Result := string.Empty;
end;

procedure TfrmMain.SaveToken(AToken: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := AToken;
    sl.SaveToFile(TPath.Combine(GetDataDir, TOKEN_FILE_NAME));
  finally
    sl.Free;
  end;
end;

procedure TfrmMain.SetButtons(Isauthorized: Boolean);
begin
  // Right panel
  actGotoUp.Enabled        := Isauthorized;
  actRefreshFolder.Enabled := Isauthorized;
  // Left panel
  actAuthorize.Enabled    := not IsAuthorized;
  actCreateFolder.Enabled :=     Isauthorized;
  actDelete.Enabled       :=     Isauthorized;
  actDownload.Enabled     :=     Isauthorized;
  actUpload.Enabled       :=     Isauthorized;
  actRename.Enabled       :=     Isauthorized;
end;

procedure TfrmMain.WaitCursorOff;
begin
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.WaitCursorOn;
begin
  Screen.Cursor := crHourGlass;
end;

procedure TfrmMain._OnAuthorize(Sender: TObject; const IsAuthorized: Boolean);
begin
  wbWww.Visible := False;
  pnlMain.Visible := True;
  SetButtons(IsAuthorized);
  if IsAuthorized then
  begin
    SaveToken(Dropbox.Token);
    btnRefresh.Click;
  end;
end;

end.

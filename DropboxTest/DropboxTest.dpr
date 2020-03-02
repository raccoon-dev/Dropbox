program DropboxTest;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {frmMain},
  Rac.Dropbox.VCL in '..\Rac.Dropbox.VCL.pas',
  Rac.Dropbox.Types in '..\Rac.Dropbox.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

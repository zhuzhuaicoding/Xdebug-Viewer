program xdebugviewer;

uses
  Forms,
  Main in 'application\Main.pas' {Form1},
  XDebugFile in 'library\XDebugFile.pas',
  Stream in 'library\Stream.pas',
  XDebugItem in 'library\XDebugItem.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;

  Application.Initialize;
  Application.Title := 'Xdebug Viewer';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

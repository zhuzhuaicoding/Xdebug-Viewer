unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, XDebugFile, XDebugItem, ActnList, ComCtrls, VirtualTrees, Menus;

type
  TNodeData = record
    Data: PXItem;
  end;
  PNodeData = ^TNodeData;

  TForm1 = class(TForm)
    OpenDialog: TOpenDialog;
    ActionList1: TActionList;
    OpenFileAction: TAction;
    StatusBar: TStatusBar;
    ProgressBar: TProgressBar;
    TreeView: TVirtualStringTree;
    PopupMenu1: TPopupMenu;
    Openfile1: TMenuItem;
    Closefile1: TMenuItem;
    CloseFileAction: TAction;
    procedure OpenFileActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure TreeViewPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure TreeViewExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeViewGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure CloseFileActionExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    XDebugFile: XFile;

    procedure UpdateProgress(Sender: TObject; const Position: Cardinal; const Total: Cardinal);
    procedure UpdateStatus(Status: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Math;

{$R *.dfm}

function FormatFloat(const X: Extended; const D: Byte = 2): String;
begin
  Result := Format('%.*f', [D, X]);
end;

function FormatDecimal(D: Cardinal): String;
begin
  Result := Format('%.0n', [D * 1.0]);
end;

function FormatSign(X: Integer): Char;
begin
  case Sign(X) of
    1: Result := '+';
    -1: Result := '-';
  else
    Result := ' ';
  end;
end;

procedure TForm1.CloseFileActionExecute(Sender: TObject);
begin
  if Assigned(XDebugFile) then begin
    TreeView.Clear;
    XDebugFile.Free;
    XDebugFile := nil;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(XDebugFile) then
    XDebugFile.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
  var ProgressBarStyle: integer;
begin
  ProgressBar.Parent := StatusBar;
  ProgressBarStyle := GetWindowLong(ProgressBar.Handle, GWL_EXSTYLE);
  ProgressBarStyle := ProgressBarStyle - WS_EX_STATICEDGE;
  SetWindowLong(ProgressBar.Handle, GWL_EXSTYLE, ProgressBarStyle);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    if Assigned(XDebugFile) then begin
      XDebugFile.Terminate;
    end;
  end;
end;

procedure TForm1.OpenFileActionExecute(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    if Assigned(XDebugFile) then begin
      TreeView.Clear;
      XDebugFile.Free;
    end;

    UpdateStatus('Opening file');

    XDebugFile := Xfile.Create(OpenDialog.FileName);
    XDebugFile.OnProgress := self.UpdateProgress;

    UpdateStatus('Processing file');

    XDebugFile.Parse;

    if XDebugFile.Terminated then begin
      CloseFileAction.Execute;
      UpdateStatus('Processing terminated');
    end else begin
      UpdateStatus('Drawing tree');
      TreeView.RootNodeCount := XDebugFile.Root^.ChildCount;
      UpdateStatus('Ready');
    end;
    ProgressBar.Position := 0;
  end;
end;

procedure TForm1.StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
begin
  if Panel = StatusBar.Panels[1] then
    with ProgressBar do begin
      Top := Rect.Top;
      Left := Rect.Left;
      Width := Rect.Right - Rect.Left;
      Height := Rect.Bottom - Rect.Top;
    end;
end;

procedure TForm1.TreeViewExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
with (Sender as TVirtualStringTree).Header do begin
  AutoFitColumns(true, smaUseColumnOption, MainColumn, MainColumn);
end;
end;

procedure TForm1.TreeViewGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure TForm1.TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var NodeData: PNodeData;
    Delta: cardinal;
begin
  NodeData := Sender.GetNodeData(Node);
  with NodeData.Data^ do
    case Column of
      -1, 6:
        CellText := FunctionName;
      0:
        CellText := FormatFloat(TimeStart, 6);
      1:
        CellText := FormatFloat(TimeEnd, 6);
      2:
        CellText := FormatFloat(TimeEnd - TimeStart, 6);
      3:
        CellText := FormatDecimal(MemoryStart);
      4:
        CellText := FormatDecimal(MemoryEnd);
      5: begin
        if MemoryEnd = 0 then
          CellText := 'FINISH'
        else begin
          Delta := MemoryEnd - MemoryStart;
          CellText := Format('%s%.0n', [FormatSign(Delta), Abs(Delta) * 1.0]);
        end;
      end;
      7:
        CellText := Format('%s (%d)', [FileName, FileLine]);
    end;
end;

procedure TForm1.TreeViewInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var PItem: PXItem;
    NodeData, ParentNodeData: PNodeData;
    I: integer;
begin
  with Sender do begin
    NodeData := GetNodeData(Node);
    ParentNodeData := GetNodeData(ParentNode);
  end;

  if Assigned(ParentNodeData) then begin
    if ParentNodeData.Data^.ChildCount > Node.Index then
      PItem := ParentNodeData.Data^.Children[Node.Index];
  end else begin
    if XDebugFile.Root^.ChildCount > Node.Index then
      PItem := XDebugFile.Root^.Children[Node.Index];
  end;

  if not Assigned(PItem) then
    raise Exception.Create('Could not locate node');

  NodeData.Data := PItem;

  for I := 1 to NodeData.Data^.ChildCount do
    Sender.AddChild(Node);

  if not Assigned(ParentNodeData) then
    Sender.Expanded[Node] := true;
end;

procedure TForm1.TreeViewPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var NodeData: PNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if vsSelected in Node.States then
    TargetCanvas.Font.Color := clHighLightText
  else if NodeData.Data.FunctionType = XFT_INTERNAL then
    TargetCanvas.Font.Color := clHighLight
  else
    TargetCanvas.Font.Color := clWindowText;
end;

procedure TForm1.UpdateProgress(Sender: TObject; const Position: Cardinal; const Total: Cardinal);
begin
  ProgressBar.Max := Total;
  ProgressBar.Position := Position;
  application.ProcessMessages;
end;

procedure TForm1.UpdateStatus(Status: string);
begin
  StatusBar.Panels[0].Text := Status;
end;

end.

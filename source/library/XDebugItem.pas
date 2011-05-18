unit XDebugItem;

interface

uses Classes;

type
  TXFunctionType = (XFT_INTERNAL, XFT_USER_DEFINED);
  TStringArray = array of string;

  PXItem = ^TXItem;
  TXItemArray = array of PXItem;
  TXItem = record
    private
      FLevel: word;
      FTimeStart: single;
      FTimeEnd: single;
      FMemoryStart: cardinal;
      FMemoryEnd: cardinal;
      FFunctionName: string;
      FFunctionType: TXFunctionType;
      FFileName: string;
      FFileLine: cardinal;
      FParamCount: word;
      FParametersStreamOffset: cardinal;
      FParametersStreamLength: cardinal;
      FParent: PXItem;
      FChildCount: cardinal;
      FChildren: TXItemArray;
      ChildrenCapacity: cardinal;
    public
      constructor Create(InitLevel: Cardinal); overload;
      constructor Create(LineData: TStringArray; Parent: PXItem; Stream: TFileStream); overload;
      procedure Finish(LineData: TStringArray);
      function GetChildren(): TXItemArray;
      function GetChild(Index: cardinal): PXItem;
      procedure AddChild(Child: PXItem);
      procedure Freeze;
      procedure Free;

      property Level: word read FLevel;
      property TimeStart: single read FTimeStart;
      property TimeEnd: single read FTimeEnd;
      property MemoryStart: cardinal read FMemoryStart;
      property MemoryEnd: cardinal read FMemoryEnd;
      property FunctionName: string read FFunctionName;
      property FunctionType: TXFunctionType read FFunctionType;
      property FileName: string read FFileName;
      property FileLine: cardinal read FFileLine;
      property ParamCount: word read FParamCount;
      property ParamStreamOffset: cardinal read FParametersStreamOffset;
      property ParamStreamLength: cardinal read FParametersStreamLength;
      property Parent: PXItem read FParent;
      property ChildCount: cardinal read FChildCount;
      property Children[Index: cardinal]: PXItem read GetChild;
  end;

implementation

uses SysUtils;

constructor TXItem.Create(initLevel: Cardinal);
begin
  FLevel := initLevel;
  FTimeStart := 0;
  FTimeEnd := 0;
  FMemoryStart := 0;
  FMemoryEnd := 0;
  FFunctionName := '';
  FFunctionType := XFT_INTERNAL;
  FFileName := '';
  FFileLine := 0;
  FParamCount := 0;
  FParametersStreamOffset := 0;
  FParametersStreamLength := 0;
  FChildCount := 0;
  ChildrenCapacity := 10;
  SetLength(FChildren, ChildrenCapacity);
end;

constructor TXItem.Create(LineData: TStringArray; Parent: PXItem; Stream: TFileStream);
begin
  if Length(LineData) <> 12 then
    raise Exception.Create('Invalid file line provided');

  FLevel := StrToInt(LineData[0]);
  FTimeStart := StrToFloat(LineData[3]);
  FMemoryStart := StrToInt(LineData[4]);
  FFunctionName := LineData[5];
  if LineData[6] = '1' then
    FFunctionType := XFT_USER_DEFINED
  else
    FFunctionType := XFT_INTERNAL;
  FFileName := LineData[8];
  FFileLine := StrToInt(LineData[9]);
  FParamCount := StrToInt(Linedata[10]);
  FParametersStreamOffset := Stream.Position;
  FParametersStreamLength := Length(LineData[11]);
  FParent := Parent;

  FChildCount := 0;
  ChildrenCapacity := 10;
  SetLength(FChildren, ChildrenCapacity);
end;

procedure TXItem.Finish(LineData: TStringArray);
begin
  if Length(LineData) <> 5 then
    raise Exception.Create('Invalid file line provided');

  FMemoryEnd := StrToInt(LineData[4]);
  FTimeEnd := StrToFloat(LineData[3]);
end;

function TXItem.GetChildren(): TXItemArray;
begin
  Result := FChildren;
end;

function TXItem.GetChild(Index: cardinal): PXItem;
begin
  Result := FChildren[Index];
end;

procedure TXItem.AddChild(Child: PXItem);
begin
  if FChildCount = ChildrenCapacity - 1 then begin
    Inc(ChildrenCapacity, 10);
    SetLength(FChildren, ChildrenCapacity);
  end;

  FChildren[FChildCount] := Child;
  Inc(FChildCount);
end;

procedure TXItem.Freeze;
  var Child: PXItem;
begin
  SetLength(FChildren, FChildCount);
  for Child in FChildren do
    Child^.Freeze;
end;

procedure TXItem.Free;
  var Child: PXItem;
begin
  for Child in FChildren do
    if Assigned(Child) then begin
      Child^.Free;
      Dispose(Child);
    end;
end;

end.

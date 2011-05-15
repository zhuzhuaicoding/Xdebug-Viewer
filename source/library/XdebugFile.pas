unit XDebugFile;

interface

uses Classes, XDebugItem;

type
  TXFileProgressEvent = procedure(Sender: TObject; const Position: Cardinal; const Total: Cardinal) of object;

  XFile = class
    private
      FProgressEvent: TXFileProgressEvent;
      FRoot: PXItem;
      Stream: TFileStream;
      function ParseLine(const Line: string; ItemParent: PXItem): PXItem;
    public
      constructor Create(Filename: string);
      destructor Destroy(); override;

      procedure parse();

      property OnProgress: TXFileProgressEvent read FProgressEvent write FProgressEvent;
      property Root: PXItem read FRoot;
  end;

implementation

uses SysUtils, Math,Stream;

function Split(const Subject: String; const Delimiter: Char; const MaxCount: Integer = 0): TStringArray;
  var Pos, Ex, L, R, C: integer;
      P: PChar;
begin
  R := Max(MaxCount, 10);
  SetLength(Result, R);
  C := 0;

  L := Length(Subject);
  P := Pointer(Subject);
  Pos := 1;
  Ex := Pos;
  while Pos <= L do begin
    if C = MaxCount - 1 then
      Break;

    if P^ = Delimiter then begin
      if C = R - 1 then begin
        Inc(R, 10);
        SetLength(Result, R);
      end;

      Result[C] := Copy(Subject, Ex, Pos - Ex);
      Inc(C);
      Ex := Pos + 1;
    end;
    Inc(P);
    Inc(Pos);
  end;

  if Pos <= L + 1 then begin
    Result[C] := Copy(Subject, Ex, L);
    Inc(C);
  end;

  if R < MaxCount then begin
    SetLength(Result, MaxCount);
    for L := C to MaxCount - 1 do
      Result[L] := '';
  end else if C < R - 1 then
    SetLength(Result, C);
end;

procedure Replace(const Subject: String; const Search: Char; const Replace: Char);
var Pos, L: integer;
    P: PChar;
begin
L := Length(Subject);
P := Pointer(Subject);

Pos := 1;
while Pos <= L do begin
  if P^ = Search then
    P^ := Replace;

  Inc(Pos);
  Inc(P);
end;
end;





constructor XFile.Create(Filename: string);
begin
  inherited Create;

  if not FileExists(Filename) then
    raise Exception.Create(Format('File %s does not exist.', [filename]));
  try
    Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  except on E: Exception do
    raise Exception.Create(Format('File %s could not be opened: %s.', [Filename, E.Message]));
  end;

  New(FRoot);
  FRoot^ := TXItem.Create(0);
end;

destructor XFile.Destroy;
begin
  if Assigned(Stream) then
    Stream.Free;

  if Assigned(FRoot) then begin
    FRoot.Free;
    Dispose(FRoot);
  end;
end;

procedure XFile.Parse;
  var Parent: PXItem;
      Line: String;
      TextStream: TTextStream;
begin
  Parent := Root;

  Stream.Seek(0, soFromBeginning);
  TextStream := TTextStream.Create(Stream);
  try
    while TextStream.ReadLn(Line) do begin
      if (Length(Line) > 0) and (Ord(Line[1]) in [48..57]) then
        Parent := parseLine(Line, Parent);

      if Assigned(FProgressEvent) then
        FProgressEvent(self, Stream.Position, Stream.Size);
    end;
  finally
    TextStream.Free;
  end;

  FRoot.Freeze;
end;

function XFile.ParseLine(const Line: string; ItemParent: PXItem): PXItem;
  var Items: TStringArray;
      L, ItemLevel: Integer;
begin
  Items := Split(Line, #9, 12);
  Replace(Items[3], '.', ',');

  L := Length(Items);
  if not L in [5, 12] then
    raise Exception.Create(Format('Invalid items count, 5 or 12 expected, %d given', [L]));

  ItemLevel := StrToInt(Items[0]);
  if ItemLevel < 1 then
    raise Exception.Create(Format('Invalid call level definition: %d', [ItemLevel]));

  if L = 5 then begin
    // Call end
    while ItemParent^.Level > ItemLevel do begin
      ItemParent := ItemParent^.Parent;

      if not Assigned(ItemParent) then
        raise Exception.Create('Could not find element parent');
    end;

    ItemParent^.Finish(Items);
    Result := ItemParent;
  end else if L = 12 then begin
    // Call start
    while ItemParent^.Level >= ItemLevel do begin
      ItemParent := ItemParent^.Parent;

      if not Assigned(ItemParent) then
        raise Exception.Create('Could not find element parent');
    end;

    New(Result);
    Result^ := TXItem.Create(Items, ItemParent, Stream);
    ItemParent.AddChild(Result);
  end;
end;

end.

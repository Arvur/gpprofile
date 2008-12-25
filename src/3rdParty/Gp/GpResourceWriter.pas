unit GpResourceWriter;

interface

uses
  Classes;

type
  TGpResourceWriter = class
  private
    resFile: TFileStream;
  public
    constructor Create(resourceFileName: string);
    destructor  Destroy; override;
    procedure WriteRCDataFromFile(const resourceName, fileName: string);
    procedure WriteRCDataFromStream(const resourceName: string; strResourceData: TStream);
    procedure WriteRCDataFromString(const resourceName, resourceData: string);
  end; { TGpResourceWriter }

implementation

uses
  SysUtils;

{ TGpResourceWriter }

constructor TGpResourceWriter.Create(resourceFileName: string);
const
  resHeader: array [1..32] of byte = (
    $00, $00, $00, $00, $20, $00, $00, $00, $FF, $FF, $00, $00,
    $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00
  );
begin
  resFile := TFileStream.Create(resourceFileName,fmCreate);
  if not assigned(resFile) then Fail
  else begin
    resFile.Write(resHeader,SizeOf(resHeader));
  end;
end; { TGpResourceWriter.Create }

destructor TGpResourceWriter.Destroy;
begin
  resFile.Free;
  resFile := nil;
end; { TGpResourceWriter.Destroy }

procedure TGpResourceWriter.WriteRCDataFromFile(const resourceName, fileName: string);
var
  strResourceFile: TFileStream;
begin
  strResourceFile := TFileStream.Create(fileName, fmOpenRead);
  try
    WriteRCDataFromStream(resourceName, strResourceFile);
  finally FreeAndNil(strResourceFile); end;
end; { TGpResourceWriter.WriteRCDataFromFile }

procedure TGpResourceWriter.WriteRCDataFromStream(const resourceName: string;
  strResourceData: TStream);
var
  hdrLen : integer;
  i      : integer;
  name   : WideString;
  padding: integer;
  resPos : integer;
begin
  resPos := resFile.Position;
  i := strResourceData.Size;
  resFile.Write(i, 4);                                            // data size
  i := 0;
  resFile.Write(i, 4);                          // placeholder for header size
  i := $000AFFFF;
  resFile.Write(i, 4);                               // resource type - RCDATA
  name := UpperCase(resourceName);
  resFile.Write(name[1], Length(name)*SizeOf(WideChar));      // resource name
  i := 0;
  resFile.Write(i, 2);                                      // zero terminated
  padding := 4-((Length(name)*SizeOf(WideChar)+2) mod 4);
  if padding < 4 then
    resFile.Write(i, padding);                                      // padding
  resFile.Write(i, 4);                                              // version
  i := $0030; // MOVEABLE, PURE
  resFile.Write(i, 2);                                                // flags
  i := $0904; // English (US)
  resFile.Write(i, 2);                                             // language
  i := 0;
  resFile.Write(i, 4);                                              // version
  resFile.Write(i, 4);                                      // characteristics
  hdrLen := resFile.Position-resPos;
  resFile.Position := resPos+4;
  resFile.Write(hdrLen, 4);                                     // header size
  resFile.Position := resFile.Size;
  resFile.CopyFrom(strResourceData, 0);                                // data
  padding := 4-(strResourceData.Size mod 4);
  if padding < 4 then
    resFile.Write(i,padding);                                       // padding
end; { TGpResourceWriter.WriteRCDataFromStream }

procedure TGpResourceWriter.WriteRCDataFromString(const resourceName, resourceData:
  string);
var
  strResourceData: TStringStream;
begin
  strResourceData := TStringStream.Create(resourceData);
  try
    WriteRCDataFromStream(resourceName, strResourceData);
  finally FreeAndNil(strResourceData); end;
end; { TGpResourceWriter.WriteRCDataFromString }

end.

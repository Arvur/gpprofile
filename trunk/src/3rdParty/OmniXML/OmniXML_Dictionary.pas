{ $OmniXML: OmniXML/OmniXML_Dictionary.pas,v 1.2 2008/01/20 21:18:47 mremec Exp $ }

(*******************************************************************************
* The contents of this file are subject to the Mozilla Public License Version
* 1.1 (the "License"); you may not use this file except in compliance with the
* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
* the specific language governing rights and limitations under the License.
*                                                                              
* The Original Code is OmniXML_Dictionary.pas
*
* The Initial Developer of the Original Code is Miha Remec
*   http://omnixml.com/
*******************************************************************************)
unit OmniXML_Dictionary;

interface

{$I OmniXML.inc}

uses
  Classes, SysUtils, IniFiles;

const
  CInvalidDicId = High(LongWord);

type
  TDicId = LongWord;
  TDictionary = class
  private
    FHashTable: TStringHash;
    FTextList: TStringList;
    FMaxItemsBeforeResize: Integer;
    procedure Resize;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function Add(const Text: WideString): TDicId;
    function Get(const Id: TDicId): WideString;
    procedure Save(const FileName: string);
  end;

implementation

const
  // customized version from http://planetmath.org/encyclopedia/GoodHashTablePrimes.html
  CGoodHashTablePrimes: array [6..30] of Cardinal = (97, 193, 389, 769, 1543, 3079, 6151,
    12289, 24593, 49157, 98317, 196613, 393241, 786433, 1572869, 3145739, 6291469,
    12582917, 25165843, 50331653, 100663319, 201326611, 402653189, 805306457, 1610612741);

function GetGoodHashSize(const Size: Cardinal): Cardinal;
var
  UpToSize: Cardinal;
  TableIndex: Integer;
begin
  TableIndex := Low(CGoodHashTablePrimes);
  UpToSize := 1 shl TableIndex;
  while Size > UpToSize do
  begin
    Inc(TableIndex);
    UpToSize := UpToSize shl 1;
  end;
  Result := CGoodHashTablePrimes[TableIndex];
end;

function ToUTF8(const Unicode: WideString): string; {$IFDEF DELPHI2005_UP}inline;{$ENDIF}
begin
  Result := UTF8Encode(Unicode);
end;

function ToUnicode(const UTF8: string): WideString; {$IFDEF DELPHI2005_UP}inline;{$ENDIF}
begin
  Result := UTF8Decode(UTF8);
end;

{ TDictionary }

constructor TDictionary.Create;
begin
  inherited;
  
  FTextList := TStringList.Create;
  Resize;
end;

destructor TDictionary.Destroy;
begin
  FreeAndNil(FHashTable);
  FreeAndNil(FTextList);

  inherited;
end;

function TDictionary.Add(const Text: WideString): TDicId;
var
  Value: Integer;
  UTF8Text: string;
begin
  UTF8Text := ToUTF8(Text);
  Value := FHashTable.ValueOf(UTF8Text);
  if Value >= 0 then
    Result := Value
  else
  begin
    Value := FTextList.Add(UTF8Text);

    if FTextList.Count < FMaxItemsBeforeResize then
      FHashTable.Add(UTF8Text, Value)
    else
      Resize;

    Result := Value;
  end;
end;

function TDictionary.Get(const Id: TDicId): WideString;
begin
  Result := ToUnicode(FTextList[Id]);
end;

procedure TDictionary.Resize;
var
  ItemIndex: Integer;
  HashSize: Cardinal;
begin
  FHashTable.Free;

  HashSize := GetGoodHashSize(FTextList.Count);

  FHashTable := TStringHash.Create(HashSize);
  FMaxItemsBeforeResize := Trunc((HashSize / 3) * 2);

  // re-add items to hash
  for ItemIndex := 0 to FTextList.Count - 1 do
    FHashTable.Add(FTextList[ItemIndex], ItemIndex);
end;

procedure TDictionary.Save(const FileName: string);
begin
  FTextList.SaveToFile(FileName);
end;

end.


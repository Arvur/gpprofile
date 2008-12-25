(*:GpProfile 'excluded units' preferences frame.
   @author Primoz Gabrijelcic
   @desc <pre>

This software is distributed under the BSD license.

Copyright (c) 2006, Primoz Gabrijelcic
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
- Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.
- Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
- The name of the Primoz Gabrijelcic may not be used to endorse or promote
  products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   Author            : Primoz Gabrijelcic
   Creation date     : 2006-06-17
   Last modification : 2006-06-17
   Version           : 1.0
</pre>*)(*
   History:
     1.0: 2006-06-17
       - Imported into CVS.
*)

unit gppFramePreferencesExcludedUnits;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Buttons, ExtCtrls, Contnrs,
  gppPreferencesModel, ActnList;

type
  TfrmPreferencesExcludedUnits = class(TFrame)
    actClear            : TAction;
    ActionList          : TActionList;
    btnAddFromFolder    : TSpeedButton;
    btnClear            : TSpeedButton;
    bvlUnits            : TBevel;
    lblExcludedUnitsHelp: TLabel;
    lblUnits            : TLabel;
    memoExclUnits       : TMemo;
    pnlExcludedUnits    : TPanel;
    procedure actClearExecute(Sender: TObject);
    procedure actClearUpdate(Sender: TObject);
    procedure btnAddFromFolderClick(Sender: TObject);
    procedure memoExclUnitsExit(Sender: TObject);
  private
    FModel: TGppPrefExcludedUnits;
  protected
    procedure HandleModelModified(Sender: TObject; modifiedComponents: TObjectList);
    procedure SetModel(value: TGppPrefExcludedUnits);
    procedure SubscribeToModel;
    procedure UnsubscribeFromModel; 
  public
    procedure Reload;
    property Model: TGppPrefExcludedUnits read FModel write SetModel;
  end; { TfrmPreferencesExcludedUnits }

implementation

uses
  FileCtrl,
  GpString;

{$R *.dfm}

procedure TfrmPreferencesExcludedUnits.actClearExecute(Sender: TObject);
begin
  Model.Units.Clear;
end; { TfrmPreferencesExcludedUnits.actClearExecute }

procedure TfrmPreferencesExcludedUnits.actClearUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (Model.Units.Count > 0);
end; { TfrmPreferencesExcludedUnits.actClearUpdate }

procedure TfrmPreferencesExcludedUnits.btnAddFromFolderClick(Sender: TObject);
var
  sl: TStringList;
  directory: string;

  procedure Iterate(mask: string);
  var
    S  : TSearchRec;
    res: integer;

    procedure AddUnit(unitName: string);
    begin
      if sl.IndexOf(unitName) < 0 then
        sl.Add(unitName);
    end; { AddUnit }

  begin { Iterate }
    res := FindFirst(IncludeTrailingPathDelimiter(directory)+mask,0,S);
    if res = 0 then begin
      repeat
        AddUnit(UpperCase(FirstEl(S.Name,'.',-1)));
        res := FindNext(S);
      until res <> 0;
      FindClose(S);
    end;
  end; { Iterate }

begin { TfrmPreferences.btnAddFromFolderClick }
  sl := TStringList.Create;
  try
    sl.Sorted := true;
    sl.Assign(Model.Units);
    if SelectDirectory('Select folder',  '', directory) then begin
      Iterate('*.pas');
      Iterate('*.dcu');
    end;
    Model.Units := sl;
  finally sl.Free; end;
end; { TfrmPreferences.btnAddFromFolderClick }

procedure TfrmPreferencesExcludedUnits.HandleModelModified(Sender: TObject;
  modifiedComponents: TObjectList);
begin
  Reload;
end; { TfrmPreferencesInstrumentation.HandleModelModified }

procedure TfrmPreferencesExcludedUnits.memoExclUnitsExit(Sender: TObject);
begin
  Model.Units.Assign(memoExclUnits.Lines);
end; { TfrmPreferencesExcludedUnits.memoExclUnitsExit }

procedure TfrmPreferencesExcludedUnits.Reload;
begin
  memoExclUnits.Lines.Assign(Model.Units);
end; { TfrmPreferencesExcludedUnits.Reload }

procedure TfrmPreferencesExcludedUnits.SetModel(value: TGppPrefExcludedUnits);
begin
  if FModel <> value then begin
    UnsubscribeFromModel;
    FModel := value;
    SubscribeToModel;
  end;
end; { TfrmPreferencesExcludedUnits.SetModel }

procedure TfrmPreferencesExcludedUnits.SubscribeToModel;
begin
  if assigned(Model) then begin
    Model.Subscribe(HandleModelModified);
    Reload;
  end;
end; { TfrmPreferencesInstrumentation.SubscribeToModel }

procedure TfrmPreferencesExcludedUnits.UnsubscribeFromModel;
begin
  if assigned(Model) then
    Model.Unsubscribe(HandleModelModified);
end; { TfrmPreferencesInstrumentation.UnsubscribeFromModel }

end.

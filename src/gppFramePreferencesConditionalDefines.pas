(*:GpProfile 'conditional defines' preferences frame.
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

unit gppFramePreferencesConditionalDefines;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ComCtrls, Buttons, ExtCtrls, gppPreferencesModel, Contnrs,
  ActnList, ImgList;

type
  TfrmPreferencesConditionalDefines = class(TFrame)
    actAddDefine         : TAction;
    actClearAllDefines   : TAction;
    actDeleteDefine      : TAction;
    ActionList           : TActionList;
    bvlSymbols           : TBevel;
    cbConsoleDefines     : TCheckBox;
    cbDisableUserDefines : TCheckBox;
    cbProjectDefines     : TCheckBox;
    cbStandardDefines    : TCheckBox;
    cbxDelphi            : TComboBox;
    imgDefines           : TImageList;
    lblSymbols           : TLabel;
    lvDefines            : TListView;
    pnlConditionalDefines: TPanel;
    sbDeleteAll          : TSpeedButton;
    sbLayoutAdd          : TSpeedButton;
    sbLayoutRemove       : TSpeedButton;
    procedure actAddDefineExecute(Sender: TObject);
    procedure actClearAllDefinesExecute(Sender: TObject);
    procedure actClearAllDefinesUpdate(Sender: TObject);
    procedure actDeleteDefineExecute(Sender: TObject);
    procedure actDeleteDefineUpdate(Sender: TObject);
    procedure cbConsoleDefinesClick(Sender: TObject);
    procedure cbDisableUserDefinesClick(Sender: TObject);
    procedure cbProjectDefinesClick(Sender: TObject);
    procedure cbStandardDefinesClick(Sender: TObject);
    procedure cbxDelphiChange(Sender: TObject);
  private
    FModel: TGppPrefConditionalDefines;
    procedure ReloadListview;
  protected
    procedure HandleModelModified(Sender: TObject; modifiedComponents: TObjectList);
    function  MapTagToIndex(tag: TGppConditionalDefine): integer;
    procedure SetModel(value: TGppPrefConditionalDefines);
    procedure SubscribeToModel;
    procedure UnsubscribeFromModel;
  public
    procedure Reload;
    property Model: TGppPrefConditionalDefines read FModel write SetModel;
  end; { TfrmPreferencesConditionalDefines }

implementation

uses
  GpStuff,
  GpDelphiInfo,
  gppCommon;

{$R *.dfm}

procedure TfrmPreferencesConditionalDefines.actAddDefineExecute(Sender: TObject);
var
  cdSymbol: string;
begin
  cdSymbol := '';
  if InputQuery('GpProfile', 'Enter symbol name', cdSymbol) then
    if Model.IndexOf(cdSymbol) >= 0 then
      ShowMessage('Symbol ' + cdSymbol + ' is already defined!')
    else
      Model.UserDefines.Add(cdSymbol);
end; { TfrmPreferencesConditionalDefines.actAddDefineExecute }

procedure TfrmPreferencesConditionalDefines.actClearAllDefinesExecute(Sender: TObject);
begin
  Model.UserDefines.Clear;
end; { TfrmPreferencesConditionalDefines.actClearAllDefinesExecute }

procedure TfrmPreferencesConditionalDefines.actClearAllDefinesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (Model.UserDefines.Count > 0);
end; { TfrmPreferencesConditionalDefines.actClearAllDefinesUpdate }

procedure TfrmPreferencesConditionalDefines.actDeleteDefineExecute(Sender: TObject);
begin
  Model.UserDefines.Delete(Model.UserDefines.IndexOf(lvDefines.Selected.Caption));
end; { TfrmPreferencesConditionalDefines.actDeleteDefineExecute }

procedure TfrmPreferencesConditionalDefines.actDeleteDefineUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    assigned(lvDefines.Selected) and
    (Model[lvDefines.Selected.Index].Tag = cdUser);
end; { TfrmPreferencesConditionalDefines.actDeleteDefineUpdate }

procedure TfrmPreferencesConditionalDefines.cbConsoleDefinesClick(Sender: TObject);
begin
  if cbConsoleDefines.Checked then
    Model.Defines := Model.Defines + [cdConsoleApp]
  else
    Model.Defines := Model.Defines - [cdConsoleApp];
end; { TfrmPreferencesConditionalDefines.cbConsoleDefinesClick }

procedure TfrmPreferencesConditionalDefines.cbDisableUserDefinesClick(Sender: TObject);
begin
  if cbDisableUserDefines.Checked then
    Model.Defines := Model.Defines - [cdUser]
  else
    Model.Defines := Model.Defines + [cdUser];
end; { TfrmPreferencesConditionalDefines.cbDisableUserDefinesClick }

procedure TfrmPreferencesConditionalDefines.cbProjectDefinesClick(Sender: TObject);
begin
  if cbProjectDefines.Checked then
    Model.Defines := Model.Defines + [cdProject]
  else
    Model.Defines := Model.Defines - [cdProject];
end; { TfrmPreferencesConditionalDefines.cbProjectDefinesClick }

procedure TfrmPreferencesConditionalDefines.cbStandardDefinesClick(Sender: TObject);
begin
  if cbStandardDefines.Checked then
    Model.Defines := Model.Defines + [cdCompiler]
  else
    Model.Defines := Model.Defines - [cdCompiler];
end; { TfrmPreferencesConditionalDefines.cbStandardDefinesClick }

procedure TfrmPreferencesConditionalDefines.cbxDelphiChange(Sender: TObject);
begin
  Model.DelphiVersion := DelphiNameToVersion(cbxDelphi.Text);
end; { TfrmPreferencesConditionalDefines.cbxDelphiChange }

procedure TfrmPreferencesConditionalDefines.HandleModelModified(Sender: TObject;
  modifiedComponents: TObjectList);
begin
  Reload;
end; { TfrmPreferencesInstrumentation.HandleModelModified }

function TfrmPreferencesConditionalDefines.MapTagToIndex(tag: TGppConditionalDefine):
  integer;
begin
  case tag of
    cdCompiler:   Result := 0;
    cdConsoleApp: Result := 1;
    cdProject:    Result := 2;
    cdUser:       Result := IFF(cdUser in Model.Defines, 3, 4);
    else raise Exception.Create('Unknown conditional define tag');
  end;
end; { TfrmPreferencesConditionalDefines.MapTagToIndex }

procedure TfrmPreferencesConditionalDefines.Reload;
var
  delphi: TGpKnownDelphiVersions;
begin
  cbStandardDefines.Checked := (cdCompiler in Model.Defines);
  cbProjectDefines.Checked := (cdProject in Model.Defines);
  cbConsoleDefines.Checked := (cdConsoleApp in Model.Defines);
  cbDisableUserDefines.Checked := not (cdUser in Model.Defines);
  cbxDelphi.Items.Clear;
  for delphi := Low(TGpKnownDelphiVersions) to High(TGpKnownDelphiVersions) do
    if delphi in CGppSupportedDelphis then
      cbxDelphi.Items.Add(CGpKnownDelphiNames[delphi]);
  cbxDelphi.ItemIndex := cbxDelphi.Items.IndexOf(CGpKnownDelphiNames[Model.DelphiVersion]);
  if cbxDelphi.ItemIndex < 0 then begin
    cbxDelphi.ItemIndex := cbxDelphi.Items.Count-1;
    Model.DelphiVersion := DelphiNameToVersion(cbxDelphi.Items[cbxDelphi.ItemIndex]); 
  end;
  ReloadListview;
end; { TfrmPreferencesConditionalDefines.Reload }

procedure TfrmPreferencesConditionalDefines.ReloadListview;
var
  condDef: TGppConditionalDefineInfo;
  iCD    : integer;
begin
  lvDefines.Items.BeginUpdate;
  try
    lvDefines.Items.Clear;
    for iCD := 0 to Model.Count - 1 do begin
      condDef := Model[iCD];
      with lvDefines.Items.Add do begin
        Caption := condDef.Value;
        ImageIndex := MapTagToIndex(condDef.Tag);
      end;
    end;
  finally lvDefines.Items.EndUpdate; end;
end; { TfrmPreferencesConditionalDefines.ReloadListview }

procedure TfrmPreferencesConditionalDefines.SetModel(value: TGppPrefConditionalDefines);
begin
  if FModel <> value then begin
    UnsubscribeFromModel;
    FModel := value;
    SubscribeToModel;
  end;
end; { TfrmPreferencesConditionalDefines.SetModel }

procedure TfrmPreferencesConditionalDefines.SubscribeToModel;
begin
  if assigned(Model) then begin
    Model.Subscribe(HandleModelModified);
    Reload;
  end;
end; { TfrmPreferencesInstrumentation.SubscribeToModel }

procedure TfrmPreferencesConditionalDefines.UnsubscribeFromModel;
begin
  if assigned(Model) then
    Model.Unsubscribe(HandleModelModified);
end; { TfrmPreferencesInstrumentation.UnsubscribeFromModel }

end.

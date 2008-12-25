(*:GpProfile preferences frame container.
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

unit gppPreferences;

{$B-,H+,J+,Q-,T-,X+} //don't change!

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Menus, ImgList, ActnList, FileCtrl, Buttons, Contnrs,
  gppPreferencesModel,
  gppFramePreferencesInstrumentation,
  gppFramePreferencesConditionalDefines,
  gppFramePreferencesExcludedUnits,
  gppFramePreferencesAnalysis;

type
  TfrmPreferences = class(TForm)
    btnDefinesDefaults  : TButton;
    bvlFrameUnderline   : TBevel;
    oxButton1           : TButton;
    oxButton2           : TButton;
    pnlFrameHolder      : TPanel;
    sbAnalysis          : TSpeedButton;
    sbConditionalDefines: TSpeedButton;
    sbExcludedUnits     : TSpeedButton;
    sbInstrumentation   : TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure DisplaySettingsPage(Sender: TObject);
  private
    FfrmAnalysis          : TfrmPreferencesAnalysis;
    FfrmConditionalDefines: TfrmPreferencesConditionalDefines;
    FfrmExcludedUnits     : TfrmPreferencesExcludedUnits;
    FfrmInstrumentation   : TfrmPreferencesInstrumentation;
    FModel                : TGppPreferences;
  protected
    procedure CreateFrames;
    procedure ReloadView;
    procedure SetColors(parent: TWinControl);
    procedure SetModel(const value: TGppPreferences);
  public
    property Model: TGppPreferences read FModel write SetModel;
  end; { TfrmPreferences }

var
  frmPreferences: TfrmPreferences;

implementation

uses
  Math,
  GraphUtil,
  GpString,
  gppLookAndFeel;
  
{$R *.DFM}

procedure TfrmPreferences.CreateFrames;
begin
  FfrmInstrumentation := TfrmPreferencesInstrumentation.Create(Self);
  with FfrmInstrumentation do begin
    Parent := pnlFrameHolder;
    Left := 0;
    Top := 0;
    Align := alClient;
    if assigned(FModel) then
      Model := FModel.Instrumentation
    else
      Model := nil;
  end;
  FfrmAnalysis := TfrmPreferencesAnalysis.Create(Self);
  with FfrmAnalysis do begin
    Parent := pnlFrameHolder;
    Left := 0;
    Top := 0;
    Align := alClient;
    if assigned(FModel) then
      Model := FModel.Analysis
    else
      Model := nil;
  end;
  FfrmConditionalDefines := TfrmPreferencesConditionalDefines.Create(Self);
  with FfrmConditionalDefines do begin
    Parent := pnlFrameHolder;
    Left := 0;
    Top := 0;
    Align := alClient;
    if assigned(FModel) then
      Model := FModel.ConditionalDefines
    else
      Model := nil;
  end;
  FfrmExcludedUnits := TfrmPreferencesExcludedUnits.Create(Self);
  with FfrmExcludedUnits do begin
    Parent := pnlFrameHolder;
    Left := 0;
    Top := 0;
    Align := alClient;
    if assigned(FModel) then
      Model := FModel.ExcludedUnits
    else
      Model := nil;
  end;
end; { TfrmPreferences.CreateFrames }

procedure TfrmPreferences.DisplaySettingsPage(Sender: TObject);
begin
  FfrmInstrumentation.Visible := sbInstrumentation.Down;
  FfrmAnalysis.Visible := sbAnalysis.Down;
  FfrmExcludedUnits.Visible := sbExcludedUnits.Down;
  FfrmConditionalDefines.Visible := sbConditionalDefines.Down;
end; { TfrmPreferences.DisplaySettingsPage }

procedure TfrmPreferences.FormCreate(Sender: TObject);
begin
  CreateFrames;
  sbInstrumentation.Down := true;
  sbInstrumentation.Click;
  SetColors(Self);
end; { TfrmPreferences.FormCreate }

procedure TfrmPreferences.ReloadView;
begin
  FfrmInstrumentation.Reload;
  FfrmAnalysis.Reload;
  FfrmExcludedUnits.Reload;
  FfrmConditionalDefines.Reload;
end; { TfrmPreferences.ReloadView }

procedure TfrmPreferences.SetColors(parent: TWinControl);
var
  iControl: integer;
  control: TControl;
begin
  for iControl := 0 to parent.ControlCount - 1 do begin
    control := parent.Controls[iControl];
    if control is TWinControl then
      SetColors(TWinControl(control));
    if (control is TLabel) and (control.Tag = 1) then
      TLabel(control).Font.Color := CGppShadowMenuColor;
  end;
end; { TfrmPreferences.SetColors }

procedure TfrmPreferences.SetModel(const value: TGppPreferences);
begin
  FModel := value;
  if assigned(FfrmInstrumentation) then
    FfrmInstrumentation.Model := value.Instrumentation;
  if assigned(FfrmAnalysis) then
    FfrmAnalysis.Model := value.Analysis;
  if assigned(FfrmConditionalDefines) then
    FfrmConditionalDefines.Model := value.ConditionalDefines;
  if assigned(FfrmExcludedUnits) then
    FfrmExcludedUnits.Model := value.ExcludedUnits;
  ReloadView;
end; { TfrmPreferences.SetModel }

end.

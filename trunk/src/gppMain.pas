(*:GpProfile v2 main unit.
   @author Primoz Gabrijelcic
   @desc <pre>

This software is distributed under the BSD license.

Copyright (c) 2008, Primoz Gabrijelcic
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
   Creation date     : 2008-01-30
   Last modification : 2008-01-30
   Version           : 1.0
</pre>*)(*
   History:
     1.0: 2008-01-30
       - Added unit header.
*)

unit gppMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Contnrs, Graphics, Controls, Forms,
  Dialogs, ComCtrls, CustomizeDlg, StdCtrls, ButtonGroup, CategoryButtons, ImgList,
  ExtCtrls, ToolWin, Menus, Buttons, ActnPopup, XPMan, AppEvnts, SynEdit, VirtualTrees,
  JLLabel, SynEditHighlighter, SynHighlighterPas, ActnList,
  //GpSharedMemory,
  gppFrameInstrument, gppFrameAnalyze, gppFrameAnalyzeMethods, gppFrameAnalyzeClasses,
  gppFrameAnalyzeThreads, gppFrameAnalyzeUnits,

  gppProjectView,
  gppProjectModel,
  gppProfileModel,
  gppPreferencesModel;

type
  TfrmGpProfile = class(TForm, IGppProjectView)
    actInsRemoveInstrumentationSet: TAction;
    actInsSaveInstrumentationSet  : TAction;
    actInsSelectInstrumentationSet: TAction;
    actInsShowAllFolders          : TAction;
    ActionList                    : TActionList;
    actLayRemoveLayout            : TAction;
    actLaySaveLayout              : TAction;
    actLaySelectLayout            : TAction;
    actLayShowCalled              : TAction;
    actLayShowCallers             : TAction;
    actLayShowSource              : TAction;
    actProfDelete                 : TAction;
    actProfExport                 : TAction;
    actProfMakeCopy               : TAction;
    actProfMove                   : TAction;
    actProfOpen                   : TAction;
    actProfReload                 : TAction;
    actProfReopen: TAction;
    actProjInstrument             : TAction;
    actProjOpen                   : TAction;
    actProjReopen                 : TAction;
    actProjRescan                 : TAction;
    actProjRestore                : TAction;
    actToolHelp                   : TAction;
    actToolOptions                : TAction;
    actVieClasses                 : TAction;
    actVieMethods                 : TAction;
    actVieSelectThread            : TAction;
    actVieThreads                 : TAction;
    actVieUnits                   : TAction;
    ApplicationEvents             : TApplicationEvents;
    bvlToolbarAnalyze             : TBevel;
    bvlToolbarInstrument          : TBevel;
    bvlToolbarInstrumentation     : TBevel;
    bvlToolbarLayout              : TBevel;
    bvlToolbarViews               : TBevel;
    cbShowAllFolders              : TCheckBox;
    cbShowCalled                  : TCheckBox;
    cbShowCallers                 : TCheckBox;
    cbShowSource                  : TCheckBox;
    dlgOpenProfile                : TOpenDialog;
    dlgOpenProject                : TOpenDialog;
    grpToolbarAnalyze             : TGroupBox;
    grpToolbarHelp                : TGroupBox;
    grpToolbarInstrumentation     : TGroupBox;
    grpToolbarLayout              : TGroupBox;
    grpToolbarProject             : TGroupBox;
    grpToolbarViews               : TGroupBox;
    lblInstrumentationSet         : TLabel;
    lblLayout                     : TLabel;
    lblSubmenuHelp                : TRotateLabel;
    lblViewThread                 : TLabel;
    pbMenuGradient                : TPaintBox;
    pnlMainMenu                   : TPanel;
    pnlSourcePreview              : TPanel;
    pnlSubmenuHelp                : TPanel;
    pnlSubmenuInstrumentationSet  : TPanel;
    pnlSubmenuLayout              : TPanel;
    pnlSubmenuReopenProfile       : TPanel;
    pnlSubmenuReopenProject       : TPanel;
    pnlSubmenuThreads             : TPanel;
    pnlToolbarAnalyze             : TPanel;
    pnlToolbarHelp                : TPanel;
    pnlToolbarInstrumentation     : TPanel;
    pnlToolbarLayout              : TPanel;
    pnlToolbarProject             : TPanel;
    pnlToolbarViews               : TPanel;
    sbAbout                       : TSpeedButton;
    sbClassesView                 : TSpeedButton;
    sbCopyProfile                 : TSpeedButton;
    sbDeleteProfile               : TSpeedButton;
    sbEmailAuthor                 : TSpeedButton;
    sbExportProfile               : TSpeedButton;
    sbHelp                        : TSpeedButton;
    sbHelpMenu                    : TSpeedButton;
    sbHomePage                    : TSpeedButton;
    sbInstrument                  : TSpeedButton;
    sbInstrumentationSetSelector  : TSpeedButton;
    sbJoinMailingList             : TSpeedButton;
    sbLayoutAdd                   : TSpeedButton;
    sbLayoutRemove                : TSpeedButton;
    sbLayoutSelector              : TSpeedButton;
    sbMethodsView                 : TSpeedButton;
    sbMoveProfile                 : TSpeedButton;
    sbOpenProfile                 : TSpeedButton;
    sbOpenProject                 : TSpeedButton;
    sbOptions                     : TSpeedButton;
    sbQuickStart                  : TSpeedButton;
    sbReloadProfile               : TSpeedButton;
    sbRemoveInstrumentationSet    : TSpeedButton;
    sbReopenProfileMenu           : TSpeedButton;
    sbReopenProjectMenu           : TSpeedButton;
    sbRescanProject               : TSpeedButton;
    sbRestore                     : TSpeedButton;
    sbSaveInstrumentationSet      : TSpeedButton;
    sbShortcuts                   : TSpeedButton;
    sbThreadSelector              : TSpeedButton;
    sbThreadsView                 : TSpeedButton;
    sbUnitsView                   : TSpeedButton;
    splitSourcePreview            : TSplitter;
    StatusBar                     : TStatusBar;
    SynPasSyn                     : TSynPasSyn;
    SynSource                     : TSynEdit;
    tcMainMenu                    : TTabControl;
    XPManifest                    : TXPManifest;
    procedure ActionListUpdate(Action: TBasicAction; var Handled: boolean);
    procedure actProfOpenExecute(Sender: TObject);
    procedure actProfReopenExecute(Sender: TObject);
    procedure actProfReopenUpdate(Sender: TObject);
    procedure actProjInstrumentExecute(Sender: TObject);
    procedure actProjOpenExecute(Sender: TObject);
    procedure actProjReopenExecute(Sender: TObject);
    procedure actProjReopenUpdate(Sender: TObject);
    procedure actProjRescanExecute(Sender: TObject);
    procedure actProjRestoreExecute(Sender: TObject);
    procedure ApplicationEventsMessage(var msg: tagMSG; var handled: boolean);
    procedure EnableIfProjectIsOpen(Sender: TObject);
    procedure ExecuteMenuItem(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HideHelpSubmenu(Sender: TObject);
    procedure MenuDropDownClick(Sender: TObject);
    procedure PaintGradientHorizontal(Sender: TObject);
    procedure PaintGradientVertical(Sender: TObject);
    procedure RedisplayAnalysisViews(Sender: TObject);
    procedure sbOptionsClick(Sender: TObject);
    procedure tcMainMenuChange(Sender: TObject);
  private
    FBaseSettingsFolder: string;
    FfrmAnalyze        : TfrmAnalyze;
    FfrmAnalyzeClasses : TfrmAnalyzeClasses;
    FfrmAnalyzeMethods : TfrmAnalyzeMethods;
    FfrmAnalyzeThreads : TfrmAnalyzeThreads;
    FfrmAnalyzeUnits   : TfrmAnalyzeUnits;
    FfrmInstrument     : TfrmInstrument;
    FMenuDropped       : TPanel;
    FMRUProfiles       : TStringList;
    FMRUProjects       : TStringList;
    FPreferencesModel  : TGppPreferences;
    FProfileModel      : IGppProfileModel;
    FProjectModel      : IGppProjectModel;
    FWindowCaption     : string;
    function  MapToDpr(const fileName: string): string;
    procedure OpenProfile(const fileName: string);
    procedure OpenProject(const fileName: string);
    procedure RebuildMenu(menuHost: TPanel; menuItems: TStringList);
  protected
    procedure AddToMRU(const fileName: string; mruList: TStringList; menuHost: TPanel; const
      persistName: string);
    function  BaseSettingsFolder: string;
    procedure BringMenusToFront;
    procedure CreateFrames;
    procedure CreateModels;
    procedure CreateViews;
    procedure DestroyModels;
    function  FormBoundsRect(const control: TControl): TRect;
    procedure HandleProjectModelModified(Sender: TObject; modifiedComponents: TObjectList);
    procedure HideSubmenu;
    procedure HookGradients(winControl: TWinControl);
    procedure LoadDefaultPreferences;
    function  LoadPreferences(const prefFile: string; preferences: TGppPreferences): boolean;
    procedure NotifyUnitInstrumenting(Sender: TObject; const unitName: string);
    procedure NotifyUnitParsing(Sender: TObject; const unitName: string);
    procedure RearrangeToolboxes;
    procedure SaveDefaultPreferences;
    function  SavePreferences(const prefFile: string; preferences: TGppPreferences): boolean;
    procedure SetColorsInAllPanels(owner: TWinControl);
    procedure SetColorsInMenusAndControls;
    procedure WordWrapCaptions;
  public
    procedure  ChangeStatus(const status: string);
  end; { TfrmGpProfile }

var
  frmGpProfile: TfrmGpProfile;

implementation

uses
  TypInfo,
  Math,
  DSiWin32,
  GraphUtil,
  GpVCL,
  GpProperty,
  GpStuff,
  GpStreams,
  GpXMLSerializer,
  GpCompositeModel,
  GpManagedClass,
  gppCommon,
  gppLookAndFeel,
  gppPreferences;

{$R *.dfm}

const
  CPreferencesExtension = '.gpprofile';
  CPreferencesFile      = 'Preferences' + CPreferencesExtension;

  CMRUProjects = 'RecentlyUsedProjects';
  CMRUProfiles = 'RecentlyUsedProfileData';

  CMenuOpenProject = 1;

procedure TfrmGpProfile.ActionListUpdate(Action: TBasicAction; var Handled: boolean);
begin
  cbShowAllFolders.Enabled := FProjectModel.IsOpen;
  sbInstrumentationSetSelector.Enabled := FProjectModel.IsOpen;
  sbSaveInstrumentationSet.Enabled := FProjectModel.IsOpen;
  sbRemoveInstrumentationSet.Enabled := FProjectModel.IsOpen;
end; { TfrmGpProfile.ActionListUpdate }

procedure TfrmGpProfile.actProfOpenExecute(Sender: TObject);
begin
  with dlgOpenProfile do begin
    if FProfileModel.Name = '' then
      FileName := '*.gpprf2'
    else
      FileName := ChangeFileExt(FProfileModel.Name, '.gpprf2');
    if Execute then
      OpenProfile(MapToDpr(FileName));
  end;
end; { TfrmGpProfile.actProfOpenExecute }

procedure TfrmGpProfile.actProfReopenExecute(Sender: TObject);
begin
  MenuDropDownClick(sbReopenProfileMenu);
end; { TfrmGpProfile.actProfReopenExecute }

procedure TfrmGpProfile.actProfReopenUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FMRUProfiles.Count > 0);
end; { TfrmGpProfile.actProfReopenUpdate }

procedure TfrmGpProfile.actProjInstrumentExecute(Sender: TObject);
begin
  FProjectModel.InstrumentProjectSource(not cbShowAllFolders.Checked);
  ChangeStatus('Instrumented');
end; { TfrmGpProfile.actProjInstrumentExecute }

procedure TfrmGpProfile.actProjOpenExecute(Sender: TObject);
begin
  with dlgOpenProject do begin
    if FProjectModel.Name = '' then
      FileName := '*.dpr'
    else
      FileName := ChangeFileExt(FProjectModel.Name, '.dpr');
    if Execute then
      OpenProject(FileName);
  end;
end; { TfrmGpProfile.actProjOpenExecute }

procedure TfrmGpProfile.actProjReopenExecute(Sender: TObject);
begin
  MenuDropDownClick(sbReopenProjectMenu);
end; { TfrmGpProfile.actProjReopenExecute }

procedure TfrmGpProfile.actProjReopenUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FMRUProjects.Count > 0);
end; { TfrmGpProfile.actProjReopenUpdate }

procedure TfrmGpProfile.actProjRescanExecute(Sender: TObject);
begin
  // TODO 1 -oPrimoz Gabrijelcic : implement: TfrmGpProfile.actProjRescanExecute
end; { TfrmGpProfile.actProjRescanExecute }

procedure TfrmGpProfile.actProjRestoreExecute(Sender: TObject);
begin
  FProjectModel.ChangeInstrumentation(false);
  FProjectModel.InstrumentProjectSource(false);
  ChangeStatus('Instrumentation removed');
end; { TfrmGpProfile.actProjRestoreExecute }

procedure TfrmGpProfile.AddToMRU(const fileName: string; mruList: TStringList; menuHost:
  TPanel; const persistName: string);
var
  idxFileName: integer;
begin
  idxFileName := mruList.IndexOf(fileName);
  if idxFileName < 0 then
    idxFileName := mruList.Add(fileName);
  if idxFileName > 0 then
    mruList.Move(idxFileName, 0);
  RebuildMenu(menuHost, mruList);
  DSiWriteRegistry(CRegistryUI, persistName, mruList.Text);
end; { TfrmGpProfile.AddToMRU }

procedure TfrmGpProfile.ApplicationEventsMessage(var msg: tagMSG; var handled: boolean);
var
  ptMouse: TPoint;
begin
  if assigned(FMenuDropped) then begin
    if (msg.message >= WM_LBUTTONUP) and (msg.Message <= WM_MOUSEWHEEL) then begin
      GetCursorPos(ptMouse);
      ptMouse := ScreenToClient(ptMouse);
      if not (PtInRect(FMenuDropped.BoundsRect, ptMouse) or
              PtInRect(FormBoundsRect(TSpeedButton(FMenuDropped.Tag)), ptMouse)) then
      begin
        HideSubmenu;
        handled := false;
      end;
    end
    else if (msg.Message = WM_KEYDOWN) and (msg.WParam = VK_ESCAPE) then begin
      HideSubmenu;
      handled := true;
    end;
  end;
end; { TfrmGpProfile.ApplicationEventsMessage }

function TfrmGpProfile.BaseSettingsFolder: string;
begin
  if FBaseSettingsFolder = '' then begin
    FBaseSettingsFolder := DSiGetFolderLocation(CSIDL_LOCAL_APPDATA);
    if FBaseSettingsFolder <> '' then
      FBaseSettingsFolder := IncludeTrailingPathDelimiter(FBaseSettingsFolder) + 'Gp'
    else
      FBaseSettingsFolder := ExtractFilePath(ParamStr(0));
    FBaseSettingsFolder := IncludeTrailingPathDelimiter(
      IncludeTrailingPathDelimiter(FBaseSettingsFolder) + 'GpProfile');
    if not ForceDirectories(FBaseSettingsFolder) then
      FBaseSettingsFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  end;
  Result := FBaseSettingsFolder;
end; { TfrmGpProfile.BaseSettingsFolder }

procedure TfrmGpProfile.BringMenusToFront;
begin
  pnlSubmenuHelp.BringToFront;
  pnlSubmenuReopenProject.BringToFront;
  pnlSubmenuReopenProfile.BringToFront;
  pnlSubmenuInstrumentationSet.BringToFront;
  pnlSubmenuLayout.BringToFront;
  pnlSubmenuThreads.BringToFront;
end; { TfrmGpProfile.BringMenusToFront }

procedure TfrmGpProfile.ChangeStatus(const status: string);
begin
  StatusBar.Panels[0].Text := status;
end; { TfrmGpProfile.ChangeStatus }

procedure TfrmGpProfile.CreateFrames;
begin
  FfrmInstrument := TfrmInstrument.Create(Self);
  with FfrmInstrument do begin
    Parent := Self;
    Left := 0;
    Top := 101;
    Align := alClient;
    TabOrder := 5;
    TabStop := True;
    Model := FProjectModel;
  end;
  HookGradients(FfrmInstrument);
  FfrmAnalyze := TfrmAnalyze.Create(Self);
  with FfrmAnalyze do begin
    Parent := Self;
    Left := 0;
    Top := 101;
    Align := alClient;
    TabOrder := 6;
    TabStop := True;
  end;
  HookGradients(FfrmAnalyze);
  FfrmAnalyzeMethods := TfrmAnalyzeMethods.Create(Self);
  with FfrmAnalyzeMethods do begin
    Parent := FfrmAnalyze;
    Left := 0;
    Top := 0;
    Align := alClient;
    TabStop := true;
  end;
  HookGradients(FfrmAnalyzeMethods);
  FfrmAnalyzeThreads := TfrmAnalyzeThreads.Create(Self);
  with FfrmAnalyzeThreads do begin
    Parent := FfrmAnalyze;
    Left := 0;
    Top := 0;
    Align := alClient;
    TabStop := true;
  end;
  FfrmAnalyzeClasses := TfrmAnalyzeClasses.Create(Self);
  with FfrmAnalyzeClasses do begin
    Parent := FfrmAnalyze;
    Left := 0;
    Top := 0;
    Align := alClient;
    TabStop := true;
  end;
  FfrmAnalyzeUnits := TfrmAnalyzeUnits.Create(Self);
  with FfrmAnalyzeUnits do begin
    Parent := FfrmAnalyze;
    Left := 0;
    Top := 0;
    Align := alClient;
    TabStop := true;
  end;
end; { TfrmGpProfile.CreateFrames }

procedure TfrmGpProfile.CreateModels;
begin
  FProjectModel := gppProjectModel.CreateModel;
  FProjectModel.Subscribe(HandleProjectModelModified);
  FProjectModel.OnInstrumenting := NotifyUnitInstrumenting;
  FProjectModel.OnParsing := NotifyUnitParsing;
  FProfileModel := gppProfileModel.CreateModel;
  FPreferencesModel := TGppPreferences.Create;
end; { TfrmGpProfile.CreateModels }

procedure TfrmGpProfile.CreateViews;
begin
  CreateFrames;
  BringMenusToFront;
  SetColorsInMenusAndControls;
  WordWrapCaptions;
  RearrangeToolboxes;
end; { TfrmGpProfile.CreateViews }

procedure TfrmGpProfile.DestroyModels;
begin
  FProjectModel.Unsubscribe(HandleProjectModelModified);
  FProjectModel := nil;
  FProfileModel := nil;
  FreeAndNil(FPreferencesModel);
end; { TfrmGpProfile.DestroyModels }

procedure TfrmGpProfile.EnableIfProjectIsOpen(Sender: TObject);
begin
  (Sender as TAction).Enabled := FProjectModel.IsOpen;
end; { TfrmGpProfile.EnableIfProjectIsOpen }

procedure TfrmGpProfile.ExecuteMenuItem(Sender: TObject);
begin
  HideHelpSubmenu(Sender);
  if (Sender as TComponent).Tag = CMenuOpenProject then
    OpenProject((Sender as TControl).Hint);
end; { TfrmGpProfile.ExecuteMenuItem }

function TfrmGpProfile.FormBoundsRect(const control: TControl): TRect;
begin
  Result.TopLeft := control.Parent.ClientToParent(control.BoundsRect.TopLeft, Self);
  Result.BottomRight := control.Parent.ClientToParent(control.BoundsRect.BottomRight, Self);
end; { TfrmGpProfile.FormBoundsRect }

procedure TfrmGpProfile.FormCreate(Sender: TObject);
begin
  FWindowCaption := Caption;
  CreateModels;
  CreateViews;
  FMRUProjects := TStringList.Create;
  FMRUProjects.Text := DSiReadRegistry(CRegistryUI, CMRUProjects, '');
  RebuildMenu(pnlSubmenuReopenProject, FMRUProjects);
  FMRUProfiles := TStringList.Create;
  FMRUProfiles.Text := DSiReadRegistry(CRegistryUI, CMRUProfiles, '');
  RebuildMenu(pnlSubmenuReopenProfile, FMRUProfiles);
  LoadDefaultPreferences;
end; { TfrmGpProfile.FormCreate }

procedure TfrmGpProfile.FormDestroy(Sender: TObject);
begin
  DestroyModels;
  FreeAndNil(FMRUProjects);
  FreeAndNil(FMRUProfiles);
end; { TfrmGpProfile.FormDestroy }

procedure TfrmGpProfile.HandleProjectModelModified(Sender: TObject;
  modifiedComponents: TObjectList);
begin
  if FProjectModel.Name = '' then
    Caption := FWindowCaption
  else
    Caption := FWindowCaption + ' - ' +
      ChangeFileExt(ExtractFileName(FProjectModel.Name), '');
end; { TfrmGpProfile.HandleProjectModelModified }

procedure TfrmGpProfile.HideHelpSubmenu(Sender: TObject);
begin
  HideSubmenu;
end; { TfrmGpProfile.HideHelpSubmenu }

procedure TfrmGpProfile.HideSubmenu;
begin
  if assigned(FMenuDropped) then begin
    FMenuDropped.Visible := false;
    FMenuDropped := nil;
  end;
end; { TfrmGpProfile.HideSubmenu }

procedure TfrmGpProfile.HookGradients(winControl: TWinControl);
var
  control : TControl;
  iControl: integer;
begin
  for iControl := 0 to winControl.ControlCount - 1 do begin
    control := winControl.Controls[iControl];
    if control is TPaintBox then begin
      if control.Width < control.Height then
        TPaintBox(control).OnPaint := PaintGradientHorizontal
      else
        TPaintBox(control).OnPaint := PaintGradientVertical;
    end
    else if control is TPanel then
      HookGradients(TPanel(control));
  end;
end; { TfrmGpProfile.HookGradients }

procedure TfrmGpProfile.LoadDefaultPreferences;
begin
  LoadPreferences(BaseSettingsFolder + CPreferencesFile, FPreferencesModel);
end; { TfrmGpProfile.LoadDefaultPreferences }

function TfrmGpProfile.LoadPreferences(const prefFile: string; preferences:
  TGppPreferences): boolean;
var
  strPreferences: TFileStream;
begin
  Result := false;
  if FileExists(prefFile) then begin
    if SafeCreateFileStream(prefFile, fmOpenRead, strPreferences) then try
      CreateGpXMLSerializer.Restore(preferences, strPreferences);
      Result := true;
    finally FreeAndNil(strPreferences); end;
  end;
end; { TfrmGpProfile.ChangeStatus }

function TfrmGpProfile.MapToDpr(const fileName: string): string;
begin
  if DSiFileExtensionIs(fileName, ['.bdsproj', '.dproj']) then
    Result := ChangeFileExt(fileName, '.dpr')
  else
    Result := fileName;
end; { TfrmGpProfile.MapToDpr }

procedure TfrmGpProfile.MenuDropDownClick(Sender: TObject);
var
  button    : TSpeedButton;
  clickPoint: TPoint;
  oldMenu   : TPanel;
begin
  button := (Sender as TSpeedButton);
  oldMenu := FMenuDropped;
  if assigned(FMenuDropped) then
    HideSubmenu;
  if button = sbHelpMenu then
    FMenuDropped := pnlSubmenuHelp
  else if button = sbReopenProjectMenu then
    FMenuDropped := pnlSubmenuReopenProject
  else if button = sbReopenProfileMenu then
    FMenuDropped := pnlSubmenuReopenProfile
  else if button = sbInstrumentationSetSelector then
    FMenuDropped := pnlSubmenuInstrumentationSet
  else if button = sbLayoutSelector then
    FMenuDropped := pnlSubmenuLayout
  else if button = sbThreadSelector then
    FMenuDropped := pnlSubmenuThreads;
  if (not assigned(FMenuDropped)) or (FMenuDropped = oldMenu) then begin
    FMenuDropped := nil;
    Exit;
  end;
  FMenuDropped.Tag := integer(Sender);
  clickPoint := button.ClientToScreen(Point(button.ClientWidth, button.ClientHeight));
  if FMenuDropped.Height + clickPoint.Y > Screen.MonitorFromPoint(clickPoint).Height then
    Dec(clickPoint.Y, button.Height);
  if GetComCtlVersion = ComCtlVersionIE5 then
    button.Invalidate;
  with ScreenToClient(clickPoint) do begin
    FMenuDropped.Left := Max(0, X - FMenuDropped.Width);
    FMenuDropped.Top := Y;
  end;
  FMenuDropped.Visible := true;
end; { TfrmGpProfile.sbLayoutSelectorClick }

procedure TfrmGpProfile.NotifyUnitInstrumenting(Sender: TObject; const unitName: string);
begin
  ChangeStatus('Instrumenting: ' + unitName);
end; { TfrmGpProfile.NotifyUnitInstrumenting }

procedure TfrmGpProfile.NotifyUnitParsing(Sender: TObject; const unitName: string);
begin
  ChangeStatus('Parsing: ' + unitName);
end; { TfrmGpProfile.NotifyUnitParsing }

procedure TfrmGpProfile.OpenProfile(const fileName: string);
begin
  AddToMRU(fileName, FMRUProfiles, pnlSubmenuReopenProfile, CMRUProfiles);
  FProfileModel.OpenProfile(fileName);
  if not FProfileModel.Load then begin
    MessageDlg((FProfileModel as IGpManagedErrorHandling).GetLastErrorMsg,
      mtError, [mbOK], -1);
    ChangeStatus('Loading error');
  end
  else
    ChangeStatus('Loaded');
end; { TfrmGpProfile.OpenProfile }

procedure TfrmGpProfile.OpenProject(const fileName: string);
begin
  AddToMRU(fileName, FMRUProjects, pnlSubmenuReopenProject, CMRUProjects);
  if not LoadPreferences(ChangeFileExt(fileName, CPreferencesExtension),
           FProjectModel.ProjectPreferences)
  then
    CreateGpXMLSerializer.Copy(FPreferencesModel, FProjectModel.ProjectPreferences);
  FProjectModel.OpenProject(fileName);
  if not FProjectModel.Parse then begin
    MessageDlg((FProjectModel as IGpManagedErrorHandling).GetLastErrorMsg,
      mtError, [mbOK], -1);
    ChangeStatus('Parsing error');
  end
  else
    ChangeStatus('Parsed');
end; { TfrmGpProfile.OpenProject }

procedure TfrmGpProfile.PaintGradientHorizontal(Sender: TObject);
begin
  GppPaintHorizontalGradient(Sender as TPaintBox);
end; { TfrmGpProfile.PaintGradientHorizontal }

procedure TfrmGpProfile.PaintGradientVertical(Sender: TObject);
begin
  GppPaintVerticalGradient(Sender as TPaintBox);
end; { TfrmGpProfile.PaintGradientVertical }

procedure TfrmGpProfile.RearrangeToolboxes;

  procedure NextTo(toolbox1, toolbox2: TGroupBox);
  begin
    toolbox2.Top := toolbox1.Top;
    toolbox2.Left := toolbox1.Left + toolbox1.Width + 3;
  end; { NextTo }

var
  firstVisibleToolbox: TGroupBox;

  procedure ChainToolbar(toolbox: TGroupBox);
  begin
    if toolbox.Visible then begin
      NextTo(firstVisibleToolbox, toolbox);
      firstVisibleToolbox := toolbox;
    end;
  end; { ChainToolbar }

begin { TfrmGpProfile.RearrangeToolboxes }
  if tcMainMenu.TabIndex = 0 then begin
    grpToolbarProject.Visible := true;
    grpToolbarAnalyze.Visible := false;
    grpToolbarLayout.Visible := false;
    firstVisibleToolbox := grpToolbarProject;
    FfrmInstrument.Visible := true;
    FfrmAnalyze.Visible := false;
  end
  else begin
    grpToolbarProject.Visible := false;
    grpToolbarAnalyze.Visible := true;
    grpToolbarLayout.Visible := true;
    firstVisibleToolbox := grpToolbarAnalyze;
    FfrmInstrument.Visible := false;
    FfrmAnalyze.Visible := true;
  end;
  grpToolbarInstrumentation.Visible := grpToolbarProject.Visible;
  grpToolbarViews.Visible := grpToolbarAnalyze.Visible;
  firstVisibleToolbox.Left := 3;
  firstVisibleToolbox.Top := 3;
  ChainToolbar(grpToolbarInstrumentation);
  ChainToolbar(grpToolbarViews);
  ChainToolbar(grpToolbarLayout);
  ChainToolbar(grpToolbarHelp);
  if tcMainMenu.TabIndex = 1 then
    RedisplayAnalysisViews(nil);
end; { TfrmGpProfile.RearrangeToolboxes }

procedure TfrmGpProfile.RebuildMenu(menuHost: TPanel; menuItems: TStringList);
const
  CButtonHeight = 20;
  CMinPanelHeight = 130;
var
  menuButton : TSpeedButton;
  menuButtons: TObjectList;
  menuPanel  : TPanel;
  iButton: Integer;

  function Button(idxButton: integer): TSpeedButton;
  begin
    Result := (menuButtons[idxButton] as TSpeedButton);
  end; { Button }

begin { TfrmGpProfile.RebuildMenu }
  menuPanel := ControlByClass(menuHost, TPanel) as TPanel;
  menuButtons := TObjectList.Create;
  try
    ControlsByClass(menuPanel, TSpeedButton, menuButtons);
    while menuButtons.Count < menuItems.Count do begin
      // create button
      menuButton := TSpeedButton.Create(menuPanel);
      if menuButtons.Count = 0 then
        menuButton.Top := 0
      else
        menuButton.Top := Button(menuButtons.Count - 1).Top + CButtonHeight;
      menuButton.Left := 0;
      menuButton.Width := menuPanel.Width;
      menuButton.Height := CButtonHeight;
      menuButton.Flat := true;
      menuButton.OnClick := ExecuteMenuItem;
      menuButton.Parent := menuPanel;
      menuButton.ShowHint := true;
      menuButton.Tag := CMenuOpenProject;
      menuButtons.Add(menuButton);
    end;
    while menuButtons.Count > menuItems.Count do begin
      menuButton := menuButtons[menuButtons.Count - 1] as TSpeedButton;
      menuButtons.Delete(menuButtons.Count - 1);
      menuButton.Free;
    end;
    for iButton := 0 to menuButtons.Count - 1 do begin
      Button(iButton).Caption := ChangeFileExt(ExtractFileName(menuItems[iButton]), '');
      Button(iButton).Hint := menuItems[iButton];
    end;
    if (menuButtons.Count = 0) or
       ((Button(menuButtons.Count - 1).Top + CButtonHeight) < CMinPanelHeight)
    then
      menuPanel.Height := CMinPanelHeight
    else
      menuPanel.Height := Button(menuButtons.Count - 1).Top + CButtonHeight;
  finally FreeAndNil(menuButtons); end;
end; { TfrmGpProfile.RebuildMenu }

procedure TfrmGpProfile.RedisplayAnalysisViews(Sender: TObject);
begin
  FfrmAnalyzeMethods.Visible := sbMethodsView.Down;
  FfrmAnalyzeClasses.Visible := sbClassesView.Down;
  FfrmAnalyzeUnits.Visible   := sbUnitsView.Down;
  FfrmAnalyzeThreads.Visible := sbThreadsView.Down;
  if sbMethodsView.Down then
    FfrmAnalyze.lblViewCaption.Caption := 'Methods'
  else if sbClassesView.Down then
    FfrmAnalyze.lblViewCaption.Caption := 'Classes'
  else if sbUnitsView.Down then
    FfrmAnalyze.lblViewCaption.Caption := 'Units'
  else
    FfrmAnalyze.lblViewCaption.Caption := 'Threads';
end; { TfrmGpProfile.RedisplayAnalysisViews }

procedure TfrmGpProfile.SaveDefaultPreferences;
begin
  SavePreferences(BaseSettingsFolder + CPreferencesFile, FPreferencesModel);
end; { TfrmGpProfile.SaveDefaultPreferences }

function TfrmGpProfile.SavePreferences(const prefFile: string; preferences:
  TGppPreferences): boolean;
var
  strPreferences: TFileStream;
begin
  Result := false;
  if SafeCreateFileStream(prefFile, fmCreate, strPreferences) then try
    CreateGpXMLSerializer.Serialize(preferences, strPreferences);
    Result := true;
  finally FreeAndNil(strPreferences); end;
end; { TfrmGpProfile.SavePreferences }

procedure TfrmGpProfile.sbOptionsClick(Sender: TObject);
var
  frmPreferences : TfrmPreferences;
  serializer     : IGpXMLSerializer;
  tempPreferences: TGppPreferences;
begin
  serializer := CreateGpXMLSerializer;
  tempPreferences := TGppPreferences.Create;
  try
    if FProjectModel.IsOpen then
      serializer.Copy(FProjectModel.ProjectPreferences, tempPreferences)
    else
      serializer.Copy(FPreferencesModel, tempPreferences);
    frmPreferences := TfrmPreferences.Create(Self);
    try
      frmPreferences.Model := tempPreferences;
      if frmPreferences.ShowModal = mrOK then begin
        if FProjectModel.IsOpen then begin
          serializer.Copy(tempPreferences, FProjectModel.ProjectPreferences);
          SavePreferences(ChangeFileExt(FProjectModel.Name, CPreferencesExtension),
            FProjectModel.ProjectPreferences);
        end
        else begin
          serializer.Copy(tempPreferences, FPreferencesModel);
          SaveDefaultPreferences;
        end;
      end;
    finally FreeAndNil(frmPreferences); end;
  finally FreeAndNil(tempPreferences); end;
end; { TfrmGpProfile.sbOptionsClick }

procedure TfrmGpProfile.SetColorsInAllPanels(owner: TWinControl);
var
  control: TControl;
  iPanel : integer;
  pnlComp: TPanel;
begin
  for iPanel := 0 to owner.ControlCount - 1 do begin
    control := owner.Controls[iPanel];
    if control is TPanel then begin
      pnlComp := TPanel(control);
      if pnlComp.Tag = 1 then begin
        pnlComp.Color := CGppHighlightMenuColor;
        pnlComp.Font.Color := CGppShadowMenuColor;
      end;
    end
    else if control is TRotateLabel then
      TRotateLabel(control).Font.Color := CGppShadowMenuColor
    else if control is TSplitter then
      TSplitter(control).Color := CGppHighlightMenuColor;
    if (control is TPanel) or (control is TFrame) then
      SetColorsInAllPanels(TWinControl(control));
  end;
end; { TfrmGpProfile.SetColorsInAllPanels }

procedure TfrmGpProfile.SetColorsInMenusAndControls;
var
  iGroup: integer;
begin
  for iGroup := 0 to pnlMainMenu.ControlCount - 1 do
    if pnlMainMenu.Controls[iGroup] is TGroupBox then
      SetColorsInAllPanels(TWinControl(pnlMainMenu.Controls[iGroup]));
  for iGroup := 0 to ControlCount - 1 do
    if (Controls[iGroup] is TPanel) or (Controls[iGroup] is TFrame) then
      SetColorsInAllPanels(TWinControl(Controls[iGroup]));
  FfrmInstrument.lblUnits.Font.Color := CGppShadowMenuColor;
  FfrmInstrument.lblClasses.Font.Color := CGppShadowMenuColor;
  FfrmInstrument.lblProcs.Font.Color := CGppShadowMenuColor;
  FfrmAnalyze.lblViewCaption.Font.Color := CGppShadowMenuColor;
  FfrmAnalyzeMethods.lblCalled.Font.Color := CGppShadowMenuColor;
  FfrmAnalyzeMethods.lblCallers.Font.Color := CGppShadowMenuColor;
end; { TfrmGpProfile.SetColorsInMenusAndControls }

procedure TfrmGpProfile.tcMainMenuChange(Sender: TObject);
begin
  RearrangeToolboxes;
end; { TfrmGpProfile.tcMainMenuChange }

procedure TfrmGpProfile.WordWrapCaptions;
var
  grpComp   : TGroupBox;
  iComp     : IGpProperty;
  iControl  : integer;
  idxCaption: integer;
  iGroup    : integer;
begin
  for iGroup := 0 to pnlMainMenu.ControlCount - 1 do
    if pnlMainMenu.Controls[iGroup] is TGroupBox then begin
      grpComp := TGroupBox(pnlMainMenu.Controls[iGroup]);
      for iControl := 0 to grpComp.ControlCount - 1 do begin
        iComp := CreateGpProperty(grpComp.Controls[iControl]);
        idxCaption := iComp.IndexOf('Caption');
        if (idxCaption >= 0) and (Pos('{}', iComp.StringValue[idxCaption]) > 0) then
          iComp.StringValue[idxCaption] := StringReplace(iComp.StringValue[idxCaption],
            '{}', #13#10, [rfReplaceAll]);
      end;
    end;
end; { TfrmGpProfile.WordWrapCaptions }

end.

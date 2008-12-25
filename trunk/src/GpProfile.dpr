program GpProfile;

uses
  Forms,
  gppMain in 'gppMain.pas' {frmGpProfile},
  gppFrameInstrument in 'gppFrameInstrument.pas' {frmInstrument: TFrame},
  gppFrameAnalyze in 'gppFrameAnalyze.pas' {frmAnalyze: TFrame},
  gppFrameAnalyzeClasses in 'gppFrameAnalyzeClasses.pas' {frmAnalyzeClasses: TFrame},
  gppFrameAnalyzeMethods in 'gppFrameAnalyzeMethods.pas' {frmAnalyzeMethods: TFrame},
  gppFrameAnalyzeThreads in 'gppFrameAnalyzeThreads.pas' {frmAnalyzeThreads: TFrame},
  gppFrameAnalyzeUnits in 'gppFrameAnalyzeUnits.pas' {frmAnalyzeUnits: TFrame},
  gppProjectModel in 'gppProjectModel.pas',
  gppParser in 'gppParser.pas',
  gppIDT in 'gppIDT.pas',
  gppcommon in 'gppcommon.pas',
  gppFileEdit in 'gppFileEdit.pas',
  gppProjectView in 'gppProjectView.pas',
  gppProfileModel in 'gppProfileModel.pas',
  gppPreferencesModel in 'gppPreferencesModel.pas',
  gppLookAndFeel in 'gppLookAndFeel.pas',
  gppPreferences in 'gppPreferences.pas' {frmPreferences},
  gppFramePreferencesInstrumentation in 'gppFramePreferencesInstrumentation.pas' {frmPreferencesInstrumentation: TFrame},
  gppFramePreferencesConditionalDefines in 'gppFramePreferencesConditionalDefines.pas' {frmPreferencesConditionalDefines: TFrame},
  gppFramePreferencesExcludedUnits in 'gppFramePreferencesExcludedUnits.pas' {frmPreferencesExcludedUnits: TFrame},
  gppFramePreferencesAnalysis in 'gppFramePreferencesAnalysis.pas' {frmPreferencesAnalysis: TFrame},
  GpDelphiInfo in '3rdParty\Gp\GpDelphiInfo.pas',
  DSiWin32 in '3rdParty\Gp\DSiWin32.pas',
  HVStringData in '3rdParty\HV\HVStringData.pas',
  HVStringBuilder in '3rdParty\HV\HVStringBuilder.pas';

{$R *.res}
{$R 'baggage.res' 'baggage.rc'}

begin
  Application.Initialize;
  Application.Title := 'GpProfile';
  Application.CreateForm(TfrmGpProfile, frmGpProfile);
  Application.CreateForm(TfrmPreferences, frmPreferences);
  Application.Run;
end.

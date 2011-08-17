program DelphiSettingManager;

{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\system.drawing.dll'}

uses
  Forms,
  fMain in 'Source\fMain.pas' {frmMain},
  ShellUtilities in 'Source\ShellUtilities.pas',
  SettingCollection in 'Source\SettingCollection.pas',
  frmSetting in 'Source\frmSetting.pas' {frmSettingList: TFrame},
  fNewSetting in 'Source\fNewSetting.pas' {frmNewSetting},
  fRenameSetting in 'Source\fRenameSetting.pas' {frmRenameSetting},
  LoadSaveCustomSetting in 'Source\LoadSaveCustomSetting.pas',
  fAbout in 'Source\fAbout.pas' {frmAbout},
  DelphiSettingRegistry in 'Source\DelphiSettingRegistry.pas',
  TreeViewController in 'Source\TreeViewController.pas',
  fEditSetting in 'Source\fEditSetting.pas' {frmEditSetting},
  ValueNamesProvider in 'Source\ValueNamesProvider.pas',
  IntfObserver in 'Design Patterns\IntfObserver.pas',
  Subject in 'Design Patterns\Subject.pas',
  SettingPersistent in 'Source\SettingPersistent.pas',
  SettingTemplate in 'Source\SettingTemplate.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Delphi / C#Builder Setting Manager (beta)';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

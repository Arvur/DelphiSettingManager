DelphiSettingManager
====================

What is Delphi Setting Manager?

Delphi / C# Builder Setting Manager (or, Delphi Setting Manager) is a utility to manage custom settings for Galileo IDEs (Delphi 6, Delphi 7, C#Builder, Delphi 8, and Delphi 2005). It reads data from the HKEY_CURRENT_USER\Software\Borland\BDS\X.X for BDS IDEs, or it reads data from HKEY_CURRENT_USER\Software\Delphi\X.X.

This utility only writes keys and values under HKEY_CURRENT_USER\Software\Borland\CustomSettings. The expected behavior of this key, you will not be able to alter your default Delphi setting, and corrupt your default Delphi setting by accident. If this utility corrupts or alter the Delphi default setting, that is a bug. Please let me know if you found a bug.

The settings for each IDE is stored under HKEY_CURRENT_USER\Software\Borland\CustomSettings\<IDE Identifier>\<Setting Name>, the File - Save Setting to File will create a .reg file of the selected custom setting. The .reg file can be imported into different PC using regedit. The .reg file does not contain the license key, and authorization code.

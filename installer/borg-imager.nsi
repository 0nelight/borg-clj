Unicode true
ManifestDPIAware true

!ifndef VERSION
  !define VERSION "0.0.0"
!endif
!ifndef PAYLOAD
  !error "PAYLOAD ist nicht gesetzt (Verzeichnis mit dem Inhalt fuer INSTDIR)"
!endif
!ifndef OUTFILE
  !define OUTFILE "borg-imager-setup-${VERSION}.exe"
!endif

!define PRODUCT       "borg-imager"
!define PRODUCT_NAME  "borg-imager"
!define PUBLISHER     "Grossegger"
!define EXE           "borgi.exe"
!define ENVKEY        "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"
!define UNINST_KEY    "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT}"

Name "${PRODUCT_NAME} ${VERSION}"
OutFile "${OUTFILE}"
RequestExecutionLevel admin
InstallDir "$PROGRAMFILES64\${PRODUCT}"
InstallDirRegKey HKLM "Software\${PRODUCT}" "InstallLocation"
SetCompressor /SOLID lzma
ShowInstDetails show
ShowUninstDetails show

VIProductVersion "${VERSION}.0"
VIAddVersionKey "ProductName" "${PRODUCT_NAME}"
VIAddVersionKey "FileVersion" "${VERSION}"
VIAddVersionKey "ProductVersion" "${VERSION}"
VIAddVersionKey "CompanyName" "${PUBLISHER}"
VIAddVersionKey "LegalCopyright" "${PUBLISHER}"
VIAddVersionKey "FileDescription" "${PRODUCT_NAME} Setup"

!include "MUI2.nsh"
!include "LogicLib.nsh"
!include "FileFunc.nsh"
!include "WinMessages.nsh"
!include "StrFunc.nsh"
${StrStr}
${UnStrRep}

!define MUI_ICON "..\resources\icon.ico"
!define MUI_UNICON "..\resources\icon.ico"
!define MUI_ABORTWARNING
!define MUI_FINISHPAGE_NOAUTOCLOSE
!define MUI_FINISHPAGE_TEXT "${PRODUCT_NAME} ${VERSION} ist installiert.$\r$\n$\r$\nDer Ordner steht im PATH. Das Abbilden von Laufwerken braucht Administratorrechte:$\r$\noeffne eine NEUE Eingabeaufforderung ALS ADMINISTRATOR und tippe$\r$\n$\r$\n    borgi ui$\r$\n$\r$\nfuer die Oberflaeche im Browser, oder$\r$\n$\r$\n    borgi disks$\r$\n    borgi image ssh://user@server/pfad/repo::heute C: --init"

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_LANGUAGE "German"
!insertmacro MUI_LANGUAGE "English"

Function .onInit
  nsExec::ExecToStack 'cmd /c tasklist /NH /FI "IMAGENAME eq ${EXE}" 2>nul | findstr /I "${EXE}"'
  Pop $0
  ${If} $0 == 0
    MessageBox MB_OKCANCEL|MB_ICONEXCLAMATION "${EXE} laeuft noch. Laufende Backups werden abgebrochen, wenn du fortsetzt." IDOK +2
      Abort
    nsExec::Exec 'taskkill /F /T /IM "${EXE}"'
    Pop $0
    Sleep 1000
  ${EndIf}
FunctionEnd

!macro AddToSystemPath dir
  ReadRegStr $0 HKLM "${ENVKEY}" "Path"
  ${If} $0 == ""
    WriteRegExpandStr HKLM "${ENVKEY}" "Path" "${dir}"
  ${Else}
    ${StrStr} $2 "$0" "${dir}"
    ${If} $2 == ""
      StrCpy $3 "$0" 1 -1
      ${If} $3 == ";"
        WriteRegExpandStr HKLM "${ENVKEY}" "Path" "$0${dir}"
      ${Else}
        WriteRegExpandStr HKLM "${ENVKEY}" "Path" "$0;${dir}"
      ${EndIf}
    ${EndIf}
  ${EndIf}
  SendMessage ${HWND_BROADCAST} ${WM_SETTINGCHANGE} 0 "STR:Environment" /TIMEOUT=2000
!macroend

Section "borg-imager" SecMain
  SetOutPath "$INSTDIR"
  SetOverwrite on
  File /r "${PAYLOAD}\*.*"

  WriteRegStr HKLM "Software\${PRODUCT}" "InstallLocation" "$INSTDIR"
  WriteRegStr HKLM "Software\${PRODUCT}" "Version" "${VERSION}"

  WriteUninstaller "$INSTDIR\Uninstall.exe"
  ${GetSize} "$INSTDIR" "/S=0K" $0 $1 $2
  WriteRegStr   HKLM "${UNINST_KEY}" "DisplayName"     "${PRODUCT_NAME}"
  WriteRegStr   HKLM "${UNINST_KEY}" "DisplayVersion"  "${VERSION}"
  WriteRegStr   HKLM "${UNINST_KEY}" "DisplayIcon"     "$INSTDIR\${EXE}"
  WriteRegStr   HKLM "${UNINST_KEY}" "Publisher"       "${PUBLISHER}"
  WriteRegStr   HKLM "${UNINST_KEY}" "InstallLocation" "$INSTDIR"
  WriteRegStr   HKLM "${UNINST_KEY}" "UninstallString" '"$INSTDIR\Uninstall.exe"'
  WriteRegStr   HKLM "${UNINST_KEY}" "QuietUninstallString" '"$INSTDIR\Uninstall.exe" /S'
  WriteRegDWORD HKLM "${UNINST_KEY}" "EstimatedSize"   "$0"
  WriteRegDWORD HKLM "${UNINST_KEY}" "NoModify"        1
  WriteRegDWORD HKLM "${UNINST_KEY}" "NoRepair"        1

  !insertmacro AddToSystemPath "$INSTDIR"
SectionEnd

Section "Uninstall"
  ReadRegStr $0 HKLM "${ENVKEY}" "Path"
  ${UnStrRep} $1 "$0" ";$INSTDIR" ""
  ${UnStrRep} $1 "$1" "$INSTDIR;" ""
  ${UnStrRep} $1 "$1" "$INSTDIR" ""
  WriteRegExpandStr HKLM "${ENVKEY}" "Path" "$1"
  SendMessage ${HWND_BROADCAST} ${WM_SETTINGCHANGE} 0 "STR:Environment" /TIMEOUT=2000

  DeleteRegKey HKLM "${UNINST_KEY}"
  DeleteRegKey HKLM "Software\${PRODUCT}"
  RMDir /r "$INSTDIR"
  RMDir /r "$LOCALAPPDATA\borg-imager\cache"
SectionEnd

(uiop:define-package :lem-powershell-mode/lsp-config)
(:use :cl)
(:export))
(in-package :lem-powershell-mode/lsp-config)

(lem-lsp-mode:define-language-spec (nix-spec lem-nix-mode:nix-mode)
  :language-id "Powershell"
  :root-uri-patterns '("PSScriptAnalyzerSettings.psd1" ".git")
  :command '("powershell-editor-services" "-SessionDetailsPath" "./session.json")
  :install-command ""
  :readme-url "https://github.com/PowerShell/PowerShellEditorServices"
  :connection-mode :stdio)

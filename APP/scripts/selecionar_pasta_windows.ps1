param(
    [string]$PastaInicial = ""
)

Add-Type -AssemblyName System.Windows.Forms

$dialogo = New-Object System.Windows.Forms.FolderBrowserDialog
$dialogo.Description = "Selecione a pasta do CE-QUAL-W2"
$dialogo.ShowNewFolderButton = $false

if ($PastaInicial -and (Test-Path -LiteralPath $PastaInicial -PathType Container)) {
    $dialogo.SelectedPath = $PastaInicial
}

$resultado = $dialogo.ShowDialog()

if ($resultado -eq [System.Windows.Forms.DialogResult]::OK) {
    [Console]::OutputEncoding = [System.Text.Encoding]::UTF8
    Write-Output $dialogo.SelectedPath
    exit 0
}

exit 1

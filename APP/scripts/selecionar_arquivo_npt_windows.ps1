param(
    [string]$PastaInicial = ""
)

Add-Type -AssemblyName System.Windows.Forms

$dialogo = New-Object System.Windows.Forms.OpenFileDialog
$dialogo.Title = "Selecione o arquivo de controle do CE-QUAL-W2"
$dialogo.Filter = "Arquivo de controle (*.npt)|*.npt|Todos os arquivos (*.*)|*.*"
$dialogo.CheckFileExists = $true
$dialogo.Multiselect = $false

if ($PastaInicial -and (Test-Path -LiteralPath $PastaInicial -PathType Container)) {
    $dialogo.InitialDirectory = $PastaInicial
}

$resultado = $dialogo.ShowDialog()

if ($resultado -eq [System.Windows.Forms.DialogResult]::OK) {
    [Console]::OutputEncoding = [System.Text.Encoding]::UTF8
    Write-Output $dialogo.FileName
    exit 0
}

exit 1

param(
    [Parameter(Mandatory = $true)][string]$Executavel,
    [Parameter(Mandatory = $true)][string]$Diretorio,
    [int]$LimiteInatividadeSegundos = 60,
    [int]$TempoMaximoSegundos = 21600
)

$ErrorActionPreference = "Stop"

function Get-OutputSnapshot {
    $arquivos = Get-ChildItem -LiteralPath $Diretorio -File -ErrorAction SilentlyContinue |
        Where-Object { $_.Extension -match '^\.(OPT|opt|prn|PRN|log|LOG)$' } |
        Sort-Object FullName
    ($arquivos | ForEach-Object {
        "$($_.FullName)|$($_.Length)|$($_.LastWriteTimeUtc.Ticks)"
    }) -join ";"
}

function Get-WindowText {
    param([System.Diagnostics.Process]$Processo)
    try {
        Add-Type -AssemblyName UIAutomationClient -ErrorAction SilentlyContinue
        Add-Type -AssemblyName UIAutomationTypes -ErrorAction SilentlyContinue
        $Processo.Refresh()
        if ($Processo.MainWindowHandle -eq 0) {
            return ""
        }
        $raiz = [System.Windows.Automation.AutomationElement]::FromHandle(
            $Processo.MainWindowHandle
        )
        $elementos = $raiz.FindAll(
            [System.Windows.Automation.TreeScope]::Descendants,
            [System.Windows.Automation.Condition]::TrueCondition
        )
        $textos = foreach ($elemento in $elementos) {
            $nome = $elemento.Current.Name
            if (![string]::IsNullOrWhiteSpace($nome)) { $nome }
        }
        ($textos -join " ")
    } catch {
        ""
    }
}

if (!(Test-Path -LiteralPath $Executavel -PathType Leaf)) {
    Write-Output "EXECUTAVEL_NAO_ENCONTRADO: $Executavel"
    exit 10
}
if (!(Test-Path -LiteralPath $Diretorio -PathType Container)) {
    Write-Output "DIRETORIO_NAO_ENCONTRADO: $Diretorio"
    exit 11
}

$inicio = Get-Date
$ultimaAtividade = $inicio
$snapshotAnterior = Get-OutputSnapshot
$arquivoErro = Join-Path $Diretorio "W2Errordump.opt"
$erroInicial = if (Test-Path -LiteralPath $arquivoErro -PathType Leaf) {
    $infoErroInicial = Get-Item -LiteralPath $arquivoErro
    "$($infoErroInicial.Length)|$($infoErroInicial.LastWriteTimeUtc.Ticks)"
} else {
    ""
}

$info = New-Object System.Diagnostics.ProcessStartInfo
$info.FileName = $Executavel
$info.WorkingDirectory = $Diretorio
$info.UseShellExecute = $true

$processo = New-Object System.Diagnostics.Process
$processo.StartInfo = $info
[void]$processo.Start()
Write-Output "PROCESSO_INICIADO PID=$($processo.Id)"

$motivo = "PROCESSO_ENCERRADO"
$codigo = 0

while (!$processo.HasExited) {
    Start-Sleep -Seconds 1
    $processo.Refresh()
    if ($processo.HasExited) { break }

    $agora = Get-Date
    $snapshotAtual = Get-OutputSnapshot

    # A interface gráfica continua consumindo um pouco de CPU mesmo quando
    # está parada em uma tela de erro. Por isso, somente alterações reais nos
    # arquivos de saída contam como progresso do modelo.
    if ($snapshotAtual -ne $snapshotAnterior) {
        $ultimaAtividade = $agora
        $snapshotAnterior = $snapshotAtual
    }

    if (Test-Path -LiteralPath $arquivoErro -PathType Leaf) {
        $infoErro = Get-Item -LiteralPath $arquivoErro
        $erroAtual = "$($infoErro.Length)|$($infoErro.LastWriteTimeUtc.Ticks)"
        if ($erroAtual -ne $erroInicial) {
            $motivo = "W2_ERRORDUMP_DETECTADO"
            $codigo = 2
            break
        }
    }

    $textoJanela = Get-WindowText -Processo $processo
    if (
        $textoJanela -match
        '(?i)(simulation|run|model)\s+(is\s+)?paused|paused\s+at|pause\s+requested'
    ) {
        # Uma pausa solicitada pelo usuário não é travamento.
        $ultimaAtividade = $agora
        continue
    }
    if ($textoJanela -match '(?i)normal termination|simulation complete|run completed') {
        $motivo = "TERMINO_NORMAL_DETECTADO"
        $codigo = 0
        break
    }
    if (($agora - $ultimaAtividade).TotalSeconds -ge $LimiteInatividadeSegundos) {
        $motivo = "SAIDAS_SEM_ATUALIZACAO"
        $codigo = 3
        break
    }
    if (($agora - $inicio).TotalSeconds -ge $TempoMaximoSegundos) {
        $motivo = "TEMPO_MAXIMO_EXCEDIDO"
        $codigo = 4
        break
    }
}

if (
    $processo.HasExited -and
    $motivo -eq "PROCESSO_ENCERRADO" -and
    $processo.ExitCode -ne 0
) {
    $motivo = "PROCESSO_ENCERROU_COM_ERRO_$($processo.ExitCode)"
    $codigo = 2
}

if (!$processo.HasExited) {
    [void]$processo.CloseMainWindow()
    if (!$processo.WaitForExit(5000)) {
        $processo.Kill()
        $processo.WaitForExit()
    }
}

Write-Output "$motivo DURACAO_SEGUNDOS=$([math]::Round(((Get-Date) - $inicio).TotalSeconds, 1))"
exit $codigo

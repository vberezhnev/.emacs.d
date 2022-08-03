import * as lsp from 'vscode-languageserver/node';
export interface ProgressReporter {
    begin(message?: string): void;
    report(message: string): void;
    end(): void;
}
export interface LspClient {
    setClientCapabilites(capabilites: lsp.ClientCapabilities): void;
    createProgressReporter(): ProgressReporter;
    publishDiagnostics(args: lsp.PublishDiagnosticsParams): void;
    showMessage(args: lsp.ShowMessageParams): void;
    logMessage(args: lsp.LogMessageParams): void;
    applyWorkspaceEdit(args: lsp.ApplyWorkspaceEditParams): Promise<lsp.ApplyWorkspaceEditResponse>;
    telemetry(args: any): void;
    rename(args: lsp.TextDocumentPositionParams): Promise<any>;
}
export declare class LspClientImpl implements LspClient {
    protected connection: lsp.Connection;
    private clientCapabilities?;
    constructor(connection: lsp.Connection);
    setClientCapabilites(capabilites: lsp.ClientCapabilities): void;
    createProgressReporter(): ProgressReporter;
    publishDiagnostics(args: lsp.PublishDiagnosticsParams): void;
    showMessage(args: lsp.ShowMessageParams): void;
    logMessage(args: lsp.LogMessageParams): void;
    telemetry(args: any): void;
    applyWorkspaceEdit(args: lsp.ApplyWorkspaceEditParams): Promise<lsp.ApplyWorkspaceEditResponse>;
    rename(args: lsp.TextDocumentPositionParams): Promise<any>;
}
//# sourceMappingURL=lsp-client.d.ts.map
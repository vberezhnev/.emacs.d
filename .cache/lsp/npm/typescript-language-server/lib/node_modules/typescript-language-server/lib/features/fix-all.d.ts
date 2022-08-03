import * as lsp from 'vscode-languageserver/node';
import { LspDocuments } from '../document';
import { TspClient } from '../tsp-client';
import { CodeActionKind } from '../utils/types';
export declare class TypeScriptAutoFixProvider {
    private readonly client;
    private static kindProviders;
    static get kinds(): CodeActionKind[];
    constructor(client: TspClient);
    provideCodeActions(kinds: CodeActionKind[], file: string, diagnostics: lsp.Diagnostic[], documents: LspDocuments): Promise<lsp.CodeAction[]>;
}
//# sourceMappingURL=fix-all.d.ts.map
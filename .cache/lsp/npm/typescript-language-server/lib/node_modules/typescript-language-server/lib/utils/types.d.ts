export declare class CodeActionKind {
    readonly value: string;
    private static readonly sep;
    static readonly Empty: CodeActionKind;
    static readonly QuickFix: CodeActionKind;
    static readonly Refactor: CodeActionKind;
    static readonly Source: CodeActionKind;
    static readonly SourceAddMissingImportsTs: CodeActionKind;
    static readonly SourceRemoveUnusedTs: CodeActionKind;
    static readonly SourceOrganizeImports: CodeActionKind;
    static readonly SourceOrganizeImportsTs: CodeActionKind;
    static readonly SourceFixAll: CodeActionKind;
    static readonly SourceFixAllTs: CodeActionKind;
    constructor(value: string);
    equals(other: CodeActionKind): boolean;
    /**
     * Checks if `other` is a sub-kind of this `CodeActionKind`.
     *
     * The kind `"refactor.extract"` for example contains `"refactor.extract"` and ``"refactor.extract.function"`,
     * but not `"unicorn.refactor.extract"`, or `"refactor.extractAll"` or `refactor`.
     *
     * @param other Kind to check.
     */
    contains(other: CodeActionKind): boolean;
    /**
     * Checks if this code action kind intersects `other`.
     *
     * The kind `"refactor.extract"` for example intersects `refactor`, `"refactor.extract"` and ``"refactor.extract.function"`,
     * but not `"unicorn.refactor.extract"`, or `"refactor.extractAll"`.
     *
     * @param other Kind to check.
     */
    intersects(other: CodeActionKind): boolean;
    /**
     * Create a new kind by appending a more specific selector to the current kind.
     *
     * Does not modify the current kind.
     */
    append(part: string): CodeActionKind;
}
//# sourceMappingURL=types.d.ts.map
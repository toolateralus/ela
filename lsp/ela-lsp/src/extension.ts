import * as vscode from 'vscode';
import * as ela from "./ela"

let lsp: ela.ElaLSP;

export function activate(context: vscode.ExtensionContext) {
  const binaryPath = '/home/josh/source/c++/ela/lsp/build/bin/ela_lsp';
  console.log("[ELA LSP] Activating extension...");
  console.log("[ELA LSP] Spawning LSP process at:", binaryPath);
  
  lsp = new ela.ElaLSP(binaryPath);

  console.log("[ELA LSP] Extension activated");

  vscode.workspace.onDidOpenTextDocument(doc => {
    console.log("[ELA LSP] Document opened:", doc.uri.fsPath, "languageId:", doc.languageId);
    if (doc.languageId === 'ela') {
      console.log("[ELA LSP] Sending didOpen to LSP");
      lsp.didOpen(doc.uri.fsPath);
    }
  });

  vscode.workspace.onDidChangeTextDocument(event => {
    const doc = event.document;
    console.log("[ELA LSP] Document changed:", doc.uri.fsPath);
    if (doc.languageId === 'ela') {
      console.log("[ELA LSP] Sending didChange to LSP");
      lsp.didChange(doc.uri.fsPath);
    }
  });

  context.subscriptions.push(
    vscode.languages.registerHoverProvider({ scheme: 'file', language: 'ela' }, {
      async provideHover(document, position) {
        console.log("[ELA LSP] Hover requested at", position.line, position.character);
        const contents = await lsp.hover(document.uri.fsPath, position.line, position.character);
        console.log("[ELA LSP] Hover contents:", contents);
        return new vscode.Hover(contents);
      }
    })
  );

  context.subscriptions.push(
    vscode.languages.registerDefinitionProvider({ scheme: 'file', language: 'ela' }, {
      async provideDefinition(document, position) {
        console.log("[ELA LSP] Definition requested at", position.line, position.character);
        const def = await lsp.definition(document.uri.fsPath, position.line, position.character);
        if (!def) {
          console.log("[ELA LSP] Definition not found");
          return null;
        }
        const range = new vscode.Range(
          def.range.start.line,
          def.range.start.character,
          def.range.end.line,
          def.range.end.character
        );
        console.log("[ELA LSP] Definition found at", def.uri, range);
        return new vscode.Location(vscode.Uri.file(def.uri), range);
      }
    })
  );
}

export function deactivate() {
  console.log("[ELA LSP] Deactivating extension");
  if (lsp) {
    lsp.kill();
  }
}

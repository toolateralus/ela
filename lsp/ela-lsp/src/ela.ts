// src/extension.ts
import * as vscode from 'vscode';
import { spawn, ChildProcess } from 'child_process';
import * as readline from 'readline';

export interface HoverResult {
  contents: string;
}

export interface DefinitionResult {
  uri: string;
  range: {
    start: { line: number; character: number };
    end: { line: number; character: number };
  };
}

export interface Diagnostic {
  range: {
    start: { line: number; character: number };
    end: { line: number; character: number };
  };
  message: string;
  severity: number;
}

export class ElaLSP {
  private proc: ChildProcess | undefined;
  private rl: readline.Interface | undefined;
  private pendingResponses: Map<number, (result: any) => void> = new Map();
  private nextId: number = 1;

  constructor(private binaryPath: string) {
    console.info(`[ElaLSP] Constructing LSP client for binary: ${binaryPath}`);
    this.spawnProcess();
  }

  public kill() {
    console.info('[ElaLSP] killing language server process...');

    if (this.rl) {
      try {
        this.rl.close();
      } catch (err) {
        console.error('[ElaLSP] error closing readline:', err);
      }
      this.rl = undefined;
    }

    if (this.proc) {
      try {
        if (this.proc.stdin && typeof (this.proc.stdin as any).end === 'function') {
          (this.proc.stdin as NodeJS.WritableStream).end();
        }
      } catch (err) {
        console.error('[ElaLSP] error ending stdin:', err);
      }

      try {
        this.proc.kill();
        console.info('[ElaLSP] process kill signal sent');
      } catch (err) {
        console.error('[ElaLSP] error killing process:', err);
      }

      this.proc = undefined;
    }

    for (const [id, resolve] of this.pendingResponses) {
      try {
        resolve({ error: 'language server killed' });
      } catch (err) {
        console.error(`[ElaLSP] error resolving pending response id=${id}:`, err);
      }
    }
    this.pendingResponses.clear();
  }

  private spawnProcess() {
    console.info('[ElaLSP] Spawning language server process...');
    try {
      this.proc = spawn(this.binaryPath, [], { stdio: ['pipe', 'pipe', 'inherit'] });
      console.info(`[ElaLSP] Spawned process pid=${this.proc.pid}`);

      this.rl = readline.createInterface({ input: this.proc!.stdout as NodeJS.ReadableStream });
      this.rl.on('line', (line) => {
        console.debug(`[ElaLSP] stdout line: ${line}`);
        try {
          const msg = JSON.parse(line);
          console.debug('[ElaLSP] parsed message:', msg);
          if ('id' in msg && this.pendingResponses.has(msg.id)) {
            console.debug(`[ElaLSP] resolving pending response id=${msg.id}`);
            this.pendingResponses.get(msg.id)!(msg.result);
            this.pendingResponses.delete(msg.id);
          } else if (msg.method === 'window/showMessage') {
            console.info('[ElaLSP] showMessage from server:', msg.params && msg.params.message);
            vscode.window.showErrorMessage(msg.params.message);
          } else if (msg.method === 'textDocument/publishDiagnostics') {
            console.info('[ElaLSP] publishDiagnostics received for', msg.params && msg.params.uri);
            this.handleDiagnostics(msg.params);
          } else {
            console.debug('[ElaLSP] unhandled message:', msg);
          }
        } catch (err) {
          console.error('[ElaLSP] failed to parse line as JSON', err);
        }
      });

      this.proc.on('exit', (code, signal) => {
        console.error(`[ElaLSP] language server exited. code=${code}, signal=${signal}`);
      });

      this.proc.on('error', (err) => {
        console.error('[ElaLSP] process error:', err);
      });
    } catch (err) {
      console.error('[ElaLSP] failed to spawn process:', err);
    }
  }

  private sendRequest(method: string, params: any): Promise<any> {
    const id = this.nextId++;
    const request = { jsonrpc: "2.0", id, method, params };
    console.debug('[ElaLSP] sendRequest:', { id, method, params });
    return new Promise((resolve, reject) => {
      this.pendingResponses.set(id, (result: any) => {
        console.debug(`[ElaLSP] response for id=${id}:`, result);
        resolve(result);
      });
      try {
        if (!this.proc || !this.proc.stdin) {
          throw new Error('Process stdin not available');
        }
        const payload = JSON.stringify(request) + '\n';
        const ok = this.proc.stdin.write(payload);
        console.debug(`[ElaLSP] wrote request id=${id} to stdin (ok=${ok})`);
      } catch (err) {
        console.error(`[ElaLSP] failed to write request id=${id}:`, err);
        this.pendingResponses.delete(id);
        reject(err);
      }
    });
  }

  async didOpen(file: string) {
    console.info('[ElaLSP] didOpen', file);
    try {
      return await this.sendRequest('textDocument/didOpen', { textDocument: { uri: file } });
    } catch (err) {
      console.error('[ElaLSP] didOpen failed:', err);
      throw err;
    }
  }

  async didChange(file: string) {
    console.info('[ElaLSP] didChange', file);
    try {
      return await this.sendRequest('textDocument/didChange', { textDocument: { uri: file } });
    } catch (err) {
      console.error('[ElaLSP] didChange failed:', err);
      throw err;
    }
  }

  async hover(file: string, line: number, character: number): Promise<string> {
    console.info('[ElaLSP] hover', { file, line, character });
    try {
      const res = await this.sendRequest('textDocument/hover', { textDocument: { uri: file }, position: { line, character } });
      console.debug('[ElaLSP] hover result:', res);
      return res.contents || '';
    } catch (err) {
      console.error('[ElaLSP] hover failed:', err);
      return '';
    }
  }

  async definition(file: string, line: number, character: number): Promise<DefinitionResult | null> {
    console.info('[ElaLSP] definition', { file, line, character });
    try {
      const res = await this.sendRequest('textDocument/definition', { textDocument: { uri: file }, position: { line, character } });
      console.debug('[ElaLSP] definition result:', res);
      return res || null;
    } catch (err) {
      console.error('[ElaLSP] definition failed:', err);
      return null;
    }
  }

  private handleDiagnostics(params: { uri: string; diagnostics: Diagnostic[] }) {
    try {
      console.debug('[ElaLSP] handleDiagnostics params:', params);
      const fileUri = vscode.Uri.file(params.uri);
      const diag = params.diagnostics.map(d => new vscode.Diagnostic(
        new vscode.Range(
          new vscode.Position(d.range.start.line, d.range.start.character),
          new vscode.Position(d.range.end.line, d.range.end.character)
        ),
        d.message,
        d.severity
      ));
      const collection = vscode.languages.createDiagnosticCollection('ela');
      collection.set(fileUri, diag);
      console.info(`[ElaLSP] set ${diag.length} diagnostics for ${params.uri}`);
    } catch (err) {
      console.error('[ElaLSP] error handling diagnostics:', err);
    }
  }
}

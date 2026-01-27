/**
 * ProcessManager - Manages Claude CLI process lifecycle
 *
 * Responsibilities:
 * - Spawn and configure CLI process
 * - Manage stdin/stdout/stderr streams
 * - Handle process lifecycle events
 * - Send/receive JSON messages
 */

import { spawn, ChildProcess } from 'child_process'
import { createInterface, Interface } from 'readline'
import { EventEmitter } from 'events'

export interface ProcessConfig {
  model?: string
  cwd?: string
  permissionMode?: 'default' | 'acceptEdits' | 'plan' | 'bypassPermissions'
  verbose?: boolean
  includePartialMessages?: boolean
}

export interface ProcessManagerEvents {
  line: (line: string) => void
  stderr: (data: Buffer) => void
  exit: (code: number | null) => void
  error: (error: Error) => void
}

export class ProcessManager extends EventEmitter {
  private process?: ChildProcess
  private readline?: Interface
  private isStarted = false
  private isStopping = false

  /**
   * Start the Claude CLI process
   */
  start(config: ProcessConfig): void {
    if (this.isStarted) {
      throw new Error('Process already started')
    }

    const args = this.buildArgs(config)

    this.process = spawn('claude', args, {
      cwd: config.cwd || process.cwd(),
      stdio: ['pipe', 'pipe', 'pipe']
    })

    this.setupStreams()
    this.setupProcessHandlers()

    this.isStarted = true
  }

  /**
   * Build CLI arguments from config
   */
  private buildArgs(config: ProcessConfig): string[] {
    const args = [
      '--print',
      '--input-format', 'stream-json',
      '--output-format', 'stream-json',
      '--verbose',  // Required for stream-json
      '--model', config.model || 'haiku'
    ]

    if (config.permissionMode) {
      args.push('--permission-mode', config.permissionMode)
    }

    if (config.includePartialMessages) {
      args.push('--include-partial-messages')
    }

    return args
  }

  /**
   * Setup stdout/stderr streams
   */
  private setupStreams(): void {
    if (!this.process) return

    this.readline = createInterface({
      input: this.process.stdout!,
      crlfDelay: Infinity
    })

    this.readline.on('line', (line) => {
      this.emit('line', line)
    })

    this.process.stderr!.on('data', (data) => {
      this.emit('stderr', data)
    })
  }

  /**
   * Setup process event handlers
   */
  private setupProcessHandlers(): void {
    if (!this.process) return

    this.process.on('exit', (code) => {
      this.emit('exit', code)
    })

    this.process.on('error', (err) => {
      this.emit('error', err)
    })
  }

  /**
   * Write JSON message to stdin
   */
  writeMessage(message: unknown): void {
    if (!this.process || !this.process.stdin) {
      throw new Error('Process not started or stdin not available')
    }

    const json = JSON.stringify(message)
    this.process.stdin.write(json + '\n')
  }

  /**
   * Stop the process gracefully
   */
  stop(): void {
    if (!this.process) {
      return
    }

    this.isStopping = true

    // Close readline first
    if (this.readline) {
      this.readline.close()
      this.readline = undefined
    }

    // Close stdin to signal graceful shutdown
    if (this.process.stdin) {
      this.process.stdin.end()
    }

    // Graceful shutdown sequence: stdin close → SIGTERM → SIGKILL
    // Give process 500ms to exit from stdin close
    const sigtermTimeout = setTimeout(() => {
      if (this.process && !this.process.killed) {
        // Send SIGTERM for graceful shutdown
        this.process.kill('SIGTERM')

        // If SIGTERM doesn't work, force kill after another 500ms
        const sigkillTimeout = setTimeout(() => {
          if (this.process && !this.process.killed) {
            this.process.kill('SIGKILL')
          }
        }, 500)

        // Clean up SIGKILL timeout if process exits
        this.process.once('exit', () => {
          clearTimeout(sigkillTimeout)
        })
      }
    }, 500)

    // Clean up timeouts if process exits naturally
    this.process.once('exit', () => {
      clearTimeout(sigtermTimeout)
      this.process = undefined
      this.isStarted = false
    })
  }

  /**
   * Check if process is running
   */
  isRunning(): boolean {
    return this.isStarted && !this.isStopping
  }

  /**
   * Get the underlying process (for advanced usage)
   */
  getProcess(): ChildProcess | undefined {
    return this.process
  }
}

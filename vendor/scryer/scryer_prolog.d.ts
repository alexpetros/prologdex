/* tslint:disable */
/* eslint-disable */
/**
 * The Scryer Prolog `Machine`.
 */
export class Machine {
  private constructor();
  free(): void;
  /**
   * Consults a module.
   */
  consultModuleString(module: string, program: string): void;
  /**
   * Runs a query.
   *
   * You can only have one query at a time. If you try to do anything with this machine while
   * doing a query an error will be thrown.
   */
  runQuery(query: string): any;
}
/**
 * A builder for a `Machine`.
 */
export class MachineBuilder {
  free(): void;
  /**
   * Creates a new `MachineBuilder` with the default configuration.
   */
  constructor();
  /**
   * Creates a new `Machine`.
   */
  build(): Machine;
}
/**
 * The state of a running query.
 */
export class QueryState {
  private constructor();
  free(): void;
  /**
   * Drops the query.
   *
   * This is useful to end a query early. Like finishing a query, control will be given back
   * to the `Machine` and any call to `next` after that will result in an error.
   */
  drop(): void;
  /**
   * Gets the next leaf answer.
   *
   * This follows the Javascript iterator protocol, so it returns an object that
   * contains a `done` field and a `value` field. If `done` is `false`, then the query ended
   * and control of the `Machine` will be given back to the `Machine` that created this query.
   * Any call after that will result in an error.
   */
  next(): any;
}

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly __wbg_machine_free: (a: number, b: number) => void;
  readonly __wbg_machinebuilder_free: (a: number, b: number) => void;
  readonly __wbg_querystate_free: (a: number, b: number) => void;
  readonly machine_consultModuleString: (a: number, b: number, c: number, d: number, e: number) => [number, number];
  readonly machine_runQuery: (a: number, b: number, c: number) => [number, number, number];
  readonly machinebuilder_build: (a: number) => number;
  readonly machinebuilder_new: () => number;
  readonly querystate_drop: (a: number) => void;
  readonly querystate_next: (a: number) => [number, number, number];
  readonly ring_core_0_17_14__bn_mul_mont: (a: number, b: number, c: number, d: number, e: number, f: number) => void;
  readonly __wbindgen_exn_store: (a: number) => void;
  readonly __externref_table_alloc: () => number;
  readonly __wbindgen_export_2: WebAssembly.Table;
  readonly __wbindgen_malloc: (a: number, b: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
  readonly __externref_table_dealloc: (a: number) => void;
  readonly __wbindgen_start: () => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;
/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {{ module: SyncInitInput }} module - Passing `SyncInitInput` directly is deprecated.
*
* @returns {InitOutput}
*/
export function initSync(module: { module: SyncInitInput } | SyncInitInput): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {{ module_or_path: InitInput | Promise<InitInput> }} module_or_path - Passing `InitInput` directly is deprecated.
*
* @returns {Promise<InitOutput>}
*/
export default function __wbg_init (module_or_path?: { module_or_path: InitInput | Promise<InitInput> } | InitInput | Promise<InitInput>): Promise<InitOutput>;

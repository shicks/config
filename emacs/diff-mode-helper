#!/usr/bin/env -S npx --yes zx

// Helper to smooth over differences between git and mercurial.
// TODO(sdh): not bothering w/ copy/rename detection for now.

// Perform P4DIFF helper functionality.
$.verbose = false;
const self = process.argv[2];
if ((process.env['P4DIFF'] || '').startsWith(self)) {
  const args = [...argv._];
  const outputFile = args.shift();
  const diffs = [];
  // Handle non-multidiff case.
  if (args.length === 2) args.unshift('');
  for (let i = 0; i < args.length; i += 3) {
    let [a, b] = args.slice(i + 1, i + 3);
    a = a.replace(/'/g, String.raw`'\'`);
    b = b.replace(/'/g, String.raw`'\'`);
    diffs.push($`diff -U0 ${a} ${b}`);
  }
  const output = await Promise.all(diffs).join('\n');
  await fs.writeFile(outputFile, `${output}\n`);
  process.exit(0);
}

async function chain(...fns) {
  let err = '';
  for (const fn of fns) {
    try {
      return await fn();
    } catch (err) {
      err += `${err.stderr}\n`;
    }
  }
  throw new Error(`All jobs failed:\n${err}`);
}

/**
 * @param {string} cmd
 * @return {Promise<boolean>} Whether cmd exists
 */
async function which(cmd) {
  try {
    await $`which ${cmd}`;
    return true;
  } catch (err) {
    return false;
  }
}

// Base class
class Differ {
  /** @param {string} root */
  constructor(root) {
    this.root = root;
  }
  /**
   * @param {string=} branch
   * @return {Promise<string>}
   */
  async diff(branch) {}
  /** @return {Promise<Differ>} */
  static async create() {}
}

class HgDiffer extends Differ {
  static async create() {
    if (!await which('hg')) throw new Error(`hg does not exist`);
    return new HgDiffer(await $`hg root`);
  }
  async diff(branch = '.') {
    return (await $`hg diff --hidden -U0 -r ${branch}`).stdout;
    // TODO: remove ansi codes?
    //  * run with --color=never
    //  * postprocess with s/\e\[[0-9,]*m//g
  }
}

class GitDiffer extends Differ {
  static async create() {
    if (!await which('git')) throw new Error(`git does not exist`);
    return new GitDiffer(await $`git rev-parse --show-toplevel`);
  }
  async diff(branch = 'HEAD') {
    return (await $`git diff -U0 ${branch}`).stdout;
  }
}

class P4Differ extends Differ {
  static async create() {
    if (!await which('p4')) throw new Error(`p4 does not exist`);
    const info = await $`p4 info`;
    for (const line of info.split('\n')) {
      const match = /Client root: (.*)/.exec(line);
      if (match) return new P4Differ(match[1]);
    }
    throw new Error(`p4Root: Could not find client root: ${info}`);
  }
  async diff() { // branch is ignored
    // Initialize self as P4DIFF
    const tmp = await $`mktemp -t p4-diff`;
    $.env['P4_DIFF'] = `${self} ${tmp}`;
    await $`p4 diff`;
    return String(await fs.readFile(tmp));
  }
}


// TODO - need to communicate which one won?
const differ = await chain(HgDiffer.create, GitDiffer.create, P4Differ.create);
const diff = await differ.diff(argv._[0]);

let added = 0;
let removed = 0;
let file = '';
let mode = '';
function print() {
  if (!mode) return;
  console.log(`${mode} ${String(added).padStart(4)} ${String(removed).padStart(4)} ${file}`);
  mode = '';
}

for (const line of diff.split('\n')) {
  let match;
  if ((match = /^--- (?:(?:a|.*\/depot)\/([^\t]*)|\/dev\/null)(?:\t.*)?/.exec(line))) {
    print();
    added = removed = 0;
    file = match[1];
    mode = file ? 'change' : 'create';
  } else if ((match = /^\+\+\+ (?:(?:b|.*\/depot)\/([^\t]*)|\/dev\/null)(?:\t.*)?/.exec(line))) {
    file = file || match[1];
    if (!match[1]) mode = 'delete';
  } else if ((match = /^[+-]/.exec(line))) {
    if (match[0] === '+') added++ ;
    if (match[0] === '-') removed++ ;
  }
}
print();

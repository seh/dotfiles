---
name: close-bazel-drift
description: |
  Ensure Bazel-related configuration is consistent with
  Gazelle-managed details. Use in Bazel-built projects when Go source
  files have are created, deleted, or edited.
compatibility: opencode
---

# Files Involved

In projects involving source code for computer programs—in particular,
written in the Go programming language—that also use _Bazel_ to build
the software, there are several kinds of files that must converge on
consistent definitions:

- Go source files (with the ".go" file extension)
- Bazel _BUILD.bazel_ files, usually in the same directory as these Go
  source files
- Top-level _go.mod_ file
- Top-level _MODULE.bazel_ file

The _BUILD.bazel_ files refer to the Go source files by name, and also
reference Bazel targets as dependencies on which the targets for the
Go source files' libraries (via the `go_library` Bazel rule) and
binaries (via the `go_binary` Bazel rule) rely.

The _MODULE.bazel_ file may reference names of Bazel external
repositories generated on behalf of Go modules listed in the _go.mod_
file.

# Tools Involved

The _go mod tidy_ tool inspects the Go source files within the module
and ensures that there are corresponding `require` directives
(/direct/, not /indirect/) in the _go.mod_ file.

The _Gazelle_ tool—invoked as _bazelisk run -- //:gazelle_—ensures
that the _BUILD.bazel_ files mention the proper set of Go source files
in the containing Bazel package, and that those Go files' module
dependencies are mentioned in the Bazel targets that _Gazelle_ writes
into those _BUILD.bazel_ files.

Finally, the _bazelisk mod tidy_ command ensures that the
_MODULE.bazel_ file mentions the proper set of external Bazel
repositories cited as dependencies in the _BUILD.bazel_ files.

(With our current version of Bazel, the _bazel mod tidy_ and the
_buildifier_ commands differ in their preferences for where blank
lines within the _MODULE.bazel_ file should occur, so we run
_buildifier_ against that file afterward to satisfy the latter's
preference.)

# Ensuring Consistency

After detecting any of the following triggering conditions, invoke the
specified sequence of commands to rectify any drift in attendant
configuration:

- A new Go source file is now present
- A previously existing Go source file was deleted, and is now gone
- The `import` statements in a Go source file changed
- The _go.mod_ file changed, and not from having just run _go mod
  tidy_

Run the following sequence of commands, preferably **all within a
subagent**:

- _go mod tidy_
- _bazelisk run -- //:gazelle_
- _bazelisk mod tidy_
- _buildifier -- MODULE.bazel_

After doing so, note whether any of the _BUILD.bazel_, _go.mod_, or
_MODULE.bazel_ files changed as a result.

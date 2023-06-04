---
tags: testing, ci
---

# `haskell-ci` how-to: caching and using your program executable

In this article I show how to extend the `haskell-ci` GitHub Actions
workflow to pass the built executable to subsequent jobs.

## Background

The Haskell *Security Response Team* has recently bootstrapped the
[*haskell/security-advisories*] database.  This repository contains:

- The security advisories themselves.  These are freeform Markdown
  files with a TOML header.  The header contains various required or
  optional fields encoding information about the security issue, the
  package it affects and the affected versions.
- Tools for maintaining the database and exporting the data in
  various formats.  The *hsec-tools* Cabal package contains a
  library that defines the advisory data format and parsers, and the
  `hsec-tools` executable which is a front-end to advisory
  processing behaviours.

With both tool sources and advisory data in the repository, our
*continuous integration (CI)* pipelines have to do several things:

- **Build and test the tools.**  We want to test against several
  recent GHC versions (to avoid inconvenience for contributors).  We
  also perform a Nix build.
- **Check the validity of the advisory data.**  Advisories have to
  conform to our schema.  We can use `hsec-tools` to check each
  advisory file.  We *should* use the version of `hsec-tools` from
  the same commit, to allow the advisory format and tooling to
  evolve in lockstep.
- **Publish advisories.**  We will likely want to set up automation
  to publish [OSV][] streams, a web site, and other relevant
  artifacts.

[repo-security-advisories]: https://github.com/haskell/security-advisories
[OSV]: https://osv.dev/

The remainder of this post explains how we use `haskell-ci` and
GitHub Actions reusable workflows to achieve the first two
objectives.  The Security Response Team has not yet tackled
*publishing*, but the same techniques should be applicable.

## Introduction to `haskell-ci`

[`haskell-ci`][repo-haskell-ci] is a tool for generating CI
workflows for Haskell projects.  It supports GitHub Actions
(actively maintained) and Travis-CI (unmaintained).  You can install
`haskell-ci` via `cabal`:

```shell
% cabal install haskell-ci
```

Alternatively, you can clone the Git repository and build from
there:

```shell
% git clone https://github.com/haskell-CI/haskell-ci
% cd haskell-ci
% cabal install
```

Now that `haskell-ci` is on the `PATH`, you can generate the GitHub
actions workflow in a couple of steps.  First, add the GHC versions
you want to test with to the [`tested-with`][doc-cabal-tested-with]
field in your package's `.cabal` file:

```
cabal-version:      2.4
name:               hsec-tools
version:            0.1.0.0
tested-with:
  GHC ==8.10.7 || ==9.0.2 || ==9.2.7 || ==9.4.5 || ==9.6.2
…
```

::: note

Run `haskell-ci list-ghc` to see the list of GHC versions it knows
about.  `haskell-ci` updates usually follow soon after GHC releases,
especially major versions.

:::

Next, run `haskell-ci github path/to/package.cabal`.  It will
inspect the `.cabal` file to see what GHC versions to include in the
build matrix, and write `.github/workflows/haskell-ci.yml`.  Then
commit the changes and push (or create a pull request).  For
example:

```shell
% haskell-ci github code/hsec-tools/hsec-tools.cabal
*INFO* Generating GitHub config for testing for GHC versions: 8.10.7 9.0.2 9.2.7 9.4.5 9.6.2
% git add code .github
% git commit -m 'ci: add haskell-ci workflow' --quiet
% git push
…
```

[repo-haskell-ci]: https://github.com/haskell-CI/haskell-ci
[doc-cabal-tested-with]: https://cabal.readthedocs.io/en/3.6/cabal-package.html#pkg-field-tested-with


### What does the `haskell-ci` workflow do?

This post is not the place to belabour the details of GitHub Actions
[workflow syntax][gh-workflow-syntax].  But I will make a few
observations about the steps in the `haskell-ci` workflow.

- The build runs on Ubuntu (version 20.04 at time of writing).  You
  can tell `haskell-ci` to install extra APT packages via option
  `--apt "space separated list"`.
- There is a job *matrix* with a different job for each of the GHC
  versions mentioned in the `tested-with` field.
- Each job downloads GHC via [GHCUp][], a popular, multi-platform
  installation tool for Haskell.
- The package under test is not built *in situ*.  Instead, a source
  distribution is built using `cabal sdist`.  It is then unpacked in
  a different location, and built and tested there.  This helps
  **detect packaging errors** (e.g. missing extra source or data
  files).
- The job caches build tools and Haskell dependencies using the
  GitHub Actions cache mechanism (discussed later in the post).
  This saves time on subsequent test runs.
- There is a step that runs `cabal check`, which checks for issues
  that Hackage may complain about if you try to publish your package
  there.  This could be a mild annoyance for private or toy
  projects.

[gh-workflow-syntax]: https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions
[GHCUp]: https://www.haskell.org/ghcup/


### Adding Haddock and HLint jobs

`haskell-ci` makes it easy to add [Haddock][] (build documentation) and
[HLint][] (source code suggestions) jobs to your workflow.  Just use the
`--haddock` and `--hlint` options when creating the workflow:

```shell
% haskell-ci github --haddock --hlint path/to/package.cabal
```

[HLint]: https://hackage.haskell.org/package/hlint
[Haddock]: https://haskell-haddock.readthedocs.io/en/latest/

The Haddock step (if enabled) runs on every job in the build matrix.
Haddock is part of the GHC toolchain so there are no extra
dependencies.

HLint *is* an extra dependency, so if the HLint step is enabled, it
will install it via `cabal v2-install`.  The HLint step is skipped
for all but one of the jobs in the matrix—by default, the most
recent version of GHC.

::: note

`haskell-ci` prefers a particular version of `HLint`.  Sometimes,
that version of HLint doesn't build against the latest version of
GHC.  Use the `--hlint-job` option to override the job:

```shell
% haskell-ci github --hlint --hlint-job 9.4.5 foo.cabal
```

:::


### Updating the build matrix

When a new release of GHC comes along, updating the `haskell-ci`
workflow is as simple as adding it to the `tested-with` list, then
running:

```shell
% haskell-ci regenerate
No haskell-ci.sh, skipping bash regeneration
*INFO* Generating GitHub config for testing for GHC versions: 8.10.7 9.0.2 9.2.7 9.4.5 9.6.2
No .travis.yml, skipping travis regeneration
```

`haskell-ci regenerate` reuses the options from the original
invocation of `haskell-ci github`.  These were recorded in a comment
starting with `# REGENDATA` in `haskell-ci.yml`.  After running
`haskell-ci regenerate`, all that's left is to commit and push the
changes.


## GitHub Actions: passing the executable between jobs

Now the `haskell-ci` job is set up, it will build and test the
package on every push or pull request.  We have a further CI use
case: using the built executable to perform additional action.  So
we now turn to the problem of **how to use data produced by the
`haskell-ci` workflow in other jobs**.

GitHub Actions provides (at least) 3 mechanisms for passing data
between jobs.

- Jobs can define [***outputs***][gh-outputs].  They must be unicode
  strings and the size limit is 50MB.  Both limitations make this
  mechanism unsuitable for passing the built executable to dependent
  jobs.
- Jobs can [***cache***][gh-cache] dependencies to speed up the
  build.  But we want to cache the *result* of the build, which will
  often be different from previous build.  It seems to me that we
  *could* use the caching mechanism, but it doesn't feel like a good
  fit.
- Jobs can upload build [***artifacts***][gh-artifacts].  This makes
  them available to subsequent jobs in the workflow.  Unlike caches,
  they can also be downloaded by anyone with access to the
  repository.  This is the appropriate mechanism for our use case.

[gh-outputs]: https://docs.github.com/en/actions/using-jobs/defining-outputs-for-jobs
[gh-cache]: https://docs.github.com/en/actions/using-workflows/caching-dependencies-to-speed-up-workflows
[gh-artifacts]: https://docs.github.com/en/actions/using-workflows/storing-workflow-data-as-artifacts

::: note

By default, GitHub retains artifacts for 90 days.  The [duration can
be customised][gh-artifact-retention-period].

:::

[gh-artifact-retention-period]: https://docs.github.com/en/actions/using-workflows/storing-workflow-data-as-artifacts#configuring-a-custom-artifact-retention-period

We need to add two steps to the `linux` job.  First, we install the
`hsec-tools` executable.  It was already built—this just copies it
to a known location.  `--install-method=copy` ensures the executable
is copied to that location, not symlinked.

```yaml
      - name: install executable
        if: matrix.compiler == 'ghc-9.6.1'
        run: |
          $CABAL v2-install $ARG_COMPILER \
            --install-method=copy exe:hsec-tools
```

The second step uses the `upload-artifact` action to archive the
executable.  The artifact *bundle name* includes the commit hash.
The file *within the bundle* keeps the name `hsec-tools`.

```yaml
      - name: upload executable
        uses: actions/upload-artifact@v3
        if: matrix.compiler == 'ghc-9.6.1'
        with:
          name: hsec-tools-${{ github.sha }}
          path: ~/.cabal/bin/hsec-tools
```

::: note

All *Haskell* dependencies are statically linked in the binary.  It
does need some system libraries including *libgmp* and *libffi*.
But we do not need to preserve the Cabal store or provide the GHC
toolchain when we use the artifact.

:::

Notice that each of the new steps has the condition:

```yaml
        if: matrix.compiler == 'ghc-9.6.1'
```

The build matrix produces jobs for several different GHC versions.
But we only need one copy of the `hsec-tools` executable.  I'm not
totally happy with this approach because the patch will need
updating as the matrix evolves.  But I can live with it for now.

## GitHub Actions: workflows and jobs

A repository can define one or more CI *workflows*, written as YAML
files in the `.github/workflows/` directory.

Each *workflow* is comprised of one or more *jobs*.  It is
straightforward to declare dependencies between jobs *within a
workflow*.  But workflows themselves are independent.  There is no
reasonable way to specify that a particular workflow depends on the
result or outputs of another workflow.

This means that for our use case, we have to create a new *job*
within the `Haskell-CI` workflow.  Because `haskell-ci.yml` is
generated by the `haskell-ci` tool, we have to patch this file.
Fortunately, `haskell-ci` provides a mechanism to apply specified
patches when generating `haskell-ci.yml` (covered in the next
section).  Unfortunately, defining and maintaining our additional
job(s) as *patches* to YAML files is more unpleasant than dealing
with them as plain YAML.

## GitHub Actions: reusable workflows

[*Reusable workflows*][gh-reusable-workflows] provide a neat
solution.  A reusable workflow is defined as a separate YAML file,
just like ordinary workflows.  The main differences are:

- Reusable workflows use the trigger condition `workflow_call`,
  instead of the usual triggers like `push` or `pull_request`.
- Reusable workflows can be parameterised by *inputs*.  The calling
  job provides the values.  An input can be required or optional.

[gh-reusable-workflows]: https://docs.github.com/en/actions/using-workflows/reusing-workflows

The main use case for reusable workflows is to enable reuse, like
subroutines in programming.  Our use case is a bit different.  We
will define the *check-advisories* behaviour as a reusable workflow.
Although we will not be using it from multiple places, it still gives us several advantages:

- Separation of concerns: checking the advisories uses an artifact
  from the `haskell-ci` build/test job, but it's a distinct task.
- Maintainability: the behaviour is specified in an ordinary
  workflow YAML file.  We do not need to edit patch files to modify
  the workflow.
- We minimise the size and complexity of the patch to be applied to
  `haskell-ci.yml`.  The patch itself should rarely change, even if
  the workflow definition changes.

## Defining the *check-advisories* workflow

The *check-advisories* workflow is defined in
`.github/workflows/check-advisories.yml`.  The full content is
below, with commentary.

```yaml
name: Check security advisories
on:
  workflow_call:
    inputs:
      artifact-name:
        required: true
        type: string
```

The `workflow_call` trigger condition establishes it as a reusable
workflow.  We also define the `artifact-name` input.  The caller is
`required` to provide it.

```yaml
jobs:
  check-advisories:
    runs-on: ubuntu-20.04
    steps:
      - uses: actions/checkout@v3
        with:
          path: source
```

The workflow has a single job called `check-advisories`.  As usual,
the first step is to check out the repository.

```yaml
      - run: mkdir -p .local/bin
      - id: download
        uses: actions/download-artifact@v3
        with:
          name: ${{ inputs.artifact-name }}
          path: ~/.local/bin
      - run: chmod +x ~/.local/bin/hsec-tools
```

Next we download the `hsec-tools` artifact to `~/.local/bin`, which
is in the `PATH`.  Then we `chmod` it to make it executable.

```yaml
      - name: run checks
        run: |
          cd source
          RESULT=0
          find advisories EXAMPLE_ADVISORY.md -type f -name "*.md" \
              | while read FILE ; do
            echo -n "$FILE: "
            hsec-tools check < "$FILE" || RESULT=1
          done
          exit $RESULT
```

Finally, we `find` all the advisory files and run `hsec-tools check`
on each one.  If any of the checks fail, the job will fail (after
checking each file—we don't want to short-circuit).

## Calling the *check-advisories* workflow

Add a new job to the `haskell-ci.yml` workflow.  It must be a
**separate job**, not a *step* of the existing `linux` job.

```yaml
  check-advisories:
    name: Invoke check-advisories workflow
    needs: linux
    uses: ./.github/workflows/check-advisories.yml
    with:
      artifact-name: hsec-tools-${{ github.sha }}
```

The meaning of the fields is as follows:

- `uses`: *calls* the `check-advisories.yml` workflow.
- `with`: specifies values for the inputs, which in our
  case is the `artifact-name`.
- `needs`: expresses the dependency on the `linux` job.

::: note

You can call workflows defined in other repositories.  For example:

```yaml
uses: user-or-org/repo/.github/workflows/workflow.yml@v1
```

:::


## Patching `haskell-ci.yml`

At this stage, I have committed the `check-advisories.yml` reusable
workflow.  I also have *uncommitted changes* to `haskell-ci.yml`:

```diff
diff --git a/.github/workflows/haskell-ci.yml b/.github/workflows/haskell-ci.yml
index d51bb64..7ff8684 100644
--- a/.github/workflows/haskell-ci.yml
+++ b/.github/workflows/haskell-ci.yml
@@ -224,3 +224,19 @@ jobs:
         with:
           key: ${{ runner.os }}-${{ matrix.compiler }}-${{ github.sha }}
           path: ~/.cabal/store
+      - name: install executable
+        if: matrix.compiler == 'ghc-9.6.1'
+        run: |
+          $CABAL v2-install $ARG_COMPILER \
+            --install-method=copy exe:hsec-tools
+      - name: upload executable
+        uses: actions/upload-artifact@v3
+        if: matrix.compiler == 'ghc-9.6.1'
+        with:
+          name: hsec-tools-${{ github.sha }}
+          path: ~/.cabal/bin/hsec-tools
+  check-advisories:
+    name: Invoke check-advisories workflow
+    needs: linux
+    uses: ./.github/workflows/check-advisories.yml
+    with:
+      artifact-name: hsec-tools-${{ github.sha }}
```

We could commit these changes *as is*, but they will be lost the
next time we run `haskell-ci regenerate`.  Instead, create a *patch*
file:

```shell
% git diff > .github/haskell-ci.patch
```

Then tell `haskell-ci` to apply the patch when (re)generating
`haskell-ci.yml`.  What I would *like to do* is run:

```shell
% haskell-ci regenerate \
    --github-patches .github/haskell-ci.patch
```

The above command regenerates the `haskell-ci.yml` and correctly
applies our patch.  But it **does not add the new arguments to the
`REGENDATA` line**.  As a consequence, subsequent executions of
`haskell-ci regenerate` will not apply the patch unless you use the
`--github-patches` option every time.  This is not what we want, and
possibly a bug (I will investigate further, but not today).

**The workaround**: manually edit `haskell-ci.yml`, inserting
`"--github-patches",".github/haskell-ci.patch"` in the `REGENDATA`
line.   As a result of that change, running `haskell-ci regenerate`
without extra arguments applies the patch.

The final step is to **commit the patch file** together with the
updated `haskell-ci.yml`.

## Final words

In this article I showed how to use `haskell-ci` to generate a
GitHub Actions workflow for testing Haskell projects.  I also
demonstrated how to extend the `haskell-ci` workflow to save a built
executable as an artifact, which can then be used by other CI jobs.

I hope it has been a useful article, both for people starting out
and wondering how to test Haskell projects, as well as for projects
with more advanced CI workflows.

One area I would like to investigate further is how to skip the
`haskell-ci` workflow when the tool code did not change.  For
example, if someone submits a pull request that adds or updates an
advisory but does touch the `hsec-tools` code.  Artifacts and cache
entries have a name or key.  Right now we use the Git *commit* hash
in the artifact name.  Perhaps we could use the Git *tree* hash of
the `code/hsec-tools` directory instead:

```shell
% git rev-parse HEAD:code/hsec-tools 
a08aa5a2ee93ed09ec0025809226571969e24e3d
```

Uploading the artifact with a name based on the tree hash seems
straightforward.  The bigger challenge is how to skip the `linux`
jobs when the artifact for the current `hsec-tools` tree already
exists.  And how to *not* skip the `check-advisories` job, even
though it depends on the `linux` jobs.  I think it's probably
possible.  But it's a *nice-to-have*; this yak's haircut will have
to wait for another day.

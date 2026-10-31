# Releasing

Releases are a Maven repository on GitHub Pages: the `gh-pages` branch, its `maven/` directory (beside the live demo),
served at https://okay-effects.github.io/okay-cardano/maven. No account and no token is needed to use it, only resolvers:
this one and those of what it depends on.

## Using a release

See README.md: the resolvers of okay-simple, okay-scala, okay-http and this one, and
`"io.github.okay-effects" %%% "okay-cardano" % "0.1.0"`. Scala 3.3 LTS, JVM and Scala.js.

## Making one

While nobody but us uses it, a change is REPUBLISHED under the current number (0.1.0), the tag moved; a new number
only when someone else depends on it.

1. Everything green with no local copies of the libraries:
   `sbt -Dsbt.ivy.home=$(mktemp -d) clean test`.
2. `ThisBuild / version` without `-SNAPSHOT`; committed, tagged (`git tag -f v0.1.0`).
3. `rm -rf target/pages-maven && sbt publish && (cd sbt-okay-uplc && sbt publish)` — the sbt plugin is a build of
   its own (sbt 1, Scala 2.12) writing into the same `target/pages-maven`.
4. Onto `gh-pages`, under `maven/`:

   ```sh
   git worktree add /tmp/okay-cardano-pages gh-pages
   mkdir -p /tmp/okay-cardano-pages/maven && rsync -ac target/pages-maven/ /tmp/okay-cardano-pages/maven/   # -c: a republished checksum keeps its size
   git -C /tmp/okay-cardano-pages add -A && git -C /tmp/okay-cardano-pages commit -m "release 0.1.0"
   git -C /tmp/okay-cardano-pages push origin gh-pages && git worktree remove /tmp/okay-cardano-pages
   git push -f origin v0.1.0
   ```


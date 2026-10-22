# Reproducing and recording failures

## The example database

zizek stores every distinct failure a test finds and replays it first on the next run, before generating anything new. The default store is `.hegel/examples/` under the working directory. Nothing is stored or replayed for a test without a database key.

- `Hegel.Hspec.prop` derives a key from the module and the test's `describe` path and name, joined with `/` after escaping `\` and `/` inside each name as `\\` and `\/`. Renaming the test or a group around it orphans its stored failures. `propWith` and `propModify` take settings and keep the derived key, and `propFor` does the same for a property that uses an Hspec fixture. A property written directly as `it "name" \fixture -> …` has no key.
- `Hegel.Tasty.testProperty` has no key, so nothing is stored. Pass one with `testPropertyWith def {databaseKey = Just "Module:name"}`, and keep it distinct from every other property's key.

libhegel switches to its `ci` profile when it sees CI environment variables such as `CI` or `GITHUB_ACTIONS`, and that profile turns the database off. When stored failures aren't replaying, check whether the run picked up `ci`.

## Replaying one case

A failure report prints a replay token, unless `printBlob` is off. To rerun exactly that case, set `HEGEL_REPLAY` to the token and `HEGEL_REPLAY_KEY` to the test's database key, or pass Tasty's `--hegel-replay` and `--hegel-replay-key`. Narrow the run to that test with Hspec's `--match` or Tasty's `-p`. The replay runs once, skipping the database and every phase. If no selected test has that key, the run is ordinary exploration instead, so a pass proves nothing. A failing report whose replay did match prints `Replay selected for identity:` followed by the key. A property run directly with `Hegel.check` replays with `Hegel.decodeReplayToken` and `Hegel.replay`.

A token is tied to the engine version that printed it and to the generators that produced the case. After either changes, the replay is rejected or reports that it diverged.

## Writing the regression test

Tokens and stored entries break when the engine or the generators change, so the lasting reproduction is an ordinary unit test with literal values. Read those values from the shrunk report. Draws appear with their labels, `annotateShow` and `Stateful.respondShow` add intermediate values, and a stateful report lists each step with its arguments. Confirm the unit test fails on the unmodified code with the same message before writing the fix.

## Settings for discovery runs

Settings are a record of optional overrides on top of the resolved profile, for example `propWith def {testCases = Just 2000} "name" body`.

- `testCases` sets how many valid test cases to run. Raise it for long searches and for runs that rely on `target`.
- `reportMultipleFailures = Just True` reports every distinct failure instead of only the smallest, and `stopOnFirstFailure = Just False` keeps generating after the first.
- `phases = Just [Reuse]` replays stored failures without generating new cases. Leaving `Shrink` out of the list reports failures unshrunk, which is faster while a harness is still changing.
- `showStatistics = Just True` prints the distribution of `event` and `eventValue` labels after the run.
- `nondeterminism` sets what happens when replaying the same choices behaves differently. `Tolerate`, the default, confirms the failure by replaying it again and attaches a caveat. `Warn` also prints a notice, and `Forbid` makes it an error.
- `unboundedChoices = Just True` lifts the per-case limit on choices, which a long-running concurrent machine needs.
- `seed` or `derandomize` makes a run repeatable. The `ci` profile already derandomizes.

## Reading a report

`Hegel.check` returns a `Report` whose `result` is one of these:

- `Ok` when every test case passed.
- `Failures` with one entry per distinct failure.
- `GaveUp` when no valid test case could be generated. This is a harness problem, not a pass.
- `Aborted` when the run itself went wrong, such as a failed health check or a malformed test.

Its `stats` count `valid`, `invalid`, and `failing` test cases. `invalid` counts discarded test cases. Steps skipped inside a stateful machine don't count as discards and appear only in that machine's step log. `valid` and `failing` both include shrink attempts and replays, so neither counts generated cases, and `failing` is not a count of bugs. The entries in `Failures` are.

A run that switched to nondeterministic handling attaches a caveat to each affected failure. libhegel 0.48.0 also prints a `nondeterministic handling` line in the `showStatistics` block for every run that found a failure, including ordinary deterministic ones, so that line alone doesn't mean the test is nondeterministic.

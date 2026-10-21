# Stateful machines

The examples assume the `BlockArguments`, `OverloadedStrings`, `OverloadedRecordDot`, `NamedFieldPuns`, and `DuplicateRecordFields` extensions.

## A differential machine

Keep the system under test and an independent model side by side in the machine's state. Each rule applies one operation to both and compares what they return, and invariants compare their overall state.

```haskell
data World = World
  { store :: Store.Handle,
    model :: Map Int Int
  }

key :: Gen Int
key = Gen.int & Gen.min 0 & Gen.max 7 & Gen.build

insert :: Stateful.Rule World IO
insert =
  Stateful.rule "insert" \w -> do
    k <- forAllWithLabel "key" key
    v <- forAllWithLabel "value" $ Gen.int & Gen.build
    liftIO $ Store.insert w.store k v
    pure w {model = Map.insert k v w.model}

lookUp :: Stateful.Rule World IO
lookUp =
  Stateful.rule "lookup" \w -> do
    k <- forAll key
    r <- liftIO $ Store.lookup w.store k
    Stateful.respondShow r
    r === Map.lookup k w.model
    pure w

sizesAgree :: Stateful.Invariant World IO
sizesAgree =
  Stateful.invariant "sizes agree" \w -> do
    n <- liftIO $ Store.size w.store
    n === Map.size w.model

machine :: Stateful.Machine World IO
machine =
  Stateful.Machine
    { initial = do
        store <- resource Store.open Store.close
        pure World {store, model = Map.empty},
      rules = [insert, lookUp],
      invariants = [sizesAgree],
      stepCount = Stateful.defaultStepCount
    }
```

`Store` stands in for the code under test. The small key range makes overwrites and repeated lookups common. Run the machine like any other property, for example with `Hegel.Hspec.prop "store agrees with Data.Map" (Stateful.run machine)`.

## Rules

A sequential rule takes the current state, may draw and run effects, and returns the next state. `Stateful.respond` and `Stateful.respondShow` record what the system returned so the failure report shows it beside the step. `forAllWithLabel` names a draw in the failure report, which matters when a rule draws several values of the same type, as `insert` does with its key and value.

The engine chooses which rules run and how many steps a case takes, up to `stepCount`. In each test case it enables a different subset of the rules, and `Stateful.weighted` sets how often a rule runs relative to the others enabled. Weights must be finite and positive.

A rule that fails an `assume` or draws from an empty pool stops early, and that step is skipped. A skipped sequential rule doesn't count toward `stepCount`, its state update is discarded, and values it took with `Pool.consume` go back into their pools. A failure report full of steps that stopped early means the harness is spending its budget on nothing, so make rules reach their preconditions more often.

## Invariants

An invariant checks the state without changing it. Every invariant runs on the initial and final states. Between them, an invariant built with `Stateful.always` runs after every step, and a plain `Stateful.invariant` is sampled about once per test case. Prefer sampled invariants when checking reads through a cache or forces lazy state, since checking after every step would hide stale reads.

## Pools

A `Pool` holds values that rules created, so later rules can act on them. Create pools in `initial` or a plain property body with `Pool.new`, or with `Pool.named` to give their values a readable name in reports. Calling either inside a rule or an invariant aborts the run.

- `Pool.add pool x` puts a value in.
- `Pool.reuse pool` is a generator of values that leaves each one in the pool, drawn as `x <- forAll (Pool.reuse pool)`.
- `Pool.consume pool` is a generator that removes the value it yields.
- `Pool.transfer src dst` is a generator that moves the value it yields between pools, and the report follows it as one value across both.

Draws from a pool shrink like any other draw, so a counterexample shrinks toward the earliest values. Drawing from an empty pool skips the step.

A skipped rule keeps any `Pool.transfer` it already made. Draw a transfer only after every check that could reject the step.

When the system and the model refer to the same object by different handles, put a logical identity of your own in the pool and map it to each side's handle in the machine state. Rules then pick operands from the model's view of the world rather than from whatever the system last returned.

## Concurrent machines

`Hegel.Stateful.Concurrent` runs rules from several workers at once. Move to it only after a sequential machine passes. It differs from a sequential machine in these ways:

- **The model is shared by reference.** Rules return `()`, so keep mutable state in an `IORef`, `MVar`, or `TVar` built in `initial`.
- **Nothing is rolled back.** A skipped rule keeps every change it made, so check preconditions before mutating anything.
- **`stepCount` counts rounds.** In each round several workers each run rules, and invariants run only at the join after a round.
- **Worker counts come from a `Concurrency` value.** `Concurrent.run (Concurrent.fixed 3) machine` uses three workers, and `upTo` and `between` let the engine vary the count.
- **Rule groups set what may overlap.** `Concurrent.rule` puts a rule in the shared ungrouped group. `Concurrent.grouped name group` puts it in a named group, and rules in different groups never run in the same round.

Groups give a way to check consistency in the middle of a history. Put the operations that mutate state in one group and a rule that reads and checks the state in another. The checking rule never overlaps a mutation, so it always sees a consistent state, at any point in the history and not only at round boundaries.

```haskell
data Account = Account
  { balance :: IORef Int,
    journal :: IORef [Int]
  }

deposit :: Concurrent.Rule Account IO
deposit =
  Concurrent.grouped "deposit" "tellers" \account -> do
    n <- forAll amount
    liftIO $ post account n

audit :: Concurrent.Rule Account IO
audit =
  Concurrent.grouped "audit" "auditors" \account -> do
    b <- liftIO $ readIORef account.balance
    j <- liftIO $ readIORef account.journal
    Stateful.respondShow b
    b === sum j

machine :: Concurrent.Machine Account IO
machine =
  Concurrent.Machine
    { initial = liftIO $ Account <$> newIORef 0 <*> newIORef [],
      rules = [deposit, audit],
      invariants = [],
      stepCount = Concurrent.defaultStepCount
    }
```

`post` appends to the journal and updates the balance, and `amount` generates a posting. Run the machine with `Concurrent.run (Concurrent.fixed 3) machine`.

zizek controls each worker's draws but not how the threads interleave, so a race is found by repetition. When a race is suspected but rare, widen its window, for example with `yield` between a read and the write that depends on it, and run more test cases. A race may not fail on every replay, and the report then says so.

## Short concurrent bursts

A concurrent machine checks invariants. To check every result of a few concurrent operations, run them with `Branch.mapConcurrently`, then look for a sequential ordering of the same operations that, replayed against the model, gives the same results and final state. Two to four operations keep the search over orderings small.

```haskell
prop_linearizable :: Property ()
prop_linearizable = do
  ops <- forAll $ Gen.list genOp & Gen.minSize 2 & Gen.maxSize 4 & Gen.build
  store <- resource Store.open Store.close
  results <- Branch.mapConcurrently (liftIO . applyOp store) ops
  final <- liftIO $ Store.toMap store
  annotateShow results
  let orderings = map unzip $ permutations $ zip ops results
      explained = or [runModel Map.empty os == (rs, final) | (os, rs) <- orderings]
  assert explained "some sequential order explains the results"
```

`applyOp` runs one operation against the system and returns its result, and `runModel` runs a list of operations against the model and returns their results and the final model.

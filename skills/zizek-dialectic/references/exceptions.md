# Exceptions in zizek properties

The examples assume the `BlockArguments`, `OverloadedStrings`, `OverloadedRecordDot`, `NamedFieldPuns`, and `DuplicateRecordFields` extensions.

zizek steers each test case with control signals, such as the signal that discards a case after a failed `assume` or an empty pool. They are thrown as asynchronous exceptions so that ordinary handlers in a property body let them through. Assertions such as `assert`, `failure`, and `===` report failures by throwing ordinary synchronous exceptions.

Two rules follow from that.

1. **Never catch `SomeException` with `Control.Exception`'s `catch`, `try`, or `handle`.** Those catch asynchronous exceptions too, so they swallow control signals and corrupt the run.
2. **Never wrap a catch-all around code that asserts.** Even a catch-all that skips asynchronous exceptions still catches assertion failures, and the property passes without checking anything.

## Catching exceptions safely

Catch the specific exception type you expect. A handler for one type never matches a control signal, so a `try` at the specific exception type is safe in every exception library. Wrap the call into the code under test, not the assertions that follow it.

Catch-alls differ by library. `safe-exceptions` and `unliftio` skip asynchronous exceptions in `catchAny`, `tryAny`, and their other catch-alls, so control signals pass through. `Control.Exception` and the `exceptions` package's `MonadCatch` catch-alls catch asynchronous exceptions too, so never use them for `SomeException` near zizek. Every catch-all still catches assertion failures, so keep all of them away from assertions.

`PropertyT` is an instance of `MonadUnliftIO` but not of `MonadThrow`, `MonadCatch`, or `MonadMask`. `UnliftIO.Exception`'s functions work directly in a property body. The others work only inside `liftIO`, around an `IO` action.

## Fault injection

Inject a fault by passing the code under test a dependency that fails on a generated call number. The code under test has to accept that dependency somehow, for example as a record of functions, a type class instance, or a handle chosen at construction. Generate the fault position along with the operations, include a position past the last call so some cases run without a fault, and check the contract on both outcomes.

```haskell
data InjectedFault = InjectedFault
  deriving stock (Show)

instance Exception InjectedFault

prop_commitIsAtomic :: Property ()
prop_commitIsAtomic = do
  writes <- forAll $ Gen.list genWrite & Gen.maxSize 10 & Gen.build
  failAt <- forAll $ Gen.int & Gen.min 1 & Gen.max (length writes + 1) & Gen.build
  calls <- liftIO $ newIORef (0 :: Int)
  store <- liftIO Store.new
  before <- liftIO $ Store.snapshot store
  let flakyWrite key value = do
        n <- atomicModifyIORef' calls \c -> (c + 1, c + 1)
        when (n == failAt) $ throwIO InjectedFault
        Store.rawWrite store key value
  result <- liftIO . try @InjectedFault $ Store.commitWith store flakyWrite writes
  after <- liftIO $ Store.snapshot store
  case result of
    Left InjectedFault -> do
      event "fault fired"
      after === before
    Right () -> after === foldl' applyWrite before writes
```

`Store`, `genWrite`, and `applyWrite` stand in for the code under test and its model, and `try` is `Control.Exception`'s, whose first type argument is the exception type. `Store.commitWith` receives the store as well as the faulty write, so it can roll back what it already applied. The `event` records how often the fault actually fired, so `showStatistics` shows whether both outcomes are being exercised. Follow a failed operation with further operations when the contract promises the structure stays usable, since a corrupted structure often fails only on its next use.

## Cancellation

`Fork.spawn` runs part of a property body on its own thread, and `Fork.cancel` interrupts it with an asynchronous exception and waits for the thread to exit. Use that pair to test code that must stay consistent when interrupted. Look for an `MVar` left empty, a resource never released, and an update left half applied, then use the structure again, since a deadlock shows up only when the next operation blocks.

An operation that finishes before the cancellation arrives exercises nothing. Call `Fork.poll` just before cancelling, record whether it returned `Nothing`, meaning the fork was still running, with `event`, and widen the window with a larger operation or a `yield` when cancellations rarely land in time.

Code under test that swallows asynchronous exceptions, such as a `Control.Exception` catch-all for `SomeException`, is itself a bug this kind of test finds. The fork never exits, so `Fork.cancel` never returns and the test hangs. Run cancellation tests under a timeout so the hang becomes a visible failure.

## Cleanup

Release what a test case acquires with `resource`, which pairs an acquire action with the release for the value it returns, `resource_` for setup and teardown that pass no value, or `registerFinalizer` for a release action alone. Finalizers run at the end of every test case, last registered first, including every shrink attempt and replay. A finalizer that throws aborts the whole run, so a finalizer must not fail and must not draw.

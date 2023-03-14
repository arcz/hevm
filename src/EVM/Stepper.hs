{-# Language GADTs #-}
{-# Language DataKinds #-}

module EVM.Stepper
  ( Action (..)
  , Stepper
  , exec
  , execFully
  , run
  , runFully
  , wait
  , ask
  , evm
  , evmIO
  , entering
  , enter
  , interpret
  )
where

-- This module is an abstract definition of EVM steppers.
-- Steppers can be run as TTY debuggers or as CLI test runners.
--
-- The implementation uses the operational monad pattern
-- as the framework for monadic interpretation.

import Prelude hiding (fail)

import Control.Monad.Operational (Program, singleton, view, ProgramViewT(..), ProgramView)
import Control.Monad.State.Strict (StateT (runStateT), execStateT)
import Control.Monad.ST (stToIO)
import Control.Monad.ST.Strict (RealWorld)
import Data.Text (Text)

import EVM hiding (result)
import EVM.Exec qualified
import EVM.Fetch qualified as Fetch
import EVM.Types (Expr, EType(..))

-- | The instruction type of the operational monad
data Action s a where

  -- | Keep executing until an intermediate result is reached
  Exec :: Action s (VMResult s)

  -- | Keep executing until an intermediate state is reached
  Run :: Action s (VM s)

  -- | Wait for a query to be resolved
  Wait :: Query s -> Action s ()

  -- | Multiple things can happen
  Ask :: Choose s -> Action s ()

  -- | Embed a VM state transformation
  EVM  :: EVM s a -> Action s a

  -- | Perform an IO action
  IOAct :: StateT (VM s) IO a -> Action s a -- they should all just be this?

-- | Type alias for an operational monad of @Action@
type Stepper s a = Program (Action s) a

-- Singleton actions

exec :: Stepper s (VMResult s)
exec = singleton Exec

run :: Stepper s (VM s)
run = singleton Run

wait :: Query s -> Stepper s ()
wait = singleton . Wait

ask :: Choose s -> Stepper s ()
ask = singleton . Ask

evm :: EVM s a -> Stepper s a
evm = singleton . EVM

evmIO :: StateT (VM s) IO a -> Stepper s a
evmIO = singleton . IOAct

-- | Run the VM until final result, resolving all queries
execFully :: Stepper s (Either (Error s) (Expr Buf))
execFully =
  exec >>= \case
    VMFailure (Query q) ->
      wait q >> execFully
    VMFailure (Choose q) ->
      ask q >> execFully
    VMFailure x ->
      pure (Left x)
    VMSuccess x ->
      pure (Right x)

-- | Run the VM until its final state
runFully :: Stepper s (VM s)
runFully = do
  vm <- run
  case vm._result of
    Nothing -> error "should not occur"
    Just (VMFailure (Query q)) ->
      wait q >> runFully
    Just (VMFailure (Choose q)) ->
      ask q >> runFully
    Just _ ->
      pure vm

entering :: Text -> Stepper s a -> Stepper s a
entering t stepper = do
  evm (EVM.pushTrace (EVM.EntryTrace t))
  x <- stepper
  evm EVM.popTrace
  pure x

enter :: Text -> Stepper s ()
enter t = evm (EVM.pushTrace (EVM.EntryTrace t))

interpret :: Fetch.Fetcher RealWorld -> VM RealWorld -> Stepper RealWorld a -> IO a
interpret fetcher vm =
  eval . view

  where
    eval
      :: ProgramView (Action RealWorld) a
      -> IO a

    eval (Return x) =
      pure x

    eval (action :>>= k) =
      case action of
        Exec -> do
          (result, vm') <- stToIO $ runStateT EVM.Exec.exec vm
          interpret fetcher vm' (k result)
        Run -> do
          vm' <- stToIO $ execStateT EVM.Exec.exec vm
          interpret fetcher vm' (k vm')
        Wait q -> do
          m <- fetcher q
          (result, vm') <- stToIO $ runStateT m vm
          interpret fetcher vm' (k result)
        Ask _ ->
          error "cannot make choices with this interpreter"
        IOAct m -> do
          (result, vm') <- runStateT m vm
          interpret fetcher vm' (k result)
        EVM m -> do
          (result, vm') <- stToIO $ runStateT m vm
          interpret fetcher vm' (k result)

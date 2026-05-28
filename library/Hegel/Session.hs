-- | Lifecycle management for the @hegel@ child process.
module Hegel.Session
  ( -- * Sessions
    Session,
    globalSession,
    openSession,
    withSession,
    closeSession,
    invalidateSession,

    -- * Configuration
    SessionConfig (..),
    defaultSessionConfig,
  )
where

import Hegel.Session.Internal
  ( Session,
    SessionConfig (..),
    closeSession,
    defaultSessionConfig,
    globalSession,
    invalidateSession,
    openSession,
    withSession,
  )

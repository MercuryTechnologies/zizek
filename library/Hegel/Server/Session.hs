-- | Lifecycle management for the @hegel@ child process.
module Hegel.Server.Session
  ( Session,
    globalSession,
    openSession,
    withSession,
    closeSession,
    invalidateSession,
    SessionConfig (..),
    defaultSessionConfig,
  )
where

import Hegel.Server.Session.Internal
  ( Session,
    SessionConfig (..),
    closeSession,
    defaultSessionConfig,
    globalSession,
    invalidateSession,
    openSession,
    withSession,
  )

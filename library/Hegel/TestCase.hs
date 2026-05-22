module Hegel.TestCase
  ( TestCase (..),
  )
where

import Hegel.DataSource (DataSource)

data TestCase = TestCase
  { dataSource :: !DataSource
  }

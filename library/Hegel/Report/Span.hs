-- | The source span types in this module are derived from
-- @Hedgehog.Internal.Show@ in the @hedgehog@ package.
--
-- Copyright 2017-2018, Jacob Stanley. All Rights Reserved.
-- Licensed under the BSD-3-Clause license; see @licenses/hedgehog.LICENSE@
-- for the full license text.
module Hegel.Report.Span
  ( LineNo (..),
    ColumnNo (..),
    Span (..),
    spanFromSrcLoc,
  )
where

import GHC.Stack (SrcLoc (..))

-- | A 1-based line number.
newtype LineNo = LineNo {unLineNo :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral)

-- | A 1-based column number.
newtype ColumnNo = ColumnNo {unColumnNo :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral)

-- | A source span: file plus start and end lines.  Columns are not tracked;
-- the renderer derives column ranges from the source text itself (see
-- @lastLineSpan@ in "Hegel.Report.Source").
data Span = Span
  { spanFile :: !FilePath,
    spanStartLine :: !LineNo,
    spanEndLine :: !LineNo
  }
  deriving stock (Eq, Ord, Show)

-- | Build a 'Span' from a GHC 'SrcLoc'. Every field is available from the
-- compiler-emitted location, so this conversion is total.
spanFromSrcLoc :: SrcLoc -> Span
spanFromSrcLoc sl =
  Span
    { spanFile = sl.srcLocFile,
      spanStartLine = fromIntegral sl.srcLocStartLine,
      spanEndLine = fromIntegral sl.srcLocEndLine
    }

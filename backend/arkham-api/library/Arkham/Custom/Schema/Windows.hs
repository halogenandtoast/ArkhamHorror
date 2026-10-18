{-# LANGUAGE TemplateHaskell #-}
-- Build-time data for the card builder; the engine never reads it, so -O2 has
-- nothing to pay off on. See Arkham.Custom.Schema.
{-# OPTIONS_GHC -O0 #-}

{- | Which window each 'Arkham.Matcher.WindowMatcher' fires on, for the card
builder.

The table itself is "Arkham.Custom.Schema.Windows.Table"; the splice here is
what refuses to build when it drifts from the engine's own types. See
"Arkham.Custom.Schema.Windows.Check".
-}
module Arkham.Custom.Schema.Windows (matcherWindows) where

import Arkham.Custom.Schema.Windows.Check (checkedTable)
import Arkham.Prelude

-- | Keyed by matcher constructor name; an empty list means "no window of its own".
matcherWindows :: [(Text, [Text])]
matcherWindows = $(checkedTable)

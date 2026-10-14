{-# LANGUAGE ImplicitParams #-}

{- | The narrative variants written for The Innsmouth Conspiracy.

This is the one place a variant is named. The campaign and its scenarios resolve
whichever one the game was created with and read beats off it, so adding a
variant means adding a line here and nothing else.
-}
module Arkham.Narrative.TheInnsmouthConspiracy (
  module Arkham.Narrative.TheInnsmouthConspiracy.Types,
  getInnsmouthNarrative,
) where

import Arkham.Helpers.Narrative
import Arkham.I18n
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Narrative.OlausWormius.TheInnsmouthConspiracy qualified as OlausWormius
import Arkham.Narrative.TheInnsmouthConspiracy.Types
import Arkham.Prelude

narratives :: (HasI18n, ReverseQueue m) => [(Text, InnsmouthNarrative m)]
narratives = [(OlausWormius.variant, OlausWormius.narrative)]

{- | The narrative in play, or the Campaign Guide's own.

Resolved where it is used, so the beats it hands back are built under that site's
i18n scope.
-}
getInnsmouthNarrative :: (HasI18n, ReverseQueue m) => m (InnsmouthNarrative m)
getInnsmouthNarrative = do
  active <- getNarrative
  pure $ fromMaybe noInnsmouthNarrative (active >>= (`lookup` narratives))

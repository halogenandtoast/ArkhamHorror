{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.UmordhothsWrath where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude

newtype UmordhothsWrath = UmordhothsWrath Attrs
  deriving newtype (Show, ToJSON, FromJSON)

umordhothsWrath :: TreacheryId -> UmordhothsWrath
umordhothsWrath uuid = UmordhothsWrath $ baseAttrs uuid "01158"

instance HasActions env investigator UmordhothsWrath where
  getActions i window (UmordhothsWrath attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env UmordhothsWrath where
  runMessage msg (UmordhothsWrath attrs@Attrs {..}) = case msg of
    Revelation _ tid | tid == treacheryId -> error "TODO"
    _ -> UmordhothsWrath <$> runMessage msg attrs

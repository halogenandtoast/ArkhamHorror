{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.RitualSite where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype RitualSite = RitualSite Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ritualSite :: RitualSite
ritualSite = RitualSite
  $ baseAttrs "01156" "Ritual Site" 3 (PerPlayer 2) Plus [Squiggle] [Cave]

instance HasModifiersFor env RitualSite where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env RitualSite where
  getActions i window (RitualSite attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env RitualSite where
  runMessage msg (RitualSite attrs) = case msg of
    EndRound -> do
      playerCount <- asks $ unPlayerCount . getCount ()
      RitualSite <$> runMessage
        msg
        (attrs & clues .~ fromGameValue (PerPlayer 2) playerCount)
    _ -> RitualSite <$> runMessage msg attrs

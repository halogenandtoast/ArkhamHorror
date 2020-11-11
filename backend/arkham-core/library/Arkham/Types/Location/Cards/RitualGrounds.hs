{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.RitualGrounds where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype RitualGrounds = RitualGrounds Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ritualGrounds :: RitualGrounds
ritualGrounds = RitualGrounds $ (baseAttrs
                                  "81017"
                                  "Ritual Grounds"
                                  2
                                  (PerPlayer 1)
                                  Equals
                                  [Hourglass, Equals]
                                  [Unhallowed]
                                )
  { locationVictory = Just 1
  }

instance HasModifiersFor env RitualGrounds where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env RitualGrounds where
  getActions i window (RitualGrounds attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env RitualGrounds where
  runMessage msg l@(RitualGrounds attrs@Attrs {..}) = case msg of
    EndTurn iid | iid `elem` locationInvestigators -> l <$ unshiftMessages
      [DrawCards iid 1 False, InvestigatorAssignDamage iid (toSource attrs) 0 1]
    _ -> RitualGrounds <$> runMessage msg attrs

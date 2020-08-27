{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.TheYellowSign where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import Lens.Micro

newtype TheYellowSign = TheYellowSign Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theYellowSign :: TreacheryId -> a -> TheYellowSign
theYellowSign uuid _ = TheYellowSign $ baseAttrs uuid "01176"

instance HasActions env investigator TheYellowSign where
  getActions i window (TheYellowSign attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env TheYellowSign where
  runMessage msg (TheYellowSign attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessage
        (BeginSkillTest
          iid
          (TreacherySource treacheryId)
          Nothing
          SkillWillpower
          4
          []
          [ InvestigatorAssignDamage iid (TreacherySource treacheryId) 0 2
          , SearchDeckForTraits iid (InvestigatorTarget iid) [Madness] -- TODO: We may need to specify weakness
          ]
          mempty
          mempty
        )
      TheYellowSign <$> runMessage msg (attrs & resolved .~ True)
    _ -> TheYellowSign <$> runMessage msg attrs

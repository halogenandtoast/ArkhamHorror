{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.DeepBelowYourHouse where

import Arkham.Import

import Arkham.Types.Card.EncounterCardMatcher
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner

newtype DeepBelowYourHouse = DeepBelowYourHouse Attrs
  deriving newtype (Show, ToJSON, FromJSON)

deepBelowYourHouse :: DeepBelowYourHouse
deepBelowYourHouse = DeepBelowYourHouse $ (baseAttrs
                                            "50021"
                                            "Ghoul Pits"
                                            EncounterSet.ReturnToTheGathering
                                            4
                                            (PerPlayer 1)
                                            Squiggle
                                            [Plus]
                                            mempty
                                          )
  { locationVictory = Just 1
  }

instance HasModifiersFor env DeepBelowYourHouse where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env DeepBelowYourHouse where
  getActions i window (DeepBelowYourHouse attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env DeepBelowYourHouse where
  runMessage msg l@(DeepBelowYourHouse attrs) = case msg of
    RevealLocation (Just iid) lid | lid == locationId attrs -> do
      unshiftMessage
        (BeginSkillTest
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          Nothing
          SkillAgility
          3
        )
      DeepBelowYourHouse <$> runMessage msg attrs
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} n
      | isSource attrs source -> do
        l <$ unshiftMessages
          (replicate
            n
            (FindAndDrawEncounterCard iid (EncounterCardMatchByCardCode "01159")
            )
          )
    _ -> DeepBelowYourHouse <$> runMessage msg attrs

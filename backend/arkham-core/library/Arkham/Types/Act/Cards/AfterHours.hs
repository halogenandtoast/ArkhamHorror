module Arkham.Types.Act.Cards.AfterHours where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Asset.Cards as Assets
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype AfterHours = AfterHours ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions, HasModifiersFor env)

afterHours :: ActCard AfterHours
afterHours = act
  (1, A)
  AfterHours
  Cards.afterHours
  (Just $ GroupClueCost (PerPlayer 3) Nothing)

instance ActRunner env => RunMessage env AfterHours where
  runMessage msg a@(AfterHours attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> a <$ pushAll
      [ AddCampaignCardToEncounterDeck Assets.jazzMulligan
      , ShuffleEncounterDiscardBackIn
      , NextAct aid "02046"
      ]
    _ -> AfterHours <$> runMessage msg attrs

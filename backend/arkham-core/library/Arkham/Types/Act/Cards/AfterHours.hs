module Arkham.Types.Act.Cards.AfterHours where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Assets
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype AfterHours = AfterHours ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

afterHours :: AfterHours
afterHours = AfterHours $ baseAttrs
  "02045"
  "After Hours"
  (Act 1 A)
  (Just $ RequiredClues (PerPlayer 3) Nothing)

instance ActionRunner env => HasActions env AfterHours where
  getActions i window (AfterHours x) = getActions i window x

instance ActRunner env => RunMessage env AfterHours where
  runMessage msg a@(AfterHours attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> a <$ pushAll
      [ AddCampaignCardToEncounterDeck Assets.jazzMulligan
      , ShuffleEncounterDiscardBackIn
      , NextAct aid "02046"
      ]
    _ -> AfterHours <$> runMessage msg attrs

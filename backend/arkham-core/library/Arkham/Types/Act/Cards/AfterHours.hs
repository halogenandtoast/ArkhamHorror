module Arkham.Types.Act.Cards.AfterHours where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Asset.Cards qualified as Assets
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message

newtype AfterHours = AfterHours ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

afterHours :: ActCard AfterHours
afterHours = act
  (1, A)
  AfterHours
  Cards.afterHours
  (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance ActRunner env => RunMessage env AfterHours where
  runMessage msg a@(AfterHours attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      jazzMulligan <- getSetAsideEncounterCard Assets.jazzMulligan
      a <$ pushAll
        [ AddToEncounterDeck jazzMulligan
        , ShuffleEncounterDiscardBackIn
        , NextAct aid "02046"
        ]
    _ -> AfterHours <$> runMessage msg attrs

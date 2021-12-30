module Arkham.Act.Cards.AfterHours where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Asset.Cards qualified as Assets
import Arkham.Act.Attrs
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message

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
        , AdvanceActDeck actDeckId (toSource attrs)
        ]
    _ -> AfterHours <$> runMessage msg attrs

module Arkham.Act.Cards.AfterHours where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message

newtype AfterHours = AfterHours ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

afterHours :: ActCard AfterHours
afterHours = act
  (1, A)
  AfterHours
  Cards.afterHours
  (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage AfterHours where
  runMessage msg a@(AfterHours attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      jazzMulligan <- getSetAsideEncounterCard Assets.jazzMulligan
      pushAll
        [ AddToEncounterDeck jazzMulligan
        , ShuffleEncounterDiscardBackIn
        , advanceActDeck attrs
        ]
      pure a
    _ -> AfterHours <$> runMessage msg attrs

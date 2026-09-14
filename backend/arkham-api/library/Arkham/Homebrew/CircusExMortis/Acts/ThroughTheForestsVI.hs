module Arkham.Homebrew.CircusExMortis.Acts.ThroughTheForestsVI (throughTheForestsVI) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Acts.ArrivingAt
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.NowArriving (Arrival (..))
import Arkham.Matcher

newtype ThroughTheForestsVI = ThroughTheForestsVI ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throughTheForestsVI :: ActCard ThroughTheForestsVI
throughTheForestsVI = act (1, A) ThroughTheForestsVI Cards.throughTheForestsVI Nothing

instance HasAbilities ThroughTheForestsVI where
  getAbilities = actAbilities1 \a ->
    restricted a 1 (InvestigatorsHaveClues (atLeast 4)) $ Objective $ forced $ RoundEnds #when

instance RunMessage ThroughTheForestsVI where
  runMessage msg a@(ThroughTheForestsVI attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      arrivingAt attrs ArrivingAtChicago (selectJust $ locationIs Locations.caboose)
      advanceActDeck attrs
      pure a
    _ -> ThroughTheForestsVI <$> liftRunMessage msg attrs

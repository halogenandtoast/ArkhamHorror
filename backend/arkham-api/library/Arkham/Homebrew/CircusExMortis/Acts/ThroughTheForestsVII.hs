module Arkham.Homebrew.CircusExMortis.Acts.ThroughTheForestsVII (throughTheForestsVII) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Acts.ArrivingAt
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.NowArriving (Arrival (..))
import Arkham.Matcher

newtype ThroughTheForestsVII = ThroughTheForestsVII ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throughTheForestsVII :: ActCard ThroughTheForestsVII
throughTheForestsVII = act (1, A) ThroughTheForestsVII Cards.throughTheForestsVII Nothing

instance HasAbilities ThroughTheForestsVII where
  getAbilities = actAbilities1 \a ->
    restricted a 1 (InvestigatorsHaveClues (atLeast 4)) $ Objective $ forced $ RoundEnds #when

instance RunMessage ThroughTheForestsVII where
  runMessage msg a@(ThroughTheForestsVII attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      arrivingAt attrs ArrivingAtMemphis (selectJust $ locationIs Locations.caboose)
      advanceActDeck attrs
      pure a
    _ -> ThroughTheForestsVII <$> liftRunMessage msg attrs

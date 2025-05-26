module Arkham.Location.Cards.ExpeditionCamp (expeditionCamp) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheUntamedWilds.Helpers

newtype ExpeditionCamp = ExpeditionCamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionCamp :: LocationCard ExpeditionCamp
expeditionCamp = symbolLabel $ location ExpeditionCamp Cards.expeditionCamp 1 (Static 0)

instance HasAbilities ExpeditionCamp where
  getAbilities (ExpeditionCamp a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "expeditionCamp.resign" $ locationResignAction a
      , restricted a 2 (AnyCriterion [Here, IsReturnTo] <> HasSupply Map) actionAbility
      ]

instance RunMessage ExpeditionCamp where
  runMessage msg l@(ExpeditionCamp attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      explorationDeck <- getExplorationDeck
      let (viewing, rest) = splitAt 3 explorationDeck
      let cardPairs = map (toSnd (`deleteFirst` viewing)) viewing
      focusCards viewing do
        setScenarioDeck ExplorationDeck rest
        chooseOneM iid $ scenarioI18n do
          questionLabeled' "expeditionCamp.bottom"
          for_ cardPairs \(c, remaining) -> targeting c do
            putCardOnBottomOfDeck iid ExplorationDeck c
            focusCards remaining do
              chooseOneAtATimeM iid do
                questionLabeled' "expeditionCamp.top"
                targets remaining (putCardOnTopOfDeck iid ExplorationDeck)
      pure l
    _ -> ExpeditionCamp <$> liftRunMessage msg attrs

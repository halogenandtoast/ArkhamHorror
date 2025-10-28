module Arkham.Location.Cards.TowersOfPnakotus (towersOfPnakotus) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.TheCityOfArchives.Helpers

newtype TowersOfPnakotus = TowersOfPnakotus LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

towersOfPnakotus :: LocationCard TowersOfPnakotus
towersOfPnakotus = location TowersOfPnakotus Cards.towersOfPnakotus 2 (PerPlayer 2)

instance HasAbilities TowersOfPnakotus where
  getAbilities (TowersOfPnakotus a) =
    extendRevealed1 a $ playerLimit PerTurn $ restricted a 1 Here actionAbility

instance RunMessage TowersOfPnakotus where
  runMessage msg l@(TowersOfPnakotus attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardableCount <- fieldMap InvestigatorHand (count (`cardMatch` NonWeakness)) iid
      scenarioI18n $ chooseAmount' iid "cardsToDiscard" "$cards" 0 discardableCount attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "$cards" -> cardAmount) (isTarget attrs -> True) -> do
      repeated cardAmount $ chooseAndDiscardCard iid (attrs.ability 1)
      shuffleDiscardBackIn iid
      drawCards iid (attrs.ability 1) (cardAmount + 1)
      pure l
    _ -> TowersOfPnakotus <$> liftRunMessage msg attrs

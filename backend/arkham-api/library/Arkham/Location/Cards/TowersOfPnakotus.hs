module Arkham.Location.Cards.TowersOfPnakotus (towersOfPnakotus, TowersOfPnakotus (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype TowersOfPnakotus = TowersOfPnakotus LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

towersOfPnakotus :: LocationCard TowersOfPnakotus
towersOfPnakotus = location TowersOfPnakotus Cards.towersOfPnakotus 2 (PerPlayer 2)

instance HasAbilities TowersOfPnakotus where
  getAbilities (TowersOfPnakotus attrs) =
    withBaseAbilities attrs [playerLimit PerTurn $ restrictedAbility attrs 1 Here actionAbility]

instance RunMessage TowersOfPnakotus where
  runMessage msg l@(TowersOfPnakotus attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardableCount <- fieldMap InvestigatorHand (count (`cardMatch` NonWeakness)) iid
      player <- getPlayer iid
      pushM
        $ chooseAmounts
          player
          "Choose number of cards to discard"
          (MaxAmountTarget discardableCount)
          [("Cards", (0, discardableCount))]
          (toTarget attrs)
      pure l
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let cardAmount = getChoiceAmount "Cards" choices
      let drawing = drawCards iid (attrs.ability 1) (cardAmount + 1)
      pushAll
        $ replicate cardAmount (toMessage $ chooseAndDiscardCard iid $ attrs.ability 1)
        <> [ShuffleDiscardBackIn iid, drawing]
      pure l
    _ -> TowersOfPnakotus <$> runMessage msg attrs

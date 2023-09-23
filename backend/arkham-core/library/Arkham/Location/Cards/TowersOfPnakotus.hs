module Arkham.Location.Cards.TowersOfPnakotus (
  towersOfPnakotus,
  TowersOfPnakotus (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype TowersOfPnakotus = TowersOfPnakotus LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

towersOfPnakotus :: LocationCard TowersOfPnakotus
towersOfPnakotus =
  location TowersOfPnakotus Cards.towersOfPnakotus 2 (PerPlayer 2)

instance HasAbilities TowersOfPnakotus where
  getAbilities (TowersOfPnakotus attrs) =
    withBaseAbilities
      attrs
      [ limitedAbility (PlayerLimit PerTurn 1)
          $ restrictedAbility attrs 1 Here
          $ ActionAbility Nothing
          $ ActionCost 1
      ]

instance RunMessage TowersOfPnakotus where
  runMessage msg l@(TowersOfPnakotus attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      discardableCount <- fieldMap InvestigatorHand (count (`cardMatch` NonWeakness)) iid
      push
        $ chooseAmounts
          iid
          "Choose number of cards to discard"
          (MaxAmountTarget discardableCount)
          [("Cards", (0, discardableCount))]
          (toTarget attrs)
      pure l
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let cardAmount = getChoiceAmount "Cards" choices
      drawing <- drawCards iid (toAbilitySource attrs 1) (cardAmount + 1)
      pushAll
        $ replicate cardAmount (toMessage $ chooseAndDiscardCard iid $ toAbilitySource attrs 1)
        <> [ShuffleDiscardBackIn iid, drawing]
      pure l
    _ -> TowersOfPnakotus <$> runMessage msg attrs

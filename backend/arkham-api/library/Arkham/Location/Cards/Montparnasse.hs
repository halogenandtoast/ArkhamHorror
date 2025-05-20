module Arkham.Location.Cards.Montparnasse (montparnasse) where

import Arkham.Ability
import Arkham.Card.CardDef
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype Montparnasse = Montparnasse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

montparnasse :: LocationCard Montparnasse
montparnasse = location Montparnasse Cards.montparnasse 2 (PerPlayer 1)

instance HasAbilities Montparnasse where
  getAbilities (Montparnasse a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 Here
      $ FastAbility
      $ HandDiscardCost 1 #any

instance RunMessage Montparnasse where
  runMessage msg a@(Montparnasse attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (DiscardCardPayment cards) -> do
      let countWillpower = count (== #willpower) . cdSkills . toCardDef
      let totalWillpower = sum $ map countWillpower cards
      gainResources iid (attrs.ability 1) totalWillpower
      pure a
    _ -> Montparnasse <$> liftRunMessage msg attrs

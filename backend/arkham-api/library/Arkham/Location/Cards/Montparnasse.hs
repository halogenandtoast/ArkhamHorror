module Arkham.Location.Cards.Montparnasse (
  montparnasse,
  Montparnasse (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.SkillType

newtype Montparnasse = Montparnasse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

montparnasse :: LocationCard Montparnasse
montparnasse = location Montparnasse Cards.montparnasse 2 (PerPlayer 1)

instance HasAbilities Montparnasse where
  getAbilities (Montparnasse attrs) =
    withBaseAbilities
      attrs
      [ limitedAbility (PlayerLimit PerRound 1)
        $ restrictedAbility attrs 1 Here
        $ FastAbility
        $ HandDiscardCost 1 #any
      | locationRevealed attrs
      ]

instance RunMessage Montparnasse where
  runMessage msg a@(Montparnasse attrs) = case msg of
    UseCardAbility iid source 1 _ (DiscardCardPayment cards)
      | isSource attrs source -> do
          let
            countWillpower = count (== SkillIcon SkillWillpower) . cdSkills . toCardDef
            totalWillpower = sum $ map countWillpower cards
          a <$ push (TakeResources iid totalWillpower (toAbilitySource attrs 1) False)
    _ -> Montparnasse <$> runMessage msg attrs

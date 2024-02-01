module Arkham.Location.Cards.Yuggoth (
  yuggoth,
  Yuggoth (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype Yuggoth = Yuggoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

yuggoth :: LocationCard Yuggoth
yuggoth = location Yuggoth Cards.yuggoth 2 (Static 3)

instance HasAbilities Yuggoth where
  getAbilities (Yuggoth a) =
    withBaseAbilities
      a
      [ restrictedAbility
          a
          1
          (Here <> AnyCriterion [CluesOnThis (AtLeast $ Static 1), CanDrawCards])
          $ ActionAbility []
          $ ActionCost 1
      ]

instance RunMessage Yuggoth where
  runMessage msg l@(Yuggoth attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      drawCard <- newCardDraw iid (toSource attrs) 1
      pushAll [FlipClues (toTarget attrs) 1, DrawCards drawCard]
      pure l
    _ -> Yuggoth <$> runMessage msg attrs

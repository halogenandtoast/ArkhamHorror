module Arkham.Location.Cards.Montmartre209 (
  montmartre209,
  Montmartre209 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype Montmartre209 = Montmartre209 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

montmartre209 :: LocationCard Montmartre209
montmartre209 = location Montmartre209 Cards.montmartre209 3 (PerPlayer 1)

instance HasAbilities Montmartre209 where
  getAbilities (Montmartre209 attrs) =
    withBaseAbilities
      attrs
      [ limitedAbility (GroupLimit PerRound 1) $
        restrictedAbility attrs 1 Here $
          ActionAbility Nothing $
            ActionCost 1
      | locationRevealed attrs
      ]

instance RunMessage Montmartre209 where
  runMessage msg a@(Montmartre209 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $
        CreateEffect
          (toCardCode attrs)
          Nothing
          (toSource attrs)
          (InvestigatorTarget iid)
      pure a
    _ -> Montmartre209 <$> runMessage msg attrs

module Arkham.Location.Cards.Kadatheron (kadatheron, Kadatheron (..)) where

import Arkham.Capability
import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype Kadatheron = Kadatheron LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kadatheron :: LocationCard Kadatheron
kadatheron = location Kadatheron Cards.kadatheron 5 (PerPlayer 1)

instance HasAbilities Kadatheron where
  getAbilities (Kadatheron attrs) =
    veiled
      attrs
      [ restrictedAbility attrs 1 (Here <> can.draw.cards You) $ ActionAbility [] $ ActionCost 3
      ]

instance RunMessage Kadatheron where
  runMessage msg l@(Kadatheron attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ drawCards iid (attrs.ability 1) 5
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.cylindersOfKadatheron
      pure . Kadatheron $ attrs & canBeFlippedL .~ False
    _ -> Kadatheron <$> runMessage msg attrs

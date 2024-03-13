module Arkham.Location.Cards.MoonBeastGalley (moonBeastGalley, MoonBeastGalley (..)) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype MoonBeastGalley = MoonBeastGalley LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor MoonBeastGalley where
  getModifiersFor target (MoonBeastGalley attrs) | attrs `is` target = do
    pure $ toModifiers attrs [AdditionalCostToEnter UnpayableCost, AdditionalCostToLeave UnpayableCost]
  getModifiersFor _ _ = pure []

moonBeastGalley :: LocationCard MoonBeastGalley
moonBeastGalley = location MoonBeastGalley Cards.moonBeastGalley 0 (Static 0)

instance HasAbilities MoonBeastGalley where
  getAbilities (MoonBeastGalley attrs) =
    extendRevealed attrs [restrictedAbility attrs 1 (Here <> CluesOnThis (static 0)) $ FastAbility Free]

instance RunMessage MoonBeastGalley where
  runMessage msg l@(MoonBeastGalley attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Flip iid (toSource attrs) (toTarget attrs)
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.offTheGalley
      pure l
    _ -> MoonBeastGalley <$> runMessage msg attrs

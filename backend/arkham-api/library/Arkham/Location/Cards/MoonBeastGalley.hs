module Arkham.Location.Cards.MoonBeastGalley (moonBeastGalley, MoonBeastGalley (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story

newtype MoonBeastGalley = MoonBeastGalley LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor MoonBeastGalley where
  getModifiersFor (MoonBeastGalley a) =
    modifySelf a [AdditionalCostToEnter UnpayableCost, AdditionalCostToLeave UnpayableCost]

moonBeastGalley :: LocationCard MoonBeastGalley
moonBeastGalley = location MoonBeastGalley Cards.moonBeastGalley 3 (Static 1)

instance HasAbilities MoonBeastGalley where
  getAbilities (MoonBeastGalley attrs) =
    extendRevealed
      attrs
      [ restricted attrs 1 (Here <> CluesOnThis (static 0)) $ FastAbility Free
      , restricted attrs 2 (DoomCountIs $ atLeast 3) $ forced $ RoundEnds #when
      ]

instance RunMessage MoonBeastGalley where
  runMessage msg l@(MoonBeastGalley attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid attrs attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid attrs attrs
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.offTheGalley
      pure l
    _ -> MoonBeastGalley <$> liftRunMessage msg attrs

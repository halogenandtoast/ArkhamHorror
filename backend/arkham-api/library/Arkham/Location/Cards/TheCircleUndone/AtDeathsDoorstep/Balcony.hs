module Arkham.Location.Cards.TheCircleUndone.AtDeathsDoorstep.Balcony (balcony) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.CardDefs.TheCircleUndone.AtDeathsDoorstep qualified as Cards
import Arkham.Location.Import.Lifted

newtype Balcony = Balcony LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

balcony :: LocationCard Balcony
balcony =
  location Balcony Cards.balcony 1 (Static 0)

instance HasAbilities Balcony where
  getAbilities (Balcony a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ parleyAction
      $ SkillIconCost 3 (singleton #intellect)

instance RunMessage Balcony where
  runMessage msg l@(Balcony attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainClues iid (attrs.ability 1) 2
      pure l
    _ -> Balcony <$> liftRunMessage msg attrs

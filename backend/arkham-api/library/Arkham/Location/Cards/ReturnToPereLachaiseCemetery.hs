module Arkham.Location.Cards.ReturnToPereLachaiseCemetery (returnToPereLachaiseCemetery) where

import Arkham.Ability
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype ReturnToPereLachaiseCemetery = ReturnToPereLachaiseCemetery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToPereLachaiseCemetery :: LocationCard ReturnToPereLachaiseCemetery
returnToPereLachaiseCemetery = location ReturnToPereLachaiseCemetery Cards.returnToPereLachaiseCemetery 2 (PerPlayer 2)

instance HasAbilities ReturnToPereLachaiseCemetery where
  getAbilities (ReturnToPereLachaiseCemetery a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ EnemyEnters #after (be a) AnyEnemy

instance RunMessage ReturnToPereLachaiseCemetery where
  runMessage msg l@(ReturnToPereLachaiseCemetery attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (enteringEnemy -> enemy) _ -> do
      roundModifier (attrs.ability 1) enemy (HorrorDealt 1)
      pure l
    _ -> ReturnToPereLachaiseCemetery <$> liftRunMessage msg attrs

module Arkham.Location.Cards.PropShop (propShop) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfMaybe)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isInvestigationOf)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype PropShop = PropShop LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

propShop :: LocationCard PropShop
propShop = location PropShop Cards.propShop 7 (PerPlayer 1)

instance HasModifiersFor PropShop where
  getModifiersFor (PropShop a) = modifySelfMaybe a do
    guardM $ isInvestigationOf (be a)
    iid <- MaybeT getSkillTestInvestigator
    n <- field InvestigatorHorror iid
    pure [ShroudModifier (-n) | n > 0]

instance HasAbilities PropShop where
  getAbilities (PropShop a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be a) #failure

instance RunMessage PropShop where
  runMessage msg l@(PropShop attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> PropShop <$> liftRunMessage msg attrs

module Arkham.Location.Cards.ShorewardSlums (shorewardSlums, ShorewardSlums (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype ShorewardSlums = ShorewardSlums LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shorewardSlums :: LocationCard ShorewardSlums
shorewardSlums = location ShorewardSlums Cards.shorewardSlums 5 (PerPlayer 1)

instance HasModifiersFor ShorewardSlums where
  getModifiersFor (ShorewardSlums a) = do
    doom <- field LocationDoom a.id
    modifySelf a [ShroudModifier ((-2) * doom) | doom > 0]

instance HasAbilities ShorewardSlums where
  getAbilities (ShorewardSlums a) =
    extendRevealed1 a $ forcedAbility a 1 $ SkillTestResult #after You (whileInvestigating a) #failure

instance RunMessage ShorewardSlums where
  runMessage msg l@(ShorewardSlums attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure l
    _ -> ShorewardSlums <$> liftRunMessage msg attrs

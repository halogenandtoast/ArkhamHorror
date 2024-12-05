module Arkham.Location.Cards.BurialGround (burialGround, BurialGround (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey (ScenarioLogKey (NoticedTheMissingBones))
import Arkham.Trait (Trait (Ghoul))

newtype BurialGround = BurialGround LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burialGround :: LocationCard BurialGround
burialGround = location BurialGround Cards.burialGround 4 (PerPlayer 1)

instance HasModifiersFor BurialGround where
  getModifiersFor (BurialGround a) =
    whenRevealed a $ modifySelect a (EnemyWithTrait Ghoul) [ForceSpawnLocation (be a)]

instance HasAbilities BurialGround where
  getAbilities (BurialGround a) =
    extendRevealed1 a $ restricted a 1 Here $ FastAbility $ GroupClueCost (PerPlayer 1) (be a)

instance RunMessage BurialGround where
  runMessage msg l@(BurialGround attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember NoticedTheMissingBones
      pure l
    _ -> BurialGround <$> liftRunMessage msg attrs

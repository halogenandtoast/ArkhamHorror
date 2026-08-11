module Arkham.Homebrew.DarkMatter.Enemies.ShadowOfThoughts (shadowOfThoughts) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Types (Field (LocationShroud))
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Projection

newtype ShadowOfThoughts = ShadowOfThoughts EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowOfThoughts :: EnemyCard ShadowOfThoughts
shadowOfThoughts =
  enemy ShadowOfThoughts Cards.shadowOfThoughts
    & setSpawnAt (LocationWithoutInvestigators <> LocationWithoutEnemies)

-- "Retaliate." and the printed fight value is X, the shroud of this enemy's location.
instance HasModifiersFor ShadowOfThoughts where
  getModifiersFor (ShadowOfThoughts a) = do
    shroud <- case a.placement of
      AtLocation lid -> fromMaybe 0 <$> field LocationShroud lid
      _ -> pure 0
    modifySelf a [AddKeyword Keyword.Retaliate, EnemyFight shroud]

{- | "Forced - After this enemy's location is switched with another location:
Move this enemy 1 location towards the nearest investigator."
-}
instance HasAbilities ShadowOfThoughts where
  getAbilities (ShadowOfThoughts a) =
    extend1 a $ mkAbility a 1 $ forced $ ScenarioEvent #after Nothing "switched"

instance RunMessage ShadowOfThoughts where
  runMessage msg e@(ShadowOfThoughts attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      moveTowardsMatching (attrs.ability 1) attrs (LocationWithInvestigator Anyone)
      pure e
    _ -> ShadowOfThoughts <$> liftRunMessage msg attrs

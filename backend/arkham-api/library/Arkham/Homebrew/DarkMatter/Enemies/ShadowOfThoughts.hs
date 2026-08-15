module Arkham.Homebrew.DarkMatter.Enemies.ShadowOfThoughts (shadowOfThoughts) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (switchedWindowFor)
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Types (Field (LocationShroud))
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Projection

newtype ShadowOfThoughts = ShadowOfThoughts EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowOfThoughts :: EnemyCard ShadowOfThoughts
shadowOfThoughts =
  enemy ShadowOfThoughts Cards.shadowOfThoughts
    & setSpawnAt (LocationWithoutInvestigators <> LocationWithoutEnemies)

instance HasModifiersFor ShadowOfThoughts where
  getModifiersFor (ShadowOfThoughts a) = do
    shroud <- runDefaultMaybeT 0 do
      lid <- MaybeT $ getLocationOf a
      MaybeT $ field LocationShroud lid
    modifySelf a [AddKeyword Keyword.Retaliate, EnemyFight shroud]

instance HasAbilities ShadowOfThoughts where
  getAbilities (ShadowOfThoughts a) =
    extend
      a
      [ restricted a 1 (thisExists a $ not_ (EnemyAt $ LocationWithInvestigator Anyone)) $ forced w
      | w <- toList (switchedWindowFor a.placement)
      ]

instance RunMessage ShadowOfThoughts where
  runMessage msg e@(ShadowOfThoughts attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      moveTowardsMatching (attrs.ability 1) attrs (LocationWithInvestigator Anyone)
      pure e
    _ -> ShadowOfThoughts <$> liftRunMessage msg attrs

module Arkham.Homebrew.DarkMatter.Enemies.TatteredCurtains (tatteredCurtains) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern Carcosa)
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype TatteredCurtains = TatteredCurtains EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "Spawn - Nearest [[Carcosa]] location."
tatteredCurtains :: EnemyCard TatteredCurtains
tatteredCurtains =
  enemy TatteredCurtains Cards.tatteredCurtains
    & setSpawnAt (NearestLocationToYou $ LocationWithTrait Carcosa)

-- | "Does not attack during the enemy phase."
instance HasModifiersFor TatteredCurtains where
  getModifiersFor (TatteredCurtains a) = modifySelf a [CannotAttack]

{- | "Forced - After you flip Tattered Curtains's location: Move it one location
towards you."
-}
instance HasAbilities TatteredCurtains where
  getAbilities (TatteredCurtains a) =
    extend1 a $ mkAbility a 1 $ forced $ FlipLocation #after You (locationWithEnemy a.id)

instance RunMessage TatteredCurtains where
  runMessage msg e@(TatteredCurtains attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTowardsMatching (attrs.ability 1) attrs (locationWithInvestigator iid)
      pure e
    _ -> TatteredCurtains <$> liftRunMessage msg attrs

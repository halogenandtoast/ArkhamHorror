module Arkham.Homebrew.DarkMatter.Enemies.ViciousByakhee (viciousByakhee) where

import Arkham.Ability
import Arkham.DamageEffect
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Brain, MiGo))

newtype ViciousByakhee = ViciousByakhee EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

viciousByakhee :: EnemyCard ViciousByakhee
viciousByakhee =
  enemy ViciousByakhee Cards.viciousByakhee
    & setSpawnAt (LocationWithTitle "Entrance Tunnel")

instance HasModifiersFor ViciousByakhee where
  getModifiersFor (ViciousByakhee a) =
    modifySelf a [AddKeyword $ Keyword.Patrol (LocationWithAsset (AssetWithTrait Brain))]

instance HasAbilities ViciousByakhee where
  getAbilities (ViciousByakhee a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy)
      $ forced
      $ PhaseBegins #when #enemy

instance RunMessage ViciousByakhee where
  runMessage msg e@(ViciousByakhee attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      migos <- select $ EnemyWithTrait MiGo <> EnemyAt (locationWithEnemy attrs)
      for_ migos $ nonAttackEnemyDamage_ Nothing (attrs.ability 1) 2
      brains <- select $ AssetWithTrait Brain <> AssetAt (locationWithEnemy attrs)
      for_ brains \aid ->
        push $ DealDamage (toTarget aid) (nonAttack Nothing (attrs.ability 1) 2)
      pure e
    _ -> ViciousByakhee <$> liftRunMessage msg attrs

module Arkham.Enemy.Cards.ViciousByakheeDarkMatter (viciousByakheeDarkMatter) where

import Arkham.Ability
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Brain, MiGo))

newtype ViciousByakheeDarkMatter = ViciousByakheeDarkMatter EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

viciousByakheeDarkMatter :: EnemyCard ViciousByakheeDarkMatter
viciousByakheeDarkMatter =
  enemy ViciousByakheeDarkMatter Cards.viciousByakheeDarkMatter
    & setSpawnAt (LocationWithTitle "Entrance Tunnel")

instance HasModifiersFor ViciousByakheeDarkMatter where
  getModifiersFor (ViciousByakheeDarkMatter a) =
    modifySelf a [AddKeyword $ Keyword.Patrol (LocationWithAsset (AssetWithTrait Brain))]

instance HasAbilities ViciousByakheeDarkMatter where
  getAbilities (ViciousByakheeDarkMatter a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy)
      $ forced
      $ PhaseBegins #when #enemy

instance RunMessage ViciousByakheeDarkMatter where
  runMessage msg e@(ViciousByakheeDarkMatter attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      migos <- select $ EnemyWithTrait MiGo <> EnemyAt (locationWithEnemy attrs)
      for_ migos $ nonAttackEnemyDamage_ Nothing (attrs.ability 1) 2
      brains <- select $ AssetWithTrait Brain <> AssetAt (locationWithEnemy attrs)
      for_ brains \aid ->
        push $ DealDamage (toTarget aid) (nonAttack Nothing (attrs.ability 1) 2)
      pure e
    _ -> ViciousByakheeDarkMatter <$> liftRunMessage msg attrs

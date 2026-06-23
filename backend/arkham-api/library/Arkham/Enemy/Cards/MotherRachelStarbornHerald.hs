module Arkham.Enemy.Cards.MotherRachelStarbornHerald (motherRachelStarbornHerald) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Resident))

newtype MotherRachelStarbornHerald = MotherRachelStarbornHerald EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

motherRachelStarbornHerald :: EnemyCard MotherRachelStarbornHerald
motherRachelStarbornHerald =
  enemy MotherRachelStarbornHerald Cards.motherRachelStarbornHerald

instance HasModifiersFor MotherRachelStarbornHerald where
  getModifiersFor (MotherRachelStarbornHerald a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance HasAbilities MotherRachelStarbornHerald where
  getAbilities (MotherRachelStarbornHerald a) =
    extend1 a
      $ restricted a 1 (exists $ EnemyAt (locationWithEnemy a) <> not_ (be a))
      $ forced
      $ EnemyDealtDamage #after AnyDamageEffect (be a) (SourceUsedBy You)

instance RunMessage MotherRachelStarbornHerald where
  runMessage msg e@(MotherRachelStarbornHerald attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      others <-
        select
          $ EnemyAt (locationWithEnemy attrs)
          <> withTrait Resident
          <> not_ (be attrs)
      chooseTargetM iid others \other ->
        initiateEnemyAttack other (attrs.ability 1) iid
      pure e
    _ -> MotherRachelStarbornHerald <$> liftRunMessage msg attrs

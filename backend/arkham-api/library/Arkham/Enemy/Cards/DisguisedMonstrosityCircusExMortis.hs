module Arkham.Enemy.Cards.DisguisedMonstrosityCircusExMortis (disguisedMonstrosityCircusExMortis) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype DisguisedMonstrosityCircusExMortis = DisguisedMonstrosityCircusExMortis EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disguisedMonstrosityCircusExMortis :: EnemyCard DisguisedMonstrosityCircusExMortis
disguisedMonstrosityCircusExMortis = enemy DisguisedMonstrosityCircusExMortis Cards.disguisedMonstrosityCircusExMortis

instance HasModifiersFor DisguisedMonstrosityCircusExMortis where
  getModifiersFor (DisguisedMonstrosityCircusExMortis a) = do
    modifySelf a [AddKeyword Keyword.Hunter, AddKeyword Keyword.Massive]
    exposed <- selectAny $ locationWithEnemy a <> RevealedLocation <> LocationWithoutClues
    when exposed $ modifySelf a [EnemyFight (-1), DamageTakenFrom AttackDamageEffect 1]

instance HasAbilities DisguisedMonstrosityCircusExMortis where
  getAbilities (DisguisedMonstrosityCircusExMortis a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyMoves #after (RevealedLocation <> LocationWithoutClues) (be a)

instance RunMessage DisguisedMonstrosityCircusExMortis where
  runMessage msg e@(DisguisedMonstrosityCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      exhaustThis attrs
      pure e
    _ -> DisguisedMonstrosityCircusExMortis <$> liftRunMessage msg attrs

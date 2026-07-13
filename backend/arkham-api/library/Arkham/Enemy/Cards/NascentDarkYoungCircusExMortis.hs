module Arkham.Enemy.Cards.NascentDarkYoungCircusExMortis (nascentDarkYoungCircusExMortis) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Window (getTotalDamage)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype NascentDarkYoungCircusExMortis = NascentDarkYoungCircusExMortis EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nascentDarkYoungCircusExMortis :: EnemyCard NascentDarkYoungCircusExMortis
nascentDarkYoungCircusExMortis =
  enemy NascentDarkYoungCircusExMortis Cards.nascentDarkYoungCircusExMortis

instance HasModifiersFor NascentDarkYoungCircusExMortis where
  getModifiersFor (NascentDarkYoungCircusExMortis a) = modifySelf a [AddKeyword Keyword.Hunter]

instance HasAbilities NascentDarkYoungCircusExMortis where
  getAbilities (NascentDarkYoungCircusExMortis a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyDealtDamage #when AnyDamageEffect (be a) (SourceUsedBy $ InvestigatorWithSealedChaosToken #moon)

instance RunMessage NascentDarkYoungCircusExMortis where
  runMessage msg e@(NascentDarkYoungCircusExMortis attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getTotalDamage -> n) _ -> do
      -- "to a minimum of 1": only reduce when the incoming damage is 2+
      when (n >= 2) $ reduceDamageTaken (attrs.ability 1) attrs 1
      pure e
    _ -> NascentDarkYoungCircusExMortis <$> liftRunMessage msg attrs

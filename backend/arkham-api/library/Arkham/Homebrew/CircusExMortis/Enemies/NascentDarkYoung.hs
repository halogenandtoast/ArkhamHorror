module Arkham.Homebrew.CircusExMortis.Enemies.NascentDarkYoung (nascentDarkYoung) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Window (getTotalDamage)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype NascentDarkYoung = NascentDarkYoung EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nascentDarkYoung :: EnemyCard NascentDarkYoung
nascentDarkYoung =
  enemy NascentDarkYoung Cards.nascentDarkYoung

instance HasModifiersFor NascentDarkYoung where
  getModifiersFor (NascentDarkYoung a) = modifySelf a [AddKeyword Keyword.Hunter]

instance HasAbilities NascentDarkYoung where
  getAbilities (NascentDarkYoung a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyDealtDamage #when AnyDamageEffect (be a) (SourceUsedBy $ InvestigatorWithSealedChaosToken (ChaosTokenFaceIs MoonToken))

instance RunMessage NascentDarkYoung where
  runMessage msg e@(NascentDarkYoung attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getTotalDamage -> n) _ -> do
      -- "to a minimum of 1": only reduce when the incoming damage is 2+
      when (n >= 2) $ reduceDamageTaken (attrs.ability 1) attrs 1
      pure e
    _ -> NascentDarkYoung <$> liftRunMessage msg attrs

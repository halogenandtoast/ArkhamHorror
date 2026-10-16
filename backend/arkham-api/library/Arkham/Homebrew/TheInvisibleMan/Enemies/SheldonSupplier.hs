module Arkham.Homebrew.TheInvisibleMan.Enemies.SheldonSupplier (sheldonSupplier) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (modifySelect)
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Matcher

newtype SheldonSupplier = SheldonSupplier EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sheldonSupplier :: EnemyCard SheldonSupplier
sheldonSupplier =
  enemy SheldonSupplier Cards.sheldonSupplier
    & setSpawnAt LocationWithoutInvestigators

{- | "When choosing a location for Griffin's forced effect, you must choose a
location with a Sheldon Supplier (whether or not there are investigators
there), if able." A constant effect with no printed ability, so he marks his own
location and 'griffinMoves' reads the mark.
-}
instance HasModifiersFor SheldonSupplier where
  getModifiersFor (SheldonSupplier a) = modifySelect a (locationWithEnemy a) [griffinMustEnter]

instance HasAbilities SheldonSupplier where
  getAbilities (SheldonSupplier a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "sheldonSupplier.resupply"
      $ mkAbility a 1
      $ forced
      $ EnemyEnters #after (locationWithEnemy a) griffin

instance RunMessage SheldonSupplier where
  runMessage msg e@(SheldonSupplier attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach griffin \eid -> placeDoom (attrs.ability 1) eid 1
      pure e
    _ -> SheldonSupplier <$> liftRunMessage msg attrs

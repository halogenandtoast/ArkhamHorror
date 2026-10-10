module Arkham.Homebrew.TheMasqueOfTheRedDeath.Enemies.MaddenedReveler (maddenedReveler) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (EnemyFight), maybeModifySelf)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (countSkullEffectsOn)
import Arkham.Matcher

newtype MaddenedReveler = MaddenedReveler EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

maddenedReveler :: EnemyCard MaddenedReveler
maddenedReveler = enemy MaddenedReveler Cards.maddenedReveler

instance HasModifiersFor MaddenedReveler where
  -- "While Maddened Reveler is ready, it gets +X fight, where X is the number of
  -- [skull] effects on its location."
  getModifiersFor (MaddenedReveler a) = maybeModifySelf a do
    guard a.ready
    lid <- MaybeT $ selectOne $ locationWithEnemy a.id
    n <- lift $ countSkullEffectsOn lid
    pure [EnemyFight n]

instance RunMessage MaddenedReveler where
  runMessage msg (MaddenedReveler attrs) = runQueueT $ MaddenedReveler <$> liftRunMessage msg attrs

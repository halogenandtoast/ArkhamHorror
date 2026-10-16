module Arkham.Homebrew.TheLegendOfSleepyHollow.Enemies.DecapitatedVictim (decapitatedVictim) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Placement

newtype DecapitatedVictim = DecapitatedVictim EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decapitatedVictim :: EnemyCard DecapitatedVictim
decapitatedVictim = enemy DecapitatedVictim Cards.decapitatedVictim

-- | "If there is doom on Decapitated Victim's location, it cannot be engaged."
instance HasModifiersFor DecapitatedVictim where
  getModifiersFor (DecapitatedVictim a) = case a.placement of
    AtLocation lid -> do
      doomed <- locationIsDoomed lid
      modifySelfWhen a doomed [CannotBeEngaged]
    _ -> pure mempty

{- | "Decapitated Victim's location gains \"Haunted - Take 1 horror.\"" The
ability is proxied onto the location, which is what makes it a haunted ability
*there* for the failed-investigate resolution.
-}
instance HasAbilities DecapitatedVictim where
  getAbilities (DecapitatedVictim a) = extend a $ case a.placement of
    AtLocation lid ->
      [scenarioI18n $ hauntedI "decapitatedVictim.haunted" (proxied (LocationSource lid) a) 1]
    _ -> []

instance RunMessage DecapitatedVictim where
  runMessage msg e@(DecapitatedVictim attrs) = runQueueT $ case msg of
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> DecapitatedVictim <$> liftRunMessage msg attrs

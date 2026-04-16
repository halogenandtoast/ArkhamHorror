module Arkham.Location.Cards.MoonlitClearing (moonlitClearing) where

import Arkham.Ability
import Arkham.Helpers.Enemy
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheTwistedHollow.Helpers
import Arkham.Spawn

newtype MoonlitClearing = MoonlitClearing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlitClearing :: LocationCard MoonlitClearing
moonlitClearing = locationWith MoonlitClearing Cards.moonlitClearing 1 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor MoonlitClearing where
  getModifiersFor (MoonlitClearing a) = whenRevealed a do
    modifySelect
      a
      (LocationWithEnemy MovingEnemy)
      [ConnectedToWhen Anywhere (be a <> LocationWithInvestigator Anyone)]

instance HasAbilities MoonlitClearing where
  getAbilities (MoonlitClearing a) =
    extendRevealed a [mkAbility a 1 $ forced $ RevealLocation #after You $ be a]

instance RunMessage MoonlitClearing where
  runMessage msg l@(MoonlitClearing attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      x <- getDarknessLevel
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed x)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      enemies <- pursuitEnemiesWithHighestFight
      chooseTargetM iid enemies \e -> spawnAt e Nothing (SpawnAtLocation attrs.id)
      pure l
    _ -> MoonlitClearing <$> liftRunMessage msg attrs

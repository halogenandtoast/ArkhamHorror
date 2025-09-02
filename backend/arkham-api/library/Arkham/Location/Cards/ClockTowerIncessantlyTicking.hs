module Arkham.Location.Cards.ClockTowerIncessantlyTicking (clockTowerIncessantlyTicking) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.FilmFatale.Helpers

newtype ClockTowerIncessantlyTicking = ClockTowerIncessantlyTicking LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clockTowerIncessantlyTicking :: LocationCard ClockTowerIncessantlyTicking
clockTowerIncessantlyTicking = locationWith ClockTowerIncessantlyTicking Cards.clockTowerIncessantlyTicking 2 (PerPlayer 1) (investigateSkillL .~ #agility)

instance HasAbilities ClockTowerIncessantlyTicking where
  getAbilities (ClockTowerIncessantlyTicking a) =
    extend1 a
      $ if a.revealed
        then scenarioI18n $ hauntedI "clockTower.haunted" a 2
        else
          restricted a 1 (exists $ enemyIs Enemies.theContessaNeedlesslySmug <> EnemyCanMove)
            $ forced
            $ UnrevealedRevealLocation #when You (be a)

instance RunMessage ClockTowerIncessantlyTicking where
  runMessage msg l@(ClockTowerIncessantlyTicking attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure l
    FailedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      moveContessa (attrs.ability 1) attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      hasCards <-
        selectAny
          $ inHandOf NotForPlay iid
          <> basic (DiscardableCard <> mapOneOf CardWithSkillIcon [#agility, #wild])
      if hasCards
        then chooseAndDiscardCardEdit iid (attrs.ability 2) \d -> d {discardFilter = mapOneOf CardWithSkillIcon [#agility, #wild]}
        else assignDamage iid (attrs.ability 2) 2
      pure l
    _ -> ClockTowerIncessantlyTicking <$> liftRunMessage msg attrs

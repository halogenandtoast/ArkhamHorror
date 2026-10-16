module Arkham.Homebrew.TheLegendOfSleepyHollow.Locations.WhitePlains (whitePlains) where

import Arkham.Ability
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (remember)
import Arkham.Trait (Trait (Geist))

newtype WhitePlains = WhitePlains LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whitePlains :: LocationCard WhitePlains
whitePlains = location WhitePlains Cards.whitePlains 4 (PerPlayer 1)

instance HasAbilities WhitePlains where
  getAbilities (WhitePlains a) =
    extendRevealed
      a
      [ scenarioI18n $ hauntedI "whitePlains.haunted" a 1
      , scenarioI18n
          $ withI18nTooltip "whitePlains.spawn"
          $ mkAbility a 2
          $ forced
          $ RevealLocation #after Anyone (be a)
      , scenarioI18n
          $ withI18nTooltip "whitePlains.skull"
          $ skillTestAbility
          $ restricted a 3 Here actionAbility
      ]

instance RunMessage WhitePlains where
  runMessage msg l@(WhitePlains attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- "2 enemies instead if there are 3 or 4 players"
      n <- getPlayerCount
      repeated (if n >= 3 then 2 else 1)
        $ findEncounterCard iid attrs (#enemy <> CardWithTrait Geist)
      pure l
    FoundEncounterCard _ (isTarget attrs -> True) card -> do
      spawnEnemyAt_ card attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      geists <- selectCount $ EnemyWithTrait Geist <> enemyAt attrs.id
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 3) attrs [#combat, #agility] (Fixed $ 4 + 2 * geists)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 3 -> True) -> do
      remember recoveredTheHorsemansSkull
      pure l
    _ -> WhitePlains <$> liftRunMessage msg attrs

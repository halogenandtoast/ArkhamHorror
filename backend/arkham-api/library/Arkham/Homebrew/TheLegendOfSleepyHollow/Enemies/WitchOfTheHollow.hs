module Arkham.Homebrew.TheLegendOfSleepyHollow.Enemies.WitchOfTheHollow (witchOfTheHollow) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Matcher
import Arkham.Placement
import Arkham.Trait (Trait (Curse))

newtype WitchOfTheHollow = WitchOfTheHollow EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchOfTheHollow :: EnemyCard WitchOfTheHollow
witchOfTheHollow = enemy WitchOfTheHollow Cards.witchOfTheHollow

instance HasAbilities WitchOfTheHollow where
  getAbilities (WitchOfTheHollow a) =
    extend
      a
      [ scenarioI18n
          $ withI18nTooltip "witchOfTheHollow.doom"
          $ mkAbility a 1
          $ forced
          $ RoundEnds #when
      , scenarioI18n
          $ withI18nTooltip "witchOfTheHollow.curse"
          $ restricted a 2 (notExists $ TreacheryWithTrait Curse <> TreacheryInThreatAreaOf You)
          $ forced
          $ EnemyAttacks #after You AnyEnemyAttack (be a)
      ]

instance RunMessage WitchOfTheHollow where
  runMessage msg e@(WitchOfTheHollow attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      findEncounterCard iid attrs (#treachery <> CardWithTrait Curse)
      pure e
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) -> do
      createTreacheryAt_ card (InThreatArea iid)
      pure e
    _ -> WitchOfTheHollow <$> liftRunMessage msg attrs

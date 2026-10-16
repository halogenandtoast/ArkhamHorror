{- | Fleshy Creation never reaches the encounter discard: each copy you defeat
goes beside the agenda deck instead, where it hardens the copies still on the
board until all three have accumulated and the set is purged.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Enemies.FleshyCreation (fleshyCreation) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Enemy (insteadOfDiscarding)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen, modifySelfWhen)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype FleshyCreation = FleshyCreation EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fleshyCreation :: EnemyCard FleshyCreation
fleshyCreation = enemy FleshyCreation Cards.fleshyCreation

besideTheAgendaDeck :: EnemyMatcher
besideTheAgendaDeck = enemyIs Cards.fleshyCreation <> EnemyWithPlacement NextToAgenda

{- | "While Fleshy Creation is next to the agenda deck, each other copy of Fleshy
Creation gets +1 fight and +1 evade. Cards cannot be attached to Fleshy Creation
while it is next to the agenda deck."

Each copy publishes its own buff for the *other* copies, so two beside the deck
stack to +2/+2 on the third -- "each other copy", read once from each of them.
-}
instance HasModifiersFor FleshyCreation where
  getModifiersFor (FleshyCreation a) = do
    let beside = a.placement == NextToAgenda
    modifySelfWhen a beside [CannotHaveAttachments]
    modifySelectWhen
      a
      beside
      (enemyIs Cards.fleshyCreation <> not_ (be a))
      [EnemyFight 1, EnemyEvade 1]

instance HasAbilities FleshyCreation where
  getAbilities (FleshyCreation a) =
    extend
      a
      [ scenarioI18n
          $ withI18nTooltip "fleshyCreation.return"
          $ mkAbility a 1
          $ forced
          $ EnemyDefeated #when You ByAny (be a)
      , scenarioI18n
          $ withI18nTooltip "fleshyCreation.purge"
          $ restricted a 2 (EnemyCount (atLeast 3) besideTheAgendaDeck)
          $ forced
          $ EnemyPlaced #after NextToAgenda (be a)
      ]

instance RunMessage FleshyCreation where
  runMessage msg e@(FleshyCreation attrs) = runQueueT $ case msg of
    -- Only the trip to the discard pile is replaced: insteadOfDiscarding drops the
    -- queued Defeated/Discard and re-raises the after-defeat windows, so "after you
    -- defeat an enemy" effects still see it.
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      insteadOfDiscarding attrs do
        healAllDamage (attrs.ability 1) attrs
        place attrs NextToAgenda
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      selectEach besideTheAgendaDeck (toDiscard (attrs.ability 2))
      pure e
    _ -> FleshyCreation <$> liftRunMessage msg attrs

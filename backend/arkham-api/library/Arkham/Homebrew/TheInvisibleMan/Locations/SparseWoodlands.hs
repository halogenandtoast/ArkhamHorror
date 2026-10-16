{- | Sparse Woodlands -- thin cover, and something already waiting in it.

The search half is a real __Forced__ on the investigation's result window.
"Shuffle the encounter deck if it was searched" needs nothing of its own:
'findEncounterCard' searches deck and discard and shuffles the deck behind
itself.
-}
module Arkham.Homebrew.TheInvisibleMan.Locations.SparseWoodlands (sparseWoodlands) where

import Arkham.Ability
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Creature))

newtype SparseWoodlands = SparseWoodlands LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sparseWoodlands :: LocationCard SparseWoodlands
sparseWoodlands = location SparseWoodlands Cards.sparseWoodlands 1 (PerPlayer 1)

instance HasAbilities SparseWoodlands where
  getAbilities (SparseWoodlands a) =
    extendRevealed
      a
      [ scenarioI18n
          $ withI18nTooltip "sparseWoodlands.lead"
          $ onlyOnce
          $ restricted a 1 (Here <> exists (ExhaustedEnemy <> withTrait Creature <> enemyAt a.id))
          $ FastAbility Free
      , scenarioI18n
          $ withI18nTooltip "sparseWoodlands.forced"
          $ groupLimit PerTurn
          $ mkAbility a 2
          $ forced
          $ SkillTestResult #after You (WhileInvestigating $ be a) (SuccessResult $ atLeast 3)
      ]

instance RunMessage SparseWoodlands where
  runMessage msg l@(SparseWoodlands attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeLead (attrs.ability 1) attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      findEncounterCard iid attrs (#enemy <> CardWithTrait Creature)
      pure l
    FoundEncounterCard _ (isTarget attrs -> True) card -> do
      spawnEnemyAt_ card attrs
      pure l
    _ -> SparseWoodlands <$> liftRunMessage msg attrs

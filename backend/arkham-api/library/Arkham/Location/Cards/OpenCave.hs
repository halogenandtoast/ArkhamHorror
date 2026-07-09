module Arkham.Location.Cards.OpenCave (openCave) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Phase
import Arkham.Trait (Trait (Dark))

newtype OpenCave = OpenCave LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openCave :: LocationCard OpenCave
openCave = locationWith OpenCave Cards.openCave 3 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor OpenCave where
  getModifiersFor (OpenCave a) = do
    modifySelfWhen a (getLocationMetaDefault False a) [RemoveTrait Dark]

instance HasAbilities OpenCave where
  getAbilities (OpenCave a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ skillTestAbility
      $ restricted a 1 (Here <> DuringTurn You)
      $ FastAbility Free

instance RunMessage OpenCave where
  runMessage msg l@(OpenCave attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      let difficulty = SumCalculation [Fixed 2, CountEnemies (ReadyEnemy <> enemyAt attrs)]
      beginSkillTest sid iid (attrs.ability 1) attrs #agility difficulty
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      push $ ScenarioSpecific "syncHybridDarkness" (toJSON attrs.id)
      pure $ OpenCave $ attrs & setMeta True
    Begin InvestigationPhase -> do
      when (getLocationMetaDefault False attrs) do
        push $ ScenarioSpecific "syncHybridDarkness" (toJSON attrs.id)
      OpenCave . setMeta @Bool False <$> liftRunMessage msg attrs
    _ -> OpenCave <$> liftRunMessage msg attrs

module Arkham.Location.Cards.CityWhichAppearsOnNoMap (
  cityWhichAppearsOnNoMap,
  CityWhichAppearsOnNoMap (..),
) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story

newtype CityWhichAppearsOnNoMap = CityWhichAppearsOnNoMap LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityWhichAppearsOnNoMap :: LocationCard CityWhichAppearsOnNoMap
cityWhichAppearsOnNoMap = location CityWhichAppearsOnNoMap Cards.cityWhichAppearsOnNoMap 6 (PerPlayer 2)

instance HasModifiersFor CityWhichAppearsOnNoMap where
  getModifiersFor (CityWhichAppearsOnNoMap a) = whenRevealed a $ maybeModifySelf a do
    n <- selectCount $ enemyAt a <> enemyIs Enemies.priestOfAThousandMasks
    pure [ShroudModifier (-n) | n > 0]

instance HasAbilities CityWhichAppearsOnNoMap where
  getAbilities (CityWhichAppearsOnNoMap attrs) =
    veiled1 attrs
      $ mkAbility attrs 1
      $ forced
      $ SkillTestResult #after You (whileInvestigating attrs) #failure

instance RunMessage CityWhichAppearsOnNoMap where
  runMessage msg l@(CityWhichAppearsOnNoMap attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theBalefulStar
      pure . CityWhichAppearsOnNoMap $ attrs & canBeFlippedL .~ False
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findEncounterCard iid attrs $ cardIs Enemies.priestOfAThousandMasks
      pure l
    FoundEncounterCard _iid (isTarget attrs -> True) card -> do
      spawnEnemyAt_ card attrs
      pure l
    _ -> CityWhichAppearsOnNoMap <$> liftRunMessage msg attrs

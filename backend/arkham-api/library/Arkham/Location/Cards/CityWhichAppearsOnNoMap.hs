module Arkham.Location.Cards.CityWhichAppearsOnNoMap (cityWhichAppearsOnNoMap, CityWhichAppearsOnNoMap (..)) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype CityWhichAppearsOnNoMap = CityWhichAppearsOnNoMap LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityWhichAppearsOnNoMap :: LocationCard CityWhichAppearsOnNoMap
cityWhichAppearsOnNoMap = location CityWhichAppearsOnNoMap Cards.cityWhichAppearsOnNoMap 6 (PerPlayer 2)

instance HasModifiersFor CityWhichAppearsOnNoMap where
  getModifiersFor target (CityWhichAppearsOnNoMap attrs) | attrs `is` target = do
    n <- selectCount $ enemyAt (toId attrs) <> enemyIs Enemies.priestOfAThousandMasks
    pure $ toModifiers attrs [ShroudModifier (-n) | n > 0]
  getModifiersFor _ _ = pure []

instance HasAbilities CityWhichAppearsOnNoMap where
  getAbilities (CityWhichAppearsOnNoMap attrs) =
    veiled
      attrs
      [ mkAbility attrs 1
          $ forced
          $ SkillTestResult #after You (WhileInvestigating $ be attrs) (FailureResult AnyValue)
      ]

instance RunMessage CityWhichAppearsOnNoMap where
  runMessage msg l@(CityWhichAppearsOnNoMap attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theBalefulStar
      pure . CityWhichAppearsOnNoMap $ attrs & canBeFlippedL .~ False
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push
        $ FindEncounterCard iid (toTarget attrs) [FromEncounterDeck, FromEncounterDiscard]
        $ cardIs Enemies.priestOfAThousandMasks
      pure l
    FoundEncounterCard _iid (isTarget attrs -> True) card -> do
      push $ SpawnEnemyAt (EncounterCard card) (toId attrs)
      pure l
    _ -> CityWhichAppearsOnNoMap <$> runMessage msg attrs

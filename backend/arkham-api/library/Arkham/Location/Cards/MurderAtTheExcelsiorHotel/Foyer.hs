module Arkham.Location.Cards.MurderAtTheExcelsiorHotel.Foyer (foyer) where

import Arkham.GameValue
import Arkham.Helpers.Cost (getCanAffordCost)
import Arkham.Location.CardDefs.MurderAtTheExcelsiorHotel qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenarios.MurderAtTheExcelsiorHotel.Helpers
import Arkham.Trait (Trait (Guest))
import Arkham.Window (getBatchId)

newtype Foyer = Foyer LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foyer :: LocationCard Foyer
foyer = location Foyer Cards.foyer 2 (PerPlayer 1)

instance HasAbilities Foyer where
  getAbilities (Foyer attrs) =
    withRevealedAbilities
      attrs
      [ scenarioI18n $ withI18nTooltip "foyer.resign" $ locationResignAction attrs
      , skillTestAbility
          $ restricted attrs 1 (exists $ enemyAt (toId attrs) <> EnemyWithTrait Guest)
          $ forced (Leaves #when You $ be attrs)
      ]

instance RunMessage Foyer where
  runMessage msg l@(Foyer attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      sid <- getRandom
      push
        $ beginSkillTest
          sid
          iid
          (attrs.ability 1)
          (BatchTarget batchId)
          #agility
        $ CountEnemies (enemyAt (toId attrs) <> EnemyWithTrait Guest)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      mtarget <- getSkillTestTarget
      case mtarget of
        Just (BatchTarget batchId) -> do
          canMove <- getCanAffordCost iid (toAbilitySource attrs 1) [] [] (ActionCost 1)
          if canMove
            then push $ SpendActions iid (toAbilitySource attrs 1) [] 1
            else push $ CancelBatch batchId
        _ -> error "invalid target, must be batch"
      pure l
    _ -> Foyer <$> runMessage msg attrs

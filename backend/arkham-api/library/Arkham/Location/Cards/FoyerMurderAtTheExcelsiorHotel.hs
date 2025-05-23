module Arkham.Location.Cards.FoyerMurderAtTheExcelsiorHotel (foyerMurderAtTheExcelsiorHotel) where

import Arkham.GameValue
import Arkham.Helpers.Cost (getCanAffordCost)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Guest))
import Arkham.Window (getBatchId)

newtype FoyerMurderAtTheExcelsiorHotel = FoyerMurderAtTheExcelsiorHotel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foyerMurderAtTheExcelsiorHotel :: LocationCard FoyerMurderAtTheExcelsiorHotel
foyerMurderAtTheExcelsiorHotel = location FoyerMurderAtTheExcelsiorHotel Cards.foyerMurderAtTheExcelsiorHotel 2 (PerPlayer 1)

instance HasAbilities FoyerMurderAtTheExcelsiorHotel where
  getAbilities (FoyerMurderAtTheExcelsiorHotel attrs) =
    withRevealedAbilities
      attrs
      [ withTooltip " You flee the scene of the crime." $ locationResignAction attrs
      , skillTestAbility
          $ restricted attrs 1 (exists $ enemyAt (toId attrs) <> EnemyWithTrait Guest)
          $ forced (Leaves #when You $ be attrs)
      ]

instance RunMessage FoyerMurderAtTheExcelsiorHotel where
  runMessage msg l@(FoyerMurderAtTheExcelsiorHotel attrs) = case msg of
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
    _ -> FoyerMurderAtTheExcelsiorHotel <$> runMessage msg attrs

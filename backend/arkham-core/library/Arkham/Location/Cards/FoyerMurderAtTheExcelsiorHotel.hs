module Arkham.Location.Cards.FoyerMurderAtTheExcelsiorHotel (
  foyerMurderAtTheExcelsiorHotel,
  FoyerMurderAtTheExcelsiorHotel (..),
)
where

import Arkham.Prelude

import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
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
      , restrictedAbility attrs 1 (exists $ enemyAt (toId attrs) <> EnemyWithTrait Guest)
          $ ForcedAbility (Leaves #when You $ LocationWithId $ toId attrs)
      ]

instance RunMessage FoyerMurderAtTheExcelsiorHotel where
  runMessage msg l@(FoyerMurderAtTheExcelsiorHotel attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      push
        $ beginSkillTest
          iid
          (toAbilitySource attrs 1)
          (BatchTarget batchId)
          #agility
          (EnemyCountDifficulty $ enemyAt (toId attrs) <> EnemyWithTrait Guest)
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

module Arkham.Location.Cards.RealmsBeyondAllInOne (realmsBeyondAllInOne) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Movement
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Movement

newtype RealmsBeyondAllInOne = RealmsBeyondAllInOne LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realmsBeyondAllInOne :: LocationCard RealmsBeyondAllInOne
realmsBeyondAllInOne = location RealmsBeyondAllInOne Cards.realmsBeyondAllInOne 9 (Static 0)

instance HasModifiersFor RealmsBeyondAllInOne where
  getModifiersFor (RealmsBeyondAllInOne a) = do
    modifySelect
      a
      (enemyIs Enemies.yogSothoth)
      [ CannotBeDamaged
      , CannotMove
      , CannotBeMoved
      , CannotBeAttackedByPlayerSourcesExcept (NotSource SourceIsPlayerCard)
      ]
    modifySelf
      a
      [ AdditionalCostToLeave
          $ OrCost
            [ SkillTestCost (toSource a) #willpower (Fixed 3)
            , SkillTestCost (toSource a) #agility (Fixed 3)
            ]
      ]

instance HasAbilities RealmsBeyondAllInOne where
  getAbilities (RealmsBeyondAllInOne a) =
    extendRevealed1 a
      $ restricted a 1 criteria
      $ forced
      $ WouldBeMovedBy #when You
      $ SourceIsLocation (locationIs Cards.anotherDimension)
      where
        criteria = if locationBeingRemoved a then Never else NoRestriction

instance RunMessage RealmsBeyondAllInOne where
  runMessage msg l@(RealmsBeyondAllInOne attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      replaceMovement iid \movement -> movement {moveDestination = ToLocation attrs.id, moveCancelable = False}
      pure l
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      cancelMovement attrs iid
      pure l
    _ -> RealmsBeyondAllInOne <$> liftRunMessage msg attrs

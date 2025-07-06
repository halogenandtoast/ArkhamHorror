module Arkham.Location.Cards.RealmsBeyondAllInOne (realmsBeyondAllInOne) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Movement
import Arkham.Window (getBatchId)
import Arkham.Window qualified as Window

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
      $ mkAbility a 1
      $ forced
      $ MovedBy #when You
      $ SourceIsLocation (locationIs Cards.anotherDimension)

instance RunMessage RealmsBeyondAllInOne where
  runMessage msg l@(RealmsBeyondAllInOne attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> bId) _ -> do
      cancelBatch bId
      -- should we get the location leaving play here
      withBatchedTimings (Window.Moves iid (attrs.ability 1) Nothing attrs.id) do
        moveToEdit (attrs.ability 1) iid attrs.id uncancellableMove
      pure l
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      cancelMovement attrs iid
      pure l
    _ -> RealmsBeyondAllInOne <$> liftRunMessage msg attrs

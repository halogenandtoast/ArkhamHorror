module Arkham.Act.Cards.TheHeartOfTheHouse (theHeartOfTheHouse) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (EnemyAttacks)
import Arkham.EnemyLocation.Cards qualified as EnemyLocations
import {-# SOURCE #-} Arkham.Game.Utils (maybeEnemyLocation)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Scenario (getGrid)
import Arkham.Location.Grid (GridLocation (..), flattenGrid)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveToEdit)
import Arkham.Movement
import Arkham.Scenarios.HemlockHouse.Helpers (flipLocationOver, locationIsUnsealed)
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Dormant, Room))

newtype TheHeartOfTheHouse = TheHeartOfTheHouse ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHeartOfTheHouse :: ActCard TheHeartOfTheHouse
theHeartOfTheHouse = act (2, A) TheHeartOfTheHouse Cards.theHeartOfTheHouse Nothing

instance HasModifiersFor TheHeartOfTheHouse where
  getModifiersFor (TheHeartOfTheHouse a) =
    modifySelect a Anyone [CannotDiscoverCluesAt $ not_ $ LocationWithInvestigator Anyone]

instance HasAbilities TheHeartOfTheHouse where
  getAbilities (TheHeartOfTheHouse a) =
    [ restricted a 1 (exists $ YourLocation <> LocationWithTrait Dormant)
        $ actionAbilityWithCost (SameLocationGroupClueCost (PerPlayer 1) YourLocation)
    , mkAbility a 2 $ forced $ EnemyAttacks #after You AnyEnemyAttack (EnemyWithTrait Room)
    , mkAbility a 3
        $ Objective
        $ forced
        $ AddedToVictory #after Nothing (cardIs EnemyLocations.shapelessCellar)
    ]

instance RunMessage TheHeartOfTheHouse where
  runMessage msg a@(TheHeartOfTheHouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        whenM (locationIsUnsealed lid) do
          placeTokens (attrs.ability 1) lid Resource 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      selectEach (locationIs EnemyLocations.shapelessCellar) \cellarLid ->
        moveToEdit (attrs.ability 2) iid cellarLid \m -> m {moveMeans = OneAtATime}
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      grid <- getGrid
      enemyLids <-
        filterM (fmap isJust . maybeEnemyLocation)
          [lid | GridLocation _ lid <- flattenGrid grid]
      for_ enemyLids flipLocationOver
      doStep 1 msg
      push R1
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      dormantLocs <- select $ LocationWithTrait Dormant
      for_ dormantLocs \lid ->
        whenM (locationIsUnsealed lid) $ placeTokens attrs lid Resource 1
      pure a
    _ -> TheHeartOfTheHouse <$> liftRunMessage msg attrs

module Arkham.Location.Cards.SuspendedReef (suspendedReef) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Trait (Trait (Summit))
import Arkham.Window (getBatchId)

newtype SuspendedReef = SuspendedReef LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suspendedReef :: LocationCard SuspendedReef
suspendedReef = location SuspendedReef Cards.suspendedReef 3 (Static 2)

instance HasAbilities SuspendedReef where
  getAbilities (SuspendedReef a) =
    if a.revealed
      then
        extendRevealed
          a
          [ restricted a 1 Here $ forced $ TurnEnds #after You
          , restricted
              a
              2
              ( DuringTurn You
                  <> exists
                    ( EnemyAt
                        $ not_ YourLocation
                        <> LocationWithTrait Summit
                        <> LocationWithDistanceFromAtMost 3 (be a) Anywhere
                    )
              )
              $ FastAbility (ClueCost $ Static 1)
          ]
      else extendUnrevealed1 a (summitEntry a 9)

instance RunMessage SuspendedReef where
  runMessage msg l@(SuspendedReef attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 9 (getBatchId -> batchId) _ -> do
      summitEntryToll attrs 9 iid batchId
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 9 -> True) -> do
      summitEntryFailed attrs 9 iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasCardsInHand <- selectAny $ inHandOf NotForPlay iid
      chooseOrRunOneM iid $ withI18n do
        when hasCardsInHand
          $ countVar 1
          $ labeled' "discardRandomCardsFromHand"
          $ randomDiscardN iid (attrs.ability 1) 1
        countVar 1 $ labeled' "takeDamage" $ assignDamage iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- "Choose an enemy at a Summit location up to 3 connections away. Swap
      -- places with the chosen enemy, ignoring its location's Forced effect."
      -- The clue is spent via the ability cost.
      enemies <-
        select
          $ EnemyAt
          $ not_ (locationWithInvestigator iid)
          <> LocationWithTrait Summit
          <> LocationWithDistanceFromAtMost 3 (be attrs) Anywhere
      chooseTargetM iid enemies $ swapPlacesWithEnemy iid
      pure l
    _ -> SuspendedReef <$> liftRunMessage msg attrs

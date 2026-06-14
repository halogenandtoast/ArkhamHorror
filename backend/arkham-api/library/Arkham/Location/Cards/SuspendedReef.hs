module Arkham.Location.Cards.SuspendedReef (suspendedReef) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose

newtype SuspendedReef = SuspendedReef LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suspendedReef :: LocationCard SuspendedReef
suspendedReef = location SuspendedReef Cards.suspendedReef 3 (Static 2)

instance HasAbilities SuspendedReef where
  getAbilities (SuspendedReef a) =
    extendRevealed
      a
      [ restricted a 1 Here $ forced $ TurnEnds #after You
      , restricted a 2 (DuringTurn You) $ FastAbility (ClueCost $ Static 1)
      ]

instance RunMessage SuspendedReef where
  runMessage msg l@(SuspendedReef attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasCardsInHand <- selectAny $ inHandOf NotForPlay iid
      chooseOrRunOneM iid $ withI18n do
        when hasCardsInHand
          $ countVar 1
          $ labeled' "discardRandomCardsFromHand"
          $ randomDiscardN iid (attrs.ability 1) 1
        countVar 1 $ labeled' "takeDamage" $ assignDamage iid (attrs.ability 1) 1
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      -- The clue is spent via the ability's cost. The swap effect itself —
      -- "choose an enemy at a Summit location up to 3 connections away and swap
      -- places with it (ignoring its location's Forced effect)" — has no engine
      -- primitive: there is no "within N connections" location matcher and no
      -- investigator<->enemy place-swap message. This also interacts with the
      -- Summit-deck / open-sky / sliding-location infra which is not yet modeled.
      -- TODO: once the Summit deck + sliding locations are implemented, target an
      -- EnemyAt a Summit LocationWithTrait within 3 connections, swap the
      -- investigator and enemy positions, and suppress the destination's Forced
      -- "when you would enter" effect.
      pure l
    _ -> SuspendedReef <$> liftRunMessage msg attrs

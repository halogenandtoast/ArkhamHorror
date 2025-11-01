module Arkham.Event.Events.LeadingLadyFinalGirl (leadingLadyFinalGirl) where

import Arkham.ClassSymbol
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Helpers.Playable
import Arkham.Modifier

newtype LeadingLadyFinalGirl = LeadingLadyFinalGirl EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadingLadyFinalGirl :: EventCard LeadingLadyFinalGirl
leadingLadyFinalGirl = event LeadingLadyFinalGirl Cards.leadingLadyFinalGirl

instance RunMessage LeadingLadyFinalGirl where
  runMessage msg e@(LeadingLadyFinalGirl attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- withoutModifiersFrom iid $ getPlayableCards attrs iid (UnpaidCost NoAction) attrs.windows
      chooseTargetM iid cards \card -> do
        temporaryModifier iid attrs (MetaModifier $ object ["leadingLady" .= card.id]) do
          playCardPayingCost iid card
        when (Survivor `elem` card.classes) $ do_ msg
      pure e
    Do (PlayThisEvent iid (is attrs -> True)) -> do
      canHealDamage <- canHaveDamageHealed attrs iid
      canHealHorror <- canHaveHorrorHealed attrs iid
      chooseOneM iid do
        labeled "Draw 1 card" $ drawCards iid attrs 1
        when canHealDamage $ labeled "Heal 1 damage" $ healDamage attrs iid 1
        when canHealHorror $ labeled "Heal 1 horror" $ healHorror attrs iid 1
        labeled "Do nothing" nothing
      pure e
    _ -> LeadingLadyFinalGirl <$> liftRunMessage msg attrs

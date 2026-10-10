module Arkham.Homebrew.AgesUnwound.Treacheries.InEveryShadow (inEveryShadow) where

import Arkham.Card
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers
import Arkham.Homebrew.AgesUnwound.Traits
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Trait (Trait (Criminal))
import Arkham.Treachery.Import.Lifted

newtype InEveryShadow = InEveryShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inEveryShadow :: TreacheryCard InEveryShadow
inEveryShadow = treachery InEveryShadow Cards.inEveryShadow

instance RunMessage InEveryShadow where
  runMessage msg t@(InEveryShadow attrs) = runQueueT $ case msg of
    {- "Revelation - Test [agility] (4). This test gets +1 difficulty for every 2
    cards in the Arkham Streets deck." -}
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      streets <- length <$> getArkhamStreetsDeck
      skillTestModifier sid attrs sid (Difficulty $ streets `div` 2)
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    {- "For each point you fail by, discard the top card of the encounter deck."

    One message with the count, not a 'doStep' countdown: nothing resolves
    per-point, so there is no loop to drive (and so no #5816 trap). -}
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      discardTopOfEncounterDeckAndHandle iid attrs n attrs
      pure t
    -- "Draw a [[Criminal]] enemy and a [[Darkness]] treachery discarded this way."
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      let criminals = filterCards (card_ $ #enemy <> CardWithTrait Criminal) cards
      let darkness = filterCards (card_ $ #treachery <> CardWithTrait Darkness) cards
      focusCards (criminals <> darkness) do
        chooseTargetM iid criminals (drawCard iid)
        chooseTargetM iid darkness (drawCard iid)
      pure t
    _ -> InEveryShadow <$> liftRunMessage msg attrs

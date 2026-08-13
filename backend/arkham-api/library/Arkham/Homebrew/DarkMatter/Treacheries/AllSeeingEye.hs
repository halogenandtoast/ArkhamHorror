module Arkham.Homebrew.DarkMatter.Treacheries.AllSeeingEye (allSeeingEye) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Homebrew.DarkMatter.Actions (pattern Scan)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern AI)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype AllSeeingEye = AllSeeingEye TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allSeeingEye :: TreacheryCard AllSeeingEye
allSeeingEye = treachery AllSeeingEye Cards.allSeeingEye

instance HasModifiersFor AllSeeingEye where
  getModifiersFor (AllSeeingEye a) =
    inThreatAreaGets a [AdditionalCostToPerformAction (IsAction Scan) discardUntilAI]
   where
    discardUntilAI = DiscardEncounterUntilFirstCost (toSource a) $ basic $ CardWithTrait AI

instance HasAbilities AllSeeingEye where
  getAbilities (AllSeeingEye a) =
    [skillTestAbility $ restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You]

instance RunMessage AllSeeingEye where
  runMessage msg t@(AllSeeingEye attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just card) -> do
      drawCard iid card
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> AllSeeingEye <$> liftRunMessage msg attrs

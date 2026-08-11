module Arkham.Homebrew.DarkMatter.Treacheries.AllSeeingEye (allSeeingEye) where

import Arkham.Ability
import Arkham.Card
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

{- | "As an additional cost to perform a scan, discard cards from the top of the
encounter deck until an [[AI]] encounter card is discarded and draw it."

The rider rides on the bearer rather than on any one scan ability, so it taxes
every scan they perform regardless of which card raised the action.
'DiscardEncounterUntilFirstCost' does the discarding while the cost is paid and
hands the matching card back as 'RequestedEncounterCard'.
-}
instance HasModifiersFor AllSeeingEye where
  getModifiersFor (AllSeeingEye a) =
    inThreatAreaGets a [AdditionalCostToPerformAction (IsAction Scan) discardUntilAI]
   where
    discardUntilAI = DiscardEncounterUntilFirstCost $ basic $ CardWithTrait AI

{- | "Forced - At the end of your turn: Test [agility] (3). If you succeed,
discard All-Seeing Eye."
-}
instance HasAbilities AllSeeingEye where
  getAbilities (AllSeeingEye a) =
    [skillTestAbility $ restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You]

{- | "Revelation - Put this card into play in your threat area."

The scan cost is paid by the scan ability, so the encounter card comes back
sourced to whichever card raised the action, not to this treachery. Match on the
bearer and on the [[AI]] trait the cost searched for instead of on the source.
-}
instance RunMessage AllSeeingEye where
  runMessage msg t@(AllSeeingEye attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    RequestedEncounterCard _ (Just iid) (Just card)
      | attrs.owner == Just iid && card `cardMatch` CardWithTrait AI -> do
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

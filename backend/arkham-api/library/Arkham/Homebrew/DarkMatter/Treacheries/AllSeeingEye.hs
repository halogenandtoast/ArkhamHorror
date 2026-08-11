module Arkham.Homebrew.DarkMatter.Treacheries.AllSeeingEye (allSeeingEye) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype AllSeeingEye = AllSeeingEye TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allSeeingEye :: TreacheryCard AllSeeingEye
allSeeingEye = treachery AllSeeingEye Cards.allSeeingEye

{- | "Revelation - Put this card into play in your threat area.
As an additional cost to perform a scan, discard cards from the top of the
encounter deck until an [[AI]] encounter card is discarded and draw it.
Forced - At the end of your turn: Test [agility] (3). If you succeed, discard
All-Seeing Eye."

TODO(homebrew): the additional scan cost is not modeled. Costs are declared on
the ability being paid for, and the Scan action is raised from several
unrelated cards, so charging it needs a scan-cost seam in the Scan helper
rather than a modifier on this treachery.
-}
instance HasAbilities AllSeeingEye where
  getAbilities (AllSeeingEye a) =
    [skillTestAbility $ restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You]

instance RunMessage AllSeeingEye where
  runMessage msg t@(AllSeeingEye attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> AllSeeingEye <$> liftRunMessage msg attrs

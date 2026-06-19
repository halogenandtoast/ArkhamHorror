module Arkham.Treachery.Cards.PsychotropicSpores (psychotropicSpores) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Helpers.Cost
import Arkham.Helpers.History
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PsychotropicSpores = PsychotropicSpores TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychotropicSpores :: TreacheryCard PsychotropicSpores
psychotropicSpores = treachery PsychotropicSpores Cards.psychotropicSpores

instance HasAbilities PsychotropicSpores where
  getAbilities (PsychotropicSpores a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ DrewCardsFromOwnDeck #after You
    , skillTestAbility $ restricted a 2 OnSameLocation actionAbility
    ]

instance RunMessage PsychotropicSpores where
  runMessage msg t@(PsychotropicSpores attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      whenNone (treacheryInThreatAreaOf iid <> treacheryIs Cards.psychotropicSpores) do
        placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- The DrewCardsFromOwnDeck window fires on the first draw of each phase, but
      -- this ability should only resolve once per round. By the time it resolves,
      -- the current draw is already recorded in the phase history, so
      -- RoundHistory (= roundHistory <> phaseHistory) is never 0. Compare against
      -- the phase total to isolate prior phases this round: if no earlier phase
      -- drew cards, this is the round's first draw and we deal horror.
      roundDrawn <- getHistoryField RoundHistory iid HistoryCardsDrawn
      phaseDrawn <- getHistoryField PhaseHistory iid HistoryCardsDrawn
      when (roundDrawn == phaseDrawn) do
        directHorror iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid $ campaignI18n do
        labeled' "psychotropicSpores.pay" do
          payEffectCost iid attrs $ DirectHorrorCost (attrs.ability 2) (InvestigatorWithId iid) 1
          skillTestModifier sid (attrs.ability 2) sid SkillTestAutomaticallySucceeds
        unscoped skip_
      beginSkillTest sid iid (attrs.ability 2) iid #intellect (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      -- Register the discard as a skill-test success option so the player can
      -- order it relative to other "on success" effects (e.g. Book of Verse
      -- drawing a card) and discard the spores before such a draw would trigger
      -- this card's own forced "first draw each round" horror.
      skillTestCardOptionVariant "discard" attrs $ toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> PsychotropicSpores <$> liftRunMessage msg attrs

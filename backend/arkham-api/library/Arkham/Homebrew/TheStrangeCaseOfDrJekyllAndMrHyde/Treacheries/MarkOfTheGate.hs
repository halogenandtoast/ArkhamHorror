-- | "Revelation - Put Mark of the Gate into play in your threat area."
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Treacheries.MarkOfTheGate (
  markOfTheGate,
) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Helpers.SkillTest (getTotalModifiedSkillValue)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype MarkOfTheGate = MarkOfTheGate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markOfTheGate :: TreacheryCard MarkOfTheGate
markOfTheGate = treachery MarkOfTheGate Cards.markOfTheGate

{- | "If your skill value was not reduced to 0 for this test, discard Mark of the Gate."
Read while the test is still live. Two ways to reach 0: the modified skill value
itself, which floors at 0, and an automatic failure -- the rules reference puts
the total skill value of an automatically failed test at 0, which is what the
[auto_fail] token does.
-}
discardUnlessZeroed :: ReverseQueue m => InvestigatorId -> TreacheryAttrs -> m ()
discardUnlessZeroed iid attrs = do
  zeroed <-
    orM
      [ selectAny $ RevealedChaosTokens WithAutoFailModifier
      , (== 0) <$> getTotalModifiedSkillValue
      ]
  unless zeroed $ toDiscardBy iid (attrs.ability 1) attrs

instance HasAbilities MarkOfTheGate where
  getAbilities (MarkOfTheGate a) =
    [ scenarioI18n
        $ withI18nTooltip "markOfTheGate.forced"
        $ skillTestAbility
        $ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage MarkOfTheGate where
  runMessage msg t@(MarkOfTheGate attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      randomDiscard iid (attrs.ability 1)
      discardUnlessZeroed iid attrs
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      discardUnlessZeroed iid attrs
      pure t
    _ -> MarkOfTheGate <$> liftRunMessage msg attrs

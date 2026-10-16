-- | "Revelation - Put Creeping Growth into play in your threat area."
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Treacheries.CreepingGrowth (
  creepingGrowth,
) where

import Arkham.Ability
import Arkham.Helpers.SkillTest (getTotalModifiedSkillValue)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype CreepingGrowth = CreepingGrowth TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

creepingGrowth :: TreacheryCard CreepingGrowth
creepingGrowth = treachery CreepingGrowth Cards.creepingGrowth

{- | "If your skill value was not reduced to 0 for this test, discard Creeping Growth."
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

instance HasAbilities CreepingGrowth where
  getAbilities (CreepingGrowth a) =
    [ scenarioI18n
        $ withI18nTooltip "creepingGrowth.forced"
        $ skillTestAbility
        $ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage CreepingGrowth where
  runMessage msg t@(CreepingGrowth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      loseResources iid (attrs.ability 1) 2
      discardUnlessZeroed iid attrs
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      discardUnlessZeroed iid attrs
      pure t
    _ -> CreepingGrowth <$> liftRunMessage msg attrs

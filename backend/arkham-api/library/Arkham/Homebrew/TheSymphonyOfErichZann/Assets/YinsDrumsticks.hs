module Arkham.Homebrew.TheSymphonyOfErichZann.Assets.YinsDrumsticks (yinsDrumsticks) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (scenarioI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Placement

newtype YinsDrumsticks = YinsDrumsticks AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yinsDrumsticks :: AssetCard YinsDrumsticks
yinsDrumsticks = asset YinsDrumsticks Cards.yinsDrumsticks

instance HasAbilities YinsDrumsticks where
  getAbilities (YinsDrumsticks a) =
    [ restricted a 1 IsCommitted
        $ freeReaction (SkillTestResult #after You AnySkillTest (SuccessResult AnyValue))
    , controlled_ a 2 fightAction_
    ]

instance RunMessage YinsDrumsticks where
  runMessage msg a@(YinsDrumsticks attrs) = runQueueT $ case msg of
    -- Nothing may commit a card from play, so the drumsticks lend themselves to the
    -- test as if they were in hand and MustBeCommitted locks that in at ST.2.
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 2) iid (CanCommitToSkillTestsAsIfInHand $ toCard attrs)
      skillTestModifier sid (attrs.ability 2) (toCardId attrs) MustBeCommitted
      skillTestModifier sid (attrs.ability 2) iid (DamageDealt 1)
      chooseFightEnemy sid iid (attrs.ability 2)
      pure a
    -- Committed for real now, so the card leaves play; preloadCommittedEntities then
    -- rebuilds it on the test, which is what carries ability 1.
    CommitCard _ card
      | card.id == toCardId attrs
      , isInPlayPlacement attrs.placement -> do
          push $ RemoveFromPlay (toSource attrs)
          pure a
    Committed iid (UseThisAbility iid' (isSource attrs -> True) 1) | iid == iid' -> do
      chooseOneM iid $ scenarioI18n $ scope "yinsDrumsticks" do
        labeled "putIntoPlay" $ putCardIntoPlay iid (toCard attrs)
        labeled "returnToHand" do
          push $ SkillTestUncommitCard iid (toCard attrs)
          addToHand iid [toCard attrs]
      pure a
    _ -> YinsDrumsticks <$> liftRunMessage msg attrs

module Arkham.Homebrew.TheSymphonyOfErichZann.Assets.YinsDrumsticks (yinsDrumsticks) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (scenarioI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Message.Lifted.Choose

newtype YinsDrumsticks = YinsDrumsticks AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yinsDrumsticks :: AssetCard YinsDrumsticks
yinsDrumsticks = asset YinsDrumsticks Cards.yinsDrumsticks

instance HasAbilities YinsDrumsticks where
  getAbilities (YinsDrumsticks a) =
    [ -- A successful test it was committed to returns it to play or to hand.
      restricted a 1 (InYourHand <> youExist You)
        $ freeReaction (SkillTestResult #after You AnySkillTest (SuccessResult AnyValue))
    , -- Fight with the drumsticks committed for +1 damage.
      controlled a 2 NoRestriction $ fightAction (exhaust a)
    ]

instance RunMessage YinsDrumsticks where
  runMessage msg a@(YinsDrumsticks attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ scenarioI18n $ scope "yinsDrumsticks" do
        labeled "putIntoPlay" $ putCardIntoPlay iid (toCard attrs)
        labeled "returnToHand" $ addToHand iid [toCard attrs]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 2) iid (DamageDealt 1)
      chooseFightEnemy sid iid (attrs.ability 2)
      pure a
    _ -> YinsDrumsticks <$> liftRunMessage msg attrs

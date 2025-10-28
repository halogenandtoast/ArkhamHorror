module Arkham.Asset.Assets.Scavenging2 (scavenging2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Playable
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (defaultWindows)

newtype Scavenging2 = Scavenging2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scavenging2 :: AssetCard Scavenging2
scavenging2 = asset Scavenging2 Cards.scavenging2

instance HasAbilities Scavenging2 where
  getAbilities (Scavenging2 a) =
    [ controlled a 1 (youExist $ can.have.cards.leaveDiscard <> DiscardWith (HasCard #item))
        $ triggered (SkillTestResult #after You #investigating (SuccessResult $ atLeast 2)) (exhaust a)
    ]

instance RunMessage Scavenging2 where
  runMessage msg a@(Scavenging2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ inDiscardOf iid <> basic #item
      focusCards cards do
        chooseOneM iid $ targets cards \card -> do
          unfocusCards
          chooseOrRunOneM iid do
            labeled "Add to hand" $ drawCardFrom iid card iid
            whenM (getIsPlayable iid attrs (UnpaidCost NoAction) (defaultWindows iid) card) do
              labeled "Play" $ playCardPayingCost iid card
      pure a
    _ -> Scavenging2 <$> liftRunMessage msg attrs

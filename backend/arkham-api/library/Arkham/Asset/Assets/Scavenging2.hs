module Arkham.Asset.Assets.Scavenging2 (Scavenging2 (..), scavenging2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Matcher
import Arkham.Strategy

newtype Scavenging2 = Scavenging2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scavenging2 :: AssetCard Scavenging2
scavenging2 = asset Scavenging2 Cards.scavenging2

instance HasAbilities Scavenging2 where
  getAbilities (Scavenging2 a) =
    [ controlledAbility a 1 (youExist $ can.have.cards.leaveDiscard <> DiscardWith (HasCard #item))
        $ ReactionAbility (SkillTestResult #after You #investigating (SuccessResult $ atLeast 2)) (exhaust a)
    ]

instance RunMessage Scavenging2 where
  runMessage msg a@(Scavenging2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromDiscard] #item $ AddToHandOrPlayFound iid 1
      pure a
    _ -> Scavenging2 <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.AncientStone1 (ancientStone1) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import Arkham.Discover
import Arkham.Helpers.Modifiers
import Arkham.Investigate
import Arkham.Location.Types (Field (..))
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Projection

newtype AncientStone1 = AncientStone1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientStone1 :: AssetCard AncientStone1
ancientStone1 = asset AncientStone1 Cards.ancientStone1

instance HasAbilities AncientStone1 where
  getAbilities (AncientStone1 a) = [restrictedAbility a 1 ControlsThis investigateAction_]

instance RunMessage AncientStone1 where
  runMessage msg a@(AncientStone1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigation <- mkInvestigate sid iid (toAbilitySource attrs 1) <&> setTarget attrs
      enabled <- skillTestModifiers sid attrs investigation.location [ShroudModifier 3]
      pushAll
        [ enabled
        , toMessage investigation
        ]
      pure a
    Successful (Action.Investigate, LocationTarget lid) iid _ target _ | attrs `is` target -> do
      amount <- min 2 <$> field LocationClues lid
      difficulty <- fromJustNote "missing" <$> getSkillTestDifficulty
      pushAll
        $ [ Msg.DiscoverClues iid $ viaInvestigate $ discover lid (toAbilitySource attrs 1) amount
          , toDiscardBy iid (toAbilitySource attrs 1) attrs
          ]
        <> [RecordCount YouHaveIdentifiedTheStone difficulty]
      pure a
    _ -> AncientStone1 <$> runMessage msg attrs

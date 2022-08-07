module Arkham.Asset.Cards.ProtectiveIncantation1
  ( protectiveIncantation1
  , ProtectiveIncantation1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Criteria
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype ProtectiveIncantation1 = ProtectiveIncantation1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

protectiveIncantation1 :: AssetCard ProtectiveIncantation1
protectiveIncantation1 =
  asset ProtectiveIncantation1 Cards.protectiveIncantation1

instance HasAbilities ProtectiveIncantation1 where
  getAbilities (ProtectiveIncantation1 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ForcedAbility
        $ TurnEnds Timing.When
        $ HasMatchingAsset
        $ AssetWithId
        $ toId a
    ]

instance RunMessage ProtectiveIncantation1 where
  runMessage msg a@(ProtectiveIncantation1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      hasResources <- fieldP InvestigatorResources (> 0) iid
      push
        $ chooseOrRunOne iid
        $ Label "Discard Protective Incantation" [Discard (toTarget attrs)]
        : [ Label "Spend 1 resource" [SpendResources iid 1] | hasResources ]
      pure a
    _ -> ProtectiveIncantation1 <$> runMessage msg attrs

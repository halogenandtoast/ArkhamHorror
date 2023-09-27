module Arkham.Asset.Cards.GateBox (
  gateBox,
  GateBox (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Movement

newtype GateBox = GateBox AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gateBox :: AssetCard GateBox
gateBox = asset GateBox Cards.gateBox

instance HasAbilities GateBox where
  getAbilities (GateBox a) =
    [ controlledAbility
        a
        1
        ( AnyCriterion
            [exists (You <> InvestigatorEngagedWith AnyEnemy), Negate (exists $ LocationWithTitle "Dream-Gate")]
        )
        $ FastAbility
        $ assetUseCost a Charge 1
    ]

instance RunMessage GateBox where
  runMessage msg a@(GateBox attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- selectList $ EnemyIsEngagedWith $ InvestigatorWithId iid
      (dreamGate, placement) <- placeLocationCard Locations.dreamGateWondrousJourney
      pushAll
        $ map (DisengageEnemy iid) enemies
        <> [placement, MoveTo $ move (toAbilitySource attrs 1) iid dreamGate]
      pure a
    _ -> GateBox <$> runMessage msg attrs

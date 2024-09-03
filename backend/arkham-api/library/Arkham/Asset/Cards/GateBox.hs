module Arkham.Asset.Cards.GateBox (gateBox, GateBox (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (PutLocationIntoPlay)
import Arkham.Movement
import Arkham.Prelude
import Arkham.Window

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
        (oneOf [youExist $ InvestigatorEngagedWith AnyEnemy, notExists $ LocationWithTitle "Dream-Gate"])
        $ FastAbility
        $ assetUseCost a Charge 1
    ]

instance RunMessage GateBox where
  runMessage msg a@(GateBox attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ enemyEngagedWith iid
      (dreamGate, placement) <- placeLocationCard Locations.dreamGateWondrousJourney
      afterPutIntoPlayWindow <- checkAfter $ PutLocationIntoPlay iid dreamGate
      pushAll
        $ map (DisengageEnemy iid) enemies
        <> [placement, afterPutIntoPlayWindow, MoveTo $ move (attrs.ability 1) iid dreamGate]
      pure a
    _ -> GateBox <$> runMessage msg attrs

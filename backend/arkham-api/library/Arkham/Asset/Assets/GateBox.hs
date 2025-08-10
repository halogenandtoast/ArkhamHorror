module Arkham.Asset.Assets.GateBox (gateBox) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (PutLocationIntoPlay)
import Arkham.Message.Lifted.Move
import Arkham.Window (WindowType (..))

newtype GateBox = GateBox AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gateBox :: AssetCard GateBox
gateBox = asset GateBox Cards.gateBox

instance HasAbilities GateBox where
  getAbilities (GateBox a) =
    [ controlled
        a
        1
        (oneOf [youExist $ InvestigatorEngagedWith AnyEnemy, notExists $ LocationWithTitle "Dream-Gate"])
        $ FastAbility
        $ assetUseCost a Charge 1
    ]

instance RunMessage GateBox where
  runMessage msg a@(GateBox attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (enemyEngagedWith iid) (disengageEnemy iid)
      dreamGate <- placeLocationCard Locations.dreamGateWondrousJourney
      checkAfter $ PutLocationIntoPlay iid dreamGate
      moveTo (attrs.ability 1) iid dreamGate
      pure a
    _ -> GateBox <$> liftRunMessage msg attrs

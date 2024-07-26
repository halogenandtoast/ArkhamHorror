module Arkham.Asset.Cards.Matchbox (matchbox, Matchbox (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Modifier

newtype Matchbox = Matchbox AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

matchbox :: AssetCard Matchbox
matchbox = asset Matchbox Cards.matchbox

instance HasAbilities Matchbox where
  getAbilities (Matchbox x) =
    [ controlledAbility x 1 (DuringTurn $ affectsOthers Anyone)
        $ FastAbility (exhaust x <> assetUseCost x Supply 1)
    ]

instance RunMessage Matchbox where
  runMessage msg a@(Matchbox attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        iid' <- selectJust TurnInvestigator
        turnModifier iid' (attrs.ability 1) lid (ShroudModifier (-1))
      pure a
    _ -> Matchbox <$> liftRunMessage msg attrs

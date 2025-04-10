module Arkham.Asset.Assets.Farsight4 (farsight4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Playable (getIsPlayable)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose

newtype Farsight4 = Farsight4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

farsight4 :: AssetCard Farsight4
farsight4 = asset Farsight4 Cards.farsight4

instance HasAbilities Farsight4 where
  getAbilities (Farsight4 a) =
    [ controlled
        a
        1
        ( DuringTurn You
            <> youExist (HandWith (LengthIs $ atLeast 8))
            <> PlayableCardExists (UnpaidCost NoAction) (InHandOf ForPlay You <> basic #event)
        )
        $ FastAbility (exhaust a)
    ]

instance RunMessage Farsight4 where
  runMessage msg a@(Farsight4 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      events <- select $ inHandOf ForPlay iid <> #event
      playableEvents <- filterM (getIsPlayable iid (toSource attrs) (UnpaidCost NoAction) windows') events
      chooseTargetM iid playableEvents \event -> playCardPayingCostWithWindows iid event windows'
      pure a
    _ -> Farsight4 <$> liftRunMessage msg attrs

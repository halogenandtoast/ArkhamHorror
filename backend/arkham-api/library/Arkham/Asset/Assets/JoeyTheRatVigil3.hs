module Arkham.Asset.Assets.JoeyTheRatVigil3 (joeyTheRatVigil3, JoeyTheRatVigil3 (..)) where

import Arkham.Ability hiding (DuringTurn)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Game.Helpers (getIsPlayable)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn, FastPlayerWindow)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait
import Arkham.Window

newtype JoeyTheRatVigil3 = JoeyTheRatVigil3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joeyTheRatVigil3 :: AssetCard JoeyTheRatVigil3
joeyTheRatVigil3 = ally JoeyTheRatVigil3 Cards.joeyTheRatVigil3 (3, 2)

instance HasAbilities JoeyTheRatVigil3 where
  getAbilities (JoeyTheRatVigil3 x) =
    [ withTooltip "Spend 1 resource: Choose an _Item_ asset from your hand and play it (paying its cost)."
        $ controlledAbility
          x
          1
          (PlayableCardExists (AuxiliaryCost (ResourceCost 1) $ UnpaidCost NoAction) (InHandOf You <> #item))
          (FastAbility $ ResourceCost 1)
    , withTooltip "Discard an _Item_ asset from play: Gain 2 resources."
        $ controlledAbility x 2 (can.gain.resources You)
        $ FastAbility
        $ DiscardAssetCost #item
    ]

instance RunMessage JoeyTheRatVigil3 where
  runMessage msg a@(JoeyTheRatVigil3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      items <- filter (member Item . toTraits) <$> field InvestigatorHand iid
      let windows'' = nub $ windows' <> [mkWhen (DuringTurn iid), mkWhen FastPlayerWindow]
      playableItems <- filterM (getIsPlayable iid (attrs.ability 1) (UnpaidCost NoAction) windows'') items
      chooseTargetM iid playableItems $ playCardPayingCost iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      gainResourcesIfCan iid (attrs.ability 2) 2
      pure a
    _ -> JoeyTheRatVigil3 <$> liftRunMessage msg attrs

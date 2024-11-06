module Arkham.Asset.Assets.JoeyTheRatVigil (joeyTheRatVigil, JoeyTheRatVigil (..)) where

import Arkham.Ability hiding (DuringTurn)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Game.Helpers (getIsPlayable)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn, FastPlayerWindow)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait
import Arkham.Window

newtype JoeyTheRatVigil = JoeyTheRatVigil AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joeyTheRatVigil :: AssetCard JoeyTheRatVigil
joeyTheRatVigil = ally JoeyTheRatVigil Cards.joeyTheRatVigil (3, 2)

-- TODO: This does not account for the 1 resource spent in the cost
instance HasAbilities JoeyTheRatVigil where
  getAbilities (JoeyTheRatVigil x) =
    [ controlledAbility
        x
        1
        (PlayableCardExists (AuxiliaryCost (ResourceCost 1) $ UnpaidCost NoAction) (InHandOf You <> #item))
        (FastAbility $ ResourceCost 1)
    ]

instance RunMessage JoeyTheRatVigil where
  runMessage msg a@(JoeyTheRatVigil attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      items <- filter (member Item . toTraits) <$> field InvestigatorHand iid
      let windows'' = nub $ windows' <> [mkWhen (DuringTurn iid), mkWhen FastPlayerWindow]
      playableItems <-
        filterM (getIsPlayable iid (attrs.ability 1) (UnpaidCost NoAction) windows'') items

      chooseTargetM iid playableItems $ playCardPayingCost iid
      pure a
    _ -> JoeyTheRatVigil <$> liftRunMessage msg attrs

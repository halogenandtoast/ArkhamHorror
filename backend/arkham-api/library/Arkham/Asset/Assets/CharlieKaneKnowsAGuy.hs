module Arkham.Asset.Assets.CharlieKaneKnowsAGuy (charlieKaneKnowsAGuy) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Slot

newtype CharlieKaneKnowsAGuy = CharlieKaneKnowsAGuy AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charlieKaneKnowsAGuy :: AssetCard CharlieKaneKnowsAGuy
charlieKaneKnowsAGuy = ally CharlieKaneKnowsAGuy Cards.charlieKaneKnowsAGuy (2, 2)

instance HasAbilities CharlieKaneKnowsAGuy where
  getAbilities (CharlieKaneKnowsAGuy a) =
    [ controlled a 1 (PlayableCardExistsWithCostReduction (Reduce 2) (InHandOf ForPlay You <> #asset <> #ally))
        $ freeReaction (AssetEntersPlay #after (be a))
    ]

instance RunMessage CharlieKaneKnowsAGuy where
  runMessage msg a@(CharlieKaneKnowsAGuy attrs) = runQueueT $ case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      pushAll $ replicate 2 (AddSlot iid AllySlot (Slot (toSource attrs) []))
      CharlieKaneKnowsAGuy <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ PlayableCardWithCostReduction NoAction 2 (inHandOf ForPlay iid <> basic (#asset <> #ally))
      chooseTargetM iid cards \c -> do
        reduceCostOf (attrs.ability 1) c 2
        playCardPayingCost iid c
      pure a
    _ -> CharlieKaneKnowsAGuy <$> liftRunMessage msg attrs

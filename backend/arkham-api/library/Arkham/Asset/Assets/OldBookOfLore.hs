module Arkham.Asset.Assets.OldBookOfLore (oldBookOfLore) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy

newtype OldBookOfLore = OldBookOfLore AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldBookOfLore :: AssetCard OldBookOfLore
oldBookOfLore = asset OldBookOfLore Cards.oldBookOfLore

instance HasAbilities OldBookOfLore where
  getAbilities (OldBookOfLore a) =
    [ controlled a 1 (exists $ affectsOthers $ colocatedWithMatch You <> can.manipulate.deck)
        $ actionAbilityWithCost (exhaust a)
    ]

instance RunMessage OldBookOfLore where
  runMessage msg a@(OldBookOfLore attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      investigators <- select =<< guardAffectsColocated iid
      chooseOneM iid do
        targets investigators \iid' -> search iid' source iid' [fromTopOfDeck 3] #any (DrawFound iid' 1)
      pure a
    _ -> OldBookOfLore <$> liftRunMessage msg attrs

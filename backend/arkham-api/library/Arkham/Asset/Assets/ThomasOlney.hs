module Arkham.Asset.Assets.ThomasOlney (thomasOlney) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Trait

newtype ThomasOlney = ThomasOlney AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thomasOlney :: AssetCard ThomasOlney
thomasOlney = ally ThomasOlney Cards.thomasOlney (3, 1)

instance HasAbilities ThomasOlney where
  getAbilities (ThomasOlney a) =
    [ restricted a 1 ControlsThis $ FastAbility (exhaust a)
    , controlled a 2 (DuringSkillTest SkillTestAtYourLocation) $ FastAbility (exhaust a)
    ]

instance RunMessage ThomasOlney where
  runMessage msg a@(ThomasOlney attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneDropDown iid [(displayTrait trait, ForTrait trait msg) | trait <- [minBound ..]]
      pure a
    ForTrait trait (UseThisAbility iid (isSource attrs -> True) 1) -> do
      discardTopOfEncounterDeckAndHandle iid (attrs.ability 1) 1 attrs
      pure $ ThomasOlney $ attrs & setMeta trait
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      for_ (maybeResult attrs.meta) \trait -> do
        when (any (`cardMatch` CardWithTrait trait) cards) do
          gainResources iid (attrs.ability 1) 2
      pure $ ThomasOlney $ attrs & setMeta Null
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discardTopOfDeckAndHandle iid (attrs.ability 2) 1 attrs
      pure a
    DiscardedTopOfDeck iid (card : _) _ (isTarget attrs -> True) -> do
      whenM (getIsCommittable iid (toCard card)) $ commitCard iid card
      pure a
    _ -> ThomasOlney <$> liftRunMessage msg attrs

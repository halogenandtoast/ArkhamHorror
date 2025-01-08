module Arkham.Asset.Assets.OnYourOwn3 (onYourOwn3, OnYourOwn3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGetsWhen)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Matcher
import Arkham.Placement

newtype Metadata = Metadata {beingDiscarded :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype OnYourOwn3 = OnYourOwn3 (AssetAttrs `With` Metadata)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onYourOwn3 :: AssetCard OnYourOwn3
onYourOwn3 = asset (OnYourOwn3 . (`with` Metadata False)) Cards.onYourOwn3

instance HasAbilities OnYourOwn3 where
  getAbilities (OnYourOwn3 (a `With` meta)) = case assetPlacement a of
    InPlayArea iid ->
      [ restricted a 0 (exists $ assetControlledBy iid <> AssetInSlot #ally) Anytime
      | not (beingDiscarded meta)
      ]
        <> [onYourOwn3Reaction]
    _ -> [onYourOwn3Reaction]
   where
    onYourOwn3Reaction =
      restricted a 1 ControlsThis
        $ ReactionAbility (PlayCard #when You $ basic $ #survivor <> #event) (exhaust a)

instance HasModifiersFor OnYourOwn3 where
  getModifiersFor (OnYourOwn3 (a `With` _)) =
    controllerGetsWhen a a.ready [CanReduceCostOf (#event <> #survivor) 2]

instance RunMessage OnYourOwn3 where
  runMessage msg a@(OnYourOwn3 (attrs `With` meta)) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 0 _ _ -> do
      toDiscardBy iid GameSource attrs
      pure . OnYourOwn3 $ attrs `with` Metadata True
    UseCardAbility iid (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      costModifier (attrs.ability 1) iid (ReduceCostOf (CardWithId card.id) 2)
      pure a
    _ -> OnYourOwn3 . (`with` meta) <$> liftRunMessage msg attrs

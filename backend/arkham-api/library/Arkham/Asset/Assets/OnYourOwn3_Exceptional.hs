module Arkham.Asset.Assets.OnYourOwn3_Exceptional (
  onYourOwn3_Exceptional,
  OnYourOwn3_Exceptional (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGetsWhen)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Matcher

newtype OnYourOwn3_Exceptional = OnYourOwn3_Exceptional AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onYourOwn3_Exceptional :: AssetCard OnYourOwn3_Exceptional
onYourOwn3_Exceptional = asset OnYourOwn3_Exceptional Cards.onYourOwn3_Exceptional

instance HasAbilities OnYourOwn3_Exceptional where
  getAbilities (OnYourOwn3_Exceptional a) =
    [ restricted a 1 ControlsThis
        $ ReactionAbility (PlayCard #when You $ basic $ #survivor <> #event) (exhaust a)
    ]

instance HasModifiersFor OnYourOwn3_Exceptional where
  getModifiersFor (OnYourOwn3_Exceptional a) =
    controllerGetsWhen a a.ready [CanReduceCostOf (#event <> #survivor) 2]

instance RunMessage OnYourOwn3_Exceptional where
  runMessage msg a@(OnYourOwn3_Exceptional attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      costModifier (attrs.ability 1) iid (ReduceCostOf (CardWithId card.id) 2)
      pure a
    _ -> OnYourOwn3_Exceptional <$> liftRunMessage msg attrs

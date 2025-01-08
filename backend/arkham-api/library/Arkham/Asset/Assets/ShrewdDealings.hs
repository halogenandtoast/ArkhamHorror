module Arkham.Asset.Assets.ShrewdDealings (shrewdDealings, ShrewdDealings (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Game.Helpers (passesLimits)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ShrewdDealings = ShrewdDealings AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrewdDealings :: AssetCard ShrewdDealings
shrewdDealings = asset ShrewdDealings Cards.shrewdDealings

instance HasModifiersFor ShrewdDealings where
  getModifiersFor (ShrewdDealings a) = controllerGets a [ReduceCostOf (#asset <> #item) 1]

instance HasAbilities ShrewdDealings where
  getAbilities (ShrewdDealings x) =
    [ restrictedAbility
        x
        1
        (ControlsThis <> exists (affectsOthers $ InvestigatorAt YourLocation <> NotYou))
        $ freeReaction (PlayCard #when You $ basic $ #asset <> #item <> NonSignature)
    ]

instance RunMessage ShrewdDealings where
  runMessage msg a@(ShrewdDealings attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      investigators <- filterM (`passesLimits` card) =<< select (affectsOthers $ colocatedWith iid)
      -- need to ensure card would not exceed limit
      chooseOneM iid do
        targets investigators \investigator -> do
          cardResolutionModifier card (attrs.ability 1) card (PlayUnderControlOf investigator)
      pure a
    _ -> ShrewdDealings <$> liftRunMessage msg attrs

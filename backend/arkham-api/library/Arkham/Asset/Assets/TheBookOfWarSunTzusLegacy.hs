module Arkham.Asset.Assets.TheBookOfWarSunTzusLegacy (theBookOfWarSunTzusLegacy) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Asset.Uses
import Arkham.Helpers.Window (getPlayedEvent)
import Arkham.Matcher

newtype TheBookOfWarSunTzusLegacy = TheBookOfWarSunTzusLegacy AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBookOfWarSunTzusLegacy :: AssetCard TheBookOfWarSunTzusLegacy
theBookOfWarSunTzusLegacy = asset TheBookOfWarSunTzusLegacy Cards.theBookOfWarSunTzusLegacy

instance HasAbilities TheBookOfWarSunTzusLegacy where
  getAbilities (TheBookOfWarSunTzusLegacy x) =
    [ restricted x 1 ControlsThis
        $ triggered
          (PlayEvent #after You (#tactic <> EventWillNotBeRemoved))
          (exhaust x <> assetUseCost x Secret 1)
    ]

instance RunMessage TheBookOfWarSunTzusLegacy where
  runMessage msg a@(TheBookOfWarSunTzusLegacy attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getPlayedEvent -> eid) _ -> do
      card <- fetchCard eid
      atEndOfTurn (attrs.ability 1) iid $ addToHand iid [card]
      pure a
    _ -> TheBookOfWarSunTzusLegacy <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.AlbrechtHusserlRecklessGambler (albrechtHusserlRecklessGambler) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Placement
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TracesToNowhere.Helpers

newtype AlbrechtHusserlRecklessGambler = AlbrechtHusserlRecklessGambler AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

albrechtHusserlRecklessGambler :: AssetCard AlbrechtHusserlRecklessGambler
albrechtHusserlRecklessGambler = allyWith AlbrechtHusserlRecklessGambler Cards.albrechtHusserlRecklessGambler (2, 2) noSlots

instance HasAbilities AlbrechtHusserlRecklessGambler where
  getAbilities (AlbrechtHusserlRecklessGambler a) =
    -- "{action}: Parley. (Codex E)." The bribe condition is printed in the codex,
    -- not on the card, and the entry lets it be triggered again, so the ability
    -- itself stays unrestricted.
    [restricted a 1 OnSameLocation parleyAction_]

instance RunMessage AlbrechtHusserlRecklessGambler where
  runMessage msg a@(AlbrechtHusserlRecklessGambler attrs) = runQueueT $ scenarioI18n $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeLocationIfNotInPlay_ Locations.tickTockClub
      selectForMaybeM (locationIs Locations.tickTockClub) $ place attrs . AtLocation
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      {- "If the investigators found record of Matilde's bribe, continue reading.
      Otherwise, Albrecht refuses to give a straight answer. Any investigator may
      trigger this codex again." -}
      codexEntry "codex.albrechtHusserl"
      whenM (remembered TheInvestigatorsFoundRecordOfMatildesBribe) do
        remember AlbrechtIsTheCulprit
        scenarioSpecific "theCulprit" ()
      pure a
    _ -> AlbrechtHusserlRecklessGambler <$> liftRunMessage msg attrs

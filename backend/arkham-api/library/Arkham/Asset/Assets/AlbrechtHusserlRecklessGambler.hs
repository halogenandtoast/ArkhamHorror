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
    [ restricted
        a
        1
        (OnSameLocation <> Remembered TheInvestigatorsFoundRecordOfMatildesBribe)
        parleyAction_
    ]

instance RunMessage AlbrechtHusserlRecklessGambler where
  runMessage msg a@(AlbrechtHusserlRecklessGambler attrs) = runQueueT $ scenarioI18n $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeLocationIfNotInPlay_ Locations.tickTockClub
      selectForMaybeM (locationIs Locations.tickTockClub) $ place attrs . AtLocation
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      codexEntry "codex.albrechtHusserl"
      remember AlbrechtIsTheCulprit
      scenarioSpecific "theCulprit" ()
      pure a
    _ -> AlbrechtHusserlRecklessGambler <$> liftRunMessage msg attrs

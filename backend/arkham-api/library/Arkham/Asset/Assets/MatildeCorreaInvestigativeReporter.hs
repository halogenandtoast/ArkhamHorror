module Arkham.Asset.Assets.MatildeCorreaInvestigativeReporter (matildeCorreaInvestigativeReporter) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TracesToNowhere.Helpers

newtype MatildeCorreaInvestigativeReporter = MatildeCorreaInvestigativeReporter AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

matildeCorreaInvestigativeReporter :: AssetCard MatildeCorreaInvestigativeReporter
matildeCorreaInvestigativeReporter =
  allyWith
    MatildeCorreaInvestigativeReporter
    Cards.matildeCorreaInvestigativeReporter
    (2, 2)
    noSlots

instance HasAbilities MatildeCorreaInvestigativeReporter where
  getAbilities (MatildeCorreaInvestigativeReporter a) =
    [ restricted
        a
        1
        ( OnSameLocation
            <> exists (locationWithAsset a <> LocationWithoutClues)
            <> Remembered TheInvestigatorsFoundRecordOfMatildesBribe
        )
        parleyAction_
    ]

instance RunMessage MatildeCorreaInvestigativeReporter where
  runMessage msg a@(MatildeCorreaInvestigativeReporter attrs) =
    runQueueT $ scenarioI18n $ case msg of
      Revelation _ (isSource attrs -> True) -> do
        placeLocationIfNotInPlay_ Locations.arkhamAdvertiser
        selectForMaybeM (locationIs Locations.arkhamAdvertiser) $ place attrs . AtLocation
        pure a
      UseThisAbility _ (isSource attrs -> True) 1 -> do
        codexEntry "codex.matildeCorrea"
        addToVictory_ attrs
        pure a
      _ -> MatildeCorreaInvestigativeReporter <$> liftRunMessage msg attrs

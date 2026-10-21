module Arkham.Asset.Assets.EugenePeelHidingSomething (eugenePeelHidingSomething) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Placement
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Trait (Trait (Syndicate))

newtype EugenePeelHidingSomething = EugenePeelHidingSomething AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eugenePeelHidingSomething :: AssetCard EugenePeelHidingSomething
eugenePeelHidingSomething =
  allyWith EugenePeelHidingSomething Cards.eugenePeelHidingSomething (2, 2) noSlots

instance HasAbilities EugenePeelHidingSomething where
  getAbilities (EugenePeelHidingSomething a) =
    [ restricted a 1 OnSameLocation
        $ freeReaction
        $ EnemyDefeated #after Anyone ByAny (EnemyWithTrait Syndicate <> EnemyAt (locationWithAsset a))
    ]

instance RunMessage EugenePeelHidingSomething where
  runMessage msg a@(EugenePeelHidingSomething attrs) = runQueueT $ scenarioI18n $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeLocationIfNotInPlay_ Locations.trainStation
      selectForMaybeM (locationIs Locations.trainStation) $ place attrs . AtLocation
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      codexEntry "codex.eugenePeel"
      remember EugeneIsNotWhatHeSeems
      scenarioSpecific "theCulprit" ()
      pure a
    _ -> EugenePeelHidingSomething <$> liftRunMessage msg attrs

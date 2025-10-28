module Arkham.Asset.Assets.RocketShipRattlingWithEnergy (rocketShipRattlingWithEnergy) where

import Arkham.Ability hiding (Cosmos, cosmos)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.ChaosToken
import Arkham.Helpers.Location
import Arkham.Helpers.Vehicle
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.FilmFatale.Helpers
import Arkham.Trait (Trait (Cosmos))

newtype RocketShipRattlingWithEnergy = RocketShipRattlingWithEnergy AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rocketShipRattlingWithEnergy :: AssetCard RocketShipRattlingWithEnergy
rocketShipRattlingWithEnergy = asset RocketShipRattlingWithEnergy Cards.rocketShipRattlingWithEnergy

instance HasAbilities RocketShipRattlingWithEnergy where
  getAbilities (RocketShipRattlingWithEnergy x) =
    [ vehicleEnterOrExitAbility x
    , scenarioI18n
        $ withI18nTooltip "rocketShip.move"
        $ fastAbility x 1 (GroupClueCost (PerPlayer 1) Anywhere) OnSameLocation
    ]

instance RunMessage RocketShipRattlingWithEnergy where
  runMessage msg a@(RocketShipRattlingWithEnergy attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) VehicleEnterExitAbility -> do
      enterOrExitVehicle iid a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cosmos <- select $ LocationWithTrait Cosmos
      chooseTargetM iid cosmos $ place attrs
      requestChaosTokens iid (attrs.ability 1) 1
      pure a
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      faces <- getModifiedChaosTokenFaces tokens
      continue_ iid
      when (any isSymbolChaosToken faces) do
        withLocationOf attrs \lid -> selectEach (InVehicleMatching (be attrs)) (`place` AtLocation lid)
        flipOverBy iid (attrs.ability 1) attrs
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      withLocationOf attrs \lid -> do
        let card = lookupCard Enemies.curiousMoonNosyNuisance (toCardId attrs)
        createEnemyAt_ card lid
        push $ Flipped (toSource attrs) card
      pure a
    _ -> RocketShipRattlingWithEnergy <$> liftRunMessage msg attrs

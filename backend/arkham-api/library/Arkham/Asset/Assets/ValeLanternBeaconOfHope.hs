module Arkham.Asset.Assets.ValeLanternBeaconOfHope (valeLanternBeaconOfHope, valeLanternBeaconOfHopeEffect) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_, modifySelect)
import Arkham.Helpers.Window (getRevealedLocation)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Trait (Trait (Dark, Forest))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype ValeLanternBeaconOfHope = ValeLanternBeaconOfHope AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeLanternBeaconOfHope :: AssetCard ValeLanternBeaconOfHope
valeLanternBeaconOfHope = asset ValeLanternBeaconOfHope Cards.valeLanternBeaconOfHope

getRevealer :: [Window] -> InvestigatorId
getRevealer [] = error "getRevealer: no RevealLocationForcedAbilities window"
getRevealer ((windowType -> Window.RevealLocationForcedAbilities iid' _ _) : _) = iid'
getRevealer (_ : xs) = getRevealer xs

instance HasModifiersFor ValeLanternBeaconOfHope where
  getModifiersFor (ValeLanternBeaconOfHope a) = do
    withLocationOf a \loc -> modifySelect a (orConnected_ $ LocationWithId loc) [RemoveTrait Dark]

instance HasAbilities ValeLanternBeaconOfHope where
  getAbilities (ValeLanternBeaconOfHope a) =
    [ controlled_ a 1
        $ triggered
          ( oneOf
              [ RevealLocationForcedAbilities
                  #when
                  (not_ You)
                  (LocationWithTrait Forest <> LocationWithAbility AbilityIsForcedAbility)
                  (Just YourLocation)
              , RevealLocationForcedAbilities
                  #when
                  You
                  (LocationWithTrait Forest <> LocationWithAbility AbilityIsForcedAbility)
                  Nothing
              ]
          )
          (exhaust a)
    , mkAbility a 2 $ forced (AssetWouldLeavePlay #when $ AssetWithId a.id)
    ]

instance RunMessage ValeLanternBeaconOfHope where
  runMessage msg a@(ValeLanternBeaconOfHope attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getRevealer &&& getRevealedLocation -> (iid, loc)) _ -> do
      createCardEffect Cards.valeLanternBeaconOfHope (effectMetaTarget loc) attrs iid
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      withLocationOf iid $ place attrs . AtLocation
      flipOverBy iid (attrs.ability 2) attrs
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.valeLanternExtinguishedLightBoon
      pure a
    _ -> ValeLanternBeaconOfHope <$> liftRunMessage msg attrs

newtype ValeLanternBeaconOfHopeEffect = ValeLanternBeaconOfHopeEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeLanternBeaconOfHopeEffect :: EffectArgs -> ValeLanternBeaconOfHopeEffect
valeLanternBeaconOfHopeEffect = cardEffect ValeLanternBeaconOfHopeEffect Cards.valeLanternBeaconOfHope

instance HasModifiersFor ValeLanternBeaconOfHopeEffect where
  getModifiersFor (ValeLanternBeaconOfHopeEffect a) = maybeModified_ a a.target do
    EffectMetaTarget (LocationTarget lid) <- hoistMaybe a.metadata
    pure
      [CannotTriggerAbilityMatching $ AbilityIsForcedAbility <> AbilityOnLocation (LocationWithId lid)]

instance RunMessage ValeLanternBeaconOfHopeEffect where
  runMessage msg e@(ValeLanternBeaconOfHopeEffect attrs) = runQueueT $ case msg of
    Do (CheckWindows ws) | any isRevealAfterWindow ws -> disableReturn e
    _ -> ValeLanternBeaconOfHopeEffect <$> liftRunMessage msg attrs
   where
    destLid =
      attrs.metadata >>= \case
        EffectMetaTarget (LocationTarget lid) -> Just lid
        _ -> Nothing
    isRevealAfterWindow w = case w.kind of
      Window.RevealLocation _ loc -> w.timing == #after && Just loc == destLid
      _ -> False

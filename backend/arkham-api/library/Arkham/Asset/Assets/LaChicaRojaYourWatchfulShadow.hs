module Arkham.Asset.Assets.LaChicaRojaYourWatchfulShadow (
  laChicaRojaYourWatchfulShadow,
  laChicaRojaYourWatchfulShadowEffect,
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Effect.Import
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelect)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Modifier (UIModifier (..))
import Arkham.Scenarios.SanguineShadows.Helpers
import Arkham.Spawn

newtype LaChicaRojaYourWatchfulShadow = LaChicaRojaYourWatchfulShadow AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laChicaRojaYourWatchfulShadow :: AssetCard LaChicaRojaYourWatchfulShadow
laChicaRojaYourWatchfulShadow = allyWith LaChicaRojaYourWatchfulShadow Cards.laChicaRojaYourWatchfulShadow (3, 3) noSlots

instance HasAbilities LaChicaRojaYourWatchfulShadow where
  getAbilities (LaChicaRojaYourWatchfulShadow a) =
    [controlled_ a 1 $ triggered (TurnEnds #when You) (exhaust a)]

instance RunMessage LaChicaRojaYourWatchfulShadow where
  runMessage msg a@(LaChicaRojaYourWatchfulShadow attrs) = runQueueT $ case msg of
    CardEnteredPlay _ card | card.id == attrs.cardId -> do
      keysFor attrs >>= traverse_ (`createScarletKeyAt_` AttachedToAsset attrs.id Nothing)
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        place iid InTheShadows
        createCardEffect Cards.laChicaRojaYourWatchfulShadow (effectMetaTarget lid) (attrs.ability 1) iid
      pure a
    _ -> LaChicaRojaYourWatchfulShadow <$> liftRunMessage msg attrs

newtype LaChicaRojaYourWatchfulShadowEffect = LaChicaRojaYourWatchfulShadowEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LaChicaRojaYourWatchfulShadowEffect where
  getModifiersFor (LaChicaRojaYourWatchfulShadowEffect a) = do
    for_ a.metaTarget.location \lid -> scenarioI18n $ do
      modified_ a lid [UIModifier $ ImportantToScenario $ ikey' "ui.lastKnownLocation"]

      for_ a.target.investigator \iid -> do
        modifySelect a AnyEnemy [ChangeSpawnWith iid $ SpawnAtLocation lid]

laChicaRojaYourWatchfulShadowEffect :: EffectArgs -> LaChicaRojaYourWatchfulShadowEffect
laChicaRojaYourWatchfulShadowEffect =
  cardEffectWith
    LaChicaRojaYourWatchfulShadowEffect
    Cards.laChicaRojaYourWatchfulShadow
    (setEffectMeta False)

instance RunMessage LaChicaRojaYourWatchfulShadowEffect where
  runMessage msg e@(LaChicaRojaYourWatchfulShadowEffect attrs) = runQueueT $ case msg of
    BeginTurn iid | isTarget iid attrs.target -> do
      mlid <- runMaybeT do
        lid <- hoistMaybe attrs.metaTarget.location
        liftGuardM (matches lid Anywhere)
        pure lid
      case mlid of
        Just lid -> place iid (AtLocation lid)
        Nothing -> do
          revealed <- select RevealedLocation
          chooseTargetM iid revealed $ place iid . AtLocation
      disableReturn e
    _ -> LaChicaRojaYourWatchfulShadowEffect <$> liftRunMessage msg attrs

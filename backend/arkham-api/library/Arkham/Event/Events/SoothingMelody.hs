module Arkham.Event.Events.SoothingMelody (soothingMelody) where

import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap

newtype SoothingMelody = SoothingMelody EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

soothingMelody :: EventCard SoothingMelody
soothingMelody = event SoothingMelody Cards.soothingMelody

instance RunMessage SoothingMelody where
  runMessage msg e@(SoothingMelody attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      doStep 1 msg
      push $ ApplyHealing (toSource attrs)
      drawCards iid attrs 1
      pure e
    DoStep n msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      modifiers' <- liftA2 (<>) (getModifiers (toCardId attrs)) (getModifiers attrs)
      let
        updateLimit :: Int -> ModifierType -> Int
        updateLimit x (MetaModifier (Object o)) =
          case fromJSON <$> KeyMap.lookup "use3" o of
            Just (Success True) -> 3
            _ -> x
        updateLimit x _ = x

      let limit = foldl' updateLimit 2 modifiers'
      let location = locationWithInvestigator iid
      damageInvestigators <- select $ HealableInvestigator (toSource attrs) #damage $ at_ location
      horrorInvestigators <- select $ HealableInvestigator (toSource attrs) #horror $ at_ location
      damageAssets <-
        select
          $ HealableAsset (toSource attrs) #damage
          $ at_ location
          <> #ally
          <> AssetControlledBy (affectsOthers Anyone)
      horrorAssets <-
        select
          $ HealableAsset (toSource attrs) #horror
          $ at_ location
          <> #ally
          <> AssetControlledBy (affectsOthers Anyone)

      chooseOneM iid do
        for_ damageInvestigators \i -> do
          damageLabeled i $ healDamageDelayed i attrs 1
        for_ horrorInvestigators \i -> do
          horrorLabeled i $ healHorrorDelayed i attrs 1
        for_ damageAssets \asset -> do
          assetDamageLabeled asset $ healDamageDelayed asset attrs 1
        for_ horrorAssets \asset -> do
          assetHorrorLabeled asset $ healHorrorDelayed asset attrs 1

      when (n < limit) $ doStep (n + 1) msg'
      pure e
    _ -> SoothingMelody <$> liftRunMessage msg attrs

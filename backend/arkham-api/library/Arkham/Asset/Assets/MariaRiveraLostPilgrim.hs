module Arkham.Asset.Assets.MariaRiveraLostPilgrim (mariaRivera) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Helpers.Window (damagedEnemy, damagedEnemyAmount, getThatEnemy)
import Arkham.Matcher
import Arkham.Trait (Trait (Cultist))
import Arkham.Window (Window, getBatchId, windowType)
import Arkham.Window qualified as Window

newtype MariaRiveraLostPilgrim = MariaRiveraLostPilgrim AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mariaRivera :: AssetCard MariaRiveraLostPilgrim
mariaRivera = ally MariaRiveraLostPilgrim Cards.mariaRivera (5, 0)

instance HasAbilities MariaRiveraLostPilgrim where
  getAbilities (MariaRiveraLostPilgrim a) =
    [ controlled_ a 1
        $ SilentForcedAbility
        $ EnemyWouldTakeDamage #when AnySource (EnemyAt (locationWithAsset a) <> EnemyWithTrait Cultist)
    , controlled_ a 2
        $ triggered
          (WouldPlaceDoomCounter #when AnySource (EnemyTargetMatches (EnemyWithTrait Cultist)))
          (exhaust a)
    , mkAbility a 3 $ forced $ AssetDefeated #when ByAny (be a)
    ]

getWouldPlaceDoomAmount :: [Window] -> Int
getWouldPlaceDoomAmount = \case
  ((windowType -> Window.WouldPlaceDoom _ _ n) : _) -> n
  (_ : rest) -> getWouldPlaceDoomAmount rest
  [] -> 0

instance RunMessage MariaRiveraLostPilgrim where
  runMessage msg a@(MariaRiveraLostPilgrim attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (damagedEnemy &&& damagedEnemyAmount -> (eid, n)) _ -> do
      -- The damage is placed, not dealt, so nothing is dealt to her controller
      reduceDamageTaken (attrs.ability 1) eid n
      placeTokens (attrs.ability 1) attrs #damage n
      checkDefeated (attrs.ability 1) attrs
      pure a
    UseCardAbility _iid (isSource attrs -> True) 2 ws@(getThatEnemy -> Just _eid) _ -> do
      -- Cancel the doom that would be placed on the Cultist enemy and place it on Maria as damage instead
      push $ CancelBatch (getBatchId ws)
      placeTokens (attrs.ability 2) attrs #damage (getWouldPlaceDoomAmount ws)
      checkDefeated (attrs.ability 2) attrs
      pure a
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      addToVictory_ attrs
      pure a
    _ -> MariaRiveraLostPilgrim <$> liftRunMessage msg attrs

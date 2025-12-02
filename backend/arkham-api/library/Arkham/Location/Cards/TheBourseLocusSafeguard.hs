module Arkham.Location.Cards.TheBourseLocusSafeguard (theBourseLocusSafeguard) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.DogsOfWar.Helpers

newtype TheBourseLocusSafeguard = TheBourseLocusSafeguard LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBourseLocusSafeguard :: LocationCard TheBourseLocusSafeguard
theBourseLocusSafeguard = symbolLabel $ location TheBourseLocusSafeguard Cards.theBourseLocusSafeguard 2 (PerPlayer 2)

instance HasModifiersFor TheBourseLocusSafeguard where
  getModifiersFor (TheBourseLocusSafeguard a) = do
    mTheClaretKnight <- selectOne $ assetIs Assets.theClaretKnightHerSwornChampion
    keyLocusTargets >>= \case
      [t] | maybe False (`isTarget` t) mTheClaretKnight -> pure ()
      _ -> do
        modifySelf a [CannotBeEnteredBy AnyEnemy]
        modifySelect a AnyEnemy [ChangeSpawnLocation (be a) (connectedTo (be a))]

instance HasAbilities TheBourseLocusSafeguard where
  getAbilities (TheBourseLocusSafeguard a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (oneOf [exists LocationWithAnyDoom, exists $ AssetControlledBy You <> AssetWithAnyDoom])
      $ actionAbilityWithCost
      $ GroupClueCost (PerPlayer 1) (be a)

instance RunMessage TheBourseLocusSafeguard where
  runMessage msg l@(TheBourseLocusSafeguard attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select LocationWithAnyDoom
      assets <- select $ assetControlledBy iid <> AssetWithAnyDoom
      chooseOneM iid do
        targets locations $ removeDoomFrom (attrs.ability 1) 1
        targets assets $ removeDoomFrom (attrs.ability 1) 1
      pure l
    _ -> TheBourseLocusSafeguard <$> liftRunMessage msg attrs

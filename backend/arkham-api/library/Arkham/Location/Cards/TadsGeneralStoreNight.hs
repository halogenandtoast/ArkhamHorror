module Arkham.Location.Cards.TadsGeneralStoreNight (tadsGeneralStoreNight) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (Resource))

newtype TadsGeneralStoreNight = TadsGeneralStoreNight LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tadsGeneralStoreNight :: LocationCard TadsGeneralStoreNight
tadsGeneralStoreNight = symbolLabel $ location TadsGeneralStoreNight Cards.tadsGeneralStoreNight 0 (Static 0)

instance HasModifiersFor TadsGeneralStoreNight where
  getModifiersFor (TadsGeneralStoreNight attrs) =
    modifySelect attrs (EnemyAt $ be attrs) [EnemyFight 1]

instance HasAbilities TadsGeneralStoreNight where
  getAbilities (TadsGeneralStoreNight a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ mkAbility a 1
      $ freeReaction
      $ PlacedToken #after AnySource (TargetIs $ toTarget a.id) Resource

instance RunMessage TadsGeneralStoreNight where
  runMessage msg l@(TadsGeneralStoreNight attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Resource 1
      pure l
    _ -> TadsGeneralStoreNight <$> liftRunMessage msg attrs

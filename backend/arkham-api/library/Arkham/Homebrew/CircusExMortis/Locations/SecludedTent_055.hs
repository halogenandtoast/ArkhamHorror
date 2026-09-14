module Arkham.Homebrew.CircusExMortis.Locations.SecludedTent_055 (secludedTent_055) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype Meta = Meta {shroudDiscountRounds :: Int}
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

newtype SecludedTent_055 = SecludedTent_055 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secludedTent_055 :: LocationCard SecludedTent_055
secludedTent_055 =
  locationWith SecludedTent_055 Cards.secludedTent_055 4 (Static 2) (setMeta $ Meta 0)

instance HasAbilities SecludedTent_055 where
  getAbilities (SecludedTent_055 a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ mkAbility a 1
      $ freeReaction (ChaosTokenReleased #after (InvestigatorAt (be a)) moonToken)

instance HasModifiersFor SecludedTent_055 where
  getModifiersFor (SecludedTent_055 a) = do
    let meta = getLocationMetaDefault (Meta 0) a
    when (meta.shroudDiscountRounds > 0) $ modifySelf a [ShroudModifier (-2)]

instance RunMessage SecludedTent_055 where
  runMessage msg (SecludedTent_055 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pure $ SecludedTent_055 $ attrs & setMeta (Meta 2)
    EndRound -> do
      let meta = getLocationMetaDefault (Meta 0) attrs
      pure $ SecludedTent_055 $ attrs & setMeta (Meta $ max 0 (meta.shroudDiscountRounds - 1))
    _ -> SecludedTent_055 <$> liftRunMessage msg attrs

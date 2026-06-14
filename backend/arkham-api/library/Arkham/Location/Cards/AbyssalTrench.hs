module Arkham.Location.Cards.AbyssalTrench (abyssalTrench) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (decreaseThisFloodLevel, increaseThisFloodLevel)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Seafloor))

newtype AbyssalTrench = AbyssalTrench LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abyssalTrench :: LocationCard AbyssalTrench
abyssalTrench = location AbyssalTrench Cards.abyssalTrench 0 (Static 1)

instance HasModifiersFor AbyssalTrench where
  getModifiersFor (AbyssalTrench a) = whenRevealed a do
    seafloorCount <- selectCount $ RevealedLocation <> LocationWithTrait Seafloor
    modifySelf a [ShroudModifier (seafloorCount `div` 2)]

instance HasAbilities AbyssalTrench where
  getAbilities (AbyssalTrench a) =
    if a.revealed
      then
        extendRevealed1 a
          $ groupLimit PerGame
          $ restricted a 1 Here
          $ freeReaction
          $ oneOf
            [ EnemyAttackedSuccessfully #after You AnySource (enemyAt a)
            , EnemyEvadedSuccessfully #after You AnySource (enemyAt a)
            ]
      else
        extendUnrevealed1 a
          $ mkAbility a 2
          $ forced
          $ Enters #when You (be a)

instance RunMessage AbyssalTrench where
  runMessage msg l@(AbyssalTrench attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      decreaseThisFloodLevel attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      increaseThisFloodLevel attrs
      floodable <- select $ connectedTo (be attrs) <> CanHaveFloodLevelIncreased
      chooseTargetM iid floodable increaseThisFloodLevel
      pure l
    _ -> AbyssalTrench <$> liftRunMessage msg attrs

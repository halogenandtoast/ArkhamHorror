module Arkham.Location.Cards.VaultedCorridor (vaultedCorridor) where

import Arkham.Ability
import Arkham.Distance
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.SkillTest.Target
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype VaultedCorridor = VaultedCorridor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultedCorridor :: LocationCard VaultedCorridor
vaultedCorridor = location VaultedCorridor Cards.vaultedCorridor 3 (PerPlayer 1)

instance HasAbilities VaultedCorridor where
  getAbilities (VaultedCorridor a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (Here <> exists (location_ $ not_ (be a) <> "Vaulted Corridor"))
      $ FastAbility Free

instance RunMessage VaultedCorridor where
  runMessage msg l@(VaultedCorridor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ls <- select $ location_ $ not_ (be attrs) <> "Vaulted Corridor"
      sid <- getRandom
      chooseOneM iid do
        for_ ls \lid -> do
          mdistance <- getDistance attrs.id lid
          for_ mdistance \(Distance distance) -> do
            targeting lid $ beginSkillTest sid iid (attrs.ability 1) lid #agility (Fixed distance)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTarget >>= \case
        Just (LocationTarget lid) -> moveTo (attrs.ability 1) iid lid
        _ -> pure ()
      pure l
    _ -> VaultedCorridor <$> liftRunMessage msg attrs

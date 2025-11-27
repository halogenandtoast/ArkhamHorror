module Arkham.Location.Cards.CondemnedGoldMine (condemnedGoldMine) where

import Arkham.Ability
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Token

newtype CondemnedGoldMine = CondemnedGoldMine LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

condemnedGoldMine :: LocationCard CondemnedGoldMine
condemnedGoldMine = location CondemnedGoldMine Cards.condemnedGoldMine 6 (Static 0)

instance HasAbilities CondemnedGoldMine where
  getAbilities (CondemnedGoldMine a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage CondemnedGoldMine where
  runMessage msg l@(CondemnedGoldMine attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      requestChaosTokens iid (attrs.ability 1) (1 + attrs.token Depletion)
      pure l
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      continue_ iid
      faces <- getModifiedChaosTokenFaces tokens
      when (none (`elem` faces) [#skull, #cultist, #tablet, #elderthing]) do
        gainResources iid (attrs.ability 1) 4
        placeTokensOn (attrs.ability 1) Depletion 1 attrs
      pure l
    _ -> CondemnedGoldMine <$> liftRunMessage msg attrs

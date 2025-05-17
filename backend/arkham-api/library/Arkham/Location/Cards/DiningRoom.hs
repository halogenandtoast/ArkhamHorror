module Arkham.Location.Cards.DiningRoom (diningRoom) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.GameValue
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DiningRoom = DiningRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diningRoom :: LocationCard DiningRoom
diningRoom = location DiningRoom Cards.diningRoom 2 (Static 0)

instance HasAbilities DiningRoom where
  getAbilities (DiningRoom a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (HealableInvestigator (a.ability 1) #horror You)) actionAbility

instance RunMessage DiningRoom where
  runMessage msg l@(DiningRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canHeal <- canHaveHorrorHealed (attrs.ability 1) iid
      when canHeal $ healHorror iid (attrs.ability 1) 1
      requestChaosTokens iid attrs 1
      pure l
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      resetChaosTokens attrs
      chaosTokenFaces <- getModifiedChaosTokenFaces tokens
      for_ chaosTokenFaces \chaosTokenFace ->
        when (chaosTokenFace `elem` [Skull, AutoFail]) do
          assignHorror iid attrs 1
          placeDoom (attrs.ability 1) attrs 1
      pure l
    _ -> DiningRoom <$> liftRunMessage msg attrs

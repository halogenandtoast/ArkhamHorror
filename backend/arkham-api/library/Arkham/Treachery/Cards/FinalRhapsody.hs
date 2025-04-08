module Arkham.Treachery.Cards.FinalRhapsody (finalRhapsody) where

import Arkham.ChaosToken
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FinalRhapsody = FinalRhapsody TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finalRhapsody :: TreacheryCard FinalRhapsody
finalRhapsody = treachery FinalRhapsody Cards.finalRhapsody

instance RunMessage FinalRhapsody where
  runMessage msg t@(FinalRhapsody attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      requestChaosTokens iid attrs 5
      pure t
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      let damageCount = count ((`elem` [Skull, AutoFail]) . chaosTokenFace) tokens
      when (damageCount > 0) do
        chooseOneM iid do
          labeled
            ("Take " <> tshow damageCount <> " damage and horror")
            $ assignDamageAndHorror iid attrs damageCount damageCount
      resetChaosTokens attrs
      pure t
    _ -> FinalRhapsody <$> liftRunMessage msg attrs

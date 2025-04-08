module Arkham.Treachery.Cards.FinalRhapsodyAdvanced (finalRhapsodyAdvanced) where

import Arkham.ChaosToken
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FinalRhapsodyAdvanced = FinalRhapsodyAdvanced TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finalRhapsodyAdvanced :: TreacheryCard FinalRhapsodyAdvanced
finalRhapsodyAdvanced = treachery FinalRhapsodyAdvanced Cards.finalRhapsodyAdvanced

instance RunMessage FinalRhapsodyAdvanced where
  runMessage msg t@(FinalRhapsodyAdvanced attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      requestChaosTokens iid attrs 5
      pure t
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      let damageCount = count (`notElem` [#eldersign, #bless]) $ filter isSymbolChaosToken $ map (.face) tokens
      when (damageCount > 0) do
        chooseOneM iid do
          labeled ("Take " <> tshow damageCount <> " damage and horror")
            $ assignDamageAndHorror iid attrs damageCount damageCount
      resetChaosTokens attrs
      pure t
    _ -> FinalRhapsodyAdvanced <$> liftRunMessage msg attrs

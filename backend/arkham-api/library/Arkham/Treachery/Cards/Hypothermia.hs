module Arkham.Treachery.Cards.Hypothermia (hypothermia) where

import Arkham.Helpers.ChaosBag
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenarios.ToTheForbiddenPeaks.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Hypothermia = Hypothermia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypothermia :: TreacheryCard Hypothermia
hypothermia = treachery Hypothermia Cards.hypothermia

instance RunMessage Hypothermia where
  runMessage msg t@(Hypothermia attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- genId
      getLevel iid >>= \case
        x | x `elem` [4, 5] -> skillTestModifier sid attrs sid RevealAnotherChaosToken
        _ -> pure ()
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      chooseOneM iid do
        whenM hasRemainingFrostTokens do
          labeled "Add 1 {frost} token to the chaos bag" $ addChaosToken #frost
        labeled ("Take 1 horror for each point you failed by (" <> tshow n <> ")")
          $ assignHorror iid attrs n
      pure t
    _ -> Hypothermia <$> liftRunMessage msg attrs

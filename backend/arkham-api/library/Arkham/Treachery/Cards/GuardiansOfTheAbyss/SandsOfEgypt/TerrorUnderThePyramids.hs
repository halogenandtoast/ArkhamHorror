module Arkham.Treachery.Cards.GuardiansOfTheAbyss.SandsOfEgypt.TerrorUnderThePyramids (terrorUnderThePyramids) where

import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Matcher
import Arkham.Treachery.CardDefs.GuardiansOfTheAbyss.SandsOfEgypt qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TerrorUnderThePyramids = TerrorUnderThePyramids TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorUnderThePyramids :: TreacheryCard TerrorUnderThePyramids
terrorUnderThePyramids = treachery TerrorUnderThePyramids Cards.terrorUnderThePyramids

instance RunMessage TerrorUnderThePyramids where
  runMessage msg t@(TerrorUnderThePyramids attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n (FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      cards <- select $ inHandOf NotForPlay iid <> basic DiscardableCard
      if null cards
        then assignHorror iid attrs n
        else do
          chooseAndDiscardCard iid attrs
          doNextStep msg
      pure t
    _ -> TerrorUnderThePyramids <$> liftRunMessage msg attrs

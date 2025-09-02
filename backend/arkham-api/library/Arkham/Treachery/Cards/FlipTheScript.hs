module Arkham.Treachery.Cards.FlipTheScript (flipTheScript) where

import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FlipTheScript = FlipTheScript TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flipTheScript :: TreacheryCard FlipTheScript
flipTheScript = treachery FlipTheScript Cards.flipTheScript

instance RunMessage FlipTheScript where
  runMessage msg t@(FlipTheScript attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs sid SkillIconsSubtract
      revelationSkillTest sid iid attrs #intellect (Fixed 0)
      pure t
    PassedThisSkillTestBy _iid (isSource attrs -> True) n | n > 0 -> do
      doStep (min 3 n) msg
      pure t
    DoStep n msg'@(PassedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      playerClueCount <- field InvestigatorClues iid
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1
        when (playerClueCount > 0) do
          countVar 1 $ labeled' "placeCluesOnYourLocation" $ placeCluesOnLocation iid attrs n
      doStep (n - 1) msg'
      pure t
    _ -> FlipTheScript <$> liftRunMessage msg attrs

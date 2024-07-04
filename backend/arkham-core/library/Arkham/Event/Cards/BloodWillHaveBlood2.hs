module Arkham.Event.Cards.BloodWillHaveBlood2 (bloodWillHaveBlood2, BloodWillHaveBlood2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (getAttackDetails)

newtype BloodWillHaveBlood2 = BloodWillHaveBlood2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodWillHaveBlood2 :: EventCard BloodWillHaveBlood2
bloodWillHaveBlood2 = event BloodWillHaveBlood2 Cards.bloodWillHaveBlood2

instance RunMessage BloodWillHaveBlood2 where
  runMessage msg e@(BloodWillHaveBlood2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let details = getAttackDetails attrs.windows
      drawCardsIfCan iid attrs $ sum . map (uncurry (+)) $ toList details.damaged
      pure e
    _ -> BloodWillHaveBlood2 <$> liftRunMessage msg attrs

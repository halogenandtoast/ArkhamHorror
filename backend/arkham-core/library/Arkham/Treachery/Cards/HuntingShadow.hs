module Arkham.Treachery.Cards.HuntingShadow where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Investigator
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype HuntingShadow = HuntingShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingShadow :: TreacheryCard HuntingShadow
huntingShadow = treachery HuntingShadow Cards.huntingShadow

instance RunMessage HuntingShadow where
  runMessage msg t@(HuntingShadow attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      canSpendClues <- getCanSpendNClues iid 1
      push
        $ chooseOrRunOne iid
        $ [Label "Spend 1 clue" [SpendClues 1 [iid]] | canSpendClues]
          <> [Label "Take 2 damage" [assignDamage iid attrs 2]]
      pure t
    _ -> HuntingShadow <$> runMessage msg attrs

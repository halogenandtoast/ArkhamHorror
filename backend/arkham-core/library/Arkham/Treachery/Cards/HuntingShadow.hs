module Arkham.Treachery.Cards.HuntingShadow where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner
import Arkham.Treachery.Helpers
import Arkham.Treachery.Cards qualified as Cards

newtype HuntingShadow = HuntingShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingShadow :: TreacheryCard HuntingShadow
huntingShadow = treachery HuntingShadow Cards.huntingShadow

instance RunMessage HuntingShadow where
  runMessage msg t@(HuntingShadow attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      playerSpendableClueCount <- getSpendableClueCount [iid]
      push $ if playerSpendableClueCount > 0
        then chooseOne
          iid
          [ Label "Spend 1 clue" [SpendClues 1 [iid]]
          , Label
            "Take 2 damage"
            [InvestigatorAssignDamage iid source DamageAny 2 0]
          ]
        else InvestigatorAssignDamage iid source DamageAny 2 0
      pure t
    _ -> HuntingShadow <$> runMessage msg attrs

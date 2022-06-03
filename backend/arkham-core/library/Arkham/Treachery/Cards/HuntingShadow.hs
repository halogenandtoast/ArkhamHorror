module Arkham.Treachery.Cards.HuntingShadow where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Query
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype HuntingShadow = HuntingShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingShadow :: TreacheryCard HuntingShadow
huntingShadow = treachery HuntingShadow Cards.huntingShadow

instance TreacheryRunner env => RunMessage HuntingShadow where
  runMessage msg t@(HuntingShadow attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      playerSpendableClueCount <- unSpendableClueCount <$> getCount iid
      if playerSpendableClueCount > 0
        then t <$ push
          (chooseOne
            iid
            [ Label "Spend 1 clue" [SpendClues 1 [iid]]
            , Label
              "Take 2 damage"
              [InvestigatorAssignDamage iid source DamageAny 2 0]
            ]
          )
        else t <$ push (InvestigatorAssignDamage iid source DamageAny 2 0)
    _ -> HuntingShadow <$> runMessage msg attrs

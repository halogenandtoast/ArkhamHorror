module Arkham.Types.Treachery.Cards.HuntingShadow where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.TreacheryId


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype HuntingShadow = HuntingShadow TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingShadow :: TreacheryId -> a -> HuntingShadow
huntingShadow uuid _ = HuntingShadow $ baseAttrs uuid "01135"

instance HasModifiersFor env HuntingShadow where
  getModifiersFor = noModifiersFor

instance HasActions env HuntingShadow where
  getActions i window (HuntingShadow attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env HuntingShadow where
  runMessage msg t@(HuntingShadow attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      playerSpendableClueCount <- unSpendableClueCount <$> getCount iid
      if playerSpendableClueCount > 0
        then t <$ unshiftMessages
          [ chooseOne
            iid
            [ Label "Spend 1 clue" [SpendClues 1 [iid]]
            , Label
              "Take 2 damage"
              [InvestigatorAssignDamage iid source DamageAny 2 0]
            ]
          , Discard $ toTarget attrs
          ]
        else t <$ unshiftMessages
          [InvestigatorAssignDamage iid source DamageAny 2 0, Discard $ toTarget attrs]
    _ -> HuntingShadow <$> runMessage msg attrs

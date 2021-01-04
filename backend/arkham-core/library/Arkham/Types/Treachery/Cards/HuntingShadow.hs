{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Treachery.Cards.HuntingShadow where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype HuntingShadow = HuntingShadow Attrs
  deriving newtype (Show, ToJSON, FromJSON)

huntingShadow :: TreacheryId -> a -> HuntingShadow
huntingShadow uuid _ = HuntingShadow $ baseAttrs uuid "01135"

instance HasModifiersFor env HuntingShadow where
  getModifiersFor = noModifiersFor

instance HasActions env HuntingShadow where
  getActions i window (HuntingShadow attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env HuntingShadow where
  runMessage msg t@(HuntingShadow attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      playerSpendableClueCount <- unSpendableClueCount <$> getCount iid
      if playerSpendableClueCount > 0
        then t <$ unshiftMessages
          [ chooseOne
            iid
            [ Label "Spend 1 clue" [SpendClues 1 [iid]]
            , Label "Take 2 damage" [InvestigatorAssignDamage iid source 2 0]
            ]
          , Discard $ toTarget attrs
          ]
        else t <$ unshiftMessages
          [InvestigatorAssignDamage iid source 2 0, Discard $ toTarget attrs]
    _ -> HuntingShadow <$> runMessage msg attrs

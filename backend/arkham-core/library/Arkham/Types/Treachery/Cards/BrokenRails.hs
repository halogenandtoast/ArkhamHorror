module Arkham.Types.Treachery.Cards.BrokenRails
  ( brokenRails
  , BrokenRails(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.TreacheryId


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype BrokenRails = BrokenRails TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenRails :: TreacheryId -> a -> BrokenRails
brokenRails uuid _ = BrokenRails $ baseAttrs uuid "02181"

instance HasModifiersFor env BrokenRails where
  getModifiersFor = noModifiersFor

instance HasActions env BrokenRails where
  getActions i window (BrokenRails attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env BrokenRails where
  runMessage msg t@(BrokenRails attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      investigatorIds <- getSetList lid
      investigatorsWhoMustDiscard <- flip filterM investigatorIds $ \iid' -> do
        damageCount <- unDamageCount <$> getCount iid'
        pure $ damageCount >= 4
      t <$ unshiftMessages
        ([ LoseActions iid' source 1 | iid' <- investigatorIds ]
        <> [ ChooseAndDiscardAsset iid'
           | iid' <- investigatorsWhoMustDiscard
           ]
        <> [Discard (toTarget attrs)]
        )
    _ -> BrokenRails <$> runMessage msg attrs

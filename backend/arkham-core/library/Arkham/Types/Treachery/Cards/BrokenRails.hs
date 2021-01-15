module Arkham.Types.Treachery.Cards.BrokenRails
  ( brokenRails
  , BrokenRails(..)
  ) where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype BrokenRails = BrokenRails Attrs
  deriving newtype (Show, ToJSON, FromJSON)

brokenRails :: TreacheryId -> a -> BrokenRails
brokenRails uuid _ = BrokenRails $ baseAttrs uuid "02180"

instance HasModifiersFor env BrokenRails where
  getModifiersFor = noModifiersFor

instance HasActions env BrokenRails where
  getActions i window (BrokenRails attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env BrokenRails where
  runMessage msg t@(BrokenRails attrs@Attrs {..}) = case msg of
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

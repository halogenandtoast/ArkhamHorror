module Arkham.Types.Treachery.Cards.BrokenRails
  ( brokenRails
  , BrokenRails(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype BrokenRails = BrokenRails TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenRails :: TreacheryCard BrokenRails
brokenRails = treachery BrokenRails Cards.brokenRails

instance TreacheryRunner env => RunMessage env BrokenRails where
  runMessage msg t@(BrokenRails attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      investigatorIds <- getSetList lid
      investigatorsWhoMustDiscard <- flip filterM investigatorIds $ \iid' -> do
        damageCount <- unDamageCount <$> getCount iid'
        pure $ damageCount >= 4
      t <$ pushAll
        ([ LoseActions iid' source 1 | iid' <- investigatorIds ]
        <> [ ChooseAndDiscardAsset iid' AnyAsset
           | iid' <- investigatorsWhoMustDiscard
           ]
        <> [Discard (toTarget attrs)]
        )
    _ -> BrokenRails <$> runMessage msg attrs

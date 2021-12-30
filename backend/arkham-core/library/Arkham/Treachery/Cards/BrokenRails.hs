module Arkham.Treachery.Cards.BrokenRails
  ( brokenRails
  , BrokenRails(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Query
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype BrokenRails = BrokenRails TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
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
        )
    _ -> BrokenRails <$> runMessage msg attrs

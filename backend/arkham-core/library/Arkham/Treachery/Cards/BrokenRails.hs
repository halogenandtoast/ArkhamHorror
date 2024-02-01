module Arkham.Treachery.Cards.BrokenRails (
  brokenRails,
  BrokenRails (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BrokenRails = BrokenRails TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

brokenRails :: TreacheryCard BrokenRails
brokenRails = treachery BrokenRails Cards.brokenRails

instance RunMessage BrokenRails where
  runMessage msg t@(BrokenRails attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      investigatorIds <- selectList $ colocatedWith iid
      investigatorsWhoMustDiscard <-
        filterM
          (fieldP InvestigatorDamage (>= 4))
          investigatorIds
      pushAll
        $ [LoseActions iid' source 1 | iid' <- investigatorIds]
        <> [ ChooseAndDiscardAsset iid' (toSource attrs) AnyAsset
           | iid' <- investigatorsWhoMustDiscard
           ]
      pure t
    _ -> BrokenRails <$> runMessage msg attrs

module Arkham.Types.Treachery.Cards.AbandonedAndAlone where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards (abandonedAndAlone)
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Treachery.Attrs

newtype AbandonedAndAlone = AbandonedAndAlone TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedAndAlone :: TreacheryCard AbandonedAndAlone
abandonedAndAlone = treachery AbandonedAndAlone Cards.abandonedAndAlone

instance HasModifiersFor env AbandonedAndAlone

instance HasAbilities env AbandonedAndAlone where
  getAbilities i window (AbandonedAndAlone attrs) = getAbilities i window attrs

instance RunMessage env AbandonedAndAlone where
  runMessage msg t@(AbandonedAndAlone attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ pushAll
        [ InvestigatorDirectDamage iid source 0 2
        , RemoveDiscardFromGame iid
        , Discard $ toTarget attrs
        ]
    _ -> AbandonedAndAlone <$> runMessage msg attrs

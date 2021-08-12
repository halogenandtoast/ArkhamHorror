module Arkham.Types.Treachery.Cards.ArousingSuspicions
  ( ArousingSuspicions(..)
  , arousingSuspicions
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype ArousingSuspicions = ArousingSuspicions TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arousingSuspicions :: TreacheryCard ArousingSuspicions
arousingSuspicions = treachery ArousingSuspicions Cards.arousingSuspicions

instance TreacheryRunner env => RunMessage env ArousingSuspicions where
  runMessage msg t@(ArousingSuspicions attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      criminals <- getSetList @EnemyId ([Criminal], lid)
      if null criminals
        then t <$ pushAll [SpendResources iid 2, Discard $ toTarget attrs]
        else t <$ pushAll
          ([ PlaceDoom (EnemyTarget eid) 1 | eid <- criminals ]
          <> [Discard $ toTarget attrs]
          )
    _ -> ArousingSuspicions <$> runMessage msg attrs

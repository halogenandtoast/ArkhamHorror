module Arkham.Types.Treachery.Cards.ArousingSuspicions
  ( ArousingSuspicions(..)
  , arousingSuspicions
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype ArousingSuspicions = ArousingSuspicions TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arousingSuspicions :: TreacheryCard ArousingSuspicions
arousingSuspicions = treachery ArousingSuspicions Cards.arousingSuspicions

instance TreacheryRunner env => RunMessage env ArousingSuspicions where
  runMessage msg t@(ArousingSuspicions attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      criminals <- getSetList @EnemyId ([Criminal], lid)
      if null criminals
        then t <$ push (SpendResources iid 2)
        else t <$ pushAll [ PlaceDoom (EnemyTarget eid) 1 | eid <- criminals ]
    _ -> ArousingSuspicions <$> runMessage msg attrs

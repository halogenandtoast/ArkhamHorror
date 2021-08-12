module Arkham.Types.Treachery.Cards.InsatiableBloodlust where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype InsatiableBloodlust = InsatiableBloodlust TreacheryAttrs
  deriving anyclass (IsTreachery, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

insatiableBloodlust :: TreacheryCard InsatiableBloodlust
insatiableBloodlust = treachery InsatiableBloodlust Cards.insatiableBloodlust

instance HasModifiersFor env InsatiableBloodlust where
  getModifiersFor _ (EnemyTarget eid) (InsatiableBloodlust attrs)
    | treacheryOnEnemy eid attrs = pure $ toModifiers
      attrs
      [EnemyFight 1, DamageDealt 1, HorrorDealt 1, CannotBeEvaded]
  getModifiersFor _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env InsatiableBloodlust where
  runMessage msg t@(InsatiableBloodlust attrs@TreacheryAttrs {..}) =
    case msg of
      Revelation _iid source | isSource attrs source -> do
        mrougarou <- fmap unStoryEnemyId <$> getId (CardCode "81028")
        case mrougarou of
          Nothing -> error "can't happen"
          Just eid -> do
            push (AttachTreachery treacheryId (EnemyTarget eid))
        InsatiableBloodlust <$> runMessage msg attrs
      EnemyDamage eid _ _ n | n > 0 -> do
        mrougarou <- fmap unStoryEnemyId <$> getId (CardCode "81028")
        t <$ when (mrougarou == Just eid) (push (Discard $ toTarget attrs))
      _ -> InsatiableBloodlust <$> runMessage msg attrs

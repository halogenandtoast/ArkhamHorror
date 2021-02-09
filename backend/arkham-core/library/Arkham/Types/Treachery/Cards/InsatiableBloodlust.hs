module Arkham.Types.Treachery.Cards.InsatiableBloodlust where


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype InsatiableBloodlust = InsatiableBloodlust TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

insatiableBloodlust :: TreacheryId -> a -> InsatiableBloodlust
insatiableBloodlust uuid _ = InsatiableBloodlust $ baseAttrs uuid "81036"

instance HasModifiersFor env InsatiableBloodlust where
  getModifiersFor _ (EnemyTarget eid) (InsatiableBloodlust attrs)
    | treacheryOnEnemy eid attrs = pure $ toModifiers
      attrs
      [EnemyFight 1, DamageDealt 1, HorrorDealt 1, CannotBeEvaded]
  getModifiersFor _ _ _ = pure []

instance HasActions env InsatiableBloodlust where
  getActions i window (InsatiableBloodlust attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env InsatiableBloodlust where
  runMessage msg t@(InsatiableBloodlust attrs@TreacheryAttrs {..}) = case msg of
    Revelation _iid source | isSource attrs source -> do
      mrougarou <- fmap unStoryEnemyId <$> getId (CardCode "81028")
      case mrougarou of
        Nothing -> error "can't happen"
        Just eid -> do
          unshiftMessage (AttachTreachery treacheryId (EnemyTarget eid))
      InsatiableBloodlust <$> runMessage msg attrs
    EnemyDamage eid _ _ n | n > 0 -> do
      mrougarou <- fmap unStoryEnemyId <$> getId (CardCode "81028")
      t <$ when
        (mrougarou == Just eid)
        (unshiftMessage (Discard $ toTarget attrs))
    _ -> InsatiableBloodlust <$> runMessage msg attrs

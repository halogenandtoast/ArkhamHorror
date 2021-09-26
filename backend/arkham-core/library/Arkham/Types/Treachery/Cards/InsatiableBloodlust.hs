module Arkham.Types.Treachery.Cards.InsatiableBloodlust where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype InsatiableBloodlust = InsatiableBloodlust TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

insatiableBloodlust :: TreacheryCard InsatiableBloodlust
insatiableBloodlust = treachery InsatiableBloodlust Cards.insatiableBloodlust

instance HasModifiersFor env InsatiableBloodlust where
  getModifiersFor _ (EnemyTarget eid) (InsatiableBloodlust attrs)
    | treacheryOnEnemy eid attrs = pure $ toModifiers
      attrs
      [EnemyFight 1, DamageDealt 1, HorrorDealt 1, CannotBeEvaded]
  getModifiersFor _ _ _ = pure []

instance HasAbilities InsatiableBloodlust where
  getAbilities (InsatiableBloodlust x) =
    [ mkAbility x 1
        $ ForcedAbility
        $ EnemyDealtDamage Timing.After AnyDamageEffect
        (enemyIs Cards.theRougarou)
        (SourceIs $ toSource x)
    ]

instance TreacheryRunner env => RunMessage env InsatiableBloodlust where
  runMessage msg t@(InsatiableBloodlust attrs@TreacheryAttrs {..}) =
    case msg of
      Revelation _iid source | isSource attrs source -> do
        mrougarou <- selectOne $ enemyIs Cards.theRougarou
        case mrougarou of
          Nothing -> error "can't happen"
          Just eid -> do
            push (AttachTreachery treacheryId (EnemyTarget eid))
        InsatiableBloodlust <$> runMessage msg attrs
      UseCardAbility _ source _ 1 _ | isSource attrs source ->
        t <$ push (Discard $ toTarget attrs)
      _ -> InsatiableBloodlust <$> runMessage msg attrs

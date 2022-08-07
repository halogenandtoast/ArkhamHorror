module Arkham.Treachery.Cards.InsatiableBloodlust where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype InsatiableBloodlust = InsatiableBloodlust TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

insatiableBloodlust :: TreacheryCard InsatiableBloodlust
insatiableBloodlust = treachery InsatiableBloodlust Cards.insatiableBloodlust

instance HasModifiersFor InsatiableBloodlust where
  getModifiersFor (EnemyTarget eid) (InsatiableBloodlust attrs)
    | treacheryOnEnemy eid attrs = pure $ toModifiers
      attrs
      [EnemyFight 1, DamageDealt 1, HorrorDealt 1, CannotBeEvaded]
  getModifiersFor _ _ = pure []

instance HasAbilities InsatiableBloodlust where
  getAbilities (InsatiableBloodlust x) =
    [ mkAbility x 1 $ ForcedAbility $ EnemyDealtDamage
        Timing.After
        AnyDamageEffect
        (enemyIs Cards.theRougarou)
        AnySource
    ]

instance RunMessage InsatiableBloodlust where
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

module Arkham.Treachery.Cards.InsatiableBloodlust where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype InsatiableBloodlust = InsatiableBloodlust TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

insatiableBloodlust :: TreacheryCard InsatiableBloodlust
insatiableBloodlust = treachery InsatiableBloodlust Cards.insatiableBloodlust

instance HasModifiersFor InsatiableBloodlust where
  getModifiersFor (EnemyTarget eid) (InsatiableBloodlust attrs)
    | treacheryOnEnemy eid attrs =
        pure
          $ toModifiers
            attrs
            [EnemyFight 1, DamageDealt 1, HorrorDealt 1, CannotBeEvaded]
  getModifiersFor _ _ = pure []

instance HasAbilities InsatiableBloodlust where
  getAbilities (InsatiableBloodlust x) =
    [ mkAbility x 1
        $ ForcedAbility
        $ EnemyDealtDamage
          #after
          AnyDamageEffect
          (enemyIs Cards.theRougarou)
          AnySource
    ]

instance RunMessage InsatiableBloodlust where
  runMessage msg t@(InsatiableBloodlust attrs@TreacheryAttrs {..}) =
    case msg of
      Revelation _iid source | isSource attrs source -> do
        rougarou <- selectJust $ enemyIs Cards.theRougarou
        push $ attachTreachery treacheryId rougarou
        pure t
      UseCardAbility _ source 1 _ _ | isSource attrs source -> do
        push $ toDiscard (toAbilitySource attrs 1) attrs
        pure t
      _ -> InsatiableBloodlust <$> runMessage msg attrs

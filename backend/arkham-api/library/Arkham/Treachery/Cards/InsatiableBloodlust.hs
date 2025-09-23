module Arkham.Treachery.Cards.InsatiableBloodlust (insatiableBloodlust) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InsatiableBloodlust = InsatiableBloodlust TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

insatiableBloodlust :: TreacheryCard InsatiableBloodlust
insatiableBloodlust = treachery InsatiableBloodlust Cards.insatiableBloodlust

instance HasModifiersFor InsatiableBloodlust where
  getModifiersFor (InsatiableBloodlust attrs) = case attrs.placement of
    AttachedToEnemy eid -> modified_ attrs eid [EnemyFight 1, DamageDealt 1, HorrorDealt 1, CannotBeEvaded]
    _ -> pure mempty

instance HasAbilities InsatiableBloodlust where
  getAbilities (InsatiableBloodlust x) =
    [ mkAbility x 1
        $ forced
        $ EnemyDealtDamage #after AnyDamageEffect (enemyIs Cards.theRougarou) AnySource
    ]

instance RunMessage InsatiableBloodlust where
  runMessage msg t@(InsatiableBloodlust attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      rougarou <- selectJust $ enemyIs Cards.theRougarou
      attachTreachery attrs rougarou
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> InsatiableBloodlust <$> liftRunMessage msg attrs

module Arkham.Enemy.Cards.SerpentOfTenochtitlan (serpentOfTenochtitlan, SerpentOfTenochtitlan (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Ancient))
import Arkham.Treachery.Cards qualified as Treacheries

newtype SerpentOfTenochtitlan = SerpentOfTenochtitlan EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serpentOfTenochtitlan :: EnemyCard SerpentOfTenochtitlan
serpentOfTenochtitlan = enemy SerpentOfTenochtitlan Cards.serpentOfTenochtitlan (3, Static 5, 3) (1, 1)

instance HasModifiersFor SerpentOfTenochtitlan where
  getModifiersFor (SerpentOfTenochtitlan a) = do
    atAncientLocation <- selectAny $ EnemyWithId a.id <> at_ (LocationWithTrait Ancient)
    modifySelf a
      $ if atAncientLocation
        then [AddKeyword Retaliate, AddKeyword Alert]
        else [AddKeyword Hunter]

instance HasAbilities SerpentOfTenochtitlan where
  getAbilities (SerpentOfTenochtitlan a) =
    extend1 a
      $ restricted a 1 (youExist $ not_ (HasMatchingTreachery $ treacheryIs Treacheries.poisoned))
      $ forced
      $ DealtDamage #after (SourceIsEnemyAttack $ be a) You

instance RunMessage SerpentOfTenochtitlan where
  runMessage msg e@(SerpentOfTenochtitlan attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      poisoned <- getSetAsidePoisoned
      push $ CreateWeaknessInThreatArea poisoned iid
      pure e
    _ -> SerpentOfTenochtitlan <$> runMessage msg attrs

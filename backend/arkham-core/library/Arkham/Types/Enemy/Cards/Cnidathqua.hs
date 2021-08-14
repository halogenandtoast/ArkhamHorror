module Arkham.Types.Enemy.Cards.Cnidathqua
  ( cnidathqua
  , Cnidathqua(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Resolution
import Arkham.Types.Target
import Arkham.Types.Window

newtype Cnidathqua = Cnidathqua EnemyAttrs
    deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cnidathqua :: EnemyCard Cnidathqua
cnidathqua = enemyWith
  Cnidathqua
  Cards.cnidathqua
  (4, PerPlayer 8, 0)
  (2, 2)
  (asSelfLocationL ?~ "cnidathqua")

instance HasModifiersFor env Cnidathqua where
  getModifiersFor _ (EnemyTarget eid) (Cnidathqua attrs) | eid == toId attrs =
    pure $ toModifiers attrs [CannotBeEvaded, CanBeFoughtAsIfAtYourLocation]
  getModifiersFor _ _ _ = pure []

instance EnemyAttrsHasAbilities env => HasAbilities env Cnidathqua where
  getAbilities i (AfterFailAttackEnemy who eid) (Cnidathqua attrs)
    | eid == toId attrs && i == who = pure [mkAbility attrs 1 ForcedAbility]
  getAbilities i window (Cnidathqua attrs) = getAbilities i window attrs

instance EnemyAttrsRunMessage env => RunMessage env Cnidathqua where
  runMessage msg e@(Cnidathqua attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      e <$ push
        (FindEncounterCard
          iid
          (toTarget attrs)
          (CardWithTitle "Writhing Appendage")
        )
    FoundEncounterCard iid target card | isTarget attrs target -> do
      lid <- getId @LocationId iid
      e <$ push (SpawnEnemyAtEngagedWith (EncounterCard card) lid iid)
    EnemyDefeated eid _ _ _ _ _ | eid == enemyId attrs -> do
      e <$ push (ScenarioResolution $ Resolution 2)
    _ -> Cnidathqua <$> runMessage msg attrs

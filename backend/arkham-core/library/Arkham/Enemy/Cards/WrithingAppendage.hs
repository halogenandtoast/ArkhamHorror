module Arkham.Enemy.Cards.WrithingAppendage
  ( writhingAppendage
  , WrithingAppendage(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Discard
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Message hiding ( EnemyAttacks, EnemyDefeated )
import Arkham.Message qualified as Msg
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Timing qualified as Timing

newtype WrithingAppendage = WrithingAppendage EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

writhingAppendage :: EnemyCard WrithingAppendage
writhingAppendage =
  enemy WrithingAppendage Cards.writhingAppendage (2, Static 2, 4) (1, 0)

instance HasAbilities WrithingAppendage where
  getAbilities (WrithingAppendage attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
    $ ForcedAbility
    $ EnemyAttacks Timing.After You AnyEnemyAttack
    $ EnemyWithId
    $ toId attrs
    , mkAbility attrs 2
    $ ForcedAbility
    $ EnemyDefeated Timing.When Anyone
    $ EnemyWithId
    $ toId attrs
    ]

instance RunMessage WrithingAppendage where
  runMessage msg e@(WrithingAppendage attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ toMessage $ randomDiscard iid attrs
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      -- TODO: Damage here should not be dealt from an investigator to avoid
      -- triggering any abilities
      mCnidathquaId <- getCnidathqua
      for_ mCnidathquaId $ \cnidathquaId ->
        push $ Msg.EnemyDamage cnidathquaId $ nonAttack iid 1
      pure e
    _ -> WrithingAppendage <$> runMessage msg attrs

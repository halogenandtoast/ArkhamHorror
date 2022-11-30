module Arkham.Enemy.Cards.WrithingAppendage
  ( writhingAppendage
  , WrithingAppendage(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Message hiding ( EnemyAttacks, EnemyDefeated )
import Arkham.Message qualified as Msg
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Source
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
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      e <$ push (RandomDiscard iid)
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      -- TODO: Damage here should not be dealt from an investigator to avoid
      -- triggering any abilities
      mCnidathquaId <- getCnidathqua
      case mCnidathquaId of
        Just cnidathquaId -> push $ Msg.EnemyDamage
          cnidathquaId
          (InvestigatorSource iid)
          NonAttackDamageEffect
          1
        Nothing -> pure ()
      WrithingAppendage <$> runMessage msg attrs
    _ -> WrithingAppendage <$> runMessage msg attrs

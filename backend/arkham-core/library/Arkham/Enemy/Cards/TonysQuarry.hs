module Arkham.Enemy.Cards.TonysQuarry (
  tonysQuarry,
  TonysQuarry (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Projection
import Arkham.Token

newtype TonysQuarry = TonysQuarry EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

tonysQuarry :: EnemyCard TonysQuarry
tonysQuarry =
  enemyWith TonysQuarry Cards.tonysQuarry (4, Static 3, 1) (1, 2)
    $ spawnAtL
    ?~ SpawnAt
      (FarthestLocationFromInvestigator (investigatorIs Investigators.tonyMorgan) Anywhere)

instance HasAbilities TonysQuarry where
  getAbilities (TonysQuarry a) =
    withBaseAbilities
      a
      [mkAbility a 1 $ ForcedAbility $ EnemySpawns #after Anywhere $ EnemyWithId (toId a)]

-- Bounty needs to be via a THEN, two things can affect a THEN:

-- * Instead, for example EOTE's That Which Has No Name

-- * Cancel, for example Wendy's Tidal Memento

--
-- Easy solution, add a DoStep and check if there is any doom
-- Complex solution, tie these two together somehow
--
-- for now we've chosen the easy option, the situation in which we've managed
-- to cancel/replace the doom placement and then placed doom anyways seems very
-- rare

instance RunMessage TonysQuarry where
  runMessage msg e@(TonysQuarry attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ PlaceTokens (toSource attrs) (toTarget attrs) Doom 1
        , DoStep 1 msg
        ]
      pure e
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _ _) -> do
      hasDoom <- fieldP EnemyDoom (> 0) (toId attrs)
      pushWhen hasDoom $ PlaceTokens (toSource attrs) (toTarget attrs) Bounty 1
      pure e
    _ -> TonysQuarry <$> runMessage msg attrs

module Arkham.Enemy.Cards.JeromeDavids (
  jeromeDavids,
  JeromeDavids (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Discard
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype JeromeDavids = JeromeDavids EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

jeromeDavids :: EnemyCard JeromeDavids
jeromeDavids = enemy JeromeDavids Cards.jeromeDavids (4, Static 4, 4) (1, 1)

instance HasAbilities JeromeDavids where
  getAbilities (JeromeDavids a) =
    withBaseAbilities a
      $ [ restrictedAbility a 1 (InvestigatorExists $ You <> InvestigatorWithDiscardableCard)
            $ ForcedAbility
            $ EnemyEngaged Timing.After You
            $ EnemyWithId
            $ toId a
        ]

instance RunMessage JeromeDavids where
  runMessage msg e@(JeromeDavids attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ toMessage $ discardFromHand iid attrs DiscardChoose 2
      pure e
    _ -> JeromeDavids <$> runMessage msg attrs

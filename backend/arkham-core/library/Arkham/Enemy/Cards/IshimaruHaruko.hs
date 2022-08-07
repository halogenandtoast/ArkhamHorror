module Arkham.Enemy.Cards.IshimaruHaruko
  ( ishimaruHaruko
  , IshimaruHaruko(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype IshimaruHaruko = IshimaruHaruko EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ishimaruHaruko :: EnemyCard IshimaruHaruko
ishimaruHaruko =
  enemy IshimaruHaruko Cards.ishimaruHaruko (6, Static 4, 3) (1, 1)

instance HasAbilities IshimaruHaruko where
  getAbilities (IshimaruHaruko a) = withBaseAbilities
    a
    [ mkAbility a 1 $ ForcedAbility $ EnemyDealtDamage
        Timing.After
        NonAttackDamageEffect
        (EnemyWithId $ toId a)
        AnySource
    ]

instance RunMessage IshimaruHaruko where
  runMessage msg e@(IshimaruHaruko attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      e <$ push (InvestigatorDrawEncounterCard iid)
    _ -> IshimaruHaruko <$> runMessage msg attrs

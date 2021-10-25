module Arkham.Types.Enemy.Cards.IshimaruHaruko
  ( ishimaruHaruko
  , IshimaruHaruko(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Timing qualified as Timing

newtype IshimaruHaruko = IshimaruHaruko EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
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

instance EnemyRunner env => RunMessage env IshimaruHaruko where
  runMessage msg e@(IshimaruHaruko attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      e <$ push (InvestigatorDrawEncounterCard iid)
    _ -> IshimaruHaruko <$> runMessage msg attrs

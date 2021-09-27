module Arkham.Types.Enemy.Cards.ElisabettaMagro
  ( elisabettaMagro
  , ElisabettaMagro(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Phase
import Arkham.Types.Timing qualified as Timing

newtype ElisabettaMagro = ElisabettaMagro EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elisabettaMagro :: EnemyCard ElisabettaMagro
elisabettaMagro =
  enemy ElisabettaMagro Cards.elisabettaMagro (3, Static 4, 4) (1, 1)

-- | Abilities
-- The first forced ability is handled by MaskedCarnevaleGoer_18
instance HasAbilities ElisabettaMagro where
  getAbilities (ElisabettaMagro attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs
        MythosPhase
    ]

instance EnemyRunner env => RunMessage env ElisabettaMagro where
  runMessage msg e@(ElisabettaMagro attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (PlaceDoom (toTarget attrs) 1)
    _ -> ElisabettaMagro <$> runMessage msg attrs

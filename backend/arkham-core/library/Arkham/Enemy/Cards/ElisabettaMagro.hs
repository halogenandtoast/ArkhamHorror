module Arkham.Enemy.Cards.ElisabettaMagro
  ( elisabettaMagro
  , ElisabettaMagro(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Timing qualified as Timing

newtype ElisabettaMagro = ElisabettaMagro EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
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

instance RunMessage ElisabettaMagro where
  runMessage msg e@(ElisabettaMagro attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      e <$ push (PlaceDoom (toTarget attrs) 1)
    _ -> ElisabettaMagro <$> runMessage msg attrs

module Arkham.Enemy.Cards.HuntingHorror
  ( huntingHorror
  , HuntingHorror(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ChaosBag.RevealStrategy
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Placement
import Arkham.RequestedTokenStrategy
import Arkham.Timing qualified as Timing
import Arkham.Token

newtype HuntingHorror = HuntingHorror EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingHorror :: EnemyCard HuntingHorror
huntingHorror = enemy HuntingHorror Cards.huntingHorror (2, Static 3, 2) (1, 1)

instance HasAbilities HuntingHorror where
  getAbilities (HuntingHorror x) = withBaseAbilities
    x
    [ mkAbility x 1 $ ForcedAbility $ PhaseBegins Timing.When $ PhaseIs
      EnemyPhase
    , mkAbility x 2
    $ ForcedAbility
    $ EnemyLeavesPlay Timing.When
    $ EnemyWithId
    $ toId x
    ]

instance RunMessage HuntingHorror where
  runMessage msg e@(HuntingHorror attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      e <$ push (RequestTokens source Nothing (Reveal 1) SetAside)
    RequestedTokens source _ tokens | isSource attrs source -> do
      push $ ResetTokens (toSource attrs)
      e <$ when
        (any
          (`elem` map tokenFace tokens)
          [Skull, Cultist, Tablet, ElderThing, AutoFail]
        )
        (push (Ready $ toTarget attrs))
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      e <$ pushAll (resolve $ PlaceEnemyInVoid enemyId)
    When (PlaceEnemyInVoid eid) | eid == enemyId ->
      pure
        . HuntingHorror
        $ attrs
        & (damageL .~ 0)
        & (doomL .~ 0)
        & (cluesL .~ 0)
        & (placementL .~ TheVoid)
    _ -> HuntingHorror <$> runMessage msg attrs

module Arkham.Enemy.Cards.AnetteMasonReincarnatedEvil (
  anetteMasonReincarnatedEvil,
  AnetteMasonReincarnatedEvil (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype AnetteMasonReincarnatedEvil = AnetteMasonReincarnatedEvil EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anetteMasonReincarnatedEvil :: EnemyCard AnetteMasonReincarnatedEvil
anetteMasonReincarnatedEvil =
  enemyWith
    AnetteMasonReincarnatedEvil
    Cards.anetteMasonReincarnatedEvil
    (5, PerPlayer 6, 3)
    (3, 1)
    (spawnAtL ?~ "Hangman's Hill")

instance HasModifiersFor AnetteMasonReincarnatedEvil where
  getModifiersFor (AnetteMasonReincarnatedEvil a) = do
    clues <- getSum <$> selectAgg Sum InvestigatorClues UneliminatedInvestigator
    modifySelf a [HealthModifier $ negate $ 2 * clues]

instance HasAbilities AnetteMasonReincarnatedEvil where
  getAbilities (AnetteMasonReincarnatedEvil a) =
    withBaseAbilities a
      $ [ forcedAbility a 1 $ PlacedBreaches Timing.After $ LocationTargetMatches $ locationWithEnemy (toId a)
        ]

instance RunMessage AnetteMasonReincarnatedEvil where
  runMessage msg e@(AnetteMasonReincarnatedEvil attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      investigators <- getInvestigatorPlayers
      pushAll
        [ chooseOne player
          $ [ assignDamageLabel investigator (toAbilitySource attrs 1) 1
            , assignHorrorLabel investigator (toAbilitySource attrs 1) 1
            ]
        | (investigator, player) <- investigators
        ]
      pure e
    _ -> AnetteMasonReincarnatedEvil <$> runMessage msg attrs

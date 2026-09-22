module Arkham.Enemy.Cards.TheCircleUndone.InTheClutchesOfChaos.AnetteMasonReincarnatedEvil (anetteMasonReincarnatedEvil) where

import Arkham.Classes
import Arkham.Enemy.CardDefs.TheCircleUndone.InTheClutchesOfChaos qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Timing qualified as Timing

newtype AnetteMasonReincarnatedEvil = AnetteMasonReincarnatedEvil EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anetteMasonReincarnatedEvil :: EnemyCard AnetteMasonReincarnatedEvil
anetteMasonReincarnatedEvil =
  enemyWith
    AnetteMasonReincarnatedEvil
    Cards.anetteMasonReincarnatedEvil
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
    -- She gets -2 health per clue the investigators hold, so gaining a clue can drop her
    -- health to or below the damage already on her. Defeat is only rechecked when damage
    -- is assigned, never when health falls.
    After (GainClues {}) -> do
      push $ checkDefeated GameSource attrs
      pure e
    _ -> AnetteMasonReincarnatedEvil <$> runMessage msg attrs

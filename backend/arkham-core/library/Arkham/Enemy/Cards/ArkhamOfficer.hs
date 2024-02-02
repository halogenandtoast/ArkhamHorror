module Arkham.Enemy.Cards.ArkhamOfficer (
  arkhamOfficer,
  ArkhamOfficer (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Projection
import Arkham.Token

newtype ArkhamOfficer = ArkhamOfficer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

arkhamOfficer :: EnemyCard ArkhamOfficer
arkhamOfficer = enemy ArkhamOfficer Cards.arkhamOfficer (3, Static 3, 2) (1, 0)

instance HasAbilities ArkhamOfficer where
  getAbilities (ArkhamOfficer attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 (exists $ locationWithEnemy (toId attrs) <> LocationWithAnyClues)
          $ ForcedAbility
          $ PhaseEnds #when #enemy
      , restrictedAbility attrs 2 OnSameLocation parleyAction_
      ]

instance RunMessage ArkhamOfficer where
  runMessage msg e@(ArkhamOfficer attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      location <- selectJust $ locationWithEnemy (toId attrs)
      pushAll [MovedClues (toSource location) (toTarget attrs) 1, FlipClues (toTarget attrs) 1]
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ parley iid (toAbilitySource attrs 1) iid #willpower 3
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      player <- getPlayer iid
      let exhausted = enemyExhausted attrs
      doom <- fieldMap EnemyTokens (countTokens #doom) (toId attrs)

      pushWhen (not exhausted || doom > 0)
        $ chooseOrRunOne player
        $ [Label "Automatically evade Arkham Officer" [Msg.EnemyEvaded iid (toId attrs)] | not exhausted]
        <> [ Label
            "Flip one of its doom to its clue side and take control of it."
            [FlipDoom (toTarget attrs) 1, MovedClues (toSource attrs) (toTarget iid) 1]
           | doom > 0
           ]
      pure e
    _ -> ArkhamOfficer <$> runMessage msg attrs

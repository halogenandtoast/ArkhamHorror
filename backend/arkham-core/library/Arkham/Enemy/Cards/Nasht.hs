module Arkham.Enemy.Cards.Nasht (
  nasht,
  Nasht (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story

newtype Nasht = Nasht EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nasht :: EnemyCard Nasht
nasht = enemy Nasht Cards.nasht (2, Static 3, 2) (0, 1)

instance HasAbilities Nasht where
  getAbilities (Nasht x) =
    withBaseAbilities
      x
      [ mkAbility x 1 parleyAction_
      , mkAbility x 2 $ forced $ EnemyDefeated #after You ByAny $ be x
      ]

instance RunMessage Nasht where
  runMessage msg e@(Nasht attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ SkillLabel sType [parley iid (attrs.ability 1) iid sType (2 + n)]
          | sType <- [#combat, #agility]
          ]
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ Flip iid (attrs.ability 1) (toTarget attrs)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ Flip iid (attrs.ability 2) (toTarget attrs)
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      theTrialOfNasht <- genCard Story.theTrialOfNasht
      pushAll [RemoveEnemy (toId attrs), ReadStory iid theTrialOfNasht ResolveIt Nothing]
      pure e
    _ -> Nasht <$> runMessage msg attrs

module Arkham.Enemy.Cards.KamanThah (
  kamanThah,
  KamanThah (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story

newtype KamanThah = KamanThah EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kamanThah :: EnemyCard KamanThah
kamanThah = enemy KamanThah Cards.kamanThah (2, Static 3, 2) (1, 0)

instance HasAbilities KamanThah where
  getAbilities (KamanThah x) =
    withBaseAbilities
      x
      [ skillTestAbility $ mkAbility x 1 parleyAction_
      , mkAbility x 2 $ ForcedAbility $ EnemyDefeated #after You ByAny $ EnemyWithId $ toId x
      ]

instance RunMessage KamanThah where
  runMessage msg e@(KamanThah attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      player <- getPlayer iid
      sid <- getRandom
      push
        $ chooseOne
          player
          [ SkillLabel
            sType
            [parley sid iid (toAbilitySource attrs 1) iid sType (Fixed $ 2 + n)]
          | sType <- [#willpower, #intellect]
          ]
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ Flip iid (toAbilitySource attrs 1) (toTarget attrs)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ Flip iid (toAbilitySource attrs 2) (toTarget attrs)
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      theTrialOfKamanThah <- genCard Story.theTrialOfKamanThah
      pushAll [RemoveEnemy (toId attrs), ReadStory iid theTrialOfKamanThah ResolveIt Nothing]
      pure e
    _ -> KamanThah <$> runMessage msg attrs

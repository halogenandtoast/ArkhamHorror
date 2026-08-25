module Arkham.Homebrew.DarkMatter.Enemies.MiGoSentinel (miGoSentinel) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (RevealChaosToken)
import Arkham.Enemy.Types (Field (EnemyClues))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Projection

newtype MiGoSentinel = MiGoSentinel EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoSentinel :: EnemyCard MiGoSentinel
miGoSentinel = enemy MiGoSentinel Cards.miGoSentinel

-- | "Aloof. Hunter."
instance HasModifiersFor MiGoSentinel where
  getModifiersFor (MiGoSentinel a) = modifySelf a [AddKeyword Keyword.Aloof, AddKeyword Keyword.Hunter]

{- | "Forced - After you reveal a [skull], [cultist], [tablet] or [elder_thing]
token at Mi-Go Sentinel's location: Move 1 of your clues onto Mi-Go Sentinel." /
"Forced - When you defeat Mi-Go Sentinel: Take control of all of its clues."
-}
instance HasAbilities MiGoSentinel where
  getAbilities (MiGoSentinel a) =
    extend
      a
      [ restricted
          a
          1
          (youExist $ at_ (locationWithEnemy a.id) <> InvestigatorWithClues (atLeast 1))
          $ forced
          $ RevealChaosToken #after You
          $ oneOf [#skull, #cultist, #tablet, #elderthing]
      , mkAbility a 2 $ forced $ EnemyDefeated #when You ByAny (be a)
      ]

instance RunMessage MiGoSentinel where
  runMessage msg e@(MiGoSentinel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTokens (attrs.ability 1) iid attrs #clue 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      clues <- field EnemyClues attrs.id
      when (clues > 0) $ moveTokens (attrs.ability 2) attrs iid #clue clues
      pure e
    _ -> MiGoSentinel <$> liftRunMessage msg attrs

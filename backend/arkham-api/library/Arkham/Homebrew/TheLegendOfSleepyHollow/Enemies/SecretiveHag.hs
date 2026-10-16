module Arkham.Homebrew.TheLegendOfSleepyHollow.Enemies.SecretiveHag (secretiveHag) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Log (remember)

newtype SecretiveHag = SecretiveHag EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretiveHag :: EnemyCard SecretiveHag
secretiveHag =
  enemy SecretiveHag Cards.secretiveHag
    & setPrey (InvestigatorWithLowestSkill #willpower UneliminatedInvestigator)

instance HasAbilities SecretiveHag where
  getAbilities (SecretiveHag a) =
    extend
      a
      [ scenarioI18n
          $ withI18nTooltip "secretiveHag.discard"
          $ restricted a 1 (youExist $ at_ $ locationWithEnemy a)
          $ forced
          $ RevealChaosToken #when You
          $ oneOf [#skull, #cultist, #tablet, #elderthing, #autofail]
      , scenarioI18n
          $ withI18nTooltip "secretiveHag.witch"
          $ mkAbility a 2
          $ freeReaction (EnemyDefeated #after Anyone ByAny (be a))
      ]

instance RunMessage SecretiveHag where
  runMessage msg e@(SecretiveHag attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscard iid (attrs.ability 1)
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      remember interrogatedAnElderWitch
      pure e
    _ -> SecretiveHag <$> liftRunMessage msg attrs

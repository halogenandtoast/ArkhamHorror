module Arkham.Enemy.Cards.RoyalEmissary (royalEmissary) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.Scenario (getIsReturnTo)
import Arkham.Matcher

newtype RoyalEmissary = RoyalEmissary EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

royalEmissary :: EnemyCard RoyalEmissary
royalEmissary =
  enemy RoyalEmissary Cards.royalEmissary (4, Static 4, 2) (2, 0)
    & setPrey (InvestigatorWithLowestSkill #willpower UneliminatedInvestigator)

instance HasModifiersFor RoyalEmissary where
  getModifiersFor (RoyalEmissary a) = whenM getIsReturnTo do
    n <- getGameValue (PerPlayer $ a.token #warning)
    modifySelfWhen a (n > 0) [HealthModifier n]

investigatorMatcher :: EnemyAttrs -> InvestigatorMatcher
investigatorMatcher a =
  oneOf
    [ at_ $ locationWithEnemy (toId a)
    , at_ $ AccessibleFrom $ locationWithEnemy (toId a)
    ]

instance HasAbilities RoyalEmissary where
  getAbilities (RoyalEmissary a) =
    extend
      a
      [ restricted a 1 (exists $ investigatorMatcher a) $ forced $ PhaseEnds #when #enemy
      , restricted a 2 IsReturnTo $ forced $ AddedToVictory #after (CardWithId a.cardId)
      ]

instance RunMessage RoyalEmissary where
  runMessage msg e@(RoyalEmissary attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorMatcher attrs) \investigator -> assignHorror investigator (attrs.ability 1) 1
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      placeTokens (attrs.ability 2) attrs #warning 1
      pure e
    DefeatedAddToVictory (isTarget attrs -> True) -> do
      let warnings = attrs.token #warning
      attrs' <- liftRunMessage msg attrs
      pure $ RoyalEmissary $ attrs' & tokensL %~ insertMap #warning warnings
    _ -> RoyalEmissary <$> liftRunMessage msg attrs

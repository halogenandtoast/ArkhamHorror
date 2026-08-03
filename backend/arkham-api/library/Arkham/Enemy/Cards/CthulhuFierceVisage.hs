module Arkham.Enemy.Cards.CthulhuFierceVisage (cthulhuFierceVisage) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Window qualified as Window
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers (getCthulhuRage)

newtype CthulhuFierceVisage = CthulhuFierceVisage EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Keywords Alert and Retaliate are on the card def. Prey - Highest [willpower].
cthulhuFierceVisage :: EnemyCard CthulhuFierceVisage
cthulhuFierceVisage =
  enemy CthulhuFierceVisage Cards.cthulhuFierceVisage
    & setPrey (InvestigatorWithHighestSkill #willpower UneliminatedInvestigator)

instance HasModifiersFor CthulhuFierceVisage where
  getModifiersFor (CthulhuFierceVisage a) = do
    rage <- getCthulhuRage
    modifySelf
      a
      [ EnemyFight rage
      , EnemyEvade rage
      , CannotBeDamaged
      , CannotBeDefeated
      , DoNotExhaust
      , DoNotExhaustEvaded
      ]

instance HasAbilities CthulhuFierceVisage where
  getAbilities (CthulhuFierceVisage a) =
    extend1 a
      $ mkAbility a 1
      $ freeReaction
      $ oneOf
        [ EnemyAttackedSuccessfully #after You AnySource (be a)
        , EnemyEvaded #after You (be a)
        ]

instance RunMessage CthulhuFierceVisage where
  runMessage msg e@(CthulhuFierceVisage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    -- "Flip it to its [[Enraged]] side." The Enraged def is the other face of this
    -- double-sided card, so the flip swaps the entity for it in place, keeping its
    -- tokens, placement and engagement (and therefore its slot on the Cthulhu Board).
    Flip _ _ (isTarget attrs -> True) -> do
      enraged <- genCard Cards.cthulhuFierceVisageEnraged
      push $ ReplaceEnemy attrs.id enraged Swap
      -- The Enraged side's "after you flip this enemy to this side" forced ability
      -- keys off this window, so it has to be raised here, where the flip happens.
      checkAfter $ Window.EnemyFlipped attrs.id
      pure e
    _ -> CthulhuFierceVisage <$> liftRunMessage msg attrs

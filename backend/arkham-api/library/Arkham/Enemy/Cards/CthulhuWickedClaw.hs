module Arkham.Enemy.Cards.CthulhuWickedClaw (cthulhuWickedClaw) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers (getCthulhuRage)

newtype CthulhuWickedClaw = CthulhuWickedClaw EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Keyword Retaliate is on the card def. Prey - Highest [combat].
cthulhuWickedClaw :: EnemyCard CthulhuWickedClaw
cthulhuWickedClaw =
  enemy CthulhuWickedClaw Cards.cthulhuWickedClaw
    & setPrey (InvestigatorWithHighestSkill #combat UneliminatedInvestigator)

instance HasModifiersFor CthulhuWickedClaw where
  getModifiersFor (CthulhuWickedClaw a) = do
    -- "X is Cthulhu's Rage" — its fight and evade are equal to Cthulhu's Rage.
    rage <- getCthulhuRage
    -- TODO: Cthulhu Board / shared traits / shared text / single-enemy
    -- interaction across the four facets has no engine support yet.
    modifySelf
      a
      [ EnemyFight rage
      , EnemyEvade rage
      , CannotBeDamaged
      , CannotBeDefeated
      , DoNotExhaust -- "cannot be exhausted"
      ]

instance HasAbilities CthulhuWickedClaw where
  getAbilities (CthulhuWickedClaw a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ oneOf
        [ EnemyAttackedSuccessfully #after You AnySource (be a)
        , EnemyEvaded #after You (be a)
        ]

instance RunMessage CthulhuWickedClaw where
  runMessage msg e@(CthulhuWickedClaw attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- After you successfully fight or evade this enemy: flip it to its
      -- Enraged side (the Enraged def is the other face of this double-sided
      -- card, so the flip swaps in cthulhuWickedClawEnraged).
      -- TODO: the corresponding Cthulhu Board flip has no clean primitive yet.
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    _ -> CthulhuWickedClaw <$> liftRunMessage msg attrs

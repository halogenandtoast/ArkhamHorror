module Arkham.Enemy.Cards.CthulhuHoaryWings (cthulhuHoaryWings) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers (getCthulhuRage)
import Arkham.Window qualified as Window

newtype CthulhuHoaryWings = CthulhuHoaryWings EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cthulhuHoaryWings :: EnemyCard CthulhuHoaryWings
cthulhuHoaryWings =
  enemy CthulhuHoaryWings Cards.cthulhuHoaryWings
    & setPrey (InvestigatorWithHighestSkill #agility UneliminatedInvestigator)

instance HasModifiersFor CthulhuHoaryWings where
  getModifiersFor (CthulhuHoaryWings a) = do
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

instance HasAbilities CthulhuHoaryWings where
  getAbilities (CthulhuHoaryWings a) =
    extend1 a
      $ mkAbility a 1
      $ freeReaction
      $ oneOf
        [ EnemyAttackedSuccessfully #after You AnySource (be a)
        , EnemyEvaded #after You (be a)
        ]

instance RunMessage CthulhuHoaryWings where
  runMessage msg e@(CthulhuHoaryWings attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      enraged <- genCard Cards.cthulhuHoaryWingsEnraged
      push $ ReplaceEnemy attrs.id enraged Swap
      checkAfter $ Window.EnemyFlipped attrs.id
      pure e
    _ -> CthulhuHoaryWings <$> liftRunMessage msg attrs

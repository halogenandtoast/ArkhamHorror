module Arkham.Homebrew.ThePhantomOfTheOpera.Enemies.ThePhantomTragicFigure (
  thePhantomTragicFigure,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Cards
import Arkham.Matcher hiding (DuringTurn)

newtype ThePhantomTragicFigure = ThePhantomTragicFigure EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePhantomTragicFigure :: EnemyCard ThePhantomTragicFigure
thePhantomTragicFigure = enemy ThePhantomTragicFigure Cards.thePhantomTragicFigure

instance HasModifiersFor ThePhantomTragicFigure where
  getModifiersFor (ThePhantomTragicFigure a) = modifySelf a [CannotReady, CannotBeEngaged]

instance HasAbilities ThePhantomTragicFigure where
  getAbilities (ThePhantomTragicFigure a) =
    extend1 a $ restricted a 1 (DuringTurn Anyone) $ Objective $ FastAbility Free

instance RunMessage ThePhantomTragicFigure where
  runMessage msg e@(ThePhantomTragicFigure attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceCurrentAct (attrs.ability 1)
      pure e
    _ -> ThePhantomTragicFigure <$> liftRunMessage msg attrs

module Arkham.Enemy.Cards.TracesToNowhere.DimensionalShamblerDraggingThemBelow (
  dimensionalShamblerDraggingThemBelow,
) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyCardsUnderneath))
import Arkham.Helpers.Modifiers (ModifierType (EnemyFight), modifySelf)
import Arkham.Matcher
import Arkham.Projection

newtype DimensionalShamblerDraggingThemBelow = DimensionalShamblerDraggingThemBelow EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalShamblerDraggingThemBelow :: EnemyCard DimensionalShamblerDraggingThemBelow
dimensionalShamblerDraggingThemBelow =
  enemy DimensionalShamblerDraggingThemBelow Cards.dimensionalShamblerDraggingThemBelow

{- | "Dimensional Shambler gets +1 fight for each card attached to it (to a
maximum of +4 fight)."
-}
instance HasModifiersFor DimensionalShamblerDraggingThemBelow where
  getModifiersFor (DimensionalShamblerDraggingThemBelow a) = do
    attached <- length <$> field EnemyCardsUnderneath a.id
    modifySelf a [EnemyFight (min 4 attached)]

instance HasAbilities DimensionalShamblerDraggingThemBelow where
  getAbilities (DimensionalShamblerDraggingThemBelow a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDealtDamage #after AnyDamageEffect (be a) AnySource

instance RunMessage DimensionalShamblerDraggingThemBelow where
  runMessage msg e@(DimensionalShamblerDraggingThemBelow attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      attached <- field EnemyCardsUnderneath attrs.id
      for_ (listToMaybe attached) \card -> do
        push $ RemoveFromUnderneath (toTarget attrs) [card]
        push $ RemovedFromGame card
      healDamage attrs (attrs.ability 1) 2
      pure e
    _ -> DimensionalShamblerDraggingThemBelow <$> liftRunMessage msg attrs

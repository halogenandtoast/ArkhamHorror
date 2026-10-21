module Arkham.Act.Cards.TracesToNowhere.AlbrechtsGamble (albrechtsGamble) where

import Arkham.Ability
import Arkham.Act.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Enemies
import Arkham.Matcher
import Arkham.Trait (Trait (Arkham))

newtype AlbrechtsGamble = AlbrechtsGamble ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

albrechtsGamble :: ActCard AlbrechtsGamble
albrechtsGamble = act (4, A) AlbrechtsGamble Cards.albrechtsGamble Nothing

instance HasAbilities AlbrechtsGamble where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ forced
        $ EnemyDefeated #after Anyone ByAny (enemyIs Enemies.albrechtHusserlDesperateGambler)
    , restricted
        a
        2
        ( InVictoryDisplay (cardIs Enemies.dimensionalShamblerDraggingThemBelow) (atLeast 1)
            <> exists (enemyIs Enemies.albrechtHusserlDesperateGambler <> EnemyAt (LocationWithTrait Arkham))
        )
        $ Objective
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage AlbrechtsGamble where
  runMessage msg a@(AlbrechtsGamble attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R4
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct aid _ _ | aid == attrs.id && onSide B attrs -> do
      push R3
      pure a
    _ -> AlbrechtsGamble <$> liftRunMessage msg attrs

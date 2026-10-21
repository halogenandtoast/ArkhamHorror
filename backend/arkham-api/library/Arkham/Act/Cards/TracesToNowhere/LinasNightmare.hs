module Arkham.Act.Cards.TracesToNowhere.LinasNightmare (linasNightmare) where

import Arkham.Ability
import Arkham.Act.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Enemies
import Arkham.Matcher
import Arkham.Trait (Trait (Arkham))

newtype LinasNightmare = LinasNightmare ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

linasNightmare :: ActCard LinasNightmare
linasNightmare = act (4, A) LinasNightmare Cards.linasNightmare Nothing

instance HasAbilities LinasNightmare where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ forced
        $ EnemyDefeated #after Anyone ByAny (enemyIs Enemies.linaHusserlVisionsOfHerOwnDemise)
    , restricted
        a
        2
        ( InVictoryDisplay (cardIs Enemies.dimensionalShamblerDraggingThemBelow) (atLeast 1)
            <> exists (enemyIs Enemies.linaHusserlVisionsOfHerOwnDemise <> EnemyAt (LocationWithTrait Arkham))
        )
        $ Objective
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage LinasNightmare where
  runMessage msg a@(LinasNightmare attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R4
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct aid _ _ | aid == attrs.id && onSide B attrs -> do
      push R1
      pure a
    _ -> LinasNightmare <$> liftRunMessage msg attrs

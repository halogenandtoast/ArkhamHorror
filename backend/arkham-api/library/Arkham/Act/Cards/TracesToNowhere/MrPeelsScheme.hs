module Arkham.Act.Cards.TracesToNowhere.MrPeelsScheme (mrPeelsScheme) where

import Arkham.Ability
import Arkham.Act.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Enemies
import Arkham.Matcher
import Arkham.Trait (Trait (Arkham))

newtype MrPeelsScheme = MrPeelsScheme ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mrPeelsScheme :: ActCard MrPeelsScheme
mrPeelsScheme = act (4, A) MrPeelsScheme Cards.mrPeelsScheme Nothing

instance HasAbilities MrPeelsScheme where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ forced
        $ EnemyDefeated #after Anyone ByAny (enemyIs Enemies.eugenePeelDoubleCrossingCultist)
    , restricted
        a
        2
        ( InVictoryDisplay (cardIs Enemies.dimensionalShamblerDraggingThemBelow) (atLeast 1)
            <> exists (enemyIs Enemies.eugenePeelDoubleCrossingCultist <> EnemyAt (LocationWithTrait Arkham))
        )
        $ Objective
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage MrPeelsScheme where
  runMessage msg a@(MrPeelsScheme attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R4
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct aid _ _ | aid == attrs.id && onSide B attrs -> do
      push R2
      pure a
    _ -> MrPeelsScheme <$> liftRunMessage msg attrs

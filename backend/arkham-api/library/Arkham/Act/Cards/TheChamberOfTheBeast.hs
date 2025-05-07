module Arkham.Act.Cards.TheChamberOfTheBeast (theChamberOfTheBeast) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Matcher

newtype TheChamberOfTheBeast = TheChamberOfTheBeast ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChamberOfTheBeast :: ActCard TheChamberOfTheBeast
theChamberOfTheBeast =
  act (2, A) TheChamberOfTheBeast Cards.theChamberOfTheBeast Nothing

instance HasAbilities TheChamberOfTheBeast where
  getAbilities = actAbilities \x ->
    [ mkAbility x 1 $ Objective $ forced $ EnemyDefeated #after Anyone ByAny $ enemyIs Cards.silasBishop
    , restricted x 2 (exists $ LocationWithTitle "The Hidden Chamber" <> LocationWithoutClues)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TheChamberOfTheBeast where
  runMessage msg a@(TheChamberOfTheBeast attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectOne (assetIs Cards.theNecronomiconOlausWormiusTranslation) >>= \case
        Just _ -> push R2
        Nothing -> push R3
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> TheChamberOfTheBeast <$> liftRunMessage msg attrs

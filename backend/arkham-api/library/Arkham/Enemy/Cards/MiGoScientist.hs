module Arkham.Enemy.Cards.MiGoScientist (miGoScientist) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetPlacement))
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Log (remembered)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Projection
import Arkham.ScenarioLogKey (ScenarioLogKey (TheMiGoResearchWasStopped))

newtype MiGoScientist = MiGoScientist EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoScientist :: EnemyCard MiGoScientist
miGoScientist = enemy MiGoScientist Cards.miGoScientist

instance HasModifiersFor MiGoScientist where
  getModifiersFor (MiGoScientist a) =
    whenM (remembered TheMiGoResearchWasStopped) $ modifySelf a [HealthModifier (-2), EnemyEvade (-2)]

instance HasAbilities MiGoScientist where
  getAbilities (MiGoScientist a) =
    extend
      a
      [ restricted a 1 (thisExists a ReadyEnemy <> exists (assetIs Assets.brainCase))
          $ forced
          $ PhaseBegins #when #enemy
      , restricted a 2 (exists $ assetIs Assets.brainCase)
          $ forced
          $ EnemyMoves #after Anywhere (be a)
      ]

instance RunMessage MiGoScientist where
  runMessage msg e@(MiGoScientist attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      brain <- selectJust $ assetIs Assets.brainCase
      placement <- field AssetPlacement brain
      case placement of
        AttachedToEnemy eid | eid == attrs.id -> moveToward attrs (LocationWithTitle "Fungus Mound")
        _ -> do
          alreadyThere <- selectAny $ assetIs Assets.brainCase <> AssetAt (locationWithEnemy attrs.id)
          if alreadyThere
            then push $ PlaceAsset brain (AttachedToEnemy attrs.id)
            else moveToward attrs $ locationWithAsset brain
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      brain <- selectJust $ assetIs Assets.brainCase
      whenM (selectAny $ assetIs Assets.brainCase <> AssetAt (locationWithEnemy attrs.id)) do
        push $ PlaceAsset brain (AttachedToEnemy attrs.id)
      pure e
    _ -> MiGoScientist <$> liftRunMessage msg attrs

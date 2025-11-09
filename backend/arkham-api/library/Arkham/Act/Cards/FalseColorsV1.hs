module Arkham.Act.Cards.FalseColorsV1 (falseColorsV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyPlacement))
import Arkham.Field
import Arkham.Helpers.Location (placementLocation)
import Arkham.Matcher
import Arkham.Message (resolve)
import Arkham.Placement
import Arkham.Projection
import Arkham.Spawn
import Arkham.Window qualified as Window

newtype FalseColorsV1 = FalseColorsV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseColorsV1 :: ActCard FalseColorsV1
falseColorsV1 = act (2, A) FalseColorsV1 Cards.falseColorsV1 Nothing

instance HasAbilities FalseColorsV1 where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ triggered (ScenarioEvent #when Nothing "wouldConceal") ConcealedXCost
    , mkAbility a 2 $ ActionAbility [#resign] (ActionCost 1)
    , restricted a 3 (not_ $ exists $ EnemyWithPlacement InTheShadows)
        $ Objective
        $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)
    ]

instance RunMessage FalseColorsV1 where
  runMessage msg a@(FalseColorsV1 attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 ws _ -> do
      cancelWindowBatch ws
      let
        go = \case
          ((Window.windowType -> Window.ScenarioEvent "wouldConceal" (Just iid) val) : _) -> do
            let (eid, _, _) = toResult @(EnemyId, ConcealedCardKind, GameValue) val
            pushAll
              $ resolve
                ( EnemySpawn
                    $ (mkSpawnDetails eid $ SpawnEngagedWith (InvestigatorWithId iid))
                      { spawnDetailsInvestigator = Just iid
                      }
                )
          (_ : rest) -> go rest
          [] -> pure ()
      go ws
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      resign iid
      pure a
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      inPlayVersion <- selectJust $ enemyIs Enemies.desiderioDelgadoAlvarez106
      placement <- field EnemyPlacement inPlayVersion
      mlocation <- placementLocation placement
      badVersion <- fetchCard inPlayVersion
      outOfPlayCard <- fetchCard Assets.desiderioDelgadoAlvarez
      let goodVersion = lookupCard Enemies.desiderioDelgadoAlvarez107.cardCode outOfPlayCard.id
      replaceCard goodVersion.id goodVersion
      removeFromGame inPlayVersion

      shuffle [goodVersion, badVersion] >>= \case
        [x, y] -> do
          x1 <- createEnemy x Unplaced
          push $ UpdateEnemy x1 $ Update EnemyPlacement placement
          for_ mlocation $ createEnemy_ y . AtLocation
        _ -> error "Invalid needs both"

      advanceActDeck attrs
      pure a
    _ -> FalseColorsV1 <$> liftRunMessage msg attrs

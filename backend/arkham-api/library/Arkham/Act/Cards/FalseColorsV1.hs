module Arkham.Act.Cards.FalseColorsV1 (falseColorsV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Matcher
import Arkham.Message (resolve)
import Arkham.Placement
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
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> FalseColorsV1 <$> liftRunMessage msg attrs

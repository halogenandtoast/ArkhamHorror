module Arkham.Act.Cards.AlejandrosPlight (AlejandrosPlight (..), alejandrosPlight) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype AlejandrosPlight = AlejandrosPlight ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alejandrosPlight :: ActCard AlejandrosPlight
alejandrosPlight = act (3, C) AlejandrosPlight Cards.alejandrosPlight Nothing

instance HasModifiersFor AlejandrosPlight where
  getModifiersFor (AlejandrosPlight a) = do
    n <- perPlayer 2
    modifySelect a (EnemyWithAsset $ assetIs Assets.alejandroVela) [HealthModifier n]

instance HasAbilities AlejandrosPlight where
  getAbilities (AlejandrosPlight a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ EnemyLeavesPlay #when
        $ EnemyWithAsset (assetIs Assets.alejandroVela)
    ]

instance RunMessage AlejandrosPlight where
  runMessage msg a@(AlejandrosPlight attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide D attrs -> do
      lead <- getLeadPlayer
      deckCount <- getActDecksInPlayCount
      alejandroVela <- selectJust $ assetIs Assets.alejandroVela
      iids <- select $ NearestToEnemy $ EnemyWithAsset $ assetIs Assets.alejandroVela
      let
        takeControlMessage =
          chooseOrRunOne lead [targetLabel iid [TakeControlOfAsset iid alejandroVela] | iid <- iids]
        nextMessage =
          if deckCount <= 1
            then R1
            else RemoveCompletedActFromGame (actDeckId attrs) (toId attrs)
      pushAll [takeControlMessage, nextMessage]
      pure a
    _ -> AlejandrosPlight <$> runMessage msg attrs

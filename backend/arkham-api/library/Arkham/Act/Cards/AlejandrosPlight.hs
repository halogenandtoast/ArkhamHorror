module Arkham.Act.Cards.AlejandrosPlight (alejandrosPlight) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
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
  runMessage msg a@(AlejandrosPlight attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide D attrs -> True) _ _ -> do
      alejandroVela <- selectJust $ assetIs Assets.alejandroVela
      iids <- select $ NearestToEnemy $ EnemyWithAsset $ assetIs Assets.alejandroVela
      leadChooseOrRunOneM $ targets iids (`takeControlOfAsset` alejandroVela)
      deckCount <- getActDecksInPlayCount
      push
        $ if deckCount <= 1
          then R1
          else RemoveCompletedActFromGame (actDeckId attrs) (toId attrs)
      pure a
    _ -> AlejandrosPlight <$> liftRunMessage msg attrs

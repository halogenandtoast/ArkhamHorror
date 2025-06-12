module Arkham.Act.Cards.AlejandrosPrison (alejandrosPrison) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype AlejandrosPrison = AlejandrosPrison ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alejandrosPrison :: ActCard AlejandrosPrison
alejandrosPrison = act (3, C) AlejandrosPrison Cards.alejandrosPrison Nothing

instance HasModifiersFor AlejandrosPrison where
  getModifiersFor (AlejandrosPrison a) = do
    modifySelect a (LocationWithAsset (assetIs Assets.alejandroVela)) [ShroudModifier 2]

instance HasAbilities AlejandrosPrison where
  getAbilities = actAbilities1' C \a ->
    restricted a 1 (exists $ LocationWithAsset (assetIs Assets.alejandroVela) <> LocationWithoutClues)
      $ Objective
      $ forced AnyWindow

instance RunMessage AlejandrosPrison where
  runMessage msg a@(AlejandrosPrison attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide D attrs -> True) _ _ -> do
      deckCount <- getActDecksInPlayCount
      alejandroVela <- selectJust $ assetIs Assets.alejandroVela
      iids <- select $ NearestToLocation $ LocationWithAsset $ assetIs Assets.alejandroVela
      leadChooseOrRunOneM $ targets iids (`takeControlOfAsset` alejandroVela)
      push
        $ if deckCount <= 1
          then R1
          else RemoveCompletedActFromGame (actDeckId attrs) (toId attrs)
      pure a
    _ -> AlejandrosPrison <$> liftRunMessage msg attrs

module Arkham.Act.Cards.FateOfTheValeV3 (fateOfTheValeV3) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Helpers.Modifiers (maybeModified_)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Investigator.Types (Field (InvestigatorLocation))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Token (Token (Kindling))
import Arkham.Treachery.Cards qualified as Treacheries

newtype FateOfTheValeV3 = FateOfTheValeV3 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor FateOfTheValeV3 where
  getModifiersFor (FateOfTheValeV3 a) = do
    selectEach (LocationWithToken Kindling) \loc -> do
      maybeModified_ a loc do
        shroud <- MaybeT $ field LocationShroud loc
        tokens <- lift $ field LocationTokens loc
        guard $ findWithDefault 0 Kindling tokens >= shroud
        pure [ScenarioModifier "ready"]

fateOfTheValeV3 :: ActCard FateOfTheValeV3
fateOfTheValeV3 = act (3, A) FateOfTheValeV3 Cards.fateOfTheValeV3 Nothing

getClueCount :: Payment -> Int
getClueCount (CluePayment _ n) = n
getClueCount (Payments ps) = sum $ map getClueCount ps
getClueCount _ = 0

instance HasAbilities FateOfTheValeV3 where
  getAbilities (FateOfTheValeV3 a) =
    extend
      a
      [ restricted a 1 (DuringTurn You) $ actionAbilityWithCost (GroupClueCostX YourLocation)
      , restricted
          a
          2
          (DuringTurn You <> exists (YourLocation <> LocationWithModifier (ScenarioModifier "ready")))
          actionAbility
      , restricted a 3 (TreacheryCount (atLeast 5) $ treacheryIs Treacheries.fire)
          $ Objective
          $ forced
          $ RoundEnds #when
      ]

instance RunMessage FateOfTheValeV3 where
  runMessage msg a@(FateOfTheValeV3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getClueCount -> cluesSpent) -> do
      field InvestigatorLocation iid >>= traverse_ \lid -> do
        playerCount <- getPlayerCount
        let kindling = cluesSpent `div` playerCount
        placeTokens (attrs.ability 1) lid Kindling kindling

        resources <- getSpendableResources iid
        items <- select $ assetControlledBy iid <> #item <> DiscardableAsset
        when (resources >= 5 || notNull items) do
          chooseOneM iid do
            labeled "Done" nothing
            when (resources >= 5) do
              labeled "Spend 5 resources to place 1 additional kindling" do
                spendResources iid 5
                placeTokens (attrs.ability 1) lid Kindling 1
            targets items \item -> do
              toDiscardBy iid (attrs.ability 1) item
              placeTokens (attrs.ability 1) lid Kindling 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      field InvestigatorLocation iid >>= \case
        Nothing -> pure a
        Just lid -> do
          kindling <- fieldMap LocationTokens (findWithDefault 0 Kindling) lid
          shroud <- fieldWithDefault 0 LocationShroud lid
          let burnedLocations = toResultDefault [] attrs.meta
          if kindling >= shroud && lid `notElem` burnedLocations
            then do
              removeTokens (attrs.ability 2) lid Kindling kindling
              drawCard iid =<< getSetAsideCard Treacheries.fire
              pure $ FateOfTheValeV3 $ attrs & metaL .~ toJSON (lid : burnedLocations)
            else pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      fireCount <- selectCount $ treacheryIs Treacheries.fire
      when (fireCount >= 5) $ advanceVia #other attrs (attrs.ability 3)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R4
      pure a
    _ -> FateOfTheValeV3 <$> liftRunMessage msg attrs

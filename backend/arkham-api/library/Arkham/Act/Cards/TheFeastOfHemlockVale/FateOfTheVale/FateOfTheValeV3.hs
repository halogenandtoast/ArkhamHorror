module Arkham.Act.Cards.TheFeastOfHemlockVale.FateOfTheVale.FateOfTheValeV3 (fateOfTheValeV3) where

import Arkham.Ability
import Arkham.Act.CardDefs.TheFeastOfHemlockVale.FateOfTheVale qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Classes.HasGame (HasGame)
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Helpers.Modifiers (maybeModified_)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Investigator.Types (Field (InvestigatorLocation))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Scenarios.TheFeastOfHemlockVale.FateOfTheVale.Helpers (scenarioI18n)
import Arkham.Token (Token (Kindling))
import Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.Fire qualified as Treacheries
import Arkham.UltimatumsAndBoons (hasBoon)
import Arkham.UltimatumsAndBoons.Types (Boon (BoonOfTheMiners), UltimatumOrBoon (Boon))

newtype FateOfTheValeV3 = FateOfTheValeV3 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor FateOfTheValeV3 where
  getModifiersFor (FateOfTheValeV3 a) = do
    selectEach (LocationWithToken Kindling) \loc -> do
      maybeModified_ a loc do
        needed <- lift $ kindlingNeeded loc
        tokens <- lift $ field LocationTokens loc
        guard $ findWithDefault 0 Kindling tokens >= needed
        pure [ScenarioModifier "ready"]

{- | Kindling that has to be on a location before it can be set alight: its
shroud, or 1 per investigator under Boon of the Miners.
-}
kindlingNeeded :: HasGame m => LocationId -> m Int
kindlingNeeded loc = do
  miners <- hasBoon BoonOfTheMiners
  if miners then getPlayerCount else fieldWithDefault 0 LocationShroud loc

fateOfTheValeV3 :: ActCard FateOfTheValeV3
fateOfTheValeV3 = act (3, A) FateOfTheValeV3 Cards.fateOfTheValeV3 Nothing

instance HasAbilities FateOfTheValeV3 where
  getAbilities (FateOfTheValeV3 a) =
    extend
      a
      [ scenarioI18n
          $ withI18nTooltip "fateOfTheValeV3.placeKindling"
          $ restricted a 1 (DuringTurn You <> not_ miners)
          $ actionAbilityWithCost PerPlayerClueCostX
      , scenarioI18n
          $ withI18nTooltip "fateOfTheValeV3.placeKindlingMiners"
          $ restricted a 4 (DuringTurn You <> miners)
          $ actionAbilityWithCost ClueCostX
      , scenarioI18n
          $ withI18nTooltip "fateOfTheValeV3.drawFire"
          $ restricted a 2 (DuringTurn You <> not_ miners <> readyLocation) actionAbility
      , scenarioI18n
          $ withI18nTooltip "fateOfTheValeV3.drawFireMiners"
          $ restricted a 5 (miners <> readyLocation)
          $ FastAbility Free
      , restricted a 3 (TreacheryCount (atLeast 5) $ treacheryIs Treacheries.fire)
          $ Objective
          $ forced
          $ RoundEnds #when
      ]
   where
    -- Abilities are pure, so the printed and Boon of the Miners versions are
    -- both declared and told apart by the criterion.
    miners = UltimatumOrBoonIsActive (Boon BoonOfTheMiners)
    readyLocation = exists (YourLocation <> LocationWithModifier (ScenarioModifier "ready"))

instance RunMessage FateOfTheValeV3 where
  runMessage msg a@(FateOfTheValeV3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) idx _ (totalCluePayment -> cluesSpent)
      | idx `elem` [1, 4] -> do
          field InvestigatorLocation iid >>= traverse_ \lid -> do
            -- Printed: X is per investigator. Boon of the Miners: X clues, X kindling.
            kindling <-
              if idx == 1 then (cluesSpent `div`) <$> getPlayerCount else pure cluesSpent
            placeTokens (attrs.ability idx) lid Kindling kindling

            resources <- getSpendableResources iid
            items <- select $ assetControlledBy iid <> #item <> DiscardableAsset
            when (resources >= 5 || notNull items) do
              chooseOneM iid $ scenarioI18n do
                labeledI "done" nothing
                when (resources >= 5) do
                  labeled "fateOfTheValeV3.spendResourcesForKindling" do
                    spendResources iid 5
                    placeTokens (attrs.ability idx) lid Kindling 1
                targets items \item -> do
                  toDiscardBy iid (attrs.ability idx) item
                  placeTokens (attrs.ability idx) lid Kindling 1
          pure a
    UseThisAbility iid (isSource attrs -> True) idx | idx `elem` [2, 5] -> do
      field InvestigatorLocation iid >>= \case
        Nothing -> pure a
        Just lid -> do
          kindling <- fieldMap LocationTokens (findWithDefault 0 Kindling) lid
          needed <- kindlingNeeded lid
          let burnedLocations = toResultDefault [] attrs.meta
          if kindling >= needed && lid `notElem` burnedLocations
            then do
              removeTokens (attrs.ability idx) lid Kindling kindling
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

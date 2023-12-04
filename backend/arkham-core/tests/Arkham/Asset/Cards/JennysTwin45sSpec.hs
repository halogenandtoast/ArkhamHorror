module Arkham.Asset.Cards.JennysTwin45sSpec (
  spec,
) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.ActiveCost
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Types (Field (..))
import Arkham.Asset.Uses (UseType (Ammo))
import Arkham.Enemy.Types (Field (..))
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Matcher (AbilityMatcher (..), assetIs)
import Arkham.Projection
import Arkham.Token

spec :: Spec
spec = describe "Jenny's Twin .45s" $ do
  it "enters play with X uses" $ gameTest $ \investigator -> do
    jennysTwin45s <- genPlayerCard Cards.jennysTwin45s
    updateInvestigator investigator $ \attrs ->
      attrs
        { investigatorTokens = setTokens Resource 5 mempty
        , investigatorHand = [PlayerCard jennysTwin45s]
        }
    pushAndRun $ playCard investigator (PlayerCard jennysTwin45s)
    activeCost <- getActiveCost
    pushAndRunAll
      $ replicate 5
      $ PayCost (activeCostId activeCost) (toId investigator) False (ResourceCost 1)
    assetId <- selectJust $ assetIs Cards.jennysTwin45s
    assert $ fieldP AssetUses ((== 5) . findWithDefault 0 Ammo) assetId

  it "gives +2 combat and does +1 damage" $ gameTest $ \investigator -> do
    jennysTwin45s <- genPlayerCard Cards.jennysTwin45s
    updateInvestigator investigator $ \attrs ->
      attrs
        { investigatorTokens = setTokens Resource 1 mempty
        , investigatorHand = [PlayerCard jennysTwin45s]
        , investigatorCombat = 3
        }
    enemy <- testEnemyWith ((Enemy.healthL ?~ Static 3) . (Enemy.fightL ?~ 5))
    location <- testLocationWith id
    pushAndRunAll [SetChaosTokens [Zero], playAssetCard jennysTwin45s investigator]
    activeCost <- getActiveCost
    pushAndRunAll
      [ PayCost (activeCostId activeCost) (toId investigator) False (ResourceCost 1)
      , spawnAt enemy location
      , moveTo investigator location
      ]
    [ability] <- getAbilitiesMatching (AbilityOnCardControlledBy $ toId investigator)
    pushAndRun $ UseAbility (toId investigator) ability []

    chooseOnlyOption "choose enemy"
    chooseOnlyOption "start skill test"
    chooseOnlyOption "apply results"
    assert $ fieldP EnemyDamage (== 2) (toId enemy)

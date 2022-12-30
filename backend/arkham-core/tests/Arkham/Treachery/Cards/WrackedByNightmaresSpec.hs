module Arkham.Treachery.Cards.WrackedByNightmaresSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Asset.Types ( Field (..) )
import Arkham.Asset.Types qualified as Asset
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher hiding ( AssetExhausted )
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Types ( Field (..) )
import Arkham.Treachery.Cards qualified as Cards

spec :: Spec
spec = describe "Wracked by Nightmares" $ do
  it "prevents controlled assets from readying" $ do
    investigator <- testJenny id
    wrackedByNightmares <- genPlayerCard Cards.wrackedByNightmares
    asset <- testAsset
      ((Asset.exhaustedL .~ True)
      . (Asset.placementL .~ InPlayArea (toId investigator))
      )
      investigator
    drawing <- drawCards (toId investigator) investigator 1
    gameTest
        investigator
        [ loadDeck investigator [wrackedByNightmares]
        , drawing
        , ReadyExhausted
        ]
        (entitiesL . assetsL %~ insertEntity asset)
      $ do
          runMessages
          selectAny
              (TreacheryInThreatAreaOf (InvestigatorWithId $ toId investigator)
              <> treacheryIs Cards.wrackedByNightmares
              )
            `shouldReturn` True
          fieldAssert AssetExhausted (== True) asset

  it "trigger actions removes restriction and takes two actions" $ do
    investigator <- testJenny id
    wrackedByNightmares <- genPlayerCard Cards.wrackedByNightmares
    asset <- testAsset
      ((Asset.exhaustedL .~ True) . (Asset.ownerL ?~ toId investigator))
      investigator
    drawing <- drawCards (toId investigator) investigator 1
    gameTest
        investigator
        [loadDeck investigator [wrackedByNightmares], drawing]
        (entitiesL . assetsL %~ insertEntity asset)
      $ do
          runMessages
          wrackedByNightmaresId <- selectJust AnyTreachery
          [discardWrackedByNightmares] <- field
            TreacheryAbilities
            wrackedByNightmaresId
          pushAll
            [ UseAbility (toId investigator) discardWrackedByNightmares []
            , ReadyExhausted
            ]
          runMessages
          selectAny
              (TreacheryInThreatAreaOf (InvestigatorWithId $ toId investigator)
              <> treacheryIs Cards.wrackedByNightmares
              )
            `shouldReturn` False
          fieldAssert AssetExhausted (== False) asset
          fieldAssert
            InvestigatorDiscard
            (== [wrackedByNightmares])
            investigator
          fieldAssert InvestigatorRemainingActions (== 1) investigator

module Arkham.Treachery.Cards.WrackedByNightmaresSpec (spec) where

import Arkham.Asset.Types (Field (..))
import Arkham.Asset.Types qualified as Asset
import Arkham.Matcher hiding (AssetExhausted)
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Types (Field (..))
import TestImport.Lifted

spec :: Spec
spec = describe "Wracked by Nightmares" $ do
  it "prevents controlled assets from readying" . gameTest $ \investigator -> do
    wrackedByNightmares <- genPlayerCard Cards.wrackedByNightmares
    asset <-
      testAsset
        ( (Asset.exhaustedL .~ True)
            . (Asset.placementL .~ InPlayArea (toId investigator))
        )
        investigator
    pushAndRunAll
      [ loadDeck investigator [wrackedByNightmares]
      , drawCards (toId investigator) investigator 1
      , ReadyExhausted
      ]
    assert
      $ selectAny
      $ TreacheryInThreatAreaOf (InvestigatorWithId $ toId investigator)
      <> treacheryIs Cards.wrackedByNightmares
    fieldAssert AssetExhausted (== True) asset

  it "trigger actions removes restriction and takes two actions" . gameTest $ \investigator -> do
    wrackedByNightmares <- genPlayerCard Cards.wrackedByNightmares
    asset <-
      testAsset
        ((Asset.exhaustedL .~ True) . (Asset.ownerL ?~ toId investigator))
        investigator
    pushAndRunAll
      [loadDeck investigator [wrackedByNightmares], drawCards (toId investigator) investigator 1]
    wrackedByNightmaresId <- selectJust AnyTreachery
    [discardWrackedByNightmares] <-
      field
        TreacheryAbilities
        wrackedByNightmaresId
    pushAndRunAll
      [ UseAbility (toId investigator) discardWrackedByNightmares []
      , ReadyExhausted
      ]
    assert
      $ selectNone
      $ TreacheryInThreatAreaOf (InvestigatorWithId $ toId investigator)
      <> treacheryIs Cards.wrackedByNightmares
    fieldAssert AssetExhausted (== False) asset
    fieldAssert
      InvestigatorDiscard
      (== [wrackedByNightmares])
      investigator
    fieldAssert InvestigatorRemainingActions (== 1) investigator

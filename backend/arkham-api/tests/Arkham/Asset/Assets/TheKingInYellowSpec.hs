module Arkham.Asset.Assets.TheKingInYellowSpec (spec) where

import Arkham.Ability (mkAbility)
import Arkham.Ability.Type (AbilityType (..))
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes.HasGame (getGame)
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "The King in Yellow" do
  -- `CardEnteredPlay` is addressed to the card, not an entity, so the copy `RemoveAsset`
  -- parks in `gameActionRemovedEntities` while an ability is open used to match The King
  -- in Yellow's `card.id == attrs.cardId` guard as well and place itself back into the
  -- threat area. Discarded by its own reaction and redrawn after a reshuffle in the same
  -- action, it then claimed a second hand slot and the engine asked the investigator to
  -- discard an unrelated hand asset to pay for it (#5899).
  it "does not place its removed copy back into play when the card re-enters play during an ability"
    . gameTest
    $ \self -> do
      magnifyingGlass <- self `putAssetIntoPlay` Assets.magnifyingGlass

      -- hold on to the card so the second entry below is the same card, as it is when the
      -- discard pile is reshuffled back in and the weakness is drawn again
      card <- genCard Assets.theKingInYellow
      let
        card' = case card of
          PlayerCard pc -> PlayerCard pc {pcOwner = Just self.id}
          other -> other
      run $ PutCardIntoPlay self.id card' Nothing NoPayment []
      kingInYellow <- selectJust $ AssetWithCardId (toCardId card')

      -- `RemoveAsset` only parks the asset while an ability is open. Nothing resolves an
      -- ability on a `TestSource`, so this one stays open for the rest of the test.
      let
        keeper =
          mkAbility
            (SourceableWithCardCode Assets.theKingInYellow (TestSource mempty))
            1
            (ForcedAbility AnyWindow)
      runAll [UseAbility self.id keeper [], Discard (Just self.id) GameSource (toTarget kingInYellow)]

      -- the parked copy is the thing that used to answer `CardEnteredPlay` a second time,
      -- so the test is only exercising the bug while it is there
      parked <- keys . entitiesAssets . gameActionRemovedEntities <$> getGame
      liftIO $ parked `shouldBe` [kingInYellow]

      run $ PutCardIntoPlay self.id card' Nothing NoPayment []

      -- one live copy, and nobody is asked to give up a slot for the dead one
      selectCount (assetIs Assets.theKingInYellow) `shouldReturn` 1
      (gameQuestion <$> getGame) `shouldReturn` mempty
      (Assets.magnifyingGlass `isInDiscardOf` self) `shouldReturn` False
      selectCount (AssetWithId magnifyingGlass) `shouldReturn` 1

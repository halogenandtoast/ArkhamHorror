module Arkham.Event.Events.JoinTheCaravan1Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Card (getModifiedCardCost)
import Arkham.Investigator.Cards qualified as Investigators
import TestImport.New

spec :: Spec
spec = describe "Join the Caravan (1)" do
  it "reduces its cost by 1 for each different class among cards you control" . gameTest $ \self -> do
    -- Jenny Barnes is a Rogue, Beat Cop is a Guardian: two classes
    card <- genCard Events.joinTheCaravan1
    addToHand self card
    _ <- self `putAssetIntoPlay` Assets.beatCop
    settle self
    getModifiedCardCost self.id card `shouldReturn` Just 3

  it "counts each class only once" . gameTestWith Investigators.normanWithers $ \self -> do
    -- Norman and Dream-Enhancing Serum are both Seeker: one class
    card <- genCard Events.joinTheCaravan1
    addToHand self card
    _ <- self `putAssetIntoPlay` Assets.dreamEnhancingSerum
    settle self
    getModifiedCardCost self.id card `shouldReturn` Just 4

  -- #5544: the discount identified "you" as the investigator whose *hand* held the
  -- card, and HandWith never matches a card that is only as-if in hand, so playing
  -- it off Norman's deck top reduced the cost by 0 and only his own -1 applied.
  it "applies when played from the top of Norman Withers' deck" . gameTestWith Investigators.normanWithers $ \self -> do
    self `withDeck` [Events.joinTheCaravan1]
    _ <- self `putAssetIntoPlay` Assets.beatCop
    settle self
    [card] <- unDeck <$> self.deck
    -- 5 - 2 (Seeker, Guardian) - 1 (Norman playing off the top of his deck)
    getModifiedCardCost self.id (toCard card) `shouldReturn` Just 2

{- | 'preloadEntities' runs *before* each message and 'preloadModifiers' *after*, so a
card only reaches its own modifiers a message or two after it lands in hand — and a
card that is merely as-if in hand (Norman's deck top) needs the modifier that puts it
there to be preloaded first.
-}
settle :: Investigator -> TestAppT ()
settle self = replicateM_ 2 (gainResources self 0)

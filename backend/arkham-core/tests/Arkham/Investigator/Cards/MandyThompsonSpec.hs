module Arkham.Investigator.Cards.MandyThompsonSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Investigator.Cards (mandyThompson, rolandBanks)
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Skills
import Arkham.Treachery.Cards qualified as Cards
import TestImport.New

spec :: Spec
spec = describe "Mandy Thompson" $ do
  context "When an investigator at your location would search their deck" $ do
    it "they may search 3 additional cards" . gameTestWith mandyThompson $ \self -> do
      oldBookOfLore <- self `putAssetIntoPlay` Assets.oldBookOfLore
      location <- testLocation
      cards <- testPlayerCards 6
      roland <- addInvestigator rolandBanks & prop @"deck" (Deck cards)
      self `moveTo` location
      roland `moveTo` location
      [doSearch] <- self `getActionsFrom` oldBookOfLore
      self `useAbility` doSearch
      chooseTarget roland
      useReaction
      chooseOptionMatching "3 additional cards" \case
        Label "Search 3 additional cards" _ -> True
        _ -> False
      -- all cards should be targetable
      for_ cards assertTarget

    it "OR they may resolve 1 additional target" . gameTestWith mandyThompson $ \self -> do
      oldBookOfLore <- self `putAssetIntoPlay` Assets.oldBookOfLore
      location <- testLocation
      cards@(c1 : _ : c3 : []) <- testPlayerCards 3
      roland <- addInvestigator rolandBanks & prop @"deck" (Deck cards)
      self `moveTo` location
      roland `moveTo` location
      [doSearch] <- self `getActionsFrom` oldBookOfLore
      self `useAbility` doSearch
      chooseTarget roland
      useReaction
      chooseOptionMatching "Additional Target" \case
        Label "Resolve 1 additional target of the search" _ -> True
        _ -> False
      chooseTarget c1
      chooseTarget c3
      roland.hand `shouldMatchListM` map toCard [c1, c3]

  context "When an investigator at your location would search the encounter deck" do
    it "they may search 3 additional cards" . gameTestWith mandyThompson $ \self -> do
      location <- testLocation
      roland <- addInvestigator rolandBanks
      self `moveTo` location
      roland `moveTo` location

      cards <-
        traverse
          genEncounterCard
          [ Cards.ancientEvils
          , Cards.frozenInFear
          , Cards.lockedDoor
          , Cards.ghoulMinion
          , Cards.swarmOfRats
          , Cards.graspingHands
          ]
      run $ SetEncounterDeck (Deck cards)

      run
        $ search
          (toId roland)
          (TestSource mempty)
          EncounterDeckTarget
          [fromTopOfDeck 3]
          #any
          (DrawFound (toId roland) 1)
      useReaction
      chooseOptionMatching "3 additional cards" \case
        Label "Search 3 additional cards" _ -> True
        _ -> False
      -- all cards should be targetable
      for_ cards assertTarget

    it "OR they may resolve 1 additional target" . gameTestWith mandyThompson $ \self -> do
      location <- testLocation
      roland <- addInvestigator rolandBanks
      self `moveTo` location
      roland `moveTo` location

      cards@[_, c1, c2] <-
        traverse
          genEncounterCard
          [ Cards.ancientEvils
          , Cards.ghoulMinion
          , Cards.swarmOfRats
          ]
      run $ SetEncounterDeck (Deck cards)

      run
        $ search
          (toId roland)
          (TestSource mempty)
          EncounterDeckTarget
          [fromTopOfDeck 3]
          #any
          (DrawFound (toId roland) 1)
      useReaction
      chooseOptionMatching "1 additional target" \case
        Label "Resolve 1 additional target of the search" _ -> True
        _ -> False
      chooseTarget c1
      chooseTarget c2
      assertAny $ enemyIs Cards.ghoulMinion
      assertAny $ enemyIs Cards.swarmOfRats

  context "elder sign" $ do
    it "is +0" . gameTestWith mandyThompson $ \self -> self.elderSignModifier `shouldReturn` PositiveModifier 0

    it "search the top 3 cards of your deck for a card and either draw it" . gameTestWith mandyThompson $ \self -> do
      cards@(c1 : rest) <- testPlayerCards 3
      withProp @"deck" (Deck cards) self
      setChaosTokens [ElderSign]
      sid <- getRandom
      runSkillTest sid self #agility 100
      skip
      chooseTarget c1
      self.hand `shouldReturn` map toCard [c1]
      (unDeck <$> self.deck) `shouldMatchListM` rest

    it "or commit it to this test, if able" . gameTestWith mandyThompson $ \self -> do
      manualDexterity <- genPlayerCard Skills.manualDexterity
      rest <- testPlayerCards 2
      withProp @"agility" 0 self
      withProp @"deck" (Deck (manualDexterity : rest)) self
      setChaosTokens [ElderSign]
      sid <- getRandom
      runSkillTest sid self #agility 2
      skip
      chooseTarget manualDexterity
      chooseOptionMatching "Commit" \case
        Label "Commit to skill test" _ -> True
        _ -> False
      assertPassedSkillTest
      self.hand `shouldReturn` []
      (unDeck <$> self.deck) `shouldMatchListM` rest

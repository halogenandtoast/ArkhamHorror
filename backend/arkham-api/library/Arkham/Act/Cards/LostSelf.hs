module Arkham.Act.Cards.LostSelf (lostSelf) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Scenario
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenario.Deck
import Arkham.SkillTest
import Arkham.SkillTestResult
import Arkham.Trait (Trait (Cave, Lair))

newtype LostSelf = LostSelf ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostSelf :: ActCard LostSelf
lostSelf = act (2, A) LostSelf Cards.lostSelf Nothing

instance HasAbilities LostSelf where
  getAbilities (LostSelf a) =
    extend
      a
      [ restricted a 1 (DuringTurn You)
          $ freeReaction
          $ SkillTestResult #after You (SkillTestAt $ LocationWithTrait Lair) #success
      , restricted a 2 DuringYourSkillTest $ FastAbility $ ClueCost $ Static 1
      , onlyOnce
          $ restricted
            a
            3
            (LocationCount 3 $ RevealedLocation <> LocationWithTrait Cave <> LocationWithoutClues)
          $ Objective
          $ forced
          $ RoundEnds #when
      ]

revealFromBottomOfAbyss :: ReverseQueue m => InvestigatorId -> Int -> m ()
revealFromBottomOfAbyss iid n = do
  abyss <- getScenarioDeck AbyssDeck
  let revealed = drop (max 0 (length abyss - n)) abyss
  unless (null revealed) $ focusCards revealed do
    chooseOneM iid do
      targets revealed \card -> do
        unfocusCards
        let rest = filter (/= card) revealed
        for_ revealed \c -> scenarioSpecific "removeFromAbyss" (toCardId c)
        shuffleCardsIntoTopOfDeck (Deck.ScenarioDeckByKey AbyssDeck) 0 rest
        scenarioSpecific "drawFromAbyss" (iid, card)

instance RunMessage LostSelf where
  runMessage msg a@(LostSelf attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      getSkillTest >>= traverse_ \st -> case skillTestResult st of
        SucceededBy _ n | n > 0 -> revealFromBottomOfAbyss iid n
        _ -> pure ()
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (AnySkillValue 2)
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceVia #other attrs (attrs.ability 3)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      eachInvestigator $ discardAllClues attrs
      lead <- getLead
      canEndThis <-
        (||)
          <$> ((&&) <$> getHasRecord DrMarquesHasAHunch <*> getHasRecord DrMarquezHasAPlan)
          <*> getHasRecord GideonFinishedTheTaleOfAnnabelleLee
      canSaveTheVale <-
        (||) <$> getHasRecord TheHemlocksMadeATruce <*> getHasRecord ThePetersFamilyWereReunited
      canBurnItAll <- getHasRecord TheValeIsFullOfFireworks
      chooseOneM lead do
        when canEndThis $ labeled "Let's end this." $ advanceToAct attrs Cards.fateOfTheValeV1 A
        when canSaveTheVale $ labeled "Save the Vale!" $ advanceToAct attrs Cards.fateOfTheValeV2 A
        when canBurnItAll $ labeled "Burn it all." $ advanceToAct attrs Cards.fateOfTheValeV3 A
        labeled "Escape with our lives." $ advanceToAct attrs Cards.fateOfTheValeV4 A
      pure a
    _ -> LostSelf <$> liftRunMessage msg attrs

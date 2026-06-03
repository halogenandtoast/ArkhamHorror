module Arkham.Act.Cards.ShatteredMemories (shatteredMemories) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.GameValue
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Scenario
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.SkillTest
import Arkham.SkillTestResult
import Arkham.Trait (Trait (Cave, Lair))

newtype ShatteredMemories = ShatteredMemories ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shatteredMemories :: ActCard ShatteredMemories
shatteredMemories = act (1, A) ShatteredMemories Cards.shatteredMemories Nothing

instance HasAbilities ShatteredMemories where
  getAbilities (ShatteredMemories a) =
    extend
      a
      [ restricted a 1 (DuringTurn You)
          $ freeReaction
          $ SkillTestResult #after You (SkillTestAt $ LocationWithTrait Lair) #success
      , restricted a 2 DuringYourSkillTest $ FastAbility $ ClueCost $ Static 1
      , onlyOnce
          $ restricted a 3 (EachUndefeatedInvestigator $ not_ $ investigatorIs Investigators.shatteredSelf)
          $ Objective
          $ forced AnyWindow
      ]

resolveTrueSelf :: ReverseQueue m => Source -> InvestigatorId -> Card -> m ()
resolveTrueSelf source fallback card = do
  let owner = fromMaybe fallback $ toCardOwner card
  void $ setOwner owner card
  healAllDamageAndHorror source owner
  oldMemory <- setOwner owner =<< genCard Assets.oldMemory
  void $ createAssetAt oldMemory (InPlayArea owner)
  scenarioSpecific "returnFromShatteredSelf" owner

revealFromBottomOfAbyss :: ReverseQueue m => Source -> InvestigatorId -> Int -> m ()
revealFromBottomOfAbyss source iid n = do
  abyss <- getScenarioDeck AbyssDeck
  let revealed = drop (max 0 (length abyss - n)) abyss
  unless (null revealed) $ focusCards revealed do
    chooseOneM iid do
      targets revealed \card -> do
        unfocusCards
        let rest = filter (/= card) revealed
        for_ revealed \c -> scenarioSpecific "removeFromAbyss" (toCardId c)
        shuffleCardsIntoTopOfDeck (Deck.ScenarioDeckByKey AbyssDeck) 0 rest
        if toCardType card == InvestigatorType
          then resolveTrueSelf source iid card
          else scenarioSpecific "drawFromAbyss" (iid, card)

instance RunMessage ShatteredMemories where
  runMessage msg a@(ShatteredMemories attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      getSkillTest >>= traverse_ \st -> case skillTestResult st of
        SucceededBy _ n | n > 0 -> revealFromBottomOfAbyss (attrs.ability 1) iid n
        _ -> pure ()
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (AnySkillValue 2)
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceVia #other attrs (attrs.ability 3)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      locations <- select RevealedLocation
      for_ locations \lid -> do
        clueValue <- fieldMapM LocationRevealClues getGameValue lid
        clues <- field LocationClues lid
        when (clueValue > clues) $ placeClues attrs lid (clueValue - clues)

      crossedOut <- getCrossedOutResidents
      let crossedResidents = nub $ MotherRachel : crossedOut
      record $ crossedOutKey MotherRachel

      crossedResidentCards <- getSetAsideCardsMatching $ mapOneOf cardIs crossedResidents
      caveLocations <- getSetAsideCardsMatching $ CardWithTrait Cave <> CardWithType LocationType
      shuffleCardsIntoDeck (Deck.ScenarioDeckByKey AbyssDeck) (crossedResidentCards <> caveLocations)

      eachInvestigator (`forInvestigator` msg)
      advanceActDeck attrs
      pure a
    ForInvestigator iid (AdvanceAct (isSide B attrs -> True) _ _) -> do
      choices <- forMaybeM [minBound .. maxBound] \resident -> do
        level <- getRelationshipLevel resident
        mSetAside <- listToMaybe <$> getSetAsideCardsMatching (cardIs resident)
        pure $ guard (level >= 4) *> fmap (\card -> (resident, card)) mSetAside
      when (notNull choices) do
        chooseOneM iid do
          labeled "Do not take control of a resident" nothing
          for_ choices \(_resident, card) -> cardLabeled card $ takeControlOfSetAsideAsset iid card
      pure a
    _ -> ShatteredMemories <$> liftRunMessage msg attrs

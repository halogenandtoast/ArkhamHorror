module Arkham.Act.Cards.FateOfTheValeV1 (fateOfTheValeV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Draw.Types
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenario.Deck
import Arkham.Scenarios.FateOfTheVale.Helpers (revealCardsFromAbyss, scenarioI18n)
import Arkham.SkillTest
import Arkham.SkillTestResult
import Arkham.Trait (Trait (Lair))

newtype FateOfTheValeV1 = FateOfTheValeV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfTheValeV1 :: ActCard FateOfTheValeV1
fateOfTheValeV1 = act (3, A) FateOfTheValeV1 Cards.fateOfTheValeV1 Nothing

instance HasAbilities FateOfTheValeV1 where
  getAbilities (FateOfTheValeV1 a) =
    extend
      a
      [ restricted a 1 (DuringTurn You)
          $ freeReaction
          $ SkillTestResult #after You (SkillTestAt $ LocationWithTrait Lair) #success
      , restricted a 2 DuringYourSkillTest $ FastAbility $ ClueCost $ Static 1
      , onlyOnce
          $ restricted a 3 (EachUndefeatedInvestigator $ at_ $ locationIs Locations.theAbyssSpiralingOblivion)
          $ Objective
          $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 2) Anywhere)
      ]

revealFromBottomOfAbyss :: ReverseQueue m => Source -> InvestigatorId -> Int -> m ()
revealFromBottomOfAbyss _source iid n = do
  abyss <- getScenarioDeck AbyssDeck
  let revealed = drop (max 0 (length abyss - n)) abyss
  unless (null revealed)
    $ revealCardsFromAbyss iid revealed
    $ ScenarioSpecific "fateOfTheValeV1ResolveRevealedAbyss" (toJSON (iid, revealed))

instance RunMessage FateOfTheValeV1 where
  runMessage msg a@(FateOfTheValeV1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      getSkillTest >>= traverse_ \st -> case skillTestResult st of
        SucceededBy _ n | n > 0 -> revealFromBottomOfAbyss (attrs.ability 1) iid n
        _ -> pure ()
      pure a
    ScenarioSpecific "fateOfTheValeV1ResolveRevealedAbyss" v -> do
      let (iid, originallyRevealed) = toResult v :: (InvestigatorId, [Card])
      abyss <- getScenarioDeck AbyssDeck
      let revealed = filter ((`elem` map toCardId abyss) . toCardId) originallyRevealed
      chooseOneM iid $ withI18n do
        labeled' "continue" do
          for_ revealed $ scenarioSpecific "removeFromAbyss" . toCardId
          let enemies = filter (\c -> toCardType c == EnemyType && not (cdDoubleSided $ toCardDef c)) revealed
          when (notNull enemies) do
            push
              $ DrewCards iid
              $ finalizeDraw (newCardDraw (attrs.ability 1) Deck.EncounterDeck (length enemies)) enemies
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (AnySkillValue 2)
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceVia #other attrs (attrs.ability 3)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      hasShard <- selectAny $ assetIs Assets.prismaticShardAlienMeteorite <> AssetControlledBy Anyone
      chooseOneM lead $ scenarioI18n do
        labeled' "fateOfTheValeV1.letMarquezCarryOutPlan" $ push R1
        when hasShard $ labeled' "fateOfTheValeV1.doItOurselves" $ push R2
      pure a
    _ -> FateOfTheValeV1 <$> liftRunMessage msg attrs

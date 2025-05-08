module Arkham.Act.Cards.PlanningTheEscape (planningTheEscape) where

import Arkham.Ability hiding (discardedCards)
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Trait

newtype PlanningTheEscape = PlanningTheEscape ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

planningTheEscape :: ActCard PlanningTheEscape
planningTheEscape = act (3, A) PlanningTheEscape Cards.planningTheEscape Nothing

instance HasModifiersFor PlanningTheEscape where
  getModifiersFor (PlanningTheEscape attrs) = do
    modifySelect attrs UnrevealedLocation [TraitRestrictedModifier ArkhamAsylum Blank]

instance HasAbilities PlanningTheEscape where
  getAbilities (PlanningTheEscape x) =
    [ restricted
        x
        1
        ( RememberedAtLeast
            (Static 4)
            [ KnowTheGuardsPatrols
            , SetAFireInTheKitchen
            , IncitedAFightAmongstThePatients
            , ReleasedADangerousPatient
            , RecalledTheWayOut
            , DistractedTheGuards
            ]
        )
        $ Objective
        $ forced AnyWindow
    | onSide A x
    ]

instance RunMessage PlanningTheEscape where
  runMessage msg a@(PlanningTheEscape attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      enemyCards <- filter ((== EnemyType) . toCardType) <$> scenarioField ScenarioCardsUnderActDeck
      discards <- scenarioField ScenarioDiscard
      let monsterCount = count (member Monster . toTraits) (onlyEncounterCards enemyCards <> discards)
      shuffleCardsIntoDeck Deck.EncounterDeck enemyCards
      shuffleEncounterDiscardBackIn
      when (monsterCount >= 3) do
        discardUntilFirst lead attrs Deck.EncounterDeck (basic $ #enemy <> CardWithTrait Monster)
      advanceActDeck attrs
      pure a
    RequestedEncounterCard (isSource attrs -> True) _ (Just card) -> do
      investigators <- select (InvestigatorWithLowestSkill #willpower UneliminatedInvestigator)
      leadChooseOrRunOneM $ targets investigators (`drawCard` card)
      pure a
    _ -> PlanningTheEscape <$> liftRunMessage msg attrs

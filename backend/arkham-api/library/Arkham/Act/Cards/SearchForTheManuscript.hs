module Arkham.Act.Cards.SearchForTheManuscript (searchForTheManuscript) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.ChaosToken.Types
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Modifier
import Arkham.Scenarios.DealingsInTheDark.Helpers

newtype SearchForTheManuscript = SearchForTheManuscript ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheManuscript :: ActCard SearchForTheManuscript
searchForTheManuscript = act (1, A) SearchForTheManuscript Cards.searchForTheManuscript Nothing

instance HasAbilities SearchForTheManuscript where
  getAbilities = actAbilities \a ->
    [ restricted
        a
        1
        ( oneOf
            [ InvestigatorsHaveClues (AtLeast $ PerPlayer 4)
            , exists (StoryWithModifier $ ScenarioModifier "cultHasEnoughClues")
            ]
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SearchForTheManuscript where
  runMessage msg a@(SearchForTheManuscript attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advanceVia #other attrs (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      cultists <- select $ NonWeaknessEnemy <> #cultist <> not_ (EnemyWithPlacement InTheShadows)
      lead <- getLead
      for_ cultists (resolveConcealed lead)
      shuffleSetAsideIntoEncounterDeck
        $ mapOneOf cardIs [Enemies.umbralHarbinger, Enemies.emissaryFromYuggoth]
      shuffleEncounterDiscardBackIn

      -- Act 2 Setup
      eachInvestigator (`place` Unplaced)
      requestChaosTokens lead attrs 1
      advanceActDeck attrs
      pure a
    RequestedChaosTokens (isSource attrs -> True) _ tkns@(tkn : _) | tkn.face.isStandard -> do
      lead <- getLead
      continue lead $ returnChaosTokens tkns >> unfocusChaosTokens

      concealed <- shuffle =<< selectMap (.id) ConcealedCardAny
      selectEach Anywhere (push . SetLocationOutOfGame)
      grandBazaars <- fmap (drop 1) . shuffle =<< getSetAsideCardsMatching "Grand Bazaar"
      let
        entrance = Pos 1 0
        (positions, eastGate) =
          bimap (map (uncurry Pos)) (uncurry Pos)
            $ if
              | tkn.face `elem` [#"-1", #"-3", #"-5", #"-7"] ->
                  ([(1, 0), (2, -1), (2, 0), (3, 0), (3, 1), (4, 0)], (4, 0))
              | tkn.face `elem` [Skull, #"+1", #"0"] -> ([(1, 0), (2, 0), (3, 0), (3, 1), (4, 0), (4, 1)], (4, 1))
              | tkn.face `elem` [#"-2", #"-4", #"-6", #"-8"] ->
                  ([(1, 0), (2, 0), (2, 1), (3, 0), (4, 0), (4, -1)], (4, -1))
              | tkn.face `elem` [Cultist, Tablet, ElderThing] ->
                  ([(1, 0), (2, -1), (2, 0), (3, -1), (3, 0), (3, 1)], (3, 1))
              | otherwise -> ([(1, 0), (2, 0), (3, 0), (4, 0), (4, 1), (4, 2)], (4, 2))

      locations <- for (zip positions grandBazaars) \(pos, loc) -> do
        locationId <- placeLocationInGrid pos loc
        scenarioI18n do
          when (pos == entrance)
            $ gameModifier attrs locationId (UIModifier $ ImportantToScenario $ ikey' "ui.entrance")
          when (pos == eastGate) do
            gameModifier attrs locationId (UIModifier $ ImportantToScenario $ ikey' "ui.eastGate")
            gameModifier attrs locationId (ScenarioModifier "eastGate")
        pure locationId

      for_ (take 1 locations) (moveAllTo attrs)

      distributeEvenlyBetween concealed locations
      pure a
    RequestedChaosTokens (isSource attrs -> True) _ tkns -> do
      lead <- getLead
      returnChaosTokens tkns
      requestChaosTokens lead attrs 1
      pure a
    _ -> SearchForTheManuscript <$> liftRunMessage msg attrs

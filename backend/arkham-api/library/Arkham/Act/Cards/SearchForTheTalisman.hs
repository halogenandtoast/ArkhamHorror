module Arkham.Act.Cards.SearchForTheTalisman (searchForTheTalisman) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as ScarletKeys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Campaigns.TheScarletKeys.Key.Types
import Arkham.Direction
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Placement
import Arkham.Projection
import Arkham.Story.Cards qualified as Stories

newtype SearchForTheTalisman = SearchForTheTalisman ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheTalisman :: ActCard SearchForTheTalisman
searchForTheTalisman = act (2, A) SearchForTheTalisman Cards.searchForTheTalisman Nothing

instance HasAbilities SearchForTheTalisman where
  getAbilities = actAbilities \a ->
    [ restricted
        a
        1
        ( oneOf
            [ InvestigatorsAtHaveClues
                (LocationWithModifier $ ScenarioModifier "eastGate")
                (AtLeast $ PerPlayer 8)
            , exists (StoryWithModifier $ ScenarioModifier "cultHasEnoughClues")
            ]
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SearchForTheTalisman where
  runMessage msg a@(SearchForTheTalisman attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advanceVia #other attrs (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      cultReachedTheTalisman <- selectAny $ StoryWithModifier $ ScenarioModifier "cultHasEnoughClues"
      if cultReachedTheTalisman
        then do
          cultists <- select $ EnemyWithMostClues #cultist
          leadChooseOneM do
            targets cultists $ createScarletKeyAt_ ScarletKeys.theTwistedAntiprism . AttachedToEnemy
        else do
          investigators <- select $ InvestigatorAt $ LocationWithModifier $ ScenarioModifier "eastGate"
          leadChooseOneM do
            targets investigators $ createScarletKeyAt_ ScarletKeys.theTwistedAntiprism . AttachedToInvestigator

      do_ msg
      selectEach OutOfGameLocation (push . ReturnLocationToGame)

      fetchCard Locations.galataDocks >>= placeLocationInGrid_ (Pos (-2) 1)

      when cultReachedTheTalisman do
        cultistClues <- selectSum EnemyClues (InPlayEnemy #cultist)
        playerClues <- selectSum InvestigatorClues Anyone
        n <- perPlayer 1
        let x = max 0 (cultistClues - playerClues) `div` n
        when (x > 0) $ doStep x msg

      lead <- getLead
      eachInvestigator (`loseAllClues` attrs)
      -- delayed so that all enemies are in play
      doStep (-1) msg
      selectJust (storyIs Stories.theUnveiling) >>= flipOverBy lead attrs
      agenda <- getSetAsideCard Agendas.theChase
      push $ SetCurrentAgendaDeck 1 [agenda]
      toDiscard GameSource attrs
      pure a
    Do msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> do
      concealed <- selectMap (.id) ConcealedCardAny
      lead <- getLead
      leadChooseOneM $ targets concealed \card -> do
        exposeConcealed lead attrs card
        do_ msg'
      pure a
    DoStep x msg'@(AdvanceAct (isSide B attrs -> True) _ _) | x > 0 -> do
      skey <- selectJust $ scarletKeyIs ScarletKeys.theTwistedAntiprism
      placement <- field ScarletKeyPlacement skey
      docks <- selectJust $ locationIs Locations.galataDocks
      case placement of
        AttachedToEnemy eid -> do
          moveTowards attrs eid docks
          doStep (x - 1) msg'
        _ -> pure ()
      pure a
    DoStep (-1) (AdvanceAct (isSide B attrs -> True) _ _) -> do
      selectEach (enemy_ #cultist) (removeAllClues attrs)
      hagia <- selectJust $ locationIs Locations.hagiaSophia
      entrance <- selectJust $ LocationInPosition (Pos 1 0)
      push $ PlacedLocationDirection hagia LeftOf entrance
      pure a
    _ -> SearchForTheTalisman <$> liftRunMessage msg attrs

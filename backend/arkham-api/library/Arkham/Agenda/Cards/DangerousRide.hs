module Arkham.Agenda.Cards.DangerousRide (dangerousRide) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelectMapM)
import Arkham.I18n
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.WrittenInRock.Helpers
import Arkham.Trait (Trait (Rail))

newtype DangerousRide = DangerousRide AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dangerousRide :: AgendaCard DangerousRide
dangerousRide = agenda (2, A) DangerousRide Cards.dangerousRide (Static 14)

{- | Written in Rock's Standalone Mode grants this agenda an extra ability:
"Forced - When the round ends: Flip each revealed empty Rail location to its
unrevealed side. (Discard all tokens.)" The scenario flags it with a scenario
modifier during setup, since 'getAbilities' cannot ask whether we are standalone.
-}
instance HasAbilities DangerousRide where
  getAbilities (DangerousRide a) =
    [ restricted a 1 (ScenarioExists $ ScenarioWithModifier $ ScenarioModifier "standaloneRailReset")
        $ forced
        $ RoundEnds #when
    ]

instance HasModifiersFor DangerousRide where
  getModifiersFor (DangerousRide a) = do
    modifySelect a (LocationWithAsset StoryAsset) [CannotBeSlidOrSwapped]
    modifySelect a (AnyEnemy) [AddKeyword Keyword.Hunter, ResolveHunterTwice]
    modifySelectMapM a Anywhere \loc -> do
      connections <- runDefaultMaybeT [] do
        pos <- MaybeT $ field LocationPosition loc
        select $ mapOneOf LocationInPosition (adjacentPositions pos)

      pure
        [ WhileEnemyMovingModifier
            $ ConnectedToWhen (LocationWithId loc) (mapOneOf LocationWithId connections)
        | notNull connections
        ]

instance RunMessage DangerousRide where
  runMessage msg a@(DangerousRide attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (RevealedLocation <> EmptyLocation <> LocationWithTrait Rail) \loc -> do
        push $ RemoveAllTokens (toSource attrs) (toTarget loc)
        push $ UnrevealLocation loc
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        chooseOneM iid $ withI18n $ countVar 1 do
          labeled' "sufferPhysicalTrauma" $ sufferPhysicalTrauma iid 1
          labeled' "sufferMentalTrauma" $ sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> DangerousRide <$> liftRunMessage msg attrs

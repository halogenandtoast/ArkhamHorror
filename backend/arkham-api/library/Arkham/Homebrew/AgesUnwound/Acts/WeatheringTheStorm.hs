module Arkham.Homebrew.AgesUnwound.Acts.WeatheringTheStorm (weatheringTheStorm) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (SkipMythosPhaseStep), modified_)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers
import Arkham.Matcher
import Arkham.Phase (MythosPhaseStep (PlaceDoomOnAgendaStep))
import Arkham.Trait (Trait (Paradox, Servitor))

newtype WeatheringTheStorm = WeatheringTheStorm ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

weatheringTheStorm :: ActCard WeatheringTheStorm
weatheringTheStorm = act (4, A) WeatheringTheStorm Cards.weatheringTheStorm Nothing

instance HasModifiersFor WeatheringTheStorm where
  getModifiersFor (WeatheringTheStorm a) =
    modified_ a (PhaseTarget #mythos) [SkipMythosPhaseStep PlaceDoomOnAgendaStep]

instance HasAbilities WeatheringTheStorm where
  getAbilities (WeatheringTheStorm a) =
    [ mkAbility a 1 $ forced $ MythosStep WhenAllDrawEncounterCard
    , onlyOnce
        $ restricted
          a
          2
          ( UnderneathCardCount (EqualTo $ Static 0) UnderAgendaDeck #location
              <> notExists (LocationWithTrait Paradox <> LocationWithAnyClues)
              <> notExists (EnemyWithTrait Servitor)
          )
        $ Objective
        $ forced
        $ MythosStep AfterCheckDoomThreshold
    ]

instance RunMessage WeatheringTheStorm where
  runMessage msg a@(WeatheringTheStorm attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      beneath <- getLocationsBeneathAgendaDeck
      for_ (nonEmpty beneath) \cards -> do
        card <- sample cards
        resolveFlippedLocation card
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      beneath <- getLocationsBeneathAgendaDeck
      when (null beneath) $ advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> WeatheringTheStorm <$> liftRunMessage msg attrs

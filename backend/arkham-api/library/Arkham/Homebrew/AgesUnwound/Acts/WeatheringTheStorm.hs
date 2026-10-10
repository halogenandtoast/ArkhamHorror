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

-- | "Skip the 'Place 1 doom on the current agenda' step of the Mythos phase."
instance HasModifiersFor WeatheringTheStorm where
  getModifiersFor (WeatheringTheStorm a) =
    modified_ a (PhaseTarget #mythos) [SkipMythosPhaseStep PlaceDoomOnAgendaStep]

{- | "__Forced__ - When the 'draw encounter cards' step of the mythos phase
begins: The lead investigator must randomly choose one of the locations beneath
the agenda deck, flip it, and resolve its text. /
__Objective__ - After checking the doom threshold, if each of the following is
true, advance:
-- There are no locations beneath the agenda deck.
-- There are no clues on [[Paradox]] locations.
-- There are no [[Servitor]] enemies in play."

Both windows are the engine's own mythos-step windows, which is what makes the
objective fire at the moment the card names -- @1.3@, after the doom threshold is
checked and before encounter cards are drawn.
-}
instance HasAbilities WeatheringTheStorm where
  getAbilities (WeatheringTheStorm a) =
    [ mkAbility a 1 $ forced $ MythosStep WhenAllDrawEncounterCard
    , restricted
        a
        2
        ( notExists (LocationWithTrait Paradox <> LocationWithAnyClues)
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
    {- "There are no locations beneath the agenda deck" is checked here rather
    than in the ability's criterion: a 'Arkham.Criteria.Criterion' cannot read a
    scenario field, and the other two conditions are matchers that can. -}
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      beneath <- getLocationsBeneathAgendaDeck
      when (null beneath) $ advanceVia #other attrs attrs
      pure a
    -- Act 4b /The Key in the Lock/: "(->R2)."
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> WeatheringTheStorm <$> liftRunMessage msg attrs

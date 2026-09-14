module Arkham.Homebrew.CircusExMortis.Acts.TheGreatTrainHorror (theGreatTrainHorror) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Matcher

newtype TheGreatTrainHorror = TheGreatTrainHorror ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGreatTrainHorror :: ActCard TheGreatTrainHorror
theGreatTrainHorror = act (4, A) TheGreatTrainHorror Cards.theGreatTrainHorror Nothing

instance HasAbilities TheGreatTrainHorror where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (DoomCountIs $ atLeast 9) $ forced $ RoundEnds #when
    , restricted
        a
        2
        (TokensOnLocation (locationIs Locations.circusEngine) #damage (AtLeast $ PerPlayer 4))
        $ Objective
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage TheGreatTrainHorror where
  runMessage msg a@(TheGreatTrainHorror attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceCurrentAgenda attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      doom <- getDoomCount
      push $ if doom <= 6 then R3 else R4
      pure a
    _ -> TheGreatTrainHorror <$> liftRunMessage msg attrs

module Arkham.Agenda.Cards.JourneyThroughTheGates (
  JourneyThroughTheGates (..),
  journeyThroughTheGates,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Act
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Trait (Trait (Steps))

newtype JourneyThroughTheGates = JourneyThroughTheGates AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

journeyThroughTheGates :: AgendaCard JourneyThroughTheGates
journeyThroughTheGates = agenda (1, A) JourneyThroughTheGates Cards.journeyThroughTheGates (Static 19)

instance HasAbilities JourneyThroughTheGates where
  getAbilities (JourneyThroughTheGates a) =
    [ restrictedAbility a 1 (exists $ InvestigatorAt $ LocationWithTrait Steps)
        $ ForcedAbility
        $ PlacedDoomCounter #when AnySource
        $ TargetIs
        $ toTarget a
    ]

instance RunMessage JourneyThroughTheGates where
  runMessage msg a@(JourneyThroughTheGates attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      step <- getCurrentActStep
      investigators <- getInvestigators
      if step == 4
        then
          pushAll $ Record TheDreamersStrayedFromThePath
            : map (\iid -> SufferTrauma iid 0 1) investigators
              <> map (InvestigatorDefeated (toSource attrs)) investigators
        else do
          for_ investigators $ \iid -> do
            player <- getPlayer iid
            push
              $ chooseOne
                player
                [ Label
                    "Test {willpower} (3) to remember that this is all a dream"
                    [beginSkillTest iid (toSource attrs) iid #willpower 3]
                , Label "Do not test" [SufferTrauma iid 1 0, InvestigatorDefeated (toSource attrs) iid]
                ]
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      pushAll [SufferTrauma iid 0 1, InvestigatorDefeated (toSource attrs) iid]
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      pushAll [SufferTrauma iid 1 0, InvestigatorDefeated (toSource attrs) iid]
      pure a
    _ -> JourneyThroughTheGates <$> runMessage msg attrs

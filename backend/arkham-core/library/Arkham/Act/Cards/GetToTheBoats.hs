module Arkham.Act.Cards.GetToTheBoats (
  GetToTheBoats (..),
  getToTheBoats,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype GetToTheBoats = GetToTheBoats ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

getToTheBoats :: ActCard GetToTheBoats
getToTheBoats = act (2, A) GetToTheBoats Cards.getToTheBoats Nothing

instance HasAbilities GetToTheBoats where
  getAbilities (GetToTheBoats x)
    | onSide A x =
        [ mkAbility x 1 $ ForcedAbility $ PhaseBegins #after #mythos
        , restrictedAbility
            x
            2
            (EachUndefeatedInvestigator $ InvestigatorAt "Canal-side")
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage GetToTheBoats where
  runMessage msg a@(GetToTheBoats attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      gondola <- genEncounterCard Locations.gondola
      leadInvestigatorId <- getLeadInvestigatorId
      pushAll
        [ InvestigatorDrewEncounterCard leadInvestigatorId gondola
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      targets <- selectTargets $ AssetWithTitle "Masked Carnevale-Goer"
      player <- getPlayer iid
      unless (null targets) $ pushAll [chooseOne player $ targetLabels targets (only . Flip iid source)]
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      let source = toAbilitySource attrs 2
      push $ AdvanceAct (toId attrs) source AdvancedWithOther
      pure a
    _ -> GetToTheBoats <$> runMessage msg attrs

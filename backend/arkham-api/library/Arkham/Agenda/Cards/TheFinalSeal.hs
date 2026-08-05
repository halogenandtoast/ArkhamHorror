module Arkham.Agenda.Cards.TheFinalSeal (theFinalSeal) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers (drawCthulhuDeckCard)
import Arkham.Token (Token (Resource))
import Arkham.Trait (Trait (Artifact, Cthulhu, Rooftop, Ruined))

newtype TheFinalSeal = TheFinalSeal AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFinalSeal :: AgendaCard TheFinalSeal
theFinalSeal = agenda (2, A) TheFinalSeal Cards.theFinalSeal (Static 10)

locationWithSigil :: LocationMatcher
locationWithSigil = LocationWithToken Resource

instance HasAbilities TheFinalSeal where
  getAbilities (TheFinalSeal a) =
    [ restricted
        a
        1
        ( exists (AssetWithTrait Artifact <> AssetControlledBy You)
            <> youExist
              (not_ $ InvestigatorAt $ oneOf [LocationWithTrait Rooftop, LocationWithResources (atLeast 1)])
        )
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
    , mkAbility a 2 $ forced $ PhaseEnds #when #enemy
    , onlyOnce
        $ restricted
          a
          3
          (LocationCount 5 locationWithSigil <> InVictoryDisplay (CardWithTrait Cthulhu) (atLeast 3))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TheFinalSeal where
  runMessage msg a@(TheFinalSeal attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      artifacts <- select $ AssetWithTrait Artifact <> AssetControlledBy (InvestigatorWithId iid)
      chooseTargetM iid artifacts \aid -> do
        addToVictory iid aid
        withLocationOf iid \lid -> placeTokens (attrs.ability 1) lid Resource 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      lead <- getLead
      drawCthulhuDeckCard lead (attrs.ability 2)
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceAgenda attrs
      pure a
    AdvanceAgendaBy (isSide B attrs -> True) means -> do
      case means of
        AgendaAdvancedWithDoom -> eachInvestigator $ investigatorDefeated attrs
        _ -> do
          ruined <- selectCount $ LocationWithTrait Ruined
          if ruined <= 5 then push R2 else push R3
      pure a
    _ -> TheFinalSeal <$> liftRunMessage msg attrs

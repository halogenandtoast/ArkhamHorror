module Arkham.Agenda.Cards.AgentsOfTheOuterGods (AgentsOfTheOuterGods (..), agentsOfTheOuterGods) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Prelude
import Arkham.Trait (Trait (Port))

newtype AgentsOfTheOuterGods = AgentsOfTheOuterGods AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agentsOfTheOuterGods :: AgendaCard AgentsOfTheOuterGods
agentsOfTheOuterGods = agenda (2, A) AgentsOfTheOuterGods Cards.agentsOfTheOuterGods (Static 9)

instance HasAbilities AgentsOfTheOuterGods where
  getAbilities (AgentsOfTheOuterGods x) =
    [ withTooltip
        "_Resign_. Venturing into the unknown has become too dangerous, so you return to safety with the information you've gathered."
        $ restrictedAbility x 1 (exists $ You <> at_ (withTrait Port))
        $ ActionAbility [Action.Resign]
        $ ActionCost 1
    ]

instance RunMessage AgentsOfTheOuterGods where
  runMessage msg a@(AgentsOfTheOuterGods attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      investigators <- getInvestigators
      pushAll $ map (InvestigatorDefeated (toSource attrs)) investigators
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Resign iid
      pure a
    _ -> AgentsOfTheOuterGods <$> runMessage msg attrs

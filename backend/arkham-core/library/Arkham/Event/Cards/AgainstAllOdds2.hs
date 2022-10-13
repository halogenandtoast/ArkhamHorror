module Arkham.Event.Cards.AgainstAllOdds2
  ( againstAllOdds2
  , AgainstAllOdds2(..)
  ) where

import Arkham.Prelude

import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.Target
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype AgainstAllOdds2 = AgainstAllOdds2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

againstAllOdds2 :: EventCard AgainstAllOdds2
againstAllOdds2 = event AgainstAllOdds2 Cards.againstAllOdds2

instance RunMessage AgainstAllOdds2 where
  runMessage msg e@(AgainstAllOdds2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ [Window _ (Window.InitiatedSkillTest _ maction skillType difficulty)] _
      | eid == toId attrs
      -> do
        base <- baseSkillValueFor skillType maction [] iid
        let n = difficulty - base
        pushAll
          [ CreateWindowModifierEffect
            EffectSkillTestWindow
            (EffectModifiers
            $ toModifiers attrs [ChangeRevealStrategy $ RevealAndChoose n 1]
            )
            (toSource attrs)
            (InvestigatorTarget iid)
          , Discard (toTarget attrs)
          ]
        pure e
    _ -> AgainstAllOdds2 <$> runMessage msg attrs

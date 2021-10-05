module Arkham.Types.Treachery.Cards.Kidnapped
  ( kidnapped
  , Kidnapped(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Scenario.Deck
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Kidnapped = Kidnapped TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kidnapped :: TreacheryCard Kidnapped
kidnapped = treachery Kidnapped Cards.kidnapped

instance HasAbilities Kidnapped where
  getAbilities (Kidnapped attrs) = case treacheryAttachedTarget attrs of
    Just (AgendaTarget aid) ->
      [ mkAbility attrs 1
          $ ForcedAbility
          $ AgendaAdvances Timing.When
          $ AgendaWithId aid
      ]
    _ -> []

instance TreacheryRunner env => RunMessage env Kidnapped where
  runMessage msg t@(Kidnapped attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ push
      (chooseOne
        iid
        [ Label
          "Test {willpower} (4)"
          [ BeginSkillTest
              iid
              (toSource attrs)
              (toTarget attrs)
              Nothing
              SkillWillpower
              4
          ]
        , Label
          "Test {agility} (4)"
          [ BeginSkillTest
              iid
              (toSource attrs)
              (toTarget attrs)
              Nothing
              SkillAgility
              4
          ]
        ]
      )
    FailedSkillTest iid _ _ (SkillTestInitiatorTarget target) _ _
      | isTarget attrs target -> do
        allies <- selectList (AssetOwnedBy You <> AssetWithTrait Ally)
        if null allies
          then
            t <$ push
              (InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0)
          else do
            agendaId <-
              fromJustNote "missing agenga"
              . headMay
              <$> getSetList @AgendaId ()
            t <$ pushAll
              [ chooseOne
                iid
                [ TargetLabel
                    (AssetTarget aid)
                    [AddToScenarioDeck PotentialSacrifices (AssetTarget aid)]
                | aid <- allies
                ]
              , AttachTreachery treacheryId (AgendaTarget agendaId)
              ]
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      t <$ push (UseScenarioSpecificAbility iid Nothing 1)
    _ -> Kidnapped <$> runMessage msg attrs

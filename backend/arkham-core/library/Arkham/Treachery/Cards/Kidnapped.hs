module Arkham.Treachery.Cards.Kidnapped (
  kidnapped,
  Kidnapped (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message
import Arkham.Scenario.Deck
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Kidnapped = Kidnapped TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
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

instance RunMessage Kidnapped where
  runMessage msg t@(Kidnapped attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      push
        $ chooseOne
          iid
          [ Label
              "Test {willpower} (4)"
              [beginSkillTest iid (toSource attrs) (toTarget attrs) SkillWillpower 4]
          , Label
              "Test {agility} (4)"
              [beginSkillTest iid (toSource attrs) (toTarget attrs) SkillAgility 4]
          ]
      pure t
    FailedSkillTest iid _ _ (SkillTestInitiatorTarget target) _ _
      | isTarget attrs target -> do
          allies <- selectList (AssetControlledBy You <> AllyAsset)
          if null allies
            then
              push
                $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0
            else do
              agendaId <- selectJust AnyAgenda
              pushAll
                [ chooseOne
                    iid
                    [ TargetLabel
                      (AssetTarget aid)
                      [AddToScenarioDeck PotentialSacrifices (AssetTarget aid)]
                    | aid <- allies
                    ]
                , AttachTreachery treacheryId (AgendaTarget agendaId)
                ]
          pure t
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push
        $ DrawRandomFromScenarioDeck
          iid
          PotentialSacrifices
          (toTarget attrs)
          1
      pure t
    DrewFromScenarioDeck _ PotentialSacrifices target cards
      | isTarget attrs target ->
          t
            <$ push (PlaceUnderneath AgendaDeckTarget cards)
    _ -> Kidnapped <$> runMessage msg attrs

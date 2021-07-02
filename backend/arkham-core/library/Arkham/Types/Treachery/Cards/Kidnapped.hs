module Arkham.Types.Treachery.Cards.Kidnapped
  ( kidnapped
  , Kidnapped(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.Window

newtype Kidnapped = Kidnapped TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kidnapped :: TreacheryCard Kidnapped
kidnapped = treachery Kidnapped Cards.kidnapped

instance HasModifiersFor env Kidnapped where
  getModifiersFor = noModifiersFor

instance HasActions env Kidnapped where
  getActions i (WhenAgendaAdvance aid) (Kidnapped attrs)
    | treacheryOnAgenda aid attrs
    = pure
      [ActivateCardAbilityAction i (mkAbility (toSource attrs) 1 ForcedAbility)]
  getActions i window (Kidnapped attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env Kidnapped where
  runMessage msg t@(Kidnapped attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessage
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
        allies <- getSetList @AssetId (iid, [Ally])
        if null allies
          then t <$ unshiftMessages
            [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0
            , Discard (toTarget attrs)
            ]
          else do
            agendaId <-
              fromJustNote "missing agenga"
              . headMay
              <$> getSetList @AgendaId ()
            t <$ unshiftMessages
              [ chooseOne
                iid
                [ TargetLabel
                    (AssetTarget aid)
                    [AddToScenarioDeck (AssetTarget aid)]
                | aid <- allies
                ]
              , AttachTreachery treacheryId (AgendaTarget agendaId)
              ]
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      t <$ unshiftMessage (UseScenarioSpecificAbility iid Nothing 1)
    _ -> Kidnapped <$> runMessage msg attrs

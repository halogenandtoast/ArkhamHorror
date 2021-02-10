module Arkham.Types.Treachery.Cards.Kidnapped
  ( kidnapped
  , Kidnapped(..)
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Kidnapped = Kidnapped TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kidnapped :: TreacheryId -> a -> Kidnapped
kidnapped uuid _ = Kidnapped $ baseAttrs uuid "02220"

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
      t <$ unshiftMessage (UseScenarioSpecificAbility iid 1)
    _ -> Kidnapped <$> runMessage msg attrs

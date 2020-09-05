{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.DisruptingTheRitual where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window
import ClassyPrelude hiding (sequence)
import Lens.Micro

newtype DisruptingTheRitualMetadata = DisruptingTheRitualMetadata { disruptingTheRitualClues :: Int }
  deriving stock (Show, Generic)

instance ToJSON DisruptingTheRitualMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "disruptingTheRitual"
  toEncoding = genericToEncoding $ aesonOptions $ Just "disruptingTheRitual"

instance FromJSON DisruptingTheRitualMetadata where
  parseJSON = genericParseJSON $ aesonOptions $ Just "disruptingTheRitual"

newtype DisruptingTheRitual = DisruptingTheRitual (Attrs `With` DisruptingTheRitualMetadata)
  deriving newtype (Show, ToJSON, FromJSON)

disruptingTheRitual :: DisruptingTheRitual
disruptingTheRitual =
  DisruptingTheRitual
    $ baseAttrs "01148" "Disrupting the Ritual" "Act 3a"
    `with` DisruptingTheRitualMetadata 0

instance (ActionRunner env investigator) => HasActions env investigator DisruptingTheRitual where
  getActions i NonFast (DisruptingTheRitual (Attrs {..} `With` _))
    | hasActionsRemaining i = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility (ActSource actId) 1 (ActionAbility 1 Nothing))
      | spendableClueCount i > 0
      ]
  getActions i window (DisruptingTheRitual (x `With` _)) =
    getActions i window x

instance (ActRunner env) => RunMessage env DisruptingTheRitual where
  runMessage msg a@(DisruptingTheRitual (attrs@Attrs {..} `With` metadata@DisruptingTheRitualMetadata {..}))
    = case msg of
      AdvanceAct aid | aid == actId && actSequence == "Act 3a" -> do
        leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
        unshiftMessage (Ask leadInvestigatorId $ ChooseOne [AdvanceAct actId])
        pure
          $ DisruptingTheRitual
          . (`with` metadata)
          $ attrs
          & (sequence .~ "Act 3b")
          & (flipped .~ True)
      AdvanceAct aid | aid == actId && actSequence == "Act 3a" ->
        a <$ unshiftMessage (Resolution 1)
      PlaceClues (ActTarget aid) n | aid == actId -> do
        playerCount <- unPlayerCount <$> asks (getCount ())
        let totalClues = n + disruptingTheRitualClues
        when (totalClues >= 2 * playerCount) (unshiftMessage (AdvanceAct actId))
        pure $ DisruptingTheRitual
          (attrs `with` DisruptingTheRitualMetadata totalClues)
      UseCardAbility iid _ (ActSource aid) _ 1 | aid == actId -> do
        a <$ unshiftMessage
          (Ask iid $ ChooseOne
            [ BeginSkillTest
              iid
              (ActSource actId)
              Nothing
              SkillWillpower
              3
              [PlaceClues (ActTarget actId) 1]
              mempty
              mempty
              mempty
            , BeginSkillTest
              iid
              (ActSource actId)
              Nothing
              SkillAgility
              3
              [PlaceClues (ActTarget actId) 1]
              mempty
              mempty
              mempty
            ]
          )
      _ -> DisruptingTheRitual . (`with` metadata) <$> runMessage msg attrs

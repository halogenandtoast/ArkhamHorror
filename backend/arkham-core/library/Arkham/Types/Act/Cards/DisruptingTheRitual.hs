{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.DisruptingTheRitual where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window
import ClassyPrelude hiding (sequence)
import Lens.Micro
import Safe (fromJustNote)

newtype DisruptingTheRitual = DisruptingTheRitual Attrs
  deriving newtype (Show, ToJSON, FromJSON)

disruptingTheRitual :: DisruptingTheRitual
disruptingTheRitual =
  DisruptingTheRitual $ (baseAttrs "01148" "Disrupting the Ritual" "Act 3a")
    { actClues = Just 0
    }

instance (ActionRunner env investigator) => HasActions env investigator DisruptingTheRitual where
  getActions i NonFast (DisruptingTheRitual Attrs {..})
    | hasActionsRemaining i Nothing mempty = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility (ActSource actId) 1 (ActionAbility 1 Nothing))
      | spendableClueCount i > 0
      ]
  getActions i window (DisruptingTheRitual x) = getActions i window x

instance (ActRunner env) => RunMessage env DisruptingTheRitual where
  runMessage msg a@(DisruptingTheRitual attrs@Attrs {..}) = case msg of
    AdvanceAct aid | aid == actId && actSequence == "Act 3a" -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      unshiftMessage (Ask leadInvestigatorId $ ChooseOne [AdvanceAct actId])
      pure
        $ DisruptingTheRitual
        $ attrs
        & (sequence .~ "Act 3b")
        & (flipped .~ True)
    AdvanceAct aid | aid == actId && actSequence == "Act 3a" ->
      a <$ unshiftMessage (Resolution 1)
    PlaceClues (ActTarget aid) n | aid == actId -> do
      playerCount <- unPlayerCount <$> asks (getCount ())
      let totalClues = n + fromJustNote "Must be set" actClues
      when (totalClues >= 2 * playerCount) (unshiftMessage (AdvanceAct actId))
      pure $ DisruptingTheRitual (attrs { actClues = Just totalClues })
    UseCardAbility iid _ (ActSource aid) _ 1 | aid == actId -> do
      a <$ unshiftMessage
        (Ask iid $ ChooseOne
          [ BeginSkillTest
            iid
            (ActSource actId)
            (ActTarget actId)
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
            (ActTarget actId)
            Nothing
            SkillAgility
            3
            [PlaceClues (ActTarget actId) 1]
            mempty
            mempty
            mempty
          ]
        )
    _ -> DisruptingTheRitual <$> runMessage msg attrs

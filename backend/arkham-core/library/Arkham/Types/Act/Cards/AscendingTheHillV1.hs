module Arkham.Types.Act.Cards.AscendingTheHillV1
  ( AscendingTheHillV1(..)
  , ascendingTheHillV1
  )
where

import Arkham.Prelude

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.ActId
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Trait

newtype AscendingTheHillV1 = AscendingTheHillV1 ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingTheHillV1 :: AscendingTheHillV1
ascendingTheHillV1 = AscendingTheHillV1
  $ baseAttrs "02278" "Ascending the Hill (v. I)" (Act 2 A) Nothing

instance HasSet Trait env LocationId => HasModifiersFor env AscendingTheHillV1 where
  getModifiersFor _ (LocationTarget lid) (AscendingTheHillV1 attrs) = do
    traits <- getSet lid
    pure $ toModifiers attrs [ CannotPlaceClues | Altered `notMember` traits ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env AscendingTheHillV1 where
  getActions i window (AscendingTheHillV1 x) = getActions i window x

instance ActRunner env => RunMessage env AscendingTheHillV1 where
  runMessage msg a@(AscendingTheHillV1 attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage
        $ chooseOne leadInvestigatorId [AdvanceAct aid (toSource attrs)]
      pure
        . AscendingTheHillV1
        $ attrs
        & (sequenceL .~ Act (unActStep $ actStep actSequence) B)
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ unshiftMessage (NextAct actId "02281")
    WhenEnterLocation _ "02284" ->
      a <$ unshiftMessage (AdvanceAct actId (toSource attrs))
    _ -> AscendingTheHillV1 <$> runMessage msg attrs

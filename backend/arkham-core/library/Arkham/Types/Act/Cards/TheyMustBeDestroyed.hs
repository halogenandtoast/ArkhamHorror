module Arkham.Types.Act.Cards.TheyMustBeDestroyed
  ( TheyMustBeDestroyed(..)
  , theyMustBeDestroyed
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution

newtype TheyMustBeDestroyed = TheyMustBeDestroyed ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyMustBeDestroyed :: TheyMustBeDestroyed
theyMustBeDestroyed = TheyMustBeDestroyed
  $ baseAttrs "02241" "They Must Be Destroyed!" (Act 2 A) Nothing

instance ActionRunner env => HasActions env TheyMustBeDestroyed where
  getActions i window (TheyMustBeDestroyed x) = do
    leadInvestigatorId <- getLeadInvestigatorId
    setAsideBroodOfYogSothothCount <- unSetAsideCount
      <$> getCount @SetAsideCount (CardCode "02255")
    inPlayBroodOfYogSothothCount <- length
      <$> getSet @EnemyId (CardCode "02255")
    if (setAsideBroodOfYogSothothCount + inPlayBroodOfYogSothothCount) == 0
      then
        pure
          [ UseAbility
              leadInvestigatorId
              (mkAbility (toSource x) 1 ForcedAbility)
          ]
      else getActions i window x

instance ActRunner env => RunMessage env TheyMustBeDestroyed where
  runMessage msg a@(TheyMustBeDestroyed attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 2)
    _ -> TheyMustBeDestroyed <$> runMessage msg attrs

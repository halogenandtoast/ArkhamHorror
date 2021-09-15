module Arkham.Types.Act.Cards.TheOath
  ( TheOath(..)
  , theOath
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Resolution
import Arkham.Types.Target
import Arkham.Types.Trait

newtype TheOath = TheOath ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theOath :: ActCard TheOath
theOath = act
  (3, A)
  TheOath
  Cards.theOath
  (Just $ GroupClueCost (PerPlayer 3) (locationIs Locations.hiddenLibrary))

instance Query LocationMatcher env => HasModifiersFor env TheOath where
  getModifiersFor _ (LocationTarget lid) (TheOath attrs) = do
    isPassageway <- member lid <$> select (LocationWithTrait Passageway)
    pure $ toModifiers
      attrs
      [ ConnectedTo (LocationWithTrait Passageway) | isPassageway ]
  getModifiersFor _ _ _ = pure []

instance ActRunner env => RunMessage env TheOath where
  runMessage msg a@(TheOath attrs) = case msg of
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ push
        (chooseOne
          leadInvestigatorId
          [ Label
            "This is an important discovery! We should take it. (-> R1)"
            [ScenarioResolution $ Resolution 1]
          , Label
            "It's just a silly trinket, and it would be wrong to steal from the Historical Society. Leave it behind (-> R2)"
            [ScenarioResolution $ Resolution 2]
          ]
        )
    _ -> TheOath <$> runMessage msg attrs

module Arkham.Types.Location.Cards.CongregationalChurch_208
  ( congregationalChurch_208
  , CongregationalChurch_208(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (congregationalChurch_208)
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Trait

newtype CongregationalChurch_208 = CongregationalChurch_208 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congregationalChurch_208 :: LocationId -> CongregationalChurch_208
congregationalChurch_208 = CongregationalChurch_208 . baseAttrs
  Cards.congregationalChurch_208
  1
  (PerPlayer 1)
  Diamond
  [Plus, Triangle, Squiggle]

instance HasModifiersFor env CongregationalChurch_208 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env CongregationalChurch_208 where
  getActions = withDrawCardUnderneathAction

instance LocationRunner env => RunMessage env CongregationalChurch_208 where
  runMessage msg l@(CongregationalChurch_208 attrs) = case msg of
    RevealLocation miid lid | lid == locationId attrs -> do
      iid <- maybe getLeadInvestigatorId pure miid
      unshiftMessage $ FindEncounterCard
        iid
        (toTarget attrs)
        (CardMatchByType (EnemyType, singleton Humanoid))
      CongregationalChurch_208 <$> runMessage msg attrs
    FoundEncounterCard _iid target card | isTarget attrs target -> do
      villageCommonsId <- fromJustNote "missing village commons"
        <$> getId (LocationWithTitle "Village Commons")
      l <$ unshiftMessage (SpawnEnemyAt (EncounterCard card) villageCommonsId)
    _ -> CongregationalChurch_208 <$> runMessage msg attrs

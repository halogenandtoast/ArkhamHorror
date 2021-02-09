module Arkham.Types.Location.Cards.CongregationalChurch_208
  ( congregationalChurch_208
  , CongregationalChurch_208(..)
  ) where


import Arkham.Types.Card.EncounterCardMatcher
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype CongregationalChurch_208 = CongregationalChurch_208 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congregationalChurch_208 :: CongregationalChurch_208
congregationalChurch_208 = CongregationalChurch_208 $ baseAttrs
  "02208"
  (Name "Congregational Church" Nothing)
  EncounterSet.BloodOnTheAltar
  1
  (PerPlayer 1)
  Diamond
  [Plus, Triangle, Squiggle]
  [Dunwich]

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
        (EncounterCardMatchByType (EnemyType, Just Humanoid))
      CongregationalChurch_208 <$> runMessage msg attrs
    FoundEncounterCard _iid target card | isTarget attrs target -> do
      villageCommonsId <- fromJustNote "missing village commons"
        <$> getId (LocationWithTitle "Village Commons")
      l <$ unshiftMessage (SpawnEnemyAt (EncounterCard card) villageCommonsId)
    _ -> CongregationalChurch_208 <$> runMessage msg attrs

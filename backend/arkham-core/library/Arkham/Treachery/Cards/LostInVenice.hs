module Arkham.Treachery.Cards.LostInVenice (
  lostInVenice,
  LostInVenice (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Movement
import Arkham.Projection
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LostInVenice = LostInVenice TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

lostInVenice :: TreacheryCard LostInVenice
lostInVenice = treachery LostInVenice Cards.lostInVenice

instance RunMessage LostInVenice where
  runMessage msg t@(LostInVenice attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      let take2damage = InvestigatorAssignDamage iid source DamageAny 2 0
      case mlid of
        Nothing -> push take2damage
        Just lid -> do
          acrossLocationId <- getAcrossLocation lid
          player <- getPlayer iid
          push
            $ chooseOne
              player
              [ Label "Take 2 damage" [take2damage]
              , Label
                  "Move to the location across from you"
                  [Move $ move source iid acrossLocationId]
              ]
      pure t
    _ -> LostInVenice <$> runMessage msg attrs

module Arkham.Treachery.Cards.LostInVenice
  ( lostInVenice
  , LostInVenice(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Treachery.Runner
import Arkham.Treachery.Cards qualified as Cards

newtype LostInVenice = LostInVenice TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
          push $ chooseOne
            iid
            [ Label "Take 2 damage" [take2damage]
            , Label
              "Move to the location across from you"
              [Move source iid lid acrossLocationId]
            ]
      pure t
    _ -> LostInVenice <$> runMessage msg attrs
